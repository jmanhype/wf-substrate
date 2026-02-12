# Implement real parallel fork/join synchronization in wf_exec. Currently par_fork creates branch entries but join_wait never resolves because the scheduler returns mock tokens and branch completion is not tracked. Fix the executor so that: (1) par_fork spawns tokens for each branch and executes them, (2) each branch token runs to its done opcode, (3) join_wait with policy all blocks until all branches have completed, (4) after join, execution continues at the next opcode. The scheduler must select real active tokens not mock_token. ACCEPTANCE TEST in test/wf_acceptance_tests.erl: Compile wf_term:par([wf_term:task(x, #{function => fun(Ctx) -> {ok, Ctx#{x_ran => true}} end}), wf_term:task(y, #{function => fun(Ctx) -> {ok, Ctx#{y_ran => true}} end})]). Execute it with wf_exec:run/3. Assert it returns {done, FinalState} (NOT {yield, _}). Assert final context contains x_ran and y_ran. Do NOT use mocks. Implementation Plan

## Implementation Plan Title

Real Parallel Fork/Join with Token-Based Scheduler Selection

## Overview

Implement functional parallel fork/join synchronization by fixing the scheduler to select real active tokens instead of mock tokens. The current scheduler returns hardcoded `mock_token` values, causing the executor to fail when executing parallel workflows. This fix enables parallel workflows where multiple branches execute concurrently, each running to completion before joining at a synchronization point.

## Current State

**Existing Implementation Issues:**

1. **wf_sched.erl:107-117**: `select_action/2` returns `{token, mock_token}` for all scheduler policies without inspecting exec_state
   - Does not extract active tokens from `exec_state.tokens`
   - No token selection logic implemented
   - Scheduler treats exec_state as opaque but needs to inspect tokens

2. **wf_exec.erl:338-364**: `step_normal/2` uses `current_token` field to fetch opcode
   - Receives `SchedDecision` parameter but doesn't use it to switch tokens
   - Trusts existing `current_token` field which becomes stale in multi-token scenarios
   - Line 357: Syncs IP with current token in multi-token mode, but token selection is not scheduler-driven

3. **wf_exec.erl:556-621**: `execute_par_fork/2` spawns multiple active tokens
   - Creates branch tokens with `status = active` (line 572)
   - Uses internal `select_next_token/1` (line 612) instead of scheduler
   - Subsequent steps need scheduler to select among active tokens

4. **wf_exec.erl:718-757**: `execute_join_wait/2` checks join counter completion
   - Correctly increments join counter via `execute_done/1` (lines 972-1056)
   - Join wait blocks until `completed >= required` (line 724)
   - Creates continuation token when satisfied (lines 732-750)

**What Works:**
- PAR_FORK correctly spawns N tokens for parallel branches
- JOIN_WAIT correctly tracks completion via join counters
- DONE opcode increments join counter
- Task function lookup from metadata works (wf_exec:lookup_task_function/2)

**What's Broken:**
- Scheduler never selects real tokens, always returns mock_token
- Executor doesn't use scheduler decision to switch between tokens
- Parallel workflows fail because executor tries to execute opcodes for non-existent mock_token

## Desired End State

**Specification:**

1. **wf_sched:select_action/2** extracts enabled actions from exec_state.tokens:
   - Filter tokens where `status =:= active`
   - Build list of `{token, TokenId}` actions
   - Deterministic policy: select first token by sorted token_id
   - Return `{token, SelectedTokenId}` or `{token, undefined}` if no active tokens

2. **wf_exec:step_normal/2** respects scheduler decision:
   - Update `current_token` field from `SchedDecision` before fetching opcode
   - Use selected token for multi-token execution
   - Handle `undefined` token (no active tokens) correctly

3. **Parallel workflows execute completely:**
   - PAR_FORK spawns N active tokens
   - Scheduler selects tokens in deterministic order
   - Each branch executes its task function
   - DONE increments join counter
   - JOIN_WAIT unblocks when all branches complete
   - Continuation token advances to next opcode

4. **Acceptance test passes:**
   ```erlang
   Term = wf_term:par([
       wf_term:task(x, #{function => fun(Ctx) -> {ok, Ctx#{x_ran => true}} end}),
       wf_term:task(y, #{function => fun(Ctx) -> {ok, Ctx#{y_ran => true}} end})
   ]),
   {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
   ExecState0 = wf_exec:new({Bytecode, Metadata}),
   {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),
   FinalCtx = wf_exec:get_ctx(ExecState1),
   %% Assert:
   ?assertEqual(true, maps:get(x_ran, FinalCtx)),
   ?assertEqual(true, maps:get(y_ran, FinalCtx))
   ```

### Key Discoveries:

- **wf_exec.hrl:7-15**: Token record has `status` field that can be `active | complete | cancelled | blocked_effect | blocked_approval`
- **wf_exec.hrl:32-46**: exec_state record has `tokens :: #{term() => #token{}}` and `current_token :: term() | undefined`
- **wf_exec.erl:356-359**: Multi-token mode syncs IP with current token's IP before fetching opcode
- **wf_sched.erl:37**: Scheduler currently treats exec_state as opaque, but this must change
- **wf_acceptance_tests.erl:24-59**: Test pattern shows how to compile term with metadata and verify context updates
- **wf_test_par.erl:82-91**: Existing test uses `undefined` policy and mock bytecode, must not break backward compatibility

## What We're NOT Doing

- **NOT implementing nondeterministic or replay scheduler policies** - Only fixing deterministic policy
- **NOT changing PAR_FORK or JOIN_WAIT semantics** - These work correctly, only token selection is broken
- **NOT modifying compiler** - wf_compile:compile/1 already generates correct bytecode with metadata
- **NOT changing task function dispatch** - wf_exec:lookup_task_function/2 already works correctly
- **NOT implementing XOR branch selection** - Only parallel fork/join in scope
- **NOT adding new opcodes** - Existing par_fork, join_wait, done opcodes are sufficient
- **NOT changing single-token execution** - Sequential workflows unaffected by changes

## Implementation Approach

**Strategy:** Fix token selection in three coordinated steps:

1. **Phase 1: Scheduler Token Extraction** - Modify wf_sched to extract active tokens from exec_state and return real token_ids
2. **Phase 2: Executor Scheduler Integration** - Modify wf_exec to use scheduler decision when selecting current token
3. **Phase 3: Acceptance Test** - Add end-to-end test verifying parallel execution with real task functions

**Key Design Decisions:**

- **Scheduler inspects exec_state record directly**: sched.erl is internal module, not external API, so breaking opacity is acceptable
- **Deterministic ordering by token_id**: Use `lists:sort/1` on token_ids for stable selection order
- **Backward compatibility with mock bytecode**: Keep returning `mock_token` when exec_state has no tokens (empty maps)
- **current_token update in step_normal**: Update field before syncing IP to ensure multi-token mode works correctly
- **No changes to PAR_FORK internal selection**: Keep using `select_next_token/1` for initial post-fork token, scheduler handles subsequent steps

**Risk Mitigation:**

- Existing tests use `undefined` policy and expect mock_token - maintain by detecting empty tokens map
- Deterministic policy uses token_id sort for stable ordering - prevents non-determinism
- Join wait logic already tested - only token selection needs fixing
- Task function dispatch already works - metadata lookup is correct

---

## Phases

### Phase 1: Fix Scheduler Token Selection

#### Overview

Modify wf_sched:select_action/2 to extract active tokens from exec_state and return real token_ids instead of mock_token. This enables the scheduler to select which token to execute next in multi-token scenarios.

#### Changes Required:

##### 1. wf_sched.erl

**File**: `src/wf_sched.erl`
**Changes**: Replace mock token implementation with real token extraction

**Location**: Lines 107-117

**Current Implementation:**
```erlang
-spec select_action(exec_state(), sched_policy() | sched_state()) -> sched_decision().
select_action(_ExecState, PolicyOrState) when is_atom(PolicyOrState) ->
    case PolicyOrState of
        deterministic -> {token, mock_token};
        nondeterministic -> {token, mock_token};
        {replay, _} -> {token, replay_token};
        undefined -> {token, mock_token}
    end;
```

**New Implementation:**
```erlang
-spec select_action(exec_state(), sched_policy() | sched_state()) -> sched_decision().
select_action(ExecState, PolicyOrState) when is_atom(PolicyOrState) ->
    %% Extract enabled actions from executor state
    EnabledActions = extract_enabled_actions(ExecState),

    case EnabledActions of
        [] ->
            %% No active tokens - backward compatibility with mock bytecode
            {token, undefined};
        [Single] ->
            %% Only one active token, execute it
            Single;
        Multiple when PolicyOrState =:= deterministic ->
            %% Select first token by sorted token_id for stable ordering
            hd(lists:sort(Multiple));
        Multiple when PolicyOrState =:= nondeterministic ->
            %% TODO: Implement random selection in future PR
            hd(lists:sort(Multiple));
        _Other ->
            %% Fallback for replay and other policies
            {token, undefined}
    end;
```

**Add helper function:**
```erlang
%% @doc Extract enabled actions from executor state
%% Returns list of {token, token_id()} for all active tokens
-spec extract_enabled_actions(exec_state()) -> [sched_decision()].
extract_enabled_actions(ExecState) ->
    TokensMap = ExecState#exec_state.tokens,
    ActiveTokens = [T || T <- maps:values(TokensMap), T#token.status =:= active],
    [{token, T#token.token_id} || T <- ActiveTokens].
```

**Update module doc to clarify token selection:**
```erlang
%% @doc Scheduler selects next action based on enabled tokens in exec_state
%% For deterministic policy: selects token with lowest token_id (sorted order)
%% Returns {token, TokenId} or {token, undefined} if no active tokens
```

##### 2. wf_exec.hrl (if needed for type specs)

**File**: `src/wf_exec.hrl`
**Changes**: No changes required - exec_state record already has tokens field

#### Success Criteria:

##### Automated Verification:

- [ ] Compile wf_sched.erl with no warnings
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] Unit tests pass: `rebar3 eunit --module=wf_sched` (if tests exist)
- [ ] Type check passes: `rebar3 shell` and load module without errors

##### Manual Verification:

- [ ] Review code changes for Erlang/OTP conventions
- [ ] Verify extract_enabled_actions/1 filters by status =:= active
- [ ] Verify deterministic policy uses lists:sort for stable ordering
- [ ] Confirm backward compatibility: returns {token, undefined} when no active tokens

**Note**: Complete automated verification, then verify the code logic matches research findings before proceeding to Phase 2.

---

### Phase 2: Integrate Scheduler Decision in Executor

#### Overview

Modify wf_exec:step_normal/2 to use the scheduler decision to select which token to execute. Update the current_token field before syncing IP and fetching opcode, ensuring the executor respects the scheduler's token selection in multi-token scenarios.

#### Changes Required:

##### 1. wf_exec.erl

**File**: `src/wf_exec.erl`
**Changes**: Update step_normal/2 to use SchedDecision for token selection

**Location**: Lines 338-364

**Current Implementation:**
```erlang
step_normal(ExecState, SchedDecision) ->
    case ExecState#exec_state.current_token of
        undefined ->
            %% No active tokens available
            case maps:values(ExecState#exec_state.tokens) of
                [] ->
                    %% All tokens complete, mark as done
                    FinalExecState = ExecState#exec_state{status = done},
                    TraceEvent = #{type => done, reason => no_tokens},
                    {FinalExecState, TraceEvent};
                _ ->
                    %% Tokens exist but none are active (blocked state)
                    TraceEvent = #{type => blocked, reason => no_active_token},
                    {ExecState, TraceEvent}
            end;
        _TokenId ->
            %% In multi-token mode, sync IP with current token before fetching opcode
            %% In single-token mode, trust ExecState.ip (token.ip is stale)
            SyncedExecState = case maps:size(ExecState#exec_state.tokens) > 1 of
                true -> sync_ip_with_current_token(ExecState);
                false -> ExecState
            end,
            Opcode = fetch_opcode(SyncedExecState),
            NewExecState = execute_opcode(Opcode, SyncedExecState, SchedDecision),
            TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
            {NewExecState, TraceEvent}
    end.
```

**New Implementation:**
```erlang
step_normal(ExecState, {token, SelectedTokenId}) ->
    %% Update current_token from scheduler decision before executing
    ExecState1 = ExecState#exec_state{current_token = SelectedTokenId},

    case SelectedTokenId of
        undefined ->
            %% No active tokens available
            case maps:values(ExecState1#exec_state.tokens) of
                [] ->
                    %% All tokens complete, mark as done
                    FinalExecState = ExecState1#exec_state{status = done},
                    TraceEvent = #{type => done, reason => no_tokens},
                    {FinalExecState, TraceEvent};
                _ ->
                    %% Tokens exist but none are active (blocked state)
                    TraceEvent = #{type => blocked, reason => no_active_token},
                    {ExecState1, TraceEvent}
            end;
        _TokenId ->
            %% In multi-token mode, sync IP with current token before fetching opcode
            %% In single-token mode, trust ExecState.ip (token.ip is stale)
            SyncedExecState = case maps:size(ExecState1#exec_state.tokens) > 1 of
                true -> sync_ip_with_current_token(ExecState1);
                false -> ExecState1
            end,
            Opcode = fetch_opcode(SyncedExecState),
            NewExecState = execute_opcode(Opcode, SyncedExecState, {token, SelectedTokenId}),
            TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
            {NewExecState, TraceEvent}
    end.
```

**Key Changes:**
- Pattern match on `{token, SelectedTokenId}` instead of accessing `ExecState#exec_state.current_token`
- Update `current_token` field immediately: `ExecState1 = ExecState#exec_state{current_token = SelectedTokenId}`
- Use `ExecState1` for all subsequent operations
- Pass updated scheduler decision to execute_opcode

##### 2. Update function spec (if needed)

**File**: `src/wf_exec.erl`
**Changes**: Add or update spec for step_normal/2

```erlang
%% @private Normal step (not blocked)
%% Selects token based on scheduler decision and executes opcode
-spec step_normal(exec_state(), sched_decision()) -> {exec_state(), map()}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Compile wf_exec.erl with no warnings
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] All existing tests pass: `rebar3 eunit`
- [ ] Specifically verify: `rebar3 eunit --module=wf_test_par`

##### Manual Verification:

- [ ] Verify step_normal/2 pattern matches on {token, SelectedTokenId}
- [ ] Confirm current_token is updated before IP sync
- [ ] Check that scheduler decision is passed through to execute_opcode
- [ ] Test with existing wf_test_par tests to ensure no regressions

**Note**: Complete automated verification, then manually verify the token selection flow before proceeding to Phase 3.

---

### Phase 3: Add Acceptance Test for Parallel Execution

#### Overview

Add an acceptance test in test/wf_acceptance_tests.erl that verifies parallel fork/join executes both branch tasks completely. The test compiles a par([task(x), task(y)]) term using wf_compile:compile/1, executes it with wf_exec:run/3, and asserts that both task functions execute and update the context.

#### Changes Required:

##### 1. test/wf_acceptance_tests.erl

**File**: `test/wf_acceptance_tests.erl`
**Changes**: Add new test function at end of file

**Location**: After line 139 (after loop_count_test_0)

**New Test Implementation:**
```erlang
%%--------------------------------------------------------------------
%% @doc Acceptance test for parallel fork/join execution
%%
%% This test verifies:
%% 1. PAR_FORK spawns multiple tokens for parallel branches
%% 2. Scheduler selects real tokens (not mock_token)
%% 3. Each branch executes its task function to completion
%% 4. JOIN_WAIT blocks until all branches complete
%% 5. Workflow returns {done, _} (not {yield, _})
%% 6. Context contains updates from all task functions
%%
%% NO MOCKS: Uses real task functions from metadata
%%--------------------------------------------------------------------
parallel_fork_join_test_() ->
    {"Parallel fork/join executes both branches and merges context",
     fun() ->
         %% Build workflow: par([task(x), task(y)])
         %% Each task sets a unique key in the context
         Term = wf_term:par([
             wf_term:task(x, #{
                 function => fun(Ctx) ->
                     {ok, Ctx#{x_ran => true}}
                 end
             }),
             wf_term:task(y, #{
                 function => fun(Ctx) ->
                     {ok, Ctx#{y_ran => true}}
                 end
             })
         ]),

         %% Compile to bytecode (returns {ok, {Bytecode, Metadata}})
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),

         %% Verify metadata was collected for both tasks
         ?assertEqual(2, map_size(Metadata)),
         ?assert(maps:is_key(x, Metadata)),
         ?assert(maps:is_key(y, Metadata)),

         %% Execute workflow with deterministic scheduler
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         Result = wf_exec:run(ExecState0, 100, deterministic),

         %% Verify result is {done, _} not {yield, _}
         ?assertMatch({done, _}, Result),

         %% Extract final state for context verification
         {done, ExecState1} = Result,

         %% Verify both tasks ran (context has both keys)
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(true, maps:get(x_ran, FinalCtx)),
         ?assertEqual(true, maps:get(y_ran, FinalCtx)),
         ?assertEqual(2, map_size(FinalCtx))
     end}.
```

**Module header update (if needed):**
Add to module doc or comments indicating this is the parallel execution acceptance test.

##### 2. Update test runner (if needed)

**File**: `test/wf_acceptance_tests.erl`
**Changes**: Export new test function (if not using test_() generator)

**Verify test is registered:**
- EUnit test_() generator automatically exports tests
- No manual registration needed

#### Success Criteria:

##### Automated Verification:

- [ ] Test compiles with no errors
- [ ] Test passes: `rebar3 eunit --module=wf_acceptance_tests`
- [ ] Specifically run new test: `rebar3 eunit --test=parallel_fork_join_test_`
- [ ] Verify test runs without mocking framework

##### Manual Verification:

- [ ] Run test manually and observe output confirms {done, _} result
- [ ] Verify context contains x_ran and y_ran keys
- [ ] Confirm both task functions executed (check trace logs if available)
- [ ] Test with 3-branch parallel to ensure scalability

**Debugging steps if test fails:**
1. Add `io:format("~p~n", [Result])` to see actual result
2. Check if scheduler is returning real token_ids (add debug in wf_sched)
3. Verify PAR_FORK spawned tokens (check ExecState#exec_state.tokens)
4. Verify JOIN_WAIT unblocked (check join counters)
5. Verify task functions were called (context should have x_ran and y_ran)

**Note**: Complete automated verification, then manually verify the test passes end-to-end.

---

## Testing Strategy

### Unit Tests:

- **wf_sched.erl**: Test extract_enabled_actions/1 with various token states
  - All tokens active → return all token_ids
  - Some tokens active → filter correctly
  - No tokens active → return []
  - Mixed status (active, complete, blocked) → filter to active only

- **wf_sched.erl**: Test deterministic policy selection
  - Single active token → return it directly
  - Multiple active tokens → select first by sorted token_id
  - No active tokens → return {token, undefined}

- **wf_exec.erl**: Test step_normal/2 token selection
  - Scheduler selects token_id → current_token updated
  - Scheduler returns undefined → handle correctly
  - Multi-token mode → IP synced with selected token

### Integration Tests:

- **wf_test_par.erl**: Run existing parallel tests
  - `par_2_branches_all_complete_test/0` - should still pass
  - `par_3_branches_all_complete_test/0` - should still pass
  - `par_mixed_completion_test/0` - should still pass
  - All tests now use real scheduler instead of mock_token

- **wf_test_seq.erl**: Ensure sequential workflows unaffected
  - Single-token execution should work identically
  - No regressions in sequential execution

- **wf_test_join.erl**: Verify join mechanics still work
  - `join_wait_all_3_test/0` - should still pass
  - Join counters increment correctly
  - Continuation token created when join satisfied

### Manual Testing Steps:

1. **Compile project**: `rebar3 compile`
2. **Run all unit tests**: `rebar3 eunit`
3. **Run acceptance tests**: `rebar3 eunit --module=wf_acceptance_tests`
4. **Run parallel tests specifically**: `rebar3 eunit --module=wf_test_par`
5. **Check dialyzer**: `rebar3 dialyzer`
6. **Manual verification of acceptance test**:
   - Execute parallel workflow in Erlang shell
   - Inspect exec_state tokens and join counters
   - Verify scheduler selects real token_ids
   - Confirm both task functions execute

### Regression Testing:

- All existing tests must pass with no changes to test code
- Existing tests using `undefined` policy should continue working
- Single-token workflows (sequential) should be unaffected
- No performance degradation expected (token selection is O(n) where n = active tokens)

## Migration Notes

**No migration required** - This is a bug fix, not a breaking change:

- Existing code using `deterministic` policy will automatically get real token selection
- Existing code using `undefined` policy will continue working (backward compatibility maintained)
- No API changes - wf_sched:select_action/2 signature unchanged
- No data format changes - exec_state record structure unchanged

**Backward Compatibility:**
- Mock bytecode tests (no tokens in exec_state) continue to work
- Scheduler returns {token, undefined} when no active tokens, which is handled by executor
- Existing tests in wf_test_par.erl use `undefined` policy and should pass unchanged

**Future Enhancements (out of scope):**
- Nondeterministic policy: Random token selection
- Replay policy: Log-based token selection for reproducible execution
- Token priority: Support priority-based scheduling
- Parallel execution with actual concurrency (beyond cooperative scheduling)

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/053-implement-real-parallel-forkjoin-synchronization-i/research.md`
- wf_exec.erl: Lines 338-364 (step_normal/2), 556-621 (execute_par_fork/2), 718-757 (execute_join_wait/2)
- wf_sched.erl: Lines 107-117 (select_action/2 - ROOT CAUSE)
- wf_exec.hrl: Lines 7-46 (token, branch_info, join_counter, exec_state records)
- wf_compile.erl: Lines 236-260 (compile_par_with_metadata/1)
- wf_acceptance_tests.erl: Lines 24-59 (task_function_dispatch_test_0 pattern), 97-139 (loop_count_test_0 pattern)
- wf_test_par.erl: Lines 82-91 (par_2_branches_all_complete_test/0 - existing test)
