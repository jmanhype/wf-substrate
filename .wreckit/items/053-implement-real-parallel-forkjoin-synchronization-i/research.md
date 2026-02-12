# Research: Implement real parallel fork/join synchronization in wf_exec. Currently par_fork creates branch entries but join_wait never resolves because the scheduler returns mock tokens and branch completion is not tracked. Fix the executor so that: (1) par_fork spawns tokens for each branch and executes them, (2) each branch token runs to its done opcode, (3) join_wait with policy all blocks until all branches have completed, (4) after join, execution continues at the next opcode. The scheduler must select real active tokens not mock_token. ACCEPTANCE TEST in test/wf_acceptance_tests.erl: Compile wf_term:par([wf_term:task(x, #{function => fun(Ctx) -> {ok, Ctx#{x_ran => true}} end}), wf_term:task(y, #{function => fun(Ctx) -> {ok, Ctx#{y_ran => true}} end})]). Execute it with wf_exec:run/3. Assert it returns {done, FinalState} (NOT {yield, _}). Assert final context contains x_ran and y_ran. Do NOT use mocks.

**Date**: 2025-01-18
**Item**: 053-implement-real-parallel-forkjoin-synchronization-i

## Research Question

The parallel fork/join synchronization in wf_exec is currently non-functional. The PAR_FORK opcode creates branch entries and spawns tokens, but the scheduler returns mock_token instead of selecting real active tokens, and JOIN_WAIT never resolves because branch completion tracking is not properly integrated with the scheduler's token selection mechanism. The acceptance test requires that a parallel workflow with two tasks completes successfully, executing both task functions and merging their context updates.

## Summary

The core issue is that `wf_sched:select_action/2` returns `{token, mock_token}` for all scheduler policies (deterministic, nondeterministic, replay) instead of inspecting the executor state to select actual active tokens. This causes the executor to fail at line 354-364 of wf_exec.erl where it expects to execute opcodes for the current token.

**Root Causes**:
1. **Scheduler Stub Implementation**: wf_sched:select_action/2 (lines 107-117) returns hardcoded `{token, mock_token}` values without inspecting exec_state
2. **No Token Selection Logic**: The scheduler doesn't extract enabled tokens from exec_state.tokens or select among them
3. **Missing Scheduler Integration**: While PAR_FORK creates multiple active tokens (lines 556-621), the scheduler never switches between them

**Acceptance Test Requirements**:
- Compile `wf_term:par([task(x, ...), task(y, ...])` using wf_compile:compile/1
- Execute with `wf_exec:run/3` returning `{done, FinalState}` (not `{yield, _}`)
- Both task functions must execute (context contains `x_ran => true` and `y_ran => true`)
- No mocks: use real task functions from metadata

## Current State Analysis

### Existing Implementation

**Executor (wf_exec.erl)**:
- **Lines 556-621**: `execute_par_fork/2` spawns N tokens for parallel branches
  - Creates branch_info record tracking tokens and join_id
  - Creates join_counter with `required = NumBranches`
  - Spawns tokens at target IPs with `status = active`
  - Removes current token and selects next token via `select_next_token/1` (line 612)
- **Lines 718-757**: `execute_join_wait/2` checks join counter completion
  - Finds active join via `find_active_join/1` (line 721)
  - If `completed >= required`, creates continuation token and advances IP
  - If not satisfied, sets status to `blocked_join`
- **Lines 972-1056**: `execute_done/1` marks token complete and increments join counter
  - Calls `increment_join_counter/3` to track branch completion
  - Checks if all tokens complete to determine if executor is done
- **Lines 544-550**: `select_next_token/1` selects first active token from tokens map
  - Returns `undefined` if no active tokens exist
- **Lines 338-364**: `step_normal/2` fetches and executes opcode for current token
  - Syncs IP with current token in multi-token mode (line 357)
  - Fetches opcode via `fetch_opcode/1` (line 360)

**Scheduler (wf_sched.erl)**:
- **Lines 107-117**: `select_action/2` returns mock decisions
  ```erlang
  select_action(_ExecState, PolicyOrState) when is_atom(PolicyOrState) ->
      case PolicyOrState of
          deterministic -> {token, mock_token};
          nondeterministic -> {token, mock_token};
          {replay, _} -> {token, replay_token};
          undefined -> {token, mock_token}
      end;
  ```
  - **Critical Issue**: Returns `mock_token` instead of real token_id from exec_state
  - Does not inspect ExecState to find active tokens
  - No token selection logic implemented

**Compiler (wf_compile.erl)**:
- **Lines 236-260**: `compile_par_with_metadata/1` compiles par to bytecode
  - Emits `{par_fork, BranchLabels}` (line 253)
  - Emits branch bytecode with labels and done opcodes (line 248-250)
  - Emits `{join_wait, all}` (line 256)
  - Collects task metadata from all branches (line 259)
- **Lines 214-217**: `compile_task_with_metadata/1` stores task metadata
  - Returns `{[{task_exec, Name}], #{Name => Metadata}}` (line 217)
  - Metadata includes function for runtime dispatch

**Current Token Management**:
- **Token Record** (wf_exec.hrl:7-15):
  ```erlang
  -record(token, {
      token_id :: term(),
      ip :: non_neg_integer(),
      scope_id :: term(),
      value :: term(),
      status :: active | complete | cancelled | blocked_effect | blocked_approval,
      instance_id :: term() | undefined,
      current_effect :: term() | undefined
  }).
  ```
- **Branch Info** (wf_exec.hrl:17-22): Tracks tokens belonging to a parallel fork
  - `branch_id`: Unique identifier for the fork
  - `tokens`: List of token_ids spawned by this fork
  - `join_id`: Associated join_counter id
  - `targets`: Original target IPs

**Existing Tests (wf_test_par.erl, wf_test_join.erl)**:
- **Lines 82-91**: `par_2_branches_all_complete_test/0` - Tests 2-branch parallel
  - Uses mock bytecode (no real task functions)
  - Expects `{done, ExecState1}` result
  - Verifies token count and completion status
- **Lines 126-138**: `par_wait_n_2_of_3_test/0` - Tests wait_n join policy
  - Verifies 2 completed, 1 cancelled
- **Lines 61-71**: `join_wait_all_3_test/0` - Tests all branches complete
  - Expects 4 tokens (root + 3 branches)
  - All marked as complete

### Integration Points

1. **Executor → Scheduler**: wf_exec:run/3 (line 394) calls `wf_sched:select_action(ExecState0, SchedPolicy)`
2. **Compiler → Executor**: wf_compile:compile/1 returns `{ok, {Bytecode, Metadata}}`
3. **Task Dispatch**: wf_exec:lookup_task_function/2 (lines 1084-1098) retrieves function from metadata
4. **Token Lifecycle**: PAR_FORK → spawn tokens → execute branches → DONE → increment join counter → JOIN_WAIT → continue

### Key Issues

**Issue 1: Scheduler Returns Mock Token**
- Location: wf_sched.erl:109
- Impact: Executor cannot execute opcodes for real tokens
- Symptom: parallel workflows fail to execute branch tasks

**Issue 2: No Enabled Actions Extraction**
- Scheduler doesn't extract active tokens from exec_state.tokens
- Missing logic to build `{token, TokenId}` enabled actions
- Deterministic policy should select first active token (by token_id or IP)

**Issue 3: Scheduler Decision Ignored**
- Executor calls `wf_sched:select_action/2` but doesn't use the decision
- Line 394 of wf_exec.erl: `SchedDecision = wf_sched:select_action(ExecState0, SchedPolicy)`
- Line 397: `{ExecState1, _TraceEvent} = step(ExecState0, SchedDecision)` - decision passed but not used in step_normal

**Issue 4: Token Selection After PAR_FORK**
- PAR_FORK selects next token via `select_next_token/1` (line 612)
- This is internal selection, not using scheduler
- Subsequent steps need scheduler to select among active tokens

## Key Files

- **src/wf_exec.erl:394** - Calls scheduler in run_loop/4
- **src/wf_exec.erl:354-364** - step_normal/2 uses current_token to fetch opcode
- **src/wf_exec.erl:556-621** - execute_par_fork/2 spawns branch tokens
- **src/wf_exec.erl:718-757** - execute_join_wait/2 checks join satisfaction
- **src/wf_exec.erl:972-1056** - execute_done/1 increments join counter
- **src/wf_exec.erl:544-550** - select_next_token/1 selects first active token
- **src/wf_sched.erl:107-117** - select_action/2 returns mock_token (ROOT CAUSE)
- **src/wf_compile.erl:236-260** - compile_par_with_metadata/1 generates bytecode
- **src/wf_term.erl:331-335** - par/1 smart constructor validates branch count
- **src/wf_vm.erl:54-65** - Opcode type definitions (par_fork, join_wait, done, task_exec)
- **test/wf_test_par.erl:82-91** - Existing parallel test with mock bytecode
- **test/wf_test_join.erl:61-71** - Existing join test
- **test/wf_acceptance_tests.erl:97-139** - Existing loop_count_test_0 shows pattern for acceptance tests

## Technical Considerations

### Dependencies

- **wf_term.erl**: Provides par/1 constructor and wf_term() type
- **wf_compile.erl**: Compiles par to {par_fork, [...]}, {join_wait, all} bytecode
- **wf_sched.erl**: Must be modified to select real tokens from exec_state
- **wf_exec.erl**: Must use scheduler decision to switch between tokens
- **No external dependencies**: Pure Erlang/OTP implementation

### Patterns to Follow

**From Existing Tests**:
1. **Acceptance Test Pattern** (wf_acceptance_tests.erl:24-59):
   - Build term using wf_term constructors with real functions
   - Compile with `{ok, {Bytecode, Metadata}} = wf_compile:compile(Term)`
   - Execute with `{done, ExecState} = wf_exec:run(ExecState0, 100, deterministic)`
   - Assert context contains expected keys
   - Use `#{key => value}` map syntax for context updates

2. **Test Structure**:
   - Use EUnit test_() generator for setup
   - fun() wrapper for test body
   - Assertions with ?assertMatch and ?assertEqual

**From Scheduler Architecture** (wf_sched.erl:7-9):
- **Behaviour Callback**: `choose/2 :: (EnabledActions, SchedState) -> {Chosen, NewSchedState}`
- **Enabled Actions**: `{token, token_id()} | {xor_branch, pos_integer()}`
- **Decision Type**: `sched_decision() :: {token, term()} | {xor_branch, pos_integer()}`

**From Executor Semantics** (SEMANTICS.md:380-447):
- **PAR_FORK**: Spawns N tokens, tracks in branch_map, does NOT advance IP
- **PAR-COMPLETE**: DONE opcode increments join counter, removes branch token
- **PAR-JOIN**: JOIN_WAIT creates continuation token when satisfied
- **Token Selection**: Scheduler selects which active token to execute next

### Conventions Observed in Codebase

1. **Opaque State**: Scheduler treats exec_state as opaque (wf_sched.erl:37)
2. **Record Updates**: Use `#record{field = Value}` syntax (wf_exec.erl:463-466)
3. **Map Operations**: Use `maps:get/2`, `maps:put/3`, `maps:remove/2`
4. **Error Handling**: Throw `badarg` for contract violations (wf_term.erl:303)
5. **Type Specs**: All functions have `-spec` declarations for dialyzer
6. **Module Docs**: Header doc describes module purpose and usage

### Scheduler Token Selection Requirements

**Deterministic Policy**:
- Extract all active tokens from exec_state.tokens
- Select token with lowest token_id (or earliest IP)
- Return `{token, SelectedTokenId}`

**Enabled Actions Extraction**:
```erlang
extract_enabled_actions(ExecState) ->
    ActiveTokens = [T || T <- maps:values(ExecState#exec_state.tokens),
                       T#token.status =:= active],
    [{token, T#token.token_id} || T <- ActiveTokens].
```

**Scheduler Integration**:
- Modify select_action/2 to extract enabled actions
- If single action, return it directly
- If multiple actions, use policy to choose (deterministic: sort by token_id)
- Pass ExecState (not opaque) to enable token inspection

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ------ | ------- | ----------- |
| **Scheduler breaks existing tests** | High | Existing tests use `undefined` policy and expect mock_token. Keep backward compatibility by detecting when tokens map is empty (mock mode) vs real tokens (production mode). |
| **Token selection causes non-determinism** | Medium | Deterministic policy must use stable ordering (sort by token_id). Document that token_id order is the deterministic order. |
| **Join wait never unblocks** | High | Ensure increment_join_counter/3 is called from execute_done/1 for all branch tokens. Test with 2-branch, 3-branch parallel. |
| **Context not merged correctly** | Medium | Task functions return {ok, Ctx} with updates. Executor must merge these into exec_state.ctx. Verify with acceptance test. |
| **Metadata not passed through** | Low | wf_compile returns {Bytecode, Metadata}. wf_exec:new/1 accepts both formats. Ensure Metadata is stored in exec_state.task_metadata. |
| **Scheduler decision ignored** | High | step_normal/2 must respect SchedDecision's token selection. Current code ignores it. Need to switch current_token based on decision. |

### Specific Failure Modes

1. **Mock token ID doesn't exist**: executor crashes at maps:get(CurrentToken, Tokens)
   - **Mitigation**: Check if token exists before use
   - **Detection**: Run acceptance test, observe crash

2. **Wrong token selected**: executor executes wrong branch
   - **Mitigation**: Deterministic policy uses consistent ordering
   - **Detection**: Inspect trace logs

3. **Join counter not incremented**: join_wait blocks forever
   - **Mitigation**: Verify execute_done/1 calls increment_join_counter/3
   - **Detection**: Test reaches quanta limit, returns {yield, _}

4. **Tasks not executed**: context missing x_ran, y_ran
   - **Mitigation**: lookup_task_function/2 retrieves real functions from metadata
   - **Detection**: Final context assertion fails

## Recommended Approach

### High-Level Strategy

Based on research findings, implement real parallel fork/join in three phases:

**Phase 1: Fix Scheduler Token Selection** (wf_sched.erl)
1. Modify `select_action/2` to inspect exec_state.tokens
2. Extract active tokens (status = active)
3. Return `{token, TokenId}` for selected token
4. Deterministic: select first active token (by token_id sort)
5. Nondeterministic: select random active token (future implementation)
6. Replay: use logged choice (future implementation)

**Phase 2: Integrate Scheduler Decision** (wf_exec.erl)
1. Modify `step_normal/2` to use SchedDecision's token_id
2. Update `current_token` field before fetching opcode
3. Ensure scheduler decision is respected for each step
4. Handle case where no tokens are active (return blocked)

**Phase 3: Verify End-to-End** (wf_acceptance_tests.erl)
1. Add acceptance test for parallel execution
2. Compile par([task(x), task(y)]) term
3. Execute and assert {done, FinalState}
4. Verify context contains x_ran and y_ran
5. No mocks: use real task functions

### Detailed Implementation Plan

**Step 1: Modify wf_sched:select_action/2**
```erlang
select_action(ExecState, Policy) when is_atom(Policy) ->
    %% Extract enabled actions from exec_state
    EnabledActions = extract_enabled_actions(ExecState),

    case EnabledActions of
        [] ->
            {token, undefined};  %% No active tokens
        [Single] ->
            Single;  %% Only one choice
        Multiple when Policy =:= deterministic ->
            %% Select first token by sorted token_id
            hd(lists:sort(Multiple))
    end.
```

**Step 2: Extract Enabled Actions**
```erlang
extract_enabled_actions(ExecState) ->
    Tokens = ExecState#exec_state.tokens,
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],
    [{token, T#token.token_id} || T <- ActiveTokens].
```

**Step 3: Update wf_exec:step_normal/2**
```erlang
step_normal(ExecState, {token, SelectedTokenId}) ->
    %% Update current_token from scheduler decision
    ExecState1 = ExecState#exec_state{current_token = SelectedTokenId},
    %% Continue with existing logic...
    case SelectedTokenId of
        undefined -> ...;
        _TokenId -> fetch_opcode_and_execute(ExecState1)
    end.
```

**Step 4: Add Acceptance Test**
```erlang
parallel_execution_test_() ->
    {"Parallel fork/join executes both branches",
     fun() ->
         Term = wf_term:par([
             wf_term:task(x, #{function => fun(Ctx) -> {ok, Ctx#{x_ran => true}} end}),
             wf_term:task(y, #{function => fun(Ctx) -> {ok, Ctx#{y_ran => true}} end})
         ]),
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(true, maps:get(x_ran, FinalCtx)),
         ?assertEqual(true, maps:get(y_ran, FinalCtx))
     end}.
```

### Validation Strategy

1. **Unit Tests**: Verify select_action/2 returns real token_ids
2. **Integration Tests**: Run wf_test_par.erl tests with real scheduler
3. **Acceptance Test**: Confirm parallel workflow completes with both tasks executed
4. **Regression Tests**: Ensure existing tests still pass (wf_test_seq.erl, wf_test_loop.erl)

## Open Questions

1. **Should scheduler use exec_state.opaque or inspect fields?**
   - Current design treats exec_state as opaque to scheduler
   - Required: Scheduler needs access to tokens map to extract active tokens
   - Options: (a) Pass exec_state record directly, (b) Add get_enabled_actions/1 to wf_exec API, (c) Make exec_state transparent to scheduler
   - **Recommendation**: Option (a) - pass record directly, scheduler is internal module

2. **How should deterministic policy order tokens?**
   - Options: (a) Sort by token_id, (b) Sort by IP, (c) Order of creation
   - **Recommendation**: Sort by token_id for stable ordering (make_ref() is monotonic)

3. **Should PAR_FORK use scheduler to select next token?**
   - Current implementation uses internal select_next_token/1 (line 612)
   - Could delegate to scheduler for consistency
   - **Recommendation**: Keep internal selection for PAR_FORK, use scheduler for subsequent steps

4. **What happens when all tokens are blocked?**
   - Current code returns {blocked, reason => no_active_token} (line 350)
   - Should executor yield or wait for external event?
   - **Recommendation**: Yield with {yield, ExecState} to allow external resumption

5. **Should join_wait block executor or check asynchronously?**
   - Current implementation sets status = blocked_join (line 754)
   - Requires step_check_join/2 to poll for completion
   - **Recommendation**: Current polling approach is correct for cooperative scheduling

6. **Backward compatibility with mock bytecode?**
   - Old tests pass plain bytecode list without metadata
   - wf_exec:new/1 handles both formats (lines 53-85)
   - **Recommendation**: Keep backward compatibility, detect metadata presence
