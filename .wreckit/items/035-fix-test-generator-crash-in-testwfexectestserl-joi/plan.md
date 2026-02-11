# Fix test generator crash in test/wf_exec_tests.erl: join_wait_test_/0 fails with badmatch error. The wf_exec:step/2 function returns a tuple that does not match the expected {ExecState, Trace} pattern — likely the PAR_FORK or JOIN_WAIT opcodes return additional data. Check the actual return type of wf_exec:step/2 for parallel opcodes and fix the test to match. The 5 cancelled tests in rebar3 eunit are from this generator crash. Target: 0 failures, 0 cancelled. Implementation Plan

## Implementation Plan Title

Fix Multi-Token Executor IP Desynchronization and blocked_join Handling

## Overview

The multi-token executor in `wf_exec` has a critical architecture bug where `ExecState.ip` (the global instruction pointer) becomes desynchronized from the current token's IP during parallel execution. This causes the executor to fetch and execute wrong instructions, leading to badmatch errors and test failures.

Additionally, the `blocked_join` status is not handled by the `step/2` function, causing the executor to attempt execution when it should be blocked waiting for parallel branches to complete.

This fix ensures that:
1. `ExecState.ip` is synchronized with the current token's IP before fetching opcodes
2. The `blocked_join` status is properly handled with appropriate state checks
3. Defensive error handling prevents crashes on edge cases (empty join counters, undefined current token)

## Current State

### What Exists Now

**Single-Token Executor** (working correctly)
- Sequential execution through bytecode
- `ExecState.ip` tracks current instruction pointer
- Tests passing: `single_task_test_`, `sequence_test_`, `done_test_`

**Multi-Token Executor** (broken)
- Parallel execution spawns multiple tokens at different IPs
- Each token has its own `ip` field in the token record
- `ExecState.ip` should track the current token's IP but doesn't
- Token switching occurs in `execute_par_fork/2`, `execute_mi_spawn/2`, and `execute_done/1`
- Tests failing: `par_fork_test_`, `join_wait_test_`, and 5 MI tests

### Root Causes Identified

**1. IP Desynchronization** (High Impact)
- When `execute_done/1` switches tokens (lines 804, 849), it updates `current_token` but NOT `ExecState.ip`
- `fetch_opcode/2` uses `ExecState.ip` to fetch instructions, not the current token's IP
- Result: Executor executes wrong opcodes, leading to state corruption and badmatch errors

**2. Missing blocked_join Handler** (High Impact)
- `execute_join_wait/2` sets status to `blocked_join` when join not satisfied (line 600)
- `step/2` only checks for `blocked_effect` status (line 154)
- Result: Executor continues execution when it should be blocked

**3. Missing Defensive Guards** (Medium Impact)
- `find_active_join/1` assumes non-empty join_counters (line 610)
- No handling for undefined `current_token` in `step_normal/2`
- Result: Badmatch errors when accessing empty maps

### Key Constraints

- Must maintain backward compatibility with single-token executor
- Must not break existing passing tests
- Must follow existing Erlang/OTP patterns
- Must preserve the `wf_exec:step/2` return type signature: `{exec_state(), map()}`

## Desired End State

### Specification

1. **IP Synchronization**: `ExecState.ip` always matches the current token's IP before opcode execution
2. **blocked_join Handling**: Executor properly blocks on JOIN_WAIT until all branches complete
3. **Defensive Error Handling**: Graceful handling of edge cases without crashes
4. **Test Success**: All 6 cancelled/failing tests pass

### Verification

Run `rebar3 eunit` and verify:
- **0 failures** (currently 0)
- **0 cancelled** (currently 5 cancelled from `join_wait_test_` generator crash)
- **Total tests**: All existing tests still pass
- **Specific test**: `join_wait_test_/0` completes all 6 steps successfully

### Key Discoveries:

- **wf_exec.erl:263-269**: `fetch_opcode/2` uses `ExecState.ip` NOT `token.ip` - this is the core design constraint
- **wf_exec.erl:804-810, 849-853**: Token switching points that don't sync IP
- **wf_exec.erl:152-158**: `step/2` only handles `blocked_effect`, missing `blocked_join` case
- **wf_exec.erl:606-611**: `find_active_join/1` will badmatch on empty join_counters
- **test/wf_exec_tests.erl:109-125**: The failing test expects 6 successful steps in parallel execution

## What We're NOT Doing

- **NOT changing the `wf_exec:step/2` function signature** - it must return `{exec_state(), map()}`
- **NOT refactoring the entire executor architecture** - only fixing the specific bugs
- **NOT changing single-token executor behavior** - only fixing multi-token executor
- **NOT modifying bytecode format** - executor fixes only
- **NOT changing other modules** (wf_sched, wf_cancel, wf_mi, wf_effect) - only wf_exec.erl

## Implementation Approach

The fix uses a **defense-in-depth** strategy:

### Primary Fix: IP Synchronization in step_normal/2

Before fetching any opcode, sync `ExecState.ip` with the current token's IP. This ensures correct instruction fetching regardless of how we got to the current token.

**Rationale**: Centralized fix handles all token switching cases (PAR_FORK, MI_SPAWN, DONE). Adds minimal overhead (one record lookup and field update per step).

### Secondary Fix: Add blocked_join Handler

Add `blocked_join` case to `step/2` that checks if join counters are satisfied. If satisfied, create continuation token and unblock. If not, remain blocked.

**Rationale**: Follows existing pattern from `step_check_effect/2`. Ensures executor doesn't continue when waiting for parallel branches.

### Tertiary Fix: Defensive Guards

Add guards for empty join_counters and undefined current_token to prevent badmatch errors.

**Rationale**: Makes executor robust against edge cases. Provides clear error messages for debugging.

### Implementation Order

1. Phase 1: Add IP sync helper and integrate into `step_normal/2` (fixes 90% of issues)
2. Phase 2: Add `blocked_join` handler (fixes remaining 10%)
3. Phase 3: Add defensive guards (prevents future edge case crashes)
4. Phase 4: Verify all tests pass (validation)

---

## Phases

### Phase 1: Fix IP Synchronization in step_normal/2

#### Overview

Add helper function to sync `ExecState.ip` with current token's IP, and call it in `step_normal/2` before fetching opcodes. This ensures correct instruction fetching during multi-token execution.

#### Changes Required:

##### 1. Add IP Sync Helper Function

**File**: `src/wf_exec.erl`
**Location**: After line 221 (after `step_normal/2` function)
**Changes**: Add new helper function to synchronize IP with current token

```erlang
%% @private Sync ExecState.ip with current token's IP
%% This is necessary in multi-token execution where each token has its own IP.
-spec sync_ip_with_current_token(exec_state()) -> exec_state().
sync_ip_with_current_token(#exec_state{current_token = undefined} = ExecState) ->
    %% No current token, keep IP as-is (will be handled elsewhere)
    ExecState;
sync_ip_with_current_token(#exec_state{current_token = CurrentToken, tokens = Tokens} = ExecState) ->
    Token = maps:get(CurrentToken, Tokens),
    ExecState#exec_state{ip = Token#token.ip}.
```

##### 2. Modify step_normal/2 to Sync IP

**File**: `src/wf_exec.erl`
**Location**: Lines 217-221
**Changes**: Add IP sync before fetching opcode

```erlang
%% @private Normal step (not blocked)
step_normal(ExecState, SchedDecision) ->
    %% Sync IP with current token before fetching opcode (critical for multi-token execution)
    SyncedExecState = sync_ip_with_current_token(ExecState),
    Opcode = fetch_opcode(SyncedExecState),
    NewExecState = execute_opcode(Opcode, SyncedExecState, SchedDecision),
    TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, TraceEvent}.
```

##### 3. Handle Undefined current_token

**File**: `src/wf_exec.erl`
**Location**: Lines 217-221 (replace existing `step_normal/2`)
**Changes**: Add guard for undefined current_token

```erlang
%% @private Normal step (not blocked)
step_normal(ExecState, SchedDecision) ->
    case ExecState#exec_state.current_token of
        undefined ->
            %% No active tokens available
            case maps:values(ExecState#exec_state.tokens) of
                [] ->
                    %% All tokens complete, mark as done
                    ExecState#exec_state{status = done};
                _ ->
                    %% Tokens exist but none are active (shouldn't happen)
                    ExecState#exec_state{status = blocked}
            end;
        _TokenId ->
            %% Sync IP with current token before fetching opcode
            SyncedExecState = sync_ip_with_current_token(ExecState),
            Opcode = fetch_opcode(SyncedExecState),
            NewExecState = execute_opcode(Opcode, SyncedExecState, SchedDecision),
            TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
            {NewExecState, TraceEvent}
    end.
```

**Wait - the second approach has a bug** - it needs to return a tuple. Let me fix that:

```erlang
%% @private Normal step (not blocked)
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
            %% Sync IP with current token before fetching opcode
            SyncedExecState = sync_ip_with_current_token(ExecState),
            Opcode = fetch_opcode(SyncedExecState),
            NewExecState = execute_opcode(Opcode, SyncedExecState, SchedDecision),
            TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
            {NewExecState, TraceEvent}
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Test `par_fork_test_` passes: `rebar3 eunit --module=wf_exec_tests --test=par_fork_test_`
- [ ] Test `join_wait_test_` no longer crashes: `rebar3 eunit --module=wf_exec_tests --test=join_wait_test_`
- [ ] Compilation succeeds: `rebar3 compile`
- [ ] No new warnings: `rebar3 compile` output shows 0 warnings

##### Manual Verification:

- [ ] Inspect `wf_exec:step/2` return values during parallel execution
- [ ] Verify `ExecState.ip` matches current token's IP after each step
- [ ] Confirm no badmatch errors in test output

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Handle blocked_join Status

#### Overview

Add handler for `blocked_join` status in `step/2`. When executor is blocked waiting for parallel branches to complete, check if join counters are satisfied. If satisfied, create continuation token and unblock. If not, remain blocked.

#### Changes Required:

##### 1. Add blocked_join Case to step/2

**File**: `src/wf_exec.erl`
**Location**: Lines 152-158
**Changes**: Add `blocked_join` case to dispatch to new handler

```erlang
%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), map()}.
step(ExecState, SchedDecision) ->
    case ExecState#exec_state.status of
        blocked_effect ->
            step_check_effect(ExecState, SchedDecision);
        blocked_join ->
            step_check_join(ExecState, SchedDecision);
        _ ->
            step_normal(ExecState, SchedDecision)
    end.
```

##### 2. Create step_check_join/2 Function

**File**: `src/wf_exec.erl`
**Location**: After line 214 (after `step_check_effect/2` function)
**Changes**: Add new handler function for blocked_join status

```erlang
%% @private Check join completion (blocked_join path)
step_check_join(ExecState, _SchedDecision) ->
    case maps:keys(ExecState#exec_state.join_counters) of
        [] ->
            %% No join counters (shouldn't happen), unblock
            {ExecState#exec_state{status = running}, #{type => join_unblocked, reason => no_joins}};
        _ ->
            %% Check if any join is satisfied
            JoinId = find_active_join(ExecState),
            JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),

            case JoinCounter#join_counter.completed >= JoinCounter#join_counter.required of
                true ->
                    %% Join satisfied, create continuation token
                    JoinCounters = maps:remove(JoinId, ExecState#exec_state.join_counters),
                    BranchMap = remove_branch_by_join(JoinId, ExecState#exec_state.branch_map),

                    ContinuationTokenId = make_ref(),
                    ContinuationToken = #token{
                        token_id = ContinuationTokenId,
                        ip = ExecState#exec_state.ip + 1,
                        scope_id = get_current_scope(ExecState),
                        value = merge_results(JoinCounter#join_counter.results, JoinCounter#join_counter.policy),
                        status = active,
                        instance_id = undefined
                    },
                    Tokens = maps:put(ContinuationTokenId, ContinuationToken, ExecState#exec_state.tokens),

                    TraceEvent = #{
                        type => join_complete,
                        join_id => JoinId,
                        results => JoinCounter#join_counter.results
                    },
                    {ExecState#exec_state{
                        ip = ExecState#exec_state.ip + 1,
                        tokens = Tokens,
                        join_counters = JoinCounters,
                        branch_map = BranchMap,
                        status = running,
                        current_token = ContinuationTokenId
                    }, TraceEvent};
                false ->
                    %% Join not satisfied, remain blocked
                    TraceEvent = #{
                        type => blocked_join,
                        join_id => JoinId,
                        completed => JoinCounter#join_counter.completed,
                        required => JoinCounter#join_counter.required
                    },
                    {ExecState, TraceEvent}
            end
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Test `join_wait_test_` passes completely: `rebar3 eunit --module=wf_exec_tests --test=join_wait_test_`
- [ ] Test completes 6 steps successfully (assertion on line 124 passes)
- [ ] All MI tests pass: `rebar3 eunit --module=wf_exec_tests --test=mi_execution_wait_all_test_`
- [ ] No crashes or badmatch errors

##### Manual Verification:

- [ ] Trace through execution to verify JOIN_WAIT blocks until branches complete
- [ ] Confirm continuation token created with correct IP after join satisfied
- [ ] Verify executor transitions from `blocked_join` to `running` correctly

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Add Defensive Guards

#### Overview

Add defensive error handling to prevent crashes on edge cases:
1. Guard for empty join_counters in `find_active_join/1`
2. Ensure proper error messages for debugging

#### Changes Required:

##### 1. Guard find_active_join/1 Against Empty join_counters

**File**: `src/wf_exec.erl`
**Location**: Lines 606-611
**Changes**: Add guard clause to handle empty join_counters

```erlang
%% @doc Find active join point
-spec find_active_join(exec_state()) -> term().
find_active_join(#exec_state{join_counters = JoinCounters}) ->
    case maps:keys(JoinCounters) of
        [] ->
            error({no_active_join, join_counters_empty});
        [JoinId | _] ->
            JoinId
    end.
```

**Note**: This error is intentional - it indicates a logic error if we try to find a join when none exist. The `step_check_join/2` function already handles the empty case before calling this.

#### Success Criteria:

##### Automated Verification:

- [ ] All wf_exec_tests pass: `rebar3 eunit --module=wf_exec_tests`
- [ ] 0 failures, 0 cancelled
- [ ] No badmatch errors related to empty join_counters

##### Manual Verification:

- [ ] Verify error message is clear if join_counters is empty unexpectedly
- [ ] Confirm normal execution path doesn't hit this error

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Comprehensive Test Validation

#### Overview

Run full test suite to ensure all tests pass and no regressions were introduced.

#### Changes Required:

No code changes - validation phase only.

#### Success Criteria:

##### Automated Verification:

- [ ] All wf_exec_tests pass: `rebar3 eunit --module=wf_exec_tests`
  - Expected: All 30+ tests pass
  - Verify: 0 failures, 0 cancelled
- [ ] Full test suite passes: `rebar3 eunit`
  - Verify: No regressions in other modules
- [ ] Compilation succeeds: `rebar3 compile`
- [ ] No warnings: `rebar3 compile` shows 0 warnings

##### Manual Verification:

- [ ] Review test output summary
- [ ] Confirm specific tests that were cancelled now pass:
  - `join_wait_test_`
  - `mi_execution_wait_all_test_`
  - `mi_execution_wait_n_test_`
  - `mi_execution_first_complete_test_`
  - Any other tests that were cascading failures
- [ ] Verify single-token executor tests still pass (no regressions)

**Note**: This is the final validation phase. Once complete, the fix is ready for commit.

---

## Testing Strategy

### Unit Tests:

The existing test suite provides comprehensive coverage:

**Single-Token Executor Tests** (should not be affected):
- `new_test_` - executor creation
- `single_task_test_` - single task execution
- `done_test_` - DONE opcode
- `sequence_test_` - SEQ_ENTER/SEQ_NEXT
- `quanta_yield_test_` - quanta exhaustion
- `run_until_done_test_` - completion

**Multi-Token Executor Tests** (should now pass):
- `par_fork_test_` - parallel fork
- `join_wait_test_` - parallel join (currently crashes)
- `xor_choose_test_` - exclusive choice

**Multiple Instance Tests** (should now pass):
- `mi_spawn_test_` - MI spawning
- `mi_spawn_fire_and_forget_test_` - MI without join
- `mi_execution_wait_all_test_` - MI wait_all policy
- `mi_execution_wait_n_test_` - MI wait_n policy
- `mi_execution_first_complete_test_` - MI first_complete policy

**Loop Tests** (should not be affected):
- `loop_count_test_` - count-based loop
- `loop_back_jump_test_` - LOOP_BACK opcode

**Cancellation Tests** (should not be affected):
- `cancel_scope_test_` - scope enter/exit
- `nested_cancel_scope_test_` - nested scopes

**Effect Tests** (should not be affected):
- `effect_yield_test_` - effect yielding

**Scheduler Tests** (should not be affected):
- `scheduler_integration_test_` - scheduler integration

### Integration Tests:

The multi-token executor integrates with:
- **wf_sched**: Scheduler selects which action to take (not token selection)
- **wf_cancel**: Cancellation propagation (not affected by this fix)
- **wf_mi**: Multiple instance management (benefits from IP sync)
- **wf_effect**: Effect yielding (not affected by this fix)

### Manual Testing Steps:

1. **Before Fix**: Run `rebar3 eunit --module=wf_exec_tests` to capture baseline failures
2. **After Phase 1**: Verify `par_fork_test_` passes and `join_wait_test_` doesn't crash immediately
3. **After Phase 2**: Verify `join_wait_test_` completes all 6 steps
4. **After Phase 3**: Verify all MI tests pass
5. **Final**: Run full test suite and verify 0 failures, 0 cancelled

## Migration Notes

### Data Migration

No data migration required - this is a pure bug fix with no API changes.

### API Compatibility

The fix maintains 100% API compatibility:
- `wf_exec:step/2` signature unchanged: `exec_state() -> {exec_state(), map()}`
- All exported functions unchanged
- Record definitions unchanged (fields only modified internally)

### Rollback Strategy

If issues arise:
1. Revert `src/wf_exec.erl` to previous version
2. Tests will revert to current failing state
3. No data corruption or persistent state issues (executor is stateless)

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/035-fix-test-generator-crash-in-testwfexectestserl-joi/research.md`
- Source: `src/wf_exec.erl:151-158` - `step/2` function entry point
- Source: `src/wf_exec.erl:216-221` - `step_normal/2` function
- Source: `src/wf_exec.erl:263-269` - `fetch_opcode/2` uses `ExecState.ip`
- Source: `src/wf_exec.erl:391-397` - `select_next_token/1` function
- Source: `src/wf_exec.erl:404-468` - `execute_par_fork/2` spawns tokens
- Source: `src/wf_exec.erl:565-603` - `execute_join_wait/2` handles joins
- Source: `src/wf_exec.erl:771-855` - `execute_done/1` handles token completion
- Source: `src/wf_exec.erl:606-611` - `find_active_join/1` assumes non-empty
- Header: `src/wf_exec.hrl:7-44` - Record definitions
- Test: `test/wf_exec_tests.erl:109-125` - `join_wait_test_/0` failing test
- Test: `test/wf_exec_tests.erl:12-20` - `mock_bytecode_par/0` test bytecode
