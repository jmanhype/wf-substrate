# Fix the 2 remaining test failures in test/wf_case_runner_tests.erl: Implementation Plan

## Implementation Plan Title

Fix State Transition and Status Reporting in wf_case_runner gen_statem

## Overview

This plan fixes two bugs in the `wf_case_runner` gen_statem implementation that cause test failures:

1. **State Transition Bug**: The `enter_state/2` function returns a plain `StateData` record instead of a gen_statem tuple, preventing proper state transitions from `running` to `done`.
2. **Status Map Inconsistency**: The `done/3` state handler returns a status map missing `ip` and `step_count` keys that tests expect.

These bugs prevent workflow cases from completing properly and cause the `status_query_test` to fail when querying the `ip` key.

## Current State

**Test Failures:**
- `running_to_done_completion_test/1` (line 46-56): Expects case to reach `done` state within 100ms, but it remains in `running` state indefinitely
- `status_query_test/1` (line 82-88): Expects status map to contain `ip` and `step_count` keys, but `done/3` handler returns map with only `state` and `result`

**Root Causes:**

**Issue 1: `enter_state/2` Returns Wrong Type** (`src/wf_case_runner.erl:391-401`)
```erlang
enter_state(TargetState, StateData) ->
    case TargetState of
        done -> StateData;        %% Returns StateData instead of {next_state, done, StateData}
        cancelled -> StateData;   %% Returns StateData instead of {next_state, cancelled, StateData}
        _Other -> StateData
    end.
```

The function is called from `execute_quantum/1` (lines 358, 365) when execution completes. The return value is used directly as the gen_statem return value:
```erlang
running(cast, {signal, Signal}, StateData) ->
    ...
    {next_state, running, execute_quantum(StateData)}.
```

When `execute_quantum/1` calls `enter_state(done, StateData1)`, it returns `StateData1` (a record), but the caller expects a gen_statem tuple like `{next_state, done, StateData1}`. This causes the gen_statem to remain in the `running` state instead of transitioning to `done`.

**Issue 2: `done/3` Status Handler Missing Fields** (`src/wf_case_runner.erl:295-298`)
```erlang
done({call, From}, status, StateData) ->
    StatusInfo = #{state => done, result => StateData#state_data.result},
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]}.
```

The `running/3` state handler (lines 205-214) returns a richer status map:
```erlang
running({call, From}, status, StateData) ->
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => running,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status
    },
    {keep_state_and_data, [{reply, From, {ok, running, StatusInfo}}]}.
```

The test at line 86 asserts `maps:is_key(ip, StatusInfo)`, which fails when the case is in the `done` state because the status map lacks the `ip` key.

## Desired End State

**Functional Requirements:**
1. Workflow cases executing the simple task bytecode (`[{'TASK_EXEC', mock_task}, {'DONE'}]`) transition from `running` to `done` state upon completion
2. Status queries return consistent map structure across all states (`running`, `done`, `cancelled`)
3. All 5 tests in `wf_case_runner_tests` pass:
   - `running_to_done_completion_test`
   - `signal_delivery_test`
   - `cancel_test`
   - `status_query_test`
   - `trace_configuration_test`

**Verification:**
```bash
rebar3 compile
rebar3 eunit --module=wf_case_runner_tests
```

Expected output: All tests pass with 0 failures.

### Key Discoveries:

- **`execute_quantum/1` Return Type is Wrong** (`src/wf_case_runner.erl:340`): The spec says it returns `state_data()`, but it actually returns gen_statem tuples like `{next_state, waiting_effect, StateData, Actions}` in the yield case (lines 376, 380). This is actually correct for the callers, but the spec needs updating.

- **`enter_state/2` is a State Transition Helper**: The function is designed to abstract state transitions, but it's currently implemented as a no-op that always returns `StateData`. It should return gen_statem tuples for state changes.

- **Status Map Consistency Pattern**: The `handle_common_event/3` function (lines 308-316) provides a fallback status handler that returns consistent maps with `ip`, `step_count`, and `status` keys. State-specific handlers should follow this pattern.

- **Test Timing is Sufficient**: The mock task completes immediately (no I/O or blocking operations), so 100ms is more than adequate for the case to complete. The failure is due to the state transition bug, not timing.

## What We're NOT Doing

1. **Fixing the function spec for `execute_quantum/1`**: While the spec is technically wrong (it says `state_data()` but returns gen_statem tuples), fixing the spec is not required to make the tests pass. The spec can be updated in a separate cleanup task.

2. **Refactoring status query handling**: Currently there are three places where status queries are handled:
   - `running/3` (lines 205-214)
   - `done/3` (lines 295-298)
   - `cancelled/3` (lines 282-285)
   - `handle_common_event/3` (lines 308-316, fallback)

   We're NOT consolidating these into a single handler. We're only fixing the `done/3` handler to return the required fields.

3. **Adding automatic termination for completed cases**: The `done` state currently keeps the process alive (returns `{keep_state_and_data, ...}`). We're NOT changing this behavior to `{stop, normal, ...}` as that's a design decision outside the scope of this bug fix.

4. **Fixing ETS table name collision risk**: The `trace_configuration_test` uses table name `wf_trace_events`. If multiple tests run concurrently, there could be ETS table name conflicts. This is a known issue but not causing test failures currently, so it's out of scope.

5. **Implementing retry logic or explicit synchronization**: The 100ms sleep in tests is sufficient for the mock task. We're NOT adding complex synchronization primitives.

## Implementation Approach

The fix consists of two minimal changes to `src/wf_case_runner.erl`:

**Phase 1: Fix `enter_state/2` to return gen_statem tuples**
- Change the function to return `{next_state, TargetState, StateData}` for state transitions
- Keep the function signature the same
- This fixes the root cause of `running_to_done_completion_test` failure

**Phase 2: Fix `done/3` status handler to include `ip` and `step_count`**
- Extract fields from `ExecState` in `StateData`
- Add `ip`, `step_count`, and `status` to the status map
- Keep the `result` field for backward compatibility
- This fixes the `status_query_test` failure

The fixes are minimal, targeted, and follow existing patterns in the codebase. Phase 1 must be completed before Phase 2, but both phases can be tested independently.

---

## Phases

### Phase 1: Fix State Transition in `enter_state/2`

#### Overview

Fix the `enter_state/2` function to return proper gen_statem tuples instead of plain `StateData` records. This allows the gen_statem to transition from `running` to `done` state when execution completes.

#### Changes Required:

##### 1. Update `enter_state/2` function

**File**: `src/wf_case_runner.erl:391-401`

**Current Implementation:**
```erlang
%% @doc Transition to new state
enter_state(TargetState, StateData) ->
    %% Set state timeout if not done/cancelled
    case TargetState of
        done ->
            StateData;
        cancelled ->
            StateData;
        _Other ->
            StateData
    end.
```

**New Implementation:**
```erlang
%% @doc Transition to new state
enter_state(done, StateData) ->
    {next_state, done, StateData};
enter_state(cancelled, StateData) ->
    {next_state, cancelled, StateData};
enter_state(_Other, StateData) ->
    {keep_state, StateData}.
```

**Rationale:**
- The function is called from `execute_quantum/1` when execution completes (lines 358, 365)
- The return value is used as the gen_statem return value in state handlers
- Must return `{next_state, TargetState, StateData}` for actual state transitions
- Must return `{keep_state, StateData}` for staying in current state
- Pattern matching on `TargetState` is clearer than case statement

#### Success Criteria:

##### Automated Verification:

- [ ] Test passes: `rebar3 eunit --module=wf_case_runner_tests --test=running_to_done_completion_test`
- [ ] Compilation succeeds: `rebar3 compile`
- [ ] No dialyzer warnings: `rebar3 dialyzer`

##### Manual Verification:

- [ ] Inspect test output to confirm case reaches `done` state
- [ ] Verify no other tests are broken by the change
- [ ] Confirm state transition happens within 100ms timeout

**Note**: After this phase, `running_to_done_completion_test` should pass, but `status_query_test` may still fail (that's Phase 2).

---

### Phase 2: Fix Status Map Consistency in `done/3`

#### Overview

Update the `done/3` state handler to return a status map with the same structure as other state handlers, including `ip`, `step_count`, and `status` fields.

#### Changes Required:

##### 1. Update `done/3` status query handler

**File**: `src/wf_case_runner.erl:295-298`

**Current Implementation:**
```erlang
done({call, From}, status, StateData) ->
    %% Return done status with result
    StatusInfo = #{state => done, result => StateData#state_data.result},
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]}.
```

**New Implementation:**
```erlang
done({call, From}, status, StateData) ->
    %% Return done status with result and execution info
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => done,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status,
        result => StateData#state_data.result
    },
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]}.
```

**Rationale:**
- Aligns with `running/3` handler (lines 205-214) which includes `ip`, `step_count`, `status`
- Maintains backward compatibility by keeping `result` field
- Test at line 86 checks `maps:is_key(ip, StatusInfo)`
- Test at line 87 checks `maps:is_key(step_count, StatusInfo)`
- Pattern matches the fallback `handle_common_event/3` structure (lines 308-316)

#### Success Criteria:

##### Automated Verification:

- [ ] Test passes: `rebar3 eunit --module=wf_case_runner_tests --test=status_query_test`
- [ ] Full test suite passes: `rebar3 eunit --module=wf_case_runner_tests`
- [ ] All 5 tests in module pass:
  - `running_to_done_completion_test`
  - `signal_delivery_test`
  - `cancel_test`
  - `status_query_test`
  - `trace_configuration_test`

##### Manual Verification:

- [ ] Verify status query returns map with all expected keys: `state`, `ip`, `step_count`, `status`, `result`
- [ ] Confirm no regressions in other tests
- [ ] Check that `ip` value is correct (should be 2 after executing both opcodes)
- [ ] Check that `step_count` value is correct (should be 2)

**Note**: After this phase, both failing tests should pass and the full test suite should succeed.

---

## Testing Strategy

### Unit Tests:

The existing EUnit tests in `test/wf_case_runner_tests.erl` provide coverage for the fixes:

1. **`running_to_done_completion_test/1`**: Verifies state transition from `running` to `done`
   - Tests Phase 1 fix
   - Waits 100ms for completion
   - Queries status and expects `{ok, done, StatusInfo}`

2. **`status_query_test/1`**: Verifies status map contains required keys
   - Tests Phase 2 fix
   - Queries status immediately (case may still be in `running` or `done`)
   - Asserts `ip` and `step_count` keys exist

3. **`signal_delivery_test/1`**: Regression test for signal handling
   - Ensures Phase 1 changes don't break cast event handling

4. **`cancel_test/1`**: Regression test for cancellation
   - Ensures Phase 1 changes don't break cancel flow
   - Tests both cancel from `running` and `enter_state(cancelled, ...)` path

5. **`trace_configuration_test/1`**: Regression test for trace configuration
   - Ensures changes don't break trace state updates

### Integration Tests:

Run the full test suite to ensure no regressions:

```bash
rebar3 eunit
```

This will execute all test modules and catch any unintended side effects.

### Manual Testing Steps:

1. **Verify state transition timing:**
   - Start a wf_case_runner with simple task bytecode
   - Query status immediately (should be `running`)
   - Query status after 100ms (should be `done`)
   - Confirm transition happens within timeout

2. **Verify status map consistency:**
   - Query status in `running` state, check keys: `state`, `ip`, `step_count`, `status`
   - Query status in `done` state, check keys: `state`, `ip`, `step_count`, `status`, `result`
   - Query status in `cancelled` state, check keys: `state`
   - Verify `ip` increments correctly (0 → 1 → 2)
   - Verify `step_count` increments correctly (0 → 1 → 2)

3. **Verify error handling:**
   - Start case with invalid bytecode (should fail gracefully)
   - Cancel case during execution (should reach `cancelled` state)
   - Timeout case (should transition to `cancelled` with timeout reason)

## Migration Notes

No data migration or API changes required. The fixes are internal implementation details that don't affect the external API:

- `wf_case_runner:start_link/3` - unchanged
- `wf_case_runner:signal/2` - unchanged
- `wf_case_runner:cancel/1` - unchanged
- `wf_case_runner:set_trace/2` - unchanged
- `wf_case_runner:status/1` - unchanged (still returns `{ok, StateName, StatusInfo}`)

The only visible change is that status maps now include additional fields (`ip`, `step_count`, `status`) in the `done` state, which is backward compatible (adding fields to a map doesn't break existing code).

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/031-fix-the-2-remaining-test-failures-in-testwfcaserun/research.md`
- Test file: `/Users/speed/wf-substrate/test/wf_case_runner_tests.erl`
- Implementation: `/Users/speed/wf-substrate/src/wf_case_runner.erl`
- Related: Item 025 (fixed scheduler policy selection in `wf_sched:select_action/2`)

### Key Code Locations:

- **Bug 1**: `src/wf_case_runner.erl:391-401` - `enter_state/2` function
- **Bug 2**: `src/wf_case_runner.erl:295-298` - `done/3` status handler
- **Test 1**: `test/wf_case_runner_tests.erl:46-56` - `running_to_done_completion_test/1`
- **Test 2**: `test/wf_case_runner_tests.erl:82-88` - `status_query_test/1`
- **Caller**: `src/wf_case_runner.erl:340-389` - `execute_quantum/1` function
- **Pattern**: `src/wf_case_runner.erl:205-214` - `running/3` status handler (correct pattern)
