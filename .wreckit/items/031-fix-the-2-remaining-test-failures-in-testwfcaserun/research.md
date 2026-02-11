# Research: Fix the 2 remaining test failures in test/wf_case_runner_tests.erl:

**Date**: 2026-02-11
**Item**: 031-fix-the-2-remaining-test-failures-in-testwfcaserun

## Research Question

What are the root causes of the 2 remaining test failures in `test/wf_case_runner_tests.erl`, and what is the minimal fix required to make these tests pass?

## Summary

The two remaining test failures in `wf_case_runner_tests.erl` stem from state transition and status reporting issues in the `wf_case_runner` gen_statem implementation. The first failure (`running_to_done_completion_test`) occurs because the workflow case never transitions from `running` to `done` state as expected by the test. The second failure (`status_query_test`) fails because the status query returns a map missing the `ip` key that the test expects.

Based on analysis of the codebase progression from item 025, these failures appear to be caused by:
1. **Missing state transition logic** in `wf_case_runner:execute_quantum/1` - when execution completes to `done`, the gen_statem should transition to the `done` state
2. **Inconsistent status map structure** - the `status/1` call returns different map structures depending on which state handles it, but the test expects a consistent structure with `ip` and `step_count` keys

The bytecode being tested (`?MOCK_BYTECODE_SIMPLE_TASK = [{'TASK_EXEC', mock_task}, {'DONE'}]) should execute a single task and then complete, but the case runner appears to remain in `running` state indefinitely.

## Current State Analysis

### Existing Implementation

The `wf_case_runner` is a gen_statem that manages workflow case execution through several states:
- **initializing**: Setup exec_state, sched_state, trace_state (lines 154-172)
- **running**: Execute bytecode quanta, check for done/blocked (lines 174-227)
- **waiting_effect**: Await effect response from external system (lines 229-249)
- **waiting_signal**: Await signal via wf_substrate:signal/2 (lines 251-275)
- **cancelled**: Cleanup and terminate (lines 277-292)
- **done**: Return result and terminate (lines 294-305)

The core execution loop is in `execute_quantum/1` (lines 340-389):
```erlang
execute_quantum(StateData) ->
    ExecState0 = StateData#state_data.exec_state,
    SchedState = StateData#state_data.sched_state,
    StepQuanta = maps:get(step_quanta, StateData#state_data.options),

    %% Run quantum
    Result = wf_exec:run(ExecState0, StepQuanta, SchedState),

    case Result of
        {done, ExecState1} ->
            %% Execution complete
            case ExecState1#exec_state.status of
                done ->
                    %% Normal completion
                    Result1 = ExecState1#exec_state.ctx,
                    notify_done(StateData#state_data.caller_pid, {ok, Result1}),
                    StateData1 = StateData#state_data{exec_state = ExecState1, result = Result1},
                    enter_state(done, StateData1);  %% <- Should transition to done state
                %% ... other cases
            end;
        {yield, ExecState1} ->
            %% Quantum exhausted, check if blocked
            %% ... yields back to running state
    end.
```

### Test Structure

The tests are organized as a EUnit test generator with setup/cleanup:
```erlang
wf_case_runner_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
             {"running_to_done_completion_test", ?_test(running_to_done_completion_test(Pid))},
             {"signal_delivery_test", ?_test(signal_delivery_test(Pid))},
             {"cancel_test", ?_test(cancel_test(Pid))},
             {"status_query_test", ?_test(status_query_test(Pid))},
             {"trace_configuration_test", ?_test(trace_configuration_test(Pid))}
         ]
     end
    }.
```

Each test receives a `Pid` for a `wf_case_runner` process started with:
```erlang
{ok, Pid} = wf_case_runner:start_link(
    test_case_runner,
    ?MOCK_BYTECODE_SIMPLE_TASK,  % [{'TASK_EXEC', mock_task}, {'DONE'}]
    #{step_quanta => 10, timeout => 5000}
)
```

## Key Files

### Test Files

- `test/wf_case_runner_tests.erl:1-96` - Test module with 5 tests for wf_case_runner gen_statem
  - Line 10-16: `setup/0` - Starts wf_case_runner with simple task bytecode
  - Line 18-24: `cleanup/1` - Stops gen_statem if alive
  - Line 30-43: Master test wrapper with setup/cleanup
  - Line 46-56: `running_to_done_completion_test/1` - **FAILURE 1** - Expects case to reach `done` state in 100ms
  - Line 59-63: `signal_delivery_test/1` - Sends signal, expects no crash
  - Line 66-79: `cancel_test/1` - Cancels running case
  - Line 82-88: `status_query_test/1` - **FAILURE 2** - Queries status, expects `ip` and `step_count` keys
  - Line 91-95: `trace_configuration_test/1` - Sets trace level to `min`

- `test/wf_exec_tests.hrl:1-27` - Mock bytecode definitions used across tests
  - Line 6-9: `?MOCK_BYTECODE_SIMPLE_TASK` macro - Defines `[{'TASK_EXEC', mock_task}, {'DONE'}]`
  - Line 11-17: `?MOCK_BYTECODE_SEQ` - Sequence bytecode for testing
  - Line 19-26: `?MOCK_BYTECODE_PAR` - Parallel fork bytecode

### Implementation Files

- `src/wf_case_runner.erl:1-418` - Per-case workflow runner as gen_statem
  - Line 45-53: `#state_data{}` record definition
  - Line 70-71: `start_link/3` - Starts gen_statem with CaseId, Bytecode, Options
  - Line 75-76: `signal/2` - Sends signal to running case
  - Line 80-81: `cancel/1` - Cancels running case
  - Line 90-91: `set_trace/2` - Sets trace configuration
  - Line 94-96: `status/1` - Gets case status via gen_statem:call
  - Line 113-151: `init/1` - Initializes exec_state, sched_state, trace_state, transitions to `running`
  - Line 174-227: `running/3` - Main execution state, handles cast events and status queries
  - Line 205-214: `running({call, From}, status, StateData)` - **ISSUE 2**: Returns status map with `ip`, `step_count`, `status` keys
  - Line 216-219: `running(state_timeout, overall_timeout, StateData)` - Handles case timeout
  - Line 278-292: `cancelled/3` - Cancelled state handler
  - Line 294-298: `done({call, From}, status, StateData)` - **ISSUE 1**: Done state returns different status map (only `state` and `result`)
  - Line 340-389: `execute_quantum/1` - **ISSUE 1**: Calls `enter_state(done, StateData1)` but doesn't explicitly transition gen_statem
  - Line 391-401: `enter_state/2` - **ROOT CAUSE**: Does NOT return gen_statem next_state tuple for terminal states
  - Line 308-316: `handle_common_event/3` - Common event handler for all states

- `src/wf_exec.erl:1-920` - Bytecode executor for workflow patterns
  - Line 51-76: `new/1` - Creates exec_state from bytecode with initial token
  - Line 138-140: `is_done/1` - Checks if executor is in terminal state (`done`, `cancelled`, `failed`)
  - Line 224-255: `run/3` - Executes N quanta, returns `{done, ExecState}` or `{yield, ExecState}` or `{effect, _, ExecState}`
  - Line 277-300: `execute_opcode/3` - Opcode dispatch with case clauses accepting both uppercase and lowercase atoms
  - Line 295-296: `execute_opcode({TaskExec, _TaskName}, ExecState, _SchedDecision)` - Handles TASK_EXEC opcode
  - Line 324-383: `execute_task_exec/2` - Executes task via `lookup_task_function/2`, returns updated exec_state with IP+1
  - Line 771-820: `execute_done/1` - **CRITICAL**: Marks token complete, checks if all tokens complete, sets `status = done`
  - Line 881-885: `lookup_task_function/2` - Mock function that always returns `{ok, Ctx}` with `task_result => ok`

- `src/wf_sched.erl:1-134` - Scheduler policy selection
  - Line 56: Export of `select_action/2` - **FIXED in item 025**: Now handles both atom and map policies
  - Line 106-117: `select_action/2` - Returns `{token, mock_token}` for deterministic/nondeterministic policies

### Supporting Files

- `src/wf_vm.erl:1-90` - Bytecode type definitions
  - Line 49-60: Opcode type definitions including `{task_exec, atom()}` and `{done}`
  - Line 62-68: Join policy types

- `src/wf_exec.hrl:1-45` - Record definitions for executor
  - Line 32-44: `#exec_state{}` record with fields: ip, bytecode, ctx, case_id, tokens, branch_map, join_counters, scope_stack, step_count, status, current_token

- `src/wf_trace.erl:1-335` - Tracing subsystem
  - Line 79-94: `new/1` - Creates trace state with ETS table named `wf_trace_events`
  - Line 82: ETS table creation with `named_table` option - **POTENTIAL ISSUE**: Tests may conflict if table already exists

## Technical Considerations

### Dependencies

**External Dependencies:**
- `gen_statem` - OTP behavior for state machines (Erlang/OTP 19+)
- `eunit` - Erlang unit testing framework

**Internal Modules:**
- `wf_exec` - Bytecode executor, returns `{done, ExecState}` when complete
- `wf_sched` - Scheduler policy selection, provides `select_action/2`
- `wf_trace` - Tracing subsystem, creates ETS table `wf_trace_events`
- `wf_vm` - Bytecode type definitions

### Patterns to Follow

**Gen_statem State Function Pattern:**
Each state function must return one of:
- `{next_state, StateName, StateData}` - Transition to new state
- `{next_state, StateName, StateData, Actions}` - Transition with actions (timeouts, etc.)
- `{keep_state_and_data, Actions}` - Stay in current state
- `{stop, Reason, StateData}` - Terminate

**Current Issue in `enter_state/2`:**
```erlang
%% Line 391-401: CURRENT IMPLEMENTATION
enter_state(TargetState, StateData) ->
    case TargetState of
        done -> StateData;        %% <- WRONG: Returns StateData, not gen_statem tuple
        cancelled -> StateData;   %% <- WRONG: Returns StateData, not gen_statem tuple
        _Other -> StateData
    end.
```

**Should be:**
```erlang
enter_state(done, StateData) ->
    {next_state, done, StateData};  %% Explicit transition
enter_state(cancelled, StateData) ->
    {next_state, cancelled, StateData};
enter_state(_Other, StateData) ->
    {keep_state, StateData}.
```

**Status Query Pattern:**
The `status/1` call should return consistent map structure across all states:
- `state` - Current state atom (running, done, cancelled, etc.)
- `ip` - Current instruction pointer (from exec_state)
- `step_count` - Number of steps executed
- `status` - Executor status (running, done, blocked, etc.)

**Current Inconsistency:**
- `running/3` state handler (line 205-214): Returns map with `state => running, ip => ..., step_count => ..., status => ...`
- `done/3` state handler (line 295-298): Returns map with `state => done, result => ...` (missing `ip` and `step_count`)

### Test Bytecode Execution Flow

The `?MOCK_BYTECODE_SIMPLE_TASK` bytecode:
```erlang
[
    {'TASK_EXEC', mock_task},  % IP 0: Execute mock task
    {'DONE'}                 % IP 1: Mark token complete
]
```

Expected execution:
1. **init/1** creates exec_state with IP=0, calls `execute_quantum/1`
2. **execute_quantum/1** calls `wf_exec:run(ExecState0, 10, SchedState)`
3. **wf_exec:run/3** executes step 0 (TASK_EXEC):
   - Calls `lookup_task_function(mock_task, ExecState)` - returns `{ok, Ctx}`
   - Updates exec_state: `ip = 1`, `step_count = 1`, `ctx = #{task_result => ok}`
4. **wf_exec:run/3** executes step 1 (DONE):
   - Calls `execute_done(ExecState)` (line 771)
   - Marks token as `complete`
   - Checks if all tokens complete (line 792-793): ActiveTokens = []
   - Sets `status = done` (line 799)
   - Returns `{done, ExecState}` (line 237)
5. **execute_quantum/1** receives `{done, ExecState1}`:
   - Calls `enter_state(done, StateData1)` (line 358)
   - **BUG**: `enter_state/2` returns `StateData` instead of `{next_state, done, StateData}`
   - gen_statem stays in `running` state instead of transitioning to `done`
6. Test queries status after 100ms:
   - gen_statem is still in `running` state
   - Test expects `{ok, done, StatusInfo}` but gets `{ok, running, StatusInfo}`
   - **FAILURE 1**: `{case_clause, {ok, running, #{...}}}` at line 50

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Breaking existing state transitions** | High | Carefully review all callers of `enter_state/2` before changing return value |
| **Inconsistent status maps break other tests** | Medium | Ensure all state handlers return consistent status map structure |
| **ETS table name collision in trace tests** | Low | Use unique table names per test case (already done in wf_test_trace_helpers.erl:73) |
| **Race condition in test timing (100ms may not be enough)** | Low | Increase sleep time or use retry logic with timeout |
| **Mock task function not completing** | Low | Verify `lookup_task_function/2` returns correct function that succeeds immediately |

## Recommended Approach

### Fix Strategy

**Phase 1: Fix `enter_state/2` to return gen_statem tuples**

**File**: `src/wf_case_runner.erl:391-401`

**Change**:
```erlang
%% BEFORE:
enter_state(TargetState, StateData) ->
    case TargetState of
        done -> StateData;
        cancelled -> StateData;
        _Other -> StateData
    end.

%% AFTER:
enter_state(done, StateData) ->
    {next_state, done, StateData};
enter_state(cancelled, StateData) ->
    {next_state, cancelled, StateData};
enter_state(_Other, StateData) ->
    {keep_state, StateData}.
```

**Rationale**: This is the root cause of failure 1. The `enter_state/2` function is called when execution completes (line 358) but returns a plain `StateData` record instead of a gen_statem next_state tuple. This causes the gen_statem to remain in the `running` state instead of transitioning to `done`.

**Phase 2: Fix `done/3` status handler to return consistent map**

**File**: `src/wf_case_runner.erl:295-298`

**Change**:
```erlang
%% BEFORE:
done({call, From}, status, StateData) ->
    StatusInfo = #{state => done, result => StateData#state_data.result},
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]};

%% AFTER:
done({call, From}, status, StateData) ->
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => done,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status,
        result => StateData#state_data.result
    },
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]};
```

**Rationale**: This is the root cause of failure 2. The test at line 86 asserts `maps:is_key(ip, StatusInfo)` but the `done/3` handler doesn't include `ip` in the status map. The fix aligns the `done/3` handler with the `running/3` handler (line 205-214) which does include `ip` and `step_count`.

### Verification Steps

1. **Apply fixes** to `src/wf_case_runner.erl`
2. **Compile**: `rebar3 compile`
3. **Run specific tests**:
   ```bash
   rebar3 eunit --module=wf_case_runner_tests --test=running_to_done_completion_test
   rebar3 eunit --module=wf_case_runner_tests --test=status_query_test
   ```
4. **Run full test suite**:
   ```bash
   rebar3 eunit --module=wf_case_runner_tests
   ```
5. **Verify no regressions**:
   ```bash
   rebar3 eunit
   ```

### Expected Outcomes

- **Before Fix**: 2 failures in `wf_case_runner_tests`
  - `running_to_done_completion_test` crashes with `{case_clause, {ok, running, #{...}}}`
  - `status_query_test` fails with `?assert(maps:is_key(ip, StatusInfo))`

- **After Fix**: 0 failures in `wf_case_runner_tests`
  - Case transitions from `running` to `done` within 100ms
  - Status query returns consistent map with `ip`, `step_count`, `status` keys
  - All 5 tests in module pass

## Open Questions

1. **Test timing reliability**: Is 100ms sufficient for case completion? The mock task should complete immediately, but if there are delays in the gen_statem message loop, the test might still be flaky. Consider using retry logic or explicit synchronization instead of `timer:sleep(100)`.

2. **Status map consistency**: Should the `done/3` handler include `result` in the status map? The current fix includes both `result` and `exec_state` fields, which might be redundant. Review whether callers of `status/1` need both pieces of information.

3. **Trace table collision**: The `trace_configuration_test` sets trace to `{min, {ets, wf_trace_events}}` (line 93). If multiple tests run concurrently and the ETS table `wf_trace_events` already exists from a previous test, will `wf_trace:new/1` fail? Consider using unique table names per test case.

4. **Gen_statem termination**: Should the `done` state call `{stop, normal, StateData}` instead of remaining alive? Currently the `done/3` handler returns `{keep_state_and_data, ...}` which keeps the process alive. Review if this is the intended behavior or if completed cases should terminate automatically.

5. **Why was `enter_state/2` written incorrectly?** Was this intentional (perhaps for future refactoring) or a bug? Understanding the original intent will help ensure the fix doesn't break planned functionality.
