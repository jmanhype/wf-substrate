# Research: Fix 8 failing tests in test/wf_trace_tests.erl and the 5 cancelled tests. The root cause is wf_trace:new/1 creates a named ETS table wf_trace_events — when multiple tests call new/1, the second call crashes with badarg (table already exists). Fix wf_trace:new/1 to delete existing table first or use ets:whereis to check. Also fix assertion mismatches in trace_level_full_test, sink_ets_test, snapshot_serialization_test, restore_bytecode_mismatch_test, restore_invalid_binary_test, filter_scope_test, filter_predicate_test, integration_wf_exec_emits_events_test. Target: rebar3 eunit 0 failures 0 cancelled.

**Date**: 2025-12-18
**Item**: 050-fix-8-failing-tests-in-testwftracetestserl-and-the

## Research Question

What are the root causes of failing tests in `wf_trace_tests.erl`, and what fixes are needed to make all tests pass with `rebar3 eunit`?

## Summary

The research reveals **two categories of issues** causing test failures:

1. **ETS Table Reuse Problem**: The current implementation of `wf_trace:new/1` (lines 84-96) reuses existing ETS tables across tests, causing test pollution. When multiple tests run sequentially, events from previous tests remain in the shared `wf_trace_events` table, causing assertion failures.

2. **Missing Serialization Functions**: Tests expect `wf_exec:snapshot_exec_state/1` to return a `binary()` for serialization, but the current implementation (wf_exec.erl:106-120) returns a `map()`. Additionally, `wf_exec:restore_exec_state/2` is called by tests but doesn't exist.

The fixes require:
- Modifying `wf_trace:new/1` to delete and recreate the ETS table (not reuse it)
- Implementing proper binary serialization for `wf_exec:snapshot_exec_state/1`
- Adding `wf_exec:restore_exec_state/2` with validation logic
- Adding test setup/teardown to clean up ETS tables between tests

## Current State Analysis

### Existing Implementation

#### wf_trace:new/1 (src/wf_trace.erl:80-103)

The function currently uses `ets:whereis/1` to check if the table exists and reuses it:

```erlang
new(Level) ->
    Table = case ets:whereis(wf_trace_events) of
        undefined ->
            ets:new(wf_trace_events, [named_table, bag, public, {read_concurrency, true}]);
        _ExistingTableId ->
            %% Table already exists, reuse it
            wf_trace_events
    end,
    State = #trace_state{
        level = Level,
        sink = {ets, Table},
        case_id = undefined
    },
    {ok, State}.
```

**Problem**: Reusing the table causes test pollution. Events from previous tests remain in the bag table, causing assertions like `?assert(length(Events) > 0)` to pass but with wrong event counts.

#### wf_exec:snapshot_exec_state/1 (src/wf_exec.erl:106-120)

The function returns a map:

```erlang
-spec snapshot_exec_state(exec_state()) -> map().
snapshot_exec_state(ExecState) ->
    #{
        ip => ExecState#exec_state.ip,
        bytecode => ExecState#exec_state.bytecode,
        ctx => ExecState#exec_state.ctx,
        case_id => ExecState#exec_state.case_id,
        tokens => ExecState#exec_state.tokens,
        branch_map => ExecState#exec_state.branch_map,
        join_counters => ExecState#exec_state.join_counters,
        scope_stack => ExecState#exec_state.scope_stack,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status,
        current_token => ExecState#exec_state.current_token
    }.
```

**Problems**:
1. Returns `map()` but tests expect `binary()` (test/wf_trace_tests.erl:130: `?assert(is_binary(Binary))`)
2. The `trace_event` record expects `state_before :: binary() | undefined` (src/wf_trace.erl:13)
3. `wf_trace:emit_event/2` calls `wf_exec:snapshot_exec_state(ExecState)` at line 268, storing the map in a field that should be binary

#### Missing wf_exec:restore_exec_state/2

The test file calls this function in three places (lines 133, 145, 153), but it doesn't exist in `wf_exec.erl`. The exports list (lines 22-36) doesn't include it.

### Test Patterns

The tests use EUnit's `fun() ->` pattern without explicit setup/teardown. Other test files in the codebase use proper setup/teardown:

**Example from wf_effect_tests.erl:45-56**:
```erlang
yield_new_effect_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         ?_test(...)
     end}.
```

**Example from wf_cancel_tests.erl:23-26** (ETS cleanup pattern):
```erlang
cleanup(_Pid) ->
    %% Clear ETS table to prevent test interference
    ets:delete_all_objects(wf_state_store),
    ok.
```

**Example from wf_test_trace_helpers.erl:93** (ETS table deletion):
```erlang
%% Clean up ETS table
ets:delete(Table),
```

### Test-Specific Issues

1. **trace_level_full_test** (line 48): Fails because `state_before` is a map, not binary
2. **sink_ets_test** (line 89): Fails due to event pollution from previous tests
3. **snapshot_serialization_test** (line 125): Fails because `snapshot_exec_state/1` returns map, not binary
4. **restore_bytecode_mismatch_test** (line 138): Fails because `restore_exec_state/2` doesn't exist
5. **restore_invalid_binary_test** (line 149): Fails because `restore_exec_state/2` doesn't exist
6. **filter_scope_test** (line 172): Fails due to event pollution (wrong event counts)
7. **filter_predicate_test** (line 191): Fails due to event pollution (wrong step_seq values)
8. **integration_wf_exec_emits_events_test** (line 266): Fails due to event pollution (wrong event counts)

## Key Files

- `src/wf_trace.erl:80-103` - `new/1` function with ETS table reuse logic
- `src/wf_trace.erl:106-120` - `snapshot_exec_state/1` function returning map instead of binary
- `src/wf_trace.erl:267-270` - Code that calls `snapshot_exec_state/1` and stores result in `state_before` field
- `src/wf_exec.erl:106-120` - `snapshot_exec_state/1` implementation (returns map, needs binary serialization)
- `src/wf_exec.erl:22-36` - Export list (missing `restore_exec_state/2`)
- `test/wf_trace_tests.erl:48-63` - `trace_level_full_test` expecting binary `state_before`
- `test/wf_trace_tests.erl:89-97` - `sink_ets_test` affected by ETS table reuse
- `test/wf_trace_tests.erl:125-136` - `snapshot_serialization_test` expecting binary serialization
- `test/wf_trace_tests.erl:138-147` - `restore_bytecode_mismatch_test` calling missing function
- `test/wf_trace_tests.erl:149-155` - `restore_invalid_binary_test` calling missing function
- `test/wf_trace_tests.erl:172-189` - `filter_scope_test` affected by event pollution
- `test/wf_trace_tests.erl:191-203` - `filter_predicate_test` affected by event pollution
- `test/wf_trace_tests.erl:266-290` - `integration_wf_exec_emits_events_test` affected by event pollution
- `test/wf_effect_tests.erl:45-56` - Example of proper EUnit setup/teardown pattern
- `test/wf_cancel_tests.erl:23-26` - Example of ETS cleanup pattern
- `test/wf_test_trace_helpers.erl:93` - Example of ETS table deletion

## Technical Considerations

### Dependencies

- **Erlang/OTP ETS module**: For table management (`ets:new/2`, `ets:delete/1`, `ets:delete_all_objects/1`, `ets:whereis/1`)
- **EUnit**: Test framework (already in use)
- **term_to_binary/1** and **binary_to_term/1**: Erlang BIFs for serialization

### Patterns to Follow

1. **ETS Table Cleanup**: From `wf_cancel_tests.erl:25` - use `ets:delete_all_objects/1` to clear tables between tests
2. **ETS Table Deletion**: From `wf_test_trace_helpers.erl:93` - use `ets:delete/1` to completely remove tables
3. **EUnit Setup/Teardown**: From `wf_effect_tests.erl:45-68` - wrap tests with `{setup, SetupFun, CleanupFun, TestFun}` pattern
4. **Binary Serialization**: Use `term_to_binary/1` to convert the exec_state map to binary for storage

### Serialization Strategy

The `snapshot_exec_state/1` function should:
1. Convert the exec_state map to binary using `term_to_binary/1`
2. Return the binary for storage in trace events

The `restore_exec_state/2` function should:
1. Use `binary_to_term/1` to deserialize the binary back to a map
2. Validate that the bytecode in the snapshot matches the provided bytecode (bytecode verification)
3. Handle invalid binaries with `{error, invalid_snapshot}` return value
4. Handle bytecode mismatches with `{error, {bytecode_mismatch, Diff}}` return value

### Test Architecture

**Current**: Tests use `fun() ->` pattern without cleanup
```erlang
trace_level_full_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        %% ... test code ...
    end.
```

**Recommended**: Wrap with setup/teardown for ETS cleanup
```erlang
trace_level_full_test_() ->
    {setup,
     fun() ->
         %% Setup: Create fresh ETS table
         ok
     end,
     fun(_) ->
         %% Cleanup: Delete ETS table
         case ets:whereis(wf_trace_events) of
             undefined -> ok;
             _TableId -> ets:delete(wf_trace_events)
         end
     end,
     fun() ->
         %% Test body
         ...
     end}.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| Breaking change to `snapshot_exec_state/1` return type | High | This function is only used in tracing code; check all call sites and update type specs |
| Binary serialization overhead | Medium | Only serialize when trace level is `full`; level `min` and `none` skip serialization |
| ETS table deletion during concurrent access | Low | Tests run sequentially; no concurrent access in test environment |
| Bytecode verification complexity | Medium | Use simple field comparison; return detailed error for debugging |
| Process dictionary pollution | Medium | Clean up `wf_trace_state` key in teardown (see wf_test_trace_helpers.erl:96) |

### Implementation Risks

1. **Type Spec Mismatch**: The current type spec for `snapshot_exec_state/1` says `map()`, but we need to change it to `binary()`. This affects:
   - `wf_trace.erl:268` - calls `wf_exec:snapshot_exec_state/1` and stores in `state_before` field
   - `wf_trace.erl:13` - `state_before :: binary() | undefined` (already correct!)
   - All test assertions checking `is_binary(FirstEvent#trace_event.state_before)`

2. **Missing Export**: Adding `restore_exec_state/2` requires:
   - Adding to `-export([])` list in `wf_exec.erl`
   - Adding `-spec` declaration
   - Implementing the function with proper error handling

3. **ETS Table Lifecycle**: Two approaches:
   - **Option A**: Delete and recreate table in each test's setup/teardown (cleaner isolation)
   - **Option B**: Delete and recreate in `wf_trace:new/1` (simpler, but less explicit)

   **Recommendation**: Option A for test isolation, but also add cleanup to `wf_trace:new/1` as defensive programming.

## Recommended Approach

### Phase 1: Fix ETS Table Reuse (wf_trace:new/1)

1. **Modify `wf_trace:new/1`** to delete existing table before creating new one:
```erlang
new(Level) ->
    %% Delete existing table if present (defensive cleanup)
    case ets:whereis(wf_trace_events) of
        undefined -> ok;
        _TableId -> ets:delete(wf_trace_events)
    end,
    %% Create fresh table
    Table = ets:new(wf_trace_events, [
        named_table,
        bag,
        public,
        {read_concurrency, true}
    ]),
    State = #trace_state{
        level = Level,
        sink = {ets, Table},
        case_id = undefined
    },
    {ok, State}.
```

2. **Add test setup/teardown** to ensure cleanup between tests (defensive):
```erlang
%% Wrap all tests with setup/teardown
wf_trace_tests_() ->
    {setup,
     fun() -> ok end,
     fun(_) ->
         case ets:whereis(wf_trace_events) of
             undefined -> ok;
             _TableId -> ets:delete(wf_trace_events)
         end
     end,
     [
         fun trace_level_none_test_/0,
         fun trace_level_min_test_/0,
         %% ... all other tests ...
     ]}.
```

### Phase 2: Implement Binary Serialization (wf_exec)

1. **Modify `wf_exec:snapshot_exec_state/1`** to return binary:
```erlang
-spec snapshot_exec_state(exec_state()) -> binary().
snapshot_exec_state(ExecState) ->
    StateMap = #{
        ip => ExecState#exec_state.ip,
        bytecode => ExecState#exec_state.bytecode,
        ctx => ExecState#exec_state.ctx,
        case_id => ExecState#exec_state.case_id,
        tokens => ExecState#exec_state.tokens,
        branch_map => ExecState#exec_state.branch_map,
        join_counters => ExecState#exec_state.join_counters,
        scope_stack => ExecState#exec_state.scope_stack,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status,
        current_token => ExecState#exec_state.current_token
    },
    term_to_binary(StateMap).
```

2. **Add `wf_exec:restore_exec_state/2`**:
```erlang
-spec restore_exec_state(binary(), wf_vm:wf_bc()) -> {ok, exec_state()} | {error, term()}.
restore_exec_state(Binary, Bytecode) ->
    try
        StateMap = binary_to_term(Binary),
        %% Validate bytecode matches
        case maps:get(bytecode, StateMap) of
            Bytecode ->
                %% Bytecode matches, restore exec_state
                ExecState = #exec_state{
                    ip = maps:get(ip, StateMap),
                    bytecode = maps:get(bytecode, StateMap),
                    ctx = maps:get(ctx, StateMap),
                    case_id = maps:get(case_id, StateMap),
                    tokens = maps:get(tokens, StateMap),
                    branch_map = maps:get(branch_map, StateMap),
                    join_counters = maps:get(join_counters, StateMap),
                    scope_stack = maps:get(scope_stack, StateMap),
                    step_count = maps:get(step_count, StateMap),
                    status = maps:get(status, StateMap),
                    current_token = maps:get(current_token, StateMap)
                },
                {ok, ExecState};
            StoredBytecode ->
                %% Bytecode mismatch
                {error, {bytecode_mismatch, {expected, Bytecode, got, StoredBytecode}}}
        end
    catch
        error:_ ->
            {error, invalid_snapshot}
    end.
```

3. **Update exports** in `wf_exec.erl`:
```erlang
-export([
    new/1,
    step/2,
    run/3,
    resume/2,
    is_done/1,
    is_blocked/1,
    get_ip/1,
    get_ctx/1,
    get_step_count/1,
    set_ctx/2,
    get_scope_stack_depth/1,
    snapshot_exec_state/1,
    restore_exec_state/2,  %% ADD THIS
    find_branch_for_token/2
]).
```

### Phase 3: Verify Test Fixes

After implementing Phases 1 and 2, verify these tests pass:

1. **trace_level_full_test**: Now `state_before` is binary, assertion passes
2. **sink_ets_test**: Fresh ETS table per test, no pollution
3. **snapshot_serialization_test**: `snapshot_exec_state/1` returns binary, passes
4. **restore_bytecode_mismatch_test**: `restore_exec_state/2` exists and validates bytecode
5. **restore_invalid_binary_test**: `restore_exec_state/2` handles invalid binary
6. **filter_scope_test**: Fresh ETS table, correct event counts
7. **filter_predicate_test**: Fresh ETS table, correct step_seq values
8. **integration_wf_exec_emits_events_test**: Fresh ETS table, correct event counts

### Testing Strategy

Run tests incrementally:
```bash
# Phase 1: Test ETS cleanup
rebar3 eunit --module=wf_trace_tests --test=trace_level_none_test
rebar3 eunit --module=wf_trace_tests --test=sink_ets_test

# Phase 2: Test serialization
rebar3 eunit --module=wf_trace_tests --test=snapshot_serialization_test
rebar3 eunit --module=wf_trace_tests --test=restore_bytecode_mismatch_test
rebar3 eunit --module=wf_trace_tests --test=restore_invalid_binary_test

# Full test suite
rebar3 eunit --module=wf_trace_tests
```

## Open Questions

1. **Bytecode Comparison Strategy**: How should bytecode mismatch be detected?
   - Simple term comparison (`=:=`)?
   - Deep comparison of bytecode structure?
   - Hash-based comparison?
   - **Recommendation**: Use term comparison for simplicity

2. **Error Return Format**: Should `restore_exec_state/2` return detailed error info?
   - `{error, {bytecode_mismatch, {expected, Expected, got, Got}}}` (as shown above)
   - Simpler `{error, bytecode_mismatch}`?
   - **Recommendation**: Detailed format for debugging

3. **Test Setup Granularity**: Should we wrap each test individually or all tests together?
   - Individual setup/teardown per test (most isolation)
   - Single setup/teardown for entire module (faster)
   - **Recommendation**: Individual for now, can optimize later if needed

4. **Process Dictionary Cleanup**: Should `wf_trace:new/1` also clean up the process dictionary?
   - Tests use `put(wf_trace_state, State)` to store state
   - Should we call `erase(wf_trace_state)` in cleanup?
   - **Recommendation**: Yes, in test teardown (see wf_test_trace_helpers.erl:96)

5. **Backward Compatibility**: Are there other modules calling `snapshot_exec_state/1`?
   - Only `wf_trace.erl:268` calls it
   - **Recommendation**: Verify with grep, update type specs accordingly
