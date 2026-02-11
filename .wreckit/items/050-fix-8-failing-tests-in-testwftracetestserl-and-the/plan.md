# Fix 8 failing tests in test/wf_trace_tests.erl and the 5 cancelled tests. The root cause is wf_trace:new/1 creates a named ETS table wf_trace_events — when multiple tests call new/1, the second call crashes with badarg (table already exists). Fix wf_trace:new/1 to delete existing table first or use ets:whereis to check. Also fix assertion mismatches in trace_level_full_test, sink_ets_test, snapshot_serialization_test, restore_bytecode_mismatch_test, restore_invalid_binary_test, filter_scope_test, filter_predicate_test, integration_wf_exec_emits_events_test. Target: rebar3 eunit 0 failures 0 cancelled. Implementation Plan

## Implementation Plan Title

Fix ETS Table Lifecycle and State Serialization for Tracing Tests

## Overview

This implementation plan fixes two categories of test failures in `wf_trace_tests.erl`:

1. **ETS Table Reuse Problem**: The current `wf_trace:new/1` implementation reuses existing ETS tables across tests, causing event pollution. When multiple tests run sequentially, events from previous tests remain in the shared `wf_trace_events` table, causing assertion failures with wrong event counts.

2. **Missing Serialization Functions**: Tests expect `wf_exec:snapshot_exec_state/1` to return a `binary()` for serialization, but the current implementation returns a `map()`. Additionally, `wf_exec:restore_exec_state/2` is called by tests but doesn't exist.

The goal is to achieve **0 failures, 0 cancelled** when running `rebar3 eunit` on the test suite.

## Current State

### ETS Table Management (src/wf_trace.erl:80-103)

The `wf_trace:new/1` function currently checks if the table exists and reuses it:

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

### State Serialization (src/wf_exec.erl:106-120)

The `snapshot_exec_state/1` function returns a map:

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
2. The `trace_event` record expects `state_before :: binary() | undefined` (src/wf_trace.hrl:10)
3. `wf_trace:emit_event/2` calls `wf_exec:snapshot_exec_state(ExecState)` at line 268, storing the map in a field that should be binary

### Missing Function (test/wf_trace_tests.erl:133, 145, 153)

Tests call `wf_exec:restore_exec_state/2` which doesn't exist:
- Line 133: `{ok, RestoredState} = wf_exec:restore_exec_state(Binary, Bytecode)`
- Line 145: `Result = wf_exec:restore_exec_state(Binary, Bytecode2)`
- Line 153: `Result = wf_exec:restore_exec_state(InvalidBinary, Bytecode)`

### Test Patterns

Tests use EUnit's `fun() ->` pattern without explicit setup/teardown:

```erlang
trace_level_full_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        %% ... test code ...
    end.
```

Other test files in the codebase use proper setup/teardown (e.g., `wf_effect_tests.erl:45-56`, `wf_cancel_tests.erl:23-26`).

## Desired End State

1. **Clean ETS Tables**: Each test gets a fresh ETS table, ensuring no event pollution
2. **Binary Serialization**: `wf_exec:snapshot_exec_state/1` returns `binary()` for proper serialization
3. **Restore Function**: `wf_exec:restore_exec_state/2` exists with bytecode validation
4. **All Tests Pass**: Running `rebar3 eunit` produces 0 failures, 0 cancelled

### Key Discoveries:

- **wf_test_trace_helpers.erl:73-96** shows the correct pattern: create unique table names per test and delete after use
- **wf_cancel_tests.erl:25** shows ETS cleanup pattern: `ets:delete_all_objects(wf_state_store)`
- **wf_trace.hrl:10** confirms `state_before` should be `binary() | undefined`
- **wf_trace.erl:267-270** stores `snapshot_exec_state/1` result in `state_before` field, which must be binary

## What We're NOT Doing

1. **NOT changing the trace_event record structure**: The record is already correct (`state_before :: binary() | undefined`)
2. **NOT modifying test logic**: Tests are correct; we fix the implementation to match test expectations
3. **NOT adding new test infrastructure**: We use existing EUnit patterns from the codebase
4. **NOT changing serialization format**: We use Erlang's built-in `term_to_binary/1` and `binary_to_term/1`
5. **NOT addressing concurrent test execution**: Tests run sequentially in EUnit; concurrent safety is out of scope

## Implementation Approach

The implementation follows two phases:

**Phase 1: Fix ETS Table Lifecycle**
- Modify `wf_trace:new/1` to delete existing table before creating new one
- This prevents event pollution between tests
- Simpler than wrapping each test in setup/teardown

**Phase 2: Fix State Serialization**
- Change `wf_exec:snapshot_exec_state/1` to return `binary()` using `term_to_binary/1`
- Add `wf_exec:restore_exec_state/2` with bytecode validation
- Update type specs to match new return types

The approach prioritizes:
1. **Minimal changes**: Fix only what's broken, don't refactor unrelated code
2. **Follow existing patterns**: Use ETS cleanup patterns from `wf_cancel_tests` and `wf_test_trace_helpers`
3. **Type safety**: Ensure type specs match implementation
4. **Incremental testing**: Each phase can be verified independently

---

## Phases

### Phase 1: Fix ETS Table Reuse in wf_trace:new/1

#### Overview

Fix the ETS table reuse problem by deleting existing tables before creating new ones. This ensures each test gets a fresh table without event pollution.

#### Changes Required:

##### 1. wf_trace.erl (src/wf_trace.erl:80-103)

**File**: `src/wf_trace.erl`
**Changes**: Modify `new/1` to delete existing table before creating new one

```erlang
%% @doc Create new trace state with specified level
-spec new(trace_level()) -> {ok, #trace_state{}}.
new(Level) ->
    %% Delete existing table if present (prevents test pollution)
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

**Rationale**:
- **Deleting vs. clearing**: `ets:delete/1` completely removes the table, ensuring a fresh start. `ets:delete_all_objects/1` would clear objects but keep the table, which is less clean.
- **No race condition**: Tests run sequentially in EUnit, so there's no risk of concurrent access during delete/create
- **Defensive programming**: The `case ets:whereis/1` guard prevents crashes if the table doesn't exist

#### Success Criteria:

##### Automated Verification:

- [ ] All ETS-related tests pass: `rebar3 eunit --module=wf_trace_tests --test=trace_level_none_test`
- [ ] sink_ets_test passes: `rebar3 eunit --module=wf_trace_tests --test=sink_ets_test`
- [ ] filter_scope_test passes: `rebar3 eunit --module=wf_trace_tests --test=filter_scope_test`
- [ ] filter_predicate_test passes: `rebar3 eunit --module=wf_trace_tests --test=filter_predicate_test`
- [ ] integration_wf_exec_emits_events_test passes: `rebar3 eunit --module=wf_trace_tests --test=integration_wf_exec_emits_events_test`

##### Manual Verification:

- [ ] Tests pass when run individually: `rebar3 eunit --module=wf_trace_tests`
- [ ] Tests pass when run with full suite: `rebar3 eunit`
- [ ] No "table already exists" errors in test output
- [ ] Event counts are correct (no pollution from previous tests)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Implement Binary Serialization for State Snapshots

#### Overview

Change `wf_exec:snapshot_exec_state/1` to return binary serialization, and add `wf_exec:restore_exec_state/2` with bytecode validation.

#### Changes Required:

##### 1. wf_exec.erl - Update snapshot_exec_state/1 (src/wf_exec.erl:106-120)

**File**: `src/wf_exec.erl`
**Changes**: Modify `snapshot_exec_state/1` to return `binary()` instead of `map()`

```erlang
%% @doc Snapshot execution state for tracing
%% Returns a binary serialization of the exec_state for storage in trace events
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

**Rationale**:
- **Preserves existing logic**: We still build the map, then serialize it
- **Type safety**: Return type changes from `map()` to `binary()`
- **No breaking changes**: Only `wf_trace:emit_event/2` calls this function, and it expects binary

##### 2. wf_exec.erl - Add restore_exec_state/2 (src/wf_exec.erl:~136)

**File**: `src/wf_exec.erl`
**Changes**: Add new function after `find_branch_for_token/2` (around line 136)

```erlang
%% @doc Restore execution state from snapshot binary
%% Validates that bytecode in snapshot matches provided bytecode
%% Returns {ok, ExecState} on success, {error, Reason} on failure
-spec restore_exec_state(binary(), wf_vm:wf_bc()) -> {ok, exec_state()} | {error, term()}.
restore_exec_state(Binary, Bytecode) ->
    try
        StateMap = binary_to_term(Binary),

        %% Validate required fields
        case maps:is_key(ip, StateMap) andalso
             maps:is_key(bytecode, StateMap) andalso
             maps:is_key(ctx, StateMap) andalso
             maps:is_key(case_id, StateMap) andalso
             maps:is_key(tokens, StateMap) andalso
             maps:is_key(branch_map, StateMap) andalso
             maps:is_key(join_counters, StateMap) andalso
             maps:is_key(scope_stack, StateMap) andalso
             maps:is_key(step_count, StateMap) andalso
             maps:is_key(status, StateMap) andalso
             maps:is_key(current_token, StateMap) of
            false ->
                {error, invalid_snapshot};
            true ->
                %% Validate bytecode matches
                StoredBytecode = maps:get(bytecode, StateMap),
                case StoredBytecode =:= Bytecode of
                    false ->
                        {error, {bytecode_mismatch, {expected, Bytecode, got, StoredBytecode}}};
                    true ->
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
                        {ok, ExecState}
                end
        end
    catch
        error:_ ->
            {error, invalid_snapshot}
    end.
```

**Rationale**:
- **Bytecode validation**: Prevents restoring state with mismatched bytecode (detects corruption)
- **Field validation**: Ensures all required fields exist in the binary
- **Error handling**: Catches binary_to_term/1 errors for invalid binaries
- **Detailed errors**: Returns structured error info for debugging

##### 3. wf_exec.erl - Update exports (src/wf_exec.erl:22-36)

**File**: `src/wf_exec.erl`
**Changes**: Add `restore_exec_state/2` to export list

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
    restore_exec_state/2,  %% ADD THIS LINE
    find_branch_for_token/2
]).
```

#### Success Criteria:

##### Automated Verification:

- [ ] trace_level_full_test passes: `rebar3 eunit --module=wf_trace_tests --test=trace_level_full_test`
- [ ] snapshot_serialization_test passes: `rebar3 eunit --module=wf_trace_tests --test=snapshot_serialization_test`
- [ ] restore_bytecode_mismatch_test passes: `rebar3 eunit --module=wf_trace_tests --test=restore_bytecode_mismatch_test`
- [ ] restore_invalid_binary_test passes: `rebar3 eunit --module=wf_trace_tests --test=restore_invalid_binary_test`
- [ ] Type checking passes: `rebar3 compile`
- [ ] No warnings about undefined functions

##### Manual Verification:

- [ ] Binary serialization produces compact representation (check with `byte_size/1`)
- [ ] Restored state matches original state (case_id, step_count match)
- [ ] Bytecode mismatch error provides useful debugging info
- [ ] Invalid binary returns `{error, invalid_snapshot}`

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to final verification.

---

### Phase 3: Final Verification

#### Overview

Run the complete test suite to ensure all tests pass with 0 failures, 0 cancelled.

#### Success Criteria:

##### Automated Verification:

- [ ] All wf_trace_tests pass: `rebar3 eunit --module=wf_trace_tests`
- [ ] Full test suite passes: `rebar3 eunit`
- [ ] No cancelled tests
- [ ] No test failures
- [ ] Compilation succeeds with no warnings: `rebar3 compile`

##### Manual Verification:

- [ ] Verify test output shows "All tests passed"
- [ ] Check that previously failing 8 tests now pass
- [ ] Confirm no regressions in other test modules
- [ ] Review any warnings or messages from compiler

**Exit Criteria**: When `rebar3 eunit` produces 0 failures, 0 cancelled, the implementation is complete.

---

## Testing Strategy

### Unit Tests:

All existing tests in `wf_trace_tests.erl` serve as unit tests:
- **trace_level_none_test**: Verifies no events emitted at level=none
- **trace_level_min_test**: Verifies only structural events emitted at level=min
- **trace_level_full_test**: Verifies all events emitted with binary state_before
- **sink_ets_test**: Verifies ETS sink stores events correctly (no pollution)
- **snapshot_serialization_test**: Verifies binary serialization and deserialization
- **restore_bytecode_mismatch_test**: Verifies bytecode validation in restore
- **restore_invalid_binary_test**: Verifies error handling for invalid binaries
- **filter_scope_test**: Verifies scope filtering with correct event counts
- **filter_predicate_test**: Verifies predicate filtering with correct step_seq values
- **integration_wf_exec_emits_events_test**: Verifies end-to-end event emission

### Integration Tests:

- **integration_wf_exec_emits_events_test**: End-to-end test of executor emitting trace events
- **integration_step_seq_monotonic_test**: Verifies step_seq monotonicity across events
- **integration_case_id_test**: Verifies case_id consistency through execution

### Manual Testing Steps:

1. **Run single test**: `rebar3 eunit --module=wf_trace_tests --test=trace_level_none_test`
   - Verify test passes in isolation

2. **Run all trace tests**: `rebar3 eunit --module=wf_trace_tests`
   - Verify all tests pass together
   - Check for no "table already exists" errors

3. **Run full suite**: `rebar3 eunit`
   - Verify no regressions in other modules
   - Confirm 0 failures, 0 cancelled

4. **Verify binary serialization**: Add temporary debug output to check binary size
   ```erlang
   Binary = wf_exec:snapshot_exec_state(ExecState),
   io:format("Binary size: ~p~n", [byte_size(Binary)])
   ```
   - Verify binary is reasonable size (< 1KB for simple exec_state)

5. **Verify ETS cleanup**: Check that ETS table is deleted between tests
   ```erlang
   %% Add debug output in wf_trace:new/1
   case ets:whereis(wf_trace_events) of
       undefined -> ok;
       TableId ->
           io:format("Deleting existing table: ~p~n", [TableId]),
           ets:delete(wf_trace_events)
   end
   ```

## Migration Notes

### Breaking Changes:

1. **snapshot_exec_state/1 return type**: Changes from `map()` to `binary()`
   - Only `wf_trace:emit_event/2` calls this function
   - The caller expects binary (state_before field type)
   - No external callers affected (verified by grep)

2. **No data migration needed**: This is test infrastructure only, not production data

### Rollback Strategy:

If tests fail after implementation:
1. **Phase 1 rollback**: Revert `wf_trace:new/1` to reuse tables
2. **Phase 2 rollback**: Revert `snapshot_exec_state/1` to return map(), remove `restore_exec_state/2`
3. **Investigate**: Check if there are other callers of `snapshot_exec_state/1` not identified in research

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/050-fix-8-failing-tests-in-testwftracetestserl-and-the/research.md`
- Test file: `test/wf_trace_tests.erl` (lines 13-336)
- wf_trace implementation: `src/wf_trace.erl` (lines 80-103 for new/1, 250-290 for emit_event/2)
- wf_exec implementation: `src/wf_exec.erl` (lines 106-120 for snapshot_exec_state/1)
- Trace records: `src/wf_trace.hrl` (line 10: state_before :: binary() | undefined)
- ETS cleanup pattern: `test/wf_cancel_tests.erl` (line 25)
- ETS table deletion: `test/wf_test_trace_helpers.erl` (line 93)
- EUnit setup/teardown example: `test/wf_effect_tests.erl` (lines 45-56)
