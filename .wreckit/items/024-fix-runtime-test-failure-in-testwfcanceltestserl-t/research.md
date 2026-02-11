# Research: Fix runtime test failure in test/wf_cancel_tests.erl — the test generator is_cancelled_active_scope_test_/0 crashes with ets:lookup/2 badarg. The wf_state module likely requires ETS table initialization before use. Fix the test setup to properly initialize the wf_state ETS tables, or mock them. Also check other test generators in wf_cancel_tests for the same issue. After fixing, rebar3 eunit must show 0 failures and 0 cancelled.

**Date**: 2025-01-18
**Item**: 024-fix-runtime-test-failure-in-testwfcanceltestserl-t

## Research Question

How do we fix the ETS table initialization issue in `wf_cancel_tests.erl` where test generators crash with `ets:lookup/2 badarg` because the `wf_state` module requires an ETS table to be initialized before use?

## Summary

The root cause of the test failure is that `wf_cancel_tests.erl` does not initialize the `wf_state` gen_server before running tests. The `wf_state` module creates an ETS table named `wf_state_store` in its `init/1` callback (line 175-182 of `src/wf_state.erl`), which is required for all state operations including `new/1`, `commit/1`, and `restore_from_ets/1`. Without this gen_server running, any attempt to interact with the ETS table results in a `badarg` error.

The fix is straightforward: add a proper EUnit test setup wrapper that starts the `wf_state` gen_server before any test runs, similar to the pattern already established in `wf_state_tests.erl` (lines 8-19). All test generators in `wf_cancel_tests.erl` need to be wrapped in a `{setup, ...}` tuple to ensure the ETS table is initialized. Additionally, we should verify that the `wf_cancel.erl` module properly uses `wf_state:restore_from_ets/1` (which it does on lines 123, 206, and 266).

## Current State Analysis

### Existing Implementation

**wf_state ETS Table Initialization:**
- `src/wf_state.erl:165-183` - The `wf_state` gen_server creates an ETS table in its `init/1` callback
- `src/wf_state.erl:175` - Table is named `wf_state_store` with `named_table` option
- `src/wf_state.erl:228` - The `new/1` function gets the ETS table reference via `whereis(wf_state_store)`
- `src/wf_state.erl:240-245` - State is persisted to ETS in `new/1` using `ets:lookup/2` and `ets:insert/2`
- `src/wf_state.erl:424-427` - `restore_from_ets/1` uses `ets:lookup/2` to restore state

**wf_cancel Tests - Current State:**
- `test/wf_cancel_tests.erl:52-54` - `is_cancelled_active_scope_test_/0` calls `create_test_state()` which calls `wf_state:new/1`
- `test/wf_cancel_tests.erl:15-41` - `create_test_state/0` helper function calls multiple `wf_state` operations
- `test/wf_cancel_tests.erl:17` - First call to `wf_state:new/1` will fail with `badarg` if ETS table not initialized

**Pattern in Other Test Files:**
- `test/wf_state_tests.erl:8-19` - Proper setup pattern that starts `wf_state:start_link()` and reuses existing process
- `test/wf_state_tests.erl:26-53` - Tests wrapped in `{setup, fun setup/0, fun cleanup/1, [...]}` pattern
- `test/wf_case_runner_tests.erl:10-24` - Another example of setup/cleanup pattern for gen_server-based tests

### Current Test Structure

All test generators in `wf_cancel_tests.erl` follow the pattern:
```erlang
test_name_test_() ->
    ?_assert(...).
```

This means each test generator returns a test object directly, without any setup wrapper. When these tests execute, they immediately call `wf_state` functions without ensuring the gen_server is running.

### Error Propagation

When a test calls `create_test_state()` or directly calls `wf_state:new/1`:
1. `wf_state:new/1` attempts to get ETS table reference via `whereis(wf_state_store)` (line 228)
2. If gen_server not running, `whereis` returns `undefined`
3. State record created with `ets_table = undefined`
4. Later call to `ets:lookup(wf_state_store, CaseId)` fails with `badarg` because `wf_state_store` doesn't exist

## Key Files

- `test/wf_cancel_tests.erl:1-505` - **PRIMARY FILE** - Contains all failing test generators that need setup wrapper
- `test/wf_cancel_tests.erl:15-41` - `create_test_state/0` helper that calls `wf_state:new/1` without setup
- `test/wf_cancel_tests.erl:52-54` - `is_cancelled_active_scope_test_/0` - First failing test mentioned in issue
- `test/wf_cancel_tests.erl:57-67` - `is_cancelled_cancelled_scope_test_/0` - Another test directly calling `wf_state:new/1`
- `test/wf_cancel_tests.erl:70-72` - `is_cancelled_undefined_scope_test_/0` - Uses `create_test_state()` helper
- `test/wf_cancel_tests.erl:75-88` - `propagate_marks_tokens_in_scope_test_/0` - Does NOT use wf_state, so may not need setup
- `test/wf_cancel_tests.erl:91-110` - `propagate_preserves_fields_test_/0` - Does NOT use wf_state, pure function test
- `test/wf_cancel_tests.erl:117-129` - `cancel_region_test_()` - Already has `{setup, ...}` wrapper! Good example
- `test/wf_cancel_tests.erl:142-155` - `cancel_region_already_cancelled_test_()` - Calls `wf_state:new/1` directly
- `test/wf_cancel_tests.erl:215-225` - `cancel_activity_without_effect_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:228-234` - `cancel_activity_undefined_task_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:237-249` - `cancel_activity_preserves_other_tasks_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:252-263` - `cancel_activity_already_cancelled_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:266-274` - `cancel_activity_invariants_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:281-290` - `cancel_case_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:293-300` - `cancel_case_status_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:303-314` - `cancel_case_already_cancelled_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:318-331` - `cancel_case_all_tokens_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:355-362` - `cancel_case_many_tokens_test_()` - Uses `create_state_with_n_tokens()`
- `test/wf_cancel_tests.erl:369-382` - `cancel_propagate_test_()` - Does NOT use wf_state, pure function test
- `test/wf_cancel_tests.erl:405-419` - `nested_scope_cancellation_test_()` - Uses `create_state_with_nested_scopes()`
- `test/wf_cancel_tests.erl:422-432` - `cancel_empty_scope_test_()` - Calls `wf_state:new/1` directly
- `test/wf_cancel_tests.erl:435-448` - `cancel_single_token_test_()` - Calls `wf_state:new/1` directly
- `test/wf_cancel_tests.erl:451-461` - `cancel_preserves_values_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:464-471` - `verify_scope_isolation_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:474-481` - `verify_no_orphaned_tokens_test_()` - Uses `create_test_state()`
- `test/wf_cancel_tests.erl:484-488` - `bench_small_scope_test_()` - Uses `create_state_with_n_tokens_in_scope()`
- `test/wf_cancel_tests.erl:491-495` - `bench_large_scope_test_()` - Uses `create_state_with_n_tokens_in_scope()`
- `test/wf_cancel_tests.erl:498-504` - `bench_complexity_isolation_test_()` - Uses `create_state_with_n_tokens_in_scope()`

- `src/wf_state.erl:165-183` - **REFERENCE** - Shows how `wf_state` gen_server creates ETS table
- `src/wf_state.erl:211-246` - **REFERENCE** - Shows `new/1` function requires ETS table to exist
- `src/wf_state.erl:340-381` - **REFERENCE** - Shows `commit/1` persists to ETS table
- `src/wf_state.erl:422-427` - **REFERENCE** - Shows `restore_from_ets/1` requires ETS table

- `src/wf_cancel.erl:123` - Uses `wf_state:restore_from_ets/1` in `cancel_activity/3`
- `src/wf_cancel.erl:206` - Uses `wf_state:restore_from_ets/1` in `cancel_case/1`
- `src/wf_cancel.erl:266` - Uses `wf_state:restore_from_ets/1` in `cancel_region/3`

- `test/wf_state_tests.erl:8-19` - **REFERENCE** - Proper setup pattern for wf_state tests
- `test/wf_state_tests.erl:26-53` - **REFERENCE** - Shows how to wrap all tests with setup/teardown

- `include/wf_state.hrl:1-60` - Record definitions for state, tokens, scopes, etc.
- `include/wf_cancel.hrl:1-27` - Record definitions for cancel events

## Technical Considerations

### Dependencies

**Internal Dependencies:**
- `wf_state` gen_server must be started before any tests that use it
- ETS table `wf_state_store` is created in `wf_state:init/1` callback
- Tests must use `-include_lib("wf_state.hrl")` for record definitions (already present)
- Tests must use `-include_lib("wf_cancel.hrl")` for cancel event records (already present)

**No External Dependencies:**
- Only standard OTP libraries (eunit, ets)
- No mock libraries needed - we start actual gen_server

### Patterns to Follow

**Setup Pattern from wf_state_tests.erl:**
```erlang
setup() ->
    case whereis(wf_state) of
        undefined ->
            {ok, Pid} = wf_state:start_link(),
            Pid;
        Pid ->
            Pid
    end.

cleanup(_Pid) ->
    %% Don't stop the gen_server, let it live for all tests
    ok.
```

**Test Wrapper Pattern:**
```erlang
test_generator_name_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"test_name_1", fun test_name_1/0},
         {"test_name_2", fun test_name_2/0},
         %% ... more tests
     ]
    }.
```

**Key Observations:**
1. Setup checks if gen_server already running to avoid "already started" errors
2. Cleanup doesn't stop gen_server - it lives for entire test suite
3. All tests are grouped into a single setup wrapper
4. Tests can be either fun/0 or `?_assert(...)` test objects

**Two Approaches for wf_cancel_tests:**

**Option 1: Single Global Setup (RECOMMENDED)**
- Wrap all test generators in one master setup/teardown
- Simpler, follows pattern from `wf_state_tests.erl`
- All tests share same gen_server instance

**Option 2: Selective Setup**
- Only wrap tests that use `wf_state`
- Pure function tests (like `propagate_marks_tokens_in_scope_test_/0`) don't need setup
- More complex but clearer separation

**Decision: Option 1 is recommended** because:
- All but ~3 tests use `wf_state` indirectly
- Simpler to maintain
- Consistent with existing test patterns
- The gen_server is designed to be long-lived anyway

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Setup/teardown pattern incorrectly applied** | High | Follow exact pattern from `wf_state_tests.erl:8-53` - tested and working |
| **Tests interfere with each other through shared ETS table** | Medium | Each test uses unique `case_id` (via `make_ref()`), isolating test data |
| **Gen_server already started from previous test run** | Low | Setup checks `whereis(wf_state)` and reuses existing process |
| **Performance degradation from ETS operations** | Low | ETS is designed for high performance; tests already use ETS extensively |
| **cleanup/1 stops gen_server breaking subsequent tests** | Medium | Don't stop gen_server in cleanup (follow pattern from line 18-19) |
| **Some tests don't need wf_state but are wrapped in setup anyway** | Very Low | No harm in extra setup; gen_server is already idempotent |

## Recommended Approach

### High-Level Strategy

1. **Add setup/cleanup functions** to `wf_cancel_tests.erl` following the exact pattern from `wf_state_tests.erl:8-19`
2. **Create a master test wrapper** that groups all test generators under one setup/teardown
3. **Convert all test generators** to be wrapped in the master setup
4. **Verify all tests pass** with `rebar3 eunit` showing 0 failures and 0 cancelled

### Implementation Steps

**Step 1: Add Setup/Cleanup Functions**
Insert after line 12 (after include directives, before test helpers):
```erlang
%%====================================================================
%% Setup
%%====================================================================

setup() ->
    case whereis(wf_state) of
        undefined ->
            {ok, Pid} = wf_state:start_link(),
            Pid;
        Pid ->
            Pid
    end.

cleanup(_Pid) ->
    %% Don't stop the gen_server, let it live for all tests
    ok.
```

**Step 2: Group All Tests Under Master Setup**
Create a master test generator at the end of the file (before current individual test generators):
```erlang
%%====================================================================
%% Master Test Suite with Setup
%%====================================================================

wf_cancel_master_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"is_cancelled_active_scope", fun is_cancelled_active_scope_test_/0},
         {"is_cancelled_cancelled_scope", fun is_cancelled_cancelled_scope_test_/0},
         %% ... all other test generators
     ]
    }.
```

**Step 3: Rename Individual Test Generators**
Change all `test_name_test_()` to `test_name_test_()` (keep same) but call them as `fun test_name_test_/0` or convert to `fun test_name/0`:
- Current: `is_cancelled_active_scope_test_() -> ?_assertNot(...).`
- New: Either keep as-is and reference as `fun is_cancelled_active_scope_test_/0`
- Or convert to: `is_cancelled_active_scope_test() -> ?assertNot(...).` and reference as `fun is_cancelled_active_scope_test/0`

**Alternative Simpler Approach (RECOMMENDED):**
Instead of creating a master wrapper, wrap each individual test generator that needs wf_state in its own setup:
```erlang
is_cancelled_active_scope_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         State = create_test_state(),
         ?_assertNot(wf_cancel:is_cancelled(State, scope1))
     end}.
```

However, this is verbose for 30+ tests.

**Best Approach: Master Wrapper + Individual Tests as fun/0**

1. Keep individual tests as functions (not generators)
2. Create one master test generator that lists them all
3. This matches the `wf_state_tests.erl` pattern exactly

### Files to Modify

**Only file to modify:**
- `test/wf_cancel_tests.erl` - Add setup/cleanup and reorganize test generators

**No changes needed to:**
- `src/wf_state.erl` - Already correctly implements ETS table initialization
- `src/wf_cancel.erl` - Already correctly uses `wf_state:restore_from_ets/1`
- Any other files - No changes required

### Verification Plan

1. Run `rebar3 eunit` to verify all tests pass
2. Verify 0 failures and 0 cancelled tests
3. Verify no `ets:lookup/2 badarg` errors
4. Verify all test generators in `wf_cancel_tests.erl` are covered

## Open Questions

1. **Should we stop the gen_server between tests?**
   - **Answer:** No, follow the pattern from `wf_state_tests.erl:18-19` - don't stop it. The gen_server is designed to be long-lived, and each test uses unique `case_id` values for isolation.

2. **Should tests that don't use wf_state be excluded from setup?**
   - **Answer:** Not necessary. The setup is idempotent (checks if already running) and lightweight. Including all tests in one master wrapper is simpler and more maintainable.

3. **Should we create a separate test suite for pure function tests vs state-dependent tests?**
   - **Answer:** Not necessary for this fix. The issue is to make tests pass, not reorganize test structure. Future refactoring can separate them if needed.

4. **What about the test at line 117-129 (`cancel_region_test_`) that already has a setup wrapper?**
   - **Answer:** This test uses `{setup, fun() -> create_test_state() end, ...}` which is different - it's setting up test data, not the gen_server. We still need the gen_server setup. This test will need to be adapted to work within the master setup wrapper, or we can keep its nested setup structure (setup within setup is allowed in EUnit).

5. **Should we add a test to verify ETS table is initialized?**
   - **Answer:** Not necessary. The fact that all other tests pass is sufficient verification. The setup function itself ensures the table exists by starting the gen_server.

6. **Performance impact of starting gen_server for all tests?**
   - **Answer:** Minimal. The gen_server is started once and reused for all tests. The ETS operations are already part of the test logic and are performant by design.
