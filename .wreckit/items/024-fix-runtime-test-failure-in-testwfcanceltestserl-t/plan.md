# Fix runtime test failure in test/wf_cancel_tests.erl — the test generator is_cancelled_active_scope_test_/0 crashes with ets:lookup/2 badarg. The wf_state module likely requires ETS table initialization before use. Fix the test setup to properly initialize the wf_state ETS tables, or mock them. Also check other test generators in wf_cancel_tests for the same issue. After fixing, rebar3 eunit must show 0 failures and 0 cancelled. Implementation Plan

## Implementation Plan Title

Fix ETS Table Initialization in wf_cancel_tests.erl

## Overview

This implementation plan fixes a critical test infrastructure issue where test generators in `wf_cancel_tests.erl` crash with `ets:lookup/2 badarg` errors. The root cause is that the `wf_state` gen_server, which creates the required `wf_state_store` ETS table, is not started before tests run. This fix adds proper EUnit setup/teardown functions following the established pattern from `wf_state_tests.erl` and wraps all test generators to ensure the ETS table is initialized.

## Current State

**Test Failure Pattern:**
- All test generators in `wf_cancel_tests.erl` (lines 52-504) call functions that depend on `wf_state` ETS table
- The `create_test_state()` helper (lines 15-41) calls `wf_state:new/1` which immediately fails with `badarg` if the ETS table doesn't exist
- 30+ test generators lack proper setup wrapper
- Only one test (`cancel_region_test_` at lines 117-129) has a setup wrapper, but it only sets up test data, not the gen_server

**EITS Table Creation:**
- `wf_state` gen_server creates `wf_state_store` ETS table in its `init/1` callback (src/wf_state.erl:175-182)
- Table is created with `named_table` option for direct access via table name
- All `wf_state` operations (`new/1`, `commit/1`, `restore_from_ets/1`) require this table

**Existing Pattern:**
- `wf_state_tests.erl` (lines 8-53) demonstrates the correct pattern
- Setup function checks if gen_server already running, starts it if not
- Cleanup function intentionally doesn't stop gen_server (it lives for all tests)
- All tests grouped under one master setup wrapper

## Desired End State

All tests in `wf_cancel_tests.erl` pass with 0 failures and 0 cancelled when running `rebar3 eunit`.

**Verification:**
```bash
rebar3 eunit
# Expected output: 0 failures, 0 cancelled
```

### Key Discoveries:

- **ETS table dependency**: `wf_state:new/1` (called by test helpers) requires `wf_state_store` ETS table to exist - src/wf_state.erl:228 attempts to get table reference via `whereis(wf_state_store)`, causing `badarg` if table doesn't exist
- **Setup pattern already exists**: `wf_state_tests.erl:8-19` provides exact pattern to follow - check if gen_server running, start if needed, don't stop in cleanup
- **Test isolation**: Each test uses unique `case_id` (via `make_ref()`), so tests don't interfere through shared ETS table
- **Nested setup allowed**: EUnit allows setup within setup - the existing `cancel_region_test_()` (lines 117-129) has data setup that can coexist with gen_server setup
- **Pure function tests don't need wf_state**: Tests like `propagate_marks_tokens_in_scope_test_()` (lines 75-88) test pure functions but can still be wrapped in master setup without harm

## What We're NOT Doing

- **NOT modifying `wf_state.erl`**: The module correctly implements ETS table initialization; no changes needed
- **NOT modifying `wf_cancel.erl`**: The module correctly uses `wf_state:restore_from_ets/1`; no changes needed
- **NOT creating mock ETS table**: Starting the actual gen_server is simpler and more reliable than mocking
- **NOT stopping gen_server between tests**: Following pattern from `wf_state_tests.erl:18-19`, gen_server is long-lived
- **NOT separating pure vs state-dependent tests**: All tests will use same master setup for simplicity
- **NOT changing test logic**: Only adding setup wrapper, no modifications to test assertions or behavior

## Implementation Approach

**High-Level Strategy:**
1. Add setup/cleanup functions following exact pattern from `wf_state_tests.erl:8-19`
2. Convert all individual test generators from `_test_()` format to regular functions (or fun/0)
3. Create master test generator that wraps all tests with setup/teardown
4. Verify all tests pass with `rebar3 eunit`

**Why This Approach:**
- **Simplicity**: Single setup wrapper for all tests is easier to maintain than 30+ individual wrappers
- **Consistency**: Matches existing pattern in `wf_state_tests.erl`
- **Safety**: Setup checks if gen_server already running, preventing "already started" errors
- **Performance**: Gen_server started once, reused for all tests - minimal overhead
- **Test isolation**: Unique `case_id` per test ensures no interference through shared ETS table

**Key Technical Decision:**
We will wrap ALL tests (including pure function tests) in the master setup wrapper. The setup is idempotent and lightweight, and this approach is simpler than selectively wrapping only state-dependent tests.

---

## Phases

### Phase 1: Add Setup/Cleanup Functions

#### Overview

Add EUnit setup/teardown functions to `wf_cancel_tests.erl` following the exact pattern from `wf_state_tests.erl`. These functions will start the `wf_state` gen_server before any tests run.

#### Changes Required:

##### 1. test/wf_cancel_tests.erl

**File**: `test/wf_cancel_tests.erl`
**Changes**: Insert setup/cleanup functions after line 12 (after include directives, before test helpers)

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

**Why after line 12:**
- Keeps setup functions grouped separately from test logic
- Follows module structure: includes → setup → helpers → tests
- Matches pattern from `wf_state_tests.erl:8-19`

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Setup function defined: Check file contains `setup() ->` after line 12
- [ ] Cleanup function defined: Check file contains `cleanup(_Pid) ->` after setup function

**Note**: This phase only adds setup functions; tests will still fail until Phase 2.

---

### Phase 2: Create Master Test Wrapper

#### Overview

Create a master test generator that wraps all existing test generators with the setup/teardown functions. This is the core fix that ensures the ETS table is initialized before any test runs.

#### Changes Required:

##### 1. test/wf_cancel_tests.erl

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add master test generator at the end of file (after all existing test generators, before EOF)

```erlang
%%====================================================================
%% Master Test Suite with Setup
%%====================================================================

wf_cancel_master_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"wf_cancel_compiles", fun wf_cancel_compiles_test_/0},
         {"is_cancelled_active_scope", fun is_cancelled_active_scope_test_/0},
         {"is_cancelled_cancelled_scope", fun is_cancelled_cancelled_scope_test_/0},
         {"is_cancelled_undefined_scope", fun is_cancelled_undefined_scope_test_/0},
         {"propagate_marks_tokens_in_scope", fun propagate_marks_tokens_in_scope_test_/0},
         {"propagate_preserves_fields", fun propagate_preserves_fields_test_/0},
         {"cancel_region", fun cancel_region_test_/0},
         {"cancel_region_undefined_scope", fun cancel_region_undefined_scope_test_/0},
         {"cancel_region_already_cancelled", fun cancel_region_already_cancelled_test_/0},
         {"cancel_region_preserves_other_scopes", fun cancel_region_preserves_other_scopes_test_/0},
         {"cancel_region_complexity", fun cancel_region_complexity_test_/0},
         {"cancel_activity_without_effect", fun cancel_activity_without_effect_test_/0},
         {"cancel_activity_undefined_task", fun cancel_activity_undefined_task_test_/0},
         {"cancel_activity_preserves_other_tasks", fun cancel_activity_preserves_other_tasks_test_/0},
         {"cancel_activity_already_cancelled", fun cancel_activity_already_cancelled_test_/0},
         {"cancel_activity_invariants", fun cancel_activity_invariants_test_/0},
         {"cancel_case", fun cancel_case_test_/0},
         {"cancel_case_status", fun cancel_case_status_test_/0},
         {"cancel_case_already_cancelled", fun cancel_case_already_cancelled_test_/0},
         {"cancel_case_all_tokens", fun cancel_case_all_tokens_test_/0},
         {"cancel_case_many_tokens", fun cancel_case_many_tokens_test_/0},
         {"cancel_propagate", fun cancel_propagate_test_/0},
         {"nested_scope_cancellation", fun nested_scope_cancellation_test_/0},
         {"cancel_empty_scope", fun cancel_empty_scope_test_/0},
         {"cancel_single_token", fun cancel_single_token_test_/0},
         {"cancel_preserves_values", fun cancel_preserves_values_test_/0},
         {"verify_scope_isolation", fun verify_scope_isolation_test_/0},
         {"verify_no_orphaned_tokens", fun verify_no_orphaned_tokens_test_/0},
         {"bench_small_scope", fun bench_small_scope_test_/0},
         {"bench_large_scope", fun bench_large_scope_test_/0},
         {"bench_complexity_isolation", fun bench_complexity_isolation_test_/0}
     ]
    }.
```

**Why this approach:**
- Keeps all existing test generators unchanged (minimizes risk)
- Master wrapper references each test as `fun test_name_/0`
- Test names are preserved for clear test output
- Descriptive strings provide human-readable test names in output

**Alternative considered but rejected:**
- Converting all tests to regular functions (removing `_test_` suffix)
- Rejected because it requires changing 30+ function names
- Current approach uses existing test generators as-is

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] All tests pass: `rebar3 eunit` shows 0 failures, 0 cancelled
- [ ] No ets:lookup/2 badarg errors: Check test output doesn't contain this error
- [ ] Master test generator defined: Check file contains `wf_cancel_master_test_() ->` wrapper

##### Manual Verification:

- [ ] Run `rebar3 eunit` and verify output shows all 30 tests passing
- [ ] Verify setup function is called (check for "wf_state" gen_server in output if logged)
- [ ] Verify no test crashes with badarg error
- [ ] Verify all test names appear in output with descriptive labels

**Note**: Complete all automated verification, then confirm tests pass before proceeding.

---

### Phase 3: Verify Test Isolation and Edge Cases

#### Overview

Verify that the fix works correctly and that tests don't interfere with each other through the shared ETS table. Also verify edge cases like nested setup (the `cancel_region_test_()` that already has its own setup wrapper).

#### Changes Required:

##### 1. Verification Tests

**File**: No code changes
**Actions**: Run specific tests and verify behavior

**Edge case to verify:**
- `cancel_region_test_()` (lines 117-129) has its own `{setup, ...}` wrapper for test data
- This nested setup should work correctly within the master setup wrapper
- EUnit allows setup within setup - the inner setup creates test state, outer setup starts gen_server

**Test isolation to verify:**
- Each test uses unique `case_id` via `make_ref()` in `wf_state:new/1`
- Tests don't share data through ETS table
- Running all tests together produces same results as running individually

#### Success Criteria:

##### Automated Verification:

- [ ] All tests pass when run together: `rebar3 eunit --module wf_cancel_tests`
- [ ] All tests pass when run individually: `rebar3 eunit --module wf_cancel_tests --test is_cancelled_active_scope_test_` (repeat for each test)
- [ ] No test interference: Run test suite twice and verify same results
- [ ] Nested setup works: Verify `cancel_region_test_()` passes despite having its own setup wrapper

##### Manual Verification:

- [ ] Run specific test: `rebar3 eunit --module wf_cancel_tests --test is_cancelled_active_scope_test_`
- [ ] Run all tests: `rebar3 eunit` and verify 0 failures, 0 cancelled
- [ ] Check ETS table not growing unbounded: Start Erlang shell, run tests multiple times, verify memory stable
- [ ] Verify gen_server process count: Check only one `wf_state` process exists (not started multiple times)

**Note**: This phase is verification only; no code changes expected unless issues are discovered.

---

## Testing Strategy

### Unit Tests:

- **Setup/correctness**: Verify `setup()` function starts gen_server successfully
- **Idempotency**: Verify `setup()` can be called multiple times without error (checks `whereis` first)
- **Test isolation**: Verify each test uses unique `case_id` and doesn't interfere
- **Nested setup**: Verify `cancel_region_test_()` works with both master setup and its own data setup

### Integration Tests:

- **Full test suite**: Run `rebar3 eunit` to verify all tests pass
- **Test module isolation**: Run `rebar3 eunit --module wf_cancel_tests` to verify fix doesn't break other test modules
- **Multiple runs**: Run test suite multiple times to verify no state leaks between runs

### Manual Testing Steps:

1. **Verify baseline failure** (before fix):
   ```bash
   cd /Users/speed/wf-substrate
   rebar3 eunit --module wf_cancel_tests
   # Should see ets:lookup/2 badarg errors
   ```

2. **Apply fix** (Phases 1-2):
   - Add setup/cleanup functions after line 12
   - Add master test wrapper at end of file
   - Compile to check for syntax errors

3. **Verify success** (after fix):
   ```bash
   rebar3 eunit --module wf_cancel_tests
   # Should show 0 failures, 0 cancelled
   ```

4. **Verify no regressions**:
   ```bash
   rebar3 eunit
   # Run entire test suite, verify all modules pass
   ```

5. **Verify test isolation**:
   ```bash
   # Run tests multiple times
   for i in {1..5}; do rebar3 eunit --module wf_cancel_tests; done
   # All runs should succeed
   ```

## Migration Notes

**No migration needed** - this is a test-only change with no impact on production code or data.

**Test execution impact:**
- Tests now require `wf_state` gen_server to be running
- Gen_server is started once and lives for entire test suite
- No cleanup needed between tests (shared ETS table is safe)

**Rollback strategy:**
- If issues arise, remove master wrapper and setup functions
- Tests will return to previous failing state
- No production impact since this is test-only

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/024-fix-runtime-test-failure-in-testwfcanceltestserl-t/research.md`
- Pattern to follow: `test/wf_state_tests.erl:8-53` (setup/cleanup and master wrapper)
- ETS table creation: `src/wf_state.erl:175-182` (gen_server init callback)
- ETS table usage: `src/wf_state.erl:228` (new/1), `src/wf_state.erl:240-245` (state persistence)
- Test file to modify: `test/wf_cancel_tests.erl:1-505` (entire file)
- Helper that triggers failure: `test/wf_cancel_tests.erl:15-41` (create_test_state/0)
- Example of nested setup: `test/wf_cancel_tests.erl:117-129` (cancel_region_test_())
