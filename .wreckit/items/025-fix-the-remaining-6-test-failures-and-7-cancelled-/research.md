# Research: Fix the remaining 6 test failures and 7 cancelled tests in rebar3 eunit. The specific failures are:

**Date**: 2026-02-11
**Item**: 025-fix-the-remaining-6-test-failures-and-7-cancelled-

## Research Question

What are the specific test failures and cancelled tests in the rebar3 eunit test suite, and what is required to fix them to achieve 0 failures and 0 cancelled tests?

## Summary

Based on investigation of the codebase, previous item completion summaries, and test structure, this task addresses remaining test failures after items 023, 024, and 030. Item 024 successfully fixed the primary ETS initialization issue in `wf_cancel_tests.erl` by adding setup/cleanup functions and a master test wrapper, achieving a 93.5% pass rate (29/31 tests). The remaining 2 failures in item 024 were identified as:

1. **verify_scope_isolation test** - The `wf_cancel:verify_scope_isolation/2` function at `src/wf_cancel.erl:357-367` returns `{error, {tokens_corrupted, [...]}}` when there are active tokens outside the cancelled scope, but this is the expected behavior (tokens in other scopes should remain active). The test expectation or the validation logic appears incorrect.

2. **bench_complexity_isolation test** - A timing-dependent performance test that expects cancellation of 10 tokens in a 10,000-token state to complete in < 1ms. The threshold may be too strict for the test environment.

The "6 test failures and 7 cancelled tests" mentioned in the title likely refer to failures across the broader test suite beyond `wf_cancel_tests.erl`. These would need to be identified by running `rebar3 eunit` and analyzing the specific failure patterns. Common failure categories in this codebase include:
- ETS table initialization issues (similar to item 024)
- State mutation and validation bugs (as seen in item 024's US-003)
- Test assertion vs test generator pattern mismatches
- Timing-dependent performance tests

## Current State Analysis

### Existing Implementation

**Test Infrastructure:**
- **Test framework**: EUnit (standard Erlang unit test framework)
- **Test profile**: Configured in `rebar.config:25-28` with test dependencies (proper) and compile options
- **Test organization**: 35 test modules in `test/` directory (re-enabled in item 022)
- **Test execution**: Run via `rebar3 eunit` (see `.github/workflows/ci.yml:26-27`)

**Known Fixes Applied (Items 023, 024, 030):**
- **Item 024**: Fixed ETS initialization in `wf_cancel_tests.erl` by adding setup/cleanup functions (lines 14-26) and master test wrapper (lines 511-548)
- **Item 024**: Fixed `wf_state:commit/1` persistence bug where AppliedState with buffered mutations was saved instead of FinalState with cleared buffer
- **Item 024**: Fixed scope-token relationship maintenance in `wf_state:enter_scope/2`, `add_token/2`, and `remove_token/2`
- **Item 024**: Fixed `wf_cancel:do_cancel_activity/2` double-wrapping return value bug
- **Item 022**: Re-enabled 33 disabled test files by fixing compilation errors (include paths, opcode case, deprecated functions)
- **Item 021**: Fixed all compilation errors and warnings to enable clean test execution

**Remaining Known Issues (from Item 024):**

From `/Users/speed/wf-substrate/.wreckit/items/024-fix-runtime-test-failure-in-testwfcanceltestserl-t/progress.log:113-125`:

1. **verify_scope_isolation test failure**:
   - Location: `test/wf_cancel_tests.erl:465-472`
   - Issue: Test calls `wf_cancel:verify_scope_isolation/2` after cancelling scope1, but validation function returns error when active tokens exist in other scopes (scope2)
   - Validation logic: `src/wf_cancel.erl:357-367` returns `{error, {tokens_corrupted, ActiveTokensOutsideScope}}` if any active tokens exist outside cancelled scope
   - Expected behavior: Tokens in scope2 should remain active after scope1 is cancelled
   - Possible fix: Either test expectation is wrong, or validation function logic is incorrect

2. **bench_complexity_isolation test failure**:
   - Location: `test/wf_cancel_tests.erl:499-505`
   - Issue: Timing-dependent test expects < 1ms (1000 microseconds) for cancelling 10 tokens in 10,000-token state
   - Threshold: `?assert(Time < 1000)` at line 505
   - Possible fix: Increase threshold to 5-10ms to account for test environment variability, or run multiple times and use average

### Test Module Structure

**EUnit Test Generator Pattern:**
```erlang
%% Master test wrapper with setup (from wf_cancel_tests.erl:511-548)
wf_cancel_master_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"test_name", fun test_function/0},
         %% ... more tests
     ]
    }.
```

**Setup Pattern (from wf_cancel_tests.erl:14-26):**
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
    %% Clear ETS table to prevent test interference
    ets:delete_all_objects(wf_state_store),
    ok.
```

**Key Pattern**: Test generators ending in `_test_()` are auto-exported by EUnit. Functions referenced in master wrapper should be regular functions (not generators) ending in `/0`, not `_/0`.

## Key Files

### Test Files (likely candidates for failures)

- `test/wf_cancel_tests.erl:1-548` - **Primary focus** - 31 tests for cancellation semantics, 2 known failures (verify_scope_isolation, bench_complexity_isolation)
- `test/wf_test_join.erl` - Join policy tests, may have failures related to branch counting and synchronization
- `test/wf_test_par.erl` - Parallel split tests, may have timing-dependent failures
- `test/wf_test_xor.erl` - Exclusive choice tests, may have failures in branch selection
- `test/wf_test_seq.erl` - Sequential composition tests
- `test/wf_test_cancel.erl` - Cancellation pattern tests (distinct from wf_cancel_tests.erl)
- `test/wf_test_mi.erl` - Multiple instance tests
- `test/wf_exec_tests.erl` - Executor implementation tests
- `test/wf_sched_tests.erl` - Scheduler policy tests
- `test/wf_state_tests.erl` - State management tests (has correct setup pattern to follow)

### Source Files (potential bug locations)

- `src/wf_cancel.erl:357-367` - **verify_scope_isolation/2** - Returns error when active tokens exist outside cancelled scope (logic may be incorrect)
- `src/wf_cancel.erl:369-384` - **verify_scope_nesting/2** - Validates child scope cancellation
- `src/wf_cancel.erl:386-401` - **verify_no_orphaned_tokens/2** - Validates cancelled tokens are in cancelled scope
- `src/wf_state.erl:340-381` - **commit/1** - Fixed in item 024 US-003 to persist FinalState instead of AppliedState
- `src/wf_state.erl:555-577` - **enter_scope/2** - Fixed in item 024 US-003 to populate scope's token list
- `src/wf_exec.erl:131` - **find_branch_for_token/2** - Has badmatch error noted in item 023 progress log

### Configuration Files

- `rebar.config:1-34` - Test profile configuration, EUnit options
- `.github/workflows/ci.yml:1-34` - CI pipeline that runs `rebar3 eunit` at line 27

### Documentation

- `docs/TESTING.md:1-613` - Comprehensive testing guide with test patterns, setup examples, and execution commands
- `/Users/speed/wf-substrate/.wreckit/items/022-re-enable-all-33-disabled-test-files-in-test-that-/COMPLETION_SUMMARY.md:1-195` - Lists all 35 re-enabled test modules

## Technical Considerations

### Dependencies

**Internal Dependencies:**
- **wf_state gen_server**: Must be started before any tests that use ETS table (creates `wf_state_store` table)
- **wf_cancel module**: Provides cancellation primitives and validation functions
- **wf_exec module**: Executor implementation for running bytecode
- **wf_vm module**: Type definitions and opcodes
- **Include headers**: `wf_state.hrl`, `wf_cancel.hrl`, `wf_exec.hrl` must be included in test files

**External Dependencies:**
- **eunit**: Built-in Erlang test framework (included in OTP)
- **proper**: Property-based testing framework (in test deps, may not be actively used)
- **timer**: Erlang standard library for timing measurements (used in performance tests)

### Patterns to Follow

**EUnit Setup Pattern (from wf_state_tests.erl:8-19):**
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
    %% Don't stop gen_server, let it live for all tests
    %% Optionally clear ETS table
    ok.
```

**Master Test Wrapper Pattern (from wf_cancel_tests.erl:511-548):**
- All test functions should be regular functions (not generators) ending in `/0`
- Master wrapper returns `{setup, fun setup/0, fun cleanup/1, [TestList]}`
- Test list contains `{"Test Name", fun test_function/0}` tuples
- Tests use `?assert*` macros (not `?_assert*`) for direct execution

**Test Isolation Pattern:**
- Each test should use unique `case_id` (via `make_ref()` or `wf_state:new/1`)
- ETS table should be cleared between tests if needed (via `ets:delete_all_objects/1`)
- Gen_server should remain running across all tests (not stopped in cleanup)

**State Mutation Pattern:**
```erlang
%% Correct pattern from wf_state.erl:340-381
commit(State) ->
    AppliedState = apply_mutations(State),
    FinalState = State#state{buffered_mutations = []},  %% Clear buffer
    ets:insert(wf_state_store, FinalState),  %% Persist cleared state
    {ok, FinalState, Receipt}.
```

### Common Failure Patterns

**1. ETS Table Not Initialized:**
- **Symptom**: `ets:lookup/2 badarg` error
- **Cause**: Test calls `wf_state:*` functions without starting `wf_state` gen_server
- **Fix**: Add setup/cleanup functions and master test wrapper (as done in item 024)

**2. Test Generator vs Direct Function:**
- **Symptom**: Tests not running, or wrong assertion style
- **Cause**: Using `?_assert*` (for generators) instead of `?assert*` (for direct execution)
- **Fix**: Convert test generators to regular functions or use correct assertion style

**3. State Mutation Replay Bug:**
- **Symptom**: Validation errors like `{scope_already_exists, ...}` or `{token_already_exists, ...}`
- **Cause**: State persisted with buffered mutations, then restored and mutations re-applied
- **Fix**: Persist FinalState with cleared buffer (as fixed in item 024 US-003)

**4. Scope-Token Relationship Missing:**
- **Symptom**: Cancellation fails to find tokens in scope
- **Cause**: Tokens added to state but not added to scope's token list
- **Fix**: Update `enter_scope`, `add_token`, `remove_token` to maintain scope-token lists (as fixed in item 024 US-003)

**5. Timing-Dependent Performance Tests:**
- **Symptom**: Test passes sometimes, fails other times
- **Cause**: Strict time threshold (< 1ms) that test environment can't guarantee
- **Fix**: Increase threshold, run multiple times and average, or remove strict timing requirement

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Test environment variability causes flaky failures** | Medium | Use relaxed thresholds for performance tests, run tests multiple times and average results |
| **Fixing test masks actual bug in production code** | High | Verify each failure carefully - determine if test expectation is wrong or code has bug. Don't disable tests, fix underlying issue |
| **ETS table cleanup causes test interference** | Medium | Ensure each test uses unique `case_id`, clear ETS table in cleanup or between tests |
| **Gen_server already started from previous test run** | Low | Setup function checks `whereis(wf_state)` and reuses existing process |
| **Performance test thresholds too strict** | Low | Increase thresholds to 5-10ms to account for CI/CD environment variability |
| **Verification function logic is incorrect** | Medium | Review `wf_cancel:verify_scope_isolation/2` logic - should active tokens in other scopes be allowed? If yes, fix function. If no, fix test |
| **Cascading failures from one module affect others** | High | Run tests in isolation (`rebar3 eunit --module=module_name`) to identify specific failure patterns |
| **Test fixes introduce regressions** | Medium | Run full test suite after each fix to ensure no new failures introduced |

## Recommended Approach

### High-Level Strategy

1. **Run full test suite** to identify all 6 failures and 7 cancelled tests:
   ```bash
   cd /Users/speed/wf-substrate
   rebar3 eunit 2>&1 | tee test_execution.log
   ```
   Analyze output to catalog specific test names, failure reasons, and cancellation causes.

2. **Categorize failures** by root cause:
   - **Category A: ETS initialization issues** - Similar to item 024, missing setup/cleanup
   - **Category B: State mutation bugs** - Similar to item 024 US-003, persistence or replay issues
   - **Category C: Test assertion bugs** - Wrong expectations (e.g., verify_scope_isolation)
   - **Category D: Timing-dependent failures** - Performance tests with strict thresholds
   - **Category E: Cancelled tests** - Tests skipped due to compilation errors, missing dependencies, or test setup issues

3. **Fix by category**:
   - **Category A**: Add setup/cleanup functions and master test wrappers (follow `wf_cancel_tests.erl:14-26, 511-548` pattern)
   - **Category B**: Debug state mutations, fix persistence/replay logic (review `wf_state:commit/1` fix from item 024)
   - **Category C**: Verify if test expectation or production code is wrong, fix accordingly (e.g., `wf_cancel:verify_scope_isolation/2`)
   - **Category D**: Increase thresholds or relax timing requirements (e.g., change `?assert(Time < 1000)` to `?assert(Time < 5000)`)
   - **Category E**: Investigate why tests are cancelled (missing functions, compilation errors, test setup issues)

4. **Verify all fixes**:
   ```bash
   # Run individual test modules
   rebar3 eunit --module=wf_cancel_tests
   rebar3 eunit --module=wf_test_join
   rebar3 eunit --module=wf_test_par
   # ... etc for each failing module

   # Run full suite
   rebar3 eunit

   # Verify 0 failures, 0 cancelled
   ```

5. **Document findings** in completion summary with:
   - List of all 6 failures with root causes
   - List of all 7 cancelled tests with reasons
   - Specific fixes applied (file:line references)
   - Before/after test results

### Known Specific Fixes

**Fix 1: verify_scope_isolation test (if validation logic is wrong)**
- **File**: `src/wf_cancel.erl:357-367`
- **Current logic**: Returns error if ANY active tokens exist outside cancelled scope
- **Possible fix**: Change validation to only check for active tokens IN cancelled scope, not outside
- **Alternative fix**: Change test expectation to expect error instead of ok (if validation is correct)

**Fix 2: bench_complexity_isolation test**
- **File**: `test/wf_cancel_tests.erl:499-505`
- **Current threshold**: `?assert(Time < 1000)` (1ms)
- **Fix**: Increase to `?assert(Time < 5000)` or `?assert(Time < 10000)` (5-10ms)
- **Alternative**: Run multiple times and use average: `lists:min([timer:tc(fun() -> ... end) || _ <- lists:seq(1, 10)])`

### Investigation Commands

```bash
# 1. Run full test suite and capture output
rebar3 eunit 2>&1 | tee /tmp/eunit_output.log

# 2. Count failures and cancelled tests
grep -c "Failed" /tmp/eunit_output.log
grep -c "Cancelled" /tmp/eunit_output.log

# 3. Extract specific test names that failed
grep -A 5 "Failed:" /tmp/eunit_output.log

# 4. Run individual test modules to isolate failures
rebar3 eunit --module=wf_cancel_tests
rebar3 eunit --module=wf_test_join
rebar3 eunit --module=wf_test_par

# 5. Run with verbose output
rebar3 eunit -v

# 6. Check for compilation errors
rebar3 compile 2>&1 | grep -i error

# 7. Verify ETS table initialization
erl -eval "wf_state:start_link(), wf_state:new(#{}), halt()."
```

## Open Questions

1. **What are the specific 6 test failures?**
   - The item title mentions "6 test failures" but doesn't list them. Need to run `rebar3 eunit` to identify which specific tests in which modules are failing.

2. **What are the specific 7 cancelled tests?**
   - "Cancelled tests" in EUnit means tests that were skipped or couldn't run. Need to identify which tests and why (compilation errors? missing dependencies? test setup issues?).

3. **Is the `verify_scope_isolation/2` logic incorrect or is the test expectation wrong?**
   - The validation function at `src/wf_cancel.erl:357-367` returns error when active tokens exist outside cancelled scope. Should these tokens be allowed to remain active? If yes, fix the function. If no, fix the test.

4. **Are the 2 known failures from item 024 included in the "6 failures" count?**
   - Item 024 had 2 remaining failures (verify_scope_isolation, bench_complexity_isolation). Are these part of the 6, or are there 4 additional failures?

5. **Are failures concentrated in specific test modules or spread across many modules?**
   - If concentrated (e.g., all in wf_test_join), suggests common bug in that module's test pattern or the production code it tests.
   - If spread across modules, suggests systematic issues (ETS initialization, state mutation, etc.).

6. **Do any tests require gen_servers or dependencies that aren't being started?**
   - Similar to the ETS initialization issue in item 024, other tests may require `wf_state`, `wf_exec`, or other gen_servers to be running.

7. **Are there compilation errors preventing some tests from running (causing cancellation)?**
   - Item 022 fixed compilation errors, but new issues may have been introduced.

8. **Should performance tests with strict timing requirements be disabled or relaxed?**
   - The bench_complexity_isolation test failure is timing-dependent. Should thresholds be increased or should these tests be marked as non-critical?

9. **Is the CI/CD environment causing test failures that don't occur locally?**
   - CI environment (`.github/workflows/ci.yml:26-27`) runs tests. Resource constraints or timing differences may cause flaky tests.

10. **What is the acceptance criteria for this item?**
    - "0 failures and 0 cancelled" is the goal. Does this mean:
      - All tests must pass (100% success rate)?
      - Or are some tests allowed to be disabled if they're flaky or testing unimplemented features?

11. **Should tests that fail due to unimplemented features be temporarily disabled?**
    - Item 022 re-enabled 33 disabled tests. Some may fail because features (e.g., effect system, governance) aren't fully implemented.

12. **Are there any new test files added since item 022 that need to be included?**
    - Item 022 restored 35 test files. Have new tests been added that need investigation?

13. **Do the "7 cancelled tests" refer to EUnit-cancelled tests or test files that are disabled?**
    - EUnit has a specific "cancelled" status for tests that couldn't run. Need to clarify if this refers to EUnit cancellations or `.disabled` files.

14. **What is the timeline for fixing these tests?**
    - Is this urgent (blocking a release) or can it be done incrementally?

15. **Should test fixes be done in one PR or multiple incremental PRs?**
    - One large PR may be hard to review. Multiple smaller PRs per module or category may be better.

16. **Are there any race conditions or concurrency issues causing test failures?**
    - If tests run in parallel and share ETS tables or gen_servers, they may interfere.

17. **Do any tests depend on specific order or shared state?**
    - Test isolation is critical. If tests depend on order, they're fragile and should be fixed.

18. **Are there any memory leaks or resource leaks in test setup/teardown?**
    - ETS tables not being deleted, gen_servers not being stopped, processes not being killed.

19. **Should test coverage be increased after fixing failures?**
    - Item 014 unit and property tests aimed for >80% coverage. Is this still the goal?

20. **What is the rollback strategy if fixes introduce regressions?**
    - Item 024 has rollback SHA `2cc5255e0e92dd1d29e8c7fa755d70dd9f34f803`. Should this item also have a rollback strategy?
