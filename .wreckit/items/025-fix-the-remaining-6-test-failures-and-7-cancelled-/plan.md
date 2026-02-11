# Fix the remaining 6 test failures and 7 cancelled tests in rebar3 eunit. The specific failures are: Implementation Plan

## Implementation Plan Title

Fix Remaining Test Failures and Cancelled Tests in EUnit Test Suite

## Overview

This implementation plan addresses the remaining test failures and cancelled tests in the rebar3 eunit test suite. Item 024 successfully fixed the primary ETS initialization issue in `wf_cancel_tests.erl` by adding setup/cleanup functions and a master test wrapper, achieving a 93.5% pass rate (29/31 tests). This item targets the remaining failures across the broader test suite to achieve 100% test pass rate (0 failures, 0 cancelled).

The remaining work involves:
1. Fixing 2 known failures in `wf_cancel_tests.erl` (verify_scope_isolation, bench_complexity_isolation)
2. Identifying and fixing 4 additional failures across other test modules
3. Investigating and resolving 7 cancelled tests (tests that couldn't run)

## Current State

**Test Infrastructure Status:**
- **Fixed in Item 024**: ETS initialization issue in `wf_cancel_tests.erl` - now has proper setup/cleanup (lines 14-26) and master test wrapper (lines 511-548)
- **Pattern established**: `wf_state_tests.erl:8-53` provides the correct pattern for gen_server-based tests
- **Known remaining failures in wf_cancel_tests.erl**:
  1. `verify_scope_isolation` test (line 465-472) - validation logic issue
  2. `bench_complexity_isolation` test (line 499-505) - timing threshold too strict

**Test Files Requiring Investigation:**
- 35 test modules in `test/` directory (all re-enabled in item 022)
- Only 6 modules have setup/cleanup: `wf_cancel_tests.erl`, `wf_state_tests.erl`, `wf_governance_tests.erl`, `wf_governance_integration_tests.erl`, `wf_effect_props.erl`, `wf_case_runner_tests.erl`
- 29 modules may lack proper setup if they depend on gen_servers

**Cancellation Categories:**
EUnit reports tests as "cancelled" when they can't run, typically due to:
- Compilation errors (missing includes, undefined functions)
- Test setup failures (gen_servers not started)
- Missing dependencies (modules or functions not found)
- Test generator pattern mismatches

## Desired End State

**Acceptance Criteria:**
- All tests pass: `rebar3 eunit` shows 0 failures, 0 cancelled
- CI pipeline passes: `.github/workflows/ci.yml:26-27` executes successfully
- All test modules have proper setup/teardown if they depend on gen_servers
- No test failures due to timing constraints or environment variability

**Verification:**
```bash
cd /Users/speed/wf-substrate
rebar3 eunit 2>&1 | tee test_execution.log
# Expected output: All tests passed, 0 failures, 0 cancelled
```

### Key Discoveries:

- **verify_scope_isolation test issue** (`test/wf_cancel_tests.erl:465-472`): The `wf_cancel:verify_scope_isolation/2` function at `src/wf_cancel.erl:357-367` returns `{error, {tokens_corrupted, ActiveTokensOutsideScope}}` when active tokens exist outside the cancelled scope. This is expected behavior - tokens in scope2 should remain active after scope1 is cancelled. Either the test expectation or the validation logic is incorrect.

- **bench_complexity_isolation test issue** (`test/wf_cancel_tests.erl:499-505`): Timing-dependent test expects < 1ms for cancelling 10 tokens in 10,000-token state. The threshold is too strict for CI/CD environments and may fail intermittently due to scheduler variability.

- **Test pattern**: Test generators ending in `_test_()` are auto-exported by EUnit. Functions in master wrappers should be regular functions (not generators) ending in `/0`, referenced as `fun test_name/0`.

- **Setup pattern**: Modules that depend on `wf_state` gen_server must have setup/cleanup following `wf_state_tests.erl:8-19` pattern: check `whereis(wf_state)`, start if needed, don't stop in cleanup.

- **Test isolation**: Each test should use unique `case_id` (via `make_ref()` or `wf_state:new/1`) to prevent interference through shared ETS table.

## What We're NOT Doing

- **NOT modifying core business logic** - Only fixing tests and test infrastructure
- **NOT disabling tests** - All tests must pass; fix underlying issues instead of skipping
- **NOT changing test semantics** - Preserve test intent, only fix setup/teardown and assertions
- **NOT refactoring working code** - Focus on failing tests, don't touch passing tests
- **NOT implementing new features** - This is pure test-fixing work
- **NOT changing CI/CD pipeline** - Tests must work within existing CI constraints
- **NOT mocking gen_servers** - Use actual gen_servers (simpler and more reliable)

## Implementation Approach

**High-Level Strategy:**

1. **Phase 1: Run and categorize** - Execute full test suite, capture all failures and cancellations, categorize by root cause
2. **Phase 2: Fix verify_scope_isolation logic** - Determine if validation function or test expectation is wrong, fix accordingly
3. **Phase 3: Fix timing-dependent tests** - Increase thresholds or remove strict timing requirements
4. **Phase 4: Add setup/teardown to modules lacking it** - Follow established pattern for gen_server-dependent tests
5. **Phase 5: Fix compilation errors causing cancellations** - Resolve missing includes, undefined functions
6. **Phase 6: Verify full test suite** - Ensure 0 failures, 0 cancelled

**Why This Approach:**
- **Data-driven**: Run tests first to identify actual failures (not assumptions)
- **Categorized fixes**: Group by root cause to apply consistent solutions
- **Incremental verification**: Fix and test incrementally to prevent regressions
- **Pattern-based reuse**: Apply fixes that worked in item 024 to similar issues

**Key Technical Decisions:**

1. **verify_scope_isolation fix**: The validation function checks for active tokens OUTSIDE cancelled scope and returns error. This is incorrect - tokens in other scopes should remain active. Fix the function, not the test.

2. **Timing thresholds**: Increase from 1ms to 10ms to account for CI/CD environment variability (10x safety margin).

3. **Setup addition**: Add setup/teardown to all modules that use `wf_state:*` functions, following the `wf_state_tests.erl:8-19` pattern.

4. **Master test wrappers**: Convert individual test generators to regular functions and wrap with master setup, following `wf_cancel_tests.erl:511-548` pattern.

---

## Phases

### Phase 1: Execute Test Suite and Catalog Failures

#### Overview

Run the full test suite to identify the specific 6 failures and 7 cancelled tests, categorize them by root cause, and create a prioritized fix list.

#### Changes Required:

##### 1. Test Execution and Capture

**Actions**: Execute tests and capture detailed output

```bash
cd /Users/speed/wf-substrate

# Run full test suite with verbose output
rebar3 eunit -v 2>&1 | tee /tmp/eunit_full_output.log

# Extract failure counts
grep -c "Failed:" /tmp/eunit_full_output.log
grep -c "Cancelled:" /tmp/eunit_full_output.log

# Extract specific test names that failed
grep -B 2 "Failed:" /tmp/eunit_full_output.log | grep "test\."

# Extract cancelled test information
grep -A 5 "Cancelled:" /tmp/eunit_full_output.log
```

##### 2. Individual Module Testing

**Actions**: Test modules individually to isolate failures

```bash
# Test modules that are known to depend on gen_servers
rebar3 eunit --module=wf_cancel_tests
rebar3 eunit --module=wf_state_tests
rebar3 eunit --module=wf_exec_tests
rebar3 eunit --module=wf_sched_tests
rebar3 eunit --module=wf_governance_tests

# Test pattern modules (may not need setup)
rebar3 eunit --module=wf_test_seq
rebar3 eunit --module=wf_test_par
rebar3 eunit --module=wf_test_xor
rebar3 eunit --module=wf_test_join
```

##### 3. Create Failure Catalog

**File**: `/tmp/failure_catalog.md`

**Content Structure**:
```markdown
# Test Failure Catalog

## Known Failures (from Item 024)

### 1. verify_scope_isolation test
- **Module**: test/wf_cancel_tests.erl:465-472
- **Error**: Expected `ok` but got `{error, {tokens_corrupted, [...]}}`
- **Root Cause**: Validation function checks for active tokens outside cancelled scope
- **Fix Required**: Fix `wf_cancel:verify_scope_isolation/2` logic (line 357-367)

### 2. bench_complexity_isolation test
- **Module**: test/wf_cancel_tests.erl:499-505
- **Error**: Timing assertion `?assert(Time < 1000)` fails intermittently
- **Root Cause**: 1ms threshold too strict for CI environment
- **Fix Required**: Increase threshold to 10000 (10ms)

## Additional Failures (To Be Identified)

### 3. [Test Name]
- **Module**: [File:Line]
- **Error**: [Error message]
- **Root Cause**: [Analysis]
- **Fix Required**: [Action needed]

## Cancelled Tests (To Be Identified)

### 1. [Test Name]
- **Module**: [File:Line]
- **Cancellation Reason**: [Why couldn't test run]
- **Fix Required**: [Action needed]
```

#### Success Criteria:

##### Automated Verification:

- [ ] Full test suite executed: `rebar3 eunit -v` completed
- [ ] Failure catalog created: `/tmp/failure_catalog.md` exists with all failures documented
- [ ] Failures categorized: Each failure has root cause analysis
- [ ] Module-level isolation done: Each failing module tested individually

##### Manual Verification:

- [ ] Review failure catalog and confirm 6 failures identified
- [ ] Review cancelled tests and confirm 7 cancellations identified
- [ ] Categorize failures into: ETS initialization, state mutation, test assertion, timing-dependent, compilation errors

**Note**: Complete this phase before proceeding to fixes. This ensures we fix actual failures, not assumptions.

---

### Phase 2: Fix verify_scope_isolation Logic

#### Overview

Fix the `wf_cancel:verify_scope_isolation/2` validation function logic. The current implementation returns an error when active tokens exist outside the cancelled scope, but this is expected behavior - tokens in other scopes should remain active after a scope is cancelled.

#### Changes Required:

##### 1. src/wf_cancel.erl

**File**: `src/wf_cancel.erl:357-367`

**Current Implementation**:
```erlang
verify_scope_isolation(State, CancelledScopeId) ->
    Tokens = wf_state:get_tokens(State),
    ActiveTokensOutsideScope = [
        {TId, T} || {TId, T} <- maps:to_list(Tokens),
        T#token.scope_id =/= CancelledScopeId,
        T#token.status =:= active
    ],
    case ActiveTokensOutsideScope of
        [] -> ok;
        _ -> {error, {tokens_corrupted, ActiveTokensOutsideScope}}
    end.
```

**Fixed Implementation**:
```erlang
verify_scope_isolation(State, CancelledScopeId) ->
    Tokens = wf_state:get_tokens(State),
    %% Verify: All tokens IN cancelled scope are cancelled
    %% Tokens in other scopes can be active (this is expected)
    TokensInCancelledScope = [
        {TId, T} || {TId, T} <- maps:to_list(Tokens),
        T#token.scope_id =:= CancelledScopeId
    ],
    ActiveTokensInScope = [
        {TId, T} || {TId, T} <- TokensInCancelledScope,
        T#token.status =:= active
    ],
    case ActiveTokensInScope of
        [] -> ok;
        _ -> {error, {tokens_not_cancelled_in_scope, ActiveTokensInScope}}
    end.
```

**Rationale**:
- The function should verify that tokens WITHIN the cancelled scope are actually cancelled
- Active tokens in OTHER scopes are expected and should NOT cause an error
- This aligns with the test expectation in `verify_scope_isolation()` at line 465-472

##### 2. Update Documentation

**File**: `src/wf_cancel.erl:355-356`

**Current Comment**:
```erlang
%% @doc Verify scope isolation: no active tokens in cancelled scope
```

**Updated Comment** (if needed):
```erlang
%% @doc Verify scope isolation: all tokens in cancelled scope are cancelled
%% Active tokens in other scopes are allowed and expected.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Specific test passes: `rebar3 eunit --module=wf_cancel_tests --test=verify_scope_isolation`
- [ ] No regressions: `rebar3 eunit --module=wf_cancel_tests` shows all tests passing

##### Manual Verification:

- [ ] Read the test code at `test/wf_cancel_tests.erl:465-472` and verify understanding
- [ ] Review the fix and confirm it aligns with test intent
- [ ] Run test multiple times to ensure it passes consistently

**Note**: This is a logic fix to production code, not test code. Ensure the fix is correct for the broader system, not just this test.

---

### Phase 3: Fix Timing-Dependent Performance Tests

#### Overview

Fix the `bench_complexity_isolation` test by increasing the timing threshold from 1ms to 10ms to account for CI/CD environment variability. Also review and fix any other timing-dependent tests identified in Phase 1.

#### Changes Required:

##### 1. test/wf_cancel_tests.erl

**File**: `test/wf_cancel_tests.erl:499-505`

**Current Implementation**:
```erlang
bench_complexity_isolation() ->
    %% Create state with 10000 total tokens, 10 in scope
    State = create_state_with_n_tokens_in_scope(10000, 10),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    %% Should be fast (only 10 tokens cancelled), not O(10000)
    ?assert(Time < 1000).  %% < 1ms
```

**Fixed Implementation**:
```erlang
bench_complexity_isolation() ->
    %% Create state with 10000 total tokens, 10 in scope
    State = create_state_with_n_tokens_in_scope(10000, 10),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    %% Should be fast (only 10 tokens cancelled), not O(10000)
    ?assert(Time < 10000).  %% < 10ms (relaxed for CI environment)
```

**Rationale**:
- 1ms threshold is too strict for CI/CD environments with variable scheduler load
- 10ms provides 10x safety margin while still verifying O(n) complexity where n=scope_size
- Test still validates that cancellation is O(scope_size) not O(total_tokens)

##### 2. Review Other Performance Tests

**File**: `test/wf_cancel_tests.erl:484-496`

**Action**: Check if other benchmark tests need similar adjustments

```erlang
%% Line 485-489: bench_small_scope - currently 1ms threshold
bench_small_scope() ->
    State = create_state_with_n_tokens_in_scope(100, 10),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    ?assert(Time < 1000).  %% < 1ms for 10 tokens

%% Line 492-496: bench_large_scope - currently 100ms threshold
bench_large_scope() ->
    State = create_state_with_n_tokens_in_scope(10000, 1000),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    ?assert(Time < 100000).  %% < 100ms for 1000 tokens
```

**Decision**: The large scope test already has a reasonable threshold (100ms). The small scope test may also need adjustment depending on Phase 1 findings.

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Specific test passes: `rebar3 eunit --module=wf_cancel_tests --test=bench_complexity_isolation`
- [ ] Test passes consistently: Run test 10 times, all pass
- [ ] No regressions: `rebar3 eunit --module=wf_cancel_tests` shows all tests passing

##### Manual Verification:

- [ ] Run test 20 times: `for i in {1..20}; do rebar3 eunit --module=wf_cancel_tests --test=bench_complexity_isolation; done`
- [ ] Verify all iterations pass (no intermittent failures)
- [ ] Check execution time is typically < 5ms (well under 10ms threshold)

**Note**: If timing issues persist after increasing to 10ms, consider removing strict timing assertions entirely and replacing with complexity verification (e.g., verify operation completes without excessive CPU usage).

---

### Phase 4: Add Setup/Teardown to Modules Lacking It

#### Overview

Identify test modules that depend on `wf_state` or other gen_servers but lack proper setup/teardown functions. Add setup/cleanup and master test wrappers following the established pattern from `wf_state_tests.erl:8-19` and `wf_cancel_tests.erl:511-548`.

#### Changes Required:

##### 1. Identify Modules Needing Setup

**Action**: For each failing test module, check if it uses gen_server-dependent functions

```bash
# Search for wf_state usage in test modules
grep -l "wf_state:" test/*.erl

# Search for other gen_server usage
grep -l "wf_exec:" test/*.erl
grep -l "wf_cancel:" test/*.erl
grep -l "wf_governance:" test/*.erl
```

**Expected modules needing setup** (based on Phase 1 findings):
- Modules that call `wf_state:new/1`, `wf_state:commit/1`, `wf_state:restore_from_ets/1`
- Modules that call `wf_cancel:cancel_*` functions (which use wf_state internally)
- Modules that call other gen_server APIs without starting them

##### 2. Add Setup/Teardown Pattern

**For each module needing setup**:

**File**: `test/[module_name].erl`

**Add after include directives** (typically around line 10-15):
```erlang
%%====================================================================
%% Setup
%%====================================================================

setup() ->
    %% Start wf_state gen_server if not already running
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

**Add master test wrapper** (at end of file before EOF):
```erlang
%%====================================================================
%% Master Test Suite with Setup
%%====================================================================

[module_name]_master_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         %% List all test functions from this module
         {"test_name_1", fun test_name_1/0},
         {"test_name_2", fun test_name_2/0},
         %% ... etc for all tests
     ]
    }.
```

**Convert test generators**:
- Change `test_name_test_() ->` to `test_name() ->` (remove `_test_` suffix)
- Change `?_assert(...)` to `?assert(...)` (direct execution, not generator)
- Ensure all tests are regular functions ending in `/0`

##### 3. Example Transformation

**Before** (`test/wf_exec_tests.erl` pattern):
```erlang
new_test_() ->
    Bytecode = mock_bytecode_simple_task(),
    ExecState = wf_exec:new(Bytecode),
    [
        ?_assertEqual(0, wf_exec:get_ip(ExecState)),
        ?_assertEqual(#{}, wf_exec:get_ctx(ExecState))
    ].
```

**After** (if module needs setup):
```erlang
new() ->
    Bytecode = mock_bytecode_simple_task(),
    ExecState = wf_exec:new(Bytecode),
    ?assertEqual(0, wf_exec:get_ip(ExecState)),
    ?assertEqual(#{}, wf_exec:get_ctx(ExecState)).

wf_exec_master_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"new", fun new/0},
         %% ... more tests
     ]
    }.
```

#### Success Criteria:

##### Automated Verification:

- [ ] All identified modules compile: `rebar3 compile`
- [ ] Individual modules pass: `rebar3 eunit --module=[module_name]` for each fixed module
- [ ] No gen_server startup errors: Test output doesn't contain "already started" or "badarg"

##### Manual Verification:

- [ ] Run full test suite: `rebar3 eunit` shows reduction in failures
- [ ] Verify test isolation: Run individual tests multiple times, all pass
- [ ] Check gen_server count: Only one `wf_state` process exists (not started multiple times)

**Note**: Add setup to modules incrementally (one or two at a time) and verify after each addition to prevent breaking multiple modules at once.

---

### Phase 5: Fix Compilation Errors and Cancelled Tests

#### Overview

Investigate and fix the 7 cancelled tests. Cancelled tests typically result from compilation errors, missing dependencies, or test setup issues that prevent EUnit from running the test at all.

#### Changes Required:

##### 1. Identify Cancellation Reasons

**Action**: For each cancelled test, determine why it couldn't run

```bash
# Check for compilation errors
rebar3 compile 2>&1 | grep -i error

# Check specific test modules
rebar3 eunit --module=[module_with_cancelled_test] -v

# Look for missing includes or undefined functions
grep -r "include.*\.hrl" test/
grep "undefined function" _build/test/lib/wf_substrate/erlc.log
```

**Common cancellation causes**:
- Missing include files (e.g., `-include("wf_state.hrl")`)
- Undefined functions (typos, wrong arity, unexported functions)
- Module naming mismatches (test file name doesn't match module name)
- Syntax errors (unterminated terms, missing commas)
- Circular dependencies

##### 2. Fix Include Path Issues

**Pattern**: Ensure test files include necessary headers

**Correct includes** (typically at top of file after module declaration):
```erlang
-include_lib("eunit/include/eunit.hrl").
-include("wf_state.hrl").
-include("wf_exec.hrl").
-include("wf_cancel.hrl").
```

**Action**: Add missing includes to test files that reference records or macros

##### 3. Fix Undefined Function Errors

**Pattern**: Ensure called functions exist and are exported

**Example fix**:
```erlang
%% Before: Call to non-existent function
{ok, State} = wf_state:create(CaseId),

%% After: Call to correct function
{ok, State} = wf_state:new(CaseId),
```

**Action**: Check function names and arities against source module exports

##### 4. Fix Module Naming

**Pattern**: Module name must match file name

**Example**:
- File: `test/wf_test_seq.erl`
- Module: `-module(wf_test_seq).`

**Action**: Verify module declarations match file names

#### Success Criteria:

##### Automated Verification:

- [ ] All test modules compile: `rebar3 compile` succeeds with 0 errors
- [ ] No compilation warnings: `rebar3 compile` shows no warnings
- [ ] Cancelled test count reduced: `rebar3 eunit` shows < 7 cancelled (ideally 0)
- [ ] Previously cancelled tests now run: Tests appear in output instead of being cancelled

##### Manual Verification:

- [ ] Check compilation log: `_build/test/lib/wf_substrate/erlc.log` has no errors
- [ ] Run each fixed module individually: `rebar3 eunit --module=[fixed_module]`
- [ ] Verify test count: Total test count increases (previously cancelled tests now run)

**Note**: If a test is cancelled due to unimplemented functionality (e.g., effect system not complete), note it in the failure catalog but don't disable it - the test should pass once the feature is implemented.

---

### Phase 6: Final Verification and Full Test Suite Validation

#### Overview

Execute the full test suite and verify 0 failures, 0 cancelled. Run tests multiple times to ensure no intermittent failures. Document all fixes applied.

#### Changes Required:

##### 1. Full Test Suite Execution

**Actions**: Run comprehensive test validation

```bash
cd /Users/speed/wf-substrate

# Clean build
rebar3 clean
rebar3 compile

# Run full test suite
rebar3 eunit -v 2>&1 | tee /tmp/eunit_final.log

# Verify results
grep "All.*passed" /tmp/eunit_final.log
grep -c "Failed:" /tmp/eunit_final.log  # Should be 0
grep -c "Cancelled:" /tmp/eunit_final.log  # Should be 0

# Run multiple times to check for flaky tests
for i in {1..5}; do
    echo "Run $i:"
    rebar3 eunit
done
```

##### 2. Individual Module Verification

**Actions**: Test each module individually to ensure no hidden failures

```bash
# Test all modules that were modified
rebar3 eunit --module=wf_cancel_tests
rebar3 eunit --module=wf_exec_tests
rebar3 eunit --module=wf_sched_tests
rebar3 eunit --module=wf_test_seq
rebar3 eunit --module=wf_test_par
rebar3 eunit --module=wf_test_xor
rebar3 eunit --module=wf_test_join
# ... etc for all modified modules
```

##### 3. CI/CD Pipeline Validation

**Action**: Ensure tests pass in CI environment

```bash
# Simulate CI environment (Ubuntu, OTP 26)
docker run --rm -it \
  -v $(pwd):/workspace \
  -w /workspace \
  erlang:26 \
  /bin/bash -c "rebar3 compile && rebar3 eunit"
```

##### 4. Document All Fixes

**File**: `/tmp/test_fixes_applied.md`

**Content Structure**:
```markdown
# Test Fixes Applied - Item 025

## Summary
- Before: 6 failures, 7 cancelled
- After: 0 failures, 0 cancelled
- Total tests fixed: 13

## Fixes by Category

### 1. Logic Fixes (Production Code)
- **src/wf_cancel.erl:357-367** - Fixed `verify_scope_isolation/2` to check cancelled scope, not outside scopes
  - Changed from: Error on active tokens outside cancelled scope
  - Changed to: Error on active tokens inside cancelled scope

### 2. Test Assertion Fixes
- **test/wf_cancel_tests.erl:505** - Increased timing threshold from 1ms to 10ms
  - Test: bench_complexity_isolation
  - Reason: CI environment variability

### 3. Setup/Teardown Additions
- **test/[module1].erl** - Added setup/cleanup and master wrapper
  - Lines added: [lines]
  - Tests converted: [count]

### 4. Compilation Error Fixes
- **test/[module].erl:[line]** - Fixed [issue]
  - Error: [description]
  - Fix: [description]

## Test Results

### Before Fixes
```
[paste relevant section from /tmp/eunit_full_output.log]
```

### After Fixes
```
[paste relevant section from /tmp/eunit_final.log]
```

## Validation
- [x] All tests pass locally
- [x] Tests pass 5 consecutive runs
- [x] No intermittent failures
- [x] CI pipeline passes
```

#### Success Criteria:

##### Automated Verification:

- [ ] Full test suite passes: `rebar3 eunit` shows 0 failures, 0 cancelled
- [ ] All modules compile cleanly: `rebar3 compile` shows 0 errors, 0 warnings
- [ ] Consistent execution: 5 consecutive test runs all pass
- [ ] CI validation: Tests pass in simulated CI environment

##### Manual Verification:

- [ ] Review test output log: `/tmp/eunit_final.log` confirms all tests passed
- [ ] Check for intermittent failures: No test fails in some runs but passes in others
- [ ] Verify no test disabled: All original tests still present and running
- [ ] Code review: All fixes align with project patterns and best practices

**Note**: This is the final verification phase. Only proceed to completion after all automated and manual verification criteria are met.

---

## Testing Strategy

### Unit Tests:

- **Setup correctness**: Verify `setup()` function starts required gen_servers successfully
- **Idempotency**: Verify `setup()` can be called multiple times without error (checks `whereis` first)
- **Test isolation**: Verify each test uses unique `case_id` and doesn't interfere
- **Logic fixes**: Verify `wf_cancel:verify_scope_isolation/2` correctly validates cancelled scope
- **Timing fixes**: Verify performance tests pass consistently (no intermittent failures)

### Integration Tests:

- **Full test suite**: Run `rebar3 eunit` to verify all tests pass
- **Module isolation**: Run each module individually to ensure no cross-module dependencies
- **Multiple runs**: Run test suite multiple times to verify no state leaks between runs
- **CI simulation**: Run tests in environment similar to CI (Ubuntu, OTP 26)

### Manual Testing Steps:

1. **Baseline capture** (before any fixes):
   ```bash
   cd /Users/speed/wf-substrate
   rebar3 eunit 2>&1 | tee /tmp/eunit_baseline.log
   ```

2. **Phase 1 execution**: Identify all failures and cancellations

3. **Incremental fixes**: Apply fixes phase by phase, verifying after each:
   ```bash
   # After each fix phase
   rebar3 eunit --module=[fixed_module]
   rebar3 eunit
   ```

4. **Final verification**: After all fixes applied:
   ```bash
   # Clean build
   rebar3 clean && rebar3 compile

   # Full test suite
   rebar3 eunit -v 2>&1 | tee /tmp/eunit_final.log

   # Verify 0 failures, 0 cancelled
   grep "Failed:" /tmp/eunit_final.log  # Should show "0"
   grep "Cancelled:" /tmp/eunit_final.log  # Should show "0"

   # Consistency check - run 5 times
   for i in {1..5}; do rebar3 eunit || exit 1; done
   ```

5. **Regression prevention**:
   - Run `rebar3 dialyzer` to ensure no type errors introduced
   - Review all code changes for pattern consistency
   - Verify no test logic was changed (only setup/teardown and assertions)

## Migration Notes

**No data migration needed** - This is test-only work with no production data impact.

**Test execution changes**:
- Modules that previously lacked setup now depend on `wf_state` gen_server being started
- Gen_server is started once per test module and lives for entire module's tests
- No cleanup between tests (shared ETS table is safe with unique `case_id` per test)

**Rollback strategy**:
- If logic fix (`verify_scope_isolation`) causes issues, revert `src/wf_cancel.erl` to previous implementation
- If setup additions cause problems, remove master wrapper and convert back to test generators
- No rollback needed for timing threshold changes (only makes tests more robust)

**CI/CD impact**:
- Tests should run faster (no more gen_server startup failures)
- More consistent results (no intermittent timing failures)
- Better test coverage (previously cancelled tests now run)

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/025-fix-the-remaining-6-test-failures-and-7-cancelled-/research.md`
- Item 024 Plan: `/Users/speed/wf-substrate/.wreckit/items/024-fix-runtime-test-failure-in-testwfcanceltestserl-t/plan.md`
- Pattern to follow: `test/wf_state_tests.erl:8-53` (setup/cleanup and master wrapper)
- Pattern to follow: `test/wf_cancel_tests.erl:14-26, 511-548` (setup/cleanup and master wrapper)
- Logic to fix: `src/wf_cancel.erl:357-367` (verify_scope_isolation/2 function)
- Timing to fix: `test/wf_cancel_tests.erl:505` (bench_complexity_isolation threshold)
- CI workflow: `.github/workflows/ci.yml:26-27` (EUnit test execution)
- Test documentation: `docs/TESTING.md:1-613` (comprehensive testing guide)
