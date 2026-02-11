# Implement CI pipeline and acceptance tests Implementation Plan

## Implementation Plan Title

CI Pipeline and Acceptance Test Suite for wf-substrate

## Overview

Implement a comprehensive CI pipeline and acceptance test suite for the wf_substrate Erlang workflow execution engine. The CI pipeline will automate compilation, testing, type checking, and acceptance validation to ensure code quality and end-to-end functionality. This work establishes the infrastructure needed for continuous integration and provides confidence that workflow execution, validation, determinism, and performance characteristics meet requirements.

The solution consists of two main components:
1. **GitHub Actions CI workflow**: Automates build, test, and validation on every push/PR
2. **Acceptance test runner** (wf_acceptance.erl): Validates 7 end-to-end criteria covering unit tests, deadlock detection, cancellation, determinism, replay, benchmarks, and documentation

## Current State

**Existing Infrastructure:**
- rebar.config:1-30 - Build configuration with `warnings_as_errors` (line 2) and Dialyzer settings (lines 17-23)
- Comprehensive EUnit test suite: ~50 test functions across 10+ test modules (wf_test_seq.erl, wf_test_par.erl, wf_test_xor.erl, wf_test_join.erl, wf_test_cancel.erl, wf_test_mi.erl, wf_test_determinism.erl, wf_test_replay.erl, plus core module tests)
- Test helpers: wf_test_helpers.erl (exec_until_done/1, exec_steps/2, token inspection), wf_test_trace_helpers.erl (run_with_trace/3, compare_traces/2)
- Validation engine: wf_validate.erl with check_deadlock/1 (lines 627-644) and check_soundness/1 (lines 646-652)
- Benchmark harness: wf_bench.erl with run_all/0 (lines 180-190) that formats table output

**What's Missing:**
- No CI infrastructure: No .github/workflows/ directory, no GitHub Actions, CircleCI, or other automation
- No acceptance test runner: No wf_acceptance.erl or equivalent script
- Incomplete documentation: ARCHITECTURE.md and SEMANTICS.md are missing (only PATTERNS.md, TESTING.md, OPERATIONS.md exist)
- Stubbed features: Cancellation (wf_exec.erl:512-529) and replay (wf_test_replay.erl:14-63) are not implemented

**Key Constraints:**
- Must use Erlang/OTP 26+ (rebar.config:3)
- Must maintain warnings_as_errors for compilation (rebar.config:2)
- Must work with existing rebar3 build system
- Must handle stubbed features gracefully (skip with clear messaging, not fail)

## Desired End State

**CI Pipeline (.github/workflows/ci.yml):**
- Runs on every push and pull request to main/wreckit branches
- Compiles with zero warnings (warnings_as_errors enforced)
- Runs full EUnit test suite (rebar3 eunit)
- Runs Dialyzer type checking (rebar3 dialyzer)
- Executes acceptance test suite
- Provides clear pass/fail status with detailed logs
- Caches Dialyzer PLT for faster builds
- Runs on ubuntu-latest with Erlang/OTP 26+

**Acceptance Test Runner (src/wf_acceptance.erl):**
- Callable via escript: `escript wf_acceptance.erl` or `rebar3 escript main`
- Validates 7 criteria:
  1. Unit tests pass (all wf_test_*.erl modules)
  2. Deadlock detection works (validates known-deadlocked workflow)
  3. Cancellation correctness (SKIPPED - stubbed, reports as pending)
  4. Determinism (identical traces on repeated runs)
  5. Replay (SKIPPED - scheduler not integrated, reports as pending)
  6. Benchmarks complete (wf_bench:run_all/0 executes without error)
  7. Documentation complete (all 5 required docs exist and are non-empty)
- Prints formatted summary report with pass/skip/fail per criterion
- Exits 0 on full pass (all pass/skip), 1 on any failure
- Runs in < 60 seconds total

**Developer Experience:**
- `rebar3 compile` fails on warnings (already configured)
- `rebar3 eunit` runs all tests (already works)
- `rebar3 dialyzer` performs type checking (already configured)
- `escript scripts/acceptance.erl` runs acceptance tests locally
- CI provides same validation as local commands

### Key Discoveries:

- **wf_validate.erl:24-31** contains `mock_bytecode_deadlock()` - perfect for acceptance test criterion #2
- **wf_test_determinism.erl:11-15** shows pattern for running workflow twice and comparing traces
- **wf_test_trace_helpers.erl:67-98** provides `run_with_trace/3` for trace collection with deterministic scheduler
- **wf_test_cancel.erl** has 7 test functions but cancellation is stubbed in wf_exec.erl:514-518, must SKIP this criterion
- **wf_test_replay.erl** has all 7 tests skipped with message "Pending scheduler integration in wf_exec:run/3", must SKIP this criterion
- **wf_bench.erl:180-190** `run_all/0` prints table to stdout and returns `ok`, some benchmarks return 0 steps (stubbed)
- **rebar.config:2** has `warnings_as_errors` already set, just need to ensure it runs in CI
- **test/wf_validate_tests.erl:24-31** shows deadlocked bytecode pattern: PAR_FORK without proper JOIN_WAIT

## What We're NOT Doing

- **NOT implementing cancellation or replay features** - These are tracked in separate items (008, etc.), acceptance runner will SKIP these criteria
- **NOT creating missing ARCHITECTURE.md or SEMANTICS.md** - Documentation check will FAIL if these are missing, which is intentional to track the debt
- **NOT fixing existing Dialyzer warnings** - If warnings exist, CI will fail, prompting fixes as part of this item or follow-up
- **NOT implementing Common Test suites** - No _SUITE.erl files exist, only EUnit tests are run
- **NOT adding CI for other platforms** - Only ubuntu-latest, no macOS/Windows matrices
- **NOT adding performance regression detection** - Benchmarks must complete but no threshold enforcement
- **NOT implementing scheduler integration for replay** - This is tracked separately, acceptance test will SKIP

## Implementation Approach

**Strategy: Incremental CI infrastructure with pragmatic acceptance testing**

The implementation follows a layered approach:
1. **CI Pipeline Foundation**: Create GitHub Actions workflow that compiles, tests, and type-checks in sequence
2. **Acceptance Test Runner**: Implement as Erlang module (not shell script) for consistency with codebase, easier testing, and better error handling
3. **Stubbed Feature Handling**: Use SKIP status with clear messaging for cancellation and replay (not blocking)
4. **Documentation Enforcement**: Check for all 5 required docs, FAIL if missing (tracks technical debt visibly)
5. **Local-First Development**: Acceptance runner executable locally before CI, enabling fast feedback

**Technical Decisions:**
- **Erlang module over shell script**: Easier to test, consistent with codebase patterns, better error messages
- **Escript wrapper**: Allows `escript scripts/acceptance.erl` for local execution without compilation
- **rebar3 compile vs escript**: Use escript for acceptance runner to avoid adding to application supervisor
- **GitHub Actions over generic CI**: Best for open source project, provides caching and parallel execution
- **Dialyzer as required step**: Type safety is critical for systems programming language like Erlang
- **Graceful SKIP handling**: Stubbed features report SKIP with clear reason, don't fail CI

---

## Phases

### Phase 1: GitHub Actions CI Pipeline

#### Overview

Create .github/workflows/ci.yml that automates compilation, EUnit tests, Dialyzer type checking, and acceptance tests. This provides continuous validation on every push and pull request.

#### Changes Required:

##### 1. GitHub Actions Workflow

**File**: `.github/workflows/ci.yml`
**Changes**: Create new file with multi-stage CI pipeline

```yaml
name: CI

on:
  push:
    branches: [ main, wreckit/* ]
  pull_request:
    branches: [ main, wreckit/* ]

jobs:
  compile-and-test:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: 'latest'

      - name: Compile
        run: rebar3 compile
        # Fails if warnings due to warnings_as_errors in rebar.config

      - name: EUnit Tests
        run: rebar3 eunit

      - name: Dialyzer
        run: rebar3 dialyzer

      - name: Acceptance Tests
        run: rebar3 escript main
```

**Rationale**: Sequential execution ensures fast feedback (compile fails early), Dialyzer catches type errors, acceptance tests validate end-to-end criteria. Caching can be added later if Dialyzer becomes slow.

##### 2. Update rebar.config for escript support

**File**: `rebar.config`
**Changes**: Add escript section to support acceptance runner

```erlang
{escript_main_app, wf_substrate}.
{escript_name, "wf_acceptance"}.
{escript_emu_args, "%%! -escript main wf_acceptance\n"}.
```

**Alternative**: Keep acceptance runner standalone as `scripts/acceptance.erl` called via `escript scripts/acceptance.erl`, avoiding rebar.config changes. **Recommended approach** to minimize configuration changes.

#### Success Criteria:

##### Automated Verification:

- [ ] GitHub Actions workflow runs without syntax errors
- [ ] Compile step succeeds (or fails with clear error if warnings exist)
- [ ] EUnit tests pass (all ~50 tests)
- [ ] Dialyzer completes (may report warnings, that's OK for now)
- [ ] Acceptance test step executes (even if some criteria fail)

##### Manual Verification:

- [ ] Push to branch triggers CI workflow
- [ ] PR to main shows CI status
- [ ] CI logs are readable and show test counts
- [ ] Failed builds have clear error messages

**Note**: Complete all automated verification, then verify CI triggers on actual push/PR before proceeding to Phase 2.

---

### Phase 2: Acceptance Test Runner Structure

#### Overview

Implement wf_acceptance.erl module with main/1 function for escript entry point. Create structure for running all 7 acceptance criteria and printing summary report.

#### Changes Required:

##### 1. Acceptance Test Module

**File**: `src/wf_acceptance.erl`
**Changes**: Create new module with escript entry point

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/wf_substrate/ebin

-mode(compile).
-include_lib("kernel/include/logger.hrl").

main(_Args) ->
    io:format("~n=== Acceptance Tests ===~n~n"),

    Results = [
        {unit_tests, check_unit_tests()},
        {deadlock_detection, check_deadlock_detection()},
        {cancellation_correctness, check_cancellation()},
        {determinism, check_determinism()},
        {replay, check_replay()},
        {benchmarks, check_benchmarks()},
        {documentation, check_documentation()}
    ],

    print_summary(Results),

    ExitCode = case [R || {_, Result} <- Results, Result =:= fail] of
        [] -> 0;
        _ -> 1
    end,
    halt(ExitCode).

print_summary(Results) ->
    io:format("~n=== Acceptance Test Summary ===~n"),
    lists:foreach(fun({Name, Result}) ->
        Format = "~-50s ~s~n",
        Label = atom_to_list(Name),
        Status = case Result of
            pass -> "\e[32mPASS\e[0m";
            skip -> "\e[33mSKIP\e[0m";
            fail -> "\e[31mFAIL\e[0m"
        end,
        io:format(Format, [Label, Status])
    end, Results),

    Passed = length([R || {_, R} <- Results, R =:= pass]),
    Skipped = length([R || {_, R} <- Results, R =:= skip]),
    Failed = length([R || {_, R} <- Results, R =:= fail]),

    io:format("~nResult: ~p (~p passed, ~p skipped, ~p failed)~n",
              [case Failed of 0 -> pass; _ -> fail end, Passed, Skipped, Failed]),

    ok.
```

**Rationale**: Uses escript format for standalone execution, ANSI colors for readability, exit code 0/1 for CI integration.

##### 2. Stub Check Functions

**File**: `src/wf_acceptance.erl`
**Changes**: Add placeholder check functions for each criterion

```erlang
check_unit_tests() -> pass.
check_deadlock_detection() -> pass.
check_cancellation() -> pass.
check_determinism() -> pass.
check_replay() -> pass.
check_benchmarks() -> pass.
check_documentation() -> pass.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles without warnings
- [ ] `escript src/wf_acceptance.erl` runs and prints summary
- [ ] All 7 criteria show PASS
- [ ] Exit code is 0 when all pass
- [ ] Exit code is 1 when any fail (test by making one return `fail`)

##### Manual Verification:

- [ ] Output is readable and formatted correctly
- [ ] ANSI colors work in terminal
- [ ] Runtime is < 5 seconds (stubbed)

**Note**: Complete all automated verification, then proceed to Phase 3 to implement actual checks.

---

### Phase 3: Unit Tests and Deadlock Detection Criteria

#### Overview

Implement the first two acceptance criteria: unit test execution and deadlock detection. These are the most critical validation checks and demonstrate the pattern for other criteria.

#### Changes Required:

##### 1. Unit Tests Criterion

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_unit_tests/0

```erlang
check_unit_tests() ->
    io:format("  Running unit tests...~n"),

    %% Run rebar3 eunit and capture output
    Output = os:cmd("rebar3 eunit"),

    %% Check for success indicators
    case string:find(Output, "All") =/= nomatch andalso
         string:find(Output, "passed") =/= nomatch of
        true ->
            %% Extract test count
            case parse_test_count(Output) of
                {ok, Count} ->
                    io:format("  PASS: ~p tests passed~n", [Count]),
                    pass;
                error ->
                    io:format("  PASS: Tests passed (count unknown)~n"),
                    pass
            end;
        false ->
            io:format("  FAIL: Unit tests failed~n"),
            io:format("  Output: ~s~n", Output),
            fail
    end.

parse_test_count(Output) ->
    %% Look for "All N tests passed"
    case re:run(Output, "All (\\d+) tests? passed", [{capture, all, list}]) of
        {match, [_, CountStr]} ->
            {ok, list_to_integer(CountStr)};
        _ ->
            error
    end.
```

**Rationale**: Uses os:cmd/1 to run rebar3, parses output for success message, extracts count for reporting. More reliable than spawning Erlang processes.

##### 2. Deadlock Detection Criterion

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_deadlock_detection/0

```erlang
check_deadlock_detection() ->
    io:format("  Checking deadlock detection...~n"),

    %% Create known-deadlocked bytecode
    DeadlockedBytecode = [
        {'PAR_FORK', [2, 4]},
        {'TASK_EXEC', a},
        {'DONE'},
        {'TASK_EXEC', b},
        {'DONE'}
    ],

    %% Run validation
    case wf_validate:validate(DeadlockedBytecode) of
        {ok, Report} ->
            %% Check if deadlock was detected
            Issues = wf_validate:get_issues(Report),
            DeadlockIssues = [I || I <- Issues, wf_validate:get_issue_type(I) =:= deadlock],

            case DeadlockIssues of
                [_|_] ->
                    io:format("  PASS: Deadlock correctly detected (~p issue(s))~n",
                              [length(DeadlockIssues)]),
                    pass;
                [] ->
                    io:format("  FAIL: Deadlock not detected~n"),
                    fail
            end;
        {error, Error} ->
            io:format("  FAIL: Validation error: ~p~n", [Error]),
            fail
    end.
```

**Rationale**: Uses wf_validate:validate/1 to run static analysis, checks for deadlock issues. Bytecode pattern from wf_validate_tests.erl:24-31.

**Note**: Must verify wf_validate exports get_issues/1 and get_issue_type/1. If not, inspect record directly:
```erlang
DeadlockIssues = [I || I <- Issues, I#issue.type =:= deadlock],
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit test check executes rebar3 eunit and returns pass
- [ ] Deadlock check creates deadlocked bytecode and detects it
- [ ] Both criteria print PASS in summary
- [ ] Deadlock check fails if validation doesn't detect deadlock (test by using valid bytecode)

##### Manual Verification:

- [ ] Unit test output shows actual test count (e.g., "47 tests passed")
- [ ] Deadlock detection runs in < 2 seconds
- [ ] Error messages are clear if checks fail

**Note**: Complete all automated verification, then proceed to Phase 4.

---

### Phase 4: Cancellation and Determinism Criteria

#### Overview

Implement cancellation correctness and determinism criteria. Cancellation will SKIP (stubbed), determinism will validate trace comparison.

#### Changes Required:

##### 1. Cancellation Correctness Criterion (SKIP)

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_cancellation/0 with skip

```erlang
check_cancellation() ->
    io:format("  Checking cancellation correctness...~n"),

    %% Cancellation is stubbed in wf_exec.erl:514-518
    %% is_scope_cancelled/2 always returns false
    %% propagate_cancellation/2 is a no-op
    io:format("  SKIP: Cancellation implementation stubbed (wf_exec.erl:514-518)~n"),
    skip.
```

**Rationale**: Clear skip with file reference tracks the debt. Alternative: Run tests and expect them to pass (but they don't test runtime behavior). Better to skip and be explicit.

##### 2. Determinism Criterion

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_determinism/0

```erlang
check_determinism() ->
    io:format("  Checking determinism...~n"),

    %% Create simple sequential workflow
    Bytecode = [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ],

    %% Run twice with deterministic scheduler
    try
        {_, Events1} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
        {_, Events2} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),

        %% Compare traces
        case wf_test_trace_helpers:compare_traces(Events1, Events2) of
            ok ->
                io:format("  PASS: Traces are identical (~p events)~n", [length(Events1)]),
                pass;
            {error, Diff} ->
                io:format("  FAIL: Traces differ: ~p~n", [Diff]),
                fail
        end
    catch
        Type:Error:Stack ->
            io:format("  FAIL: Exception: ~p:~p~n  Stack: ~p~n", [Type, Error, Stack]),
            fail
    end.
```

**Rationale**: Uses existing test helpers (wf_test_trace_helpers) for consistency. Pattern from wf_test_determinism.erl:11-15. Sequential workflow is simplest deterministic case.

#### Success Criteria:

##### Automated Verification:

- [ ] Cancellation check returns SKIP with clear message
- [ ] Determinism check runs workflow twice and compares traces
- [ ] Determinism check returns PASS (traces are identical)
- [ ] Determinism check fails if traces differ (test by modifying bytecode between runs)

##### Manual Verification:

- [ ] Determinism check completes in < 5 seconds
- [ ] Error handling catches exceptions gracefully
- [ ] Trace comparison shows event count on pass

**Note**: Complete all automated verification, then proceed to Phase 5.

---

### Phase 5: Replay and Benchmarks Criteria

#### Overview

Implement replay (SKIP) and benchmarks criteria. Replay is not implemented yet, benchmarks must complete successfully.

#### Changes Required:

##### 1. Replay Criterion (SKIP)

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_replay/0 with skip

```erlang
check_replay() ->
    io:format("  Checking replay...~n"),

    %% Replay requires scheduler integration in wf_exec:run/3
    %% All wf_test_replay.erl tests are skipped
    %% See wf_test_trace_helpers.erl:105-111 extract_scheduler_choices/1 returns []
    io:format("  SKIP: Replay scheduler not integrated (wf_test_replay.erl:14-63)~n"),
    skip.
```

**Rationale**: Replay tests are all skipped in wf_test_replay.erl with clear reason. References specific test module lines.

##### 2. Benchmarks Criterion

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_benchmarks/0

```erlang
check_benchmarks() ->
    io:format("  Running benchmarks...~n"),

    %% Run all benchmarks and capture output
    try
        Result = wf_bench:run_all(),

        case Result of
            ok ->
                %% Check for stubbed benchmarks (0 steps)
                %% This is OK, we just verify completion
                io:format("  PASS: Benchmarks completed~n"),
                pass;
            Error ->
                io:format("  FAIL: Benchmark error: ~p~n", [Error]),
                fail
        end
    catch
        Type:Error:Stack ->
            io:format("  FAIL: Benchmark exception: ~p:~p~n  Stack: ~p~n",
                      [Type, Error, Stack]),
            fail
    end.
```

**Rationale**: wf_bench:run_all/0 prints table and returns ok. Stubbed benchmarks show "SKIPPED" in output but don't fail. We only verify completion, not results.

#### Success Criteria:

##### Automated Verification:

- [ ] Replay check returns SKIP with clear message
- [ ] Benchmarks check calls wf_bench:run_all/0
- [ ] Benchmarks check returns PASS (completes without error)
- [ ] Benchmarks check fails if wf_bench throws exception

##### Manual Verification:

- [ ] Benchmarks print table to stdout during test
- [ ] Benchmarks complete in < 30 seconds (most time is in seq_throughput with 10k tasks)
- [ ] Stubbed benchmarks show SKIPPED in output

**Note**: Complete all automated verification, then proceed to Phase 6.

---

### Phase 6: Documentation Criterion

#### Overview

Implement documentation completeness check. Verifies all 5 required documentation files exist and are non-empty.

#### Changes Required:

##### 1. Documentation Criterion

**File**: `src/wf_acceptance.erl`
**Changes**: Implement check_documentation/0

```erlang
check_documentation() ->
    io:format("  Checking documentation...~n"),

    RequiredDocs = [
        {"docs/ARCHITECTURE.md", "Architecture overview"},
        {"docs/SEMANTICS.md", "Workflow semantics"},
        {"docs/PATTERNS.md", "Workflow patterns"},
        {"docs/TESTING.md", "Testing guide"},
        {"docs/OPERATIONS.md", "Operations guide"}
    ],

    Results = lists:map(fun({Path, Description}) ->
        Exists = filelib:is_file(Path),
        case Exists of
            true ->
                {ok, Info} = file:read_file_info(Path),
                Size = Info#file_info.size,
                case Size > 0 of
                    true ->
                        io:format("  ✓ ~s (~.0f bytes)~n", [Path, Size]),
                        {Path, ok};
                    false ->
                        io:format("  ✗ ~s (empty file)~n", [Path]),
                        {Path, empty}
                end;
            false ->
                io:format("  ✗ ~s (not found)~n", [Path]),
                {Path, missing}
        end
    end, RequiredDocs),

    %% Check for any failures
    Failed = [P || {P, Status} <- Results, Status =/= ok],

    case Failed of
        [] ->
            io:format("  PASS: All documentation present~n"),
            pass;
        _ ->
            io:format("  FAIL: ~p documentation file(s) missing or empty~n", [length(Failed)]),
            fail
    end.
```

**Rationale**: Checks file existence and size > 0. Reports each file with checkmark/cross. Fails if any missing or empty (tracks technical debt).

#### Success Criteria:

##### Automated Verification:

- [ ] Documentation check exists all 5 files
- [ ] Files with content report PASS
- [ ] Missing or empty files report FAIL with count
- [ ] Output shows file sizes

##### Manual Verification:

- [ ] Existing docs (PATTERNS.md, TESTING.md, OPERATIONS.md) show checkmarks
- [ ] Missing docs (ARCHITECTURE.md, SEMANTICS.md) show crosses
- [ ] Total check fails due to missing files

**Note**: Complete all automated verification, then proceed to Phase 7.

---

### Phase 7: Integration and Local Testing

#### Overview

Integrate acceptance runner with CI pipeline, add local execution support, and verify end-to-end functionality.

#### Changes Required:

##### 1. Update GitHub Actions to use acceptance runner

**File**: `.github/workflows/ci.yml`
**Changes**: Modify acceptance test step

```yaml
      - name: Acceptance Tests
        run: escript src/wf_acceptance.erl
```

**Rationale**: Direct escript execution, no rebar3 dependency for acceptance runner.

##### 2. Make acceptance runner executable (shell wrapper)

**File**: `scripts/acceptance.sh`
**Changes**: Create shell wrapper for convenience

```bash
#!/bin/bash
# Convenience wrapper for acceptance tests
set -e

cd "$(dirname "$0")/.."
escript src/wf_acceptance.erl "$@"
```

```bash
chmod +x scripts/acceptance.sh
```

**Rationale**: Provides `./scripts/acceptance.sh` for local testing, easier to remember than escript path.

##### 3. Update rebar.config (optional)

**File**: `rebar.config`
**Changes**: Add shell script to profiles (optional)

```erlang
{shell, [
    {scripts, ["scripts/acceptance.sh"]}
]}.
```

**Alternative**: Skip this change, keep scripts standalone.

#### Success Criteria:

##### Automated Verification:

- [ ] `escript src/wf_acceptance.erl` runs from project root
- [ ] `./scripts/acceptance.sh` runs and produces same output
- [ ] CI workflow calls acceptance runner and shows summary
- [ ] CI passes if all criteria pass or skip (excluding FAIL)

##### Manual Verification:

- [ ] Push to wreckit branch triggers CI
- [ ] CI logs show acceptance test summary with colored output
- [ ] CI passes (or fails with clear reason if docs missing)
- [ ] Local execution matches CI execution

**Note**: Complete all automated verification, then verify end-to-end with actual CI run.

---

## Testing Strategy

### Unit Tests:

- Test each check function in isolation with mock data
- Test failure modes (e.g., deadlock not detected, traces differ)
- Test exception handling (catch clauses in all checks)
- Test exit code logic (0 for pass/skip, 1 for fail)
- Test output formatting (summary report, ANSI colors)

### Integration Tests:

- Run acceptance runner against actual wf_substrate codebase
- Verify all criteria execute in < 60 seconds total
- Verify CI workflow runs on push/PR
- Verify local execution matches CI execution
- Test with missing docs (should FAIL)
- Test with broken unit test (should FAIL)

### Manual Testing Steps:

1. **Local acceptance test execution**:
   ```bash
   cd /Users/speed/wf-substrate
   escript src/wf_acceptance.erl
   # Verify summary shows PASS/SKIP/FAIL for each criterion
   # Verify exit code: echo $? (should be 0 or 1)
   ```

2. **Convenience script execution**:
   ```bash
   ./scripts/acceptance.sh
   # Should produce identical output to escript
   ```

3. **CI workflow execution**:
   ```bash
   git push wreckit/020-ci-and-acceptance
   # Open GitHub Actions tab, verify workflow runs
   # Check logs for acceptance test summary
   ```

4. **Failure scenario testing**:
   ```bash
   # Temporarily break a unit test
   # Push and verify CI fails with clear error
   # Verify acceptance test catches it in criterion #1
   ```

5. **Documentation debt tracking**:
   ```bash
   # Verify docs/ARCHITECTURE.md and docs/SEMANTICS.md show as FAIL
   # Create placeholder docs, verify they then show as PASS
   ```

## Migration Notes

**No data migration required** - This is new infrastructure, no existing systems to migrate.

**Developer workflow changes**:
- Before pushing, run `./scripts/acceptance.sh` to catch issues early
- CI will automatically run acceptance tests on every push
- Failed builds must be fixed before merging

**CI workflow addition**:
- Add `.github/workflows/ci.yml` to repo
- No git hooks or pre-commit hooks required (CI provides validation)

**Acceptance test runner deployment**:
- Add `src/wf_acceptance.erl` to source tree
- Add `scripts/acceptance.sh` to source tree
- No application code changes required

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/020-ci-and-acceptance/research.md`
- Build config: `rebar.config:1-30` (warnings_as_errors, Dialyzer settings)
- Validation engine: `src/wf_validate.erl:609-652` (check_deadlock/1, check_soundness/1)
- Test helpers: `test/wf_test_helpers.erl:1-62`, `test/wf_test_trace_helpers.erl:1-193`
- Determinism tests: `test/wf_test_determinism.erl:11-15` (trace comparison pattern)
- Benchmarks: `src/wf_bench.erl:180-190` (run_all/0)
- Cancellation stub: `src/wf_exec.erl:512-529` (is_scope_cancelled/2, propagate_cancellation/2)
- Replay tests: `test/wf_test_replay.erl:14-63` (all skipped)
- Deadlock mock: `test/wf_validate_tests.erl:24-31` (mock_bytecode_deadlock/0)
