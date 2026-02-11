# Research: Implement CI pipeline and acceptance tests

**Date**: 2025-01-10
**Item**: 020-ci-and-acceptance

## Research Question

Implement CI scripts and an acceptance test runner for the wf_substrate project.

CI pipeline (GitHub Actions or similar shell scripts):
1. rebar3 compile — verify clean compilation with no warnings (warnings_as_errors).
2. rebar3 eunit — run all EUnit test suites, fail on any test failure.
3. rebar3 ct — run Common Test suites if any (for integration tests).
4. rebar3 dialyzer — run Dialyzer for type checking (optional but recommended).
5. Acceptance test runner.

Acceptance test runner (wf_acceptance.erl or shell script) that validates the following end-to-end criteria:
1. All unit tests pass (wf_test_*.erl modules).
2. Validator correctly detects a known-deadlocked workflow (construct a deliberately deadlocked term, run wf_validate, assert deadlock is reported).
3. Cancellation correctness: run a workflow with cancel region, trigger cancel mid-execution, verify no scope corruption and proper cancel events.
4. Determinism: run same workflow twice with deterministic scheduler, assert identical traces.
5. Replay: run with nondeterministic scheduler, record, replay, verify match.
6. Benchmarks: run wf_bench:run_all/0, verify it completes without error, report metrics to stdout.
7. Documentation completeness: verify that docs/ARCHITECTURE.md, docs/SEMANTICS.md, docs/PATTERNS.md, docs/TESTING.md, and docs/OPERATIONS.md all exist and are non-empty.

The acceptance runner exits 0 on full pass, 1 on any failure, with a summary report of pass/fail per criterion.

## Summary

The wf_substrate project requires a CI pipeline and acceptance test suite to validate end-to-end functionality. The project currently has:
- **No CI infrastructure**: No GitHub Actions, CircleCI, or other CI configuration exists
- **Comprehensive test suite**: Extensive EUnit tests exist in test/wf_test_*.erl modules
- **Partial documentation**: Only 3 of 5 required docs exist (OPERATIONS.md, PATTERNS.md, TESTING.md; missing ARCHITECTURE.md and SEMANTICS.md)
- **Validation engine**: wf_validate.erl provides deadlock detection and workflow correctness checks
- **Benchmarks**: wf_bench.erl implements performance benchmarks but some are stubbed
- **Stubbed features**: Cancellation and replay tests exist but are skipped pending implementation

The CI pipeline needs to:
1. Compile with warnings_as_errors (already configured in rebar.config:2)
2. Run EUnit tests (~50 test functions across 10+ test modules)
3. Run Dialyzer type checking (already configured in rebar.config:17-23)
4. Execute acceptance tests validating 7 criteria above

The acceptance runner can be implemented as either:
- An Erlang module (wf_acceptance.erl) called via escript
- A shell script wrapper calling rebar3 and Erlang functions

## Current State Analysis

### Existing Implementation

**Build Configuration** (rebar.config):
- Line 2: `warnings_as_errors` enabled for strict compilation
- Lines 17-23: Dialyzer configured with warnings for error_handling, race_conditions, unmatched_returns
- Line 27: Test profile with debug_info enabled

**Test Infrastructure**:
- test/wf_test_seq.erl: 7 tests for sequential composition
- test/wf_test_par.erl: 9 tests for parallel fork/join
- test/wf_test_xor.erl: 7 tests for exclusive choice
- test/wf_test_join.erl: 11 tests for all join policies
- test/wf_test_cancel.erl: 7 tests for cancellation (stubbed implementation)
- test/wf_test_mi.erl: 6 tests for multiple instances (cannot run - MI_SPAWN opcode handler missing)
- test/wf_test_determinism.erl: 7 tests for deterministic execution
- test/wf_test_replay.erl: All tests skipped (pending scheduler integration)
- test/wf_validate_tests.erl: Unit tests for validation engine
- test/wf_exec_tests.erl, wf_sched_tests.erl, etc.: Core module tests

**Test Helpers**:
- test/wf_test_helpers.erl: exec_until_done/1, exec_steps/2, token status helpers
- test/wf_test_trace_helpers.erl: run_with_trace/3, compare_traces/2 for trace comparison

**Validation Engine** (src/wf_validate.erl):
- Lines 532-561: check_dead_transitions/1 - detects unreachable code
- Lines 563-589: check_option_to_complete/1 - verifies no livelocks
- Lines 591-607: check_proper_completion/1 - validates single terminal token
- Lines 609-633: check_deadlock/1 - detects active tokens with no enabled transitions
- Lines 635-652: check_soundness/1 - composite of all 5 checks

**Benchmark Module** (src/wf_bench.erl):
- Lines 87-89: bench_seq_throughput/0 - 10,000 task sequence
- Lines 99-101: bench_par_fork_join/0 - 100 parallel branches
- Lines 111-113: bench_discriminator_repeat/0 - 1,000 discriminator iterations
- Lines 127-129: bench_deep_nesting/0 - 100 nested levels
- Lines 143-152: bench_mi_instances/0 - STUBBED (pending item 009)
- Lines 167-176: bench_state_store/0 - STUBBED (pending items 006, 010)
- Line 180-190: run_all/0 - executes all benchmarks and formats table

**Cancellation Implementation** (src/wf_exec.erl):
- Lines 512-529: Stubbed implementation of is_scope_cancelled/2 and propagate_cancellation/2
- test/wf_test_cancel.erl: Tests exist but only verify bytecode structure, not runtime behavior
- src/wf_example_cancel_region.erl: Example workflow with cancellation region

**Determinism Testing**:
- test/wf_test_determinism.erl: Tests sequential, parallel, XOR, nested patterns for deterministic execution
- Uses wf_test_trace_helpers:run_with_trace/3 with deterministic scheduler policy
- Lines 11-15: determinism_simple_seq_test/0 example - runs twice, compares traces

**Replay Testing**:
- test/wf_test_replay.erl: All 7 test functions skipped
- Lines 14, 23, 27, 31, 39, 47, 55, 63: `{skip, "Pending scheduler integration in wf_exec:run/3"}`
- wf_trace.erl:182-224: to_replay_log/1 and from_replay_log/1 exist but execution is stubbed

**Documentation Status**:
- docs/OPERATIONS.md: EXISTS - 729 lines covering supervision, failure modes, telemetry, configuration, deployment
- docs/PATTERNS.md: EXISTS - 149 lines mapping 43 workflow patterns to implementation
- docs/TESTING.md: EXISTS - 613 lines covering test organization, property tests, coverage, CI
- docs/ARCHITECTURE.md: MISSING
- docs/SEMANTICS.md: MISSING

**CI Infrastructure**:
- No .github/workflows/ directory exists
- No Makefile, shell scripts, or other CI automation
- No existing acceptance test runner

## Key Files

### Core Modules
- `src/wf_exec.erl:1-100` - Executor implementation with new/1, step/2, run/3, is_done/1, is_blocked/1
- `src/wf_validate.erl:1-653` - Validation engine with deadlock detection (check_deadlock/1), soundness checks
- `src/wf_bench.erl:1-355` - Benchmark harness with run_all/0 that prints formatted table

### Test Modules
- `test/wf_test_determinism.erl:1-128` - 7 determinism tests using trace comparison
- `test/wf_test_cancel.erl:1-229` - 7 cancellation tests (runtime behavior stubbed)
- `test/wf_test_replay.erl:1-90` - 7 replay tests (all skipped pending scheduler)
- `test/wf_test_helpers.erl:1-62` - Helper functions for execution and token inspection
- `test/wf_test_trace_helpers.erl:1-193` - Trace collection and comparison utilities

### Configuration
- `rebar.config:1-30` - Build configuration with warnings_as_errors, dialyzer settings, test profile

### Documentation
- `docs/TESTING.md:569-613` - CI pipeline section (placeholder, no actual CI config exists)
- `docs/PATTERNS.md:1-149` - Pattern coverage table showing implemented vs stubbed features
- `docs/OPERATIONS.md:1-729` - Operations guide with monitoring and troubleshooting

## Technical Considerations

### Dependencies

**External Dependencies**:
- Erlang/OTP 26+ (required in rebar.config:3)
- rebar3 build tool
- EUnit (included in OTP)
- Dialyzer (included in OTP)
- Common Test (included in OTP)

**Internal Modules to Integrate With**:
- wf_validate:validate/1 for deadlock detection criterion
- wf_exec:run/3 for workflow execution
- wf_trace for trace collection and comparison
- wf_test_trace_helpers:compare_traces/2 for determinism verification
- wf_bench:run_all/0 for benchmark execution
- wf_test_cancel.erl for cancellation correctness tests
- wf_test_determinism.erl for determinism tests

### Patterns to Follow

**Existing Patterns**:
1. **Test Naming Convention**: All test modules use wf_test_<pattern>.erl naming
2. **Mock Bytecode Generators**: Each test module defines mock_bytecode_*() functions (e.g., wf_test_seq.erl:78-85)
3. **EUnit Test Structure**: Tests use -include_lib("eunit/include/eunit.hrl") and ?assert* macros
4. **Setup/Teardown**: Some tests use {setup, fun setup/0, fun cleanup/1, [...]}

**Exit Code Conventions**:
- Erlang convention: exit(0) for success, exit(1) for failure
- Shell script convention: return 0 for success, non-zero for failure
- Acceptance runner should follow both conventions

**Reporting Format**:
- wf_bench.erl:232-256 shows table formatting with io:format
- wf_validate.erl:175-192 shows report formatting with structured output

**Integration Points**:
1. **Unit Test Execution**: `rebar3 eunit` runs all wf_test_*.erl modules
2. **Common Test**: `rebar3 ct` for integration tests (currently none exist)
3. **Validation**: wf_validate:validate/1 returns {ok, Report} | {error, [Issue]}
4. **Tracing**: wf_trace:new/1 creates trace state, wf_trace:get_events/1 retrieves events
5. **Benchmarks**: wf_bench:run_all/0 prints table to stdout

### Known Limitations

1. **Cancellation Stubbed**: wf_exec.erl:512-529 - cancellation returns false/noop
2. **Scheduler Integration Missing**: wf_test_trace_helpers.erl:69 - scheduler policy parameter ignored
3. **Replay Tests Skipped**: All wf_test_replay.erl tests return {skip, "..."}
4. **MI Tests Cannot Run**: wf_test_mi.erl:11 references non-existent wf_mi.hrl, no MI_SPAWN handler
5. **Missing Documentation**: ARCHITECTURE.md and SEMANTICS.md don't exist yet
6. **No CT Suites**: No _SUITE.erl files in test/ directory

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|---------|------------|
| Stubbed cancellation and replay tests fail acceptance criteria | High | Make acceptance criteria conditional - skip tests for stubbed features, report as "SKIPPED (pending implementation)" |
| Missing ARCHITECTURE.md and SEMANTICS.md | High | Either create placeholder docs or make documentation check optional/future work |
| CI pipeline dependencies not available in minimal environments | Medium | Use OTP 26+ requirement, document Erlang installation in CI |
| Dialyzer warnings on existing code | Medium | Run Dialyzer first, address warnings before enforcing in CI |
| Long-running benchmarks block CI | Medium | Run benchmarks in separate job or with timeout, make non-blocking |
| Flaky determinism tests due to concurrent execution | Low | Use deterministic scheduler explicitly, isolate test processes |
| Shell script portability issues | Low | Use escript (Erlang script) for acceptance runner instead of bash |

## Recommended Approach

### Phase 1: CI Pipeline (GitHub Actions)

**File**: .github/workflows/ci.yml

1. **Compile Stage**:
   - Checkout code
   - Install Erlang/OTP 26+ using erlef/setup-beam@v1
   - Run `rebar3 compile` (warnings_as_errors already enabled in rebar.config:2)
   - Fail if any warnings

2. **Unit Test Stage**:
   - Run `rebar3 eunit`
   - Fail if any test fails
   - Parse output for test summary

3. **Type Check Stage** (optional but recommended):
   - Run `rebar3 dialyzer`
   - Fail on any type warnings
   - Cache PLT for faster builds

4. **Acceptance Test Stage**:
   - Compile acceptance runner (wf_acceptance.erl or shell script)
   - Execute acceptance tests
   - Generate summary report
   - Exit 0 on full pass, 1 on any failure

**Alternative**: Use shell script (scripts/ci.sh) for local testing before pushing

### Phase 2: Acceptance Test Runner

**Option A: Erlang Module (wf_acceptance.erl)**

```erlang
-module(wf_acceptance).
-export([run/0]).

run() ->
    io:format("~n=== Acceptance Tests ===~n"),
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
```

**Option B: Shell Script (scripts/acceptance.sh)**

```bash
#!/bin/bash
set -e

echo "=== Acceptance Tests ==="

# Run unit tests
echo "1. Unit tests..."
rebar3 eunit || { echo "FAIL: Unit tests"; exit 1; }
echo "PASS: Unit tests"

# Check deadlock detection
echo "2. Deadlock detection..."
erl -noshell -s wf_acceptance check_deadlock -s init stop || { echo "FAIL: Deadlock detection"; exit 1; }
echo "PASS: Deadlock detection"

# ... other criteria ...
```

**Recommended**: Use Erlang module (Option A) for consistency with rest of codebase, easier to test and maintain.

### Phase 3: Implement Acceptance Criteria

**1. Unit Tests Criterion**:
- Execute `rebar3 eunit` via erlang:open_port/2 or os:cmd/1
- Parse output for "All N tests passed" or similar
- Return pass/fail

**2. Deadlock Detection Criterion**:
- Create known-deadlocked bytecode (PAR_FORK without proper join)
- Call wf_validate:check_deadlock/1
- Assert at least one deadlock issue found
- Mock: test/wf_validate_tests.erl:24-31 (mock_bytecode_deadlock)

**3. Cancellation Correctness Criterion**:
- Run wf_test_cancel.erl test that exercises cancel region
- Verify scope stack remains consistent
- Check no memory leaks or orphaned tokens
- Limitation: Cancellation is stubbed, so this will be SKIP

**4. Determinism Criterion**:
- Run same workflow twice with deterministic scheduler
- Use wf_test_trace_helpers:run_with_trace/3
- Compare traces with wf_test_trace_helpers:compare_traces/2
- Assert traces are identical
- Example: wf_test_determinism.erl:11-15

**5. Replay Criterion**:
- Run workflow with trace collection
- Extract replay log via wf_trace:to_replay_log/1
- Replay workflow (currently stubbed)
- Limitation: Replay execution not implemented, so this will be SKIP

**6. Benchmarks Criterion**:
- Call wf_bench:run_all/0
- Verify it completes without error
- Check output contains benchmark metrics
- Note: Some benchmarks return 0 steps (STUBBED markers)

**7. Documentation Criterion**:
- Check file exists for each required doc:
  - docs/ARCHITECTURE.md
  - docs/SEMANTICS.md
  - docs/PATTERNS.md (exists)
  - docs/TESTING.md (exists)
  - docs/OPERATIONS.md (exists)
- Verify file size > 0 bytes
- Report missing docs as FAIL

### Phase 4: Output Format

**Summary Report**:
```
=== Acceptance Test Summary ===
1. Unit Tests...................... PASS (47 tests passed)
2. Deadlock Detection.............. PASS (1 deadlock found)
3. Cancellation Correctness...... SKIP (stubbed implementation)
4. Determinism................... PASS (traces identical)
5. Replay......................... SKIP (scheduler not integrated)
6. Benchmarks.................... PASS (4/6 benchmarks completed)
7. Documentation.................. FAIL (2 missing: ARCHITECTURE.md, SEMANTICS.md)

Result: FAIL (2 failed, 2 skipped, 3 passed)
```

**Exit Codes**:
- 0: All criteria passed (or all failures are acceptable skips)
- 1: One or more criteria failed

## Open Questions

1. **Documentation Requirement**: Should acceptance tests fail if ARCHITECTURE.md and SEMANTICS.md are missing, or should those be documented as future work?

2. **Stubbed Features**: How should acceptance runner handle stubbed cancellation and replay? Options:
   - Fail acceptance (blocking until implemented)
   - Skip with warning (acknowledge as TODO)
   - Create minimal implementations that pass basic checks

3. **Benchmark Failures**: If a benchmark is stubbed (returns 0 steps), should that be considered pass or fail?

4. **CI Platform**: Use GitHub Actions (recommended for open source) or provide generic shell script that works with any CI?

5. **Dialyzer Enforcement**: Should Dialyzer warnings block CI or just be reported? Current rebar.config has warnings configured but not enforced.

6. **Test Timeout**: What's acceptable timeout for full acceptance test suite? Benchmarks may take 30-60 seconds.

7. **Local Testing**: Should acceptance runner be executable locally (e.g., `rebar3 acceptance` or `make acceptance`) or only in CI environment?
