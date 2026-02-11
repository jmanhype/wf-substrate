# Research: Implement unit and property-based test suites

**Date**: 2025-01-18
**Item**: 014-unit-and-property-tests

## Research Question

Implement comprehensive test suites using EUnit (no external test dependencies):

- wf_test_term.erl: test AST constructors and smart constructors. Verify type specs, structural validation (well_formed/1 accepts valid terms, rejects invalid), derived patterns produce expected term structures.
- wf_test_seq.erl: test sequential composition. Single sequence, nested sequences, sequence with effects, sequence with cancellation mid-way.
- wf_test_par.erl: test parallel split and synchronization. Par with 2/3/N branches, all branches complete, par with mixed completion times, par feeding into synchronizing merge.
- wf_test_xor.erl: test exclusive choice and simple merge. XOR with 2/3/N branches, signal selects correct branch, unselected branches are disabled, simple merge converges correctly.
- wf_test_join.erl: test all join policies. wait_all (all must complete), wait_n (N of M complete then cancel rest), first_complete (discriminator — first done cancels others). Verify join counter mechanics.
- wf_test_cancel.erl: test activity/case/region cancellation. Cancel running task, cancel case with active branches, cancel region preserves unrelated scopes, cancel with pending effects.
- wf_test_mi.erl: test multiple instance patterns. Fixed count, dynamic count, MI with wait_all, MI with wait_n, MI with first_complete, MI cancellation of remaining instances.

Minimal property-based testing framework (no PropEr dependency): implement a simple random term generator (random_term/1 with depth limit) and invariant checkers. Properties: any generated term passes well_formed/1, any well-formed term compiles without error, compiled bytecode has no unresolved labels, execution of any compiled term terminates within bounded steps.

## Summary

The wf-substrate project currently has basic EUnit test coverage for core modules (wf_state, wf_exec, wf_sched, wf_validate, wf_mi, wf_trace) but lacks comprehensive test suites for high-level workflow patterns. The task requires implementing 7 new test modules (wf_test_*.erl) to test workflow pattern constructs, plus a minimal property-based testing framework.

**Key findings:**
1. **No AST/term algebra exists yet**: wf_term.erl, wf_compile.erl, and wf_core.erl mentioned in PROMPT.md do not exist. Tests must work directly with bytecode (wf_vm:wf_bc()) or wait for these modules to be implemented.
2. **Existing test patterns**: Current tests use EUnit with setup/teardown, mock bytecode generators, and property-like tests using lists and comprehensions.
3. **Bytecode-based testing**: All existing tests work directly with bytecode tuples (e.g., `{'TASK_EXEC', task}`, `{'PAR_FORK', [1, 3]}`), not high-level pattern terms.
4. **No property-based testing framework**: No PropEr, Triq, or other PBT libraries are used. A custom generator will be needed.
5. **Testing infrastructure in place**: EUnit is configured in rebar.config, tests are organized in test/ directory, and test helpers already exist.

## Current State Analysis

### Existing Implementation

**Test Infrastructure:**
- **rebar.config:26-29**: Test profile configured with debug_info
- **test/** directory contains 7 test modules covering core functionality:
  - `wf_substrate_tests.erl:8-16`: Empty placeholder module
  - `wf_state_tests.erl:1-250`: Comprehensive state management tests with setup/teardown pattern
  - `wf_exec_tests.erl:1-346`: Executor tests with mock bytecode generators
  - `wf_sched_tests.erl:1-362`: Scheduler policy tests including property-like tests
  - `wf_validate_tests.erl:1-229`: Validation engine tests with state collection
  - `wf_mi_tests.erl:1-182`: Multiple instance tests with helper functions
  - `wf_trace_tests.erl:1-335`: Trace and replay tests

**Test Patterns Observed:**
1. **Setup/Teardown Pattern** (wf_state_tests.erl:8-49):
   ```erlang
   setup() -> ... %% Initialize gen_server
   cleanup(_Pid) -> ... %% Cleanup
   wf_state_test_() ->
       {setup, fun setup/0, fun cleanup/1, [Tests]}.
   ```

2. **Mock Bytecode Generators** (wf_exec_tests.erl:6-20):
   ```erlang
   mock_bytecode_simple_task() ->
       [{'TASK_EXEC', mock_task}, {'DONE'}].
   ```

3. **Test Generator Pattern** (wf_exec_tests.erl:26-33):
   ```erlang
   new_test_() ->
       Bytecode = mock_bytecode_simple_task(),
       ExecState = wf_exec:new(Bytecode),
       [?_assertEqual(0, wf_exec:get_ip(ExecState)), ...].
   ```

4. **Property-like Tests** (wf_sched_tests.erl:339-346):
   ```erlang
   prop_deterministic_reproducibility_test() ->
       %% Property: deterministic always returns same choice for same set
       Choices = [element(1, wf_sched:choose(Enabled, State)) || _ <- lists:seq(1, 100)],
       ?assertEqual(lists:usort(Choices), [hd(Choices)]).
   ```

**Missing Components:**
- **wf_term.erl**: AST/term algebra module (referenced in PROMPT.md:50 but not implemented)
- **wf_compile.erl**: Compiler from terms to bytecode (PROMPT.md:52)
- **wf_core.erl**: Kernel pattern basis and derived patterns (PROMPT.md:51)
- **wf_cancel.erl**: Cancellation semantics (PROMPT.md:57)
- **well_formed/1 function**: Validation function for AST terms (referenced in item requirements)

### Integration Points

**Core Modules to Test:**
- **wf_vm.erl:1-50**: Bytecode type definitions (wf_bc(), opcode(), join_policy(), loop_policy(), mi_policy())
- **wf_exec.erl:1-100+**: Executor with step/2, run/3, state management
- **wf_sched.erl**: Scheduling policies (deterministic, nondeterministic, replay)
- **wf_validate.erl:1-100+**: Static analysis and bounded model checking
- **wf_mi.erl:1-150+**: Multiple instance semantics (spawn_instances/4, check_join/1, cancel_remaining/2)
- **wf_state.erl:1-150+**: Transactional state store

**Record Definitions** (from .hrl files):
- **wf_exec.hrl:7-43**: token, branch_info, join_counter, exec_state records
- **wf_validate.hrl:7-29**: validation_state, issue, report records
- **wf_mi.hrl:8-23**: mi_instance, mi_config records

## Key Files

### Source Files

- **src/wf_vm.erl:1-50** - Bytecode type definitions
  - Defines wf_bc(), opcode(), join_policy(), loop_policy(), mi_policy()
  - Critical for understanding valid bytecode structure

- **src/wf_exec.erl:1-100+** - Executor implementation
  - new/1, step/2, run/3 functions
  - Token management and execution state
  - Tests already exist but can be extended

- **src/wf_sched.erl** - Scheduling policies
  - wf_sched_deterministic.erl, wf_sched_nondeterministic.erl, wf_sched_replay.erl
  - Comprehensive tests already exist (wf_sched_tests.erl)

- **src/wf_validate.erl:1-100+** - Validation engine
  - new/1, explore/2, validate/2
  - Checks: dead_transitions, proper_completion, deadlock, soundness

- **src/wf_mi.erl:1-150+** - Multiple instance patterns
  - spawn_instances/4, collect_result/3, check_join/1, cancel_remaining/2
  - eval_instance_count/2 for policy evaluation

- **src/wf_state.erl:1-150+** - State management
  - Transactional buffer-validate-commit protocol
  - Token and scope management

### Test Files (Existing)

- **test/wf_exec_tests.erl:1-346** - Executor tests
  - Mock bytecode generators (lines 6-68)
  - Single-token executor tests (lines 70-95)
  - Multi-token executor tests (lines 97-135)
  - Loop support tests (lines 137-181)
  - Cancellation support tests (lines 183-239)
  - Multiple instance tests (lines 241-345)

- **test/wf_sched_tests.erl:1-362** - Scheduler tests
  - Deterministic policy tests (lines 8-60)
  - Nondeterministic policy tests (lines 62-137)
  - Replay policy tests (lines 139-213)
  - Property-based tests (lines 336-362)

- **test/wf_validate_tests.erl:1-229** - Validation tests
  - Core data structure tests (lines 54-77)
  - Exploration engine tests (lines 80-141)
  - Correctness checks (lines 144-180)

- **test/wf_mi_tests.erl:1-182** - MI tests
  - eval_instance_count tests (lines 10-26)
  - spawn_instances tests (lines 31-52)
  - check_join tests (lines 58-77)
  - cancel_remaining tests (lines 83-94)

### Documentation

- **PROMPT.md:1-100+** - Project specification
  - Defines deliverables including wf_test_*.erl modules (line 63)
  - Specifies pattern coverage requirements (lines 74-91)
  - Outlines testing requirements (line 10)

- **docs/SCHEDULER.md:1-204** - Scheduler documentation
  - API reference and usage examples
  - Testing instructions

## Technical Considerations

### Dependencies

**External Dependencies:**
- **None**: rebar.config:13 explicitly sets `{deps, []}`
- **EUnit**: Standard library (include_lib("eunit/include/eunit.hrl"))
- **No PropEr/Triq**: Must implement custom property-based testing

**Internal Dependencies:**
- **wf_vm**: Bytecode type definitions (wf_vm:wf_bc(), wf_vm:opcode())
- **wf_exec**: Executor for running test workflows
- **wf_sched**: Scheduler policies for nondeterministic testing
- **wf_validate**: Validation engine for static analysis
- **wf_mi**: Multiple instance semantics
- **wf_state**: State management (for integration tests)

### Patterns to Follow

**Existing Test Conventions:**

1. **Module Organization:**
   - Test modules mirror source modules with `_tests.erl` suffix
   - Include record definitions from src/ directory (e.g., `-include("../src/wf_exec.hrl")`)

2. **Test Structure:**
   ```erlang
   -module(ModuleName).
   -include_lib("eunit/include/eunit.hrl").
   -include("../src/record_header.hrl").

   %% Mock bytecode generators
   mock_bytecode_pattern() -> [...].

   %% Test generators
   feature_test_() ->
       [?_assertEqual(Expected, Actual)].

   %% Helper functions
   helper_function() -> ...
   ```

3. **Mock Bytecode Patterns:**
   - Simple task: `[{'TASK_EXEC', task_name}, {'DONE'}]`
   - Sequence: `[{'SEQ_ENTER', 0}, {'TASK_EXEC', task_a}, {'SEQ_NEXT', 3}, ...]`
   - Parallel: `[{'PAR_FORK', [1, 3]}, {'TASK_EXEC', task_a}, {'DONE'}, ...]`
   - XOR: `[{'XOR_CHOOSE', [1, 3]}, {'TASK_EXEC', task_a}, {'DONE'}, ...]`
   - Loop: `[{'LOOP_CHECK', {count, 3}}, {'TASK_EXEC', task_a}, {'LOOP_BACK', 0}, ...]`
   - Cancel: `[{'CANCEL_SCOPE', {enter, scope1}}, {'TASK_EXEC', task_a}, {'CANCEL_SCOPE', {exit, scope1}}, ...]`
   - MI: `[{'MI_SPAWN', {{fixed, 2}, wait_all, 2}}, {'TASK_EXEC', task}, {'DONE'}, {'JOIN_WAIT', wait_all}]`

4. **Assertion Patterns:**
   - State assertions: `?assertEqual(Expected, wf_exec:get_ip(ExecState))`
   - Record field access: `ExecState#exec_state.tokens`
   - Map assertions: `?assert(maps:is_key(Key, Map))`
   - Match assertions: `?assertMatch({ok, _Report}, Result)`

**Property-Based Testing Pattern (Custom):**
```erlang
%% From wf_sched_tests.erl:339-346
prop_deterministic_reproducibility_test() ->
    %% Property: deterministic always returns same choice for same set
    Enabled = [{token, make_ref()}, {token, make_ref()}, {xor_branch, 1}],
    {ok, State} = wf_sched:new(deterministic, []),
    Choices = [element(1, wf_sched:choose(Enabled, State)) || _ <- lists:seq(1, 100)],
    ?assertEqual(lists:usort(Choices), [hd(Choices)]).
```

### Testing Strategy

**For wf_test_term.erl (AST validation):**
- **Challenge**: wf_term.erl doesn't exist yet
- **Options**:
  1. Defer until wf_term.erl is implemented
  2. Create placeholder tests that validate bytecode structure directly
  3. Test term structure as tuples following wf_vm opcode format

**For pattern test modules (seq, par, xor, join, cancel, mi):**
- Work directly with bytecode (wf_vm:wf_bc())
- Use mock bytecode generators for each pattern
- Test compilation (if wf_compile exists) or direct execution
- Verify semantic properties using wf_exec:step/2 and wf_exec:run/3

**For property-based testing:**
- Implement simple random term generator: `random_term(Depth) -> wf_bc()`
- Use rand module with explicit state for reproducibility
- Test invariants:
  - well_formed/1 validation (if implemented)
  - Compilation success (if wf_compile exists)
  - Label resolution in bytecode
  - Bounded execution termination

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **wf_term/wf_compile not implemented** | High | Tests must work with bytecode directly; create placeholder tests for AST features; coordinate with items 002-004 |
| **No well_formed/1 function exists** | High | Use wf_validate:validate/2 as proxy; implement structural validation in test helpers |
| **Missing cancellation module (wf_cancel)** | High | Test cancellation through CANCEL_SCOPE opcodes in wf_exec; defer wf_cancel-specific tests until implemented |
| **Property-based testing from scratch** | Medium | Start with simple generators using rand:uniform/1; leverage existing property-like tests in wf_sched_tests as template |
| **Test execution time with random generators** | Low | Use bounded depth (e.g., Depth = 5); limit test iterations (e.g., N = 100); use deterministic seeds |
| **Coverage of edge cases** | Medium | Review wf_validate checks for edge cases; use wf_validate:explore/2 to discover states; test with extreme values (0, large N) |
| **Dependency on scheduler policies** | Low | Use deterministic policy for reproducibility; test nondeterministic separately with fixed seeds |

## Recommended Approach

### Phase 1: Foundation (Quick Wins)

**1. Create wf_test_mi.erl first** (Low Risk)
- wf_mi.erl is fully implemented with comprehensive API
- Existing tests in wf_mi_tests.erl provide template
- Test coverage:
  - Fixed count: spawn N instances, verify all execute
  - Dynamic count: eval_instance_count with min/max bounds
  - Join policies: wait_all, wait_n, first_complete, none
  - Cancellation: cancel_remaining/2 cancels correct instances
- Use existing helper functions from wf_mi_tests.erl:100-182

**2. Extend wf_exec_tests.erl** (Low Risk)
- Add tests for cancellation patterns (wf_exec_tests.erl:183-239 already has basic tests)
- Add tests for join policies (wait_all, wait_n, first_complete)
- Add tests for mixed parallel completion times
- Test edge cases: empty branches, single branch, large N

**3. Extend wf_validate_tests.erl** (Low Risk)
- Add tests for deadlock detection in complex workflows
- Add tests for livelock detection
- Add tests for unreachable code detection
- Test validation error messages and paths

### Phase 2: Pattern Test Modules

**4. Implement wf_test_seq.erl**
- Test sequential composition patterns:
  - Single sequence: Task A → Task B → Task C
  - Nested sequences: Seq(Seq(A, B), C)
  - Sequence with effects: TASK_EXEC → EFFECT → SEQ_NEXT
  - Sequence with cancellation: Cancel mid-sequence
- Mock bytecode using SEQ_ENTER and SEQ_NEXT opcodes
- Verify execution order and IP advancement
- Test step count increment

**5. Implement wf_test_par.erl**
- Test parallel split patterns:
  - 2 branches: PAR_FORK [1, 3] → both execute → JOIN_WAIT all
  - 3 branches: PAR_FORK [1, 3, 5] → all execute → JOIN_WAIT all
  - N branches: Parameterized test with N from 2 to 10
  - Mixed completion: Par with tasks that take different step counts
  - Synchronizing merge: Par feeding into JOIN_WAIT with sync_merge policy
- Verify all branches execute
- Verify join counter mechanics
- Test with wait_all, wait_n, first_complete policies

**6. Implement wf_test_xor.erl**
- Test exclusive choice patterns:
  - 2 branches: XOR_CHOOSE [1, 3] → one branch executes → simple merge
  - 3 branches: XOR_CHOOSE [1, 3, 5] → one executes
  - N branches: Parameterized test
  - Signal selection: Use nondeterministic scheduler with fixed seed
  - Unselected branches: Verify they don't execute
  - Simple merge convergence: Multiple branches converge to single DONE
- Test with deterministic scheduler to verify branch selection
- Test with nondeterministic scheduler for coverage
- Verify only one branch executes

**7. Implement wf_test_join.erl**
- Test all join policies:
  - wait_all: All M branches must complete
  - wait_n: N of M complete, rest cancelled
  - first_complete: First done cancels others
  - Discriminator: Alias for first_complete
  - N-of-M: Explicit {n_of_m, N, M} policy
- Verify join counter mechanics:
  - Counter increments on branch completion
  - Join satisfied when counter >= threshold
  - Remaining branches cancelled on early satisfaction
- Test with different M values (2, 3, 5, 10)
- Test edge cases: N=0, N=M, N>M (error)

**8. Implement wf_test_cancel.erl**
- Test cancellation patterns:
  - Cancel running task: CANCEL_SCOPE wraps task, exit mid-execution
  - Cancel case: All tokens cancelled, status = cancelled
  - Cancel region: CANCEL_SCOPE enter/exit, unrelated scopes preserved
  - Cancel with pending effects: Effects cancelled or compensated
- Verify token status changes to cancelled
- Verify scope stack management
- Test nested cancel scopes
- Test cancellation during PAR, XOR, MI patterns

### Phase 3: Property-Based Testing Framework

**9. Implement minimal PBT framework**

**Create test/wf_prop.erl:**
```erlang
-module(wf_prop).
-export([random_term/1, for_all/3]).

%% Random bytecode generator with depth limit
random_term(0) -> [{'DONE'}];
random_term(Depth) when Depth > 0 ->
    case rand:uniform(10) of
        N when N =< 3 -> % Simple task
            [{'TASK_EXEC', random_task()}, {'DONE'}];
        N when N =< 5 -> % Sequence
            [
                {'SEQ_ENTER', 0},
                {'TASK_EXEC', random_task()},
                {'SEQ_NEXT', 3},
                random_task_term(Depth - 1)
            ];
        N when N =< 7 -> % Parallel
            Branch1 = random_term(Depth - 1),
            Branch2 = random_term(Depth - 1),
            merge_par_branches(Branch1, Branch2);
        _ -> % XOR
            Branch1 = random_term(Depth - 1),
            Branch2 = random_term(Depth - 1),
            merge_xor_branches(Branch1, Branch2)
    end.

%% Property test runner
for_all(Generator, Property, NumTests) ->
    lists:foreach(fun(N) ->
        Term = Generator(N),
        Property(Term)
    end, lists:seq(1, NumTests)).
```

**10. Implement wf_test_term.erl**
- **If wf_term exists**: Test AST constructors and well_formed/1
- **If wf_term doesn't exist** (current state):
  - Create placeholder tests that validate bytecode structure
  - Test opcode format validation
  - Test label resolution (all jump targets valid)
  - Test structural properties: no orphaned branches, balanced scopes
- Properties:
  - Any generated term passes wf_validate:validate/2 (or well_formed/1)
  - Compiled bytecode has valid jump targets
  - Execution terminates within bounded steps (use wf_exec:run/3 with timeout)

**11. Add property tests to all modules**
- Extend wf_test_seq.erl with: random sequences always execute completely
- Extend wf_test_par.erl with: random par workflows satisfy join policies
- Extend wf_test_xor.erl with: random xor workflows always select valid branch
- Extend wf_test_join.erl with: random join workflows never deadlock
- Extend wf_test_cancel.erl with: cancelled workflows terminate cleanly
- Extend wf_test_mi.erl with: random MI workflows satisfy instance count bounds

### Phase 4: Integration and Documentation

**12. Create test helper module**
- Create test/wf_test_helpers.erl with common utilities:
  - Bytecode generators for each pattern
  - Execution helpers with deterministic scheduler
  - State extraction helpers
  - Assertion helpers for complex invariants

**13. Update documentation**
- Add docs/TESTING.md with:
  - Test organization (wf_test_*.erl modules)
  - How to run tests: `rebar3 eunit`
  - Property-based testing approach
  - Coverage goals (e.g., 80% line coverage)
  - Reproduction of failing tests (seed values, trace logs)

**14. Continuous Integration**
- Add test coverage reporting to rebar.config
- Add makefile targets for testing:
  - `make test`: Run all tests
  - `make test-unit`: Run unit tests only
  - `make test-prop`: Run property tests only
  - `make coverage`: Generate coverage report

### Implementation Order (Recommended)

1. **wf_test_mi.erl** (wf_mi is complete, low risk)
2. **wf_test_seq.erl** (simplest pattern)
3. **wf_test_par.erl** (builds on seq)
4. **wf_test_xor.erl** (similar to par)
5. **wf_test_join.erl** (builds on par)
6. **wf_test_cancel.erl** (uses all patterns)
7. **wf_prop.erl** (PBT framework)
8. **wf_test_term.erl** (depends on wf_term existence, may defer)

## Open Questions

1. **wf_term.erl Status**: Does wf_term.erl exist or is it planned? The item references "AST constructors and smart constructors" and "well_formed/1" function, but these don't exist in the current codebase. Should wf_test_term.erl:
   - Wait for wf_term.erl to be implemented?
   - Test bytecode structure directly as a proxy?
   - Test term algebra if it's defined as simple Erlang terms (tuples)?

2. **wf_compile.erl Status**: Does a compiler from pattern terms to bytecode exist? Without it, tests must work directly with bytecode, which limits testing of "derived patterns produce expected term structures."

3. **wf_cancel.erl Status**: Is there a dedicated cancellation module? Current cancellation is implemented via CANCEL_SCOPE opcodes in wf_exec.erl. Should tests:
   - Focus on CANCEL_SCOPE opcode testing?
   - Wait for high-level wf_cancel API?

4. **Property-Based Testing Scope**: How comprehensive should the PBT framework be?
   - Minimal: Simple generators + basic invariants (as specified in item)
   - Full: PropEr-style with shrinking, generators for all types
   - Recommendation: Start minimal, extend based on needs

5. **Test Coverage Goals**: What are the coverage targets?
   - Line coverage percentage?
   - Pattern coverage (all 43 control-flow patterns)?
   - Edge case coverage?

6. **Integration with wf_validate**: Should wf_test_*.erl modules use wf_validate:validate/2 as part of their test suite, or is validation separate from unit tests?

7. **Reproduction of Failing Property Tests**: How should failing property tests be reported?
   - Log the seed value for reproduction?
   - Log the generated term that failed?
   - Use deterministic seeds in CI?

8. **Execution Time Budget**: What's the acceptable test execution time?
   - Property tests can be slow with many iterations
   - Should we limit iterations in CI vs. local development?

9. **Coordination with Other Items**: This item depends on or overlaps with:
   - Item 002 (pattern-term-algebra)
   - Item 003 (architecture-and-semantics-docs)
   - Item 004 (bytecode-compiler)
   - Should these be implemented first, or can tests proceed with bytecode-only approach?

10. **Test Data Management**: Should we create a library of test workflows (wf_example_*.erl) that test modules can use, or should each test module define its own mock bytecode generators?
