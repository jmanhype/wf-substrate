# Implement unit and property-based test suites Implementation Plan

## Implementation Plan Title

Comprehensive EUnit test suites for workflow pattern execution and minimal property-based testing framework

## Overview

Implement 7 comprehensive test modules (wf_test_*.erl) for workflow pattern constructs using EUnit, plus a minimal property-based testing framework. Tests work directly with bytecode (wf_vm:wf_bc()) since the AST layer (wf_term, wf_compile, wf_core) has not been implemented yet. The test suite validates sequential composition, parallel split, exclusive choice, join policies, cancellation, and multiple instance patterns. A custom property-based testing framework (no external dependencies) provides random bytecode generation and invariant checking.

## Current State

**Existing Test Infrastructure:**
- **rebar.config:26-29**: EUnit test profile configured with debug_info
- **test/** directory contains 7 test modules:
  - wf_substrate_tests.erl: Empty placeholder
  - wf_state_tests.erl: State management tests with setup/teardown
  - wf_exec_tests.erl: Executor tests with mock bytecode generators (1-346 lines)
  - wf_sched_tests.erl: Scheduler policy tests with property-like tests (1-362 lines)
  - wf_validate_tests.erl: Validation engine tests (1-229 lines)
  - wf_mi_tests.erl: Multiple instance tests (1-182 lines)
  - wf_trace_tests.erl: Trace and replay tests (1-335 lines)

**Test Patterns Established:**
- Mock bytecode generators for patterns (wf_exec_tests.erl:6-68)
- Setup/teardown pattern for gen_server tests (wf_state_tests.erl:8-49)
- Property-like tests using list comprehensions (wf_sched_tests.erl:339-346)
- Record inclusion: `-include("../src/wf_exec.hrl")` for test modules

**Missing Components (Critical):**
- **wf_term.erl**: AST/term algebra module (item 002, state=idea)
- **wf_compile.erl**: Compiler from terms to bytecode (item 004, state=idea)
- **wf_core.erl**: Kernel pattern basis (item 002, state=idea)
- **wf_cancel.erl**: Dedicated cancellation module (not implemented)
- **well_formed/1 function**: AST validation (doesn't exist)

**Available for Testing:**
- **wf_vm.erl:1-50**: Bytecode type definitions (wf_bc(), opcode(), join_policy(), loop_policy(), mi_policy())
- **wf_exec.erl:1-100+**: Executor with step/2, run/3, state management
- **wf_sched.erl**: Scheduling policies (deterministic, nondeterministic, replay)
- **wf_validate.erl:1-100+**: Validation engine (static analysis, bounded model checking)
- **wf_mi.erl:1-150+**: Multiple instance semantics (spawn_instances/4, check_join/1, cancel_remaining/2)
- **wf_state.erl:1-150+**: Transactional state store

## Desired End State

**Deliverables:**
1. **7 new test modules** in test/ directory:
   - wf_test_term.erl: Bytecode structure validation (placeholder for AST tests)
   - wf_test_seq.erl: Sequential composition tests
   - wf_test_par.erl: Parallel split and synchronization tests
   - wf_test_xor.erl: Exclusive choice and simple merge tests
   - wf_test_join.erl: All join policy tests
   - wf_test_cancel.erl: Activity/case/region cancellation tests
   - wf_test_mi.erl: Multiple instance pattern tests

2. **Property-based testing framework**: test/wf_prop.erl with random bytecode generation

3. **Test helper module**: test/wf_test_helpers.erl with common utilities

4. **Updated documentation**: docs/TESTING.md with test organization and coverage goals

**Verification:**
- All tests pass: `rebar3 eunit`
- Test execution time < 30 seconds for full suite
- Property tests use bounded depth (Depth = 5) and limited iterations (N = 100)
- Failing property tests log seed values and generated terms for reproduction
- Code coverage report generated: `rebar3 cover`

### Key Discoveries:

- **wf_term/wf_compile/wf_core don't exist** (items 002 and 004 are still in "idea" state) - tests must work directly with bytecode
- **No well_formed/1 function** - use wf_validate:validate/2 as proxy for structural validation
- **wf_cancel module doesn't exist** - test cancellation through CANCEL_SCOPE opcodes in wf_exec
- **Existing test patterns use mock bytecode generators** - follow this pattern from wf_exec_tests.erl:6-68
- **Property-like tests already exist in wf_sched_tests.erl:339-346** - use as template
- **No external test dependencies allowed** - rebar.config:13 explicitly sets `{deps, []}`
- **wf_mi.erl is fully implemented** - start with wf_test_mi.erl as lowest-risk module

## What We're NOT Doing

- **Waiting for wf_term/wf_compile implementation** - tests will work with bytecode directly
- **Implementing full PropEr-style framework** - minimal custom generator only
- **Testing AST-level pattern constructors** - deferred until items 002-004 are complete
- **Testing high-level wf_cancel API** - test CANCEL_SCOPE opcodes directly
- **Creating external dependencies** - pure Erlang/stdlib only
- **Testing workflow effect system (wf_effect)** - out of scope for this item
- **Performance benchmarking** - handled by separate item (wf_bench.erl)

## Implementation Approach

**Strategy: Incremental bytecode-based testing**

Since wf_term, wf_compile, and wf_core don't exist yet, all tests will work directly with bytecode (wf_vm:wf_bc()). Each test module includes:
1. **Mock bytecode generators** for pattern variations (2, 3, N branches)
2. **Unit tests** for semantic properties using wf_exec:step/2 and wf_exec:run/3
3. **Property tests** using custom random bytecode generator (wf_prop.erl)
4. **Helper functions** for state extraction and assertions

**Test Phases (ordered by risk):**
1. **Phase 1**: wf_test_mi.erl (wf_mi is complete, lowest risk)
2. **Phase 2**: wf_test_seq.erl (simplest pattern)
3. **Phase 3**: wf_test_par.erl (builds on seq)
4. **Phase 4**: wf_test_xor.erl (similar to par)
5. **Phase 5**: wf_test_join.erl (builds on par)
6. **Phase 6**: wf_test_cancel.erl (uses all patterns)
7. **Phase 7**: wf_prop.erl (PBT framework)
8. **Phase 8**: wf_test_term.erl (placeholder for AST tests)
9. **Phase 9**: wf_test_helpers.erl + docs/TESTING.md

**Key Design Decisions:**
1. **Bytecode-first approach**: Tests use manual bytecode construction following wf_vm.erl:1-50 type definitions
2. **Mock bytecode generators**: Each test module defines generators like `mock_bytecode_seq_3_tasks()`
3. **Deterministic scheduler for reproducibility**: Use `wf_sched_deterministic` for unit tests
4. **Fixed seeds for property tests**: Use `rand:export_seed/0` to log failing test cases
5. **Bounded execution**: Property tests use Depth = 5 to limit bytecode complexity
6. **Validation proxy**: Use wf_validate:validate/2 instead of well_formed/1

---

## Phases

### Phase 1: Implement wf_test_mi.erl

#### Overview

Create comprehensive tests for multiple instance patterns. wf_mi.erl is fully implemented with spawn_instances/4, check_join/1, collect_result/3, and cancel_remaining/2. Existing wf_mi_tests.erl provides a template to follow.

#### Changes Required:

##### 1. Create test/wf_test_mi.erl

**File**: `test/wf_test_mi.erl`
**Changes**: New test module for multiple instance patterns

```erlang
-module(wf_test_mi).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").
-include("../src/wf_mi.hrl").

%% Mock bytecode generators for MI patterns
mock_bytecode_mi_fixed_3() ->
    [
        {'MI_SPAWN', {{fixed, 3}, wait_all, 10}},
        {'TASK_EXEC', mi_task},
        {'DONE'},
        {'JOIN_WAIT', wait_all}
    ].

mock_bytecode_mi_dynamic() ->
    [
        {'MI_SPAWN', {{dynamic, 2, 5}, wait_all, 10}},
        {'TASK_EXEC', mi_task},
        {'DONE'},
        {'JOIN_WAIT', wait_all}
    ].

mock_bytecode_mi_wait_n() ->
    [
        {'MI_SPAWN', {{fixed, 5}, {wait_n, 3}, 10}},
        {'TASK_EXEC', mi_task},
        {'DONE'},
        {'JOIN_WAIT', {wait_n, 3}}
    ].

mock_bytecode_mi_first_complete() ->
    [
        {'MI_SPAWN', {{fixed, 4}, first_complete, 10}},
        {'TASK_EXEC', mi_task},
        {'DONE'},
        {'JOIN_WAIT', first_complete}
    ].

%% Unit tests for fixed count MI
mi_fixed_count_all_complete_test() ->
    Bytecode = mock_bytecode_mi_fixed_3(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(3 + 1 + 1 + 1, wf_exec:get_step_count(ExecState1)). % spawn + 3*(task+done) + join

%% Unit tests for dynamic count MI
mi_dynamic_count_test() ->
    Bytecode = mock_bytecode_mi_dynamic(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)).

%% Unit tests for wait_n join policy
mi_wait_n_join_test() ->
    Bytecode = mock_bytecode_mi_wait_n(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify 3 of 5 completed, 2 cancelled
    Tokens = ExecState1#exec_state.tokens,
    CancelledCount = lists:foldl(fun(_Id, Token, Acc) ->
        case Token#token.status of
            cancelled -> Acc + 1;
            _ -> Acc
        end
    end, 0, maps:to_list(Tokens)),
    ?assertEqual(2, CancelledCount).

%% Unit tests for first_complete join policy
mi_first_complete_test() ->
    Bytecode = mock_bytecode_mi_first_complete(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify only 1 completed, rest cancelled
    Tokens = ExecState1#exec_state.tokens,
    CompleteCount = lists:foldl(fun(_Id, Token, Acc) ->
        case Token#token.status of
            complete -> Acc + 1;
            _ -> Acc
        end
    end, 0, maps:to_list(Tokens)),
    ?assertEqual(1, CompleteCount).

%% Unit tests for MI cancellation
mi_cancellation_test() ->
    Bytecode = mock_bytecode_mi_fixed_3(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Run until MI_SPAWN completes
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    %% Cancel all instances
    ExecState2 = wf_exec:cancel(ExecState1),
    ?assertEqual(cancelled, ExecState2#exec_state.status).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_mi`
- [ ] No compiler warnings
- [ ] Code coverage > 90% for wf_mi.erl

##### Manual Verification:

- [ ] Fixed count MI (3 instances) spawns correct number of tokens
- [ ] Dynamic count MI respects min/max bounds
- [ ] wait_all join policy requires all instances to complete
- [ ] wait_n join policy completes after N instances
- [ ] first_complete join policy cancels remaining instances
- [ ] MI cancellation propagates to all instance tokens

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Implement wf_test_seq.erl

#### Overview

Test sequential composition patterns including single sequences, nested sequences, sequences with effects, and sequences with cancellation mid-way. SEQ_ENTER and SEQ_NEXT opcodes manage sequential execution.

#### Changes Required:

##### 1. Create test/wf_test_seq.erl

**File**: `test/wf_test_seq.erl`
**Changes**: New test module for sequential composition

```erlang
-module(wf_test_seq).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Mock bytecode generators
mock_bytecode_seq_2_tasks() ->
    [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].

mock_bytecode_seq_3_tasks() ->
    [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'SEQ_NEXT', 5},
        {'TASK_EXEC', task_c},
        {'DONE'}
    ].

mock_bytecode_nested_seq() ->
    %% Outer: task_a -> (task_b -> task_c) -> task_d
    [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_b},
        {'SEQ_NEXT', 7},
        {'TASK_EXEC', task_c},
        {'SEQ_NEXT', 9},
        {'TASK_EXEC', task_d},
        {'DONE'}
    ].

mock_bytecode_seq_with_cancel() ->
    [
        {'CANCEL_SCOPE', {enter, seq_scope}},
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 4},
        {'TASK_EXEC', task_b},  %% Will be cancelled
        {'CANCEL_SCOPE', {exit, seq_scope}}
    ].

%% Unit tests
single_sequence_test() ->
    Bytecode = mock_bytecode_seq_2_tasks(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(6, wf_exec:get_step_count(ExecState1)). % enter + task_a + next + task_b + done + scope_exit

nested_sequence_test() ->
    Bytecode = mock_bytecode_nested_seq(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(11, wf_exec:get_step_count(ExecState1)).

sequence_with_cancellation_test() ->
    Bytecode = mock_bytecode_seq_with_cancel(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until task_a completes
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),
    %% Cancel mid-sequence
    ExecState4 = wf_exec:cancel(ExecState3),
    ?assertEqual(cancelled, ExecState4#exec_state.status).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_seq`
- [ ] No compiler warnings
- [ ] Code coverage > 85% for seq-related code in wf_exec.erl

##### Manual Verification:

- [ ] Single sequence executes tasks in order
- [ ] Nested sequences execute correctly (inner then outer)
- [ ] SEQ_NEXT advances IP to correct target
- [ ] Cancellation mid-sequence terminates execution

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Implement wf_test_par.erl

#### Overview

Test parallel split (PAR_FORK) and synchronization (JOIN_WAIT). Par with 2/3/N branches, all branches complete, mixed completion times, par feeding into synchronizing merge. Verify join counter mechanics.

#### Changes Required:

##### 1. Create test/wf_test_par.erl

**File**: `test/wf_test_par.erl`
**Changes**: New test module for parallel patterns

```erlang
-module(wf_test_par).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Mock bytecode generators
mock_bytecode_par_2_branches() ->
    [
        {'PAR_FORK', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'JOIN_WAIT', all}
    ].

mock_bytecode_par_3_branches() ->
    [
        {'PAR_FORK', [1, 3, 5]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'TASK_EXEC', task_c},
        {'DONE'},
        {'JOIN_WAIT', all}
    ].

mock_bytecode_par_mixed_completion() ->
    [
        {'PAR_FORK', [1, 3, 5]},
        {'TASK_EXEC', task_a},           % 1 step
        {'DONE'},
        {'SEQ_ENTER', 0},                 % branch with seq (2 steps)
        {'TASK_EXEC', task_b},
        {'SEQ_NEXT', 7},
        {'TASK_EXEC', task_c},
        {'DONE'},
        {'JOIN_WAIT', all}
    ].

mock_bytecode_par_n_branches(N) ->
    %% Generate PAR_FORK with N branches
    BranchTargets = lists:seq(1, 2*N - 1, 2),
    BranchBytecode = lists:flatten([
        [
            {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
            {'DONE'}
        ] || I <- lists:seq(1, N)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', all}].

%% Unit tests
par_2_branches_test() ->
    Bytecode = mock_bytecode_par_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify both branches completed
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(3, maps:size(Tokens)). % root + 2 branch tokens

par_3_branches_test() ->
    Bytecode = mock_bytecode_par_3_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)).

par_mixed_completion_test() ->
    Bytecode = mock_bytecode_par_mixed_completion(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)).

par_n_branches_test_() ->
    %% Parameterized test for N = 2, 3, 5, 10
    lists:map(fun(N) ->
        {io_lib:format("par_~p_branches", [N]), fun() ->
            Bytecode = mock_bytecode_par_n_branches(N),
            ExecState0 = wf_exec:new(Bytecode),
            {done, ExecState1} = wf_exec:run(ExecState0, 1000, undefined),
            ?assert(wf_exec:is_done(ExecState1))
        end}
    end, [2, 3, 5, 10]).

%% Test join counter mechanics
par_join_counter_test() ->
    Bytecode = mock_bytecode_par_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until PAR_FORK
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    %% Verify join counter created
    ?assert(maps:size(ExecState1#exec_state.join_counters) > 0).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_par`
- [ ] No compiler warnings
- [ ] Parameterized tests pass for N = 2, 3, 5, 10

##### Manual Verification:

- [ ] PAR_FORK spawns correct number of branch tokens
- [ ] All branches execute to completion
- [ ] JOIN_WAIT with all policy waits for all branches
- [ ] Join counter increments correctly
- [ ] Mixed completion times handled correctly

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Implement wf_test_xor.erl

#### Overview

Test exclusive choice (XOR_CHOOSE) and simple merge. XOR with 2/3/N branches, signal selects correct branch, unselected branches are disabled, simple merge converges correctly.

#### Changes Required:

##### 1. Create test/wf_test_xor.erl

**File**: `test/wf_test_xor.erl`
**Changes**: New test module for exclusive choice patterns

```erlang
-module(wf_test_xor).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Mock bytecode generators
mock_bytecode_xor_2_branches() ->
    [
        {'XOR_CHOOSE', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].

mock_bytecode_xor_3_branches() ->
    [
        {'XOR_CHOOSE', [1, 3, 5]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'TASK_EXEC', task_c},
        {'DONE'}
    ].

mock_bytecode_xor_simple_merge() ->
    [
        {'XOR_CHOOSE', [1, 3]},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 5},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].

%% Unit tests
xor_2_branches_test() ->
    Bytecode = mock_bytecode_xor_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify only one branch executed (2 steps + DONE)
    ?assertEqual(3, wf_exec:get_step_count(ExecState1)).

xor_3_branches_test() ->
    Bytecode = mock_bytecode_xor_3_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(3, wf_exec:get_step_count(ExecState1)).

xor_simple_merge_test() ->
    Bytecode = mock_bytecode_xor_simple_merge(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Both branches converge to same DONE
    ?assertEqual(4, wf_exec:get_step_count(ExecState1)).

%% Test with deterministic scheduler
xor_deterministic_selection_test() ->
    Bytecode = mock_bytecode_xor_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Run again with same bytecode
    ExecState2 = wf_exec:new(Bytecode),
    {done, ExecState3} = wf_exec:run(ExecState2, 100, undefined),
    %% Same branch should be selected (deterministic)
    ?assertEqual(wf_exec:get_step_count(ExecState1),
                 wf_exec:get_step_count(ExecState3)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_xor`
- [ ] No compiler warnings
- [ ] Deterministic scheduler produces same branch selection

##### Manual Verification:

- [ ] XOR_CHOOSE selects exactly one branch
- [ ] Unselected branches don't execute
- [ ] Simple merge converges correctly to DONE
- [ ] N-branch XOR works for N = 2, 3, 5

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Implement wf_test_join.erl

#### Overview

Test all join policies: wait_all (all must complete), wait_n (N of M complete then cancel rest), first_complete (discriminator), and N-of-M join. Verify join counter mechanics.

#### Changes Required:

##### 1. Create test/wf_test_join.erl

**File**: `test/wf_test_join.erl`
**Changes**: New test module for join policies

```erlang
-module(wf_test_join).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Mock bytecode generators
mock_bytecode_join_wait_all(M) ->
    %% PAR_FORK to M branches, JOIN_WAIT all
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([
        [
            {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
            {'DONE'}
        ] || I <- lists:seq(1, M)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', all}].

mock_bytecode_join_wait_n(M, N) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([
        [
            {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
            {'DONE'}
        ] || I <- lists:seq(1, M)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', {wait_n, N}}].

mock_bytecode_join_first_complete(M) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([
        [
            {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
            {'DONE'}
        ] || I <- lists:seq(1, M)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', first_complete}].

%% Unit tests
join_wait_all_3_test() ->
    Bytecode = mock_bytecode_join_wait_all(3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% All 3 branches completed
    Tokens = ExecState1#exec_state.tokens,
    CompleteCount = lists:foldl(fun(_Id, Token, Acc) ->
        case Token#token.status of
            complete -> Acc + 1;
            _ -> Acc
        end
    end, 0, maps:to_list(Tokens)),
    ?assertEqual(3, CompleteCount).

join_wait_n_3_of_5_test() ->
    Bytecode = mock_bytecode_join_wait_n(5, 3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% 3 completed, 2 cancelled
    Tokens = ExecState1#exec_state.tokens,
    CompleteCount = lists:foldl(fun(_Id, Token, Acc) ->
        case Token#token.status of
            complete -> Acc + 1;
            _ -> Acc
        end
    end, 0, maps:to_list(Tokens)),
    ?assertEqual(3, CompleteCount).

join_first_complete_test() ->
    Bytecode = mock_bytecode_join_first_complete(4),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Only 1 completed, rest cancelled
    Tokens = ExecState1#exec_state.tokens,
    CompleteCount = lists:foldl(fun(_Id, Token, Acc) ->
        case Token#token.status of
            complete -> Acc + 1;
            _ -> Acc
        end
    end, 0, maps:to_list(Tokens)),
    ?assertEqual(1, CompleteCount).

%% Join counter mechanics test
join_counter_increments_test() ->
    Bytecode = mock_bytecode_join_wait_all(3),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until PAR_FORK
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    %% Get join counter
    JoinCounters = ExecState1#exec_state.join_counters,
    ?assert(maps:size(JoinCounters) > 0),
    [{_JoinId, Counter}] = maps:to_list(JoinCounters),
    ?assertEqual(0, Counter#join_counter.completed).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_join`
- [ ] No compiler warnings
- [ ] Test M = 2, 3, 5, 10 for all policies

##### Manual Verification:

- [ ] wait_all requires all M branches to complete
- [ ] wait_n completes after N branches, cancels rest
- [ ] first_complete completes after 1 branch, cancels rest
- [ ] Join counter increments on each branch completion
- [ ] Join satisfied when counter >= threshold

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Implement wf_test_cancel.erl

#### Overview

Test activity/case/region cancellation using CANCEL_SCOPE opcodes. Cancel running task, cancel case with active branches, cancel region preserves unrelated scopes, cancel with pending effects.

#### Changes Required:

##### 1. Create test/wf_test_cancel.erl

**File**: `test/wf_test_cancel.erl`
**Changes**: New test module for cancellation patterns

```erlang
-module(wf_test_cancel).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Mock bytecode generators
mock_bytecode_cancel_task() ->
    [
        {'CANCEL_SCOPE', {enter, task_scope}},
        {'TASK_EXEC', long_task},
        {'CANCEL_SCOPE', {exit, task_scope}}
    ].

mock_bytecode_cancel_case_with_par() ->
    [
        {'CANCEL_SCOPE', {enter, case_scope}},
        {'PAR_FORK', [2, 4]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'CANCEL_SCOPE', {exit, case_scope}}
    ].

mock_bytecode_cancel_nested_scopes() ->
    [
        {'CANCEL_SCOPE', {enter, outer_scope}},
        {'TASK_EXEC', task_a},
        {'CANCEL_SCOPE', {enter, inner_scope}},
        {'TASK_EXEC', task_b},
        {'CANCEL_SCOPE', {exit, inner_scope}},
        {'TASK_EXEC', task_c},
        {'CANCEL_SCOPE', {exit, outer_scope}}
    ].

%% Unit tests
cancel_running_task_test() ->
    Bytecode = mock_bytecode_cancel_task(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Enter scope, start task
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    %% Cancel case
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    %% Token should be cancelled
    CurrentTokenId = ExecState3#exec_state.current_token,
    Token = maps:get(CurrentTokenId, ExecState3#exec_state.tokens),
    ?assertEqual(cancelled, Token#token.status).

cancel_case_with_active_branches_test() ->
    Bytecode = mock_bytecode_cancel_case_with_par(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until PAR_FORK
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    %% Cancel case
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    %% All tokens should be cancelled
    Tokens = ExecState3#exec_state.tokens,
    lists:foreach(fun(_Id, Token) ->
        ?assertEqual(cancelled, Token#token.status)
    end, maps:to_list(Tokens)).

cancel_nested_scopes_test() ->
    Bytecode = mock_bytecode_cancel_nested_scopes(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute into inner scope
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),
    {ExecState4, _} = wf_exec:step(ExecState3, undefined),
    %% Verify scope stack depth = 2 (outer + inner)
    ?assertEqual(2, wf_exec:get_scope_stack_depth(ExecState4)),
    %% Exit inner scope
    {ExecState5, _} = wf_exec:step(ExecState4, undefined),
    ?assertEqual(1, wf_exec:get_scope_stack_depth(ExecState5)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_cancel`
- [ ] No compiler warnings
- [ ] Scope stack management correct

##### Manual Verification:

- [ ] Cancel running task sets token status to cancelled
- [ ] Cancel case cancels all active tokens
- [ ] Nested cancel scopes manage stack correctly
- [ ] Cancellation preserves unrelated scopes

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 7: Implement Property-Based Testing Framework

#### Overview

Create minimal property-based testing framework (wf_prop.erl) with random bytecode generator and invariant checkers. No external dependencies (no PropEr/Triq).

#### Changes Required:

##### 1. Create test/wf_prop.erl

**File**: `test/wf_prop.erl`
**Changes**: New PBT framework module

```erlang
-module(wf_prop).
-export([random_term/1, for_all/3, quickcheck/2]).

%%====================================================================
%% Random Bytecode Generator
%%====================================================================

%% @doc Generate random bytecode with depth limit
-spec random_term(non_neg_integer()) -> wf_vm:wf_bc().
random_term(0) ->
    [{'DONE'}];
random_term(Depth) when Depth > 0 ->
    case rand:uniform(10) of
        N when N =< 4 ->
            %% Simple task sequence
            NumTasks = rand:uniform(3),
            lists:flatten([
                [{'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))}]
             || I <- lists:seq(1, NumTasks)]) ++ [{'DONE'}];
        N when N =< 6 ->
            %% Parallel pattern
            NumBranches = rand:uniform(3) + 1, % 2-4 branches
            generate_par(NumBranches, Depth - 1);
        N when N =le 8 ->
            %% XOR pattern
            NumBranches = rand:uniform(3) + 1,
            generate_xor(NumBranches, Depth - 1);
        _ ->
            %% Sequence with cancellation
            [
                {'CANCEL_SCOPE', {enter, random_scope()}},
                {'TASK_EXEC', random_task()},
                {'CANCEL_SCOPE', {exit, random_scope()}}
            ]
    end.

%% Generate parallel pattern
generate_par(NumBranches, Depth) ->
    BranchTargets = lists:seq(1, 2*NumBranches - 1, 2),
    BranchBytecode = lists:flatten([random_term(Depth) || _ <- lists:seq(1, NumBranches)]),
    [{'PAR_FORK', BranchTargets} | BranchBytecode] ++ [{'JOIN_WAIT', all}].

%% Generate XOR pattern
generate_xor(NumBranches, Depth) ->
    BranchTargets = lists:seq(1, 2*NumBranches - 1, 2),
    BranchBytecode = lists:flatten([random_term(Depth) || _ <- lists:seq(1, NumBranches)]),
    [{'XOR_CHOOSE', BranchTargets} | BranchBytecode].

%% Helpers
random_task() ->
    list_to_atom("task_" ++ integer_to_list(rand:uniform(1000))).

random_scope() ->
    list_to_atom("scope_" ++ integer_to_list(rand:uniform(100))).

%%====================================================================
%% Property Test Runner
%%====================================================================

%% @doc Run property test N times
-spec for_all(fun(() -> wf_vm:wf_bc()), fun((wf_vm:wf_bc()) -> any()), pos_integer()) -> ok.
for_all(Generator, Property, NumTests) ->
    lists:foreach(fun(N) ->
        Term = Generator(),
        Property(Term)
    end, lists:seq(1, NumTests)),
    ok.

%% @doc Quickcheck-style property test with failure reporting
-spec quickcheck(fun(() -> wf_vm:wf_bc()), fun((wf_vm:wf_bc()) -> boolean())) -> {ok, pos_integer()} | {error, wf_vm:wf_bc(), term()}.
quickcheck(Generator, Property) ->
    quickcheck_loop(Generator, Property, 0).

quickcheck_loop(Generator, Property, N) ->
    case N >= 100 of
        true ->
            {ok, N};
        false ->
            Term = Generator(),
            try
                case Property(Term) of
                    true -> quickcheck_loop(Generator, Property, N + 1);
                    false -> {error, Term, {failed, N}}
                end
            catch
                _:Exception ->
                    {error, Term, Exception}
            end
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Basic property tests pass
- [ ] Random generator produces valid bytecode
- [ ] No infinite loops in random generation

##### Manual Verification:

- [ ] random_term(0) produces simple DONE bytecode
- [ ] random_term(5) produces complex nested patterns
- [ ] Generator terminates (bounded by depth)
- [ ] for_all/3 runs property function N times

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 8: Implement wf_test_term.erl

#### Overview

Create bytecode structure validation tests (placeholder for AST tests). Test opcode format validation, label resolution, structural properties.

#### Changes Required:

##### 1. Create test/wf_test_term.erl

**File**: `test/wf_test_term.erl`
**Changes**: New test module for bytecode validation

```erlang
-module(wf_test_term).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%%====================================================================
%% Bytecode Structure Tests
%%====================================================================

%% Test opcode format validation
opcode_format_test() ->
    %% Valid opcodes from wf_vm:wf_bc() type
    ValidOpcodes = [
        {'TASK_EXEC', task},
        {'DONE'},
        {'SEQ_ENTER', 0},
        {'SEQ_NEXT', 5},
        {'PAR_FORK', [1, 3]},
        {'XOR_CHOOSE', [1, 3]},
        {'JOIN_WAIT', all},
        {'CANCEL_SCOPE', {enter, scope}}
    ],
    lists:foreach(fun(Op) ->
        ?assert(is_tuple(Op)),
        ?assert(is_atom(element(1, Op)))
    end, ValidOpcodes).

%% Test label resolution in bytecode
label_resolution_test() ->
    %% PAR_FORK with valid targets
    Bytecode = [
        {'PAR_FORK', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'JOIN_WAIT', all}
    ],
    %% All label targets should be within bytecode bounds
    {_, Labels} = lists:keyfind('PAR_FORK', 1, Bytecode),
    MaxIP = length(Bytecode) - 1,
    lists:foreach(fun(Label) ->
        ?assert(Label >= 0 andalso Label =< MaxIP)
    end, Labels).

%% Test structural properties
balanced_scopes_test() ->
    %% CANCEL_SCOPE enter/exit should be balanced
    Bytecode = [
        {'CANCEL_SCOPE', {enter, scope1}},
        {'TASK_EXEC', task},
        {'CANCEL_SCOPE', {exit, scope1}}
    ],
    ExecState = wf_exec:new(Bytecode),
    ?assertEqual(1, wf_exec:get_scope_stack_depth(ExecState)).

%% Property: any generated bytecode compiles without error
prop_random_bytecode_executable_test() ->
    %% Use wf_prop:for_all to test random bytecode
    Property = fun(Bytecode) ->
        ExecState = wf_exec:new(Bytecode),
        ?assert(is_record(ExecState, exec_state))
    end,
    wf_prop:for_all(fun() -> wf_prop:random_term(5) end, Property, 50).

%% Property: execution terminates within bounded steps
prop_bounded_termination_test() ->
    Property = fun(Bytecode) ->
        ExecState0 = wf_exec:new(Bytecode),
        Result = wf_exec:run(ExecState0, 1000, undefined),
        ?assertMatch({done, _} | {yield, _} | {error, _}, Result)
    end,
    wf_prop:for_all(fun() -> wf_prop:random_term(3) end, Property, 50).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] Tests pass: `rebar3 eunit --module=wf_test_term`
- [ ] No compiler warnings
- [ ] Property tests run 50 iterations each

##### Manual Verification:

- [ ] Opcodes match wf_vm:opcode() type definition
- [ ] Label targets are within bytecode bounds
- [ ] CANCEL_SCOPE enter/exit are balanced
- [ ] Random bytecode executes without crashing

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 9: Create Test Helpers and Documentation

#### Overview

Create wf_test_helpers.erl with common utilities and docs/TESTING.md with test organization and coverage goals.

#### Changes Required:

##### 1. Create test/wf_test_helpers.erl

**File**: `test/wf_test_helpers.erl`
**Changes**: New test helper module

```erlang
-module(wf_test_helpers).
-export([
    exec_until_done/1,
    exec_steps/2,
    get_token_statuses/1,
    count_tokens_with_status/2,
    assert_join_counter/2
]).

%% @doc Execute until done (max 1000 steps)
-spec exec_until_done(wf_exec:exec_state()) -> wf_exec:exec_state().
exec_until_done(ExecState) ->
    case wf_exec:run(ExecState, 1000, undefined) of
        {done, DoneState} -> DoneState;
        {yield, YieldState} -> exec_until_done(YieldState)
    end.

%% @doc Execute N steps
-spec exec_steps(wf_exec:exec_state(), pos_integer()) -> wf_exec:exec_state().
exec_steps(ExecState, 0) -> ExecState;
exec_steps(ExecState, N) ->
    {NewState, _} = wf_exec:step(ExecState, undefined),
    exec_steps(NewState, N - 1).

%% @doc Get all token statuses
-spec get_token_statuses(wf_exec:exec_state()) -> [atom()].
get_token_statuses(ExecState) ->
    Tokens = ExecState#exec_state.tokens,
    [Token#token.status || {_Id, Token} <- maps:to_list(Tokens)].

%% @doc Count tokens with specific status
-spec count_tokens_with_status(wf_exec:exec_state(), atom()) -> non_neg_integer().
count_tokens_with_status(ExecState, Status) ->
    lists:count(fun(S) -> S =:= Status end, get_token_statuses(ExecState)).

%% @doc Assert join counter value
-spec assert_join_counter(wf_exec:exec_state(), {non_neg_integer(), non_neg_integer()}) -> boolean().
assert_join_counter(ExecState, {ExpectedCompleted, ExpectedRequired}) ->
    JoinCounters = ExecState#exec_state.join_counters,
    case maps:size(JoinCounters) of
        0 -> false;
        _ ->
            [{_JoinId, Counter}] = maps:to_list(JoinCounters),
            Counter#join_counter.completed =:= ExpectedCompleted andalso
            Counter#join_counter.required =:= ExpectedRequired
    end.
```

##### 2. Create docs/TESTING.md

**File**: `docs/TESTING.md`
**Changes**: New testing documentation

```markdown
# Testing Guide

## Test Organization

### Test Modules

- **wf_test_term.erl**: Bytecode structure validation (placeholder for AST tests)
- **wf_test_seq.erl**: Sequential composition tests
- **wf_test_par.erl**: Parallel split and synchronization tests
- **wf_test_xor.erl**: Exclusive choice and simple merge tests
- **wf_test_join.erl**: All join policy tests
- **wf_test_cancel.erl**: Activity/case/region cancellation tests
- **wf_test_mi.erl**: Multiple instance pattern tests

### Existing Test Modules

- **wf_exec_tests.erl**: Executor implementation tests
- **wf_sched_tests.erl**: Scheduler policy tests
- **wf_validate_tests.erl**: Validation engine tests
- **wf_state_tests.erl**: State management tests
- **wf_mi_tests.erl**: Multiple instance semantics tests
- **wf_trace_tests.erl**: Trace and replay tests

## Running Tests

### Run All Tests

```bash
rebar3 eunit
```

### Run Specific Test Module

```bash
rebar3 eunit --module=wf_test_seq
```

### Run Specific Test Function

```bash
rebar3 eunit --test=wf_test_seq:seq_2_tasks_test
```

### Generate Coverage Report

```bash
rebar3 cover
```

Coverage report generated in `_build/test/cover/index.html`

## Coverage Goals

- **Line coverage**: > 80% for all core modules
- **Pattern coverage**: All 43 workflow patterns tested
- **Edge case coverage**: Test boundary conditions (0, 1, large N)

## Property-Based Testing

### Running Property Tests

Property tests use custom framework (wf_prop.erl) with random bytecode generation.

```erlang
%% Property test example
prop_random_bytecode_executable_test() ->
    Property = fun(Bytecode) ->
        ExecState = wf_exec:new(Bytecode),
        ?assert(is_record(ExecState, exec_state))
    end,
    wf_prop:for_all(fun() -> wf_prop:random_term(5) end, Property, 100).
```

### Reproducing Failing Property Tests

Failing property tests log:
1. Seed value: Use `rand:export_seed/0` to get seed
2. Generated term: The bytecode that failed
3. Exception or assertion failure

Reproduce with:
```erlang
%% Set seed
rand:seed(exs1024, {Seed1, Seed2, Seed3}),
%% Run failing test
wf_test_term:prop_random_bytecode_executable_test().
```

## Test Patterns

### Mock Bytecode Generators

Each test module defines mock bytecode generators:

```erlang
mock_bytecode_seq_2_tasks() ->
    [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].
```

### Setup/Teardown Pattern

For gen_server tests:

```erlang
setup() -> {ok, Pid} = wf_state:start_link(), Pid.
cleanup(Pid) -> gen_server:stop(Pid).
my_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(assert_something())
    ]}.
```

### Parameterized Tests

```erlang
par_n_branches_test_() ->
    lists:map(fun(N) ->
        {io_lib:format("par_~p_branches", [N]), fun() ->
            Bytecode = mock_bytecode_par_n_branches(N),
            ExecState0 = wf_exec:new(Bytecode),
            {done, ExecState1} = wf_exec:run(ExecState0, 1000, undefined),
            ?assert(wf_exec:is_done(ExecState1))
        end}
    end, [2, 3, 5, 10]).
```

## Test Execution Time

Target test execution times:
- Unit tests: < 10 seconds
- Property tests: < 20 seconds (100 iterations)
- Full test suite: < 30 seconds

## Continuous Integration

CI runs:
```bash
rebar3 compile
rebar3 eunit
rebar3 cover
```

Tests must pass before merging PR.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests compile: `rebar3 compile`
- [ ] All tests pass: `rebar3 eunit`
- [ ] Test helpers compile and export all functions
- [ ] docs/TESTING.md is readable and accurate

##### Manual Verification:

- [ ] wf_test_helpers functions work correctly
- [ ] docs/TESTING.md documents all test modules
- [ ] Coverage report generates successfully
- [ ] Full test suite completes in < 30 seconds

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

## Testing Strategy

### Unit Tests:

- **Pattern execution semantics**: Each workflow pattern (seq, par, xor, join, cancel, mi) tested in isolation
- **Opcode correctness**: Each opcode (SEQ_ENTER, PAR_FORK, etc.) produces expected state changes
- **Edge cases**: Boundary values (0, 1, large N), empty branches, single branch
- **State transitions**: Token status changes, IP advancement, scope stack management
- **Join counter mechanics**: Counter increments, threshold checking, satisfaction conditions

### Integration Tests:

- **Multi-pattern workflows**: Combine seq + par + xor + join
- **Cancellation propagation**: Cancel through nested patterns
- **Multiple instances**: MI with various join policies
- **Validation integration**: wf_validate:validate/2 on complex workflows

### Manual Testing Steps:

1. **Run full test suite**: `rebar3 eunit`
2. **Check test execution time**: Should complete in < 30 seconds
3. **Generate coverage report**: `rebar3 cover`, verify > 80% coverage
4. **Test random bytecode generation**: Run property tests multiple times to ensure no flakiness
5. **Verify deterministic behavior**: Run same test twice, verify identical results
6. **Test failure reproduction**: Introduce assertion failure, verify seed/value logging

## Migration Notes

No data migration required. This is green-field test development.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/014-unit-and-property-tests/research.md`
- Source: `/Users/speed/wf-substrate/src/wf_vm.erl` (bytecode type definitions)
- Source: `/Users/speed/wf-substrate/src/wf_exec.erl` (executor implementation)
- Source: `/Users/speed/wf-substrate/src/wf_mi.erl` (multiple instance semantics)
- Source: `/Users/speed/wf-substrate/src/wf_validate.erl` (validation engine)
- Test: `/Users/speed/wf-substrate/test/wf_exec_tests.erl` (mock bytecode patterns)
- Test: `/Users/speed/wf-substrate/test/wf_mi_tests.erl` (MI test template)
- Test: `/Users/speed/wf-substrate/test/wf_sched_tests.erl` (property-like tests)
- Config: `/Users/speed/wf-substrate/rebar.config` (no external deps)
- PROMPT: `/Users/speed/wf-substrate/PROMPT.md` (project specification)
