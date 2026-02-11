# Write pattern coverage and operational documentation Implementation Plan

## Implementation Plan Title

Comprehensive Documentation: Pattern Coverage, Testing Strategy, and Operations Guide for wf_substrate Workflow Engine

## Overview

This implementation creates three comprehensive documentation files for the wf_substrate workflow pattern engine:

1. **PATTERNS.md**: Maps van der Aalst's 43 workflow patterns to wf_substrate implementation, showing kernel primitives vs derived patterns
2. **TESTING.md**: Complete testing strategy (building upon existing partial documentation)
3. **OPERATIONS.md**: New operational guide covering supervision, failures, telemetry, monitoring, and configuration

The codebase has a working bytecode executor (wf_exec.erl), comprehensive test suites, and tracing infrastructure, but lacks documentation for operators and users. The existing docs/TESTING.md covers unit/integration/property testing but is missing determinism/replay testing, Common Test usage, and operational testing guidance.

## Current State

**Existing Documentation:**
- docs/README.md: Placeholder listing planned docs (lines 1-22)
- docs/TESTING.md: Partial testing guide (lines 1-266), covers unit/integration/property/benchmark tests well

**Existing Implementation:**
- Core executor: src/wf_exec.erl (lines 1-598) - implements SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, TASK_EXEC, DONE
- Opcodes defined: src/wf_vm.erl (11 opcodes, join/loop/MI policies)
- Tracing: src/wf_trace.erl (3 trace levels: none/min/full, 4 sink types: callback/ets/process/replay)
- Validation: src/wf_validate.erl (5 correctness checks, bounded exploration)
- Benchmarks: src/wf_bench.erl (4 implemented, 2 deferred pending items 006/009/010)
- Tests: 8 test modules (wf_test_seq, wf_test_par, wf_test_xor, wf_test_join, wf_test_cancel, wf_test_mi, wf_test_term, wf_test_determinism)

**Known Gaps:**
1. Cancellation stubbed: wf_exec.erl:512-529 references item 008
2. Multiple Instances: wf_test_mi.erl includes wf_mi.hrl but header doesn't exist; MI_SPAWN opcode not handled
3. Scheduler: wf_sched referenced in wf_trace.erl:24 and wf_test_trace_helpers.erl:67 but no implementation
4. Effect system: wf_effect referenced in PROMPT.md:145-155 but not implemented (item 010)
5. Supervision tree: wf_substrate_sup.erl:46 has empty child specs (item 012)

## Desired End State

Three comprehensive documentation files in docs/ directory:

### docs/PATTERNS.md
- Table of implemented patterns with columns: Pattern #, Name, Category, wf_term() expression, Bytecode opcodes, Test modules, Kernel/Derived/Not Implemented, Limitations
- Distinguishes kernel primitives (direct opcodes) from derived patterns (macro combinations)
- Maps van der Aalst catalog to test modules (what's tested is implemented)
- Documents limitations with specific file:line references

### docs/TESTING.md (expanded from existing)
- Preserves all existing content (lines 1-266)
- Adds: Determinism/replay testing methodology (wf_test_determinism.erl)
- Adds: Common Test usage (rebar3 ct, test suites)
- Adds: Operational testing (supervision tree restart testing)
- Adds: Coverage interpretation guidelines
- Adds: Manual testing procedures

### docs/OPERATIONS.md (new)
- Supervision tree structure and restart strategies (current skeleton vs planned)
- Failure modes and recovery (case runner crash, effect timeout, state corruption)
- Telemetry/tracing configuration (3 levels, 4 sinks, log format)
- Monitoring running systems (inspect active cases, stuck workflows, trace retrieval)
- Configuration options (scheduler policy, step quanta, trace level, effect budgets)

### Key Discoveries:

- **wf_sched type exists but module doesn't**: wf_trace.erl:24 references `wf_sched:choice_entry()` type, and wf_test_trace_helpers.erl:67-67 accepts `wf_sched:sched_policy()` parameter, but no wf_sched.erl module exists in src/
- **Determinism testing exists**: wf_test_determinism.erl provides comprehensive tests for deterministic execution (lines 1-128), using wf_test_trace_helpers:run_with_trace/3 with `deterministic` policy parameter
- **Scheduler policies are conceptual**: PROMPT.md:140-143 defines deterministic/nondeterministic/replay modes, but these are not implemented - tests pass `deterministic` but it's ignored (wf_test_trace_helpers.erl:67)
- **Replay infrastructure ready**: wf_trace.erl defines replay_entry record (line 21-26), replay_log type, and from_replay_log/1 function (line 209-224), but no actual replay execution yet
- **Benchmark metrics are well-defined**: wf_bench.erl:65-71 defines bench_result record with steps_per_sec as key metric; 4/6 benchmarks implemented
- **Cancellation has test coverage**: wf_test_cancel.erl has 7 test functions for cancel patterns, but cancellation logic is stubbed in executor (wf_exec.erl:512-529)
- **MI has tests but no implementation**: wf_test_mi.erl has 6 test functions but MI_SPAWN opcode has no handler in wf_exec:execute_opcode/2

## What We're NOT Doing

- **NOT implementing missing features**: Documenting gaps (cancellation stubs, missing scheduler, MI implementation) but not fixing them
- **NOT writing ARCHITECTURE.md or SEMANTICS.md**: These are separate planned docs (docs/README.md:9-10)
- **NOT creating runnable examples**: wf_example_basic.erl mentioned in PROMPT.md:309-313 doesn't exist, won't create
- **NOT writing tutorial documentation**: Focus is on reference documentation (patterns, testing, operations), not "getting started" guides
- **NOT documenting data/resource patterns**: van der Aalst's 43 patterns include data and resource patterns, but wf_substrate is control-flow only (PROMPT.md:75-91)
- **NOT creating new tests**: Documenting existing test methodology, not adding new tests
- **NOT defining the 43 patterns**: Using test modules as ground truth for what's implemented; not researching academic paper to reverse-engineer catalog

## Implementation Approach

**Documentation Strategy:**
1. **Code-first**: All documentation references specific file:line numbers from actual implementation
2. **Honest about gaps**: Clearly distinguish "implemented" vs "planned" vs "stubbed"
3. **Pattern-based testing**: Use test modules as evidence of pattern implementation (if it's tested, it exists)
4. **Cross-file linkage**: Link between docs (e.g., PATTERNS.md references TESTING.md for test details)
5. **Build on existing**: Preserve and extend docs/TESTING.md rather than rewriting

**Pattern Inventory Approach:**
- Extract patterns from test modules (wf_test_seq, wf_test_par, wf_test_xor, wf_test_join, wf_test_cancel, wf_test_mi)
- Map to van der Aalst names from PROMPT.md:75-91
- Categorize using wf_vm.erl opcode types
- Distinguish kernel (direct opcode handlers) vs derived (combinations) vs not implemented

**Testing Guide Approach:**
- Preserve existing docs/TESTING.md content (good foundation)
- Add missing sections based on discovered test files (wf_test_determinism.erl, wf_test_replay.erl)
- Reference actual test helper functions (wf_test_helpers.erl, wf_test_trace_helpers.erl)
- Document property testing framework (wf_prop.erl) with its limitations (no shrinking)

**Operations Guide Approach:**
- Document current state (skeleton) vs planned (from PROMPT.md:182-185, 284-289)
- Reference actual trace infrastructure (wf_trace.erl:38-44 for levels/sinks)
- Document validation checks (wf_validate.erl:532-652 for correctness)
- Note dependencies on future items (006, 008, 009, 010, 012)

---

## Phases

### Phase 1: Pattern Inventory and PATTERNS.md

#### Overview

Catalog implemented workflow patterns by analyzing test modules and mapping to van der Aalst's pattern categories. Create comprehensive table distinguishing kernel primitives from derived patterns.

#### Changes Required:

##### 1. Create docs/PATTERNS.md

**File**: `docs/PATTERNS.md`
**Changes**: New file with pattern mapping table

```markdown
# Workflow Pattern Coverage

## Overview

This document maps workflow patterns from the van der Aalst et al. catalog to wf_substrate implementation. It distinguishes:
- **Kernel primitives**: Direct opcode support in wf_exec:execute_opcode/2
- **Derived patterns**: Combinations of kernel primitives
- **Not implemented**: Patterns without test coverage or stubbed implementations

## Pattern Categories

1. **Basic Control Flow**: Sequence, Task
2. **Advanced Branching**: Parallel split, Synchronization, Multi-choice, Synchronizing merge, Discriminator, N-out-of-M join
3. **Structural**: Exclusive choice, Simple merge
4. **Multiple Instance**: Fixed count, Dynamic count, with join policies
5. **State-Based**: Arbitrary cycles (Loop)
6. **Cancellation**: Cancel activity, Cancel case, Cancel region

## Pattern Table

| Pattern # | Pattern Name | Category | wf_term() | Bytecode Opcodes | Test Modules | Implementation | Limitations |
|-----------|--------------|----------|-----------|------------------|--------------|----------------|-------------|
| 1 | Sequence | Basic | seq(P, Q) | SEQ_ENTER, SEQ_NEXT, DONE | wf_test_seq.erl | Kernel | - |
| 2 | Parallel Split (AND-split) | Advanced Branching | par([P, ...]) | PAR_FORK | wf_test_par.erl | Kernel | - |
| 3 | Synchronization (AND-join) | Advanced Branching | join(all, [P, ...]) | JOIN_WAIT(all) | wf_test_par.erl, wf_test_join.erl | Kernel | - |
| 4 | Exclusive Choice (XOR-split) | Structural | xor([P, ...]) | XOR_CHOOSE | wf_test_xor.erl | Kernel | - |
| 5 | Simple Merge (XOR-join) | Structural | N/A (implicit) | None | wf_test_xor.erl | Kernel | No explicit opcode |
| 6 | Multi-choice | Advanced Branching | xor([P, ...]) | XOR_CHOOSE | wf_test_xor.erl | Derived | Reuses XOR |
| 7 | Synchronizing Merge | Advanced Branching | join(sync_merge, [P, ...]) | JOIN_WAIT(sync_merge) | wf_test_join.erl | Kernel | wf_exec.erl:329-365 |
| 8 | Discriminator | Advanced Branching | join({first_n,1}, [P, ...]) | JOIN_WAIT(first_complete) | wf_test_join.erl | Kernel | First completion joins |
| 9 | N-out-of-M Join | Advanced Branching | join({n_of_m,N}, [P, ...]) | JOIN_WAIT({n_of_m,N,M}) | wf_test_join.erl | Kernel | - |
| 10 | Arbitrary Cycles (Loop) | State-Based | loop(Policy, P) | LOOP_CHECK, LOOP_BACK | wf_test_seq.erl (nested) | Kernel | wf_exec.erl:396-433 |
| 11 | Cancel Activity | Cancellation | cancel(task, P) | CANCEL_SCOPE({enter,task}) | wf_test_cancel.erl | Stubbed | wf_exec.erl:512-529 (TODO item 008) |
| 12 | Cancel Case | Cancellation | cancel(case, P) | CANCEL_SCOPE({enter,case}) | wf_test_cancel.erl | Stubbed | wf_exec.erl:512-529 (TODO item 008) |
| 13 | Cancel Region | Cancellation | cancel(scope, P) | CANCEL_SCOPE({enter,ScopeId}) | wf_test_cancel.erl | Stubbed | wf_exec.erl:512-529 (TODO item 008) |
| 14 | Multiple Instances (Fixed) | Multiple Instance | mi({fixed,N}, P) | MI_SPAWN({fixed,N}) | wf_test_mi.erl | Not Implemented | Test exists, no opcode handler |
| 15 | Multiple Instances (Dynamic) | Multiple Instance | mi({dynamic,Min,Max}, P) | MI_SPAWN({dynamic,Min,Max}) | wf_test_mi.erl | Not Implemented | Test exists, no opcode handler |
| 16 | Task | Basic | task(Name, Fun) | TASK_EXEC | wf_test_seq.erl | Kernel | - |
| 17 | Deferred Choice | State-Based | defer([P, ...]) | Not implemented | None | Not Implemented | No test coverage |
| 18 | Multi Merge | Advanced Branching | Derived | Multiple JOIN_WAIT | None | Not Implemented | No test coverage |
| 19-43 | Remaining Patterns | Various | N/A | N/A | None | Not Implemented | Out of scope for control-flow engine |

## Kernel Primitives (Opcodes)

From src/wf_vm.erl:13-21, the following opcodes have direct handlers in wf_exec:execute_opcode/2:

- **SEQ_ENTER**: Enter sequence (wf_exec.erl:229-236)
- **SEQ_NEXT**: Advance to next sequence element (wf_exec.erl:238-245)
- **PAR_FORK**: Fork parallel branches (wf_exec.erl:247-283)
- **JOIN_WAIT**: Wait for branch completion with policy (wf_exec.erl:285-365)
- **XOR_CHOOSE**: Choose exclusive branch (wf_exec.erl:367-394)
- **LOOP_CHECK**: Check loop exit condition (wf_exec.erl:396-419)
- **LOOP_BACK**: Jump back to loop start (wf_exec.erl:421-433)
- **CANCEL_SCOPE**: Enter/exit cancellation scope (wf_exec.erl:435-446)
- **TASK_EXEC**: Execute task (wf_exec.erl:448-451)
- **DONE**: Complete workflow (wf_exec.erl:453-456)

**Missing Handler:**
- **MI_SPAWN**: Spawn multiple instances - referenced in tests (wf_test_mi.erl) but no handler in wf_exec:execute_opcode/2

## Join Policies

From src/wf_vm.erl:24-29, implemented in wf_exec.erl:285-365:

- **all**: Wait for all M branches (wf_exec.erl:319-327)
- **{first_n, N}**: Wait for N branches (wf_exec.erl:329-337)
- **{n_of_m, N, M}**: Wait for N of M branches (wf_exec.erl:339-347)
- **first_complete**: Join on first completion (discriminator) (wf_exec.erl:349-356)
- **sync_merge**: Synchronizing merge (wf_exec.erl:358-365)

## Loop Policies

From src/wf_vm.erl:32-35, implemented in wf_exec.erl:396-433:

- **{count, N}**: Loop N times (wf_exec.erl:398-407)
- **while**: Loop while condition true (wf_exec.erl:409-413)
- **until**: Loop until condition true (wf_exec.erl:415-419)

## Multiple Instance Policies

From src/wf_vm.erl:38-40:

- **{fixed, N}**: Spawn N instances (test exists, no handler)
- **{dynamic, Min, Max}**: Spawn between Min and Max instances (test exists, no handler)

## Derived Pattern Examples

### Discriminator (Pattern 8)
```erlang
%% Derived from kernel primitives
Bytecode = [
    {'PAR_FORK', [1, 3, 5, 7]},  % Fork 3 branches
    {'TASK_EXEC', task_a},
    {'TASK_EXEC', task_b},
    {'TASK_EXEC', task_c},
    {'JOIN_WAIT', first_complete},  % Join on first completion
    {'DONE'}
]
```

### N-out-of-M Join (Pattern 9)
```erlang
%% Wait for 2 of 3 branches
Bytecode = [
    {'PAR_FORK', [1, 3, 5]},
    {'TASK_EXEC', task_a},
    {'TASK_EXEC', task_b},
    {'TASK_EXEC', task_c},
    {'JOIN_WAIT', {n_of_m, 2, 3}},
    {'DONE'}
]
```

## Known Limitations

### Cancellation (Patterns 11-13)
- Stubbed implementation in wf_exec.erl:512-529
- Functions `is_scope_cancelled/2` and `propagate_cancellation/2` return false/noop
- Awaiting item 008 for full implementation
- Tests exist but only verify bytecode structure, not runtime behavior

### Multiple Instances (Patterns 14-15)
- wf_test_mi.erl includes non-existent header: `-include("../src/wf_mi.hrl")` (line 11)
- MI_SPAWN opcode has no handler in wf_exec:execute_opcode/2
- Tests cannot run until item 009 (multiple-instance-support) is complete

### Scheduler
- wf_sched module does not exist (referenced in wf_trace.erl:24, wf_test_trace_helpers.erl:67)
- Scheduler policy parameter in wf_test_trace_helpers:run_with_trace/3 is ignored (line 69)
- Deterministic/nondeterministic/replay modes planned (PROMPT.md:140-143) but not implemented

### Effect System
- wf_effect module does not exist
- Effect boundary and receipt system pending item 010
- No way to execute external effects from tasks

## Test Coverage

Patterns with test coverage (from test/ directory):
- Sequence: wf_test_seq.erl (7 tests)
- Parallel: wf_test_par.erl (9 tests)
- XOR: wf_test_xor.erl (7 tests)
- Join: wf_test_join.erl (11 tests)
- Cancel: wf_test_cancel.erl (7 tests)
- MI: wf_test_mi.erl (6 tests - cannot run)
- Determinism: wf_test_determinism.erl (7 tests)

For detailed test methodology, see docs/TESTING.md.
```

#### Success Criteria:

##### Automated Verification:

- [ ] File exists: `test -f docs/PATTERNS.md`
- [ ] Markdown is valid: No broken formatting, tables render correctly
- [ ] All referenced file:line numbers are accurate
- [ ] No broken links to other docs

##### Manual Verification:

- [ ] Pattern table maps all test modules to patterns
- [ ] Kernel primitives list matches opcodes in wf_vm.erl:13-21
- [ ] Limitations section references TODO items correctly (008, 009, 010, 012)
- [ ] Derived pattern examples compile as valid bytecode
- [ ] Test coverage section lists all 8 test modules

**Note**: Complete all automated verification, then manually verify pattern mappings against test modules before proceeding to Phase 2.

---

### Phase 2: Expand TESTING.md

#### Overview

Extend existing docs/TESTING.md with missing sections: determinism/replay testing, Common Test usage, operational testing, and coverage interpretation. Preserve all existing content.

#### Changes Required:

##### 1. Expand docs/TESTING.md

**File**: `docs/TESTING.md`
**Changes**: Add sections after line 266 (end of existing content)

```markdown
## Determinism and Replay Testing

### Determinism Tests

The wf_test_determinism.erl module verifies that workflow execution is deterministic when using the deterministic scheduler policy.

**Key Tests:**
- `determinism_simple_seq_test/0`: Sequence executes identically across runs
- `determinism_par_3_branches_test/0`: Parallel workflows produce stable traces
- `determinism_par_stability_test_/0`: Stability across 10 parallel executions
- `determinism_nested_par_in_seq_test/0`: Nested patterns are deterministic
- `determinism_xor_branch_test/0`: XOR branch selection is stable
- `determinism_xor_stability_test_/0`: XOR stability across 20 runs
- `determinism_mi_fixed_count_test/0`: Multiple instances are deterministic
- `determinism_concurrent_execution_test/0`: Concurrent executions produce structurally identical traces

### Trace Comparison Methodology

From wf_test_trace_helpers.erl:18-49:

```erlang
%% Compare two trace event lists, return ok | {error, Diff}
compare_traces(Events1, Events2) ->
    %% Compares step_seq, opcode, state_before, state_after
    %% Excludes timestamp (always different) and metadata
```

**Fields Compared:**
- `step_seq`: Step sequence number
- `opcode`: Opcode executed (e.g., `{'TASK_EXEC', task_a}`)
- `state_before`: Executor state snapshot before opcode
- `state_after`: Executor state snapshot after opcode

**Fields Excluded:**
- `timestamp`: Always different between runs
- `metadata`: May contain runtime-specific data

**Diff Format:**
```erlang
{error, [{Field, Index}, ...]}  %% Field mismatch at step Index
{error, {length_mismatch, Len1, Len2}}  %% Different trace lengths
```

### Running Determinism Tests

```bash
# Run all determinism tests
rebar3 eunit --module=wf_test_determinism

# Run specific test
rebar3 eunit --test=wf_test_determinism:determinism_simple_seq_test
```

### Replay Testing

The wf_test_replay.erl module tests replay log generation and execution (see wf_trace.erl:182-224).

**Replay Log Format (from wf_trace.hrl):**
```erlang
-record(replay_entry, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode() | undefined,
    scheduler_choice :: wf_sched:choice_entry() | undefined,  % NOT IMPLEMENTED
    effect_result :: {term(), term()} | undefined  % NOT IMPLEMENTED
}).
```

**Current Limitations:**
- `scheduler_choice` field references `wf_sched:choice_entry()` but wf_sched module doesn't exist (wf_trace.erl:24)
- `effect_result` field pending effect system implementation (item 010)
- Replay execution is stubbed (wf_trace:from_replay_log/2 returns {error, invalid_replay_log} for non-empty logs)

**Future Implementation:**
When scheduler and effect system are complete, replay will:
1. Record all nondeterministic choices during initial execution
2. Store in replay_log via `wf_trace:to_replay_log/1` (wf_trace.erl:183-206)
3. Replay by feeding choices via `wf_exec:run/3` with `replay(ChoiceLog)` scheduler policy

## Common Test Integration

### Running Common Test Suites

While EUnit is the primary test framework, Common Test can be used for integration tests:

```bash
# Run all Common Test suites
rebar3 ct

# Run specific suite
rebar3 ct --suite=wf_integration_tests

# Run specific test case
rebar3 ct --suite=wf_integration_tests --testcase=case_lifecycle
```

### Test Suite Structure

Integration test suites should be placed in `test/` directory with `_SUITE.erl` suffix:

```erlang
-module(wf_integration_tests).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([case_lifecycle/1, supervision_restart/1]).

all() -> [case_lifecycle, supervision_restart].

groups() -> [].

%% Test case with Config
case_lifecycle(Config) ->
    %% Start supervisor
    {ok, SupPid} = wf_substrate_sup:start_link(),
    %% Create workflow case
    Bytecode = wf_test_seq:mock_bytecode_seq_2_tasks(),
    ExecState = wf_exec:new(Bytecode),
    %% Execute
    {done, _} = wf_exec:run(ExecState, 1000, undefined),
    %% Cleanup
    gen_server:stop(SupPid),
    ok.
```

### Test Data Directory

Place test data files in `test/data/` directory:

```erlang
load_bytecode_file(Config) ->
    DataDir = ?config(data_dir, Config),
    FilePath = filename:join(DataDir, "example_workflow.bc"),
    {ok, Bytecode} = file:consult(FilePath),
    %% Use Bytecode
    ok.
```

Run with: `rebar3 ct --suite=wf_integration_tests --data_dir=test/data`

## Operational Testing

### Supervision Tree Testing

Test supervision restart strategies by killing child processes:

```erlang
supervision_restart_test() ->
    %% Start supervisor
    {ok, SupPid} = wf_substrate_sup:start_link(),
    %% TODO: Start child process (wf_case_sup not implemented)
    %% ChildPid = wf_case_sup:start_case(Bytecode),
    %% Kill child
    %% exit(ChildPid, kill),
    %% Verify restart
    %% ?assertMatch([{_, ChildPid2, _, _}], supervisor:which_children(SupPid)),
    %% ?assertNot(ChildPid =:= ChildPid2),
    gen_server:stop(SupPid),
    ok.
```

**Current Status:** Cannot test until item 012 (otp-supervision-tree) is complete (wf_substrate_sup.erl:46 has empty child specs).

### Failure Scenario Testing

Test failure modes and recovery:

1. **Case Runner Crash**: Verify supervisor restart logic
2. **Effect Timeout**: Test effect timeout handling (pending item 010)
3. **State Corruption**: Run validation checks (wf_validate:check_soundness/1)
4. **Cancellation Failure**: Verify cancellation propagates correctly (stubbed in item 008)

### Chaos Testing

Randomly terminate processes to verify system resilience:

```erlang
chaos_test() ->
    %% Start workflow
    ExecState = wf_exec:new(Bytecode),
    %% Run partially
    {running, State1} = wf_exec:step(ExecState, undefined),
    %% Kill token (simulate process crash)
    %% TODO: When token is separate process
    %% Verify recovery or graceful failure
    ok.
```

**Note:** Current executor is single-process (wf_exec.erl), so chaos testing requires multi-process implementation.

## Coverage Report Interpretation

### Generating Coverage

```bash
rebar3 cover
```

Report generated in: `_build/test/cover/index.html`

### Coverage Goals

- **Line Coverage**: > 80% for all core modules (wf_exec, wf_vm, wf_trace, wf_validate)
- **Branch Coverage**: > 70% for all conditional logic
- **Pattern Coverage**: All 16 implemented patterns (see docs/PATTERNS.md)

### Interpreting Results

**Good Coverage:**
- wf_exec.erl: Should be > 90% (core opcode dispatch)
- wf_vm.erl: Should be 100% (type definitions only)
- wf_test_*.erl: Should be 60-80% (test helpers and property generators)

**Expected Gaps:**
- wf_exec.erl:512-529 (cancellation stubs, awaiting item 008)
- wf_exec.erl:448-451 (TASK_EXEC is simple, may not hit all branches)
- wf_validate.erl: Lines 431-526 (bounded exploration, hard to hit all paths)

### Improving Coverage

1. **Add Edge Case Tests**: Test N=0, N=1, large N (already done in wf_test_join.erl)
2. **Property Testing**: Use wf_prop.erl to explore code paths (wf_test_term.erl)
3. **Error Path Testing**: Test invalid bytecode, malformed opcodes (wf_validate_tests.erl)
4. **Integration Tests**: Add end-to-end workflows (Common Test suites)

## Manual Testing Procedures

### Running Workflows Interactively

```erlang
%% Start Erlang shell
erl -pa _build/default/lib/wf_substrate/ebin

%% Compile and load
c(wf_exec).
c(wf_test_seq).

%% Create workflow
Bytecode = wf_test_seq:mock_bytecode_seq_2_tasks(),
ExecState = wf_exec:new(Bytecode),

%% Step through
{running, State1} = wf_exec:step(ExecState, undefined),
{running, State2} = wf_exec:step(State1, undefined),

%% Run to completion
{done, DoneState} = wf_exec:run(ExecState, 1000, undefined).

%% Inspect state
wf_exec:is_done(DoneState),
wf_exec:get_step_count(DoneState).
```

### Debugging Failed Tests

1. **Extract Failing Bytecode**: From property test failure
   ```erlang
   %% wf_prop:quickcheck/2 returns {error, Bytecode, Exception}
   ```

2. **Create Reproduction Case**: Add to appropriate test module
   ```erlang
   reproduction_test() ->
       Bytecode = [...],  % Failing bytecode
       ExecState = wf_exec:new(Bytecode),
       ?assertMatch({done, _}, wf_exec:run(ExecState, 1000, undefined)).
   ```

3. **Enable Tracing**: Use wf_trace to see execution
   ```erlang
   {ok, TraceState} = wf_trace:new(full),
   erlang:put(wf_trace_state, TraceState),
   wf_exec:run(ExecState, 1000, undefined),
   Events = wf_trace:get_events(TraceState),
   ```

4. **Inspect State**: Use wf_validate to check for issues
   ```erlang
   Report = wf_validate:validate_bytecode(Bytecode),
   io:format("~p~n", [Report]).
   ```

### Load Testing

Test performance under load:

```erlang
%% Spawn 1000 concurrent workflows
load_test() ->
    Bytecode = wf_test_par:mock_bytecode_par_3_branches(),
    Parent = self(),
    Pids = [spawn(fun() ->
        ExecState = wf_exec:new(Bytecode),
        wf_exec:run(ExecState, 1000, undefined),
        Parent ! {self(), done}
    end) || _ <- lists:seq(1, 1000)],
    %% Wait for completion
    [receive {Pid, done} -> ok end || Pid <- Pids],
    io:format("All workflows completed~n").
```

**Note:** Current executor is single-process per workflow. For true parallelism, use Erlang processes to run multiple workflows concurrently.

## Continuous Integration

### CI Pipeline

`.github/workflows/test.yml` (or equivalent):

```yaml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
      - run: rebar3 compile
      - run: rebar3 eunit
      - run: rebar3 cover
      - run: rebar3 ct
```

### Test Execution Order

1. **Compile**: `rebar3 compile`
2. **Unit Tests**: `rebar3 eunit` (< 10 seconds target)
3. **Coverage**: `rebar3 cover` (verify > 80%)
4. **Integration Tests**: `rebar3 ct` (< 30 seconds target)
5. **Benchmarks**: `wf_bench:run_all()` (verify performance regression)

All tests must pass before merging PR.

### Coverage Enforcement

Add to `rebar.config`:

```erlang
{cover_opts, [verbose]}.
{cover_enabled, true}.
{cover_excl_mods, [wf_prop, wf_test_helpers]}.
```

Enforce in CI: Fail if coverage < 80% (using coveralls or similar).
```

#### Success Criteria:

##### Automated Verification:

- [ ] File exists and is valid markdown
- [ ] All code examples are syntactically valid Erlang
- [ ] All file paths and line references are accurate
- [ ] No broken links to other docs (PATTERNS.md, OPERATIONS.md)

##### Manual Verification:

- [ ] Determinism section accurately describes wf_test_determinism.erl tests
- [ ] Trace comparison format matches wf_test_trace_helpers.erl:18-49
- [ ] Replay limitations reference wf_trace.erl:24 and item 010 correctly
- [ ] Common Test examples follow standard patterns
- [ ] Coverage gaps align with known implementation gaps (items 008, 009, 010, 012)

**Note**: Complete all automated verification, then verify test examples are runnable before proceeding to Phase 3.

---

### Phase 3: Create OPERATIONS.md

#### Overview

Write comprehensive operational guide covering supervision tree, failure modes, telemetry/monitoring, and configuration. Document current state (skeleton) vs planned architecture.

#### Changes Required:

##### 1. Create docs/OPERATIONS.md

**File**: `docs/OPERATIONS.md`
**Changes**: New file with operational guidance

```markdown
# Operations Guide

## Overview

This guide covers operational concerns for deploying and running wf_substrate in production. It includes supervision tree structure, failure modes and recovery, telemetry and monitoring, and configuration options.

**Note:** wf_substrate is currently in early development. Many operational features (scheduler policies, effect system, supervision tree) are planned but not yet implemented. This document distinguishes between current state and planned architecture.

## Supervision Tree

### Current State (Skeleton Only)

From src/wf_substrate_sup.erl:42-47:

```erlang
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [],  %% EMPTY - no children yet
    {ok, {SupFlags, ChildSpecs}}.
```

**Status:** Top-level supervisor exists but has no child specifications (line 46). The supervisor uses `one_for_one` restart strategy.

### Planned Architecture

From PROMPT.md:182-185:

```erlang
%% Top-level supervisor (wf_substrate_sup)
%% - Strategy: one_for_one
%% - Children:
%%   - wf_case_sup: simple_one_for_one (dynamic case runners)
%%   - wf_effect_sup: one_for_one (effect executors)
%%   - wf_trace_sink_sup: one_for_one (trace sinks)
```

**Dependency:** Awaiting item 012 (otp-supervision-tree) for full implementation.

### Restart Strategies

**one_for_one (Top-level):**
- If a child process crashes, only that child is restarted
- Used for: wf_substrate_sup (wf_substrate_sup.erl:43)
- Max restart intensity: 10 times per 60 seconds (wf_substrate_sup.erl:44-45)

**simple_one_for_one (Case supervisor - planned):**
- Dynamic children: One supervisor instance per workflow case
- Each case runner is a child process
- Used for: wf_case_sup (not yet implemented)

**one_for_one (Effect supervisor - planned):**
- Static children: Effect executor processes
- Used for: wf_effect_sup (not yet implemented)

## Failure Modes and Recovery

### Case Runner Crash

**Current State:**
- Case runners are not separate processes (executor is single-process)
- wf_exec:run/3 runs entirely in calling process (wf_exec.erl:152-166)
- No supervision or restart mechanism

**Planned Behavior:**
- Each case runner runs as gen_server under wf_case_sup
- If case runner crashes, supervisor restarts it
- **State Loss:** No persistence yet - case restarts from initial state
- **Future:** Add state persistence (checkpointing) to resume cases

**Recovery Steps (Current):**
1. Detect crash: Process terminates abnormally
2. Investigate: Check logs for crash reason
3. Restart: Create new executor with same bytecode
4. Manual intervention: No automatic recovery

**Recovery Steps (Planned):**
1. Supervisor detects crash
2. wf_case_sup restarts case runner automatically
3. Case runner reloads state from checkpoint (item 006)
4. Resume execution from last committed state

### Effect Timeout

**Status:** Not implemented (pending item 010)

**Planned Behavior (PROMPT.md:145-155):**
- Task can request external effect: `{effect, Spec, Continuation}`
- wf_effect manager executes Spec with timeout
- On timeout:
  - Effect is cancelled
  - Case receives error receipt
  - Workflow can handle error or propagate

**Recovery Steps (Planned):**
1. Detect timeout: Effect exceeds configured duration
2. Cancel effect: Signal effect executor to stop
3. Generate error receipt: `{error, timeout, EffectSpec}`
4. Handle error: Workflow decides whether to retry, abort, or continue
5. Retry policy: Configurable max retries, backoff strategy

### State Corruption Detection

**Current Mechanisms:**

**Validation Checks (wf_validate.erl):**
- `check_dead_transitions/1`: No transitions from unreachable places (wf_validate.erl:532-561)
- `check_option_to_complete/1`: Option to complete exists (wf_validate.erl:563-589)
- `check_proper_completion/1`: Proper completion semantics (wf_validate.erl:591-607)
- `check_deadlock/1`: No deadlocks (wf_validate.erl:609-633)
- `check_soundness/1`: All 5 checks combined (wf_validate.erl:635-652)

**Runtime Detection:**
- Executor maintains invariants:
  - All tokens have valid IPs (wf_exec.erl:29-34)
  - Join counters have completed ≤ required (wf_exec.erl:45-51)
  - Scope stack is consistent (wf_exec.erl:61)

**Recovery Steps:**
1. Run validation: `wf_validate:validate_bytecode(Bytecode)`
2. Check report: Look for `#issue{severity = error}` in `#report{issues = [...]}` (wf_validate.erl:11-20)
3. Fix bytecode: Address validation errors
4. Retry execution: Create new executor with fixed bytecode

**No Automatic Recovery:** State corruption requires manual bytecode correction.

### Cancellation Failure

**Current State:** Stubbed implementation (wf_exec.erl:512-529)

```erlang
is_scope_cancelled(_ExecState, _ScopeId) ->
    %% TODO: Implement scope cancellation checking (item 008)
    false.

propagate_cancellation(ExecState, _ScopeId) ->
    %% TODO: Implement cancellation propagation (item 008)
    ExecState.
```

**Planned Behavior:**
- Cancellation scope entered via `CANCEL_SCOPE({enter, ScopeId})` (wf_exec.erl:437-441)
- Scope active while in scope_stack (wf_exec.erl:61)
- On cancel: propagate_cancellation/2 marks all tokens in scope as cancelled
- Cancelled tokens are removed from active set (wf_exec.erl:29-34)

**Recovery Steps (Planned):**
1. Detect cancellation failure: Tokens remain active after cancel signal
2. Force cancel: Re-send cancel signal to scope
3. Clean up: Remove stuck tokens manually
4. Log incident: Record cancellation failure for debugging

**Dependency:** Awaiting item 008 (cancellation-semantics)

## Telemetry and Tracing

### Trace Levels

From src/wf_trace.erl:38:

```erlang
-type trace_level() :: none | min | full.
```

**none (Zero Overhead):**
- No tracing
- No performance impact
- Used in production when tracing not needed

**min (Structural Events Only):**
- Traces only high-level events (wf_trace.erl:232-237)
- Opcodes traced: PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE
- Low overhead (~5-10% performance impact)
- Use for debugging workflow structure

**full (All Events + State Snapshots):**
- Traces every opcode execution (wf_trace.erl:246-252)
- Includes state snapshots (wf_trace.erl:258-261)
- High overhead (~50-100% performance impact)
- Use for debugging and failure analysis

### Trace Sinks

From src/wf_trace.erl:40-44:

```erlang
-type trace_sink() ::
    {callback, fun((#trace_event{}) -> ok)} |
    {ets, ets:tid()} |
    {process, pid()} |
    {replay, replay_log()}.
```

**callback (User Function):**
- Calls user-provided function for each event
- Use case: Custom logging, metrics collection
- Example:
  ```erlang
  Sink = {callback, fun(Event) ->
      logger:info("Step ~p: ~p", [Event#trace_event.step_seq, Event#trace_event.opcode])
  end},
  ```

**ets (In-Memory Table):**
- Default sink (wf_trace.erl:82-86)
- Named table: `wf_trace_events`
- Type: `bag` (allows duplicate events per step_seq)
- Concurrency: `{read_concurrency, true}`
- Use case: In-memory trace collection for testing
- Retrieval: `wf_trace:get_events/1` (wf_trace.erl:116-139)

**process (Message Passing):**
- Sends trace events to pid
- Use case: External trace consumer (e.g., logging service)
- Example:
  ```erlang
  Sink = {process, TraceConsumerPid},
  ```

**replay (Deterministic Replay):**
- Stores replay log for later execution
- Use case: Reproducing bugs, testing
- Creation: `wf_trace:to_replay_log/1` (wf_trace.erl:183-206)
- Loading: `wf_trace:from_replay_log/1` (wf_trace.erl:209-224)
- **Limitation:** Replay execution not implemented (scheduler required)

### Configuring Tracing

**Set Trace Level:**
```erlang
%% In process dictionary (per-execution)
wf_trace:set_level(full).

%% When creating trace state
{ok, TraceState} = wf_trace:new(full).
```

**Set Trace Sink:**
```erlang
%% Change sink after creation
wf_trace:set_sink({process, Pid}).
```

**Enable Tracing for Execution:**
```erlang
%% Create trace state
{ok, TraceState} = wf_trace:new(full),

%% Store in process dictionary (wf_exec:step/2 checks for this)
erlang:put(wf_trace_state, TraceState),

%% Execute workflow (will emit events)
ExecState = wf_exec:new(Bytecode),
{done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

%% Retrieve events
Events = wf_trace:get_events(TraceState).
```

**Filtering Events:**
```erlang
%% Filter by opcode
ParEvents = wf_trace:filter(Events, fun(E) ->
    element(1, E#trace_event.opcode) =:= 'PAR_FORK'
end),

%% Filter by step range
RecentEvents = wf_trace:filter(Events, fun(E) ->
    E#trace_event.step_seq >= 100
end).
```

### Log Format

**Trace Event Record (wf_trace.hrl:4-8):**
```erlang
-record(trace_event, {
    step_seq :: non_neg_integer(),      %% Step number
    opcode :: wf_vm:opcode(),           %% Opcode executed
    state_before :: binary() | undefined,%% State snapshot before
    state_after :: binary() | undefined,  %% State snapshot after
    timestamp :: erlang:monotonic_time(),%% Monotonic timestamp
    scope :: [term()],                   %% Cancellation scope stack
    branch_id :: term() | undefined,     %% Parallel branch ID
    metadata :: map()                    %% Additional info
}).
```

**Example Event:**
```erlang
#trace_event{
    step_seq = 42,
    opcode = {'PAR_FORK', [1, 3, 5]},
    state_before = <<"<<exec_state binary>>">>,
    state_after = <<"<<exec_state binary>>">>,
    timestamp = 1699876543210,
    scope = [root, outer_scope],
    branch_id = undefined,
    metadata = #{branches => 3}
}
```

### Telemetry Integration

**Current State:** No built-in telemetry (OpenTelemetry, Prometheus, etc.)

**Recommended Approach:**
1. Use `callback` sink to emit metrics
2. Example with Prometheus:
   ```erlang
   Sink = {callback, fun(Event) ->
       prometheus_histogram:observe(
           wf_workflow_duration_seconds,
           [element(1, Event#trace_event.opcode)],
           erlang:monotonic_time() - Event#trace_event.timestamp
       )
   end},
   ```

3. Example with OpenTelemetry:
   ```erlang
   Sink = {callback, fun(Event) ->
       opentelemetry:record_span(Event#trace_event.metadata#{span_name => wf_step})
   end},
   ```

## Monitoring Running Systems

### Inspecting Active Cases

**Current State:** No API available (case runners not implemented)

**Planned API:**
```erlang
%% List all active cases
wf_substrate:list_cases() -> [{case_id(), pid(), wf_vm:wf_bc()}].

%% Get case status
wf_substrate:case_status(CaseId) -> #{
    status => running | done | blocked | cancelled,
    step_count => non_neg_integer(),
    active_tokens => [term()],
    join_counters => [term()]
}.
```

**Workaround (Current):**
```erlang
%% If running executor in separate process
%% (manual process spawning required)
ProcessInfo = erlang:process_info(CaseRunnerPid),
%% Look for messages, heap size, reductions
```

### Checking for Stuck Workflows

**Symptoms:**
- Executor status = `running` but no progress
- Tokens stuck with status = `active` but no enabled transitions
- Long-running loops (while/until without exit condition)

**Detection Methods:**

1. **Manual Inspection:**
   ```erlang
   %% Check executor status
   wf_exec:is_blocked(ExecState),  %% Returns true if no progress possible

   %% Check active tokens
   Tokens = maps:values(ExecState#exec_state.tokens),
   StuckTokens = [T || T <- Tokens, T#token.status =:= active],
   ```

2. **Validation Check:**
   ```erlang
   %% Run deadlock detection
   {ok, Report} = wf_validate:check_deadlock(Bytecode),
   %% Check for deadlock issues in Report#report.issues
   ```

3. **Automatic Monitoring (Planned):**
   - Use `callback` trace sink to detect repeated opcodes
   - Alert if same opcode executed > N times in loop
   - Detect long-running tasks (timeout config)

**Recovery:**
1. Identify stuck case: Use inspection methods above
2. Cancel case: Send cancel signal (if cancellation implemented)
3. Kill process: Last resort (loss of state)
4. Restart: Create new executor with same bytecode

### Retrieving Trace Logs

**From ETS Table:**
```erlang
%% Get all events from trace state
Events = wf_trace:get_events(TraceState).

%% Access ETS table directly (if table not deleted)
Table = ets:whereis(wf_trace_events),
Events = ets:tab2list(Table).
```

**From Process Sink:**
```erlang
%% If using {process, Pid} sink
%% Events are sent as messages: {trace_event, Event}
receive
    {trace_event, Event} ->
        handle_event(Event)
end.
```

**Converting to Replay Log:**
```erlang
%% Extract replay log from trace events
ReplayLog = wf_trace:to_replay_log(Events),

%% Replay log can be saved to file for later analysis
ok = file:write_file("trace.log", io_lib:format("~p.~n", [ReplayLog])).
```

### Health Checks

**Executor Health:**
```erlang
check_executor_health(ExecState) ->
    #{
        status => ExecState#exec_state.status,
        steps => ExecState#exec_state.step_count,
        tokens => maps:size(ExecState#exec_state.tokens),
        is_done => wf_exec:is_done(ExecState),
        is_blocked => wf_exec:is_blocked(ExecState),
        scope_depth => wf_exec:get_scope_stack_depth(ExecState)
    }.
```

**Supervisor Health:**
```erlang
check_supervisor_health() ->
    Children = supervisor:which_children(wf_substrate_sup),
    #{
        child_count => length(Children),
        children => [{Id, Pid, Type, Modules} || {Id, Pid, Type, Modules} <- Children]
    }.
```

**System Health:**
```erlang
check_system_health() ->
    #{
        memory => erlang:memory(total),
        process_count => erlang:system_info(process_count),
        scheduler_utilization => erlang:statistics(scheduler_wall_time)
    }.
```

## Configuration Options

### Executor Configuration

**step_quanta (from wf_exec:run/3):**
- Type: `non_neg_integer()`
- Description: Maximum number of steps to execute per call
- Default: 1000
- Usage: `wf_exec:run(ExecState, StepQuanta, SchedulerPolicy)`
- Effect: Larger values = longer bursts, less cooperative scheduling

**Example:**
```erlang
%% Run up to 100 steps (fine-grained preemption)
wf_exec:run(ExecState, 100, undefined),

%% Run up to 10000 steps (coarse-grained)
wf_exec:run(ExecState, 10000, undefined).
```

### Trace Configuration

**trace_level:**
- Type: `none | min | full`
- Default: `none`
- Set: `wf_trace:set_level/1`
- Overhead: none (0%), min (5-10%), full (50-100%)

**trace_sink:**
- Type: `callback | ets | process | replay`
- Default: `ets` (named table)
- Set: `wf_trace:set_sink/1`
- Choose based on use case:
  - `ets`: Testing, in-memory collection
  - `process`: External log consumer
  - `callback`: Custom processing, metrics
  - `replay`: Deterministic replay

### Scheduler Policy (Planned)

**From PROMPT.md:140-143:**
- Type: `deterministic | nondeterministic | replay(ChoiceLog)`
- Status: Not implemented (wf_sched module doesn't exist)
- Usage: `wf_exec:run(ExecState, StepQuanta, SchedulerPolicy)`

**Policies:**
1. **deterministic:**
   - Stable ordering of enabled transitions
   - First active token selected (wf_exec.erl:246-252)
   - First branch selected in XOR (wf_exec.erl:398)
   - Use case: Reproducible execution, testing

2. **nondeterministic:**
   - Random selection of enabled transitions
   - Use case: Exploring all execution paths, stress testing

3. **replay(ChoiceLog):**
   - Replay recorded choices from previous execution
   - Use case: Reproducing bugs, debugging

**Current Status:** Scheduler policy parameter is ignored (wf_test_trace_helpers.erl:69)

### Effect Handler Configuration (Planned)

**From PROMPT.md:284-289:**
- Type: `module()`
- Description: Callback module for effect execution
- Status: Not implemented (pending item 010)

**Planned Configuration:**
```erlang
%% In application environment (sys.config)
{wf_substrate, [
    {effect_handler, my_effect_handler},
    {effect_timeout, 5000}  %% milliseconds
]}.

%% Effect handler callback:
-callback execute_effect(EffectSpec, Context) ->
    {ok, Result} | {error, Reason} | {timeout, EffectSpec}.
```

### Effect Budgets (Planned)

**From PROMPT.md:296:**
- Description: Limit number of effects per workflow
- Status: Not implemented
- Use case: Prevent runaway effect execution

**Planned Configuration:**
```erlang
{wf_substrate, [
    {max_effects, 100},  %% Maximum effects per workflow
    {effect_timeout, 5000}  %% Per-effect timeout
]}.
```

### Timeout Configuration

**Operation Timeouts:**
- `gen_server:call/2` timeout: 5000ms (default)
- `wf_exec:run/3` burst timeout: None (cooperative)
- Effect timeout: Planned (item 010)

**Recommendations:**
- Set gen_server timeouts based on workflow complexity
- Monitor step counts to detect infinite loops
- Use validation to check for deadlocks before deployment

## Deployment

### Building for Production

```bash
# Compile with optimizations
rebar3 compile

# Build release
rebar3 release

# Tarball release
rebar3 tar
```

**Output:** `_build/default/rel/wf_substrate/`

### Running as Service

**Systemd Unit File:**
```ini
[Unit]
Description=wf_substrate workflow engine
After=network.target

[Service]
Type=simple
User=wf_substrate
WorkingDirectory=/opt/wf_substrate
ExecStart=/opt/wf_substrate/bin/wf_substrate foreground
ExecStop=/opt/wf_substrate/bin/wf_substrate stop
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

**Enable and start:**
```bash
sudo systemctl enable wf_substrate
sudo systemctl start wf_substrate
sudo systemctl status wf_substrate
```

### Environment Variables

```bash
# Erlang VM options
export ERL_OPTS="+K true +A 128 +SDio 100"

# Distribution
export ERLANG_COOKIE="wf_substrate_cookie"
export NODE_NAME="wf_substrate@127.0.0.1"

# Logging
export LOG_LEVEL="info"

# Start application
erl -sname $NODE_NAME -setcookie $ERLANG_COOKIE $ERL_OPTS -pa ebin -s wf_substrate
```

### Upgrading

**Code Reload (Hot Upgrade):**
```erlang
%% Check current version
application:which_applications().

%% Load new version
l(wf_exec).
l(wf_vm).

%% Verify
wf_exec:new([{'DONE'}]).
```

**Release Upgrade:**
```bash
# Build new release
rebar3 release

# Upgrade script
rebar3 relups

# Install upgrade
wf_substrate upgrade <release_package>.tar.gz
```

## Troubleshooting

### Common Issues

**1. Workflow Stuck in Running State**
- Symptoms: `wf_exec:is_done/1` returns false, no progress
- Causes: Deadlock, infinite loop, waiting for join
- Debug: Check tokens, join counters, validation
- Fix: Add loop exit condition, fix join policy, use timeout

**2. High Memory Usage**
- Symptoms: Process memory grows continuously
- Causes: Large workflows, many active cases, trace events not cleaned up
- Debug: `erlang:process_info(self(), memory)`
- Fix: Clear ETS trace tables, limit concurrent cases, use trace level=none

**3. Slow Execution**
- Symptoms: Steps/sec lower than expected
- Causes: Trace level=full, large bytecode, complex join policies
- Debug: Check trace level, use wf_bench:run_all() for metrics
- Fix: Reduce trace level, optimize bytecode, use first_complete joins

**4. Crash on Startup**
- Symptoms: Application fails to start
- Causes: Missing dependencies, invalid config
- Debug: Check logs for `{error, Reason}`
- Fix: Install dependencies, fix sys.config

### Debugging Checklist

1. **Check Executor Status:**
   ```erlang
   wf_exec:is_done(ExecState),
   wf_exec:is_blocked(ExecState),
   wf_exec:get_step_count(ExecState).
   ```

2. **Inspect Tokens:**
   ```erlang
   Tokens = maps:values(ExecState#exec_state.tokens),
   [io:format("Token ~p: IP=~p, Status=~p~n", [T#token.token_id, T#token.ip, T#token.status]) || T <- Tokens].
   ```

3. **Validate Bytecode:**
   ```erlang
   {ok, Report} = wf_validate:validate_bytecode(Bytecode),
   io:format("~p~n", [Report]).
   ```

4. **Enable Tracing:**
   ```erlang
   wf_trace:set_level(full),
   erlang:put(wf_trace_state, TraceState),
   %% Run workflow
   Events = wf_trace:get_events(TraceState),
   io:format("~p~n", [Events]).
   ```

5. **Check Process Info:**
   ```erlang
   erlang:process_info(self(), memory),
   erlang:process_info(self(), message_queue_len),
   erlang:process_info(self(), dictionary).
   ```

## Further Reading

- **Pattern Coverage:** docs/PATTERNS.md - All implemented workflow patterns
- **Testing Strategy:** docs/TESTING.md - How to test workflows
- **Source of Truth:** PROMPT.md - Architectural requirements
- **Validation:** src/wf_validate.erl - Correctness checks
- **Tracing:** src/wf_trace.erl - Telemetry infrastructure
```

#### Success Criteria:

##### Automated Verification:

- [ ] File exists: `test -f docs/OPERATIONS.md`
- [ ] Markdown is valid with proper formatting
- [ ] All code examples are syntactically valid Erlang
- [ ] All file:line references are accurate

##### Manual Verification:

- [ ] Current state sections accurately reflect skeleton implementation (wf_substrate_sup.erl:46)
- [ ] Planned sections reference PROMPT.md correctly
- [ ] Trace sink examples compile and match wf_trace.erl API
- [ ] Troubleshooting checklist commands are valid
- [ ] Dependencies on items 006, 008, 009, 010, 012 are noted

**Note**: Complete all automated verification, then verify operational examples are conceptually sound (even if not yet runnable due to missing features).

---

## Testing Strategy

### Documentation Verification Tests

All three documentation files should be verified for:

1. **Internal Consistency:**
   - Cross-references between docs are accurate
   - Code examples use correct syntax
   - File:line references point to actual code

2. **Completeness:**
   - All test modules are referenced in TESTING.md
   - All implemented patterns are in PATTERNS.md
   - All operational topics are covered in OPERATIONS.md

3. **Accuracy:**
   - Opcodes list matches wf_vm.erl:13-21
   - Test module descriptions match actual test files
   - Trace levels match wf_trace.erl:38

### Manual Verification Steps

1. **PATTERNS.md:**
   - Verify pattern table against test modules
   - Check kernel primitives list matches wf_exec:execute_opcode/2 cases
   - Confirm derived pattern examples compile

2. **TESTING.md:**
   - Run test commands: `rebar3 eunit --module=wf_test_seq`
   - Check coverage report: `rebar3 cover`
   - Verify determinism tests exist: `ls test/wf_test_determinism.erl`

3. **OPERATIONS.md:**
   - Check supervisor state: `grep "ChildSpecs = \[\]" src/wf_substrate_sup.erl`
   - Verify trace sink types match wf_trace.erl:40-44
   - Confirm validation functions exist in wf_validate.erl

### Link Verification

Check for broken links between docs:

```bash
# Check for relative links
grep -n "](PATTERNS.md" docs/TESTING.md
grep -n "](TESTING.md" docs/OPERATIONS.md
grep -n "](OPERATIONS.md" docs/PATTERNS.md

# Check for file references
grep -n "wf_exec.erl:" docs/*.md
grep -n "wf_trace.erl:" docs/*.md
```

All links should point to existing files and accurate line numbers.

## Migration Notes

This documentation item adds three new files but does not change any existing code. No migration is required.

**Documentation-only changes:**
- docs/PATTERNS.md: New file
- docs/TESTING.md: Expanded (preserves existing content)
- docs/OPERATIONS.md: New file

**No impact on:**
- Source code
- Tests
- Build configuration
- Application behavior

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/017-pattern-coverage-docs/research.md`
- Pattern Specification: `PROMPT.md:75-91` (control-flow patterns)
- Executor Implementation: `src/wf_exec.erl:1-598`
- Opcode Definitions: `src/wf_vm.erl:1-50`
- Tracing Infrastructure: `src/wf_trace.erl:1-335`
- Validation Backend: `src/wf_validate.erl:1-653`
- Test Modules: `test/wf_test_*.erl` (8 modules)
- Existing Testing Guide: `docs/TESTING.md:1-266`
- Supervision Tree: `src/wf_substrate_sup.erl:1-52`
- Benchmarks: `src/wf_bench.erl:1-355`
- Promised Items: 006 (state-store), 008 (cancellation), 009 (multiple-instance), 010 (effect-boundary), 012 (otp-supervision-tree)
