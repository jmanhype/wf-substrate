# Implement performance benchmarks Implementation Plan

## Implementation Plan Title

Performance Benchmark Suite for wf-substrate Bytecode Executor

## Overview

Implement `wf_bench.erl`: a comprehensive microbenchmarking suite that measures and validates the performance characteristics of the wf-substrate executor. The benchmarks will demonstrate that the executor's per-step overhead is O(1) and does not scale with AST complexity, proving the effectiveness of the flat bytecode design versus recursive AST dispatch.

The benchmark suite will measure:
1. **Sequence throughput** - 10,000 sequential tasks (validates O(1) per-step cost)
2. **Parallel fork+join** - 100 branches with synchronizing join (measures fork/join overhead)
3. **Repeated discriminator** - 1,000 iterations of first_complete pattern (measures cancellation latency)
4. **Deep nesting** - 100 levels of nested seq/par (validates flat bytecode advantage)
5. **Multiple instances** - 100 fixed instances (DEFERRED - requires MI_SPAWN implementation)
6. **State store** - 10,000 mutations (DEFERRED - requires wf_state module)

All benchmarks report: `{name, steps, wall_time_us, steps_per_sec, memory_words}`.

## Current State

**Existing Implementation:**

**Executor Core** (`src/wf_exec.erl`):
- Lines 78-101: `new/1` creates executor state with step counter initialized to 0
- Lines 113-146: `step/2` executes single opcode, increments step_count, returns updated state and trace event
- Lines 148-165: `run/3` executes N quanta with yielding support, suitable for sustained throughput measurement
- Lines 28-51: Core records: `#token{}`, `#branch_info{}`, `#join_counter{}` defined inline (no separate .hrl file)
- Lines 259-323: `execute_par_fork/2` - spawns N branch tokens, creates join counter (1 step)
- Lines 325-363: `execute_join_wait/2` - checks join counter, blocks or continues (1 step)

**Bytecode Type System** (`src/wf_vm.erl`):
- Lines 9-21: Opcode type definitions (generic tuples with atom tags)
- Lines 23-29: Join policies: all, {first_n, N}, {n_of_m, N, M}, first_complete, sync_merge
- Lines 31-35: Loop policies: {count, N}, while, until
- Lines 37-40: MI policies: {fixed, N}, {dynamic, Min, Max} (type defined but not implemented)

**Test Patterns for Bytecode Generation** (`test/wf_test_seq.erl`, `test/wf_test_par.erl`):
- Lines 179-185: `generate_seq_n_tasks/1` - generates N-task sequence (pattern for benchmark #1)
- Lines 208-218: `mock_bytecode_par_n_branches/1` - generates N-branch parallel (pattern for benchmark #2)
- Lines 66-76: `mock_bytecode_par_first_complete/1` - N-branch with first_complete (pattern for benchmark #3)

**Missing Components:**
1. **wf_mi.erl** - Multiple instance module does NOT exist (MI_SPAWN opcode not implemented in executor)
2. **wf_state.erl** - State store module does NOT exist (item 006 not implemented)
3. **wf_exec.hrl** - Header file does NOT exist (records defined inline in wf_exec.erl)
4. **wf_mi.hrl** - Header file does NOT exist (referenced in tests but file missing)
5. **wf_bench.erl** - Benchmark module to be created

**Constraints Discovered:**
- Tests in `wf_test_mi.erl` include non-existent `wf_mi.hrl` - those tests would fail to compile
- `wf_exec.erl` has no MI_SPAWN handler (lines 186-208 show opcode dispatch, no mi_spawn case)
- No loop support in bytecode for repeated patterns (LOOP_CHECK exists but requires counter context setup)
- Must use `erlang:monotonic_time(microsecond)` for timing (no external benchmarking libraries)
- Must use `erlang:process_info(self(), memory)` for memory measurement

## Desired End State

**Deliverable:** `src/wf_bench.erl` module with the following structure:

```erlang
-module(wf_bench).

%% Benchmark API
-export([
    bench_seq_throughput/0,
    bench_par_fork_join/0,
    bench_discriminator_repeat/0,
    bench_deep_nesting/0,
    bench_mi_instances/0,       % Deferred: returns skipped result
    bench_state_store/0,         % Deferred: returns skipped result
    run_all/0
]).

%% Internal helpers
-export([
    run_benchmark/2,
    generate_seq_n_tasks/1,
    generate_par_n_branches/1,
    generate_discriminator_pattern/1,
    generate_deep_nesting/1,
    format_result_table/1
]).
```

**Benchmark Result Record:**
```erlang
-record(bench_result, {
    name :: binary(),
    steps :: non_neg_integer(),
    wall_time_us :: non_neg_integer(),
    steps_per_sec :: float(),
    memory_words :: non_neg_integer()
}).
```

**Expected Output Table Format:**
```
| Benchmark                      |      Steps |   Time (us) |       Steps/sec | Memory (words) |
|--------------------------------+------------+------------+-----------------+----------------|
| seq_throughput_10000_tasks     |      20000 |      12345 |       1620452.1 |          12345 |
| par_fork_join_100_branches     |        202 |       1234 |      16370016.5 |           1234 |
| discriminator_repeat_1000_iter |       5000 |       5678 |        880593.1 |           5678 |
| deep_nesting_100_levels        |        400 |        456 |       8771929.8 |            456 |
| mi_instances_100_fixed         |          0 |          0 |             0.0 |              0 | * SKIPPED: MI_SPAWN not implemented *
| state_store_10000_mutations    |          0 |          0 |             0.0 |              0 | * SKIPPED: wf_state not implemented *
```

**Key Invariant Demonstrated:**
Per-step overhead is bounded and O(1). Steps/sec should remain approximately constant regardless of:
- Sequence length (benchmark #1)
- Branch count (benchmark #2)
- Nesting depth (benchmark #4)

### Key Discoveries:

- **Pattern from `wf_test_seq.erl:179-185`**: Generate N-task sequences with `[{'TASK_EXEC', task_N}, {'SEQ_NEXT', Target}]` pattern
- **Pattern from `wf_test_par.erl:208-218`**: Generate N-branch parallels with `[{'PAR_FORK', TargetIPs}, BranchBytecode..., {'JOIN_WAIT', all}]`
- **Pattern from `wf_test_par.erl:66-76`**: First-join discriminator with `{'JOIN_WAIT', first_complete}` - executor lines 325-363 show first_complete cancellation handling
- **Executor step counting**: `wf_exec:get_step_count/1` (lines 113-116) returns accurate step count for all opcodes
- **Memory measurement**: Use `erlang:process_info(self(), memory)` before/after, compute delta in words
- **Missing modules confirmed**: MI_SPAWN not in executor dispatch (lines 186-208), wf_state.erl doesn't exist

## What We're NOT Doing

- **Implementing MI_SPAWN opcode** - This is item 009's responsibility; benchmark #5 will return skipped result
- **Implementing wf_state module** - This is item 006's responsibility; benchmark #6 will return skipped result
- **Implementing loop support for benchmarks** - Will manually unroll loops in bytecode (1000 iterations = 5000 opcodes)
- **Statistical analysis** - Single run per benchmark (no median of 10, no warm-up iterations) - BEAM has no JIT, warm-up not needed
- **Cross-process benchmarks** - All benchmarks run in single process (no scheduler interference testing)
- **CPU profiling integration** - Not using fprof or eprof (manual timing only)
- **External benchmarking libraries** - Using only erlang:monotonic_time/1 and erlang:process_info/2
- **Creating wf_exec.hrl header file** - Records stay inline in wf_exec.erl, benchmarks use `-include_lib("eunit/include/eunit.hrl")` pattern
- **Fixing wf_test_mi.erl compilation** - Out of scope; that test file's missing include is a separate issue

## Implementation Approach

**Strategy:** Create a standalone benchmark module that:
1. Adapts bytecode generation patterns from existing test modules
2. Executes benchmarks using `wf_exec:run/3` with high quanta limit
3. Measures wall-clock time with `erlang:monotonic_time(microsecond)`
4. Measures memory delta with `erlang:process_info(self(), memory)`
5. Prints formatted result table using `io:format/2`
6. Returns skipped results for benchmarks #5 and #6 with clear skip reason

**Rationale:**
- **Single-process execution**: Avoids scheduler complexity, focuses on executor overhead
- **Manual loop unrolling**: LOOP_CHECK requires context setup (counter in ctx), manual unroll is simpler
- **No warm-up**: BEAM is interpreted, no JIT compilation to warm up
- **No statistical aggregation**: Benchmarking executor overhead, not system variance; single run sufficient for O(1) validation
- **Module location**: Place in `src/` as production module (not test/) because it's a deliverable (wf_bench.erl)

**Decisions Made:**
1. **Loop unrolling for benchmark #3**: Generate 1000 discriminator iterations manually (5000 opcodes total) rather than implementing loop counter in context
2. **MI benchmark (#5)**: Return skipped result with message "SKIPPED: MI_SPAWN opcode not implemented (item 009)"
3. **State store benchmark (#6)**: Return skipped result with message "SKIPPED: wf_state module not implemented (item 006)"
4. **Memory measurement**: Measure delta (after - before) in words, report absolute value
5. **Bytecode generation timing**: NOT included in measurement (pre-generate all bytecode before starting timer)
6. **GC before benchmarks**: Call `erlang:garbage_collect()` before each benchmark to reduce noise
7. **Result table**: Use fixed-width columns with `~-30s`, `~10w`, `~12w`, `~15.2f`, `~12w` format specifiers

---

## Phases

### Phase 1: Scaffolding and Result Reporting

#### Overview

Create the benchmark module structure with result record, reporting infrastructure, and the `run_all/0` entry point. This phase establishes the framework that all subsequent benchmarks will use.

#### Changes Required:

##### 1. Create `src/wf_bench.erl` module

**File**: `src/wf_bench.erl`
**Changes**: Create new file with module exports, includes, and result record

```erlang
-module(wf_bench).

%% Benchmark API
-export([
    bench_seq_throughput/0,
    bench_par_fork_join/0,
    bench_discriminator_repeat/0,
    bench_deep_nesting/0,
    bench_mi_instances/0,
    bench_state_store/0,
    run_all/0
]).

%% Include executor records (defined inline in wf_exec.erl)
-include_lib("eunit/include/eunit.hrl").

%% Benchmark result record
-record(bench_result, {
    name :: binary(),
    steps :: non_neg_integer(),
    wall_time_us :: non_neg_integer(),
    steps_per_sec :: float(),
    memory_words :: non_neg_integer()
}).

%% Type spec
-type bench_result() :: #bench_result{}.
```

##### 2. Implement result formatting

**File**: `src/wf_bench.erl`
**Changes**: Add `format_result_table/1` function to print formatted results

```erlang
%% @doc Format and print benchmark results as table
-spec format_result_table([bench_result()]) -> ok.
format_result_table(Results) ->
    %% Print header
    Header = "| ~-30s | ~10s | ~12s | ~15s | ~12s |~n",
    io:format(Header, ["Benchmark", "Steps", "Time (us)", "Steps/sec", "Memory (words)"]),
    io:format("~s~n", [lists:duplicate(98, $-)]),

    %% Print each result
    lists:foreach(fun(Result) ->
        case Result of
            #bench_result{steps = 0} ->
                %% Skipped benchmark
                io:format("| ~-30s | ~10s | ~12s | ~15s | ~12s |~n",
                          [Result#bench_result.name, "SKIPPED", "", "", ""]);
            _ ->
                %% Normal result
                io:format("| ~-30s | ~10w | ~12w | ~15.2f | ~12w |~n",
                          [Result#bench_result.name,
                           Result#bench_result.steps,
                           Result#bench_result.wall_time_us,
                           Result#bench_result.steps_per_sec,
                           Result#bench_result.memory_words])
        end
    end, Results),
    ok.
```

##### 3. Implement core benchmark runner

**File**: `src/wf_bench.erl`
**Changes**: Add `run_benchmark/2` function that executes bytecode and measures performance

```erlang
%% @doc Run a single benchmark with timing and memory measurement
-spec run_benchmark(binary(), wf_vm:wf_bc()) -> bench_result().
run_benchmark(Name, Bytecode) ->
    %% Force GC before measurement to reduce noise
    erlang:garbage_collect(),

    %% Measure initial memory
    {memory, MemBefore} = erlang:process_info(self(), memory),

    %% Execute and time
    StartTime = erlang:monotonic_time(microsecond),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 1000000, undefined),
    EndTime = erlang:monotonic_time(microsecond),

    %% Measure final memory
    {memory, MemAfter} = erlang:process_info(self(), memory),

    %% Compute metrics
    Steps = wf_exec:get_step_count(ExecState1),
    WallTimeUs = EndTime - StartTime,
    StepsPerSec = case WallTimeUs of
        0 -> 0.0;
        _ -> (Steps * 1000000) / WallTimeUs
    end,
    MemoryWords = abs(MemAfter - MemBefore),

    #bench_result{
        name = Name,
        steps = Steps,
        wall_time_us = WallTimeUs,
        steps_per_sec = StepsPerSec,
        memory_words = MemoryWords
    }.
```

##### 4. Implement stub benchmarks

**File**: `src/wf_bench.erl`
**Changes**: Add stub implementations for all 6 benchmarks (to be filled in Phase 2)

```erlang
%% @doc Benchmark 1: Sequence throughput (10,000 tasks)
-spec bench_seq_throughput() -> bench_result().
bench_seq_throughput() ->
    Bytecode = generate_seq_n_tasks(10000),
    run_benchmark(<<"seq_throughput_10000_tasks">>, Bytecode).

%% @doc Benchmark 2: Parallel fork+join (100 branches)
-spec bench_par_fork_join() -> bench_result().
bench_par_fork_join() ->
    Bytecode = generate_par_n_branches(100),
    run_benchmark(<<"par_fork_join_100_branches">>, Bytecode).

%% @doc Benchmark 3: Repeated discriminator (1,000 iterations)
-spec bench_discriminator_repeat() -> bench_result().
bench_discriminator_repeat() ->
    Bytecode = generate_discriminator_repeat(1000),
    run_benchmark(<<"discriminator_repeat_1000_iter">>, Bytecode).

%% @doc Benchmark 4: Deep nesting (100 levels)
-spec bench_deep_nesting() -> bench_result().
bench_deep_nesting() ->
    Bytecode = generate_deep_nesting(100),
    run_benchmark(<<"deep_nesting_100_levels">>, Bytecode).

%% @doc Benchmark 5: Multiple instances (100 fixed) - DEFERRED
-spec bench_mi_instances() -> bench_result().
bench_mi_instances() ->
    %% MI_SPAWN opcode not implemented in wf_exec.erl (item 009)
    #bench_result{
        name = <<"mi_instances_100_fixed">>,
        steps = 0,
        wall_time_us = 0,
        steps_per_sec = 0.0,
        memory_words = 0
    }.

%% @doc Benchmark 6: State store (10,000 mutations) - DEFERRED
-spec bench_state_store() -> bench_result().
bench_state_store() ->
    %% wf_state module not implemented (item 006)
    #bench_result{
        name = <<"state_store_10000_mutations">>,
        steps = 0,
        wall_time_us = 0,
        steps_per_sec = 0.0,
        memory_words = 0
    }.
```

##### 5. Implement run_all/0 entry point

**File**: `src/wf_bench.erl`
**Changes**: Add `run_all/0` that executes all benchmarks and prints table

```erlang
%% @doc Run all benchmarks and print formatted table
-spec run_all() -> ok.
run_all() ->
    Results = [
        bench_seq_throughput(),
        bench_par_fork_join(),
        bench_discriminator_repeat(),
        bench_deep_nesting(),
        bench_mi_instances(),
        bench_state_store()
    ],
    format_result_table(Results),
    ok.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc -I src -o ebin src/wf_bench.erl`
- [ ] No warnings from compiler
- [ ] `wf_bench:run_all()` executes without crashing
- [ ] Table header prints with correct column alignment
- [ ] All 6 benchmark entries appear in output

##### Manual Verification:

- [ ] Table output matches format specification (98-character divider line, correct column widths)
- [ ] Skipped benchmarks (#5, #6) show "SKIPPED" in Steps column
- [ ] Output is readable and properly aligned

**Note**: Complete automated verification, then proceed to Phase 2 (bytecode generation).

---

### Phase 2: Bytecode Generation for Benchmarks 1-4

#### Overview

Implement bytecode generation functions adapted from existing test patterns. This phase focuses on generating the bytecode for benchmarks 1-4 (all active benchmarks, excluding deferred MI and state store).

#### Changes Required:

##### 1. Implement sequence generator (benchmark #1)

**File**: `src/wf_bench.erl`
**Changes**: Add `generate_seq_n_tasks/1` adapted from `wf_test_seq.erl:179-185`

```erlang
%% @doc Generate sequence with N tasks (adapted from wf_test_seq:generate_seq_n_tasks/1)
%% Pattern: SEQ_ENTER, (TASK_EXEC, SEQ_NEXT)*N-1, TASK_EXEC, DONE
-spec generate_seq_n_tasks(pos_integer()) -> wf_vm:wf_bc().
generate_seq_n_tasks(N) ->
    %% Generate task opcodes: [TASK_EXEC task_1, SEQ_NEXT 3, TASK_EXEC task_2, SEQ_NEXT 5, ...]
    TaskOpcodes = lists:flatten([[
        {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
        {'SEQ_NEXT', 2*I + 1}
    ] || I <- lists:seq(1, N-1)]) ++ [{'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(N))}],
    %% Prepend SEQ_ENTER, append DONE
    [{'SEQ_ENTER', 0} | TaskOpcodes] ++ [{'DONE'}].
```

**Expected step count for N=10000**: 20,000 steps (1 SEQ_ENTER + 10,000×(1 TASK_EXEC + 1 SEQ_NEXT) - 1 SEQ_NEXT + 1 DONE = 20,000)

##### 2. Implement parallel fork+join generator (benchmark #2)

**File**: `src/wf_bench.erl`
**Changes**: Add `generate_par_n_branches/1` adapted from `wf_test_par.erl:208-218`

```erlang
%% @doc Generate parallel with N branches (adapted from wf_test_par:mock_bytecode_par_n_branches/1)
%% Pattern: PAR_FORK [1,3,5,...,2N-1], (TASK_EXEC, DONE)×N, JOIN_WAIT all
-spec generate_par_n_branches(pos_integer()) -> wf_vm:wf_bc().
generate_par_n_branches(N) ->
    %% Target IPs: [1, 3, 5, ..., 2N-1]
    BranchTargets = lists:seq(1, 2*N - 1, 2),

    %% Generate branch bytecode: each branch is TASK_EXEC, DONE
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
        {'DONE'}
    ] || I <- lists:seq(1, N)]),

    %% Assemble: PAR_FORK, branches..., JOIN_WAIT
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', all}].
```

**Expected step count for N=100**: 202 steps (1 PAR_FORK + 100×(1 TASK_EXEC + 1 DONE) + 1 JOIN_WAIT = 202)

##### 3. Implement discriminator repeat generator (benchmark #3)

**File**: `src/wf_bench.erl`
**Changes**: Add `generate_discriminator_repeat/1` - manually unroll loop with 5-branch first_complete pattern

```erlang
%% @doc Generate repeated discriminator pattern (N iterations)
%% Pattern: (PAR_FORK [5 branches], (TASK_EXEC, DONE)×5, JOIN_WAIT first_complete)×N
%% Manually unrolled (no loop support in bytecode for benchmarking)
-spec generate_discriminator_repeat(pos_integer()) -> wf_vm:wf_bc().
generate_discriminator_repeat(N) ->
    %% Generate N iterations of discriminator
    IterationBytecode = lists:flatten([generate_discriminator_iteration(I) || I <- lists:seq(1, N)]),
    %% Add DONE at end
    IterationBytecode ++ [{'DONE'}].

%% @private Generate single discriminator iteration
generate_discriminator_iteration(IterNum) ->
    NumBranches = 5,
    %% Target IPs: starting at IterNum * 10 offset to avoid conflicts
    BaseIP = IterNum * 10,
    BranchTargets = [BaseIP + 1, BaseIP + 3, BaseIP + 5, BaseIP + 7, BaseIP + 9],

    %% Generate branch bytecode
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom(["disc_", integer_to_list(IterNum), "_task_", integer_to_list(I)])},
        {'DONE'}
    ] || I <- lists:seq(1, NumBranches)]),

    %% Assemble iteration
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', first_complete}].
```

**Expected step count for N=1000**: ~5,000 steps (1 PAR_FORK + 5×(1 TASK_EXEC + 1 DONE) + 1 JOIN_WAIT = 8 steps/iteration, but first_complete cancels remaining 4, so ~5 steps/iteration = 5,000)

##### 4. Implement deep nesting generator (benchmark #4)

**File**: `src/wf_bench.erl`
**Changes**: Add `generate_deep_nesting/1` - recursive generation of nested seq(par(seq(task, task), seq(task, task)))

```erlang
%% @doc Generate deeply nested structure: seq(par(seq(task, task), seq(task, task)), ...)
%% N levels of nesting, but flat bytecode execution
-spec generate_deep_nesting(pos_integer()) -> wf_vm:wf_bc().
generate_deep_nesting(N) ->
    %% Generate nested structure recursively
    generate_nested_levels(N, 0).

%% @private Generate N levels of nesting
%% Each level: SEQ_ENTER, PAR_FORK [2 targets], SEQ_ENTER, TASK_EXEC, SEQ_NEXT, TASK_EXEC, DONE, SEQ_ENTER, TASK_EXEC, SEQ_NEXT, TASK_EXEC, DONE, JOIN_WAIT all, SEQ_NEXT
%% After last level: DONE
generate_nested_levels(0, _BaseIP) ->
    [{'DONE'}];
generate_nested_levels(N, BaseIP) ->
    %% Current level structure
    LevelBytecode = [
        {'SEQ_ENTER', 0},
        {'PAR_FORK', [BaseIP + 2, BaseIP + 7]},  % Fork to 2 branches
        %% Branch 1: seq(task, task)
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', list_to_atom(["nest_", integer_to_list(N), "_b1_t1"])},
        {'SEQ_NEXT', BaseIP + 5},
        {'TASK_EXEC', list_to_atom(["nest_", integer_to_list(N), "_b1_t2"])},
        {'DONE'},
        %% Branch 2: seq(task, task)
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', list_to_atom(["nest_", integer_to_list(N), "_b2_t1"])},
        {'SEQ_NEXT', BaseIP + 11},
        {'TASK_EXEC', list_to_atom(["nest_", integer_to_list(N), "_b2_t2"])},
        {'DONE'},
        {'JOIN_WAIT', all},
        {'SEQ_NEXT', BaseIP + 14}  % Jump to next level
    ],
    %% Recursively generate next level
    LevelBytecode ++ generate_nested_levels(N - 1, BaseIP + 14).
```

**Expected step count for N=100**: ~400 steps (1 SEQ_ENTER + 1 PAR_FORK + 2×(1 SEQ_ENTER + 2×TASK_EXEC + 1 SEQ_NEXT + 1 DONE) + 1 JOIN_WAIT + 1 SEQ_NEXT = 14 steps/level × 100 levels - last SEQ_NEXT + 1 DONE ≈ 1,400 steps)

**Note**: Actual step count will be measured and reported; the key invariant is that per-step cost does not increase with nesting depth.

#### Success Criteria:

##### Automated Verification:

- [ ] Bytecode generation functions compile without errors
- [ ] `wf_bench:bench_seq_throughput()` executes and returns result with steps > 0
- [ ] `wf_bench:bench_par_fork_join()` executes and returns result with steps > 0
- [ ] `wf_bench:bench_discriminator_repeat()` executes and returns result with steps > 0
- [ ] `wf_bench:bench_deep_nesting()` executes and returns result with steps > 0
- [ ] All benchmarks show steps_per_sec > 0

##### Manual Verification:

- [ ] Step counts match expected values (within ±10%)
- [ ] Benchmark #1 shows ~20,000 steps for 10,000 tasks
- [ ] Benchmark #2 shows ~202 steps for 100 branches
- [ ] Benchmark #3 shows ~5,000 steps for 1,000 iterations
- [ ] Benchmark #4 shows ~1,400 steps for 100 levels
- [ ] Table prints all 4 benchmarks with actual data

**Note**: Complete automated and manual verification, then proceed to Phase 3 (validation and documentation).

---

### Phase 3: Validation and Documentation

#### Overview

Validate that benchmarks demonstrate O(1) per-step overhead, add inline documentation, and verify integration with existing codebase.

#### Changes Required:

##### 1. Add inline documentation

**File**: `src/wf_bench.erl`
**Changes**: Add module doc and function specifications

```erlang
%%%-------------------------------------------------------------------
%%% @doc wf_bench: Performance benchmarks for wf-substrate executor
%%%
%%% This module implements microbenchmarks measuring executor performance
%%% characteristics. Each benchmark executes a specific bytecode pattern
%%% and reports:
%%% - steps: Number of opcodes executed
%%% - wall_time_us: Wall-clock time in microseconds
%%% - steps_per_sec: Throughput (steps per second)
%%% - memory_words: Memory delta in words (after - before)
%%%
%%% Key invariant demonstrated: Per-step overhead is O(1) and does not
%%% scale with AST complexity (flat bytecode execution).
%%%
%%% Usage:
%%%   wf_bench:run_all().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_bench).

%% @doc Run sequence throughput benchmark (10,000 tasks)
%% Demonstrates O(1) per-step cost for sequential execution.
-spec bench_seq_throughput() -> bench_result().

%% @doc Run parallel fork+join benchmark (100 branches)
%% Measures fork overhead, join overhead, and total time.
-spec bench_par_fork_join() -> bench_result().

%% @doc Run repeated discriminator benchmark (1,000 iterations)
%% Measures cancellation latency per iteration (first_complete join).
-spec bench_discriminator_repeat() -> bench_result().

%% @doc Run deep nesting benchmark (100 levels)
%% Verifies that per-step cost does not scale with AST depth
%% (flat bytecode vs recursive AST dispatch).
-spec bench_deep_nesting() -> bench_result().

%% @doc Run multiple instances benchmark (100 fixed) - DEFERRED
%% Returns skipped result (MI_SPAWN opcode not implemented).
-spec bench_mi_instances() -> bench_result().

%% @doc Run state store benchmark (10,000 mutations) - DEFERRED
%% Returns skipped result (wf_state module not implemented).
-spec bench_state_store() -> bench_result().

%% @doc Run all benchmarks and print formatted table
-spec run_all() -> ok.
```

##### 2. Add validation comments to key benchmarks

**File**: `src/wf_bench.erl`
**Changes**: Add comments explaining what each benchmark validates

```erlang
%% @doc Benchmark 1: Sequence throughput (10,000 tasks)
%%
%% Validated invariant: O(1) per-step cost for sequential execution.
%%
%% Expected: steps ≈ 20,000 (2 steps per task: TASK_EXEC + SEQ_NEXT)
%% Target: steps_per_sec should be high and consistent regardless of sequence length
%%
-spec bench_seq_throughput() -> bench_result().
bench_seq_throughput() ->
    Bytecode = generate_seq_n_tasks(10000),
    run_benchmark(<<"seq_throughput_10000_tasks">>, Bytecode).

%% @doc Benchmark 4: Deep nesting (100 levels)
%%
%% Validated invariant: Per-step cost does NOT scale with AST depth.
%%
%% This benchmark generates 100 levels of nested seq(par(seq(task, task), seq(task, task))).
%% The resulting AST is 100 levels deep, but the bytecode is flat (list of opcodes).
%% Per-step cost should be identical to shallow equivalent with same number of steps.
%%
%% Expected: steps ≈ 1,400 (14 steps per level × 100 levels)
%% Validation: Compare steps_per_sec to benchmark #1 - should be similar
%%
-spec bench_deep_nesting() -> bench_result().
bench_deep_nesting() ->
    Bytecode = generate_deep_nesting(100),
    run_benchmark(<<"deep_nesting_100_levels">>, Bytecode).
```

##### 3. Add TODO comments for deferred benchmarks

**File**: `src/wf_bench.erl`
**Changes**: Add clear TODO comments pointing to items 009 and 006

```erlang
%% @doc Benchmark 5: Multiple instances (100 fixed instances)
%%
%% DEFERRED until item 009 (multiple-instance-support) is complete.
%%
%% Requires:
%% - wf_mi.erl module implementation
%% - MI_SPAWN opcode handler in wf_exec:execute_opcode/2 (lines 186-208)
%% - wf_mi.hrl header file with mi_instance, mi_config records
%%
%% Current executor (wf_exec.erl) has no MI_SPAWN case in opcode dispatch.
%%
-spec bench_mi_instances() -> bench_result().
bench_mi_instances() ->
    %% TODO: Implement when item 009 is complete
    %% Bytecode should be: [{'MI_SPAWN', {{fixed, 100}, wait_all, 10}}, {'TASK_EXEC', mi_task}, {'DONE'}, {'JOIN_WAIT', wait_all}]
    #bench_result{
        name = <<"mi_instances_100_fixed">>,
        steps = 0,
        wall_time_us = 0,
        steps_per_sec = 0.0,
        memory_words = 0
    }.

%% @doc Benchmark 6: State store (10,000 mutations with periodic commits)
%%
%% DEFERRED until item 006 (state-store-and-atomic-commit) and item 010 (effect-boundary-and-receipts) are complete.
%%
%% Requires:
%% - wf_state.erl module with transaction API
%% - Effect boundary opcodes in executor
%% - Receipt generation for state mutations
%%
%% Alternative (if needed): Benchmark context mutations using wf_exec:set_ctx/2,
%% but this is not transactional state.
%%
-spec bench_state_store() -> bench_result().
bench_state_store() ->
    %% TODO: Implement when items 006 and 010 are complete
    %% Bytecode should include state mutations: {'STATE_MUT', key, value}, {'STATE_COMMIT', true}
    #bench_result{
        name = <<"state_store_10000_mutations">>,
        steps = 0,
        wall_time_us = 0,
        steps_per_sec = 0.0,
        memory_words = 0
    }.
```

##### 4. Add example usage to module doc

**File**: `src/wf_bench.erl`
**Changes**: Add example usage in module documentation

```erlang
%%%-------------------------------------------------------------------
%%% @doc wf_bench: Performance benchmarks for wf-substrate executor
%%% [existing doc...]
%%%
%%% Example output:
%%% ```
%%% | Benchmark                      |      Steps |   Time (us) |       Steps/sec | Memory (words) |
%%% |--------------------------------+------------+------------+-----------------+----------------|
%%% | seq_throughput_10000_tasks     |      20000 |      12345 |       1620452.1 |          12345 |
%%% | par_fork_join_100_branches     |        202 |       1234 |      16370016.5 |           1234 |
%%% | discriminator_repeat_1000_iter |       5000 |       5678 |        880593.1 |           5678 |
%%% | deep_nesting_100_levels        |       1400 |        456 |       3070175.4 |            456 |
%%% | mi_instances_100_fixed         |          0 |          0 |             0.0 |              0 | * SKIPPED
%%% | state_store_10000_mutations    |          0 |          0 |             0.0 |              0 | * SKIPPED
%%% ```
%%%
%%% Interpreting results:
%%% - steps_per_sec should be consistently high across all benchmarks
%%% - Similar steps_per_sec in benchmarks #1 and #4 demonstrates flat bytecode advantage
%%% - Lower steps_per_sec in benchmark #3 is expected (cancellation overhead per iteration)
%%%
%%% @end
%%%-------------------------------------------------------------------
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles with documentation: `erlc -I src -o ebin src/wf_bench.erl`
- [ ] Documentation generates without warnings: `erl -noshell -eval "edoc:application(wf_substrate, [{dir, \"src\"}]), init:stop()."`
- [ ] All benchmarks execute successfully with `wf_bench:run_all()`
- [ ] No compiler warnings (warnings_as_errors enabled in rebar.config)

##### Manual Verification:

- [ ] All active benchmarks (#1-#4) show steps_per_sec > 100,000 (baseline threshold)
- [ ] Benchmark #4 (deep nesting) steps_per_sec is within same order of magnitude as benchmark #1 (demonstrates O(1) per-step cost)
- [ ] Deferred benchmarks (#5, #6) show "SKIPPED" in Steps column with 0 values
- [ ] Table output is readable and properly formatted
- [ ] Module documentation is clear and comprehensive

**Note**: This is the final phase. After completion, the benchmark suite is ready for use and can be integrated into CI/CD if desired.

---

## Testing Strategy

### Unit Tests:

Not applicable - benchmarks are measurement tools, not unit-tested code. The benchmarks themselves serve as integration tests for the executor.

### Integration Tests:

The `run_all/0` function serves as an integration test that:
- Executes all benchmarks end-to-end
- Verifies executor can handle large bytecode (10,000 tasks, 1,000 iterations)
- Demonstrates O(1) per-step cost invariant
- Validates executor step counting accuracy

### Manual Testing Steps:

1. **Compile the module:**
   ```bash
   erlc -I src -o ebin src/wf_bench.erl
   ```

2. **Run all benchmarks:**
   ```erlang
   erl -pa ebin -eval "wf_bench:run_all(), init:stop()."
   ```

3. **Verify output:**
   - All 4 active benchmarks complete successfully
   - Steps/sec values are reasonable (> 100,000)
   - No crashes or errors
   - Table is properly formatted

4. **Validate O(1) invariant:**
   - Compare steps/sec from benchmark #1 (seq) and benchmark #4 (deep nesting)
   - Should be within same order of magnitude (demonstrates flat bytecode advantage)

5. **Test individual benchmarks:**
   ```erlang
   erl -pa ebin -eval "R = wf_bench:bench_seq_throughput(), io:format(\"~p~n\", [R]), init:stop()."
   ```

## Migration Notes

No migration required - this is a new module with no external dependencies or data compatibility concerns.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/016-performance-benchmarks/research.md`
- Executor core: `/Users/speed/wf-substrate/src/wf_exec.erl:78-165` (new/1, step/2, run/3, get_step_count/1)
- Executor opcodes: `/Users/speed/wf-substrate/src/wf_exec.erl:186-596` (execute_opcode/2 and handlers)
- Bytecode types: `/Users/speed/wf-substrate/src/wf_vm.erl:9-40` (wf_bc(), opcode(), join_policy())
- Test patterns: `/Users/speed/wf-substrate/test/wf_test_seq.erl:179-185` (generate_seq_n_tasks/1)
- Test patterns: `/Users/speed/wf-substrate/test/wf_test_par.erl:208-218` (mock_bytecode_par_n_branches/1)
- Test patterns: `/Users/speed/wf-substrate/test/wf_test_par.erl:66-76` (mock_bytecode_par_first_complete/1)
