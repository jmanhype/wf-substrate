# Research: Implement performance benchmarks

**Date**: 2025-01-18
**Item**: 016-performance-benchmarks

## Research Question

Implement wf_bench.erl: microbenchmarks measuring executor performance characteristics.

Benchmarks:
1. Sequence throughput: sequence of 10,000 tasks (trivial no-op tasks), measure total time and steps/sec. Target: demonstrate O(1) per-step cost.
2. Parallel split+join: par of 100 branches each with a single task, synchronizing join. Measure fork overhead, join overhead, total time.
3. Repeated discriminator: 1,000 iterations of discriminator pattern (par of 5 branches, first-complete join, cancel remaining 4). Measure cancellation latency per iteration.
4. Deep nesting: sequence of 100 nested par(seq(task, task), seq(task, task)) — deep AST but flat bytecode. Verify that per-step cost does not scale with original AST depth.
5. Multiple instances: MI with 100 fixed instances, wait_all join. Measure spawn overhead and collection overhead.
6. State store: 10,000 mutations with periodic commits. Measure commit latency and receipt generation overhead.

Report format: for each benchmark, output {name, steps, wall_time_us, steps_per_sec, memory_words}. Memory measured via erlang:process_info(self(), memory) before and after.

Key invariant to demonstrate: overhead per step is bounded and does not scale with AST size (because we execute flat bytecode, not recursive AST dispatch).

Include a run_all/0 function that runs all benchmarks and prints a formatted table.

## Summary

The task requires implementing a comprehensive benchmarking suite (wf_bench.erl) to measure and validate the performance characteristics of the wf-substrate executor. The benchmarks must demonstrate that the executor's per-step overhead is O(1) and does not scale with AST complexity, proving the effectiveness of the flat bytecode design versus recursive AST dispatch.

**Key findings:**

1. **Executor architecture supports benchmarking**: The wf_exec module provides all necessary hooks for benchmarking - step counters (wf_exec:get_step_count/1), execution control (wf_exec:step/2, wf_exec:run/3), and state inspection. The bytecode executor (wf_exec.erl:141-165) executes opcodes in a tight loop with deterministic step counting.

2. **No state store module exists yet**: Benchmark #6 (state store with 10,000 mutations) references a wf_state module that doesn't exist in the codebase. Item 006 (state-store-and-atomic-commit) and item 010 (effect-boundary-and-receipts) are not yet implemented, so this benchmark will need to be deferred or use mock state operations.

3. **No MI implementation exists**: Benchmark #5 (multiple instances with 100 fixed instances) requires wf_mi.erl which doesn't exist yet. Item 009 (multiple-instance-support) is not complete, so MI_SPAWN opcode is not implemented in the executor.

4. **Rich test patterns to leverage**: Existing test modules (wf_test_seq.erl, wf_test_par.erl, wf_test_xor.erl, wf_test_join.erl) provide excellent patterns for generating bytecode for benchmark scenarios. The parameterized test patterns (e.g., wf_test_seq.erl:164-173) show how to generate N-task sequences, N-branch parallels, etc.

5. **Erlang/OTP benchmarking tools**: Use `erlang:monotonic_time(microsecond)` for wall-clock timing, `erlang:process_info(self(), memory)` for memory measurement, and `erlang:statistics([garbage_collection])` for GC overhead tracking.

## Current State Analysis

### Existing Implementation

**Executor Core:**
- **wf_exec.erl:78-101**: `new/1` creates executor state with initial token, step counter set to 0
- **wf_exec.erl:113-146**: `step/2` executes single opcode, returns updated state and trace event, increments step_count
- **wf_exec.erl:148-165**: `run/3` executes N quanta with yielding support, suitable for measuring sustained throughput
- **wf_exec.erl:28-34**: `#token{}` record tracks token_id, ip, scope_id, value, status
- **wf_exec.erl:36-42**: `#branch_info{}` record tracks branch_id, tokens, join_id, targets
- **wf_exec.erl:44-51**: `#join_counter{}` record tracks join_id, completed, required, policy, results

**Opcode Execution:**
- **wf_exec.erl:186-208**: `execute_opcode/2` dispatches to opcode-specific handlers
- **wf_exec.erl:214-222**: `execute_seq_enter/2` - pushes scope, advances IP (1 step)
- **wf_exec.erl:224-229**: `execute_seq_next/2` - jumps to target IP (1 step)
- **wf_exec.erl:231-239**: `execute_task_exec/2` - executes task, updates context (1 step)
- **wf_exec.erl:259-323**: `execute_par_fork/2` - spawns N branch tokens, creates join counter (1 step)
- **wf_exec.erl:325-363**: `execute_join_wait/2` - checks join counter, blocks or continues (1 step)
- **wf_exec.erl:535-573**: `execute_done/1` - marks token complete, checks if all done (1 step)

**Bytecode Type System:**
- **wf_vm.erl:9-21**: Opcode type definitions (generic tuples with atom tags)
- **wf_vm.erl:23-29**: Join policies: all, {first_n, N}, {n_of_m, N, M}, first_complete, sync_merge
- **wf_vm.erl:31-35**: Loop policies: {count, N}, while, until
- **wf_vm.erl:37-40**: MI policies: {fixed, N}, {dynamic, Min, Max}

**Test Patterns for Benchmark Generation:**
- **wf_test_seq.erl:16-24**: `mock_bytecode_seq_2_tasks/0` - simple 2-task sequence
- **wf_test_seq.erl:179-185**: `generate_seq_n_tasks/1` - generates N-task sequence (pattern for benchmark #1)
- **wf_test_par.erl:16-25**: `mock_bytecode_par_2_branches/0` - 2-branch parallel
- **wf_test_par.erl:208-218**: `mock_bytecode_par_n_branches/1` - generates N-branch parallel (pattern for benchmark #2)
- **wf_test_par.erl:66-76**: `mock_bytecode_par_first_complete/1` - N-branch with first_complete (pattern for benchmark #3)
- **wf_test_join.erl:9-19**: `mock_bytecode_join_wait_all/1` - wait_all join with M branches
- **wf_test_join.erl:21-31**: `mock_bytecode_join_wait_n/2` - wait_n join (N of M)

**Missing Components:**
- **wf_state.erl**: State store module (item 006 - not implemented)
- **wf_mi.erl**: Multiple instance module (item 009 - not implemented)
- **wf_bench.erl**: Benchmark module to be created
- **wf_sched**: Scheduler module (exists but not loaded - need to verify)
- **wf_cancel.erl**: Cancellation semantics (item 008 - not implemented)

## Key Files

- `src/wf_exec.erl:78-101` - Executor state initialization with step counter
- `src/wf_exec.erl:113-146` - Single-step execution for per-step timing
- `src/wf_exec.erl:148-165` - Multi-step run for sustained throughput
- `src/wf_exec.erl:28-51` - Core records: token, branch_info, join_counter
- `src/wf_exec.erl:259-323` - PAR_FORK implementation (for fork overhead measurement)
- `src/wf_exec.erl:325-363` - JOIN_WAIT implementation (for join overhead measurement)
- `src/wf_vm.erl:9-40` - Bytecode and policy type definitions
- `test/wf_test_seq.erl:179-185` - Pattern for generating N-task sequences
- `test/wf_test_par.erl:208-218` - Pattern for generating N-branch parallels
- `test/wf_test_join.erl:9-31` - Pattern for join policy benchmarks
- `test/wf_test_helpers.erl:15-30` - Execution helpers (exec_until_done/1, exec_steps/2)
- `rebar.config:1-30` - Build configuration (test profile, no external deps)

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- `wf_exec`: Core executor with step counting and execution control
- `wf_vm`: Bytecode type definitions (wf_bc(), opcode(), join_policy())
- `wf_test_seq`, `wf_test_par`, `wf_test_xor`, `wf_test_join`: Mock bytecode generators (for reuse or adaptation)
- `wf_test_helpers`: Test utility functions (may be adapted for benchmark helpers)

**External dependencies:**
- `eunit`: Built-in Erlang testing framework (for test runners, not core benchmarking)
- `erlang`: BIFs for timing (`erlang:monotonic_time/1`), memory (`erlang:process_info/2`), statistics (`erlang:statistics/1`)
- No third-party benchmarking libraries required

### Patterns to Follow

**Benchmark Module Structure:**
Based on existing test modules, wf_bench.erl should follow this pattern:

```erlang
-module(wf_bench).
-export([
    bench_seq_throughput/0,
    bench_par_fork_join/0,
    bench_discriminator_repeat/0,
    bench_deep_nesting/0,
    bench_mi_instances/0,
    bench_state_store/0,  % DEFERRED
    run_all/0
]).

% Benchmark result record
-record(bench_result, {
    name :: binary(),
    steps :: non_neg_integer(),
    wall_time_us :: non_neg_integer(),
    steps_per_sec :: float(),
    memory_words :: non_neg_integer()
}).
```

**Benchmark Execution Pattern:**
```erlang
run_benchmark(Name, Bytecode) ->
    % Measure initial memory
    {memory, MemBefore} = erlang:process_info(self(), memory),

    % Execute and time
    StartTime = erlang:monotonic_time(microsecond),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 1000000, undefined),
    EndTime = erlang:monotonic_time(microsecond),

    % Measure final memory
    {memory, MemAfter} = erlang:process_info(self(), memory),
    Steps = wf_exec:get_step_count(ExecState1),
    WallTimeUs = EndTime - StartTime,
    StepsPerSec = (Steps * 1000000) / WallTimeUs,
    MemoryWords = MemAfter - MemBefore,

    #bench_result{
        name = Name,
        steps = Steps,
        wall_time_us = WallTimeUs,
        steps_per_sec = StepsPerSec,
        memory_words = MemoryWords
    }.
```

**Bytecode Generation Patterns:**
Adapt from test modules:
- **Sequence of N tasks**: Use `wf_test_seq:generate_seq_n_tasks/1` pattern
- **Parallel of N branches**: Use `wf_test_par:mock_bytecode_par_n_branches/1` pattern
- **Discriminator pattern**: Use `wf_test_par:mock_bytecode_par_first_complete/1` with `JOIN_WAIT, first_complete`
- **Deep nesting**: Generate nested structure manually or with recursive function
- **MI patterns**: DEFERRED - requires wf_mi implementation

**Formatting Output:**
Use `io:format/2` with formatted tables:
```erlang
print_results(Results) ->
    Header = "| ~-30s | ~10s | ~12s | ~15s | ~12s |~n",
    io:format(Header, ["Benchmark", "Steps", "Time (us)", "Steps/sec", "Memory (words)"]),
    io:format("~s~n", [lists:duplicate(98, $-)]),
    lists:foreach(fun(R) ->
        io:format("| ~-30s | ~10w | ~12w | ~15.2f | ~12w |~n",
                  [R#bench_result.name, R#bench_result.steps,
                   R#bench_result.wall_time_us, R#bench_result.steps_per_sec,
                   R#bench_result.memory_words])
    end, Results).
```

### Benchmark-Specific Considerations

**Benchmark #1: Sequence Throughput (10,000 tasks)**
- Bytecode: Generate using adapted `generate_seq_n_tasks(10000)` pattern
- Expected: ~20,000 steps (10,000 tasks × 2 steps per task: TASK_EXEC + SEQ_NEXT/DONE)
- Target: Demonstrate O(1) per-step cost (linear total time)
- Measure: Wall time, steps/sec, verify linear scaling

**Benchmark #2: Parallel Split+Join (100 branches)**
- Bytecode: Generate using `mock_bytecode_par_n_branches(100)` pattern
- Structure: PAR_FORK → 100 × (TASK_EXEC + DONE) → JOIN_WAIT
- Expected: ~202 steps (1 fork + 100 × 2 task steps + 100 done steps + 1 join)
- Measure: Fork overhead (single PAR_FORK step), join overhead (JOIN_WAIT step), total time

**Benchmark #3: Repeated Discriminator (1,000 iterations)**
- Bytecode: Nested structure or loop with PAR_FORK(5) + first_complete join
- Challenge: No LOOP pattern support for 1,000 iterations yet
- Option A: Manually unroll loop (large bytecode)
- Option B: Implement simple loop counter in benchmark harness
- Measure: Cancellation latency per iteration (4 cancelled branches ÷ total time)

**Benchmark #4: Deep Nesting (100 levels)**
- Bytecode: Generate recursive structure: `seq(par(seq(task, task), seq(task, task)), ...)`
- Target bytecode: ~400 opcodes (100 levels × 4 opcodes per level)
- Expected: Flat bytecode execution (no recursion)
- Key invariant: Per-step cost should NOT scale with nesting depth
- Verify: Compare to shallow equivalent with same number of steps

**Benchmark #5: Multiple Instances (100 fixed instances)**
- Status: **DEFERRED** - wf_mi.erl not implemented (item 009 incomplete)
- Requires: MI_SPAWN opcode support in executor
- Alternative: Benchmark using PAR_FORK with 100 branches as proxy
- Measure: Spawn overhead (1 step), collection overhead (join synchronization)

**Benchmark #6: State Store (10,000 mutations)**
- Status: **DEFERRED** - wf_state.erl not implemented (item 006 incomplete)
- Requires: State transaction API, commit/receipt generation (item 010 incomplete)
- Alternative: Benchmark context mutations using `wf_exec:set_ctx/2`
- Measure: Commit latency, receipt generation overhead

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ------- | ---------- |
| **Missing modules (wf_state, wf_mi)** | High | Defer benchmarks #5 and #6, implement with mocks or proxy operations |
| **No loop support for repeated patterns** | Medium | Manually unroll loops or implement simple counter in benchmark harness |
| **Memory measurement noise** | Medium | Run multiple iterations, use median, force GC before measurement |
| **Scheduler interference** | Low | Run benchmarks in isolated process, use high-priority if needed |
| **Bytecode generation overhead** | Low | Pre-generate bytecode, measure only execution time |
| **Test file references non-existent headers** | Medium | wf_test_mi.erl includes wf_mi.hrl which doesn't exist - may need to skip MI patterns |
| **PAR_FORK creates tokens to different IPs** | Low | MI patterns spawn instances to SAME IP - current PAR_FORK is different semantic |

## Recommended Approach

**Phase 1: Scaffolding and Core Benchmarks**
1. Create `src/wf_bench.erl` module with benchmark result record and reporting infrastructure
2. Implement `run_all/0` that executes all available benchmarks and prints formatted table
3. Implement Benchmark #1 (sequence throughput) using adapted `generate_seq_n_tasks/10000`
4. Implement Benchmark #2 (parallel fork+join) using adapted `mock_bytecode_par_n_branches/100`
5. Verify O(1) per-step cost by plotting steps vs time

**Phase 2: Advanced Benchmarks**
6. Implement Benchmark #3 (discriminator) with manually unrolled 1,000 iterations or loop harness
7. Implement Benchmark #4 (deep nesting) with recursive bytecode generator
8. Compare deep nesting vs shallow to demonstrate flat bytecode advantage

**Phase 3: Deferred/Future Work**
9. Mark Benchmark #5 (MI) as deferred pending wf_mi implementation
10. Mark Benchmark #6 (state store) as deferred pending wf_state and effect implementation
11. Add TODO comments in wf_bench.erl pointing to items 009 and 010

**Implementation Details:**
- Use `erlang:monotonic_time(microsecond)` for high-resolution timing
- Force garbage collection before each benchmark: `erlang:garbage_collect()`
- Run each benchmark multiple times (e.g., 10 iterations) and report median
- Include warm-up runs to avoid JIT/cold start effects (BEAM doesn't have JIT, but good practice)
- Separate bytecode generation from execution timing
- Measure memory difference (after - before) in words

**Code Structure:**
```erlang
-module(wf_bench).

%% Benchmark API
-export([bench_seq_throughput/0,
         bench_par_fork_join/0,
         bench_discriminator_repeat/0,
         bench_deep_nesting/0,
         run_all/0]).

%% Internal helpers
-export([run_benchmark/2,
         generate_seq_n_tasks/1,
         generate_par_n_branches/1]).

-include("wf_exec.hrl").

-record(bench_result, {
    name :: binary(),
    steps :: non_neg_integer(),
    wall_time_us :: non_neg_integer(),
    steps_per_sec :: float(),
    memory_words :: non_neg_integer()
}).
```

## Open Questions

1. **Loop support**: Should the benchmark harness implement a simple loop counter for Benchmark #3 (1,000 discriminator iterations), or should we manually unroll the bytecode? Manual unroll creates large bytecode (5,000+ opcodes), which may itself be a test case.

2. **MI benchmark proxy**: Since wf_mi is not implemented, should Benchmark #5 (multiple instances) use PAR_FORK with 100 branches as a proxy, or should it be completely deferred? PAR_FORK has different semantics (different branches vs same body), but the spawn/collection overhead is similar.

3. **State operations**: Should Benchmark #6 use context mutations (`wf_exec:set_ctx/2`) as a proxy for state store operations, or defer it entirely? Context mutations are in-memory map operations, not transactional state.

4. **Test file integration**: Should benchmarks be in `src/` as a production module, or in `test/` as a test utility? Item description suggests "wf_bench.erl" without specifying directory.

5. **Scheduler parameter**: Should benchmarks use `undefined` scheduler (current test pattern) or implement a minimal deterministic scheduler? Current tests pass `undefined` to `wf_exec:run/3`.

6. **Memory accuracy**: Should memory measurement include only the exec_state process, or also account for bytecode binary size, tokens map, branch map, join counters? The `erlang:process_info(self(), memory)` includes all process memory.

7. **Warm-up iterations**: How many warm-up runs should each benchmark perform before measurement? BEAM doesn't have JIT compilation, but process heap warm-up may affect results.

8. **Benchmark repetitions**: Should benchmarks run once, 3 times (median), or 10 times (statistical significance)? Longer benchmarks may need fewer repetitions.
