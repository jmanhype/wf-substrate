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
%%% Usage:
%%%   wf_bench:run_all().
%%%
%%% @end
%%%-------------------------------------------------------------------
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

%% Internal helpers
-export([
    run_benchmark/2,
    generate_seq_n_tasks/1,
    generate_par_n_branches/1,
    generate_discriminator_repeat/1,
    generate_deep_nesting/1,
    format_result_table/1
]).

%%====================================================================
%% Types
%%====================================================================

%% Benchmark result record
-record(bench_result, {
    name :: binary(),
    steps :: non_neg_integer(),
    wall_time_us :: non_neg_integer(),
    steps_per_sec :: float(),
    memory_words :: non_neg_integer()
}).

-type bench_result() :: #bench_result{}.

%%====================================================================
%% Benchmark API Functions
%%====================================================================

%% @doc Run sequence throughput benchmark (10,000 tasks)
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

%% @doc Run parallel fork+join benchmark (100 branches)
%%
%% Measures fork overhead, join overhead, and total time.
%%
%% Expected: steps ≈ 202 (1 PAR_FORK + 100×(TASK_EXEC + DONE) + 1 JOIN_WAIT)
%% Target: High steps/sec demonstrates efficient fork/join
%%
-spec bench_par_fork_join() -> bench_result().
bench_par_fork_join() ->
    Bytecode = generate_par_n_branches(100),
    run_benchmark(<<"par_fork_join_100_branches">>, Bytecode).

%% @doc Run repeated discriminator benchmark (1,000 iterations)
%%
%% Measures cancellation latency per iteration (first_complete join).
%%
%% Expected: steps ≈ 5,000 (5 steps/iteration × 1000)
%% Target: Cancellation overhead should be reasonable
%%
-spec bench_discriminator_repeat() -> bench_result().
bench_discriminator_repeat() ->
    Bytecode = generate_discriminator_repeat(1000),
    run_benchmark(<<"discriminator_repeat_1000_iter">>, Bytecode).

%% @doc Run deep nesting benchmark (100 levels)
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

%% @doc Run multiple instances benchmark (100 fixed instances)
%%
%% DEFERRED until item 009 (multiple-instance-support) is complete.
%%
%% Requires:
%%% - wf_mi.erl module implementation
%%% - MI_SPAWN opcode handler in wf_exec:execute_opcode/2 (lines 186-208)
%%% - wf_mi.hrl header file with mi_instance, mi_config records
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

%% @doc Run state store benchmark (10,000 mutations with periodic commits)
%%
%% DEFERRED until item 006 (state-store-and-atomic-commit) and item 010 (effect-boundary-and-receipts) are complete.
%%
%% Requires:
%%% - wf_state.erl module with transaction API
%%% - Effect boundary opcodes in executor
%%% - Receipt generation for state mutations
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

%%====================================================================
%% Internal Helper Functions
%%====================================================================

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

%% @doc Generate repeated discriminator pattern (N iterations)
%% Pattern: (PAR_FORK [5 branches], (TASK_EXEC, DONE)×5, JOIN_WAIT first_complete)×N, DONE
%% Manually unrolled (no loop support in bytecode for benchmarking)
-spec generate_discriminator_repeat(pos_integer()) -> wf_vm:wf_bc().
generate_discriminator_repeat(N) ->
    %% Generate N iterations of discriminator, adjusting IPs for each iteration
    %% Each iteration is 12 opcodes: PAR_FORK + 5×(TASK_EXEC + DONE) + JOIN_WAIT
    %% So iteration i starts at offset (i-1) * 12
    IterationBytecodes = [generate_discriminator_iteration_with_offset(I, (I-1) * 12) || I <- lists:seq(1, N)],

    %% Flatten and add DONE at end
    lists:flatten(IterationBytecodes) ++ [{'DONE'}].

%% @private Generate single discriminator iteration with IP offset
generate_discriminator_iteration_with_offset(_IterNum, Offset) ->
    NumBranches = 5,
    %% Target IPs relative to iteration start: 1, 3, 5, 7, 9
    %% Add offset to get absolute IPs
    BranchTargets = [1 + Offset, 3 + Offset, 5 + Offset, 7 + Offset, 9 + Offset],

    %% Generate branch bytecode
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom("disc_task_" ++ integer_to_list(I))},
        {'DONE'}
    ] || I <- lists:seq(1, NumBranches)]),

    %% Assemble iteration
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', first_complete}].

%% @doc Generate deeply nested structure: seq(par(seq(task, task), seq(task, task)))
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
        {'TASK_EXEC', list_to_atom("nest_" ++ integer_to_list(N) ++ "_b1_t1")},
        {'SEQ_NEXT', BaseIP + 5},
        {'TASK_EXEC', list_to_atom("nest_" ++ integer_to_list(N) ++ "_b1_t2")},
        {'DONE'},
        %% Branch 2: seq(task, task)
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', list_to_atom("nest_" ++ integer_to_list(N) ++ "_b2_t1")},
        {'SEQ_NEXT', BaseIP + 11},
        {'TASK_EXEC', list_to_atom("nest_" ++ integer_to_list(N) ++ "_b2_t2")},
        {'DONE'},
        {'JOIN_WAIT', all},
        {'SEQ_NEXT', BaseIP + 14}  % Jump to next level
    ],
    %% Recursively generate next level
    LevelBytecode ++ generate_nested_levels(N - 1, BaseIP + 14).
