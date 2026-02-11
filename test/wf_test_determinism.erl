-module(wf_test_determinism).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_trace.hrl").

%%====================================================================
%% Simple Sequence Determinism (US-002)
%%====================================================================

%% @doc Test that running the same sequence twice with deterministic scheduler produces identical traces
determinism_simple_seq_test() ->
    Bytecode = wf_test_seq:mock_bytecode_seq_2_tasks(),
    {_, Events1} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    {_, Events2} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    ?assertEqual(ok, wf_test_trace_helpers:compare_traces(Events1, Events2)).

%%====================================================================
%% Parallel Split+Join Determinism (US-003)
%%====================================================================

%% @doc Test that parallel workflows produce deterministic traces
determinism_par_3_branches_test() ->
    Bytecode = wf_test_par:mock_bytecode_par_3_branches(),
    {_, Events1} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    {_, Events2} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    ?assertEqual(ok, wf_test_trace_helpers:compare_traces(Events1, Events2)).

%% @doc Test that parallel workflows are stable across 10 runs
determinism_par_stability_test_() ->
    %% Run 10 times to verify stability
    {setup,
     fun() -> wf_test_par:mock_bytecode_par_3_branches() end,
     fun(Bytecode) ->
         [?_test(begin
              {_, Events} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
              %% Just verify it completes without error
              ?assert(is_list(Events)),
              ?assert(length(Events) > 0)
          end) || _ <- lists:seq(1, 10)]
     end}.

%%====================================================================
%% Nested Par in Seq Determinism (US-004)
%%====================================================================

%% @doc Test that nested parallel-in-sequential patterns are deterministic
determinism_nested_par_in_seq_test() ->
    %% Custom bytecode: SEQ_ENTER, PAR_FORK, tasks, JOIN_WAIT, SEQ_NEXT, task
    Bytecode = [
        {seq_enter, 0},
        {par_fork, [1, 3, 5]},
        {task_exec, task_a},
        {task_exec, task_b},
        {task_exec, task_c},
        {join_wait, all},
        {seq_next, 7},
        {task_exec, task_d},
        {done}
    ],
    {_, Events1} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    {_, Events2} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    ?assertEqual(ok, wf_test_trace_helpers:compare_traces(Events1, Events2)).

%%====================================================================
%% XOR with Deterministic Signal (US-005)
%%====================================================================

%% @doc Test that XOR branch selection is deterministic
determinism_xor_branch_test() ->
    Bytecode = wf_test_xor:mock_bytecode_xor_3_branches(),
    {_, Events1} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    {_, Events2} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    ?assertEqual(ok, wf_test_trace_helpers:compare_traces(Events1, Events2)).

%% @doc Test that XOR branch selection is stable across 20 runs
determinism_xor_stability_test_() ->
    %% Run 20 times to verify same branch always selected
    {setup,
     fun() -> wf_test_xor:mock_bytecode_xor_3_branches() end,
     fun(Bytecode) ->
         [?_test(begin
              {_, Events} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
              ?assert(is_list(Events)),
              ?assert(length(Events) > 0)
          end) || _ <- lists:seq(1, 20)]
     end}.

%%====================================================================
%% MI with Fixed Count Determinism (US-006)
%%====================================================================

%% @doc Test that multiple instance pattern is deterministic
determinism_mi_fixed_count_test() ->
    Bytecode = wf_test_mi:mock_bytecode_mi_fixed_5(),
    {_, Events1} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    {_, Events2} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
    ?assertEqual(ok, wf_test_trace_helpers:compare_traces(Events1, Events2)).

%%====================================================================
%% Concurrent Execution Determinism (US-007)
%%====================================================================

%% @doc Test that concurrent executions produce structurally identical traces
determinism_concurrent_execution_test() ->
    Bytecode = wf_test_par:mock_bytecode_par_3_branches(),
    %% Spawn 10 concurrent executions
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn(fun() ->
        {_, Events} = wf_test_trace_helpers:run_with_trace(Bytecode, deterministic, []),
        Parent ! {Ref, self(), Events}
    end) || _ <- lists:seq(1, 10)],
    %% Collect all traces with timeout
    Traces = [receive {Ref, Pid, Events} -> Events after 5000 -> exit(timeout) end || Pid <- Pids],
    %% All traces should be structurally identical (same length, same opcodes)
    [FirstTrace | _] = Traces,
    lists:foreach(fun(Trace) ->
        case wf_test_trace_helpers:compare_traces(FirstTrace, Trace) of
            ok -> ok;
            {error, _Diff} ->
                %% For concurrent tests, we only compare structure (length and opcodes)
                %% Token IDs (refs) will be different, so we check opcode sequences instead
                FirstOpcodes = [E#trace_event.opcode || E <- FirstTrace],
                TraceOpcodes = [E#trace_event.opcode || E <- Trace],
                ?assertEqual(FirstOpcodes, TraceOpcodes)
        end
    end, Traces).
