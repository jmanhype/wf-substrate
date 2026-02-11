-module(wf_test_par).
-export([
    mock_bytecode_par_2_branches/0,
    mock_bytecode_par_3_branches/0,
    mock_bytecode_par_mixed_completion/0,
    mock_bytecode_par_wait_n/2,
    mock_bytecode_par_first_complete/1
]).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%%====================================================================
%% Mock Bytecode Generators for Parallel Patterns
%%====================================================================

%% 2-branch parallel
mock_bytecode_par_2_branches() ->
    [
        {'PAR_FORK', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'JOIN_WAIT', all}
    ].

%% 3-branch parallel
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

%% Mixed completion times (branch 1: 1 step, branch 2: seq with 2 steps)
mock_bytecode_par_mixed_completion() ->
    [
        {'PAR_FORK', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_b},
        {'SEQ_NEXT', 7},
        {'TASK_EXEC', task_c},
        {'DONE'},
        {'JOIN_WAIT', all}
    ].

%% N-branch parallel with wait_n join
mock_bytecode_par_wait_n(N, M) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
        {'DONE'}
    ] || I <- lists:seq(1, M)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', {wait_n, N}}].

%% N-branch parallel with first_complete join
mock_bytecode_par_first_complete(M) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
        {'DONE'}
    ] || I <- lists:seq(1, M)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', first_complete}].

%%====================================================================
%% Unit Tests: 2-Branch Parallel
%%====================================================================

par_2_branches_all_complete_test() ->
    Bytecode = mock_bytecode_par_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify both branches completed
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(3, maps:size(Tokens)), % root + 2 branch tokens
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(3, CompleteCount).

%%====================================================================
%% Unit Tests: 3-Branch Parallel
%%====================================================================

par_3_branches_all_complete_test() ->
    Bytecode = mock_bytecode_par_3_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(4, maps:size(Tokens)), % root + 3 branch tokens
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(4, CompleteCount).

%%====================================================================
%% Unit Tests: Mixed Completion Times
%%====================================================================

par_mixed_completion_test() ->
    Bytecode = mock_bytecode_par_mixed_completion(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Both branches should complete
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(3, maps:size(Tokens)),
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(3, CompleteCount).

%%====================================================================
%% Unit Tests: wait_n Join Policy
%%====================================================================

par_wait_n_2_of_3_test() ->
    Bytecode = mock_bytecode_par_wait_n(2, 3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% 2 completed, 1 cancelled
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(4, TotalTokens), % root + 3 branches
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(3, CompleteCount), % root + 2 completed
    ?assertEqual(1, CancelledCount).

%%====================================================================
%% Unit Tests: first_complete Join Policy
%%====================================================================

par_first_complete_test() ->
    Bytecode = mock_bytecode_par_first_complete(4),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Only 1 completed, rest cancelled
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(5, TotalTokens), % root + 4 branches
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(2, CompleteCount), % root + 1 completed
    ?assertEqual(3, CancelledCount).

%%====================================================================
%% Unit Tests: Join Counter Mechanics
%%====================================================================

par_join_counter_created_test() ->
    Bytecode = mock_bytecode_par_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until PAR_FORK
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    %% Verify join counter created
    JoinCounters = ExecState1#exec_state.join_counters,
    ?assert(maps:size(JoinCounters) > 0),
    [{_JoinId, Counter}] = maps:to_list(JoinCounters),
    ?assertEqual(0, Counter#join_counter.completed),
    ?assertEqual(2, Counter#join_counter.required).

par_join_counter_increments_test() ->
    Bytecode = mock_bytecode_par_3_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until all branches complete
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Verify join counter reached required
    JoinCounters = ExecState1#exec_state.join_counters,
    ?assert(maps:size(JoinCounters) > 0),
    [{_JoinId, Counter}] = maps:to_list(JoinCounters),
    ?assertEqual(3, Counter#join_counter.completed),
    ?assertEqual(3, Counter#join_counter.required).

%%====================================================================
%% Parameterized Tests: N-Branch Parallel
%%====================================================================

par_n_branches_test_() ->
    %% Test N = 2, 3, 5, 10 branches
    lists:map(fun(M) ->
        {io_lib:format("par_~p_branches", [M]), fun() ->
            Bytecode = mock_bytecode_par_n_branches(M),
            ExecState0 = wf_exec:new(Bytecode),
            {done, ExecState1} = wf_exec:run(ExecState0, 1000, undefined),
            ?assert(wf_exec:is_done(ExecState1)),
            %% Verify all M branches completed
            Tokens = ExecState1#exec_state.tokens,
            ?assertEqual(M + 1, maps:size(Tokens))
        end}
    end, [2, 3, 5, 10]).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Generate parallel with N branches
mock_bytecode_par_n_branches(N) ->
    BranchTargets = lists:seq(1, 2*N - 1, 2),
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
        {'DONE'}
    ] || I <- lists:seq(1, N)]),
    [
        {'PAR_FORK', BranchTargets}
    | BranchBytecode] ++
    [{'JOIN_WAIT', all}].

%% @doc Count tokens with specific status
count_tokens_with_status(ExecState, Status) ->
    lists:count(fun(_Id, Token) ->
        Token#token.status =:= Status
    end, maps:to_list(ExecState#exec_state.tokens)).
