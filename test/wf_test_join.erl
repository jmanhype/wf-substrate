-module(wf_test_join).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").

%%====================================================================
%% Mock Bytecode Generators for Join Policies
%%====================================================================

%% wait_all policy with M branches
mock_bytecode_join_wait_all(M) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([[
        {task_exec, list_to_atom("task_" ++ integer_to_list(I))},
        {done}
    ] || I <- lists:seq(1, M)]),
    [
        {par_fork, BranchTargets}
    | BranchBytecode] ++
    [{join_wait, all}].

%% wait_n policy: N of M complete
mock_bytecode_join_wait_n(M, N) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([[
        {task_exec, list_to_atom("task_" ++ integer_to_list(I))},
        {done}
    ] || I <- lists:seq(1, M)]),
    [
        {par_fork, BranchTargets}
    | BranchBytecode] ++
    [{join_wait, {wait_n, N}}].

%% first_complete policy (discriminator)
mock_bytecode_join_first_complete(M) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([[
        {task_exec, list_to_atom("task_" ++ integer_to_list(I))},
        {done}
    ] || I <- lists:seq(1, M)]),
    [
        {par_fork, BranchTargets}
    | BranchBytecode] ++
    [{join_wait, first_complete}].

%% sync_merge policy
mock_bytecode_join_sync_merge(M) ->
    BranchTargets = lists:seq(1, 2*M - 1, 2),
    BranchBytecode = lists:flatten([[
        {task_exec, list_to_atom("task_" ++ integer_to_list(I))},
        {done}
    ] || I <- lists:seq(1, M)]),
    [
        {par_fork, BranchTargets}
    | BranchBytecode] ++
    [{join_wait, sync_merge}].

%%====================================================================
%% Unit Tests: wait_all Policy
%%====================================================================

join_wait_all_3_test() ->
    Bytecode = mock_bytecode_join_wait_all(3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% All 3 branches completed
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(4, TotalTokens), % root + 3 branches
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(4, CompleteCount).

join_wait_all_5_test() ->
    Bytecode = mock_bytecode_join_wait_all(5),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(6, TotalTokens), % root + 5 branches
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(6, CompleteCount).

%%====================================================================
%% Unit Tests: wait_n Policy
%%====================================================================

join_wait_n_2_of_3_test() ->
    Bytecode = mock_bytecode_join_wait_n(3, 2),
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

join_wait_n_3_of_5_test() ->
    Bytecode = mock_bytecode_join_wait_n(5, 3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    _Tokens = ExecState1#exec_state.tokens,
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(4, CompleteCount), % root + 3 completed
    ?assertEqual(2, CancelledCount).

join_wait_n_1_of_4_test() ->
    Bytecode = mock_bytecode_join_wait_n(4, 1),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    _Tokens = ExecState1#exec_state.tokens,
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(2, CompleteCount), % root + 1 completed
    ?assertEqual(3, CancelledCount).

%%====================================================================
%% Unit Tests: first_complete Policy
%%====================================================================

join_first_complete_4_test() ->
    Bytecode = mock_bytecode_join_first_complete(4),
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

join_first_complete_10_test() ->
    Bytecode = mock_bytecode_join_first_complete(10),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    _Tokens = ExecState1#exec_state.tokens,
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(2, CompleteCount), % root + 1 completed
    ?assertEqual(9, CancelledCount).

%%====================================================================
%% Unit Tests: sync_merge Policy
%%====================================================================

join_sync_merge_test() ->
    Bytecode = mock_bytecode_join_sync_merge(3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% All branches should complete (sync_merge is like wait_all)
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(4, TotalTokens), % root + 3 branches
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(4, CompleteCount).

%%====================================================================
%% Unit Tests: Join Counter Mechanics
%%====================================================================

join_counter_increments_on_completion_test() ->
    Bytecode = mock_bytecode_join_wait_all(3),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until PAR_FORK creates join counter
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    %% Get join counter
    JoinCounters = ExecState1#exec_state.join_counters,
    ?assert(maps:size(JoinCounters) > 0),
    [{_JoinId, Counter}] = maps:to_list(JoinCounters),
    ?assertEqual(0, Counter#join_counter.completed),
    ?assertEqual(3, Counter#join_counter.required).

join_counter_threshold_check_test() ->
    Bytecode = mock_bytecode_join_wait_n(5, 3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Verify join counter satisfied at threshold
    JoinCounters = ExecState1#exec_state.join_counters,
    [{_JoinId, Counter}] = maps:to_list(JoinCounters),
    ?assertEqual(3, Counter#join_counter.completed),
    ?assertEqual(3, Counter#join_counter.required).

join_counter_all_required_test() ->
    Bytecode = mock_bytecode_join_wait_all(4),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    JoinCounters = ExecState1#exec_state.join_counters,
    [{_JoinId, Counter}] = maps:to_list(JoinCounters),
    ?assertEqual(4, Counter#join_counter.completed),
    ?assertEqual(4, Counter#join_counter.required).

%%====================================================================
%% Unit Tests: Edge Cases
%%====================================================================

join_edge_case_n_equals_m_test() ->
    Bytecode = mock_bytecode_join_wait_n(3, 3),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% All should complete (N=M)
    _Tokens = ExecState1#exec_state.tokens,
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(0, CancelledCount).

join_edge_case_n_equals_1_test() ->
    Bytecode = mock_bytecode_join_wait_n(5, 1),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Only 1 should complete
    _Tokens = ExecState1#exec_state.tokens,
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(2, CompleteCount), % root + 1 completed
    ?assertEqual(4, CancelledCount).

%%====================================================================
%% Parameterized Tests: Various M Values
%%====================================================================

join_wait_all_m_branches_test_() ->
    %% Test M = 2, 3, 5, 10 branches
    lists:map(fun(M) ->
        {io_lib:format("join_wait_all_~p_branches", [M]), fun() ->
            Bytecode = mock_bytecode_join_wait_all(M),
            ExecState0 = wf_exec:new(Bytecode),
            {done, ExecState1} = wf_exec:run(ExecState0, 1000, undefined),
            ?assert(wf_exec:is_done(ExecState1)),
            %% All M branches completed
            Tokens = ExecState1#exec_state.tokens,
            ?assertEqual(M + 1, maps:size(Tokens))
        end}
    end, [2, 3, 5, 10]).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Count tokens with specific status
count_tokens_with_status(ExecState, Status) ->
    lists:count(fun(_Id, Token) ->
        Token#token.status =:= Status
    end, maps:to_list(ExecState#exec_state.tokens)).
