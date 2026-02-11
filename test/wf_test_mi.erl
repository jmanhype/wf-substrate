-module(wf_test_mi).
-export([
    mock_bytecode_mi_fixed_3/0,
    mock_bytecode_mi_fixed_5/0,
    mock_bytecode_mi_dynamic/0,
    mock_bytecode_mi_wait_n/0,
    mock_bytecode_mi_first_complete/0
]).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_mi.hrl").

%%====================================================================
%% Mock Bytecode Generators for MI Patterns
%%====================================================================

%% Fixed count MI with 3 instances, wait_all join policy
mock_bytecode_mi_fixed_3() ->
    [
        {mi_spawn, {{fixed, 3}, wait_all, 10}},
        {task_exec, mi_task},
        {done},
        {join_wait, wait_all}
    ].

%% Fixed count MI with 5 instances
mock_bytecode_mi_fixed_5() ->
    [
        {mi_spawn, {{fixed, 5}, wait_all, 10}},
        {task_exec, mi_task},
        {done},
        {join_wait, wait_all}
    ].

%% Dynamic count MI with min=2, max=5
mock_bytecode_mi_dynamic() ->
    [
        {mi_spawn, {{dynamic, 2, 5}, wait_all, 10}},
        {task_exec, mi_task},
        {done},
        {join_wait, wait_all}
    ].

%% MI with wait_n join policy (3 of 5)
mock_bytecode_mi_wait_n() ->
    [
        {mi_spawn, {{fixed, 5}, {wait_n, 3}, 10}},
        {task_exec, mi_task},
        {done},
        {join_wait, {wait_n, 3}}
    ].

%% MI with first_complete join policy
mock_bytecode_mi_first_complete() ->
    [
        {mi_spawn, {{fixed, 4}, first_complete, 10}},
        {task_exec, mi_task},
        {done},
        {join_wait, first_complete}
    ].

%% MI with none join policy
mock_bytecode_mi_none() ->
    [
        {mi_spawn, {{fixed, 3}, none, 10}},
        {task_exec, mi_task},
        {done}
    ].

%%====================================================================
%% Unit Tests: Fixed Count MI
%%====================================================================

mi_fixed_count_3_all_complete_test() ->
    Bytecode = mock_bytecode_mi_fixed_3(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify 3 instances spawned and completed
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(4, maps:size(Tokens)), % root + 3 instances
    %% All tokens should be complete
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(4, CompleteCount).

mi_fixed_count_5_all_complete_test() ->
    Bytecode = mock_bytecode_mi_fixed_5(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(6, maps:size(Tokens)), % root + 5 instances
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(6, CompleteCount).

%%====================================================================
%% Unit Tests: Dynamic Count MI
%%====================================================================

mi_dynamic_count_test() ->
    Bytecode = mock_bytecode_mi_dynamic(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Dynamic count uses min value (2)
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(3, maps:size(Tokens)), % root + 2 instances
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(3, CompleteCount).

%%====================================================================
%% Unit Tests: wait_n Join Policy
%%====================================================================

mi_wait_n_3_of_5_test() ->
    Bytecode = mock_bytecode_mi_wait_n(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify 3 of 5 completed, 2 cancelled
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(6, TotalTokens), % root + 5 instances
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(4, CompleteCount), % root + 3 completed instances
    ?assertEqual(2, CancelledCount). % 2 cancelled instances

%%====================================================================
%% Unit Tests: first_complete Join Policy
%%====================================================================

mi_first_complete_test() ->
    Bytecode = mock_bytecode_mi_first_complete(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify only 1 completed, rest cancelled
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(5, TotalTokens), % root + 4 instances
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    CancelledCount = count_tokens_with_status(ExecState1, cancelled),
    ?assertEqual(2, CompleteCount), % root + 1 completed instance
    ?assertEqual(3, CancelledCount). % 3 cancelled instances

%%====================================================================
%% Unit Tests: none Join Policy
%%====================================================================

mi_none_join_test() ->
    Bytecode = mock_bytecode_mi_none(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% All instances complete independently
    Tokens = ExecState1#exec_state.tokens,
    TotalTokens = maps:size(Tokens),
    ?assertEqual(4, TotalTokens), % root + 3 instances
    CompleteCount = count_tokens_with_status(ExecState1, complete),
    ?assertEqual(4, CompleteCount).

%%====================================================================
%% Unit Tests: MI Cancellation
%%====================================================================

mi_cancellation_mid_execution_test() ->
    Bytecode = mock_bytecode_mi_fixed_3(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Run until MI_SPAWN completes
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    %% Cancel all instances
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    %% All tokens should be cancelled
    CancelledCount = count_tokens_with_status(ExecState3, cancelled),
    ?assert(CancelledCount > 0).

%%====================================================================
%% Unit Tests: Instance ID Assignment
%%====================================================================

mi_instance_id_unique_test() ->
    Bytecode = mock_bytecode_mi_fixed_5(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Extract instance IDs from tokens
    Tokens = maps:values(ExecState1#exec_state.tokens),
    InstanceIds = [T#token.instance_id || T <- Tokens, T#token.instance_id =/= undefined],
    %% All instance IDs should be unique
    ?assertEqual(5, length(lists:usort(InstanceIds))).

mi_instance_sequential_numbering_test() ->
    Bytecode = mock_bytecode_mi_fixed_3(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Extract instance numbers from token values
    Tokens = maps:values(ExecState1#exec_state.tokens),
    InstanceNums = [maps:get(instance_num, T#token.value, 0)
                    || T <- Tokens, T#token.instance_id =/= undefined],
    %% Should be sequential [1, 2, 3]
    ?assertEqual([1, 2, 3], lists:sort(InstanceNums)).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Count tokens with specific status
count_tokens_with_status(ExecState, Status) ->
    Tokens = ExecState#exec_state.tokens,
    lists:count(fun(_Id, Token) ->
        Token#token.status =:= Status
    end, maps:to_list(Tokens)).
