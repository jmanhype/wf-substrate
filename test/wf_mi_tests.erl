-module(wf_mi_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_mi.hrl").
-include("wf_exec.hrl").

%%====================================================================
%% Unit Tests: eval_instance_count
%%====================================================================

eval_instance_count_fixed_positive_test() ->
    ?assertEqual({ok, 5}, wf_mi:eval_instance_count({fixed, 5}, #{})).

eval_instance_count_fixed_zero_test() ->
    ?assertEqual({error, zero_instances}, wf_mi:eval_instance_count({fixed, 0}, #{})).

eval_instance_count_dynamic_valid_test() ->
    ?assertEqual({ok, 2}, wf_mi:eval_instance_count({dynamic, 2, 10}, #{})).

eval_instance_count_dynamic_invalid_range_test() ->
    ?assertEqual({error, {invalid_range, 10, 2}},
                 wf_mi:eval_instance_count({dynamic, 10, 2}, #{})).

eval_instance_count_dynamic_zero_min_test() ->
    ?assertEqual({error, zero_minimum},
                 wf_mi:eval_instance_count({dynamic, 0, 10}, #{})).

%%====================================================================
%% Unit Tests: spawn_instances
%%====================================================================

spawn_instances_fixed_test() ->
    {ok, Instances, Config} = wf_mi:spawn_instances({fixed, 3}, wait_all, 10, root),
    ?assertEqual(3, length(Instances)),
    ?assertEqual(3, length(Config#mi_config.instance_ids)).

spawn_instances_dynamic_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({dynamic, 2, 10}, wait_all, 10, root),
    ?assertEqual(2, length(Instances)).

spawn_instances_too_many_test() ->
    ?assertError({too_many_instances, 1001, 1000},
                 wf_mi:spawn_instances({fixed, 1001}, wait_all, 10, root)).

spawn_instances_unique_ids_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, 100}, wait_all, 10, root),
    Ids = [I#mi_instance.instance_id || I <- Instances],
    ?assertEqual(100, length(lists:usort(Ids))).

spawn_instances_sequential_numbers_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, 5}, wait_all, 10, root),
    Nums = [I#mi_instance.instance_num || I <- Instances],
    ?assertEqual([1, 2, 3, 4, 5], lists:sort(Nums)).

%%====================================================================
%% Unit Tests: check_join
%%====================================================================

check_join_not_satisfied_test() ->
    ExecState0 = create_mi_state({fixed, 3}, wait_all),
    %% Complete 2 out of 3
    [{i1, Id1}, {i2, Id2}, {i3, _Id3}] = get_instance_ids(ExecState0),
    {ExecState1, _} = wf_mi:collect_result(Id1, result1, ExecState0),
    {ExecState2, _} = wf_mi:collect_result(Id2, result2, ExecState1),
    ?assertEqual({ok, not_satisfied}, wf_mi:check_join(ExecState2)).

check_join_satisfied_test() ->
    ExecState0 = create_mi_state({fixed, 3}, wait_all),
    %% Complete all 3
    [{i1, Id1}, {i2, Id2}, {i3, Id3}] = get_instance_ids(ExecState0),
    {ExecState1, _} = wf_mi:collect_result(Id1, result1, ExecState0),
    {ExecState2, _} = wf_mi:collect_result(Id2, result2, ExecState1),
    {ExecState3, _} = wf_mi:collect_result(Id3, result3, ExecState2),
    ?assertEqual({ok, satisfied}, wf_mi:check_join(ExecState3)).

check_join_no_active_join_test() ->
    ExecState = create_mi_state({fixed, 2}, none),
    ?assertEqual({error, no_active_join}, wf_mi:check_join(ExecState)).

%%====================================================================
%% Unit Tests: cancel_remaining
%%====================================================================

cancel_remaining_test() ->
    ExecState = create_mi_state({fixed, 3}, {wait_n, 2}),
    %% Get first two instance IDs
    [{i1, Id1}, {i2, Id2}, {i3, Id3}] = get_instance_ids(ExecState),

    {ExecState1, _} = wf_mi:collect_result(Id1, result1, ExecState),
    {ExecState2, JoinSat} = wf_mi:collect_result(Id2, result2, ExecState1),
    ?assertEqual(true, JoinSat),

    ExecState3 = wf_mi:cancel_remaining(Id2, ExecState2),
    Token3 = get_token_for_instance(Id3, ExecState3),
    ?assertEqual(cancelled, Token3#token.status).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create test exec state with MI instances
create_mi_state(MIPolicy, MIJoinPolicy) ->
    {ok, Instances, _Config} = wf_mi:spawn_instances(MIPolicy, MIJoinPolicy, 10, root),
    Tokens = lists:map(fun(I) ->
        Tid = I#mi_instance.token_id,
        Iid = I#mi_instance.instance_id,
        {Tid, #token{
            token_id = Tid,
            ip = 10,
            scope_id = root,
            value = #{instance_id => Iid},
            status = active,
            instance_id = Iid
        }}
    end, Instances),

    BranchId = make_ref(),
    TokenIds = [Tid || {Tid, _} <- Tokens],
    JoinId = case MIJoinPolicy of
        none -> undefined;
        _ -> make_ref()
    end,

    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = TokenIds,
        join_id = JoinId,
        targets = [10]
    },

    JoinCounters = case JoinId of
        undefined -> #{};
        _ ->
            Required = case MIJoinPolicy of
                wait_all -> length(Instances);
                {wait_n, N} -> N;
                first_complete -> 1
            end,
            #{JoinId => #join_counter{
                join_id = JoinId,
                completed = 0,
                required = Required,
                policy = all,
                results = []
            }}
    end,

    #exec_state{
        ip = 0,
        bytecode = [],
        ctx = #{},
        tokens = maps:from_list(Tokens),
        branch_map = #{BranchId => BranchInfo},
        join_counters = JoinCounters,
        scope_stack = [root],
        step_count = 0,
        status = running,
        current_token = hd(TokenIds)
    }.

%% Get token for instance ID
get_token_for_instance(InstanceId, ExecState) ->
    Tokens = maps:values(ExecState#exec_state.tokens),
    case lists:search(fun(T) -> T#token.instance_id =:= InstanceId end, Tokens) of
        {value, Token} -> Token;
        false -> error({token_not_found, InstanceId})
    end.

%% Get instance IDs from state (assign symbolic names for testing)
get_instance_ids(ExecState) ->
    Tokens = maps:values(ExecState#exec_state.tokens),
    SortedTokens = lists:sort(fun(A, B) ->
        maps:get(instance_num, A#token.value, 0) =< maps:get(instance_num, B#token.value, 0)
    end, [T || T <- Tokens, T#token.instance_id =/= undefined]),
    T1 = lists:nth(1, SortedTokens),
    T2 = lists:nth(2, SortedTokens),
    T3 = lists:nth(3, SortedTokens),
    [
        {i1, T1#token.instance_id},
        {i2, T2#token.instance_id},
        {i3, T3#token.instance_id}
    ].
