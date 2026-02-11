%%%-------------------------------------------------------------------
%%% @doc Multiple Instance Pattern Semantics (WP16)
%%%
%%% This module implements multiple instance patterns from the workflow
%%% pattern catalog. MI patterns spawn N concurrent instances of the same
%%% workflow fragment with configurable synchronization policies.
%%%
%%% == Instance Creation Modes ==
%%% - Fixed count: N instances determined at compile time ({fixed, N})
%%% - Dynamic count: N instances determined at runtime from ctx ({dynamic, Min, Max})
%%%
%%% == Join Policies ==
%%% - wait_all: Block until all M instances complete (synchronization barrier)
%%% - {wait_n, N}: Block until N of M instances complete, cancel remainder
%%% - first_complete: Block until first instance completes, cancel all others
%%% - none: Fire-and-forget (no synchronization, continuation proceeds immediately)
%%%
%%% == Instance Lifecycle ==
%%% 1. Spawn: N instances created with unique instance_id, token, and instance_num
%%% 2. Execute: Each instance executes body bytecode concurrently (cooperative scheduling)
%%% 3. Complete: Instance marks complete, result collected, join counter incremented
%%% 4. Join Satisfied: Results merged, continuation token created, remaining instances cancelled
%%%
%%% == Integration ==
%%% - wf_exec: MI_SPAWN opcode handler calls wf_mi:spawn_instances/4
%%% - wf_exec: execute_done/2 calls wf_mi:collect_result/3 for MI instances
%%% - wf_exec: JOIN_WAIT handler calls wf_mi:check_join/2
%%% - wf_mi:cancel_remaining/2 cancels instances when join satisfied early
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_mi).

%% Include record definitions FIRST (before exports that use them)
-include("wf_exec.hrl").
-include("wf_mi.hrl").

%% API exports
-export([
    spawn_instances/4,
    collect_result/3,
    check_join/1,
    cancel_remaining/2,
    find_branch_for_instance/3,
    eval_instance_count/2  %% Exported for testing
]).

%% Type exports
-export_type([
    mi_join_policy/0
]).  %% mi_instance and mi_config defined in wf_mi.hrl

%% Note: exec_state, branch_info, and token records are defined in wf_exec
%% We reference them here but don't need to include the header

%%====================================================================
%% Types
%%====================================================================

%% MI join policies
-type mi_join_policy() ::
    wait_all |
    {wait_n, pos_integer()} |
    first_complete |
    none.  %% Fire-and-forget

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Spawn N instances from MI configuration
%% Returns {ok, Instances, Config} where Instances is list of #mi_instance{} records
-spec spawn_instances(wf_vm:mi_policy(), mi_join_policy(),
                      non_neg_integer(), term()) ->
    {ok, [#mi_instance{}], #mi_config{}}.
spawn_instances(MIPolicy, MIJoinPolicy, BodyIP, ScopeId) ->
    %% Evaluate instance count
    case eval_instance_count(MIPolicy, #{}) of
        {error, Reason} ->
            error({instance_count_error, Reason});
        {ok, NumInstances} when NumInstances > 1000 ->
            error({too_many_instances, NumInstances, 1000});
        {ok, NumInstances} ->
            %% Spawn N instances
            Instances = lists:map(fun(N) ->
                InstanceId = {instance, make_ref()},
                TokenId = make_ref(),

                #mi_instance{
                    instance_id = InstanceId,
                    token_id = TokenId,
                    instance_num = N,
                    status = active,
                    result = undefined
                }
            end, lists:seq(1, NumInstances)),

            %% Create MI config
            Config = #mi_config{
                policy = MIPolicy,
                join_policy = MIJoinPolicy,
                body_ip = BodyIP,
                scope_id = ScopeId,
                instance_ids = [I#mi_instance.instance_id || I <- Instances]
            },

            {ok, Instances, Config}
    end.

%% @doc Collect instance result and update join state
%% Returns {UpdatedExecState, JoinSatisfied} where JoinSatisfied is boolean()
-spec collect_result(term(), term(), term()) ->
    {term(), boolean()}.
collect_result(InstanceId, Result, ExecState) ->
    BranchMap = ExecState#exec_state.branch_map,
    Tokens = ExecState#exec_state.tokens,

    case find_branch_for_instance(InstanceId, BranchMap, Tokens) of
        {ok, BranchId} ->
            BranchInfo = maps:get(BranchId, BranchMap),
            JoinId = BranchInfo#branch_info.join_id,

            case JoinId of
                undefined ->
                    %% Fire-and-forget, no join
                    {ExecState, false};
                _JoinId ->
                    %% Increment join counter
                    JoinCounters = ExecState#exec_state.join_counters,
                    JoinCounter = maps:get(JoinId, JoinCounters),
                    UpdatedJoinCounter = JoinCounter#join_counter{
                        completed = JoinCounter#join_counter.completed + 1,
                        results = [Result | JoinCounter#join_counter.results]
                    },
                    UpdatedJoinCounters = maps:put(JoinId, UpdatedJoinCounter, JoinCounters),

                    %% Check if join satisfied
                    JoinSatisfied = UpdatedJoinCounter#join_counter.completed >=
                                    UpdatedJoinCounter#join_counter.required,
                    {ExecState#exec_state{join_counters = UpdatedJoinCounters}, JoinSatisfied}
            end;
        error ->
            %% Instance not found in branch map (should not happen)
            {ExecState, false}
    end.

%% @doc Check if join policy is satisfied
%% Returns {ok, satisfied} | {ok, not_satisfied} | {error, term()}
-spec check_join(term()) ->
    {ok, satisfied | not_satisfied} | {error, term()}.
check_join(ExecState) ->
    JoinCounters = ExecState#exec_state.join_counters,
    case maps:to_list(JoinCounters) of
        [{_JoinId, #join_counter{completed = Completed, required = Required}}] ->
            case Completed >= Required of
                true -> {ok, satisfied};
                false -> {ok, not_satisfied}
            end;
        [] ->
            {error, no_active_join};
        _Multiple ->
            {error, multiple_active_joins}
    end.

%% @doc Cancel remaining instances when join satisfied early
%% Returns updated exec_state with remaining instances marked cancelled
-spec cancel_remaining(term(), term()) -> term().
cancel_remaining(CompletedInstanceId, ExecState) ->
    BranchMap = ExecState#exec_state.branch_map,
    Tokens = ExecState#exec_state.tokens,

    %% Find branch for completed instance
    case find_branch_for_instance(CompletedInstanceId, BranchMap, Tokens) of
        {ok, BranchId} ->
            BranchInfo = maps:get(BranchId, BranchMap),
            AllTokenIds = BranchInfo#branch_info.tokens,

            %% Find all active instances (excluding completed one)
            ActiveTokenIds = lists:filter(fun(TokenId) ->
                Token = maps:get(TokenId, Tokens),
                Token#token.status =:= active andalso
                Token#token.instance_id =/= CompletedInstanceId
            end, AllTokenIds),

            %% Mark remaining instances as cancelled
            UpdatedTokens = lists:foldl(fun(TokenId, AccTokens) ->
                Token = maps:get(TokenId, AccTokens),
                AccTokens#{TokenId => Token#token{status = cancelled}}
            end, Tokens, ActiveTokenIds),

            ExecState#exec_state{tokens = UpdatedTokens};
        error ->
            %% Instance not found (should not happen)
            ExecState
    end.

%% @doc Find branch for a given instance ID
%% Returns {ok, BranchId} or error
-spec find_branch_for_instance(term(), term(), term()) ->
    {ok, term()} | error.
find_branch_for_instance(InstanceId, BranchMap, Tokens) ->
    %% Find token with this instance_id
    TokenId = case lists:search(fun({_Tid, Token}) ->
        Token#token.instance_id =:= InstanceId
    end, maps:to_list(Tokens)) of
        {value, {Tid, _Token}} -> Tid;
        false -> error
    end,

    case TokenId of
        error -> error;
        _ ->
            %% Find branch containing this token
            find_branch_for_token(TokenId, BranchMap)
    end.

%% @doc Find branch for a given token ID (internal)
-spec find_branch_for_token(term(), term()) ->
    {ok, term()} | error.
find_branch_for_token(TokenId, BranchMap) ->
    case [BranchId || {BranchId, #branch_info{tokens = Tokens}} <- maps:to_list(BranchMap),
                      lists:member(TokenId, Tokens)] of
        [BranchId | _] -> {ok, BranchId};
        [] -> error
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Evaluate instance count (fixed or dynamic)
%% For v1, dynamic policy returns Min instances (stub)
%% v2: Evaluate user function Fun :: ctx() -> pos_integer()
-spec eval_instance_count(wf_vm:mi_policy(), map()) -> {ok, pos_integer()} | {error, term()}.
eval_instance_count({fixed, N}, _Ctx) when N > 0 ->
    {ok, N};
eval_instance_count({fixed, 0}, _Ctx) ->
    {error, zero_instances};
eval_instance_count({dynamic, Min, Max}, _Ctx) when Min > 0, Min =< Max ->
    %% v1: Return Min instances (stub)
    %% v2: Call user function Fun :: ctx() -> pos_integer()
    {ok, Min};
eval_instance_count({dynamic, Min, Max}, _Ctx) when Min > Max ->
    {error, {invalid_range, Min, Max}};
eval_instance_count({dynamic, 0, _Max}, _Ctx) ->
    {error, zero_minimum}.
