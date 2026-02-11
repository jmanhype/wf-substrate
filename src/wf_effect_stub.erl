-module(wf_effect_stub).

%% API
-export([
    new_spec/5,
    yield/4,
    get_effect_cost/1
]).

-include("wf_governance.hrl").

%% Effect specification record (matching item 010 plan)
-record(effect_spec, {
    effect_id :: term(),
    effect_type :: atom(),
    payload :: term(),
    idempotency_key :: term() | undefined,
    timeout :: pos_integer() | undefined,
    cost :: integer()  %% Cost for budget tracking
}).

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Create new effect specification
-spec new_spec(term(), non_neg_integer(), term(), atom(), term()) -> #effect_spec{}.
new_spec(CaseId, StepSeq, ScopeId, EffectType, Payload) ->
    EffectId = {CaseId, StepSeq, ScopeId},
    #effect_spec{
        effect_id = EffectId,
        effect_type = EffectType,
        payload = Payload,
        idempotency_key = undefined,
        timeout = undefined,
        cost = get_default_cost(EffectType)
    }.

%% @doc Yield effect with governance checks
%% NOTE: This is a stub implementation. Real implementation in item 010.
-spec yield(term(), non_neg_integer(), term(), #effect_spec{}) ->
    {ok, term()} | {error, #governance_error{}}.
yield(CaseId, _StepSeq, ScopeId, EffectSpec) ->
    %% 1. Check allowlist for scope
    case wf_governance:get_allowlist(ScopeId) of
        {ok, AllowList} ->
            EffectType = EffectSpec#effect_spec.effect_type,
            case lists:member(EffectType, AllowList) of
                false ->
                    Error = #governance_error{
                        error_type = allowlist_violation,
                        scope = ScopeId,
                        task_id = undefined,
                        detail = list_to_binary(
                            io_lib:format("Effect type ~p not in allowlist",
                                [EffectType])),
                        timestamp = erlang:timestamp()
                    },
                    {error, Error};
                true ->
                    %% 2. Check budget for case
                    case wf_budget:check_budget(CaseId) of
                        {error, _} = Error ->
                            Error;
                        ok ->
                            %% 3. Execute effect (mock)
                            execute_effect_mock(EffectSpec)
                    end
            end;
        {error, not_found} ->
            %% No allowlist configured, allow all effects
            case wf_budget:check_budget(CaseId) of
                {error, _} = Error ->
                    Error;
                ok ->
                    execute_effect_mock(EffectSpec)
            end
    end.

%% @doc Get cost for effect type
-spec get_effect_cost(#effect_spec{}) -> integer().
get_effect_cost(EffectSpec) ->
    EffectSpec#effect_spec.cost.

%%%====================================================================
%%% Internal functions
%%%====================================================================

%% @doc Mock effect execution
%% TODO: Replace with real wf_effect implementation in item 010
-spec execute_effect_mock(#effect_spec{}) -> {ok, term()}.
execute_effect_mock(_EffectSpec) ->
    %% In real implementation (item 010), this would:
    %% 1. Store effect in ETS table
    %% 2. Spawn handler process
    %% 3. Return {ok, Effect}
    %% For now, return mock result
    {ok, #{result => mock_effect_done}}.

%% @doc Default costs for common effect types
-spec get_default_cost(atom()) -> integer().
get_default_cost(http_get) -> 1;
get_default_cost(http_post) -> 2;
get_default_cost(file_write) -> 3;
get_default_cost(file_read) -> 1;
get_default_cost(db_query) -> 5;
get_default_cost(send_email) -> 10;
get_default_cost(_) -> 1.
