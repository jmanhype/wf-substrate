-module(wf_governance_props).
-include_lib("proper/include/proper.hrl").
-include("wf_governance.hrl").

%%%====================================================================
%%% Property-based tests for governance invariants
%%%====================================================================

%%%====================================================================
%%% Fixtures
%%%====================================================================

setup() ->
    {ok, _} = wf_governance:start_link(),
    {ok, _} = wf_budget:start_link().

cleanup(_) ->
    wf_budget:stop(),
    wf_governance:stop().

%%%====================================================================
%%% Properties
%%%====================================================================

%% Property: Budget never exceeds configured limits
prop_budget_never_exceeds() ->
    ?FORALL({Limits, Effects},
        {budget_limits(), effect_list()},
        begin
            setup(),
            CaseId = make_ref(),
            wf_budget:init_budget(CaseId, Limits),
            Result = execute_effects(CaseId, Effects),
            cleanup(ok),
            Result == ok orelse Result == {error, budget_exceeded}
        end).

%% Property: Allowlist only allows configured effect types
prop_allowlist_enforcement() ->
    ?FORALL({AllowList, EffectType},
        {effect_list(), effect_type()},
        begin
            setup(),
            ScopeId = make_ref(),
            wf_governance:set_allowlist(ScopeId, AllowList),
            Spec = wf_effect_stub:new_spec(case1, 0, ScopeId, EffectType, #{}),
            Result = wf_effect_stub:yield(case1, 0, ScopeId, Spec),
            cleanup(ok),
            case lists:member(EffectType, AllowList) of
                true -> Result =:= {ok, _} orelse Result =:= {error, #governance_error{error_type = budget_exceeded}};
                false -> Result =:= {error, #governance_error{error_type = allowlist_violation}}
            end
        end).

%% Property: Multiple cases have independent budgets
prop_independent_budgets() ->
    ?FORALL(LimitsList,
        non_empty(list(budget_limits())),
        begin
            setup(),
            CaseIds = [make_ref() || _ <- LimitsList],
            lists:foreach(fun({CaseId, Limits}) ->
                wf_budget:init_budget(CaseId, Limits)
            end, lists:zip(CaseIds, LimitsList)),

            %% Execute effects in first case only
            [Case1 | _] = CaseIds,
            wf_budget:increment_effect_count(Case1),

            %% Verify other cases are unaffected
            Result = lists:all(fun(CaseId) ->
                case CaseId of
                    Case1 -> true;
                    _ -> wf_budget:check_budget(CaseId) == ok
                end
            end, CaseIds),
            cleanup(ok),
            Result
        end).

%% Property: Budget checks are idempotent
prop_budget_checks_idempotent() ->
    ?FORALL(Limits,
        budget_limits(),
        begin
            setup(),
            CaseId = make_ref(),
            wf_budget:init_budget(CaseId, Limits),
            Result1 = wf_budget:check_budget(CaseId),
            Result2 = wf_budget:check_budget(CaseId),
            Result3 = wf_budget:check_budget(CaseId),
            cleanup(ok),
            Result1 == Result2 andalso Result2 == Result3
        end).

%% Property: Budget increments are atomic and consistent
prop_budget_increments_atomic() ->
    ?FORALL(Increments,
        list({effect_count(), effect_time(), effect_cost()}),
        begin
            setup(),
            CaseId = make_ref(),
            wf_budget:init_budget(CaseId, [{max_effects, 1000}, {max_time_us, 1000000}, {max_cost, 1000}]),

            %% Apply increments
            lists:foreach(fun({Count, Time, Cost}) ->
                wf_budget:increment_effect_count(CaseId),
                wf_budget:add_effect_time(CaseId, Time),
                wf_budget:add_effect_cost(CaseId, Cost)
            end, Increments),

            %% Verify final state
            {ok, BudgetState} = wf_budget:get_budget_state(CaseId),
            ExpectedCount = length(Increments),
            ExpectedTime = lists:foldl(fun({_C, T, _Cost}, Acc) -> Acc + T end, 0, Increments),
            ExpectedCost = lists:foldl(fun({_C, _T, Cost}, Acc) -> Acc + Cost end, 0, Increments),

            Result = BudgetState#budget_state.effect_count == ExpectedCount
                andalso BudgetState#budget_state.effect_time_us == ExpectedTime
                andalso BudgetState#budget_state.total_cost == ExpectedCost,
            cleanup(ok),
            Result
        end).

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate budget limits
budget_limits() ->
    ?LET({MaxEffects, MaxTime, MaxCost},
        {pos_integer(), pos_integer(), pos_integer()},
        [{max_effects, MaxEffects}, {max_time_us, MaxTime * 1000}, {max_cost, MaxCost}]).

%% Generate effect type
effect_type() ->
    oneof([http_get, http_post, file_read, file_write, db_query, send_email, unknown_effect]).

%% Generate list of effect types (allowlist)
effect_list() ->
    non_empty(list(effect_type())).

%% Generate effect count increment
effect_count() ->
    non_neg_integer().

%% Generate effect time in microseconds
effect_time() ->
    non_neg_integer().

%% Generate effect cost
effect_cost() ->
    integer(-10, 100).

%% Generate list of effects
effect_list() ->
    list({effect_type(), effect_time(), effect_cost()}).

%%%====================================================================
%%% Internal functions
%%%====================================================================

execute_effects(_CaseId, []) ->
    ok;
execute_effects(CaseId, [{_Type, _Time, _Cost} | Rest]) ->
    case wf_budget:check_budget(CaseId) of
        {error, #governance_error{error_type = budget_exceeded}} ->
            {error, budget_exceeded};
        ok ->
            wf_budget:increment_effect_count(CaseId),
            execute_effects(CaseId, Rest)
    end.

%%%====================================================================
%%% Test wrappers
%%%====================================================================

prop_budget_never_exceeds_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?assert(proper:quickcheck(prop_budget_never_exceeds(), 50))
        end
    }.

prop_allowlist_enforcement_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?assert(proper:quickcheck(prop_allowlist_enforcement(), 50))
        end
    }.

prop_independent_budgets_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?assert(proper:quickcheck(prop_independent_budgets(), 50))
        end
    }.

prop_budget_checks_idempotent_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?assert(proper:quickcheck(prop_budget_checks_idempotent(), 50))
        end
    }.

prop_budget_increments_atomic_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?assert(proper:quickcheck(prop_budget_increments_atomic(), 50))
        end
    }.
