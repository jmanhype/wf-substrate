-module(wf_sched_deterministic).
-behaviour(wf_sched).

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/1,
    choose/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create deterministic scheduler state (stateless)
-spec new(proplists:proplist()) -> {ok, wf_sched:sched_state()}.
new(_Options) ->
    %% Deterministic policy is stateless
    {ok, #{policy => deterministic}}.

%% @doc Choose action with stable ordering
%% Sorts enabled actions: first by type (token < xor_branch), then by ID
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, State) when is_list(EnabledActions), length(EnabledActions) > 0 ->
    %% Stable sort: deterministic ordering
    Sorted = stable_sort(EnabledActions),
    {hd(Sorted), State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Stable sort of enabled actions
%% Ordering: token < xor_branch, then by ID/index
-spec stable_sort([wf_sched:enabled_action()]) -> [wf_sched:enabled_action()].
stable_sort(Actions) ->
    lists:sort(fun compare_actions/2, Actions).

%% @private Compare two enabled actions for ordering
-spec compare_actions(wf_sched:enabled_action(), wf_sched:enabled_action()) -> boolean().
compare_actions({TypeA, IdA}, {TypeB, IdB}) ->
    if
        TypeA < TypeB ->
            true;
        TypeA > TypeB ->
            false;
        TypeA =:= TypeB ->
            %% Same type, compare IDs
            %% Convert refs to binaries for deterministic ordering
            compare_ids(IdA, IdB)
    end.

%% @private Compare two IDs (may be refs, integers, or other terms)
-spec compare_ids(term(), term()) -> boolean().
compare_ids(IdA, IdB) when is_reference(IdA), is_reference(IdB) ->
    %% Refs need conversion for deterministic ordering
    term_to_binary(IdA) < term_to_binary(IdB);
compare_ids(IdA, IdB) ->
    %% Other types use standard Erlang ordering
    IdA < IdB.
