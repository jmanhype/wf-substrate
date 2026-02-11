-module(wf_sched).

%%====================================================================
%% Behaviour Definition
%%====================================================================

-callback choose(EnabledActions :: [enabled_action()],
                 SchedState :: sched_state()) ->
    {Chosen :: enabled_action(), NewSchedState :: sched_state()}.

%%====================================================================
%% Types
%%====================================================================

-type enabled_action() ::
    {token, token_id()} |
    {xor_branch, pos_integer()}.

-type token_id() :: term().

-type sched_policy() ::
    deterministic |
    nondeterministic |
    {replay, choice_log()}.

-type sched_state() :: term().

-type choice_log() :: [choice_entry()].

-type choice_entry() :: {
    StepSeq :: non_neg_integer(),
    EnabledSet :: [enabled_action()],
    Chosen :: enabled_action()
}.

-type sched_decision() :: {token, term()} | {xor_branch, pos_integer()}.
-type exec_state() :: term().  %% Opaque for scheduler

%%====================================================================
%% Exports
%%====================================================================

-export_type([
    enabled_action/0,
    sched_policy/0,
    sched_state/0,
    choice_log/0,
    choice_entry/0
]).

-export([
    new/2,
    choose/2,
    get_log/1,
    from_log/1,
    select_action/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new scheduler state for given policy
-spec new(sched_policy(), proplists:proplist()) -> {ok, sched_state()}.
new(Policy, Options) ->
    case Policy of
        deterministic ->
            wf_sched_deterministic:new(Options);
        nondeterministic ->
            wf_sched_nondeterministic:new(Options);
        {replay, ChoiceLog} ->
            wf_sched_replay:new(ChoiceLog, Options)
    end.

%% @doc Choose next action from enabled set
-spec choose([enabled_action()], sched_state()) ->
    {enabled_action(), sched_state()}.
choose(EnabledActions, SchedState) when is_list(EnabledActions) ->
    case EnabledActions of
        [] ->
            error({no_enabled_actions, SchedState});
        [Single] ->
            {Single, SchedState};
        _Multiple ->
            PolicyMod = extract_policy_module(SchedState),
            PolicyMod:choose(EnabledActions, SchedState)
    end.

%% @doc Extract choice log from scheduler state (nondeterministic only)
-spec get_log(sched_state()) -> choice_log().
get_log(SchedState) ->
    case SchedState of
        #{log := Log} ->
            lists:reverse(Log);
        _ ->
            []
    end.

%% @doc Create replay scheduler from recorded choice log
-spec from_log(choice_log()) -> sched_state().
from_log(ChoiceLog) when is_list(ChoiceLog) ->
    #{log => ChoiceLog, position => 1, policy => replay}.

%% @doc Legacy API for backward compatibility
%% Accepts both sched_policy() atom and sched_state() map
-spec select_action(exec_state(), sched_policy() | sched_state()) -> sched_decision().
select_action(_ExecState, PolicyOrState) when is_atom(PolicyOrState) ->
    case PolicyOrState of
        deterministic -> {token, mock_token};
        nondeterministic -> {token, mock_token};
        {replay, _} -> {token, replay_token};
        undefined -> {token, mock_token}
    end;
select_action(ExecState, SchedState) when is_map(SchedState) ->
    %% Extract policy from sched state map
    Policy = maps:get(policy, SchedState, deterministic),
    select_action(ExecState, Policy).

%%====================================================================
%% Internal Functions
%%====================================================================

-spec extract_policy_module(sched_state()) -> module().
extract_policy_module(#{policy := deterministic}) ->
    wf_sched_deterministic;
extract_policy_module(#{policy := nondeterministic}) ->
    wf_sched_nondeterministic;
extract_policy_module(#{policy := replay}) ->
    wf_sched_replay;
extract_policy_module(undefined) ->
    wf_sched_deterministic;
extract_policy_module(_State) ->
    wf_sched_deterministic.
