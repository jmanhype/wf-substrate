-module(wf_sched_nondeterministic).
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

%% @doc Create nondeterministic scheduler state with optional seed
-spec new(proplists:proplist()) -> {ok, wf_sched:sched_state()}.
new(Options) ->
    %% Initialize random seed
    Seed = proplists:get_value(seed, Options),
    RandState = case Seed of
        undefined ->
            %% Use default algorithm with time-based seed
            rand:seed_s(exsss);
        {A, B, C} when is_integer(A), is_integer(B), is_integer(C) ->
            %% Use provided seed
            rand:seed_s(exsss, {A, B, C})
    end,

    %% Initialize state with empty log and rand state
    {ok, #{
        policy => nondeterministic,
        log => [],
        step_seq => 0,
        rand_state => RandState
    }}.

%% @doc Choose action randomly and log the choice
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, #{log := Log, step_seq := StepSeq, rand_state := RandState0} = State)
    when is_list(EnabledActions), length(EnabledActions) > 0 ->

    %% Random selection: pick index 1..N with explicit state
    N = length(EnabledActions),
    {Index, RandState1} = rand:uniform_s(N, RandState0),
    Chosen = lists:nth(Index, EnabledActions),

    %% Log choice for replay
    Entry = {StepSeq, EnabledActions, Chosen},
    NewLog = [Entry | Log],

    %% Update state
    NewState = State#{
        log => NewLog,
        step_seq => StepSeq + 1,
        rand_state => RandState1
    },

    {Chosen, NewState}.

%%====================================================================
%% Internal Functions
%%====================================================================
