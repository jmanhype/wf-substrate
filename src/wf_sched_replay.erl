-module(wf_sched_replay).
-behaviour(wf_sched).

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/2,
    choose/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create replay scheduler from choice log
-spec new(wf_sched:choice_log(), proplists:proplist()) -> {ok, wf_sched:sched_state()}.
new(ChoiceLog, _Options) when is_list(ChoiceLog) ->
    %% Validate log format
    ok = validate_log(ChoiceLog),

    %% Initialize state with log and position
    {ok, #{
        policy => replay,
        log => ChoiceLog,
        position => 1
    }}.

%%====================================================================
%% Behaviour Callback
%%====================================================================

%% @doc Replay next choice from log, validate enabled set matches
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, #{log := ChoiceLog, position := Position} = State)
    when is_list(EnabledActions) ->

    %% Check if log exhausted
    case Position > length(ChoiceLog) of
        true ->
            error({replay_complete, Position});
        false ->
            %% Fetch recorded choice
            {_StepSeq, RecordedEnabled, Chosen} = lists:nth(Position, ChoiceLog),

            %% Validate enabled set matches recording
            case validate_enabled_set(EnabledActions, RecordedEnabled) of
                ok ->
                    %% Match! Return recorded choice
                    NewState = State#{position => Position + 1},
                    {Chosen, NewState};
                {error, {divergence, Expected, Actual}} ->
                    error({divergence, Expected, Actual})
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate choice log format
-spec validate_log(wf_sched:choice_log()) -> ok | no_return().
validate_log([]) ->
    ok;
validate_log([{StepSeq, EnabledSet, Chosen} | Rest])
    when is_integer(StepSeq), is_list(EnabledSet) ->
    %% Verify chosen action is in enabled set
    case lists:member(Chosen, EnabledSet) of
        true ->
            validate_log(Rest);
        false ->
            error({invalid_log, {chosen_not_in_enabled, Chosen, EnabledSet}})
    end;
validate_log([InvalidEntry | _]) ->
    error({invalid_log, {bad_entry, InvalidEntry}}).

%% @private Validate that actual enabled set matches recorded set
-spec validate_enabled_set([wf_sched:enabled_action()], [wf_sched:enabled_action()]) ->
    ok | {error, {divergence, term(), term()}}.
validate_enabled_set(Actual, Recorded) ->
    %% Use exact equality for strict divergence detection
    case Actual =:= Recorded of
        true ->
            ok;
        false ->
            {error, {divergence, Recorded, Actual}}
    end.
