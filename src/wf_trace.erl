-module(wf_trace).

%% Include record definitions
-include("wf_exec.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(trace_event, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode(),
    state_before :: binary() | undefined,
    state_after :: binary() | undefined,
    timestamp :: erlang:monotonic_time(),
    scope :: [term()],
    branch_id :: term() | undefined,
    metadata :: map()
}).

-record(replay_entry, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode() | undefined,
    scheduler_choice :: wf_sched:choice_entry() | undefined,
    effect_result :: {term(), term()} | undefined
}).

-record(trace_state, {
    level :: trace_level(),
    sink :: trace_sink(),
    case_id :: term()
}).

%%====================================================================
%% Types
%%====================================================================

-type trace_level() :: none | min | full.

-type trace_sink() ::
    {callback, fun((#trace_event{}) -> ok)} |
    {ets, ets:tid()} |
    {process, pid()} |
    {replay, replay_log()}.

-type trace_event() :: #trace_event{}.
-type replay_entry() :: #replay_entry{}.
-type replay_log() :: [replay_entry()].

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/1,
    emit/2,
    set_sink/2,
    get_events/1,
    to_replay_log/1,
    from_replay_log/1,
    filter/2,
    set_level/1,
    get_level/0
]).

-export_type([
    trace_level/0,
    trace_sink/0,
    trace_event/0,
    replay_entry/0,
    replay_log/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new trace state with specified level
-spec new(trace_level()) -> {ok, #trace_state{}}.
new(Level) ->
    %% Create default ETS table sink
    Table = ets:new(wf_trace_events, [
        named_table,
        bag,  %% Allow duplicate events (multiple events per step_seq)
        public,
        {read_concurrency, true}
    ]),

    State = #trace_state{
        level = Level,
        sink = {ets, Table},
        case_id = undefined
    },
    {ok, State}.

%% @doc Set trace level in process dictionary
-spec set_level(trace_level()) -> ok.
set_level(Level) ->
    case get(wf_trace_state) of
        undefined ->
            %% No trace state, create temporary
            State = #trace_state{
                level = Level,
                sink = {ets, wf_trace_events},
                case_id = undefined
            },
            put(wf_trace_state, State),
            ok;
        State ->
            put(wf_trace_state, State#trace_state{level = Level}),
            ok
    end.

%% @doc Get current trace level from process dictionary
-spec get_level() -> trace_level().
get_level() ->
    case get(wf_trace_state) of
        undefined -> none;
        State -> State#trace_state.level
    end.

%% @doc Set trace sink (callback, ETS, or process)
-spec set_sink(#trace_state{}, trace_sink()) -> #trace_state{}.
set_sink(State, Sink) ->
    State#trace_state{sink = Sink}.

%% @doc Get all events from trace state sink
-spec get_events(#trace_state{}) -> [#trace_event{}].
get_events(#trace_state{sink = {ets, Table}}) ->
    ets:tab2list(Table);

get_events(#trace_state{sink = {process, _Pid}}) ->
    %% Process sink doesn't store events
    [];

get_events(#trace_state{sink = {callback, _Fun}}) ->
    %% Callback sink doesn't store events
    [];

get_events(#trace_state{sink = {replay, _ReplayLog}}) ->
    %% Replay sink doesn't store events
    [].

%% @doc Emit trace event (checks trace level)
-spec emit(#exec_state{}, map()) -> ok.
emit(ExecState, TraceEvent) ->
    Level = get_level(),
    Opcode = maps:get(opcode, TraceEvent),

    case Level of
        none ->
            %% No-op, zero overhead
            ok;
        min ->
            %% Emit only structural events
            case is_structural_opcode(Opcode) of
                true -> emit_event(ExecState, TraceEvent);
                false -> ok
            end;
        full ->
            %% Emit all events
            emit_event(ExecState, TraceEvent)
    end.

%% @doc Filter events by opcode, scope, branch, or predicate
-spec filter([#trace_event{}], {opcode, wf_vm:opcode()}) -> [#trace_event{}];
            ([#trace_event{}], {scope, term()}) -> [#trace_event{}];
            ([#trace_event{}], {branch, term()}) -> [#trace_event{}];
            ([#trace_event{}], {predicate, fun((#trace_event{}) -> boolean())}) -> [#trace_event{}].
filter(Events, {opcode, Opcode}) ->
    [E || E <- Events, E#trace_event.opcode =:= Opcode];

filter(Events, {scope, ScopeId}) ->
    [E || E <- Events, lists:member(ScopeId, E#trace_event.scope)];

filter(Events, {branch, BranchId}) ->
    [E || E <- Events, E#trace_event.branch_id =:= BranchId];

filter(Events, {predicate, Fun}) when is_function(Fun, 1) ->
    [E || E <- Events, Fun(E)].

%% @doc Extract replay log from trace events
-spec to_replay_log([#trace_event{}]) -> replay_log().
to_replay_log(TraceEvents) ->
    lists:filtermap(fun(#trace_event{step_seq = StepSeq, opcode = Opcode, metadata = Metadata}) ->
        case maps:get(scheduler_choice, Metadata, undefined) of
            undefined ->
                case maps:get(effect_result, Metadata, undefined) of
                    undefined -> false;
                    EffectResult ->
                        {true, #replay_entry{
                            step_seq = StepSeq,
                            opcode = Opcode,
                            scheduler_choice = undefined,
                            effect_result = EffectResult
                        }}
                end;
            SchedChoice ->
                {true, #replay_entry{
                    step_seq = StepSeq,
                    opcode = Opcode,
                    scheduler_choice = SchedChoice,
                    effect_result = maps:get(effect_result, Metadata, undefined)
                }}
        end
    end, TraceEvents).

%% @doc Create trace state from replay log (for replay execution)
-spec from_replay_log(replay_log()) -> {ok, #trace_state{}} | {error, term()}.
from_replay_log(ReplayLog) ->
    try
        %% Validate replay log format
        ok = validate_replay_log(ReplayLog),

        %% Create trace state with replay sink
        State = #trace_state{
            level = full,
            sink = {replay, ReplayLog},
            case_id = undefined
        },
        {ok, State}
    catch
        _:_ ->
            {error, invalid_replay_log}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if opcode is structural (emitted at level=min)
-spec is_structural_opcode(wf_vm:opcode()) -> boolean().
is_structural_opcode({OpcodeName, _Arg}) ->
    lists:member(OpcodeName, ['PAR_FORK', 'JOIN_WAIT', 'CANCEL_SCOPE', 'DONE']);
is_structural_opcode({'DONE'}) ->
    true;
is_structural_opcode(_Other) ->
    false.

%% @private Build trace event and emit to sink
-spec emit_event(#exec_state{}, map()) -> ok.
emit_event(ExecState, TraceEvent) ->
    Level = get_level(),
    State = case get(wf_trace_state) of
        undefined -> error(no_trace_state);
        S -> S
    end,

    %% Build full trace event
    StepSeq = ExecState#exec_state.step_count + 1,
    Timestamp = erlang:monotonic_time(microsecond),
    Scope = ExecState#exec_state.scope_stack,

    %% Extract branch_id (if current token is in a branch)
    BranchId = extract_branch_id(ExecState),

    %% Capture state before (if level=full)
    StateBefore = case Level of
        full -> wf_exec:snapshot_exec_state(ExecState);
        min -> undefined
    end,

    %% State after will be captured by caller (post-execution)
    StateAfter = maps:get(state_after, TraceEvent, undefined),

    %% Build metadata
    Metadata = maps:get(metadata, TraceEvent, #{}),

    Event = #trace_event{
        step_seq = StepSeq,
        opcode = maps:get(opcode, TraceEvent),
        state_before = StateBefore,
        state_after = StateAfter,
        timestamp = Timestamp,
        scope = Scope,
        branch_id = BranchId,
        metadata = Metadata
    },

    %% Send to sink
    emit_to_sink(Event, State#trace_state.sink).

%% @private Extract branch_id for current token
-spec extract_branch_id(#exec_state{}) -> term() | undefined.
extract_branch_id(#exec_state{current_token = TokenId, branch_map = BranchMap}) ->
    case wf_exec:find_branch_for_token(TokenId, BranchMap) of
        {ok, BranchId} -> BranchId;
        error -> undefined
    end.

%% @private Emit event to sink
-spec emit_to_sink(#trace_event{}, trace_sink()) -> ok.
emit_to_sink(Event, {callback, Fun}) ->
    try
        Fun(Event),
        ok
    catch
        _:_ ->
            %% Log error, don't crash executor
            ok
    end;

emit_to_sink(Event, {ets, Table}) ->
    try
        ets:insert(Table, Event),
        ok
    catch
        _:_ ->
            %% Log error, don't crash executor
            ok
    end;

emit_to_sink(Event, {process, Pid}) ->
    try
        Pid ! {trace_event, Event},
        ok
    catch
        _:_ ->
            %% Log error, don't crash executor
            ok
    end;

emit_to_sink(_Event, {replay, _ReplayLog}) ->
    %% Replay sink: don't emit events (validate externally)
    ok.

%% @private Validate replay log format
-spec validate_replay_log(replay_log()) -> ok | no_return().
validate_replay_log([]) ->
    ok;
validate_replay_log([#replay_entry{} | Rest]) ->
    validate_replay_log(Rest);
validate_replay_log([_Invalid | _]) ->
    error({invalid_replay_log, bad_entry}).
