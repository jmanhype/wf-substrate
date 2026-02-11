%%%-------------------------------------------------------------------
%%% @doc Per-case workflow runner as gen_statem
%%%
%%% Manages the lifecycle of a single workflow case execution:
%%% - initializing: Setup exec_state, sched_state, trace_state
%%% - running: Execute bytecode quanta, check for done/blocked
%%% - waiting_effect: Await effect response from external system
%%% - waiting_signal: Await signal via wf_substrate:signal/2
%%% - cancelled: Cleanup and terminate
%%% - done: Return result and terminate
%%%
%%% State timeout support for overall case timeout.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_case_runner).
-behaviour(gen_statem).

%% API
-export([start_link/3]).
-export([signal/2, cancel/1, cancel_region/2, set_trace/2, status/1]).
-export([wf_case_pid_name/1]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4,
    initializing/3,
    running/3,
    waiting_effect/3,
    waiting_signal/3,
    cancelled/3,
    done/3
]).

%% Include record definitions
-include("wf_exec.hrl").
-include("wf_trace.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(state_data, {
    case_id :: term(),
    exec_state :: wf_exec:exec_state() | undefined,
    sched_state :: wf_sched:sched_state() | undefined,
    trace_state :: #trace_state{} | undefined,
    options :: map(),
    caller_pid :: pid() | undefined,
    result :: term() | undefined
}).

%%====================================================================
%% Types
%%====================================================================

-type state_data() :: #state_data{}.
-type state_name() :: initializing | running | waiting_effect | waiting_signal | cancelled | done.

-export_type([state_data/0, state_name/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Start case runner with CaseId, Bytecode, Options
-spec start_link(term(), wf_vm:wf_bc(), map()) -> {ok, pid()}.
start_link(CaseId, Bytecode, Options) ->
    gen_statem:start_link(?MODULE, [CaseId, Bytecode, Options], []).

%% @doc Send signal to running case
-spec signal(pid(), term()) -> ok.
signal(Pid, Signal) ->
    gen_statem:cast(Pid, {signal, Signal}).

%% @doc Cancel running case
-spec cancel(pid()) -> ok.
cancel(Pid) ->
    gen_statem:cast(Pid, cancel).

%% @doc Cancel specific region within case
-spec cancel_region(pid(), term()) -> ok.
cancel_region(Pid, ScopeId) ->
    gen_statem:cast(Pid, {cancel_region, ScopeId}).

%% @doc Set trace configuration
-spec set_trace(pid(), {wf_trace:trace_level(), wf_trace:trace_sink()}) -> ok.
set_trace(Pid, {Level, Sink}) ->
    gen_statem:cast(Pid, {set_trace, Level, Sink}).

%% @doc Get case status
-spec status(pid()) -> {ok, state_name(), map()}.
status(Pid) ->
    gen_statem:call(Pid, status).

%% @doc Generate registered name for case PID
-spec wf_case_pid_name(term()) -> atom().
wf_case_pid_name(CaseId) when is_atom(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId));
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ lists:flatten(io_lib:format("~p", [CaseId]))).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

%% @private State functions mode for explicit state modeling
callback_mode() -> state_functions.

%% @private Initialize gen_statem
init([CaseId, Bytecode, Options]) ->
    %% Extract options with defaults
    StepQuanta = maps:get(step_quanta, Options, 100),
    Timeout = maps:get(timeout, Options, 5000),
    TraceLevel = maps:get(trace_level, Options, none),
    SchedPolicy = maps:get(scheduler_policy, Options, deterministic),

    %% Create initial exec_state from bytecode
    ExecState0 = wf_exec:new(Bytecode),

    %% Set case_id in exec_state
    ExecState1 = ExecState0#exec_state{case_id = CaseId},

    %% Create scheduler state
    {ok, SchedState} = wf_sched:new(SchedPolicy, []),

    %% Create trace state
    {ok, TraceState} = wf_trace:new(TraceLevel),
    TraceState1 = TraceState#trace_state{case_id = CaseId},

    %% Store trace state in process dictionary for wf_trace:emit/2
    put(wf_trace_state, TraceState1),

    StateData = #state_data{
        case_id = CaseId,
        exec_state = ExecState1,
        sched_state = SchedState,
        trace_state = TraceState1,
        options = #{
            step_quanta => StepQuanta,
            timeout => Timeout,
            scheduler_policy => SchedPolicy
        },
        caller_pid = undefined,
        result = undefined
    },

    %% Transition to running state (initializing complete)
    {ok, running, StateData, [{state_timeout, Timeout, overall_timeout}]}.

%% @private Initializing state: setup complete, transition to running
initializing(state_timeout, initialization_timeout, StateData) ->
    %% Setup complete, transition to running
    Timeout = maps:get(timeout, StateData#state_data.options, 5000),
    {next_state, running, StateData, [{state_timeout, Timeout, overall_timeout}]};

initializing(cast, {set_trace, Level, Sink}, StateData) ->
    %% Update trace configuration
    TraceState0 = StateData#state_data.trace_state,
    TraceState1 = TraceState0#trace_state{level = Level},
    TraceState2 = wf_trace:set_sink(TraceState1, Sink),
    put(wf_trace_state, TraceState2),
    {next_state, initializing, StateData#state_data{trace_state = TraceState2}};

initializing(info, Msg, _StateData) ->
    log_unexpected_message(initializing, Msg),
    {keep_state_and_data};

initializing(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, initializing, StateData).

%% @private Running state: execute bytecode quanta
running(cast, {signal, Signal}, StateData) ->
    %% Handle external signal
    ExecState0 = StateData#state_data.exec_state,
    Ctx0 = ExecState0#exec_state.ctx,
    Ctx1 = maps:put(last_signal, Signal, Ctx0),
    ExecState1 = ExecState0#exec_state{ctx = Ctx1},
    {next_state, running, execute_quantum(StateData#state_data{exec_state = ExecState1})};

running(cast, cancel, StateData) ->
    %% Cancel entire case
    ExecState0 = StateData#state_data.exec_state,
    ExecState1 = ExecState0#exec_state{status = cancelled},
    notify_done(StateData#state_data.caller_pid, {error, cancelled}),
    {next_state, cancelled, StateData#state_data{exec_state = ExecState1}};

running(cast, {cancel_region, ScopeId}, StateData) ->
    %% Cancel specific region (emit trace event, continue execution)
    %% TODO: Implement region cancellation logic
    TraceState0 = StateData#state_data.trace_state,
    wf_trace:emit(TraceState0, region_cancel, #{scope_id => ScopeId}),
    {next_state, running, StateData};

running(cast, {set_trace, Level, Sink}, StateData) ->
    %% Update trace configuration
    TraceState0 = StateData#state_data.trace_state,
    TraceState1 = TraceState0#trace_state{level = Level},
    TraceState2 = wf_trace:set_sink(TraceState1, Sink),
    put(wf_trace_state, TraceState2),
    {next_state, running, StateData#state_data{trace_state = TraceState2}};

running({call, From}, status, StateData) ->
    %% Return current status
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => running,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status
    },
    {keep_state_and_data, [{reply, From, {ok, running, StatusInfo}}]};

running(state_timeout, overall_timeout, StateData) ->
    %% Overall case timeout
    notify_done(StateData#state_data.caller_pid, {error, timeout}),
    {next_state, cancelled, StateData};

running(info, Msg, _StateData) ->
    %% Handle unexpected info messages
    log_unexpected_message(running, Msg),
    {keep_state_and_data};

running(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, running, StateData).

%% @private Waiting for effect response
waiting_effect(cast, {effect_response, _Result}, StateData) ->
    %% Resume execution with effect result
    {next_state, running, execute_quantum(StateData)};

waiting_effect(cast, cancel, StateData) ->
    %% Cancel while waiting for effect
    notify_done(StateData#state_data.caller_pid, {error, cancelled}),
    {next_state, cancelled, StateData};

waiting_effect(state_timeout, overall_timeout, StateData) ->
    %% Timeout while waiting for effect
    notify_done(StateData#state_data.caller_pid, {error, timeout}),
    {next_state, cancelled, StateData};

waiting_effect(info, Msg, _StateData) ->
    log_unexpected_message(waiting_effect, Msg),
    {keep_state_and_data};

waiting_effect(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, waiting_effect, StateData).

%% @private Waiting for external signal
waiting_signal(cast, {signal, Signal}, StateData) ->
    %% Signal received, resume execution
    ExecState0 = StateData#state_data.exec_state,
    Ctx0 = ExecState0#exec_state.ctx,
    Ctx1 = maps:put(last_signal, Signal, Ctx0),
    ExecState1 = ExecState0#exec_state{ctx = Ctx1},
    {next_state, running, execute_quantum(StateData#state_data{exec_state = ExecState1})};

waiting_signal(cast, cancel, StateData) ->
    %% Cancel while waiting for signal
    notify_done(StateData#state_data.caller_pid, {error, cancelled}),
    {next_state, cancelled, StateData};

waiting_signal(state_timeout, overall_timeout, StateData) ->
    %% Timeout while waiting for signal
    notify_done(StateData#state_data.caller_pid, {error, timeout}),
    {next_state, cancelled, StateData};

waiting_signal(info, Msg, _StateData) ->
    log_unexpected_message(waiting_signal, Msg),
    {keep_state_and_data};

waiting_signal(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, waiting_signal, StateData).

%% @private Cancelled state: cleanup and terminate
cancelled(cast, cancel, StateData) ->
    %% Already cancelled
    {stop, normal, StateData};

cancelled({call, From}, status, _StateData) ->
    %% Return cancelled status
    StatusInfo = #{state => cancelled},
    {keep_state_and_data, [{reply, From, {ok, cancelled, StatusInfo}}]};

cancelled(info, Msg, _StateData) ->
    log_unexpected_message(cancelled, Msg),
    {keep_state_and_data};

cancelled(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, cancelled, StateData).

%% @private Done state: return result and terminate
done({call, From}, status, StateData) ->
    %% Return done status with result
    StatusInfo = #{state => done, result => StateData#state_data.result},
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]};

done(info, Msg, _StateData) ->
    log_unexpected_message(done, Msg),
    {keep_state_and_data};

done(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, done, StateData).

%% @private Common event handler for all states
handle_common_event({call, From}, status, CurrentState, StateData) ->
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => CurrentState,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status
    },
    {keep_state_and_data, [{reply, From, {ok, CurrentState, StatusInfo}}]};

handle_common_event(_EventType, _EventContent, _CurrentState, _StateData) ->
    {keep_state_and_data}.

%% @private gen_statem terminate
terminate(_Reason, _StateName, #state_data{case_id = CaseId}) ->
    %% Unregister from registry
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined -> ok;
        _Pid -> unregister(PidName)
    end,
    ok.

%% @private gen_statem code_change
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute one quantum of bytecode (N steps or until terminal)
-spec execute_quantum(state_data()) -> state_data().
execute_quantum(StateData) ->
    ExecState0 = StateData#state_data.exec_state,
    SchedState = StateData#state_data.sched_state,
    StepQuanta = maps:get(step_quanta, StateData#state_data.options),

    %% Run quantum
    Result = wf_exec:run(ExecState0, StepQuanta, SchedState),

    case Result of
        {done, ExecState1} ->
            %% Execution complete
            case ExecState1#exec_state.status of
                done ->
                    %% Normal completion
                    Result1 = ExecState1#exec_state.ctx,
                    notify_done(StateData#state_data.caller_pid, {ok, Result1}),
                    StateData1 = StateData#state_data{exec_state = ExecState1, result = Result1},
                    enter_state(done, StateData1);
                cancelled ->
                    %% Cancelled
                    StateData#state_data{exec_state = ExecState1};
                _ ->
                    %% Other terminal status
                    notify_done(StateData#state_data.caller_pid, {error, ExecState1#exec_state.status}),
                    enter_state(done, StateData#state_data{exec_state = ExecState1})
            end;
        {yield, ExecState1} ->
            %% Quantum exhausted, check if blocked
            case wf_exec:is_blocked(ExecState1) of
                true ->
                    %% Determine what we're waiting for
                    case ExecState1#exec_state.status of
                        blocked_effect ->
                            Timeout = maps:get(timeout, StateData#state_data.options, 5000),
                            StateData1 = StateData#state_data{exec_state = ExecState1},
                            {next_state, waiting_effect, StateData1, [{state_timeout, Timeout, overall_timeout}]};
                        blocked_signal ->
                            Timeout = maps:get(timeout, StateData#state_data.options, 5000),
                            StateData1 = StateData#state_data{exec_state = ExecState1},
                            {next_state, waiting_signal, StateData1, [{state_timeout, Timeout, overall_timeout}]};
                        _ ->
                            %% Other blocked state, continue running
                            StateData#state_data{exec_state = ExecState1}
                    end;
                false ->
                    %% Not blocked, continue running
                    StateData#state_data{exec_state = ExecState1}
            end
    end.

%% @doc Transition to new state
enter_state(TargetState, StateData) ->
    %% Set state timeout if not done/cancelled
    case TargetState of
        done ->
            StateData;
        cancelled ->
            StateData;
        _Other ->
            StateData
    end.

%% @doc Notify caller of completion
-spec notify_done(pid() | undefined, term()) -> ok.
notify_done(undefined, _Result) ->
    ok;
notify_done(CallerPid, Result) when is_pid(CallerPid) ->
    CallerPid ! {case_completed, Result},
    ok.

%% @doc Log unexpected messages
log_unexpected_message(StateName, Msg) ->
    %% Use logger for unexpected messages
    case element(1, Msg) of
        '$gen_' -> ok;  %% Ignore gen_statem internal messages
        _ -> logger:warning("[wf_case_runner] Unexpected message in state ~p: ~p", [StateName, Msg])
    end.
