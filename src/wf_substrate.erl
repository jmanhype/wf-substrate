%%%-------------------------------------------------------------------
%%% @doc Public API for wf_substrate
%%%
%%% This module provides the main entry point for the workflow pattern
%%% substrate. It exposes functions for creating cases, sending signals,
%%% cancelling cases, awaiting results, querying status, and configuring
%%% tracing.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_substrate).

%% Public API
-export([
    new_case/3,
    signal/2,
    cancel/1,
    cancel_region/2,
    await/2,
    status/1,
    trace/3,
    validate/2
]).

%% Include records
-include("wf_exec.hrl").

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: term().
-type signal() :: term().
-type wf_term() :: wf_vm:wf_bc().  %% For now, accept bytecode directly
-type ctx() :: map().
-type options() :: #{
    step_quanta => pos_integer(),
    timeout => pos_integer(),
    trace_level => wf_trace:trace_level(),
    scheduler_policy => wf_sched:sched_policy()
}.

-type case_status() :: #{
    state => atom(),
    ip => non_neg_integer(),
    step_count => non_neg_integer(),
    status => atom()
}.

-export_type([case_id/0, case_status/0, options/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new workflow case
%% CaseId: Unique identifier for the case
%% Term: Workflow bytecode (wf_vm:wf_bc()) - TODO: wf_term() in item 004
%% Ctx: Initial context map
-spec new_case(case_id(), wf_term(), ctx()) -> {ok, pid()} | {error, term()}.
new_case(CaseId, Bytecode, Ctx) when is_list(Bytecode), is_map(Ctx) ->
    %% TODO: Item 004 will call wf_compile:compile(Term) here
    %% For now, accept bytecode directly
    Options = #{},
    case wf_case_sup:start_case(CaseId, Bytecode, Options) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Send signal to running case
-spec signal(case_id(), signal()) -> ok | {error, term()}.
signal(CaseId, Signal) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:signal(Pid, Signal),
            ok
    end.

%% @doc Cancel entire case
-spec cancel(case_id()) -> ok | {error, term()}.
cancel(CaseId) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:cancel(Pid),
            ok
    end.

%% @doc Cancel specific region within case
-spec cancel_region(case_id(), term()) -> ok | {error, term()}.
cancel_region(CaseId, ScopeId) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:cancel_region(Pid, ScopeId),
            ok
    end.

%% @doc Await case completion with timeout
-spec await(case_id(), pos_integer() | infinity) -> {ok, term()} | {error, term()}.
await(CaseId, Timeout) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            %% Monitor case process
            Ref = monitor(process, Pid),
            receive
                {case_completed, Result} ->
                    demonitor(Ref, [flush]),
                    Result;
                {'DOWN', Ref, process, Pid, Reason} ->
                    {error, Reason}
            after Timeout ->
                demonitor(Ref, [flush]),
                {error, timeout}
            end
    end.

%% @doc Get current case status
-spec status(case_id()) -> {ok, case_status()} | {error, term()}.
status(CaseId) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            try wf_case_runner:status(Pid) of
                {ok, _State, StatusInfo} ->
                    {ok, StatusInfo}
            catch
                _:_ ->
                    {error, status_unavailable}
            end
    end.

%% @doc Configure tracing for a case
-spec trace(case_id(), wf_trace:trace_level(), wf_trace:trace_sink()) -> ok | {error, term()}.
trace(CaseId, Level, Sink) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:set_trace(Pid, {Level, Sink}),
            ok
    end.

%% @doc Validate workflow term (stub - item 013 will implement)
-spec validate(wf_term(), options()) -> ok | {error, term()}.
validate(_Bytecode, _Options) ->
    %% TODO: Implement in item 013
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate registered name for case PID
-spec wf_case_pid_name(term()) -> atom().
wf_case_pid_name(CaseId) when is_atom(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId));
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ lists:flatten(io_lib:format("~p", [CaseId]))).
