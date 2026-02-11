-module(wf_test_trace_helpers).
-include_lib("eunit/include/eunit.hrl").
-export([
    compare_traces/2,
    format_trace_diff/1,
    run_with_trace/3,
    extract_scheduler_choices/1
]).

-include("wf_trace.hrl").
-include("wf_exec.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Trace Comparison
%%====================================================================

%% @doc Compare two trace event lists, return ok | {error, Diff}
%% Compares step_seq, opcode, state_before, state_after
%% Excludes timestamp (always different) and metadata
-spec compare_traces([#trace_event{}], [#trace_event{}]) -> ok | {error, term()}.
compare_traces(Events1, Events2) ->
    case length(Events1) =:= length(Events2) of
        false ->
            {error, {length_mismatch, length(Events1), length(Events2)}};
        true ->
            compare_events(Events1, Events2, 1, [])
    end.

%% @doc Compare individual events, collect differences
compare_events([], [], _Index, Acc) ->
    case Acc of
        [] -> ok;
        _ -> {error, lists:reverse(Acc)}
    end;
compare_events([E1 | Rest1], [E2 | Rest2], Index, Acc) ->
    Diff = compare_event(E1, E2, Index),
    compare_events(Rest1, Rest2, Index + 1, Acc ++ Diff).

%% @doc Compare single event fields
compare_event(E1, E2, Index) ->
    Checks = [
        {step_seq, E1#trace_event.step_seq =:= E2#trace_event.step_seq},
        {opcode, E1#trace_event.opcode =:= E2#trace_event.opcode},
        {state_before, E1#trace_event.state_before =:= E2#trace_event.state_before},
        {state_after, E1#trace_event.state_after =:= E2#trace_event.state_after}
    ],
    FailedChecks = [{Field, Index} || {Field, Result} <- Checks, Result =:= false],
    FailedChecks.

%% @doc Format diff for readable output
-spec format_trace_diff({error, term()}) -> iolist().
format_trace_diff({error, {length_mismatch, Len1, Len2}}) ->
    io_lib:format("Trace length mismatch: expected ~p, got ~p~n", [Len1, Len2]);
format_trace_diff({error, DiffList}) ->
    ["Trace differences:~n" |
     [[io_lib:format("  Step ~p: field ~p mismatch~n", [Idx, Field]) || {Field, Idx} <- DiffList]]].

%%====================================================================
%% Workflow Execution with Trace Collection
%%====================================================================

%% @doc Execute workflow with trace collection
%% Returns {DoneState, TraceEvents}
%% Note: Currently the executor doesn't fully integrate with scheduler policy.
%% The SchedPolicy parameter is reserved for future use.
-spec run_with_trace(list(), wf_sched:sched_policy(), list()) ->
    {wf_exec:exec_state(), [#trace_event{}]}.
run_with_trace(Bytecode, _SchedPolicy, Options) ->
    TraceLevel = proplists:get_value(trace_level, Options, full),

    %% Create unique table name for concurrent execution
    TableName = list_to_atom("wf_trace_events_" ++ pid_to_list(self())),

    %% Create trace state with custom table
    Table = ets:new(TableName, [bag, public, {read_concurrency, true}]),
    TraceState = #trace_state{
        level = TraceLevel,
        sink = {ets, Table},
        case_id = undefined
    },

    %% Store trace state in process dictionary for wf_exec:step
    erlang:put(wf_trace_state, TraceState),

    ExecState = wf_exec:new(Bytecode),
    {done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

    %% Retrieve events before cleanup
    Events = wf_trace:get_events(TraceState),

    %% Clean up ETS table
    ets:delete(Table),

    %% Clean up process dictionary
    erlang:put(wf_trace_state, undefined),

    {DoneState, Events}.

%%====================================================================
%% Scheduler Choice Extraction
%%====================================================================

%% @doc Extract scheduler choice log from trace state
%% Note: Currently the executor doesn't log scheduler choices to trace metadata.
%% This function returns an empty list pending full scheduler integration.
-spec extract_scheduler_choices(wf_trace:trace_state()) -> wf_sched:choice_log().
extract_scheduler_choices(_TraceState) ->
    %% TODO: Extract scheduler choices from trace metadata once
    %% executor integrates scheduler policy (see wf_exec:run/3)
    [].

%%====================================================================
%% Unit Tests
%%====================================================================

%% @doc Test compare_traces with identical traces
compare_traces_identical_test() ->
    Event1 = #trace_event{
        step_seq = 1,
        opcode = {task_exec, task_a},
        state_before = <<>>,
        state_after = <<>>,
        timestamp = 1000,
        scope = [],
        branch_id = undefined,
        metadata = #{}
    },
    Event2 = #trace_event{
        step_seq = 2,
        opcode = {task_exec, task_b},
        state_before = <<>>,
        state_after = <<>>,
        timestamp = 2000,
        scope = [],
        branch_id = undefined,
        metadata = #{}
    },
    ?assertEqual(ok, compare_traces([Event1, Event2], [Event1, Event2])).

%% @doc Test compare_traces with different lengths
compare_traces_length_mismatch_test() ->
    Event1 = #trace_event{
        step_seq = 1,
        opcode = {task_exec, task_a},
        state_before = <<>>,
        state_after = <<>>,
        timestamp = 1000,
        scope = [],
        branch_id = undefined,
        metadata = #{}
    },
    Result = compare_traces([Event1], [Event1, Event1]),
    ?assertMatch({error, {length_mismatch, 1, 2}}, Result).

%% @doc Test compare_traces with different opcodes
compare_traces_opcode_mismatch_test() ->
    Event1 = #trace_event{
        step_seq = 1,
        opcode = {task_exec, task_a},
        state_before = <<>>,
        state_after = <<>>,
        timestamp = 1000,
        scope = [],
        branch_id = undefined,
        metadata = #{}
    },
    Event2 = #trace_event{
        step_seq = 1,
        opcode = {task_exec, task_b},  %% Different opcode
        state_before = <<>>,
        state_after = <<>>,
        timestamp = 1000,
        scope = [],
        branch_id = undefined,
        metadata = #{}
    },
    Result = compare_traces([Event1], [Event2]),
    ?assertMatch({error, [{opcode, 1}]}, Result).

%% @doc Test format_trace_diff with length mismatch
format_trace_diff_length_test() ->
    Diff = {error, {length_mismatch, 5, 3}},
    Formatted = format_trace_diff(Diff),
    ?assert(is_list(Formatted)).

%% @doc Test format_trace_diff with field differences
format_trace_diff_fields_test() ->
    Diff = {error, [{opcode, 1}, {state_before, 2}]},
    Formatted = format_trace_diff(Diff),
    ?assert(is_list(Formatted)).

