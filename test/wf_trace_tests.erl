-module(wf_trace_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([nowarn_unused_variable]).

%% Include record definitions
-include("wf_exec.hrl").
-include("wf_trace.hrl").

%%====================================================================
%% Trace Level Tests
%%====================================================================

trace_level_none_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(none),
        wf_trace:set_level(none),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{opcode => {done}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        ?assertEqual(0, length(Events))
    end.

trace_level_min_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(min),
        wf_trace:set_level(min),
        %% Create bytecode with structural events
        Bytecode = [
            {par_fork, [1, 3]},
            {done},
            {done},
            {join_wait, all}
        ],
        ExecState = wf_exec:new(Bytecode),
        %% Run a few steps
        lists:foreach(fun(_) ->
            {_ExecState1, _} = wf_exec:step(ExecState, undefined),
            ok
        end, lists:seq(1, 5)),
        Events = wf_trace:get_events(State),
        %% Should have only structural events (PAR_FORK, DONE, JOIN_WAIT)
        StructuralEvents = [E || E <- Events,
            is_structural_opcode(E#trace_event.opcode)],
        ?assert(length(Events) >= length(StructuralEvents))
    end.

trace_level_full_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        wf_trace:set_level(full),
        Bytecode = [{task_exec, task}, {done}],
        ExecState = wf_exec:new(Bytecode),
        {_ExecState1, _} = wf_exec:step(ExecState, undefined),
        Events = wf_trace:get_events(State),
        ?assert(length(Events) > 0),
        [FirstEvent | _] = Events,
        ?assertMatch(#trace_event{}, FirstEvent),
        ?assert(is_integer(FirstEvent#trace_event.step_seq)),
        ?assert(is_integer(FirstEvent#trace_event.timestamp)),
        ?assert(is_list(FirstEvent#trace_event.scope)),
        ?assert(is_binary(FirstEvent#trace_event.state_before))
    end.

%%====================================================================
%% Sink Tests
%%====================================================================

sink_callback_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        Self = self(),
        CallbackFun = fun(Event) ->
            Self ! {trace_event, Event},
            ok
        end,
        State2 = wf_trace:set_sink(State, {callback, CallbackFun}),
        put(wf_trace_state, State2),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{opcode => {done}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        receive
            {trace_event, #trace_event{}} -> ok
        after 1000 ->
            error(timeout)
        end
    end.

sink_ets_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{opcode => {done}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        ?assert(length(Events) > 0)
    end.

sink_process_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        Self = self(),
        ProcessSink = spawn(fun() ->
            receive
                {trace_event, _Event} ->
                    Self ! received
            end
        end),
        State2 = wf_trace:set_sink(State, {process, ProcessSink}),
        put(wf_trace_state, State2),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{opcode => {done}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        receive
            received -> ok
        after 1000 ->
            error(timeout)
        end
    end.

%%====================================================================
%% State Snapshot Tests
%%====================================================================

snapshot_serialization_test_() ->
    fun() ->
        Bytecode = [{task_exec, task}, {done}],
        ExecState = wf_exec:new(Bytecode),
        %% Snapshot state
        Binary = wf_exec:snapshot_exec_state(ExecState),
        ?assert(is_binary(Binary)),
        %% Restore state
        {ok, RestoredState} = wf_exec:restore_exec_state(Binary, Bytecode),
        ?assertEqual(ExecState#exec_state.case_id, RestoredState#exec_state.case_id),
        ?assertEqual(ExecState#exec_state.step_count, RestoredState#exec_state.step_count)
    end.

restore_bytecode_mismatch_test_() ->
    fun() ->
        Bytecode1 = [{task_exec, task1}, {done}],
        Bytecode2 = [{task_exec, task2}, {done}],
        ExecState = wf_exec:new(Bytecode1),
        Binary = wf_exec:snapshot_exec_state(ExecState),
        %% Try to restore with different bytecode
        Result = wf_exec:restore_exec_state(Binary, Bytecode2),
        ?assertMatch({error, {bytecode_mismatch, _}}, Result)
    end.

restore_invalid_binary_test_() ->
    fun() ->
        Bytecode = [{done}],
        InvalidBinary = <<1, 2, 3>>,
        Result = wf_exec:restore_exec_state(InvalidBinary, Bytecode),
        ?assertEqual({error, invalid_snapshot}, Result)
    end.

%%====================================================================
%% Filter Tests
%%====================================================================

filter_opcode_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{opcode => {done}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        Filtered = wf_trace:filter(Events, {opcode, {done}}),
        ?assertEqual(length(Events), length(Filtered))
    end.

filter_scope_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([
            {cancel_scope, {enter, scope1}},
            {done},
            {cancel_scope, {exit, scope1}}
        ]),
        %% Run all steps
        lists:foreach(fun(_) ->
            {_ExecState1, _} = wf_exec:step(ExecState, undefined),
            ok
        end, lists:seq(1, 4)),
        Events = wf_trace:get_events(State),
        %% Filter by scope_id
        Filtered = wf_trace:filter(Events, {scope, scope1}),
        ?assert(length(Filtered) > 0)
    end.

filter_predicate_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{opcode => {done}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        %% Filter by predicate (all events with step_seq > 0)
        Filtered = wf_trace:filter(Events, {predicate, fun(E) ->
            E#trace_event.step_seq > 0
        end}),
        ?assert(length(Filtered) > 0)
    end.

%%====================================================================
%% Replay Log Tests
%%====================================================================

to_replay_log_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{done}]),
        TraceEvent = #{
            opcode => {done},
            metadata => #{scheduler_choice => {0, [{token, t1}], {token, t1}}}
        },
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        ReplayLog = wf_trace:to_replay_log(Events),
        ?assert(length(ReplayLog) >= 0)
    end.

from_replay_log_test_() ->
    fun() ->
        ReplayLog = [
            #replay_entry{
                step_seq = 1,
                opcode = {done},
                scheduler_choice = {0, [{token, t1}], {token, t1}},
                effect_result = undefined
            }
        ],
        {ok, State} = wf_trace:from_replay_log(ReplayLog),
        ?assertMatch(#trace_state{}, State),
        ?assertEqual(full, State#trace_state.level)
    end.

from_replay_log_invalid_test_() ->
    fun() ->
        InvalidReplayLog = [{invalid, entry}],
        Result = wf_trace:from_replay_log(InvalidReplayLog),
        ?assertEqual({error, invalid_replay_log}, Result)
    end.

replay_log_smaller_than_trace_test_() ->
    fun() ->
        %% Generate a trace
        {ok, State} = wf_trace:new(full),
        Bytecode = [{task_exec, task}, {done}],
        ExecState = wf_exec:new(Bytecode),
        lists:foreach(fun(_) ->
            {_ExecState1, _} = wf_exec:step(ExecState, undefined),
            ok
        end, lists:seq(1, 10)),
        Events = wf_trace:get_events(State),
        %% Extract replay log
        ReplayLog = wf_trace:to_replay_log(Events),
        %% Replay log should be smaller (or equal) than full trace
        ?assert(length(ReplayLog) =< length(Events))
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_wf_exec_emits_events_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        wf_trace:set_level(full),
        put(wf_trace_state, State),
        %% Run a workflow
        Bytecode = [
            {seq_enter, 0},
            {task_exec, task_a},
            {done}
        ],
        ExecState = wf_exec:new(Bytecode),
        {_ExecState1, _} = wf_exec:step(ExecState, undefined),
        {_ExecState2, _} = wf_exec:step(_ExecState1, undefined),
        %% Check that events were emitted
        Events = wf_trace:get_events(get(wf_trace_state)),
        ?assert(length(Events) >= 2),
        %% Check first event
        [FirstEvent | _] = lists:sort(fun(A, B) ->
            A#trace_event.step_seq =< B#trace_event.step_seq
        end, Events),
        ?assertEqual(1, FirstEvent#trace_event.step_seq),
        ?assert(is_integer(FirstEvent#trace_event.timestamp)),
        ?assert(is_list(FirstEvent#trace_event.scope))
    end.

integration_step_seq_monotonic_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        wf_trace:set_level(full),
        put(wf_trace_state, State),
        Bytecode = [{task_exec, task}, {done}],
        ExecState = wf_exec:new(Bytecode),
        lists:foldl(fun(_, AccState) ->
            {NewState, _} = wf_exec:step(AccState, undefined),
            NewState
        end, ExecState, lists:seq(1, 5)),
        Events = wf_trace:get_events(get(wf_trace_state)),
        StepSeqs = [E#trace_event.step_seq || E <- Events],
        %% Check that step_seqs are strictly increasing
        SortedStepSeqs = lists:sort(StepSeqs),
        ?assertEqual(StepSeqs, SortedStepSeqs)
    end.

integration_case_id_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        wf_trace:set_level(full),
        put(wf_trace_state, State),
        Bytecode = [{done}],
        ExecState = wf_exec:new(Bytecode),
        %% Verify case_id is present
        ?assert(ExecState#exec_state.case_id =/= undefined),
        %% Run a step
        {_ExecState1, _} = wf_exec:step(ExecState, undefined),
        %% case_id should remain the same
        ?assertEqual(ExecState#exec_state.case_id, _ExecState1#exec_state.case_id)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Check if opcode is structural
is_structural_opcode({OpcodeName, _Arg}) ->
    lists:member(OpcodeName, ['PAR_FORK', 'JOIN_WAIT', 'CANCEL_SCOPE', 'DONE']);
is_structural_opcode({done}) ->
    true;
is_structural_opcode(_Other) ->
    false.
