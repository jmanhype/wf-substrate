-module(wf_test_seq).
-export([
    mock_bytecode_seq_2_tasks/0,
    mock_bytecode_seq_3_tasks/0,
    mock_bytecode_nested_seq/0,
    mock_bytecode_seq_with_cancel/0,
    mock_bytecode_seq_empty/0
]).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").

%%====================================================================
%% Mock Bytecode Generators for Sequential Composition
%%====================================================================

%% Single sequence: Task A → Task B
mock_bytecode_seq_2_tasks() ->
    [
        {seq_enter, 0},
        {task_exec, task_a},
        {seq_next, 3},
        {task_exec, task_b},
        {done}
    ].

%% Single sequence: Task A → Task B → Task C
mock_bytecode_seq_3_tasks() ->
    [
        {seq_enter, 0},
        {task_exec, task_a},
        {seq_next, 3},
        {task_exec, task_b},
        {seq_next, 5},
        {task_exec, task_c},
        {done}
    ].

%% Nested sequences: Seq(Seq(A, B), C)
mock_bytecode_nested_seq() ->
    [
        {seq_enter, 0},
        {task_exec, task_a},
        {seq_next, 3},
        {seq_enter, 0},
        {task_exec, task_b},
        {seq_next, 7},
        {task_exec, task_c},
        {seq_next, 9},
        {task_exec, task_d},
        {done}
    ].

%% Sequence with cancellation
mock_bytecode_seq_with_cancel() ->
    [
        {cancel_scope, {enter, seq_scope}},
        {seq_enter, 0},
        {task_exec, task_a},
        {seq_next, 4},
        {task_exec, task_b},
        {cancel_scope, {exit, seq_scope}}
    ].

%% Empty sequence
mock_bytecode_seq_empty() ->
    [
        {seq_enter, 0},
        {done}
    ].

%%====================================================================
%% Unit Tests: Single Sequence
%%====================================================================

seq_2_tasks_executed_in_order_test() ->
    Bytecode = mock_bytecode_seq_2_tasks(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute SEQ_ENTER
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    ?assertEqual(1, wf_exec:get_ip(ExecState1)),
    %% Execute TASK_EXEC(task_a)
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    ?assertEqual(2, wf_exec:get_ip(ExecState2)),
    %% Execute SEQ_NEXT (jump to IP 3)
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),
    ?assertEqual(3, wf_exec:get_ip(ExecState3)),
    %% Execute TASK_EXEC(task_b)
    {ExecState4, _} = wf_exec:step(ExecState3, undefined),
    ?assertEqual(4, wf_exec:get_ip(ExecState4)),
    %% Execute DONE
    {ExecState5, _} = wf_exec:step(ExecState4, undefined),
    ?assert(wf_exec:is_done(ExecState5)),
    ?assertEqual(5, wf_exec:get_step_count(ExecState5)).

seq_3_tasks_executed_in_order_test() ->
    Bytecode = mock_bytecode_seq_3_tasks(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(7, wf_exec:get_step_count(ExecState1)). % enter + 3 tasks + 2 next + done

%%====================================================================
%% Unit Tests: Nested Sequences
%%====================================================================

nested_sequence_executes_correctly_test() ->
    Bytecode = mock_bytecode_nested_seq(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% enter, task_a, next, enter, task_b, next, task_c, next, task_d, done
    ?assertEqual(10, wf_exec:get_step_count(ExecState1)).

%%====================================================================
%% Unit Tests: Sequence with Cancellation
%%====================================================================

seq_cancellation_mid_sequence_test() ->
    Bytecode = mock_bytecode_seq_with_cancel(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until task_a completes
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % CANCEL_SCOPE enter
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % SEQ_ENTER
    {ExecState3, _} = wf_exec:step(ExecState2, undefined), % TASK_EXEC task_a
    {ExecState4, _} = wf_exec:step(ExecState3, undefined), % SEQ_NEXT
    %% Cancel mid-sequence
    ExecState5 = wf_exec:cancel(ExecState4),
    ?assertEqual(cancelled, ExecState5#exec_state.status),
    %% Token should be cancelled
    CurrentTokenId = ExecState5#exec_state.current_token,
    Token = maps:get(CurrentTokenId, ExecState5#exec_state.tokens),
    ?assertEqual(cancelled, Token#token.status).

%%====================================================================
%% Unit Tests: Empty Sequence
%%====================================================================

empty_sequence_completes_test() ->
    Bytecode = mock_bytecode_seq_empty(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(2, wf_exec:get_step_count(ExecState1)). % enter + done

%%====================================================================
%% Unit Tests: SEQ_NEXT Jump Target
%%====================================================================

seq_next_advances_to_correct_target_test() ->
    Bytecode = mock_bytecode_seq_3_tasks(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute to first SEQ_NEXT
    lists:foldl(fun(_, AccState) ->
        {NewState, _} = wf_exec:step(AccState, undefined),
        NewState
    end, ExecState0, [1, 2, 3]), % execute enter, task_a, next
    %% Verify IP is at task_b (IP 3)
    ?assertEqual(3, wf_exec:get_ip(ExecState0)).

%%====================================================================
%% Parameterized Tests
%%====================================================================

seq_n_tasks_test_() ->
    %% Generate test for N = 2, 3, 5 tasks
    lists:map(fun(N) ->
        {io_lib:format("seq_~p_tasks", [N]), fun() ->
            Bytecode = generate_seq_n_tasks(N),
            ExecState0 = wf_exec:new(Bytecode),
            {done, ExecState1} = wf_exec:run(ExecState0, 1000, undefined),
            ?assert(wf_exec:is_done(ExecState1))
        end}
    end, [2, 3, 5]).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Generate sequence with N tasks
generate_seq_n_tasks(N) ->
    TaskOpcodes = lists:flatten([[
        {task_exec, list_to_atom("task_" ++ integer_to_list(I))},
        {seq_next, 2*I + 1}
    ] || I <- lists:seq(1, N-1)]) ++ [{task_exec, list_to_atom("task_" ++ integer_to_list(N))}],
    [{seq_enter, 0} | TaskOpcodes] ++ [{done}].
