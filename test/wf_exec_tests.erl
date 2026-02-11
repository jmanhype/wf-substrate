-module(wf_exec_tests).
-include_lib("eunit/include/eunit.hrl").

%% Mock bytecode generators
mock_bytecode_simple_task() ->
    [{'TASK_EXEC', mock_task}, {'DONE'}].

mock_bytecode_seq() ->
    [{'SEQ_ENTER', 0}, {'TASK_EXEC', task_a}, {'SEQ_NEXT', 3}, {'TASK_EXEC', task_b}, {'DONE'}].

%%====================================================================
%% Phase 1 Tests: exec_state Creation
%%====================================================================

new_test_() ->
    Bytecode = mock_bytecode_simple_task(),
    ExecState = wf_exec:new(Bytecode),
    [
        ?_assertEqual(0, wf_exec:get_ip(ExecState)),
        ?_assertEqual(#{}, wf_exec:get_ctx(ExecState)),
        ?_assertEqual(0, wf_exec:get_step_count(ExecState))
    ].

%%====================================================================
%% Phase 2 Tests: Single-Token Executor
%%====================================================================

%% Test single task execution
single_task_test_() ->
    Bytecode = mock_bytecode_simple_task(),
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(1, wf_exec:get_ip(ExecState1)),
        ?_assertEqual(1, wf_exec:get_step_count(ExecState1)),
        ?_assertNot(wf_exec:is_done(ExecState1))
    ].

%% Test DONE opcode
done_test_() ->
    Bytecode = [{'DONE'}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assert(wf_exec:is_done(ExecState1)),
        ?_assertNot(wf_exec:is_blocked(ExecState1))
    ].

%% Test sequence execution
sequence_test_() ->
    Bytecode = mock_bytecode_seq(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute SEQ_ENTER
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),
    %% Execute TASK_EXEC(task_a)
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    %% Execute SEQ_NEXT (jump to IP 3)
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    [
        ?_assertEqual(3, wf_exec:get_ip(ExecState3)),
        ?_assertEqual(3, wf_exec:get_step_count(ExecState3))
    ].

%% Test quanta execution and yielding
quanta_yield_test_() ->
    Bytecode = mock_bytecode_seq(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Run with Quanta=2, should yield after 2 steps
    Result = wf_exec:run(ExecState0, 2, undefined),
    [
        ?_assertMatch({yield, _ExecState}, Result),
        ?_assertEqual(2, wf_exec:get_step_count(element(2, Result)))
    ].

%% Test run until done
run_until_done_test_() ->
    Bytecode = [{'TASK_EXEC', task}, {'DONE'}],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 100, undefined),
    [
        ?_assertMatch({done, _ExecState}, Result),
        ?_assert(wf_exec:is_done(element(2, Result)))
    ].

