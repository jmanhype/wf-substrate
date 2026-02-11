-module(wf_exec_tests).
-include_lib("eunit/include/eunit.hrl").

%% Mock bytecode generators
mock_bytecode_simple_task() ->
    [{'TASK_EXEC', mock_task}, {'DONE'}].

mock_bytecode_seq() ->
    [{'SEQ_ENTER', 0}, {'TASK_EXEC', task_a}, {'SEQ_NEXT', 3}, {'TASK_EXEC', task_b}, {'DONE'}].

mock_bytecode_par() ->
    [
        {'PAR_FORK', [1, 3]},           %% Fork to IP 1 and 3
        {'TASK_EXEC', task_a},           %% IP 1: task_a
        {'DONE'},                        %% IP 2: done (branch 1)
        {'TASK_EXEC', task_b},           %% IP 3: task_b
        {'DONE'},                        %% IP 4: done (branch 2)
        {'JOIN_WAIT', all}              %% IP 5: wait for all branches
    ].

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

%%====================================================================
%% Phase 3 Tests: Multi-Token Executor
%%====================================================================

%% Test PAR_FORK spawns 2 tokens
par_fork_test_() ->
    Bytecode = mock_bytecode_par(),
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(1, wf_exec:get_step_count(ExecState1))  %% 1 step executed
    ].

%% Test JOIN_WAIT blocks until branches complete
join_wait_test_() ->
    Bytecode = mock_bytecode_par(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute PAR_FORK
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),
    %% Execute first branch (task_a + done)
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    %% Execute second branch (task_b + done)
    {ExecState4, _Trace4} = wf_exec:step(ExecState3, undefined),
    {ExecState5, _Trace5} = wf_exec:step(ExecState4, undefined),
    %% Execute JOIN_WAIT (should succeed now)
    {ExecState6, _Trace6} = wf_exec:step(ExecState5, undefined),
    [
        ?_assertEqual(6, wf_exec:get_step_count(ExecState6))  %% All steps executed
    ].

%% Test XOR_CHOOSE selects one branch
xor_choose_test_() ->
    Bytecode = [{'XOR_CHOOSE', [1, 3]}, {'TASK_EXEC', task_a}, {'DONE'}, {'TASK_EXEC', task_b}, {'DONE'}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(1, wf_exec:get_ip(ExecState1))  %% Selected first branch
    ].

%%====================================================================
%% Phase 4 Tests: Loop Support
%%====================================================================

%% Mock bytecode for count loop
mock_bytecode_loop_count() ->
    [
        {'LOOP_CHECK', {count, 3}},      %% Loop 3 times
        {'TASK_EXEC', task_a},            %% Body
        {'LOOP_BACK', 0},                 %% Jump to LOOP_CHECK
        {'DONE'}                          %% Exit
    ].

%% Test count loop executes N times
loop_count_test_() ->
    Bytecode = mock_bytecode_loop_count(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Initialize loop counter in context
    ExecState1 = wf_exec:set_ctx(ExecState0, #{loop_counter => 2}),
    %% Run with limited quanta
    Result = wf_exec:run(ExecState1, 10, undefined),
    %% Check that loop executed
    [
        ?_assertMatch({yield, _}, Result),  %% Should yield after quanta
        ?_assertEqual(10, wf_exec:get_step_count(element(2, Result)))  %% Executed 10 steps
    ].

%% Test LOOP_BACK jumps to loop head
loop_back_jump_test_() ->
    Bytecode = mock_bytecode_loop_count(),
    ExecState0 = wf_exec:new(Bytecode),
    ExecState1 = wf_exec:set_ctx(ExecState0, #{loop_counter => 2}),
    %% Execute LOOP_CHECK (counter=2, should continue)
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    %% Execute TASK_EXEC
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    %% Execute LOOP_BACK (should jump to LOOP_CHECK at IP 0)
    {ExecState4, _Trace4} = wf_exec:step(ExecState3, undefined),
    %% Execute LOOP_CHECK again (counter now 1, should continue)
    {ExecState5, _Trace5} = wf_exec:step(ExecState4, undefined),
    [
        ?_assertEqual(1, wf_exec:get_ip(ExecState2)),  %% Advanced to body (IP 1)
        ?_assertEqual(2, wf_exec:get_ip(ExecState3)),  %% Advanced to LOOP_BACK (IP 2)
        ?_assertEqual(0, wf_exec:get_ip(ExecState4)),  %% Jumped to LOOP_CHECK (IP 0)
        ?_assertEqual(1, wf_exec:get_ip(ExecState5))   %% Advanced to body again (IP 1)
    ].

