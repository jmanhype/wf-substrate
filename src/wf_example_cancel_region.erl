-module(wf_example_cancel_region).
-export([term/0, run/0, expected_trace_structure/0]).
-include_lib("kernel/include/logger.hrl").
-include("wf_trace.hrl").

-define(exec_state, exec_state).  % Opaque record placeholder

%% @doc Returns the cancel region workflow bytecode term
%% Pattern: seq(task(start_process), cancel_region(seq(task(long_task), task(another_long_task))), task(continue_main))
%% Demonstrates: CANCEL_SCOPE with enter/exit, SEQ_ENTER, SEQ_NEXT, TASK_EXEC
%%
%% CANCEL_SCOPE marks region boundaries:
%% 1. Enter main cancellation scope
%% 2. Start main process
%% 3. Enter cancelable region
%% 4. Execute long-running subprocess (would be cancelled if signal received)
%% 5. Exit cancel region
%% 6. Continue main flow (should execute even if region cancelled)
%% 7. Exit main scope
%%
%% LIMITATION: Cancellation is stubbed in wf_exec.erl:512-529 (awaiting item 008)
%% This example shows the bytecode structure but won't actually cancel at runtime.
%% The workflow will execute completely, ignoring the cancel region semantics.
term() ->
    [
        {'CANCEL_SCOPE', {enter, main_flow}},     % Enter main cancellation scope
        {'TASK_EXEC', start_process},             % Start main process
        {'CANCEL_SCOPE', {enter, cancel_region}}, % Enter cancelable region
        {'SEQ_ENTER', 0},                          % Start sequential subprocess
        {'TASK_EXEC', long_task},                  % Long-running task (would be cancelled)
        {'SEQ_NEXT', 7},                           % Jump to next task
        {'TASK_EXEC', another_long_task},          % Another long task (would be cancelled)
        {'DONE'},                                  % End subprocess
        {'CANCEL_SCOPE', {exit, cancel_region}},   % Exit cancel region
        {'TASK_EXEC', continue_main_flow},         % Continue main flow (should execute after cancel)
        {'CANCEL_SCOPE', {exit, main_flow}}       % Exit main scope
    ].

%% @doc Executes the workflow and returns execution summary
%% Note: Tracing not yet integrated into executor (awaiting item 010)
run() ->
    Bytecode = term(),
    ExecState = wf_exec:new(Bytecode),
    {done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

    %% Print execution summary
    Steps = wf_exec:get_step_count(DoneState),
    io:format("~n=== Cancel Region Workflow ===~n"),
    io:format("Steps executed: ~p~n", [Steps]),
    io:format("Status: ~p~n", [wf_exec:is_done(DoneState)]),
    io:format("~nPattern: Cancel Region~n"),
    io:format("  1. Enter main cancellation scope~n"),
    io:format("  2. Start main process~n"),
    io:format("  3. Enter cancelable region~n"),
    io:format("  4. Execute long-running subprocess~n"),
    io:format("  5. Exit cancel region~n"),
    io:format("  6. Continue main flow~n"),
    io:format("  7. Exit main scope~n"),
    io:format("~nLIMITATION: Cancellation is stubbed (wf_exec.erl:512-529)~n"),
    io:format("The bytecode structure is correct, but actual cancellation not yet implemented.~n"),

    {ok, executed}.

%% @doc Expected sequence of opcode categories
%% NOTE: Since tracing is not integrated, this returns a placeholder
expected_trace_structure() ->
    [
        cancel_scope_enter,  % Enter main scope
        task_exec,           % start_process
        cancel_scope_enter,  % Enter cancel region
        seq_enter,           % Start subprocess
        task_exec,           % long_task
        seq_next,            % Jump to next task
        task_exec,           % another_long_task
        done,                % End subprocess
        cancel_scope_exit,   % Exit cancel region
        task_exec,           % continue_main_flow
        cancel_scope_exit    % Exit main scope
    ].
