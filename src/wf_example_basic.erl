-module(wf_example_basic).
-export([term/0, run/0, expected_trace_structure/0]).
-include_lib("kernel/include/logger.hrl").
-include("wf_trace.hrl").

-define(exec_state, exec_state).  % Opaque record placeholder

%% @doc Returns the vending machine workflow bytecode term
%% Pattern: seq(task(insert_coin), xor([seq(task(select_drink), task(dispense)), task(refund)]))
%% Demonstrates: SEQ_ENTER, SEQ_NEXT, XOR_CHOOSE, TASK_EXEC, DONE
%%
%% This workflow models a simple vending machine:
%% 1. User inserts a coin
%% 2. User either selects a drink (which gets dispensed) OR requests a refund
%% 3. Workflow completes
%%
%% The XOR_CHOOSE opcode implements exclusive choice - only one branch executes.
term() ->
    [
        {'SEQ_ENTER', 0},          % Start sequential scope (outer sequence)
        {'TASK_EXEC', insert_coin}, % Task: insert coin
        {'XOR_CHOOSE', [3, 7]},    % Choose: select drink (IP=3) OR refund (IP=7)
        {'SEQ_ENTER', 0},          % Start nested sequence for drink selection
        {'TASK_EXEC', select_drink},% Task: select drink
        {'SEQ_NEXT', 6},           % Jump to dispense task
        {'TASK_EXEC', dispense},    % Task: dispense drink
        {'DONE'},                  % Complete drink selection branch
        {'TASK_EXEC', refund},     % Task: refund coin
        {'DONE'}                   % Complete refund branch
    ].

%% @doc Executes the workflow and returns execution summary
%% Note: Tracing not yet integrated into executor (awaiting item 010)
%% This example demonstrates bytecode patterns and execution without trace events.
run() ->
    Bytecode = term(),
    ExecState = wf_exec:new(Bytecode),
    {done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

    %% Print execution summary
    Steps = wf_exec:get_step_count(DoneState),
    io:format("~n=== Vending Machine Workflow ===~n"),
    io:format("Steps executed: ~p~n", [Steps]),
    io:format("Status: ~p~n", [wf_exec:is_done(DoneState)]),
    io:format("~nPattern: Sequence + XOR Choice~n"),
    io:format("  1. Insert coin~n"),
    io:format("  2. Choose: Select drink OR Get refund~n"),
    io:format("  3. Complete workflow~n"),
    io:format("~nNOTE: XOR_CHOOSE selects first branch (select_drink) deterministically~n"),

    {ok, executed}.

%% @doc Expected sequence of opcode categories for validation
%% Note: Since tracing is not integrated, this returns a placeholder
%% The actual trace structure cannot be validated without executor integration
expected_trace_structure() ->
    [
        seq_enter,      % Enter outer sequence
        task_exec,     % insert_coin
        xor_choose,    % Choose branch
        seq_enter,     % Enter nested sequence
        task_exec,     % select_drink
        seq_next,      % Jump to dispense
        task_exec,     % dispense
        done           % Complete workflow
    ].
