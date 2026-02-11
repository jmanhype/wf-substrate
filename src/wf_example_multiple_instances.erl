-module(wf_example_multiple_instances).
-export([term/0, run/0, expected_trace_structure/0]).
-include_lib("kernel/include/logger.hrl").
-include("wf_trace.hrl").

-define(exec_state, exec_state).  % Opaque record placeholder

%% @doc Returns the multiple instances workflow bytecode term
%% Pattern: mi({fixed, 5}, seq(task(process_item), done)), wait_n(3)
%% Demonstrates: PAR_FORK (workaround for MI_SPAWN), JOIN_WAIT with {wait_n, 3}
%%
%% Multiple Instances pattern:
%% 1. Spawn 5 parallel instances (using PAR_FORK workaround)
%% 2. Each instance processes one item
%% 3. Join waits for 3 of 5 instances to complete (partial completion)
%% 4. Remaining 2 instances cancelled automatically (when item 008 implemented)
%% 5. Aggregate results from completed instances
%%
%% LIMITATION: MI_SPAWN opcode is not implemented (item 009 incomplete)
%% This example uses PAR_FORK to simulate multiple instances with explicit branches.
%% The bytecode below manually creates 5 branches, which MI_SPAWN would generate automatically.
term() ->
    [
        %% PAR_FORK workaround: simulate 5 MI instances
        {'PAR_FORK', [1, 3, 5, 7, 9]},  % Spawn 5 parallel branches
        {'TASK_EXEC', process_item_1},   % Instance 1
        {'DONE'},                        % Complete instance 1
        {'TASK_EXEC', process_item_2},   % Instance 2
        {'DONE'},                        % Complete instance 2
        {'TASK_EXEC', process_item_3},   % Instance 3
        {'DONE'},                        % Complete instance 3
        {'TASK_EXEC', process_item_4},   % Instance 4
        {'DONE'},                        % Complete instance 4
        {'TASK_EXEC', process_item_5},   % Instance 5
        {'DONE'},                        % Complete instance 5
        {'JOIN_WAIT', {wait_n, 3}},      % Wait for 3 of 5 instances to complete
        {'TASK_EXEC', aggregate_results},% Aggregate results from completed instances
        {'DONE'}                         % Complete workflow
    ].

%% @doc Executes the workflow and returns execution summary
%% Note: Tracing not yet integrated into executor (awaiting item 010)
run() ->
    Bytecode = term(),
    ExecState = wf_exec:new(Bytecode),
    {done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

    %% Print execution summary
    Steps = wf_exec:get_step_count(DoneState),
    io:format("~n=== Multiple Instances Workflow ===~n"),
    io:format("Steps executed: ~p~n", [Steps]),
    io:format("Status: ~p~n", [wf_exec:is_done(DoneState)]),
    io:format("~nPattern: Multiple Instances with Wait-N Join~n"),
    io:format("  1. Spawn 5 parallel instances (PAR_FORK workaround)~n"),
    io:format("  2. Each instance processes an item~n"),
    io:format("  3. Join waits for 3 of 5 to complete~n"),
    io:format("  4. Remaining instances cancelled (awaiting item 008)~n"),
    io:format("  5. Aggregate results~n"),
    io:format("~nLIMITATION: Using PAR_FORK to simulate MI_SPAWN (awaiting item 009)~n"),

    {ok, executed}.

%% @doc Expected sequence of opcode categories
%% NOTE: Since tracing is not integrated, this returns a placeholder
expected_trace_structure() ->
    [
        par_fork,             % Fork into 5 instances
        task_exec,            % process_item_1
        done,                 % Complete instance 1
        task_exec,            % process_item_2
        done,                 % Complete instance 2
        task_exec,            % process_item_3
        done,                 % Complete instance 3 (satisfies join)
        join_wait,            % Join satisfied (3 of 5)
        task_exec,            % aggregate_results
        done                  % Complete workflow
    ].
