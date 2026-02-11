-module(wf_example_discriminator).
-export([term/0, run/0, expected_trace_structure/0]).
-include_lib("kernel/include/logger.hrl").
-include("wf_trace.hrl").

-define(exec_state, exec_state).  % Opaque record placeholder

%% @doc Returns the discriminator workflow bytecode term
%% Pattern: discriminator(par([task(approve_manager), task(approve_director), task(approve_vp)]))
%% Demonstrates: PAR_FORK, JOIN_WAIT with first_complete, DONE
%%
%% The discriminator pattern:
%% 1. Fork into 3 parallel approval tasks (manager, director, VP)
%% 2. First task to complete causes the join to fire immediately
%% 3. Remaining tasks are cancelled automatically (when item 008 implemented)
%% 4. Result notification task executes
%%
%% This is useful for approval workflows where any one approval is sufficient.
term() ->
    [
        {'PAR_FORK', [1, 3, 5]},      % Fork into 3 parallel branches
        {'TASK_EXEC', approve_manager},% Branch 1: manager approval
        {'DONE'},                      % Complete branch 1
        {'TASK_EXEC', approve_director},% Branch 2: director approval
        {'DONE'},                      % Complete branch 2
        {'TASK_EXEC', approve_vp},     % Branch 3: VP approval
        {'DONE'},                      % Complete branch 3
        {'JOIN_WAIT', first_complete}, % Wait for FIRST completion only
        {'TASK_EXEC', notify_result},   % Notify result
        {'DONE'}                       % Complete workflow
    ].

%% @doc Executes the workflow and returns execution summary
%% Note: Tracing not yet integrated into executor (awaiting item 010)
run() ->
    Bytecode = term(),
    ExecState = wf_exec:new(Bytecode),
    {done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

    %% Print execution summary
    Steps = wf_exec:get_step_count(DoneState),
    io:format("~n=== Discriminator (Approval) Workflow ===~n"),
    io:format("Steps executed: ~p~n", [Steps]),
    io:format("Status: ~p~n", [wf_exec:is_done(DoneState)]),
    io:format("~nPattern: Discriminator (First Complete Join)~n"),
    io:format("  1. Fork into 3 parallel approval tasks~n"),
    io:format("  2. First task to complete fires join~n"),
    io:format("  3. Remaining tasks cancelled (awaiting item 008)~n"),
    io:format("  4. Notify result~n"),

    {ok, executed}.

%% @doc Expected sequence of opcode categories
%% First branch (approve_manager) completes first, satisfying the join
expected_trace_structure() ->
    [
        par_fork,       % Fork into 3 branches
        task_exec,      % approve_manager (completes first)
        done,           % Complete branch 1
        join_wait,      % Join satisfied by first completion
        task_exec,      % notify_result
        done            % Complete workflow
    ].
