%%%-------------------------------------------------------------------
%%% @doc Basic usage examples for wf_substrate
%%% @end
%%%-------------------------------------------------------------------
-module(basic_usage).
-export([run_simple_workflow/0, run_parallel_workflow/0]).

%% @doc Run a simple sequential workflow
run_simple_workflow() ->
    %% Define bytecode for sequence: Task A -> Task B -> Done
    Bytecode = [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ],

    %% Start application
    {ok, _Apps} = application:ensure_all_started(wf_substrate),

    %% Create new case
    {ok, Pid} = wf_substrate:new_case(simple_seq, Bytecode, #{}),

    %% Await completion
    Result = wf_substrate:await(simple_seq, 5000),

    %% Clean up
    application:stop(wf_substrate),

    Result.

%% @doc Run a parallel workflow
run_parallel_workflow() ->
    %% Define bytecode for parallel: Task A || Task B
    Bytecode = [
        {'PAR_FORK', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'JOIN_WAIT', all}
    ],

    %% Start application
    {ok, _Apps} = application:ensure_all_started(wf_substrate),

    %% Create new case with tracing enabled
    {ok, Pid} = wf_substrate:new_case(parallel_flow, Bytecode, #{}),

    %% Configure tracing
    ok = wf_substrate:trace(parallel_flow, min, {ets, wf_trace_events}),

    %% Send signal during execution
    ok = wf_substrate:signal(parallel_flow, {user_event, data}),

    %% Check status
    {ok, StatusInfo} = wf_substrate:status(parallel_flow),

    %% Await completion
    Result = wf_substrate:await(parallel_flow, 10000),

    %% Clean up
    application:stop(wf_substrate),

    Result.
