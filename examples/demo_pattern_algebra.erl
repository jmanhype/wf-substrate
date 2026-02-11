%%%-------------------------------------------------------------------
%%% @doc Demo script showing pattern term algebra usage
%%% @end
%%%-------------------------------------------------------------------
-module(demo_pattern_algebra).
-compile([export_all]).

%% Demo: Create a simple workflow
demo_simple() ->
    Fun = fun(Ctx) -> {ok, Ctx} end,

    %% Create some tasks
    Task1 = wf_term:task(init, #{function => Fun, description => <<"Initialize">>}),
    Task2 = wf_term:task(process, #{function => Fun, description => <<"Process">>}),
    Task3 = wf_term:task(finalize, #{function => Fun, description => <<"Finalize">>}),

    %% Compose them sequentially
    Seq = wf_term:seq(Task1, wf_term:seq(Task2, Task3)),

    %% Validate
    case wf_term:well_formed(Seq) of
        ok -> io:format("Sequential workflow is well-formed~n");
        {error, Errors} -> io:format("Errors: ~p~n", [Errors])
    end.

%% Demo: Create a parallel workflow
demo_parallel() ->
    Fun = fun(Ctx) -> {ok, Ctx} end,

    %% Create parallel branches
    Task1 = wf_term:task(branch1, #{function => Fun}),
    Task2 = wf_term:task(branch2, #{function => Fun}),
    Task3 = wf_term:task(branch3, #{function => Fun}),

    %% Execute in parallel
    Par = wf_term:par([Task1, Task2, Task3]),

    %% Validate
    case wf_term:well_formed(Par) of
        ok -> io:format("Parallel workflow is well-formed~n");
        {error, Errors} -> io:format("Errors: ~p~n", [Errors])
    end.

%% Demo: Use derived patterns
demo_derived() ->
    Fun = fun(Ctx) -> {ok, Ctx} end,

    %% Create tasks
    TaskA = wf_term:task(task_a, #{function => Fun}),
    TaskB = wf_term:task(task_b, #{function => Fun}),
    TaskC = wf_term:task(task_c, #{function => Fun}),
    Cont = wf_term:task(continuation, #{function => Fun}),

    %% Use simple_merge pattern
    SimpleMerge = wf_core:simple_merge([TaskA, TaskB], Cont),
    io:format("Simple merge: ~p~n", [SimpleMerge]),

    %% Use synchronizing_merge pattern
    SyncMerge = wf_core:synchronizing_merge([TaskA, TaskB], Cont),
    io:format("Sync merge: ~p~n", [SyncMerge]),

    %% Use discriminator pattern
    Disc = wf_core:discriminator([TaskA, TaskB, TaskC], Cont),
    io:format("Discriminator: ~p~n", [Disc]),

    %% Use n_out_of_m pattern
    NOfM = wf_core:n_out_of_m(2, [TaskA, TaskB, TaskC], Cont),
    io:format("N-of-M: ~p~n", [NOfM]),

    ok.
