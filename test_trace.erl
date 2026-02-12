-module(test_trace).
-compile(export_all).

test() ->
    Term = wf_term:par([
        wf_term:task(x, #{
            function => fun(Ctx) -> 
                io:format("Task X running~n"),
                {ok, Ctx#{x_ran => true}}
            end
        }),
        wf_term:task(y, #{
            function => fun(Ctx) -> 
                io:format("Task Y running~n"),
                {ok, Ctx#{y_ran => true}}
            end
        })
    ]),
    {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
    ExecState0 = wf_exec:new({Bytecode, Metadata}),
    
    io:format("=== Starting execution ===~n"),
    Result = wf_exec:run(ExecState0, 20, deterministic),
    io:format("=== Result ===~n"),
    case Result of
        {done, FinalState} ->
            io:format("Done! FinalCtx: ~p~n", [wf_exec:get_ctx(FinalState)]);
        {yield, YieldState} ->
            io:format("Yielded after ~p steps~n", [wf_exec:get_step_count(YieldState)])
    end.
