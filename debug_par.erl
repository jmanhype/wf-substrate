-module(debug_par).
-compile(export_all).

test() ->
    Term = wf_term:par([
        wf_term:task(x, #{
            function => fun(Ctx) -> {ok, Ctx#{x_ran => true}} end
        }),
        wf_term:task(y, #{
            function => fun(Ctx) -> {ok, Ctx#{y_ran => true}} end
        })
    ]),
    {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
    io:format("Bytecode:~n~p~n~n", [Bytecode]),
    
    ExecState0 = wf_exec:new({Bytecode, Metadata}),
    io:format("Starting execution...~n"),
    
    Result = wf_exec:run(ExecState0, 20, deterministic),
    case Result of
        {done, FinalState} ->
            io:format("~nDone!~nFinalCtx: ~p~n", [wf_exec:get_ctx(FinalState)]);
        {yield, YieldState} ->
            io:format("~nYielded after ~p steps~n", [wf_exec:get_step_count(YieldState)])
    end.
