-module(test_trace3).
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
    
    ExecState1 = wf_exec:step(ExecState0, deterministic),
    io:format("Step 1: ~p~n", [element(1, ExecState1)]),
    
    ExecState2 = wf_exec:step(element(2, ExecState1), deterministic),
    io:format("Step 2: ~p~n", [element(1, ExecState2)]),
    
    ExecState3 = wf_exec:step(element(2, ExecState2), deterministic),
    io:format("Step 3: ~p~n", [element(1, ExecState3)]),
    
    ExecState4 = wf_exec:step(element(2, ExecState3), deterministic),
    io:format("Step 4: ~p~n", [element(1, ExecState4)]),
    
    ExecState5 = wf_exec:step(element(2, ExecState4), deterministic),
    io:format("Step 5: ~p~n", [element(1, ExecState5)]),
    
    ok.
