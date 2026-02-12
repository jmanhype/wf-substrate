-module(test_trace4).
-compile(export_all).

test() ->
    Term = wf_term:par([
        wf_term:task(x, #{
            function => fun(Ctx) -> 
                io:format("Task X running, step ~p~n", [get_step()]),
                {ok, Ctx#{x_ran => true}}
            end
        }),
        wf_term:task(y, #{
            function => fun(Ctx) -> 
                io:format("Task Y running, step ~p~n", [get_step()]),
                {ok, Ctx#{y_ran => true}}
            end
        })
    ]),
    {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
    ExecState0 = wf_exec:new({Bytecode, Metadata}),
    
    io:format("Running 10 steps...~n"),
    put(step_count, 0),
    ExecState1 = run_n(ExecState0, 10),
    
    io:format("~nFinal Status: ~p~n", [wf_exec:is_done(ExecState1)]).

run_n(ExecState, 0) -> ExecState;
run_n(ExecState, N) ->
    put(step_count, get(step_count) + 1),
    case wf_exec:run(ExecState, 1, deterministic) of
        {done, ES} -> 
            io:format("Done!~n"),
            ES;
        {yield, ES} ->
            io:format("Step ~p: yield~n", [get(step_count)]),
            run_n(ES, N-1)
    end.

get_step() -> get(step_count).
