-module(test_trace2).
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
    
    io:format("=== Step by step ===~n"),
    ExecState1 = step_n(ExecState0, 10),
    
    io:format("=== Final State ===~n"),
    io:format("Status: ~p~n", [ExecState1#exec_state.status]),
    io:format("IP: ~p~n", [ExecState1#exec_state.ip]),
    io:format("Tokens: ~p~n", [[{TID, T#token.status, T#token.ip} || {TID, T} <- maps:to_list(ExecState1#exec_state.tokens)]]).

step_n(ExecState, 0) -> ExecState;
step_n(ExecState, N) ->
    case wf_exec:step(ExecState, deterministic) of
        {running, ExecState1} -> step_n(ExecState1, N-1);
        {done, ExecState1} -> 
            io:format("Done at step ~p~n", [10-N]),
            ExecState1;
        {blocked, ExecState1} ->
            io:format("Blocked at step ~p~n", [10-N]),
            ExecState1
    end.
