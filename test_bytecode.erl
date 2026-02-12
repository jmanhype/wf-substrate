-module(test_bytecode).
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
    io:format("Bytecode:~n~p~n", [Bytecode]),
    io:format("Metadata:~n~p~n", [Metadata]).
