-module(test_trace7).
-compile(export_all).

test() ->
    Term = wf_term:par([
        wf_term:task(x, #{function => fun(Ctx) -> io:format('Task X~n'), {ok, Ctx#{x_ran => true}} end}),
        wf_term:task(y, #{function => fun(Ctx) -> io:format('Task Y~n'), {ok, Ctx#{y_ran => true}} end})
    ]),
    {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
    ExecState0 = wf_exec:new({Bytecode, Metadata}),
    
    %% Run 5 steps
    run_n(ExecState0, 5).

run_n(ES, 0) -> ok;
run_n(ES, N) ->
    io:format('~n--- Step ~p ---~n', [6-N]),
    TokenCount = maps:size(ES#exec_state.tokens),
    io:format('Tokens: ~p~n', [TokenCount]),
    ActiveCount = length([T || T <- maps:values(ES#exec_state.tokens), T#token.status =:= active]),
    io:format('Active: ~p~n', [ActiveCount]),
    Current = ES#exec_state.current_token,
    Token = maps:get(Current, ES#exec_state.tokens),
    io:format('Current: status=~p, ip=~p~n', [Token#token.status, Token#token.ip]),
    
    case wf_exec:run(ES, 1, deterministic) of
        {done, Final} ->
            io:format('DONE!~n'),
            io:format('FinalCtx: ~p~n', [wf_exec:get_ctx(Final)]);
        {yield, Next} ->
            io:format('Yielded~n'),
            run_n(Next, N-1)
    end.
