-module(test_trace5).
-compile(export_all).

test() ->
    Term = wf_term:par([
        wf_term:task(x, #{
            function => fun(Ctx) -> 
                io:format(">>> Task X running~n"),
                {ok, Ctx#{x_ran => true}}
            end
        }),
        wf_term:task(y, #{
            function => fun(Ctx) -> 
                io:format(">>> Task Y running~n"),
                {ok, Ctx#{y_ran => true}}
            end
        })
    ]),
    {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
    ExecState0 = wf_exec:new({Bytecode, Metadata}),
    
    io:format("~n=== Step 0 (initial) ===~n"),
    print_state(ExecState0),
    
    {running, ExecState1} = wf_exec:step(ExecState0, {token, get_root_token(ExecState0)}),
    io:format("~n=== Step 1 (after PAR_FORK) ===~n"),
    print_state(ExecState1),
    
    {running, ExecState2} = wf_exec:step(ExecState1, {token, get_first_active(ExecState1)}),
    io:format("~n=== Step 2 (after task X) ===~n"),
    print_state(ExecState2),
    
    {running, ExecState3} = wf_exec:step(ExecState2, {token, get_first_active(ExecState2)}),
    io:format("~n=== Step 3 (after X DONE) ===~n"),
    print_state(ExecState3),
    
    {running, ExecState4} = wf_exec:step(ExecState3, {token, get_first_active(ExecState3)}),
    io:format("~n=== Step 4 (after task Y?) ===~n"),
    print_state(ExecState4),
    
    ok.

print_state(ES) ->
    io:format("IP: ~p~n", [ES#exec_state.ip]),
    io:format("Current: ~p~n", [ES#exec_state.current_token]),
    io:format("Tokens:~n"),
    maps:fold(fun(TID, T, _) ->
        Status = T#token.status,
        IP = T#token.ip,
        io:format("  ~p: status=~p, ip=~p~n", [print_tid(TID), Status, IP]),
        ok
    end, ok, ES#exec_state.tokens),
    io:format("JoinCounters: ~p~n", [maps:size(ES#exec_state.join_counters)]).

print_tid(TID) when is_reference(TID) ->
    <<A:32, B:32, C:32>> = erlang:term_to_binary(TID),
    <<X:8, Y:8, Z:8>> = <<A:24>>,
    lists:flatten(io_lib:format("~p", [[X,Y,Z]]));
print_tid(TID) -> TID.

get_root_token(ES) ->
    [TID] = [TID || {TID, T} <- maps:to_list(ES#exec_state.tokens), T#token.scope_id =:= root],
    TID.

get_first_active(ES) ->
    [{TID, _} | _] = [{TID, T} || {TID, T} <- maps:to_list(ES#exec_state.tokens), T#token.status =:= active],
    TID.
