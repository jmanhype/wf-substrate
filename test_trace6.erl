-module(test_trace6).
-include_lib("wf_exec.hrl").
-compile(export_all).
-module(test_trace6).
-compile(export_all).

test() ->
    Term = wf_term:par([
        wf_term:task(x, #{function => fun(Ctx) -> {ok, Ctx#{x_ran => true}} end}),
        wf_term:task(y, #{function => fun(Ctx) -> {ok, Ctx#{y_ran => true}} end})
    ]),
    {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
    ExecState0 = wf_exec:new({Bytecode, Metadata}),
    
    io:format("Initial tokens: ~p~n", [maps:keys(ExecState0#exec_state.tokens)]),
    
    %% Step 1: PAR_FORK
    {running, ExecState1} = wf_exec:step(ExecState0, {token, hd(maps:keys(ExecState0#exec_state.tokens))}),
    io:format("~nAfter PAR_FORK:~n"),
    io:format("  Tokens: ~p~n", [maps:keys(ExecState1#exec_state.tokens)]),
    io:format("  Current: ~p~n", [ExecState1#exec_state.current_token]),
    io:format("  BranchMap: ~p~n", [maps:keys(ExecState1#exec_state.branch_map)]),
    
    %% Get active tokens after PAR_FORK
    Active = [TID || {TID, T} <- maps:to_list(ExecState1#exec_state.tokens), T#token.status =:= active],
    io:format("  Active tokens: ~p~n", [length(Active)]),
    
    %% Step 2: Execute first branch token
    [FirstActive | _] = Active,
    io:format("~nExecuting token ~p~n", [print_ref(FirstActive)]),
    {running, ExecState2} = wf_exec:step(ExecState1, {token, FirstActive}),
    io:format("~nAfter task X:~n"),
    io:format("  Tokens: ~p~n", [[print_ref(TID) || TID <- maps:keys(ExecState2#exec_state.tokens)]]),
    
    %% Check active tokens
    Active2 = [{TID, T#token.status, T#token.ip} || {TID, T} <- maps:to_list(ExecState2#exec_state.tokens)],
    io:format("  Token details: ~p~n", [[{print_ref(TID), Status, IP} || {TID, Status, IP} <- Active2]]),
    
    %% Step 3: Execute DONE for branch X
    {running, ExecState3} = wf_exec:step(ExecState2, {token, ExecState2#exec_state.current_token}),
    io:format("~nAfter X DONE:~n"),
    Active3 = [{TID, T#token.status} || {TID, T} <- maps:to_list(ExecState3#exec_state.tokens)],
    io:format("  Token details: ~p~n", [[{print_ref(TID), Status} || {TID, Status} <- Active3]]),
    io:format("  Join counters: ~p~n", [maps:values(ExecState3#exec_state.join_counters)]),
    
    ok.

print_ref(R) when is_reference(R) ->
    <<_:80>> = erlang:term_to_binary(R),
    "Ref".
