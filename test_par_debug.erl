-module(test_par_debug).
-compile(export_all).

test() ->
    %% Simple parallel: 2 branches
    Bytecode = [
        {par_fork, [1, 3]},
        {task_exec, task_a},
        {done},
        {task_exec, task_b},
        {done},
        {join_wait, all}
    ],
    
    ExecState0 = wf_exec:new(Bytecode),
    io:format("Initial state:~n  IP: ~p~n  Tokens: ~p~n", [
        ExecState0#exec_state.ip,
        maps:size(ExecState0#exec_state.tokens)
    ]),
    
    Result = wf_exec:run(ExecState0, 10, undefined),
    io:format("Result: ~p~n", [Result]),
    
    case Result of
        {done, ExecState1} ->
            io:format("Final state:~n  IP: ~p~n  Tokens: ~p~n  Status: ~p~n", [
                ExecState1#exec_state.ip,
                maps:to_list(ExecState1#exec_state.tokens),
                ExecState1#exec_state.status
            ]),
            lists:foreach(fun({TokenId, Token}) ->
                io:format("  Token ~p: IP=~p, Status=~p~n", [TokenId, Token#token.ip, Token#token.status])
            end, maps:to_list(ExecState1#exec_state.tokens));
        {yield, ExecState1} ->
            io:format("Yielded after ~p steps~n", [ExecState1#exec_state.step_count]),
            io:format("Current state:~n  IP: ~p~n  Tokens: ~p~n", [
                ExecState1#exec_state.ip,
                maps:to_list(ExecState1#exec_state.tokens)
            ])
    end.
