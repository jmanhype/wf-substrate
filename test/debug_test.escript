-module(debug_test).
-export([test_discriminator/0]).

test_discriminator() ->
    %% Test discriminator with 2 iterations
    BC = wf_bench:generate_discriminator_repeat(2),
    io:format("Bytecode length: ~p~n", [length(BC)]),
    io:format("First 15 opcodes: ~p~n", [lists:sublist(BC, 15)]),

    ES0 = wf_exec:new(BC),
    {done, ES1} = wf_exec:run(ES0, 10000, undefined),
    io:format("Final: step_count=~p~n", [wf_exec:get_step_count(ES1)]),
    io:format("Final: ip=~p~n", [element(2, ES1)]),

    ok.
