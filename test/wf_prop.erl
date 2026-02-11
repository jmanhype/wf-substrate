-module(wf_prop).
-export([random_term/1, for_all/3, quickcheck/2]).

%%====================================================================
%% Random Bytecode Generator
%%====================================================================

%% @doc Generate random bytecode with depth limit
-spec random_term(non_neg_integer()) -> wf_vm:wf_bc().
random_term(0) ->
    [{'DONE'}];
random_term(Depth) when Depth > 0 ->
    case rand:uniform(10) of
        N when N =< 4 ->
            %% Simple task sequence
            NumTasks = rand:uniform(3) + 1, % 1-3 tasks
            lists:flatten([
                [{'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))}]
             || I <- lists:seq(1, NumTasks)]) ++ [{'DONE'}];
        N when N =< 6 ->
            %% Parallel pattern
            NumBranches = rand:uniform(3) + 1, % 2-4 branches
            generate_par(NumBranches, Depth - 1);
        N when N =< 8 ->
            %% XOR pattern
            NumBranches = rand:uniform(3) + 1, % 2-4 branches
            generate_xor(NumBranches, Depth - 1);
        _ ->
            %% Sequence with cancellation
            [
                {'CANCEL_SCOPE', {enter, random_scope()}},
                {'TASK_EXEC', random_task()},
                {'CANCEL_SCOPE', {exit, random_scope()}}
            ]
    end.

%% Generate parallel pattern
generate_par(NumBranches, Depth) ->
    BranchTargets = lists:seq(1, 2*NumBranches - 1, 2),
    BranchBytecode = lists:flatten([random_term(Depth) || _ <- lists:seq(1, NumBranches)]),
    [{'PAR_FORK', BranchTargets} | BranchBytecode] ++ [{'JOIN_WAIT', all}].

%% Generate XOR pattern
generate_xor(NumBranches, Depth) ->
    BranchTargets = lists:seq(1, 2*NumBranches - 1, 2),
    BranchBytecode = lists:flatten([random_term(Depth) || _ <- lists:seq(1, NumBranches)]),
    [{'XOR_CHOOSE', BranchTargets} | BranchBytecode].

%% Helpers
random_task() ->
    list_to_atom("task_" ++ integer_to_list(rand:uniform(1000))).

random_scope() ->
    list_to_atom("scope_" ++ integer_to_list(rand:uniform(100))).

%%====================================================================
%% Property Test Runner
%%====================================================================

%% @doc Run property test N times
-spec for_all(fun(() -> wf_vm:wf_bc()), fun((wf_vm:wf_bc()) -> any()), pos_integer()) -> ok.
for_all(Generator, Property, NumTests) ->
    lists:foreach(fun(_N) ->
        Term = Generator(),
        Property(Term)
    end, lists:seq(1, NumTests)),
    ok.

%% @doc Quickcheck-style property test with failure reporting
-spec quickcheck(fun(() -> wf_vm:wf_bc()), fun((wf_vm:wf_bc()) -> boolean())) -> {ok, pos_integer()} | {error, wf_vm:wf_bc(), term()}.
quickcheck(Generator, Property) ->
    quickcheck_loop(Generator, Property, 0).

quickcheck_loop(Generator, Property, N) ->
    case N >= 100 of
        true ->
            {ok, N};
        false ->
            Term = Generator(),
            try
                case Property(Term) of
                    true -> quickcheck_loop(Generator, Property, N + 1);
                    false -> {error, Term, {failed, N}}
                end
            catch
                _:Exception ->
                    {error, Term, Exception}
            end
    end.
