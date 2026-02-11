%%%-------------------------------------------------------------------
%%% @doc EUnit test suite for wf_core
%%%
%%% Tests all derived patterns.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_core_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test simple_merge/2.
%%--------------------------------------------------------------------
simple_merge_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid simple_merge
        Term = wf_core:simple_merge([Task1, Task2], Cont),
        ?assertMatch({x_or, _}, Term),

        %% Verify structure: x_or([seq(Task1, Cont), seq(Task2, Cont)])
        ?assertEqual(2, length(element(2, Term))),
        ?assertMatch({seq, Task1, Cont}, lists:nth(1, element(2, Term))),
        ?assertMatch({seq, Task2, Cont}, lists:nth(2, element(2, Term))),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Invalid: too few alternatives (should throw)
        ?assertError(badarg, wf_core:simple_merge([Task1], Cont))
    end.

%%--------------------------------------------------------------------
%% @doc Test synchronizing_merge/2.
%%--------------------------------------------------------------------
synchronizing_merge_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid synchronizing_merge
        Term = wf_core:synchronizing_merge([Task1, Task2], Cont),
        ?assertMatch({par, _}, Term),

        %% Verify structure: par([seq(Task1, Cont), seq(Task2, Cont)])
        ?assertEqual(2, length(element(2, Term))),
        ?assertMatch({seq, Task1, Cont}, lists:nth(1, element(2, Term))),
        ?assertMatch({seq, Task2, Cont}, lists:nth(2, element(2, Term))),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Invalid: too few branches (should throw)
        ?assertError(badarg, wf_core:synchronizing_merge([Task1], Cont))
    end.

%%--------------------------------------------------------------------
%% @doc Test discriminator/2.
%%--------------------------------------------------------------------
discriminator_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Task3 = wf_term:task(task3, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid discriminator
        Term = wf_core:discriminator([Task1, Task2, Task3], Cont),
        ?assertMatch({join, {first_n, 1}, _}, Term),

        %% Verify structure: join({first_n, 1}, [cancel(Scope1, seq(Task1, Cont)), ...])
        {join, {first_n, 1}, ScopedBranches} = Term,
        ?assertEqual(3, length(ScopedBranches)),

        %% Verify each branch is cancel-wrapped with continuation
        lists:foreach(
            fun(ScopedBranch) ->
                ?assertMatch({cancel, _, {seq, _, Cont}}, ScopedBranch)
            end,
            ScopedBranches
        ),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Invalid: too few branches (should throw)
        ?assertError(badarg, wf_core:discriminator([Task1], Cont)),

        %% Verify scopes are unique
        Scopes = [element(2, SB) || SB <- ScopedBranches],
        ?assertEqual(3, length(lists:usort(Scopes)))
    end.

%%--------------------------------------------------------------------
%% @doc Test n_out_of_m/3.
%%--------------------------------------------------------------------
n_out_of_m_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Task3 = wf_term:task(task3, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid n_out_of_m: 2 out of 3
        Term = wf_core:n_out_of_m(2, [Task1, Task2, Task3], Cont),
        ?assertMatch({seq, _, _}, Term),

        %% Verify structure: seq(join({n_of_m, 2, 3}, [Task1, Task2, Task3]), Cont)
        {seq, Join, SeqCont} = Term,
        ?assertMatch({join, {n_of_m, 2, 3}, _}, Join),
        ?assertMatch(Cont, SeqCont),

        %% Verify branches are in join
        ?assertEqual(3, length(element(3, Join))),
        ?assertMatch(Task1, lists:nth(1, element(3, Join))),
        ?assertMatch(Task2, lists:nth(2, element(3, Join))),
        ?assertMatch(Task3, lists:nth(3, element(3, Join))),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Edge cases
        ?assertEqual(ok, wf_term:well_formed(wf_core:n_out_of_m(1, [Task1, Task2], Cont))),
        ?assertEqual(ok, wf_term:well_formed(wf_core:n_out_of_m(2, [Task1, Task2], Cont))),

        %% Invalid: N > M (should throw)
        ?assertError(badarg, wf_core:n_out_of_m(5, [Task1, Task2], Cont)),

        %% Invalid: N <= 0 (should throw)
        ?assertError(badarg, wf_core:n_out_of_m(0, [Task1, Task2], Cont))
    end.
