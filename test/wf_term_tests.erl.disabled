%%%-------------------------------------------------------------------
%%% @doc EUnit test suite for wf_term
%%%
%%% Tests all constructors, smart constructors, and validation logic.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_term_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test raw constructors create correct tuples.
%%--------------------------------------------------------------------
raw_constructor_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,

        %% Raw task constructor
        Task = wf_term:raw_task(test_task, #{function => Fun}),
        ?assertMatch({task, test_task, #{function := _}}, Task),

        %% Raw seq constructor
        Seq = wf_term:raw_seq(Task, Task),
        ?assertMatch({seq, {task, _, _}, {task, _, _}}, Seq),

        %% Raw par constructor
        Par = wf_term:raw_par([Task, Task]),
        ?assertMatch({par, [{task, _, _}, {task, _, _}]}, Par),

        %% Raw x_or constructor
        XOr = wf_term:raw_x_or([Task, Task]),
        ?assertMatch({x_or, [{task, _, _}, {task, _, _}]}, XOr),

        %% Raw join constructor
        Join = wf_term:raw_join(all, [Task, Task]),
        ?assertMatch({join, all, [{task, _, _}, {task, _, _}]}, Join),

        %% Raw loop constructor
        Loop = wf_term:raw_loop(while, Task),
        ?assertMatch({loop, while, {task, _, _}}, Loop),

        %% Raw defer constructor
        Defer = wf_term:raw_defer([Task, Task]),
        ?assertMatch({defer, [{task, _, _}, {task, _, _}]}, Defer),

        %% Raw cancel constructor
        Cancel = wf_term:raw_cancel(test_scope, Task),
        ?assertMatch({cancel, test_scope, {task, _, _}}, Cancel),

        %% Raw mi constructor
        Mi = wf_term:raw_mi({fixed, 3}, Task),
        ?assertMatch({mi, {fixed, 3}, {task, _, _}}, Mi)
    end.

%%--------------------------------------------------------------------
%% @doc Test smart constructors with valid inputs.
%%--------------------------------------------------------------------
smart_constructor_valid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,

        %% Valid task constructor
        Task = wf_term:task(test_task, #{function => Fun}),
        ?assertMatch({task, test_task, #{function := _}}, Task),

        %% Valid seq constructor
        Seq = wf_term:seq(Task, Task),
        ?assertMatch({seq, _, _}, Seq),

        %% Valid par constructor
        Par = wf_term:par([Task, Task]),
        ?assertMatch({par, _}, Par),

        %% Valid x_or constructor
        XOr = wf_term:x_or([Task, Task]),
        ?assertMatch({x_or, _}, XOr),

        %% Valid join constructors
        Join1 = wf_term:join(all, [Task, Task]),
        ?assertMatch({join, all, _}, Join1),

        Join2 = wf_term:join(sync_merge, [Task, Task]),
        ?assertMatch({join, sync_merge, _}, Join2),

        Join3 = wf_term:join({first_n, 1}, [Task, Task]),
        ?assertMatch({join, {first_n, 1}, _}, Join3),

        Join4 = wf_term:join({n_of_m, 2, 3}, [Task, Task, Task]),
        ?assertMatch({join, {n_of_m, 2, 3}, _}, Join4),

        %% Valid loop constructors
        Loop1 = wf_term:loop(while, Task),
        ?assertMatch({loop, while, _}, Loop1),

        Loop2 = wf_term:loop(until, Task),
        ?assertMatch({loop, until, _}, Loop2),

        Loop3 = wf_term:loop({count, 5}, Task),
        ?assertMatch({loop, {count, 5}, _}, Loop3),

        %% Valid defer constructor
        Defer = wf_term:defer([Task, Task]),
        ?assertMatch({defer, _}, Defer),

        %% Valid cancel constructor
        Cancel = wf_term:cancel(test_scope, Task),
        ?assertMatch({cancel, test_scope, _}, Cancel),

        %% Valid mi constructors
        Mi1 = wf_term:mi({fixed, 3}, Task),
        ?assertMatch({mi, {fixed, 3}, _}, Mi1),

        Mi2 = wf_term:mi({dynamic, 1, 5}, Task),
        ?assertMatch({mi, {dynamic, 1, 5}, _}, Mi2)
    end.

%%--------------------------------------------------------------------
%% @doc Test smart constructors with invalid inputs (should throw).
%%--------------------------------------------------------------------
smart_constructor_invalid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),

        %% Invalid task: missing function key
        ?assertError({badarg, {invalid_task_metadata, _, _}},
                     wf_term:task(test_task, #{timeout => 100})),

        %% Invalid task: function not a fun
        ?assertError({badarg, {invalid_task_metadata, _, _}},
                     wf_term:task(test_task, #{function => not_a_fun})),

        %% Invalid par: too few branches
        ?assertError(badarg, wf_term:par([Task])),

        %% Invalid x_or: too few branches
        ?assertError(badarg, wf_term:x_or([Task])),

        %% Invalid join: too few branches
        ?assertError(badarg, wf_term:join(all, [Task])),

        %% Invalid join: bad policy
        ?assertError({badarg, {invalid_join_policy, bad_policy}},
                     wf_term:join(bad_policy, [Task, Task])),

        %% Invalid loop: bad policy
        ?assertError({badarg, {invalid_loop_policy, bad_policy}},
                     wf_term:loop(bad_policy, Task)),

        %% Invalid defer: too few branches
        ?assertError(badarg, wf_term:defer([Task])),

        %% Invalid mi: bad policy
        ?assertError({badarg, {invalid_mi_policy, bad_policy}},
                     wf_term:mi(bad_policy, Task))
    end.

%%--------------------------------------------------------------------
%% @doc Test well_formed/1 with valid terms.
%%--------------------------------------------------------------------
well_formed_valid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),

        %% Valid simple terms
        ?assertEqual(ok, wf_term:well_formed(Task)),

        %% Valid seq
        Seq = wf_term:seq(Task, Task),
        ?assertEqual(ok, wf_term:well_formed(Seq)),

        %% Valid par
        Par = wf_term:par([Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Par)),

        %% Valid x_or
        XOr = wf_term:x_or([Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(XOr)),

        %% Valid join
        Join = wf_term:join(all, [Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Join)),

        %% Valid loop
        Loop = wf_term:loop(while, Task),
        ?assertEqual(ok, wf_term:well_formed(Loop)),

        %% Valid defer
        Defer = wf_term:defer([Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Defer)),

        %% Valid cancel
        Cancel = wf_term:cancel(test_scope, Task),
        ?assertEqual(ok, wf_term:well_formed(Cancel)),

        %% Valid mi
        Mi = wf_term:mi({fixed, 3}, Task),
        ?assertEqual(ok, wf_term:well_formed(Mi)),

        %% Valid nested term
        Nested = wf_term:seq(
            wf_term:par([Task, Task]),
            wf_term:x_or([Task, Task])
        ),
        ?assertEqual(ok, wf_term:well_formed(Nested))
    end.

%%--------------------------------------------------------------------
%% @doc Test well_formed/1 with invalid terms.
%%--------------------------------------------------------------------
well_formed_invalid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),

        %% Invalid: par with 1 branch (using raw constructor to bypass smart check)
        BadPar = wf_term:raw_par([Task]),
        ?assertMatch({error, [{invalid_branch_count, par, 1, 2}]},
                     wf_term:well_formed(BadPar)),

        %% Invalid: x_or with 1 branch
        BXOr = wf_term:raw_x_or([Task]),
        ?assertMatch({error, [{invalid_branch_count, x_or, 1, 2}]},
                     wf_term:well_formed(BXOr)),

        %% Invalid: join with bad policy
        BadJoin = wf_term:raw_join(bad_policy, [Task, Task]),
        ?assertMatch({error, [{invalid_join_policy, bad_policy}]},
                     wf_term:well_formed(BadJoin)),

        %% Invalid: loop with bad policy
        BadLoop = wf_term:raw_loop(bad_policy, Task),
        ?assertMatch({error, [{invalid_loop_policy, bad_policy}]},
                     wf_term:well_formed(BadLoop)),

        %% Invalid: defer with 1 branch
        BadDefer = wf_term:raw_defer([Task]),
        ?assertMatch({error, [{invalid_branch_count, defer, 1, 2}]},
                     wf_term:well_formed(BadDefer)),

        %% Invalid: mi with bad policy
        BadMi = wf_term:raw_mi(bad_policy, Task),
        ?assertMatch({error, [{invalid_mi_policy, bad_policy}]},
                     wf_term:well_formed(BadMi))
    end.
