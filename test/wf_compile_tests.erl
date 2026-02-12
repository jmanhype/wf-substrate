%%%-------------------------------------------------------------------
%%% @doc Unit tests for wf_compile bytecode compiler
%%%
%%% Tests compilation of each kernel primitive, label resolution,
%%% and validation of compiled bytecode.
%%% @end
-module(wf_compile_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Helpers
%%%===================================================================

%% Mock task metadata
mock_task_fun() ->
    fun(_Ctx) -> {ok, #{}} end.

mock_task_metadata() ->
    #{function => mock_task_fun()}.

%%%===================================================================
%%% Phase 3: Task and Sequence Tests
%%%===================================================================

compile_task_test_() ->
    {"Compile single task",
     fun() ->
         Term = {task, my_task, mock_task_metadata()},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         ?assertEqual([{task_exec, my_task}], Bytecode)
     end}.

compile_seq_test_() ->
    {"Compile sequence of two tasks",
     fun() ->
         Term = {seq,
                 {task, task_a, mock_task_metadata()},
                 {task, task_b, mock_task_metadata()}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         ?assertMatch([{seq_enter, 0}, {task_exec, task_a},
                       {seq_next, _}, {task_exec, task_b}],
                      Bytecode),
         %% Verify seq_next target is integer (resolved label)
         {_, SEQ_NEXT_IP} = lists:nth(3, Bytecode),
         ?assert(is_integer(SEQ_NEXT_IP))
     end}.

%%%===================================================================
%%% Phase 4: Parallel and Exclusive Choice Tests
%%%===================================================================

compile_par_test_() ->
    {"Compile parallel fork of two tasks",
     fun() ->
         Term = {par, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         FlatBytecode = Bytecode,
         ?assertMatch([{par_fork, [_Label1, _Label2]}, _, _, _, _, {join_wait, all}],
                      FlatBytecode),
         %% Verify par_fork operand is list of integers (resolved labels)
         {_, ForkLabels} = lists:nth(1, FlatBytecode),
         ?assertEqual(2, length(ForkLabels)),
         ?assert(lists:all(fun is_integer/1, ForkLabels))
     end}.

compile_xor_test_() ->
    {"Compile exclusive choice of two tasks",
     fun() ->
         Term = {x_or, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         FlatBytecode = Bytecode,
         ?assertMatch([{xor_choose, [_Label1, _Label2]}, _, _, _, _],
                      FlatBytecode),
         %% Verify xor_choose operand is list of integers
         {_, ChooseLabels} = lists:nth(1, FlatBytecode),
         ?assertEqual(2, length(ChooseLabels)),
         ?assert(lists:all(fun is_integer/1, ChooseLabels)),
         %% Verify no join_wait present
         ?assertNot(lists:keymember(join_wait, 1, FlatBytecode))
     end}.

%%%===================================================================
%%% Phase 5: Loop and Join Tests
%%%===================================================================

compile_loop_count_test_() ->
    {"Compile count loop",
     fun() ->
         Term = {loop, {count, 3}, {task, task_a, mock_task_metadata()}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         FlatBytecode = Bytecode,
         ?assertMatch([{loop_check, {count, 3}}, {task_exec, task_a},
                       {loop_back, _}], FlatBytecode),
         %% Verify loop_back operand is integer (backward jump)
         {_, LOOP_BACK_IP} = lists:nth(3, FlatBytecode),
         ?assert(is_integer(LOOP_BACK_IP))
     end}.

compile_join_first_n_test_() ->
    {"Compile join with first_n policy",
     fun() ->
         Term = {join, {first_n, 1}, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         FlatBytecode = Bytecode,
         ?assertMatch([_, _, _, _, _, {join_wait, {first_n, 1}}],
                      FlatBytecode)
     end}.

validate_join_policy_test_() ->
    {"Validate join policy format",
     fun() ->
         %% Invalid n_of_m where N > M should fail
         Term = {join, {n_of_m, 5, 3}, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         ?assertMatch({error, {invalid_term, {badarg, {invalid_join_policy, _}}}},
                      wf_compile:compile(Term))
     end}.

%%%===================================================================
%%% Phase 6: Cancel and Multiple Instances Tests
%%%===================================================================

compile_cancel_test_() ->
    {"Compile cancel scope",
     fun() ->
         Term = {cancel, my_scope, {task, task_a, mock_task_metadata()}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         ?assertMatch([{cancel_scope, {enter, my_scope}}, {task_exec, task_a},
                       {cancel_scope, {exit, my_scope}}], Bytecode)
     end}.

compile_mi_test_() ->
    {"Compile multiple instances",
     fun() ->
         Term = {mi, {fixed, 3}, {task, task_a, mock_task_metadata()}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         ?assertMatch([{mi_spawn, {fixed, 3}}, {task_exec, task_a}, {done},
                       {join_wait, all}], Bytecode)
     end}.

validate_mi_policy_test_() ->
    {"Validate mi policy format",
     fun() ->
         %% Invalid fixed policy (0 or negative)
         Term = {mi, {fixed, 0}, {task, task_a, mock_task_metadata()}},
         ?assertMatch({error, {invalid_term, {badarg, {invalid_mi_policy, _}}}},
                      wf_compile:compile(Term))
     end}.

%%%===================================================================
%%% Phase 7: Integration Tests
%%%===================================================================

compile_seq_par_test_() ->
    {"Compile seq(par([a, b]), c)",
     fun() ->
         Term = {seq,
                 {par, [
                     {task, task_a, mock_task_metadata()},
                     {task, task_b, mock_task_metadata()}
                 ]},
                 {task, task_c, mock_task_metadata()}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         %% Should have: seq_enter, par_fork, 2 branches with done, join_wait, seq_next, task_c
         ?assertMatch([{seq_enter, 0}, {par_fork, [2, 4]}, _, _, _, _, {join_wait, all},
                       {seq_next, 8}, {task_exec, task_c}], Bytecode)
     end}.

compile_par_seq_test_() ->
    {"Compile par([seq(a, b), seq(c, d)])",
     fun() ->
         Term = {par, [
             {seq, {task, task_a, mock_task_metadata()}, {task, task_b, mock_task_metadata()}},
             {seq, {task, task_c, mock_task_metadata()}, {task, task_d, mock_task_metadata()}}
         ]},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         FlatBytecode = Bytecode,
         %% Should have: par_fork, 2 sequences, each with seq_enter/seq_next, done, done, join_wait
         ?assertEqual(2, length([1 || {done} <- FlatBytecode])),
         ?assert(lists:keymember(join_wait, 1, FlatBytecode))
     end}.

compile_loop_par_test_() ->
    {"Compile loop({count, 2}, par([a, b]))",
     fun() ->
         Term = {loop, {count, 2},
                  {par, [
                      {task, task_a, mock_task_metadata()},
                      {task, task_b, mock_task_metadata()}
                  ]}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         FlatBytecode = Bytecode,
         %% Should have: loop_check, par_fork, 2 branches with done, join_wait, loop_back
         ?assertEqual(8, length(FlatBytecode)),
         ?assertMatch({loop_check, {count, 2}}, lists:nth(1, FlatBytecode)),
         ?assertMatch({par_fork, [_, _]}, lists:nth(2, FlatBytecode)),
         ?assertMatch({join_wait, all}, lists:nth(7, FlatBytecode)),
         ?assertMatch({loop_back, _}, lists:nth(8, FlatBytecode))
     end}.

label_resolution_test_() ->
    {"Verify all labels resolved in complex workflow",
     fun() ->
         Term = {loop, {count, 3},
                  {par, [
                      {seq, {task, task_a, mock_task_metadata()},
                             {task, task_b, mock_task_metadata()}},
                      {seq, {task, task_c, mock_task_metadata()},
                             {task, task_d, mock_task_metadata()}}
                  ]}},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         %% Verify no label markers remain
         ?assertNot(lists:any(fun({label, _}) -> true; (_) -> false end, Bytecode)),
         %% Verify all label operands are integers
         ?assert(lists:all(fun
             ({_, {label, _}}) -> false;
             ({_, Labels}) when is_list(Labels) ->
                 lists:all(fun is_integer/1, Labels);
             (_) -> true
         end, Bytecode))
     end}.

validate_branch_count_test_() ->
    {"Validate par requires at least 2 branches",
     fun() ->
         %% Single branch par should fail
         Term = {par, [{task, task_a, mock_task_metadata()}]},
         ?assertMatch({error, _}, wf_compile:compile(Term))
     end}.

%%%===================================================================
%%% Phase 8: Comprehensive Integration Test
%%%===================================================================

compiler_integration_test_() ->
    {"Verify complete compiler with complex workflow",
     fun() ->
         %% Complex workflow: loop(seq(par([a, b]), c), cancel(scope, xor([d, e])))
         Term = {loop, {count, 2},
                  {seq,
                   {par, [
                       {task, task_a, mock_task_metadata()},
                       {task, task_b, mock_task_metadata()}
                   ]},
                   {cancel, my_scope,
                    {x_or, [
                        {task, task_c, mock_task_metadata()},
                        {task, task_d, mock_task_metadata()}
                    ]}}
                  }},
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),
         %% Verify bytecode is flat list
         ?assert(is_list(Bytecode)),
         %% Verify no nested lists
         ?assertNot(lists:any(fun is_list/1, Bytecode)),
         %% Verify no label markers remain
         ?assertNot(lists:any(fun({label, _}) -> true; (_) -> false end, Bytecode)),
         %% Verify all opcodes are valid
         ?assert(lists:all(fun
             ({seq_enter, _}) -> true;
             ({seq_next, _}) -> true;
             ({par_fork, _}) -> true;
             ({join_wait, _}) -> true;
             ({xor_choose, _}) -> true;
             ({loop_back, _}) -> true;
             ({loop_check, _}) -> true;
             ({cancel_scope, _}) -> true;
             ({mi_spawn, _}) -> true;
             ({task_exec, _}) -> true;
             ({done}) -> true;
             (_) -> false
         end, Bytecode))
     end}.
