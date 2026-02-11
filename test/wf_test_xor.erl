-module(wf_test_xor).
-export([
    mock_bytecode_xor_2_branches/0,
    mock_bytecode_xor_3_branches/0,
    mock_bytecode_xor_simple_merge/0,
    mock_bytecode_xor_n_branches/1
]).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%%====================================================================
%% Mock Bytecode Generators for Exclusive Choice Patterns
%%====================================================================

%% 2-branch XOR
mock_bytecode_xor_2_branches() ->
    [
        {'XOR_CHOOSE', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].

%% 3-branch XOR
mock_bytecode_xor_3_branches() ->
    [
        {'XOR_CHOOSE', [1, 3, 5]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'TASK_EXEC', task_c},
        {'DONE'}
    ].

%% XOR with simple merge (converge to same DONE)
mock_bytecode_xor_simple_merge() ->
    [
        {'XOR_CHOOSE', [1, 3]},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 5},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].

%% N-branch XOR
mock_bytecode_xor_n_branches(N) ->
    BranchTargets = lists:seq(1, 2*N - 1, 2),
    BranchBytecode = lists:flatten([[
        {'TASK_EXEC', list_to_atom("task_" ++ integer_to_list(I))},
        {'DONE'}
    ] || I <- lists:seq(1, N)]),
    [{'XOR_CHOOSE', BranchTargets} | BranchBytecode].

%%====================================================================
%% Unit Tests: 2-Branch XOR
%%====================================================================

xor_2_branches_one_executes_test() ->
    Bytecode = mock_bytecode_xor_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Verify only one branch executed (2 steps: TASK + DONE)
    ?assertEqual(3, wf_exec:get_step_count(ExecState1)), % choose + task + done
    %% Only 1 token (root token selected one branch)
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(1, maps:size(Tokens)).

%%====================================================================
%% Unit Tests: 3-Branch XOR
%%====================================================================

xor_3_branches_one_executes_test() ->
    Bytecode = mock_bytecode_xor_3_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    ?assertEqual(3, wf_exec:get_step_count(ExecState1)),
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(1, maps:size(Tokens)).

%%====================================================================
%% Unit Tests: Simple Merge
%%====================================================================

xor_simple_merge_converges_test() ->
    Bytecode = mock_bytecode_xor_simple_merge(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Both branches converge to same DONE
    ?assertEqual(4, wf_exec:get_step_count(ExecState1)), % choose + task + next + done
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(1, maps:size(Tokens)).

%%====================================================================
%% Unit Tests: Deterministic Selection
%%====================================================================

xor_deterministic_selection_test() ->
    Bytecode = mock_bytecode_xor_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Run again with same bytecode
    ExecState2 = wf_exec:new(Bytecode),
    {done, ExecState3} = wf_exec:run(ExecState2, 100, undefined),
    %% Same branch should be selected (deterministic scheduler)
    ?assertEqual(wf_exec:get_step_count(ExecState1),
                 wf_exec:get_step_count(ExecState3)).

xor_branch_selection_consistent_test() ->
    %% Run 10 times, should always select same branch with deterministic scheduler
    Bytecode = mock_bytecode_xor_3_branches(),
    StepCounts = [begin
        ExecState = wf_exec:new(Bytecode),
        {done, DoneState} = wf_exec:run(ExecState, 100, undefined),
        wf_exec:get_step_count(DoneState)
    end || _ <- lists:seq(1, 10)],
    %% All should be same (deterministic)
    ?assertEqual(1, length(lists:usort(StepCounts))).

%%====================================================================
%% Unit Tests: Unselected Branches
%%====================================================================

xor_unselected_branches_dont_execute_test() ->
    Bytecode = mock_bytecode_xor_2_branches(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    %% Verify only one token exists (unselected branches not executed)
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(1, maps:size(Tokens)).

%%====================================================================
%% Parameterized Tests: N-Branch XOR
%%====================================================================

xor_n_branches_test_() ->
    %% Test N = 2, 3, 5 branches
    lists:map(fun(N) ->
        {io_lib:format("xor_~p_branches", [N]), fun() ->
            Bytecode = mock_bytecode_xor_n_branches(N),
            ExecState0 = wf_exec:new(Bytecode),
            {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
            ?assert(wf_exec:is_done(ExecState1)),
            %% Only one branch should execute
            ?assertEqual(3, wf_exec:get_step_count(ExecState1)),
            Tokens = ExecState1#exec_state.tokens,
            ?assertEqual(1, maps:size(Tokens))
        end}
    end, [2, 3, 5]).
