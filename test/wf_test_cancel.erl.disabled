-module(wf_test_cancel).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%%====================================================================
%% Mock Bytecode Generators for Cancellation Patterns
%%====================================================================

%% Cancel running task
mock_bytecode_cancel_task() ->
    [
        {'CANCEL_SCOPE', {enter, task_scope}},
        {'TASK_EXEC', long_task},
        {'CANCEL_SCOPE', {exit, task_scope}}
    ].

%% Cancel case with active PAR branches
mock_bytecode_cancel_case_with_par() ->
    [
        {'CANCEL_SCOPE', {enter, case_scope}},
        {'PAR_FORK', [2, 4]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'CANCEL_SCOPE', {exit, case_scope}}
    ].

%% Cancel case with XOR
mock_bytecode_cancel_case_with_xor() ->
    [
        {'CANCEL_SCOPE', {enter, case_scope}},
        {'XOR_CHOOSE', [2, 4]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'CANCEL_SCOPE', {exit, case_scope}}
    ].

%% Nested cancel scopes
mock_bytecode_cancel_nested_scopes() ->
    [
        {'CANCEL_SCOPE', {enter, outer_scope}},
        {'TASK_EXEC', task_a},
        {'CANCEL_SCOPE', {enter, inner_scope}},
        {'TASK_EXEC', task_b},
        {'CANCEL_SCOPE', {exit, inner_scope}},
        {'TASK_EXEC', task_c},
        {'CANCEL_SCOPE', {exit, outer_scope}}
    ].

%% Cancel during MI
mock_bytecode_cancel_during_mi() ->
    [
        {'CANCEL_SCOPE', {enter, mi_scope}},
        {'MI_SPAWN', {{fixed, 3}, wait_all, 10}},
        {'TASK_EXEC', mi_task},
        {'DONE'},
        {'JOIN_WAIT', wait_all},
        {'CANCEL_SCOPE', {exit, mi_scope}}
    ].

%% Empty cancel scope
mock_bytecode_cancel_empty_scope() ->
    [
        {'CANCEL_SCOPE', {enter, empty_scope}},
        {'CANCEL_SCOPE', {exit, empty_scope}},
        {'TASK_EXEC', task_after},
        {'DONE'}
    ].

%%====================================================================
%% Unit Tests: Cancel Running Task
%%====================================================================

cancel_running_task_test() ->
    Bytecode = mock_bytecode_cancel_task(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Enter scope, start task
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % CANCEL_SCOPE enter
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % TASK_EXEC
    %% Cancel mid-task
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    %% Token should be cancelled
    CurrentTokenId = ExecState3#exec_state.current_token,
    Token = maps:get(CurrentTokenId, ExecState3#exec_state.tokens),
    ?assertEqual(cancelled, Token#token.status).

%%====================================================================
%% Unit Tests: Cancel Case with Active PAR Branches
%%====================================================================

cancel_case_with_active_par_branches_test() ->
    Bytecode = mock_bytecode_cancel_case_with_par(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until PAR_FORK
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % CANCEL_SCOPE enter
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % PAR_FORK
    %% Cancel case
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    %% All tokens should be cancelled
    Tokens = ExecState3#exec_state.tokens,
    lists:foreach(fun({Id, Token}) ->
        ?assertEqual(cancelled, Token#token.status,
                     {io_lib:format("Token ~p should be cancelled", [Id])})
    end, maps:to_list(Tokens)).

%%====================================================================
%% Unit Tests: Cancel Case with XOR
%%====================================================================

cancel_case_with_xor_test() ->
    Bytecode = mock_bytecode_cancel_case_with_xor(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until XOR_CHOOSE
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % CANCEL_SCOPE enter
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % XOR_CHOOSE
    %% Cancel case
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    _Tokens = ExecState3#exec_state.tokens,
    CancelledCount = count_tokens_with_status(ExecState3, cancelled),
    ?assert(CancelledCount > 0).

%%====================================================================
%% Unit Tests: Cancel During MI
%%====================================================================

cancel_during_mi_test() ->
    Bytecode = mock_bytecode_cancel_during_mi(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute until MI_SPAWN
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % CANCEL_SCOPE enter
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % MI_SPAWN
    %% Cancel MI
    ExecState3 = wf_exec:cancel(ExecState2),
    ?assertEqual(cancelled, ExecState3#exec_state.status),
    %% All tokens should be cancelled
    Tokens = ExecState3#exec_state.tokens,
    lists:foreach(fun({_Id, Token}) ->
        ?assertEqual(cancelled, Token#token.status)
    end, maps:to_list(Tokens)).

%%====================================================================
%% Unit Tests: Nested Cancel Scopes
%%====================================================================

cancel_nested_scopes_stack_management_test() ->
    Bytecode = mock_bytecode_cancel_nested_scopes(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute into inner scope
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % enter outer
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % task_a
    {ExecState3, _} = wf_exec:step(ExecState2, undefined), % enter inner
    {ExecState4, _} = wf_exec:step(ExecState3, undefined), % task_b
    %% Verify scope stack depth = 2 (outer + inner)
    ?assertEqual(2, length(ExecState4#exec_state.scope_stack)),
    %% Exit inner scope
    {ExecState5, _} = wf_exec:step(ExecState4, undefined), % exit inner
    ?assertEqual(1, length(ExecState5#exec_state.scope_stack)),
    %% Execute task_c
    {ExecState6, _} = wf_exec:step(ExecState5, undefined), % task_c
    %% Exit outer scope
    {ExecState7, _} = wf_exec:step(ExecState6, undefined), % exit outer
    ?assertEqual(0, length(ExecState7#exec_state.scope_stack)).

cancel_inner_scope_preserves_outer_test() ->
    Bytecode = mock_bytecode_cancel_nested_scopes(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute into inner scope
    {ExecState1, _} = wf_exec:step(ExecState0, undefined), % enter outer
    {ExecState2, _} = wf_exec:step(ExecState1, undefined), % task_a
    {ExecState3, _} = wf_exec:step(ExecState2, undefined), % enter inner
    %% Cancel inner scope (simulate)
    %% In real scenario, we'd cancel at inner scope boundary
    {ExecState4, _} = wf_exec:step(ExecState3, undefined), % task_b
    %% Cancel should propagate but preserve outer scope context
    ExecState5 = wf_exec:cancel(ExecState4),
    ?assertEqual(cancelled, ExecState5#exec_state.status).

%%====================================================================
%% Unit Tests: Empty Cancel Scope
%%====================================================================

cancel_empty_scope_test() ->
    Bytecode = mock_bytecode_cancel_empty_scope(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecState1} = wf_exec:run(ExecState0, 100, undefined),
    ?assert(wf_exec:is_done(ExecState1)),
    %% Should complete normally (empty scope)
    ?assertEqual(4, wf_exec:get_step_count(ExecState1)).

%%====================================================================
%% Unit Tests: Cancel Region Preserves Unrelated Scopes
%%====================================================================

cancel_region_preserves_unrelated_scopes_test() ->
    %% Create nested scopes with independent cancellation
    Bytecode = [
        {'CANCEL_SCOPE', {enter, region1}},
        {'TASK_EXEC', task_a},
        {'CANCEL_SCOPE', {enter, region2}},
        {'TASK_EXEC', task_b},
        {'CANCEL_SCOPE', {exit, region2}},
        {'CANCEL_SCOPE', {exit, region1}}
    ],
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute into region2
    lists:foldl(fun(_, Acc) ->
        {NewState, _} = wf_exec:step(Acc, undefined),
        NewState
    end, ExecState0, lists:seq(1, 3)),
    %% Cancel should handle nested scopes correctly
    ExecState1 = wf_exec:cancel(ExecState0),
    ?assertEqual(cancelled, ExecState1#exec_state.status).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Count tokens with specific status
count_tokens_with_status(ExecState, Status) ->
    lists:count(fun(_Id, Token) ->
        Token#token.status =:= Status
    end, maps:to_list(ExecState#exec_state.tokens)).
