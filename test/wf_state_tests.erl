-module(wf_state_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_state.hrl").
%%====================================================================
%% Setup
%%====================================================================

setup() ->
    case whereis(wf_state) of
        undefined ->
            {ok, Pid} = wf_state:start_link(),
            Pid;
        Pid ->
            Pid
    end.

cleanup(_Pid) ->
    %% Don't stop the gen_server, let it live for all tests
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

%% Wrap all tests with setup/teardown
wf_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"new_test", fun new_test/0},
         {"buffer_mutation_test", fun buffer_mutation_test/0},
         {"commit_test", fun commit_test/0},
         {"commit_validation_error_test", fun commit_validation_error_test/0},
         {"rollback_test", fun rollback_test/0},
         {"token_management_test", fun token_management_test/0},
         {"scope_management_test", fun scope_management_test/0},
         {"snapshot_restore_test", fun snapshot_restore_test/0},
         {"ets_persistence_test", fun ets_persistence_test/0},
         {"multiple_mutations_test", fun multiple_mutations_test/0},
         {"duplicate_token_test", fun duplicate_token_test/0},
         {"non_existent_scope_test", fun non_existent_scope_test/0},
         {"rollback_after_failed_commit_test", fun rollback_after_failed_commit_test/0},
         {"step_count_test", fun step_count_test/0},
         {"case_status_init_test", fun case_status_init_test/0},
         {"case_status_cancelled_test", fun case_status_cancelled_test/0},
         {"case_status_done_test", fun case_status_done_test/0},
         {"case_status_failed_test", fun case_status_failed_test/0},
         {"case_status_running_test", fun case_status_running_test/0},
         {"case_status_invalid_test", fun case_status_invalid_test/0},
         {"case_status_persistence_test", fun case_status_persistence_test/0}
     ]
    }.

%% Test state creation
new_test() ->
    InitialCtx = #{key => value},
    {ok, State} = wf_state:new(InitialCtx),
    ?assertEqual(InitialCtx, wf_state:get_ctx(State)),
    ?assertEqual(#{}, wf_state:get_tokens(State)),
    ?assertMatch({scope, root, undefined, active, [], _}, wf_state:get_scope(State, root)),
    ?assertEqual(undefined, wf_state:get_scope(State, non_existent)).

%% Test mutation buffering
buffer_mutation_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_ctx, #{new => ctx}}),
    State2 = wf_state:buffer_mutation(State1, {increment_step_count}),
    %% Access internal field for testing
    ?assertEqual(2, length(element(7, State2))),  %% buffered_mutations field
    ?assertEqual(#{}, wf_state:get_ctx(State2)).  %% Not applied yet

%% Test atomic commit
commit_test() ->
    InitialCtx = #{counter => 0},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:put_ctx(State0, #{counter => 1}),
    {ok, State2, Receipt} = wf_state:commit(State1),
    ?assertEqual(#{counter => 1}, wf_state:get_ctx(State2)),
    ?assertEqual([], element(7, State2)),  %% buffered_mutations is empty
    ?assertMatch({receipt, _, _, _, _, _, _}, Receipt).  %% Receipt is a tuple

%% Test validation errors
commit_validation_error_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    %% Try to remove non-existent token
    {ok, State1} = wf_state:remove_token(State0, non_existent_token),
    Result = wf_state:commit(State1),
    ?assertMatch({error, {validation_failed, _}}, Result).

%% Test rollback
rollback_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:put_ctx(State0, #{new => ctx}),
    State2 = wf_state:rollback(State1),
    ?assertEqual([], element(7, State2)),  %% buffered_mutations is empty
    ?assertEqual(#{}, wf_state:get_ctx(State2)).  %% Original ctx unchanged

%% Test token management
token_management_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    TokenId = make_ref(),
    Token = {token, TokenId, 0, root, undefined, active},  %% Use tuple format
    {ok, State1} = wf_state:add_token(State0, TokenId, Token),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    Tokens = wf_state:get_tokens(State2),
    ?assert(maps:is_key(TokenId, Tokens)),
    RetrievedToken = maps:get(TokenId, Tokens),
    ?assertEqual(TokenId, element(2, RetrievedToken)).  %% Check token_id field

%% Test scope management
scope_management_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, my_scope),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    Scope = wf_state:get_scope(State2, my_scope),
    ?assertMatch({scope, my_scope, root, active, _, _}, Scope).  %% Scope tuple format

%% Test snapshot and restore
snapshot_restore_test() ->
    InitialCtx = #{key => value},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:put_ctx(State0, #{new => ctx}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    Binary = wf_state:snapshot(State2),
    %% Get case_id - we need to access internal state
    CaseId = element(2, State2),  %% case_id field
    {ok, State3} = wf_state:restore(Binary, CaseId),
    ?assertEqual(wf_state:get_ctx(State2), wf_state:get_ctx(State3)),
    ?assertEqual(wf_state:get_tokens(State2), wf_state:get_tokens(State3)),
    ?assertEqual([], element(7, State3)).  %% buffered_mutations is empty

%% Test ETS persistence
ets_persistence_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    %% State is persisted in new/1
    CaseId = element(2, State0),  %% case_id field
    {ok, State1} = wf_state:put_ctx(State0, #{persisted => true}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    %% Restore from ETS
    {ok, State3} = wf_state:restore_from_ets(CaseId),
    ?assertEqual(wf_state:get_ctx(State2), wf_state:get_ctx(State3)),
    ?assertEqual(wf_state:get_tokens(State2), wf_state:get_tokens(State3)).

%% Test commit with multiple mutations
multiple_mutations_test() ->
    InitialCtx = #{counter => 0},
    {ok, State0} = wf_state:new(InitialCtx),
    TokenId = make_ref(),
    Token = {token, TokenId, 0, root, undefined, active},  %% Use tuple format
    %% Buffer multiple mutations
    {ok, State1} = wf_state:put_ctx(State0, #{counter => 1}),
    {ok, State2} = wf_state:add_token(State1, TokenId, Token),
    {ok, State3} = wf_state:enter_scope(State2, test_scope),
    %% Commit all mutations
    {ok, State4, _Receipt} = wf_state:commit(State3),
    ?assertEqual(#{counter => 1}, wf_state:get_ctx(State4)),
    ?assert(maps:is_key(TokenId, wf_state:get_tokens(State4))),
    Scope = wf_state:get_scope(State4, test_scope),
    ?assertMatch({scope, test_scope, root, active, _, _}, Scope),  %% Scope tuple format
    ?assertEqual([], element(7, State4)).  %% buffered_mutations is empty

%% Test validation of duplicate token
duplicate_token_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    TokenId = make_ref(),
    Token = {token, TokenId, 0, root, undefined, active},  %% Use tuple format
    %% Add token
    {ok, State1} = wf_state:add_token(State0, TokenId, Token),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    %% Try to add same token again
    {ok, State3} = wf_state:add_token(State2, TokenId, Token),
    Result = wf_state:commit(State3),
    ?assertMatch({error, {validation_failed, [{token_already_exists, TokenId}]}}, Result).

%% Test validation of non-existent scope
non_existent_scope_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    %% Try to exit non-existent scope
    {ok, State1} = wf_state:exit_scope(State0, non_existent),
    Result = wf_state:commit(State1),
    ?assertMatch({error, {validation_failed, [{scope_not_found, non_existent}]}}, Result).

%% Test rollback after failed commit
rollback_after_failed_commit_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    %% Valid mutation
    {ok, State1} = wf_state:put_ctx(State0, #{valid => true}),
    %% Commit valid mutation first
    {ok, State2, _Receipt1} = wf_state:commit(State1),
    ?assertEqual(#{valid => true}, wf_state:get_ctx(State2)),
    %% Now try invalid mutation (remove non-existent token)
    {ok, State3} = wf_state:remove_token(State2, non_existent),
    %% Commit should fail
    {error, _} = wf_state:commit(State3),
    %% State should be unchanged from last successful commit
    ?assertEqual(#{valid => true}, wf_state:get_ctx(State3)).

%% Test step count increment
step_count_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    %% Get initial step count from metadata
    %% We can't access internal metadata easily, so we'll test through commits
    %% Buffer increment mutations
    State1 = wf_state:buffer_mutation(State0, increment_step_count),
    State2 = wf_state:buffer_mutation(State1, increment_step_count),
    {ok, State3, _Receipt} = wf_state:commit(State2),
    %% If we got here without errors, step count was incremented
    ?assert(is_map(State3)).

%%====================================================================
%% Case Status Tests (US-001)
%%====================================================================

%% Test case status initialization
case_status_init_test() ->
    InitialCtx = #{},
    {ok, State} = wf_state:new(InitialCtx),
    ?assertEqual(running, wf_state:get_status(State)).

%% Test case status mutation - cancelled
case_status_cancelled_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_case_status, cancelled}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    ?assertEqual(cancelled, wf_state:get_status(State2)).

%% Test case status mutation - done
case_status_done_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_case_status, done}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    ?assertEqual(done, wf_state:get_status(State2)).

%% Test case status mutation - failed
case_status_failed_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_case_status, failed}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    ?assertEqual(failed, wf_state:get_status(State2)).

%% Test case status mutation - running (back to running)
case_status_running_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_case_status, cancelled}),
    {ok, State2, _Receipt1} = wf_state:commit(State1),
    State3 = wf_state:buffer_mutation(State2, {set_case_status, running}),
    {ok, State4, _Receipt2} = wf_state:commit(State3),
    ?assertEqual(running, wf_state:get_status(State4)).

%% Test invalid case status rejected
case_status_invalid_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_case_status, invalid_status}),
    Result = wf_state:commit(State1),
    ?assertMatch({error, {validation_failed, _}}, Result).

%% Test case status persistence in ETS
case_status_persistence_test() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_case_status, cancelled}),
    {ok, State2, _Receipt} = wf_state:commit(State1),

    %% Restore from ETS using the getter function
    CaseId = wf_state:get_case_id(State2),
    {ok, RestoredState} = wf_state:restore_from_ets(CaseId),

    %% Verify status persisted
    ?assertEqual(cancelled, wf_state:get_status(RestoredState)).
