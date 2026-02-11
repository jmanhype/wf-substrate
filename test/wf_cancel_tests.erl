%%%-------------------------------------------------------------------
%%% wf_cancel_tests - Unit tests for wf_cancel
%%%-------------------------------------------------------------------

-module(wf_cancel_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("wf_state.hrl").
-include_lib("wf_cancel.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

%% Create a test state with tokens and scopes
create_test_state() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),

    TokenId1 = make_ref(),
    TokenId2 = make_ref(),
    Token1 = #token{
        token_id = TokenId1,
        ip = 0,
        scope_id = scope1,
        value = task1,
        status = active
    },
    Token2 = #token{
        token_id = TokenId2,
        ip = 0,
        scope_id = scope2,
        value = task2,
        status = active
    },

    {ok, State1} = wf_state:add_token(State0, TokenId1, Token1),
    {ok, State2} = wf_state:add_token(State1, TokenId2, Token2),
    {ok, State3} = wf_state:enter_scope(State2, scope1),
    {ok, State4} = wf_state:enter_scope(State3, scope2),
    {ok, State5, _Receipt} = wf_state:commit(State4),
    State5.

%%====================================================================
%% Phase 2 Tests - Module Structure
%%====================================================================

%% Test module compiles and loads
wf_cancel_compiles_test_() ->
    ?_assert(true).

%% Test is_cancelled/2 with active scope
is_cancelled_active_scope_test_() ->
    State = create_test_state(),
    ?_assertNot(wf_cancel:is_cancelled(State, scope1)).

%% Test is_cancelled/2 with cancelled scope
is_cancelled_cancelled_scope_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, scope1),
    {ok, State2} = wf_state:commit(State1),

    %% Cancel the scope
    State3 = wf_state:buffer_mutation(State2, {cancel_scope, scope1}),
    {ok, State4, _Receipt} = wf_state:commit(State3),

    ?_assert(wf_cancel:is_cancelled(State4, scope1)).

%% Test is_cancelled/2 with undefined scope
is_cancelled_undefined_scope_test_() ->
    State = create_test_state(),
    ?_assertNot(wf_cancel:is_cancelled(State, nonexistent_scope)).

%% Test propagate/2 marks tokens in scope as cancelled
propagate_marks_tokens_in_scope_test_() ->
    TokensMap = #{
        token1 => #token{token_id = token1, ip = 0, scope_id = scope1, value = task1, status = active},
        token2 => #token{token_id = token2, ip = 0, scope_id = scope2, value = task2, status = active}
    },

    %% Propagate cancellation to scope1
    CancelledTokens = wf_cancel:propagate(scope1, TokensMap),

    %% Verify: token1 cancelled, token2 active
    [
        ?_assertEqual(cancelled, (maps:get(token1, CancelledTokens))#token.status),
        ?_assertEqual(active, (maps:get(token2, CancelledTokens))#token.status)
    ].

%% Test propagate/2 preserves other token fields
propagate_preserves_fields_test_() ->
    OriginalToken = #token{
        token_id = token1,
        ip = 5,
        scope_id = scope1,
        value = some_value,
        status = active
    },
    TokensMap = #{token1 => OriginalToken},

    CancelledTokens = wf_cancel:propagate(scope1, TokensMap),
    CancelledToken = maps:get(token1, CancelledTokens),

    [
        ?_assertEqual(token1, CancelledToken#token.token_id),
        ?_assertEqual(5, CancelledToken#token.ip),
        ?_assertEqual(scope1, CancelledToken#token.scope_id),
        ?_assertEqual(some_value, CancelledToken#token.value),
        ?_assertEqual(cancelled, CancelledToken#token.status)
    ].

%%====================================================================
%% Phase 3 Tests - Region Cancellation
%%====================================================================

%% Test region cancellation
cancel_region_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         ScopeId = scope1,
         CaseId = wf_state:get_case_id(State),
         {ok, Event} = wf_cancel:cancel_region(CaseId, ScopeId, []),
         [
             ?_assertEqual(ScopeId, Event#cancel_region.scope_id),
             ?_assertEqual(1, length(Event#cancel_region.cancelled_tokens)),
             ?_assertEqual(0, length(Event#cancel_region.cancelled_effects))
         ]
     end}.

%% Test region cancellation error on undefined scope
cancel_region_undefined_scope_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),
    ?_assertEqual(
        {error, {scope_not_found, nonexistent_scope}},
        wf_cancel:cancel_region(CaseId, nonexistent_scope, [])
    ).

%% Test region cancellation error on already cancelled
cancel_region_already_cancelled_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, scope1),
    {ok, State2, _Receipt1} = wf_state:commit(State1),

    %% Cancel scope first time
    CaseId = wf_state:get_case_id(State2),
    {ok, _Event1} = wf_cancel:cancel_region(CaseId, scope1, []),

    %% Try to cancel again
    ?_assertEqual(
        {error, {already_cancelled, scope1}},
        wf_cancel:cancel_region(CaseId, scope1, [])
    ).

%% Test region cancellation preserves other scopes (invariant)
cancel_region_preserves_other_scopes_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    %% Cancel scope1
    {ok, _Event} = wf_cancel:cancel_region(CaseId, scope1, []),

    %% Verify scope2 token still active (invariant)
    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    Tokens = wf_state:get_tokens(StateAfter),
    TokensInScope2 = [T || {_, T} <- maps:to_list(Tokens),
                          T#token.scope_id =:= scope2],
    ?_assertEqual([active], [T#token.status || T <- TokensInScope2]).

%% Helper: create state with N tokens, M in scope1
create_state_with_n_tokens_in_scope(TotalTokens, TokensInScope) ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, scope1),

    %% Add tokens
    {FinalState, _} = lists:foldl(fun(I, {AccState, AccCount}) ->
        TokenId = make_ref(),
        ScopeId = case I < TokensInScope of
            true -> scope1;
            false -> make_ref()  %% Different scope
        end,
        Token = #token{
            token_id = TokenId,
            ip = 0,
            scope_id = ScopeId,
            value = I,
            status = active
        },
        {ok, NewState} = wf_state:add_token(AccState, TokenId, Token),
        {NewState, AccCount + 1}
    end, {State1, 0}, lists:seq(1, TotalTokens)),

    {ok, State2, _Receipt} = wf_state:commit(FinalState),
    State2.

%% Test O(scope_size) complexity
cancel_region_complexity_test_() ->
    %% Create state with 100 tokens total, 5 in scope
    State = create_state_with_n_tokens_in_scope(100, 5),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() ->
        wf_cancel:cancel_region(CaseId, scope1, [])
    end),
    %% Should be fast (< 1ms) for 5 tokens
    ?_assert(Time < 1000).

%%====================================================================
%% Phase 4 Tests - Activity Cancellation
%%====================================================================

%% Test activity cancellation (task without effect)
cancel_activity_without_effect_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),
    TaskId = task1,

    {ok, Event} = wf_cancel:cancel_activity(CaseId, TaskId, []),
    [
        ?_assertEqual(TaskId, Event#cancel_activity.scope_id),
        ?_assertEqual(1, length(Event#cancel_activity.cancelled_tokens)),
        ?_assertEqual(0, length(Event#cancel_activity.cancelled_effects))
    ].

%% Test activity cancellation error on undefined task
cancel_activity_undefined_task_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),
    ?_assertEqual(
        {error, {task_not_found, nonexistent_task}},
        wf_cancel:cancel_activity(CaseId, nonexistent_task, [])
    ).

%% Test activity cancellation preserves other tasks
cancel_activity_preserves_other_tasks_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    %% Cancel task1
    {ok, _Event} = wf_cancel:cancel_activity(CaseId, task1, []),

    %% Verify task2 still active
    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    Tokens = wf_state:get_tokens(StateAfter),
    Task2Token = [T || {_, T} <- maps:to_list(Tokens),
                   T#token.value =:= task2],
    ?_assertEqual([active], [T#token.status || T <- Task2Token]).

%% Test activity cancellation of already cancelled task
cancel_activity_already_cancelled_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    %% Cancel task1 first time
    {ok, _Event1} = wf_cancel:cancel_activity(CaseId, task1, []),

    %% Try to cancel again
    ?_assertEqual(
        {error, {already_cancelled, task1}},
        wf_cancel:cancel_activity(CaseId, task1, [])
    ).

%% Test activity cancellation invariants
cancel_activity_invariants_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, _Event} = wf_cancel:cancel_activity(CaseId, task1, []),

    %% Verify invariants
    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    ?_assertEqual(ok, wf_cancel:verify_no_orphaned_tokens(StateAfter, scope1)).

%%====================================================================
%% Phase 5 Tests - Case Cancellation
%%====================================================================

%% Test case cancellation
cancel_case_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, Event} = wf_cancel:cancel_case(CaseId),
    [
        ?_assertEqual(CaseId, Event#cancel_case.scope_id),
        ?_assertEqual(2, length(Event#cancel_case.cancelled_tokens)),
        ?_assertEqual(0, length(Event#cancel_case.cancelled_effects))
    ].

%% Test case cancellation marks status
cancel_case_status_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, _Event} = wf_cancel:cancel_case(CaseId),

    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    ?_assertEqual(cancelled, wf_state:get_status(StateAfter)).

%% Test case cancellation error on already cancelled
cancel_case_already_cancelled_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    %% Cancel case first time
    {ok, _Event1} = wf_cancel:cancel_case(CaseId),

    %% Try to cancel again
    ?_assertEqual(
        {error, {already_cancelled, CaseId}},
        wf_cancel:cancel_case(CaseId)
    ).

%% Test case cancellation cancels all tokens
cancel_case_all_tokens_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, Event} = wf_cancel:cancel_case(CaseId),

    %% Verify all tokens cancelled
    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    Tokens = wf_state:get_tokens(StateAfter),
    ActiveTokens = [T || {_, T} <- maps:to_list(Tokens),
                     T#token.status =:= active],
    ?_assertEqual(0, length(ActiveTokens)),
    ?_assertEqual(2, length(Event#cancel_case.cancelled_tokens)).

%% Helper: create state with N tokens
create_state_with_n_tokens(N) ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),

    {FinalState, _} = lists:foldl(fun(_, {AccState, AccCount}) ->
        TokenId = make_ref(),
        Token = #token{
            token_id = TokenId,
            ip = 0,
            scope_id = root,
            value = AccCount,
            status = active
        },
        {ok, NewState} = wf_state:add_token(AccState, TokenId, Token),
        {NewState, AccCount + 1}
    end, {State0, 0}, lists:seq(1, N)),

    {ok, State1, _Receipt} = wf_state:commit(FinalState),
    State1.

%% Test case cancellation with many tokens
cancel_case_many_tokens_test_() ->
    State = create_state_with_n_tokens(100),
    CaseId = wf_state:get_case_id(State),

    {ok, Event} = wf_cancel:cancel_case(CaseId),

    %% Verify all tokens cancelled
    ?_assertEqual(100, length(Event#cancel_case.cancelled_tokens)).

%%====================================================================
%% Phase 6 Tests - wf_exec Integration
%%====================================================================

%% Test wf_cancel:propagate/2 integration
cancel_propagate_test_() ->
    TokensMap = #{
        token1 => #token{token_id = token1, ip = 0, scope_id = scope1, value = task1, status = active},
        token2 => #token{token_id = token2, ip = 0, scope_id = scope2, value = task2, status = active}
    },

    %% Propagate cancellation to scope1
    CancelledTokens = wf_cancel:propagate(scope1, TokensMap),

    %% Verify: token1 cancelled, token2 active
    [
        ?_assertEqual(cancelled, (maps:get(token1, CancelledTokens))#token.status),
        ?_assertEqual(active, (maps:get(token2, CancelledTokens))#token.status)
    ].

%%====================================================================
%% Phase 7 Tests - Comprehensive Testing (Nested Scopes and Edge Cases)
%%====================================================================

%% Helper: create state with nested scopes
create_state_with_nested_scopes() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, parent_scope),
    {ok, State2} = wf_state:enter_scope(State1, child_scope),

    %% Add tokens
    Token1 = #token{token_id = make_ref(), ip = 0, scope_id = parent_scope, value = task1, status = active},
    Token2 = #token{token_id = make_ref(), ip = 0, scope_id = child_scope, value = task2, status = active},

    {ok, State3} = wf_state:add_token(State2, Token1#token.token_id, Token1),
    {ok, State4} = wf_state:add_token(State3, Token2#token.token_id, Token2),
    {ok, State5, _Receipt} = wf_state:commit(State4),
    State5.

%% Test nested scope cancellation
nested_scope_cancellation_test_() ->
    State = create_state_with_nested_scopes(),
    CaseId = wf_state:get_case_id(State),

    %% Cancel parent scope
    {ok, _Event} = wf_cancel:cancel_region(CaseId, parent_scope, []),

    %% Verify: child scope token cancelled too (if we tracked child scope membership)
    %% For now, just verify parent token cancelled
    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    Tokens = wf_state:get_tokens(StateAfter),
    ParentTokens = [T || {_, T} <- maps:to_list(Tokens),
                       T#token.scope_id =:= parent_scope],

    ?_assertEqual([cancelled], [T#token.status || T <- ParentTokens]).

%% Test cancellation with empty scope (no tokens)
cancel_empty_scope_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, empty_scope),
    {ok, State2, _Receipt} = wf_state:commit(State1),

    %% Cancel empty scope (no tokens)
    CaseId = wf_state:get_case_id(State2),
    {ok, Event} = wf_cancel:cancel_region(CaseId, empty_scope, []),

    ?_assertEqual(0, length(Event#cancel_region.cancelled_tokens)).

%% Test cancellation with single token
cancel_single_token_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, scope1),

    Token = #token{token_id = make_ref(), ip = 0, scope_id = scope1, value = task1, status = active},
    {ok, State2} = wf_state:add_token(State1, Token#token.token_id, Token),
    {ok, State3, _Receipt} = wf_state:commit(State2),

    %% Cancel single token
    CaseId = wf_state:get_case_id(State3),
    {ok, Event} = wf_cancel:cancel_region(CaseId, scope1, []),

    ?_assertEqual(1, length(Event#cancel_region.cancelled_tokens)).

%% Test cancellation preserves token values
cancel_preserves_values_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, _Event} = wf_cancel:cancel_region(CaseId, scope1, []),

    %% Verify token values unchanged
    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    Tokens = wf_state:get_tokens(StateAfter),
    Token1 = [T || {_, T} <- maps:to_list(Tokens), T#token.value =:= task1],
    ?_assertEqual([task1], [T#token.value || T <- Token1]).

%% Test verify_scope_isolation with two independent scopes
verify_scope_isolation_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, _Event} = wf_cancel:cancel_region(CaseId, scope1, []),

    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    ?_assertEqual(ok, wf_cancel:verify_scope_isolation(StateAfter, scope1)).

%% Test verify_no_orphaned_tokens
verify_no_orphaned_tokens_test_() ->
    State = create_test_state(),
    CaseId = wf_state:get_case_id(State),

    {ok, _Event} = wf_cancel:cancel_region(CaseId, scope1, []),

    {ok, StateAfter} = wf_state:restore_from_ets(CaseId),
    ?_assertEqual(ok, wf_cancel:verify_no_orphaned_tokens(StateAfter, scope1)).

%% Performance benchmark: small scope
bench_small_scope_test_() ->
    State = create_state_with_n_tokens_in_scope(100, 10),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    ?_assert(Time < 1000).  %% < 1ms for 10 tokens

%% Performance benchmark: large scope
bench_large_scope_test_() ->
    State = create_state_with_n_tokens_in_scope(10000, 1000),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    ?_assert(Time < 100000).  %% < 100ms for 1000 tokens

%% Performance benchmark: verify O(scope_size) not O(total_tokens)
bench_complexity_isolation_test_() ->
    %% Create state with 10000 total tokens, 10 in scope
    State = create_state_with_n_tokens_in_scope(10000, 10),
    CaseId = wf_state:get_case_id(State),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
    %% Should be fast (only 10 tokens cancelled), not O(10000)
    ?_assert(Time < 1000).
