%%%-------------------------------------------------------------------
%%% @doc wf_cancel - Structured Cancellation Semantics
%%%
%%% This module implements three granularities of cancellation for workflow
%%% execution:
%%%
%%% == Activity Cancellation ==
%%% `cancel_activity(CaseId, TaskId, Options)` - Cancel a single task
%%% - If task has yielded effect, effect is cancelled via wf_effect
%%% - If task not yet started, token is marked cancelled (skipped)
%%% - Produces #cancel_activity{} event with audit trail
%%%
%%% == Case Cancellation ==
%%% `cancel_case(CaseId)` - Cancel entire workflow instance
%%% - All active tokens and pending effects cancelled
%%% - Case transitions to terminal cancelled state
%%% - Produces #cancel_case{} event with audit trail
%%%
%%% == Region Cancellation ==
%%% `cancel_region(CaseId, ScopeId, Options)` - Cancel scoped subtree
%%% - Only tokens within scope are cancelled
%%% - Tokens outside scope unaffected (invariant preserved)
%%% - Produces #cancel_region{} event with audit trail
%%%
%%% == Performance Characteristics ==
%%% Cancellation propagation is O(scope_size), not O(workflow_size).
%%% Uses #scope.tokens list for efficient token lookup without scanning
%%% all tokens in the workflow.
%%%
%%% == State Consistency ==
%%% Uses wf_state's atomic commit protocol for state consistency.
%%% All mutations (token updates, scope status changes) are applied
%%% atomically via commit/1. Cancellation cannot corrupt state.
%%%
%%% == Event Production ==
%%% Each cancellation produces structured audit events:
%%% - scope_id: Identifier of cancelled scope
%%% - cancelled_tokens: List of token IDs cancelled
%%% - cancelled_effects: List of effect IDs cancelled
%%% - timestamp: When cancellation occurred
%%%
%%% == Limitations ==
%%% - Effect cancellation stubbed (wf_effect not implemented, item 010)
%%% - Activity cancellation uses O(n) token scan (documented for v2)
%%% - wf_exec integration partial (executor has inline state, not wf_state)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_cancel).

%% API exports
-export([
    cancel_activity/3,
    cancel_case/1,
    cancel_region/3,
    is_cancelled/2,
    propagate/2,
    verify_scope_isolation/2,
    verify_scope_nesting/2,
    verify_no_orphaned_tokens/2
]).

%% Type exports
-export_type([
    cancel_event/0,
    cancel_scope/0
]).

%%====================================================================
%% Types
%%====================================================================

-type task_id() :: term().
-type case_id() :: term().
-type scope_id() :: term().
-type token_id() :: term().
-type effect_id() :: term().

%% Cancellation scope types
-type cancel_scope() ::
    {activity, task_id()} |
    {case_cancel, case_id()} |
    {region, scope_id()}.

%% Cancel event records
-record(cancel_activity, {
    scope_id :: task_id(),
    cancelled_tokens :: [token_id()],
    cancelled_effects :: [effect_id()],
    timestamp :: erlang:timestamp()
}).

-record(cancel_case, {
    scope_id :: case_id(),
    cancelled_tokens :: [token_id()],
    cancelled_effects :: [effect_id()],
    timestamp :: erlang:timestamp()
}).

-record(cancel_region, {
    scope_id :: scope_id(),
    cancelled_tokens :: [token_id()],
    cancelled_effects :: [effect_id()],
    timestamp :: erlang:timestamp()
}).

-type cancel_event() :: #cancel_activity{} | #cancel_case{} | #cancel_region{}.

%% Include wf_state records
-include_lib("wf_state.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Cancel a single task (activity)
%% If task has yielded an effect, effect is cancelled.
%% If task not yet started, token is marked cancelled (skipped).
-spec cancel_activity(case_id(), task_id(), proplists:proplist()) ->
    {ok, #cancel_activity{}} | {error, term()}.
cancel_activity(CaseId, TaskId, _Options) ->
    case wf_state:restore_from_ets(CaseId) of
        {error, Reason} ->
            {error, {state_restore_failed, Reason}};
        {ok, State} ->
            do_cancel_activity(State, TaskId)
    end.

%% @private Perform activity cancellation
do_cancel_activity(State, TaskId) ->
    %% Find token executing task (O(n) scan)
    TokensMap = wf_state:get_tokens(State),
    case find_token_for_task(TokensMap, TaskId) of
        undefined ->
            {error, {task_not_found, TaskId}};
        {ok, Token} ->
            %% Check if task already cancelled
            case Token#token.status of
                cancelled ->
                    {error, {already_cancelled, TaskId}};
                _ ->
                    %% Check if task yielded effect (stub for wf_effect)
                    case get_effect_for_token(Token) of
                        {ok, EffectId} ->
                            %% Cancel effect
                            case cancel_effect(EffectId) of
                                ok ->
                                    {ok, cancel_token_and_create_event(State, Token, [EffectId])};
                                {error, Reason} ->
                                    {error, {effect_cancel_failed, Reason}}
                            end;
                        undefined ->
                            %% Task not started, skip
                            {ok, cancel_token_and_create_event(State, Token, [])}
                    end
            end
    end.

%% @private Find token by task ID
%% O(n) scan - acceptable for v1, optimize in v2 with task_id -> token_id index
find_token_for_task(TokensMap, TaskId) ->
    Tokens = maps:to_list(TokensMap),
    case [T || {_, T} <- Tokens, T#token.value =:= TaskId] of
        [Token] -> {ok, Token};
        [] -> undefined
    end.

%% @private Get effect for token (stub for wf_effect)
get_effect_for_token(_Token) ->
    %% TODO: Integrate with wf_effect when item 010 implemented
    %% For v1, assume no effect yielded
    undefined.

%% @private Cancel effect (stub for wf_effect)
cancel_effect(_EffectId) ->
    %% TODO: Call wf_effect:cancel_effect/2 when item 010 implemented
    ok.

%% @private Cancel token and create event
cancel_token_and_create_event(State, Token, CancelledEffects) ->
    TokenId = Token#token.token_id,
    CancelledToken = Token#token{status = cancelled},

    %% Buffer and commit token update
    State1 = wf_state:buffer_mutation(
        State,
        {update_token, TokenId, fun(_) -> CancelledToken end}
    ),
    {ok, _State2, _Receipt} = wf_state:commit(State1),

    %% Create event
    Event = #cancel_activity{
        scope_id = Token#token.value,  %% Task ID
        cancelled_tokens = [TokenId],
        cancelled_effects = CancelledEffects,
        timestamp = erlang:timestamp()
    },
    {ok, Event}.

%% @doc Cancel an entire workflow instance
%% All active tokens and pending effects are cancelled.
%% Case transitions to terminal cancelled state.
-spec cancel_case(case_id()) -> {ok, #cancel_case{}} | {error, term()}.
cancel_case(CaseId) ->
    case wf_state:restore_from_ets(CaseId) of
        {error, Reason} ->
            {error, {state_restore_failed, Reason}};
        {ok, State} ->
            do_cancel_case(State)
    end.

%% @private Perform case cancellation
do_cancel_case(State) ->
    %% Check if already cancelled
    case wf_state:get_status(State) of
        cancelled ->
            {error, {already_cancelled, State#state.case_id}};
        _ ->
            %% Get all active tokens
            TokensMap = wf_state:get_tokens(State),
            ActiveTokens = [
                T || {_, T} <- maps:to_list(TokensMap),
                T#token.status =:= active
            ],

            %% Mark all tokens as cancelled
            {State1, CancelledTokenIds} = mark_all_tokens_cancelled(State, ActiveTokens),

            %% Cancel all effects (stub for wf_effect)
            CancelledEffectIds = cancel_effects_for_tokens(ActiveTokens),

            %% Mark case status as cancelled
            State2 = wf_state:buffer_mutation(State1, {set_case_status, cancelled}),
            {ok, _State3, _Receipt} = wf_state:commit(State2),

            %% Create event
            Event = #cancel_case{
                scope_id = State#state.case_id,
                cancelled_tokens = lists:reverse(CancelledTokenIds),
                cancelled_effects = CancelledEffectIds,
                timestamp = erlang:timestamp()
            },
            {ok, Event}
    end.

%% @private Mark all tokens as cancelled via mutations
mark_all_tokens_cancelled(State, Tokens) ->
    lists:foldl(fun(Token, {AccState, AccIds}) ->
        TokenId = Token#token.token_id,
        CancelledToken = Token#token{status = cancelled},
        NewState = wf_state:buffer_mutation(
            AccState,
            {update_token, TokenId, fun(_) -> CancelledToken end}
        ),
        {NewState, [TokenId | AccIds]}
    end, {State, []}, Tokens).

%% @doc Cancel a scoped region
%% Only tokens within the named scope are cancelled.
%% Tokens outside the scope are unaffected.
-spec cancel_region(case_id(), scope_id(), proplists:proplist()) ->
    {ok, #cancel_region{}} | {error, term()}.
cancel_region(CaseId, ScopeId, _Options) ->
    %% Restore state from ETS
    case wf_state:restore_from_ets(CaseId) of
        {error, Reason} ->
            {error, {state_restore_failed, Reason}};
        {ok, State} ->
            do_cancel_region(State, ScopeId)
    end.

%% @private Perform region cancellation
%% O(scope_size) complexity - uses #scope.tokens list to avoid scanning all tokens
do_cancel_region(State, ScopeId) ->
    %% Check scope exists and not already cancelled
    case wf_state:get_scope(State, ScopeId) of
        undefined ->
            {error, {scope_not_found, ScopeId}};
        #scope{status = cancelled} ->
            {error, {already_cancelled, ScopeId}};
        #scope{tokens = TokenIds} ->
            %% Get tokens in scope (O(scope_size) using scope#tokens list)
            %% This is the key optimization - we only touch tokens in the scope
            TokensMap = wf_state:get_tokens(State),
            TokensInScope = get_tokens_by_ids(TokensMap, TokenIds),

            %% Mark tokens as cancelled
            {State1, CancelledTokenIds} = mark_tokens_cancelled(State, TokensInScope),

            %% Cancel effects yielded by tokens (stub for wf_effect)
            CancelledEffectIds = cancel_effects_for_tokens(TokensInScope),

            %% Update scope status to cancelled
            State2 = wf_state:buffer_mutation(State1, {cancel_scope, ScopeId}),
            {ok, _State3, _Receipt} = wf_state:commit(State2),

            %% Create cancel event
            Event = #cancel_region{
                scope_id = ScopeId,
                cancelled_tokens = lists:reverse(CancelledTokenIds),
                cancelled_effects = CancelledEffectIds,
                timestamp = erlang:timestamp()
            },
            {ok, Event}
    end.

%% @private Get tokens by list of token IDs
get_tokens_by_ids(TokensMap, TokenIds) ->
    [maps:get(Id, TokensMap) || Id <- TokenIds].

%% @private Mark tokens as cancelled via mutations
mark_tokens_cancelled(State, Tokens) ->
    lists:foldl(fun(Token, {AccState, AccIds}) ->
        TokenId = Token#token.token_id,
        CancelledToken = Token#token{status = cancelled},
        NewState = wf_state:buffer_mutation(
            AccState,
            {update_token, TokenId, fun(_) -> CancelledToken end}
        ),
        {NewState, [TokenId | AccIds]}
    end, {State, []}, Tokens).

%% @private Cancel effects for tokens (stub for wf_effect)
cancel_effects_for_tokens(_Tokens) ->
    %% TODO: Integrate with wf_effect when item 010 implemented
    %% For v1, assume no effects yielded
    [].

%% @doc Check if scope is cancelled
-spec is_cancelled(wf_state:state(), scope_id()) -> boolean().
is_cancelled(State, ScopeId) ->
    case wf_state:get_scope(State, ScopeId) of
        #scope{status = cancelled} -> true;
        _ -> false
    end.

%% @doc Propagate cancellation to tokens in scope
%% For use by wf_exec when scope exits
-spec propagate(scope_id(), #{term() => #token{}}) ->
    #{term() => #token{}}.
propagate(ScopeId, TokensMap) ->
    %% Mark tokens in scope as cancelled
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).

%%====================================================================
%% Invariant Verification (for testing)
%%====================================================================

%% @doc Verify scope isolation: unrelated scopes unaffected
-spec verify_scope_isolation(wf_state:state(), scope_id()) -> ok | {error, term()}.
verify_scope_isolation(State, CancelledScopeId) ->
    Tokens = wf_state:get_tokens(State),
    ActiveTokensOutsideScope = [
        {TId, T} || {TId, T} <- maps:to_list(Tokens),
        T#token.scope_id =/= CancelledScopeId,
        T#token.status =:= active
    ],
    case ActiveTokensOutsideScope of
        [] -> ok;
        _ -> {error, {tokens_corrupted, ActiveTokensOutsideScope}}
    end.

%% @doc Verify scope nesting: child scopes cancelled when parent cancelled
-spec verify_scope_nesting(wf_state:state(), scope_id()) -> ok | {error, term()}.
verify_scope_nesting(State, ParentScopeId) ->
    ScopesMap = wf_state:get_scopes(State),
    ChildScopes = [
        S || {_, S} <- maps:to_list(ScopesMap),
        S#scope.parent_scope =:= ParentScopeId
    ],
    CancelledChildren = [
        S || S <- ChildScopes,
        S#scope.status =:= cancelled
    ],
    case length(CancelledChildren) =:= length(ChildScopes) of
        true -> ok;
        false -> {error, {child_scopes_not_cancelled, ChildScopes}}
    end.

%% @doc Verify no orphaned tokens: all cancelled tokens in cancelled scope
-spec verify_no_orphaned_tokens(wf_state:state(), scope_id()) -> ok | {error, term()}.
verify_no_orphaned_tokens(State, CancelledScopeId) ->
    Tokens = wf_state:get_tokens(State),
    CancelledTokens = [
        {TId, T} || {TId, T} <- maps:to_list(Tokens),
        T#token.status =:= cancelled
    ],
    OrphanedTokens = [
        {TId, T} || {TId, T} <- CancelledTokens,
        T#token.scope_id =/= CancelledScopeId
    ],
    case OrphanedTokens of
        [] -> ok;
        _ -> {error, {orphaned_cancelled_tokens, OrphanedTokens}}
    end.
