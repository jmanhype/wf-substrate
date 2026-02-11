%%%-------------------------------------------------------------------
%%% @doc Workflow State Store with Atomic Commit Protocol
%%%
%%% This module provides transactional state management for workflow execution.
%%% All state updates go through a buffer-apply-commit protocol:
%%%
%%% 1. Buffer: Mutations accumulated during reduction quantum
%%% 2. Validate: Consistency checks before applying
%%% 3. Apply: Mutations applied atomically (all-or-nothing)
%%% 4. Receipt: Immutable receipt produced for audit/replay
%%%
%%% == Crash Resilience ==
%%% State is persisted in ETS table on every commit. If process crashes,
%%% state can be restored from ETS. Crash mid-quantum leaves no partial
%%% state because mutations are buffered and not applied until commit.
%%%
%%% == Architecture ==
%%% - ETS table owned by separate gen_server (wf_state_store)
%%% - State is per-case (identified by case_id)
%%% - Single-writer (case runner) with concurrent readers
%%% - Receipts form append-only log for auditing
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_state).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    new/1,
    get_ctx/1,
    put_ctx/2,
    get_tokens/1,
    add_token/3,
    remove_token/2,
    enter_scope/2,
    exit_scope/2,
    get_scope/2,
    buffer_mutation/2,
    commit/1,
    rollback/1,
    snapshot/1,
    restore/2,
    restore_from_ets/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Type exports
-export_type([
    state/0,
    mutation/0,
    ctx/0,
    token_id/0,
    scope_id/0,
    case_id/0,
    receipt/0
]).

%%====================================================================
%% Records
%%====================================================================

%% Token record: Logical thread of execution (matches wf_exec structure)
-record(token, {
    token_id :: token_id(),
    ip :: non_neg_integer(),
    scope_id :: scope_id(),
    value :: term(),
    status :: active | complete | cancelled
}).

%% Scope record: Cancel scope
-record(scope, {
    scope_id :: scope_id(),
    parent_scope :: scope_id() | undefined,
    status :: active | cancelled,
    tokens :: [token_id()],                   %% Tokens in this scope
    entered_at :: erlang:timestamp()          %% Entry timestamp
}).

%% Metadata record: Execution metadata
-record(metadata, {
    step_count :: non_neg_integer(),
    start_time :: erlang:timestamp(),
    last_commit_time :: erlang:timestamp() | undefined
}).

%% Mutation record: Single state mutation
-record(mutation, {
    id :: mutation_id(),                      %% Unique mutation ID
    type :: mutation(),                       %% Mutation type and data
    timestamp :: erlang:timestamp()           %% When buffered
}).

%% Receipt record: Stub for item 010 integration
-record(receipt, {
    receipt_id :: term(),
    case_id :: case_id(),
    mutations :: [#mutation{}],
    timestamp :: erlang:timestamp(),
    state_before_hash :: binary(),
    state_after_hash :: binary()
}).

%% State record: Per-case execution state
-record(state, {
    case_id :: case_id(),                    %% Unique identifier
    ctx :: ctx(),                            %% User context (opaque map)
    tokens :: #{token_id() => #token{}},     %% Active tokens
    scopes :: #{scope_id() => #scope{}},     %% Cancel scopes
    metadata :: #metadata{},                  %% Execution metadata
    buffered_mutations :: [#mutation{}],      %% Buffered mutations
    ets_table :: ets:tid() | undefined       %% ETS table reference
}).

%%====================================================================
%% Types
%%====================================================================

-type ctx() :: map().
-type token_id() :: term().
-type scope_id() :: term().
-type case_id() :: term().
-type mutation_id() :: reference().

%% State type
-opaque state() :: #state{}.

%% Mutation type: Union of all possible mutations
-type mutation() ::
    {set_ctx, ctx()} |
    {update_ctx, fun((ctx()) -> ctx())} |
    {add_token, token_id(), #token{}} |
    {remove_token, token_id()} |
    {update_token, token_id(), fun((#token{}) -> #token{})} |
    {enter_scope, scope_id(), scope_id()} | %% {ScopeId, ParentScope}
    {exit_scope, scope_id()} |
    {cancel_scope, scope_id()} |
    {increment_step_count} |
    {set_metadata, term()}.

%% Receipt type (stub for item 010)
-type receipt() :: #receipt{}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Start state store server (owns ETS table)
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private gen_server init
init([]) ->
    %% Create ETS table (owned by this process)
    %% Records are stored as tuples, with the record name as element 1
    %% case_id is element 2, so keypos should be 2
    ets:new(wf_state_store, [
        named_table,
        set,
        {keypos, 2},  %% case_id is the second field in #state tuple
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    {ok, #{}}.

%% @private gen_server handle_call (unused)
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private gen_server handle_cast (unused)
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private gen_server handle_info (unused)
handle_info(_Info, State) ->
    {noreply, State}.

%% @private gen_server terminate
terminate(_Reason, _State) ->
    %% ETS table deleted automatically when process terminates
    ok.

%% @private gen_server code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new state store with initial context
-spec new(ctx()) -> {ok, state()}.
new(InitialCtx) when is_map(InitialCtx) ->
    CaseId = make_ref(),
    RootScopeId = root,
    RootScope = #scope{
        scope_id = RootScopeId,
        parent_scope = undefined,
        status = active,
        tokens = [],
        entered_at = erlang:timestamp()
    },
    Metadata = #metadata{
        step_count = 0,
        start_time = erlang:timestamp(),
        last_commit_time = undefined
    },
    %% Get ETS table reference
    EtsTable = whereis(wf_state_store),
    State = #state{
        case_id = CaseId,
        ctx = InitialCtx,
        tokens = #{},
        scopes = #{RootScopeId => RootScope},
        metadata = Metadata,
        buffered_mutations = [],
        ets_table = EtsTable
    },
    %% Persist to ETS
    case ets:lookup(wf_state_store, CaseId) of
        [] ->
            ets:insert(wf_state_store, State);
        [_] ->
            error({case_id_already_exists, CaseId})
    end,
    {ok, State}.

%% @doc Get current context
-spec get_ctx(state()) -> ctx().
get_ctx(#state{ctx = Ctx}) ->
    Ctx.

%% @doc Get all tokens
-spec get_tokens(state()) -> #{token_id() => #token{}}.
get_tokens(#state{tokens = Tokens}) ->
    Tokens.

%% @doc Get scope by ID (returns undefined if not found)
-spec get_scope(state(), scope_id()) -> #scope{} | undefined.
get_scope(#state{scopes = Scopes}, ScopeId) ->
    maps:get(ScopeId, Scopes, undefined).

%% @doc Update context (buffers mutation)
-spec put_ctx(state(), ctx()) -> {ok, state()}.
put_ctx(State, NewCtx) when is_map(NewCtx) ->
    Mutation = {set_ctx, NewCtx},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @doc Add token (buffers mutation)
-spec add_token(state(), token_id(), #token{}) -> {ok, state()}.
add_token(State, TokenId, Token) when is_record(Token, token) ->
    Mutation = {add_token, TokenId, Token},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @doc Remove token (buffers mutation)
-spec remove_token(state(), token_id()) -> {ok, state()}.
remove_token(State, TokenId) ->
    Mutation = {remove_token, TokenId},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @doc Enter cancel scope (buffers mutation)
-spec enter_scope(state(), scope_id()) -> {ok, state()}.
enter_scope(State, ScopeId) ->
    %% Parent scope is root for now
    %% In future, maintain explicit scope stack
    ParentScope = root,
    Mutation = {enter_scope, ScopeId, ParentScope},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @doc Exit cancel scope (buffers mutation)
-spec exit_scope(state(), scope_id()) -> {ok, state()}.
exit_scope(State, ScopeId) ->
    Mutation = {exit_scope, ScopeId},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @doc Buffer a mutation during reduction quantum
%% Mutations are accumulated and applied atomically at commit/1.
-spec buffer_mutation(state(), mutation()) -> state().
buffer_mutation(State, Mutation) ->
    MutationRecord = #mutation{
        id = make_ref(),
        type = Mutation,
        timestamp = erlang:timestamp()
    },
    BufferedMutations = [MutationRecord | State#state.buffered_mutations],
    NewState = State#state{buffered_mutations = BufferedMutations},
    %% Warn if buffer is large (> 1000 mutations)
    case length(BufferedMutations) of
        N when N > 1000 ->
            %% Log warning (use logger in production)
            io:format("WARNING: Large mutation buffer: ~p mutations~n", [N]),
            NewState;
        _ ->
            NewState
    end.

%% @doc Commit buffered mutations atomically
%% Returns {ok, NewState, Receipt} on success, {error, Reason} on validation failure.
-spec commit(state()) -> {ok, state(), receipt()} | {error, term()}.
commit(State) ->
    Mutations = lists:reverse(State#state.buffered_mutations),
    case validate_mutations(Mutations, State) of
        ok ->
            try
                %% Apply mutations atomically
                StateBefore = hash_state(State),
                AppliedState = apply_mutations(Mutations, State),
                StateAfter = hash_state(AppliedState),

                %% Persist to ETS
                ets:insert(wf_state_store, AppliedState),

                %% Create receipt
                Receipt = #receipt{
                    receipt_id = make_ref(),
                    case_id = AppliedState#state.case_id,
                    mutations = Mutations,
                    timestamp = erlang:timestamp(),
                    state_before_hash = StateBefore,
                    state_after_hash = StateAfter
                },

                %% Clear buffer and update last_commit_time
                Metadata = AppliedState#state.metadata,
                UpdatedMetadata = Metadata#metadata{
                    last_commit_time = erlang:timestamp()
                },
                FinalState = AppliedState#state{
                    buffered_mutations = [],
                    metadata = UpdatedMetadata
                },

                {ok, FinalState, Receipt}
            catch
                Error:Reason:Stacktrace ->
                    %% Rollback on any exception
                    error({commit_failed, Error, Reason, Stacktrace})
            end;
        {error, Reasons} ->
            {error, {validation_failed, Reasons}}
    end.

%% @doc Rollback buffered mutations (discard without applying)
-spec rollback(state()) -> state().
rollback(State) ->
    State#state{buffered_mutations = []}.

%% @doc Snapshot state to binary (for debugging/replay)
%% Note: Non-serializable fields (buffered_mutations, ets_table) are excluded.
-spec snapshot(state()) -> binary().
snapshot(State) ->
    %% Filter out non-serializable fields
    CleanState = State#state{
        buffered_mutations = [],  %% Don't snapshot buffered mutations
        ets_table = undefined      %% Don't snapshot ETS reference
    },
    term_to_binary(CleanState).

%% @doc Restore state from snapshot binary
-spec restore(binary(), case_id()) -> {ok, state()} | {error, term()}.
restore(Binary, CaseId) ->
    try binary_to_term(Binary) of
        State ->
            %% Verify case_id matches
            case State#state.case_id of
                CaseId ->
                    %% Reset ETS reference and buffered mutations
                    RestoredState = State#state{
                        buffered_mutations = [],
                        ets_table = whereis(wf_state_store)
                    },
                    {ok, RestoredState};
                _OtherId ->
                    {error, {case_id_mismatch, CaseId}}
            end
    catch
        _:_ ->
            {error, invalid_snapshot}
    end.

%% @doc Restore state from ETS (for crash recovery)
-spec restore_from_ets(case_id()) -> {ok, state()} | {error, not_found}.
restore_from_ets(CaseId) ->
    case ets:lookup(wf_state_store, CaseId) of
        [State] -> {ok, State};
        [] -> {error, not_found}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Hash state for receipt verification
-spec hash_state(state()) -> binary().
hash_state(State) ->
    %% Hash critical fields (ctx, tokens, scopes, metadata)
    Data = {
        State#state.ctx,
        State#state.tokens,
        State#state.scopes,
        State#state.metadata
    },
    erlang:md5(term_to_binary(Data)).

%% @private Validate mutations before applying
-spec validate_mutations([#mutation{}], state()) -> ok | {error, [term()]}.
validate_mutations(Mutations, State) ->
    case validate_all(Mutations, State, []) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

%% @private Validate all mutations, collect errors
-spec validate_all([#mutation{}], state(), [term()]) -> [term()].
validate_all([], _State, Errors) ->
    lists:usort(Errors);  %% Deduplicate errors
validate_all([Mutation | Rest], State, Errors) ->
    case validate_single_mutation(Mutation, State) of
        ok -> validate_all(Rest, State, Errors);
        {error, Reason} -> validate_all(Rest, State, [Reason | Errors])
    end.

%% @private Validate single mutation
-spec validate_single_mutation(#mutation{}, state()) -> ok | {error, term()}.
validate_single_mutation(#mutation{type = {set_ctx, NewCtx}}, _State) when is_map(NewCtx) ->
    ok;
validate_single_mutation(#mutation{type = {set_ctx, _InvalidCtx}}, _State) ->
    {error, {invalid_context, not_a_map}};

validate_single_mutation(#mutation{type = {add_token, TokenId, _Token}}, #state{tokens = Tokens}) ->
    case maps:is_key(TokenId, Tokens) of
        true -> {error, {token_already_exists, TokenId}};
        false -> ok
    end;

validate_single_mutation(#mutation{type = {remove_token, TokenId}}, #state{tokens = Tokens}) ->
    case maps:is_key(TokenId, Tokens) of
        false -> {error, {token_not_found, TokenId}};
        true -> ok
    end;

validate_single_mutation(#mutation{type = {update_token, TokenId, _Fun}}, #state{tokens = Tokens}) ->
    case maps:is_key(TokenId, Tokens) of
        false -> {error, {token_not_found, TokenId}};
        true -> ok
    end;

validate_single_mutation(#mutation{type = {enter_scope, ScopeId, _ParentScope}}, #state{scopes = Scopes}) ->
    case maps:is_key(ScopeId, Scopes) of
        true -> {error, {scope_already_exists, ScopeId}};
        false -> ok
    end;

validate_single_mutation(#mutation{type = {exit_scope, ScopeId}}, #state{scopes = Scopes}) ->
    case maps:is_key(ScopeId, Scopes) of
        false -> {error, {scope_not_found, ScopeId}};
        true -> ok
    end;

validate_single_mutation(#mutation{type = {cancel_scope, ScopeId}}, #state{scopes = Scopes}) ->
    case maps:is_key(ScopeId, Scopes) of
        false -> {error, {scope_not_found, ScopeId}};
        true -> ok
    end;

validate_single_mutation(#mutation{type = increment_step_count}, _State) ->
    ok;

validate_single_mutation(#mutation{type = {set_metadata, _Metadata}}, _State) ->
    ok;

validate_single_mutation(#mutation{type = {update_ctx, _Fun}}, _State) ->
    ok;

validate_single_mutation(_Mutation, _State) ->
    {error, unknown_mutation_type}.

%% @private Apply mutations atomically
-spec apply_mutations([#mutation{}], state()) -> state().
apply_mutations(Mutations, State) ->
    %% Apply mutations in order (they were buffered in reverse)
    lists:foldl(fun(Mutation, AccState) ->
        apply_single_mutation(Mutation, AccState)
    end, State, Mutations).

%% @private Apply single mutation
-spec apply_single_mutation(#mutation{}, state()) -> state().
apply_single_mutation(#mutation{type = {set_ctx, NewCtx}}, State) ->
    State#state{ctx = NewCtx};

apply_single_mutation(#mutation{type = {update_ctx, UpdateFun}}, State)
  when is_function(UpdateFun, 1) ->
    NewCtx = UpdateFun(State#state.ctx),
    State#state{ctx = NewCtx};

apply_single_mutation(#mutation{type = {add_token, TokenId, Token}}, State) ->
    Tokens = State#state.tokens,
    State#state{tokens = Tokens#{TokenId => Token}};

apply_single_mutation(#mutation{type = {remove_token, TokenId}}, State) ->
    Tokens = State#state.tokens,
    State#state{tokens = maps:remove(TokenId, Tokens)};

apply_single_mutation(#mutation{type = {update_token, TokenId, UpdateFun}}, State)
  when is_function(UpdateFun, 1) ->
    Tokens = State#state.tokens,
    Token = maps:get(TokenId, Tokens),
    UpdatedToken = UpdateFun(Token),
    State#state{tokens = Tokens#{TokenId => UpdatedToken}};

apply_single_mutation(#mutation{type = {enter_scope, ScopeId, ParentScope}}, State) ->
    Scopes = State#state.scopes,
    NewScope = #scope{
        scope_id = ScopeId,
        parent_scope = ParentScope,
        status = active,
        tokens = [],
        entered_at = erlang:timestamp()
    },
    State#state{scopes = Scopes#{ScopeId => NewScope}};

apply_single_mutation(#mutation{type = {exit_scope, ScopeId}}, State) ->
    Scopes = State#state.scopes,
    State#state{scopes = maps:remove(ScopeId, Scopes)};

apply_single_mutation(#mutation{type = {cancel_scope, ScopeId}}, State) ->
    Scopes = State#state.scopes,
    Scope = maps:get(ScopeId, Scopes),
    UpdatedScope = Scope#scope{status = cancelled},
    State#state{scopes = Scopes#{ScopeId => UpdatedScope}};

apply_single_mutation(#mutation{type = increment_step_count}, State) ->
    Metadata = State#state.metadata,
    UpdatedMetadata = Metadata#metadata{
        step_count = Metadata#metadata.step_count + 1
    },
    State#state{metadata = UpdatedMetadata};

apply_single_mutation(#mutation{type = {set_metadata, NewMetadata}}, State) ->
    State#state{metadata = NewMetadata}.
