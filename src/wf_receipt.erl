%%%-------------------------------------------------------------------
%%% @doc Receipt Storage for Completed Effects
%%%
%%% This module implements an append-only receipt log for effect
%%% execution results. Each receipt contains:
%%% - receipt_id: Unique identifier
%%% - effect_id: Causal ID linking receipt to effect
%%% - effect_spec_hash: Hash of effect_spec for verification
%%% - timestamp: When effect completed
%%% - result: Effect execution result (ok/error/cancelled)
%%% - duration_us: Execution duration in microseconds
%%%
%%% == Receipt Semantics ==
%%% - Append-only: Receipts are never modified after creation
%%% - Verifiable: Hash ensures receipt matches effect_spec
%%% - Queryable: Lookup by effect_id or idempotency_key
%%% - Auditable: Form immutable audit trail per case
%%%
%%% == Architecture ==
%%% - Receipts stored in ETS table (ordered_set for append-only)
%%% - Table owned by wf_receipt gen_server (crash resilience)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_receipt).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    new/3,
    store/2,
    lookup/2,
    lookup_by_key/2,
    all/1,
    verify/2,
    hash_effect_spec/1
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
    receipt/0
]).

-include("wf_effect.hrl").

%%====================================================================
%% Records
%%====================================================================

%% Receipt record: Immutable receipt for completed effect
-record(receipt, {
    receipt_id,
    case_id,
    effect_id,
    effect_spec_hash,
    idempotency_key,
    timestamp,
    result,
    duration_us
}).

%%====================================================================
%% Types
%%====================================================================

-type receipt_id() :: term().
-type effect_id() :: {term(), non_neg_integer(), term()}.
-type case_id() :: term().
-type idempotency_key() :: term().
-type effect_spec() :: #effect_spec{}.
-opaque receipt() :: #receipt{}.

%%====================================================================
%% gen_server callbacks
%%====================================================================
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
    %% Create ETS table for receipts (append-only log)
    ets:new(wf_receipts, [
        named_table,
        ordered_set,  %% Ordered by receipt_id for append-only semantics
        {keypos, #receipt.receipt_id},
        public
    ]),
    {ok, #{}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    %% ETS table deleted automatically when process terminates
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new receipt from effect_id, result, duration
%% Note: Hash and idempotency_key filled by store/2
-spec new(effect_id(), {ok, term()} | {error, term()} | {cancelled, term()}, non_neg_integer()) -> receipt().
new(EffectId, Result, DurationUs) ->
    %% Extract case_id from effect_id tuple {CaseId, StepSeq, ScopeId}
    CaseId = element(1, EffectId),
    #receipt{
        receipt_id = make_ref(),
        case_id = CaseId,
        effect_id = EffectId,
        effect_spec_hash = <<>>,  %% Placeholder
        idempotency_key = undefined,  %% Placeholder
        timestamp = erlang:timestamp(),
        result = Result,
        duration_us = DurationUs
    }.

%% @doc Store receipt with effect_spec hash and idempotency_key
-spec store(receipt(), effect_spec()) -> ok.
store(Receipt, EffectSpec) ->
    %% Hash effect_spec for verification
    EffectSpecHash = hash_effect_spec(EffectSpec),

    %% Update receipt with hash and idempotency_key
    UpdatedReceipt = Receipt#receipt{
        effect_spec_hash = EffectSpecHash,
        idempotency_key = EffectSpec#effect_spec.idempotency_key
    },

    %% Store in ETS (append-only)
    ets:insert(wf_receipts, UpdatedReceipt),
    ok.

%% @doc Lookup receipt by case_id and effect_id
%% Returns the most recent receipt for this effect_id
-spec lookup(case_id(), effect_id()) -> {ok, receipt()} | not_found.
lookup(_CaseId, EffectId) ->
    Pattern = #receipt{effect_id = EffectId, _ = '_'},
    case ets:match_object(wf_receipts, Pattern) of
        [] -> not_found;
        Receipts ->
            %% Return the most recent receipt (last in ordered_set)
            {ok, lists:last(Receipts)}
    end.

%% @doc Lookup receipt by case_id and idempotency_key
-spec lookup_by_key(case_id(), idempotency_key()) -> {ok, receipt()} | not_found.
lookup_by_key(CaseId, IdempotencyKey) ->
    Pattern = #receipt{case_id = CaseId, idempotency_key = IdempotencyKey, _ = '_'},
    case ets:match_object(wf_receipts, Pattern) of
        [Receipt] -> {ok, Receipt};
        [] -> not_found
    end.

%% @doc Get all receipts for a case_id
-spec all(case_id()) -> [receipt()].
all(CaseId) ->
    Pattern = #receipt{case_id = CaseId, _ = '_'},
    ets:match_object(wf_receipts, Pattern).

%% @doc Verify receipt matches effect_spec (hash check)
-spec verify(receipt(), effect_spec()) -> boolean().
verify(Receipt, EffectSpec) ->
    ExpectedHash = hash_effect_spec(EffectSpec),
    Receipt#receipt.effect_spec_hash =:= ExpectedHash.

%% @doc Hash effect_spec for verification
-spec hash_effect_spec(effect_spec()) -> binary().
hash_effect_spec(EffectSpec) ->
    %% Hash critical fields (effect_id, effect_type, payload)
    Data = {
        EffectSpec#effect_spec.effect_id,
        EffectSpec#effect_spec.effect_type,
        EffectSpec#effect_spec.payload
    },
    crypto:hash(sha256, term_to_binary(Data)).
