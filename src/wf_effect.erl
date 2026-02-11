%%%-------------------------------------------------------------------
%%% @doc Effect System Boundary for External IO/Tool Calls
%%%
%%% This module implements the effect system that separates pure workflow
%%% execution from impure side effects. Tasks yield {effect, EffectSpec,
%%% Continuation} tuples, the executor suspends, and the effect system
%%% executes the effect asynchronously.
%%%
%%% == Effect Semantics ==
%%% - Cancelable: Effects can be cancelled when their scope is cancelled
%%% - Idempotent: Effects with idempotency_key return cached results
%%% - Causal Tracking: Each effect has unique ID derived from case_id + step_seq + scope
%%%
%%% == Architecture ==
%%% - Effects are submitted via yield/4
%%% - Effects execute asynchronously in spawned processes
%%% - Receipts are created on completion (success, failure, or cancellation)
%%% - Receipts are stored in append-only log (wf_receipt)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_effect).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    new_spec/5,
    set_idempotency_key/2,
    set_timeout/2,
    yield/4,
    cancel_effect/2,
    is_cancelled/1,
    get_result/1
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
    effect_id/0,
    effect_type/0,
    effect_spec/0,
    effect/0,
    case_id/0,
    step_seq/0,
    scope_id/0,
    idempotency_key/0,
    effect_status/0,
    effect_result/0
]).

%%====================================================================
%% Records
%%====================================================================

%% Effect specification (submitted by task)
-record(effect_spec, {
    effect_id :: {term(), non_neg_integer(), term()},
    effect_type :: atom(),
    payload :: term(),
    idempotency_key :: term() | undefined,
    timeout :: pos_integer() | undefined
}).

%% Active effect (running/completed/cancelled)
-record(effect, {
    effect_id :: {term(), non_neg_integer(), term()},
    spec :: effect_spec(),
    status :: pending | complete | cancelled | failed,
    submitted_at :: erlang:timestamp(),
    completed_at :: erlang:timestamp() | undefined,
    result :: {ok, term()} | {error, term()} | {cancelled, term()} | undefined,
    reason :: term() | undefined
}).

%%====================================================================
%% Types
%%====================================================================

%% Effect ID: Causal ID derived from case_id + step_seq + scope_id
-type effect_id() :: {case_id(), step_seq(), scope_id()}.

%% Effect type: Atom identifying the kind of effect
%% Examples: http_get, http_post, file_write, database_query, etc.
-type effect_type() :: atom().

%% Case ID: Unique identifier for a workflow case
-type case_id() :: term().

%% Step sequence: Execution step counter (monotonically increasing)
-type step_seq() :: non_neg_integer().

%% Scope ID: Cancellation scope identifier
-type scope_id() :: term().

%% Idempotency key: User-provided key for at-most-once semantics
-type idempotency_key() :: term().

%% Effect status: Lifecycle state
-type effect_status() :: pending | complete | cancelled | failed.

%% Effect result: Result of effect execution
-type effect_result() :: {ok, term()} | {error, term()} | {cancelled, term()}.

%% Effect spec: Opaque effect specification
-opaque effect_spec() :: #effect_spec{}.

%% Effect: Active effect record
-opaque effect() :: #effect{}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Start effect store server (owns ETS table)
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
    %% Create ETS table for active effects
    ets:new(wf_effects, [
        named_table,
        set,
        {keypos, #effect.effect_id},
        public,
        {read_concurrency, true},
        {write_concurrency, true}
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

%% @doc Create new effect specification
%% Components: case_id, step_seq, scope_id, effect_type, payload
-spec new_spec(case_id(), step_seq(), scope_id(), effect_type(), term()) -> effect_spec().
new_spec(CaseId, StepSeq, ScopeId, EffectType, Payload) ->
    EffectId = {CaseId, StepSeq, ScopeId},
    #effect_spec{
        effect_id = EffectId,
        effect_type = EffectType,
        payload = Payload,
        idempotency_key = undefined,
        timeout = undefined
    }.

%% @doc Set idempotency key on effect spec
-spec set_idempotency_key(effect_spec(), idempotency_key()) -> effect_spec().
set_idempotency_key(EffectSpec, IdempotencyKey) ->
    EffectSpec#effect_spec{idempotency_key = IdempotencyKey}.

%% @doc Set timeout on effect spec (milliseconds)
-spec set_timeout(effect_spec(), pos_integer()) -> effect_spec().
set_timeout(EffectSpec, Timeout) ->
    EffectSpec#effect_spec{timeout = Timeout}.

%% @doc Submit effect for execution (check idempotency first)
%% Returns {ok, Effect} or {ok, Receipt} (cached)
-spec yield(case_id(), step_seq(), scope_id(), effect_spec()) -> {ok, effect()} | {ok, term()}.
yield(CaseId, StepSeq, ScopeId, EffectSpec) ->
    EffectId = {CaseId, StepSeq, ScopeId},

    %% Check idempotency_key
    case EffectSpec#effect_spec.idempotency_key of
        undefined ->
            %% No idempotency, execute new effect
            execute_new_effect(EffectId, EffectSpec);
        IdempotencyKey ->
            %% Check for cached receipt
            case wf_receipt:lookup_by_key(CaseId, IdempotencyKey) of
                {ok, Receipt} ->
                    %% Cached result found, return receipt
                    {ok, Receipt};
                not_found ->
                    %% No cached result, execute new effect
                    execute_new_effect(EffectId, EffectSpec)
            end
    end.

%% @doc Cancel running effect
-spec cancel_effect(effect_id(), term()) -> ok | {error, term()}.
cancel_effect(EffectId, Reason) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = pending, spec = EffectSpec} = Effect] ->
            %% Mark effect as cancelled
            CancelledEffect = Effect#effect{
                status = cancelled,
                reason = Reason,
                completed_at = erlang:timestamp()
            },
            ets:insert(wf_effects, CancelledEffect),

            %% Create cancelled receipt
            Receipt = wf_receipt:new(EffectId, {cancelled, Reason}, 0),
            wf_receipt:store(Receipt, EffectSpec),
            ok;
        [#effect{status = complete}] ->
            %% Effect already completed, cannot cancel
            {error, {effect_already_complete, EffectId}};
        [#effect{status = cancelled}] ->
            %% Effect already cancelled
            {error, {effect_already_cancelled, EffectId}};
        [] ->
            {error, {effect_not_found, EffectId}}
    end.

%% @doc Check if effect is cancelled
-spec is_cancelled(effect_id()) -> boolean().
is_cancelled(EffectId) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = cancelled}] -> true;
        _ -> false
    end.

%% @doc Get effect result (polling interface for executor)
%% Returns effect_result() | pending | {error, effect_not_found}
-spec get_result(effect_id()) -> effect_result() | pending | {error, term()}.
get_result(EffectId) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = complete, result = Result}] ->
            Result;
        [#effect{status = pending}] ->
            pending;
        [#effect{status = cancelled, reason = Reason}] ->
            {cancelled, Reason};
        [#effect{status = failed, reason = Reason}] ->
            {error, Reason};
        [] ->
            {error, effect_not_found}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Execute new effect (async)
execute_new_effect(EffectId, EffectSpec) ->
    Effect = #effect{
        effect_id = EffectId,
        spec = EffectSpec,
        status = pending,
        submitted_at = erlang:timestamp(),
        completed_at = undefined,
        result = undefined,
        reason = undefined
    },

    %% Store effect in ETS
    ets:insert(wf_effects, Effect),

    %% Execute async (spawn_link for isolation)
    spawn_link(fun() ->
        StartTime = erlang:monotonic_time(microsecond),
        Result = execute_effect_handler(EffectSpec),
        EndTime = erlang:monotonic_time(microsecond),
        DurationUs = EndTime - StartTime,
        complete_effect(EffectId, Result, DurationUs)
    end),

    {ok, Effect}.

%% @private Execute effect handler (mock for v1)
execute_effect_handler(EffectSpec) ->
    Handler = get_handler_for_type(EffectSpec#effect_spec.effect_type),
    try
        Handler(EffectSpec)
    catch
        Type:Error:Stacktrace ->
            {error, {handler_crashed, Type, Error, Stacktrace}}
    end.

%% @private Get handler for effect type (expanded for v1)
get_handler_for_type(mock_effect) ->
    fun(_Spec) -> {ok, mock_result} end;
get_handler_for_type(mock_delay) ->
    fun(_Spec) ->
        timer:sleep(100),  %% Simulate IO delay
        {ok, delayed_result}
    end;
get_handler_for_type(mock_error) ->
    fun(_Spec) -> {error, mock_error} end;
get_handler_for_type(mock_crash) ->
    fun(_Spec) -> erlang:error(mock_crash) end;
get_handler_for_type(http_get) ->
    fun(Spec) ->
        Url = maps:get(url, Spec#effect_spec.payload),
        %% Mock HTTP GET
        {ok, #{status => 200, body => <<"response">>, url => Url}}
    end;
get_handler_for_type(file_write) ->
    fun(Spec) ->
        Path = maps:get(path, Spec#effect_spec.payload),
        Content = maps:get(content, Spec#effect_spec.payload),
        %% Mock file write
        {ok, #{path => Path, bytes_written => byte_size(Content)}}
    end;
get_handler_for_type(EffectType) ->
    fun(_Spec) -> {error, {unknown_effect_type, EffectType}} end.

%% @private Complete effect (create receipt)
complete_effect(EffectId, Result, DurationUs) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = pending, spec = EffectSpec} = Effect] ->
            %% Mark effect complete
            CompletedEffect = Effect#effect{
                status = complete,
                result = Result,
                completed_at = erlang:timestamp()
            },
            ets:insert(wf_effects, CompletedEffect),

            %% Create receipt
            Receipt = wf_receipt:new(EffectId, Result, DurationUs),
            wf_receipt:store(Receipt, EffectSpec),
            ok;
        [#effect{status = cancelled}] ->
            %% Effect was cancelled, discard result
            {cancelled, effect_cancelled};
        [] ->
            {error, effect_not_found}
    end.
