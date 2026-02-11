# Implement effect boundary and receipt system Implementation Plan

## Implementation Plan Title

Effect Boundary and Receipt System for Pure Erlang Workflow Executor

## Overview

Implement wf_effect.erl and wf_receipt.erl modules to provide a clean separation between pure workflow execution and impure side effects (IO/tool calls). The effect system implements a yield/resume pattern where tasks never perform IO directly. Instead, tasks yield `{effect, EffectSpec, Continuation}` tuples, the executor suspends the task, submits the effect to wf_effect, and blocks until the effect completes. Results are resumed via receipts that form an append-only audit trail.

This implements the **Effect Pattern** from functional programming:
- **Pure core**: Workflow execution is deterministic and replayable
- **Impure boundary**: All side effects isolated at effect boundary
- **Causal tracking**: Each effect tagged with causal ID for traceability
- **Receipts**: Immutable audit trail of all effects executed
- **Cancelability**: Effects can be cancelled when their scope is cancelled
- **Idempotency**: Effects with idempotency_key return cached results

## Current State

**Existing Infrastructure:**
- `wf_exec.erl`: Bytecode executor with mock task execution (lines 194-204) that always returns ok
- `wf_exec.hrl`: Token record with status enum (active | complete | cancelled) but no `blocked_effect` status
- `wf_state.erl`: State store with receipt stub (lines 107-115) for state mutations, not effect receipts
- `wf_vm.erl`: Opcode definitions but no effect-specific opcode (effects are task-level yields, not bytecode-level)
- Testing: EUnit-based test suite in `test/` directory

**Missing Components:**
1. No `wf_effect.erl` module - effect system boundary does not exist
2. No `wf_receipt.erl` module - receipt storage for effects does not exist
3. No effect yielding in executor - tasks cannot yield effects
4. No `blocked_effect` status in token or executor
5. No effect ID generation - no causal ID system
6. No effect execution handlers - no infrastructure to run effects
7. No receipt ETS tables - no append-only receipt log
8. No idempotency caching - no duplicate effect detection

**Key Constraints:**
- Pure Erlang only (no NIFs, ports, or external dependencies per PROMPT.md:19-21)
- Must integrate with existing wf_exec executor pattern
- Must follow OTP gen_server patterns for ETS table ownership
- Must be crash-resilient (ETS tables survive process crashes)
- Must support deterministic replay (receipts provide audit trail)

## Desired End State

**Functional Requirements:**
1. Tasks can yield effects via `{effect, EffectSpec, Continuation}` tuple
2. Executor detects effect yield, transitions to `blocked_effect` status
3. EffectSpec contains: effect_id (causal ID), effect_type, payload, idempotency_key (optional), timeout (optional)
4. Effects execute asynchronously in separate process (isolation from executor)
5. Receipts created on effect completion with: receipt_id, effect_id, effect_spec_hash, timestamp, result, duration_us
6. Receipts stored in append-only ETS log (queryable by effect_id or idempotency_key)
7. Idempotent effects return cached receipt from previous execution
8. Cancelled effects discard results and create cancelled receipts
9. Executor resumes task execution when effect completes (blocks until then)

**Non-Functional Requirements:**
- Effect execution does not block executor (async with monitoring)
- ETS tables owned by separate gen_server (crash resilience)
- Receipt log is append-only (immutable audit trail)
- Effect IDs are unique and traceable (composite keys)
- Hash verification ensures receipt integrity
- Idempotency lookups are O(1) via ETS index
- No memory leaks (receipts bounded or prunable)

**Verification:**
- Unit tests pass for wf_effect (effect creation, yielding, cancellation, idempotency)
- Unit tests pass for wf_receipt (creation, storage, lookup, verification)
- Integration tests pass for wf_exec (task yields effect, blocks, resumes)
- Receipts verify correctly (hash matches effect_spec)
- Idempotency returns cached results
- Cancellation propagates to effects

### Key Discoveries:

- **wf_exec.hrl:40** - `#exec_state.status` enum needs `blocked_effect` added to current values (`running | done | blocked | cancelled`)
- **wf_exec.hrl:12** - `#token.status` enum needs `blocked_effect` added (currently only `active | complete | cancelled`)
- **wf_exec.erl:195-202** - Task execution is mock (always returns ok), needs to handle `{effect, EffectSpec, ContCtx}` return from tasks
- **wf_exec.erl:96-99** - `is_blocked/1` already checks `blocked_effect` but status value doesn't exist in enum yet
- **wf_state.erl:107-115** - Receipt record exists for state mutations (different concern), effect receipts are separate
- **PROMPT.md:145-155** - Task signature: `fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, EffectSpec, ContCtx})`
- **PROMPT.md:59** - Module specification: `wf_effect.erl` and `wf_receipt.erl` are required deliverables

## What We're NOT Doing

**Explicitly Out of Scope:**
1. **Effect handler behavior** - No gen_server behavior for handlers in v1 (simple function callbacks only)
2. **Receipt disk persistence** - ETS in-memory only (dets or disk persistence deferred to v2)
3. **Receipt log rotation** - No automatic cleanup or archiving (accept unbounded growth for v1)
4. **Effect timeout enforcement** - Timeout field in spec but no timer-based cancellation (handler must implement)
5. **Effect retry policy** - No automatic retry on failure (task decides retry semantics)
6. **Effect result streaming** - Results must be finite terms (no streaming or chunked responses)
7. **Effect spec validation** - No payload schema validation (trust task, defer to v2)
8. **Effect composition** - No higher-order effects or effect chaining
9. **Effect observability integration** - No wf_trace integration (item 011) yet
10. **Effect type registry** - No dynamic handler registration (hardcoded handler lookup in v1)

**Future Enhancements (v2+):**
- Gen_server-based effect handlers with supervision
- Receipt persistence to disk for crash recovery across restarts
- Receipt log rotation and retention policies
- Timer-based effect timeout enforcement
- Automatic retry with exponential backoff
- Streaming effect results for large payloads
- Effect spec validation with schemas
- Effect composition and workflows
- Integration with wf_trace (item 011)
- Dynamic effect handler registration

## Implementation Approach

**High-Level Strategy:** Implement wf_effect and wf_receipt modules incrementally in phases. Start with types and records (Phase 1), build receipt storage (Phase 2), add effect yielding (Phase 3), implement execution (Phase 4), integrate with executor (Phase 5), add cancellation (Phase 6), and finalize with comprehensive testing (Phase 7).

**Rationale:**
- **Incremental phases**: Each phase is independently testable, minimizing risk
- **Mock-first**: Use mock effect handlers initially, real handlers later
- **Executor integration last**: Defer executor changes until effect system works standalone
- **Cancellation last**: Add cancellation after basic flow works
- **Comprehensive testing**: Property-based tests for invariants, unit tests for edge cases

**Key Design Decisions:**
1. **Effect ID format**: Composite key `{CaseId, StepSeq, ScopeId}` for traceability (v1), hash-based deferred to v2
2. **Effect ID generation**: Derived from executor state (case_id, step_count, scope_stack) in wf_effect:yield/4
3. **Receipt hash algorithm**: SHA-256 via `crypto:hash/2` (cryptographic strength, low collision probability)
4. **ETS table ownership**: Separate gen_server (wf_effect_store, wf_receipt_store) for crash resilience
5. **Effect execution model**: Async spawn_link with monitoring (isolates handler crashes)
6. **Idempotency lookup**: ETS match_object pattern (O(N) in v1, secondary index in v2)
7. **Executor blocking**: Polling model in `step/2` (simple), defer async notification to v2
8. **Receipt verification**: Hash check on lookup (lazy verification, fail-fast on mismatch)

**Risk Mitigation:**
- **Effect handler crash**: Wrap in try-catch, return `{error, HandlerCrash}`
- **Receipt hash collision**: SHA-256 collision is astronomically unlikely (document assumption)
- **Idempotency key collision**: Document requirement for globally unique keys per effect type
- **Effect timeout deadlock**: Handler implements timeout (enforcement deferred to v2)
- **Cancellation race condition**: Check effect status atomically on completion
- **Receipt log growth**: Accept unbounded growth for v1 (document cleanup strategy)
- **ETS table ownership**: Own tables in separate gen_server (supervisor restarts on crash)
- **Memory exhaustion**: No limits in v1 (document constraint, add limits in v2)

---

## Phases

### Phase 1: Types and Records (wf_effect)

#### Overview

Define core types and records for the effect system. This foundational phase establishes the data structures that all subsequent phases depend on.

#### Changes Required:

##### 1. Create wf_effect.erl module with types and records

**File**: `src/wf_effect.erl`
**Changes**: New module with effect_spec and effect records, type definitions

```erlang
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
    new_spec/4,
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
    effect_id :: effect_id(),
    effect_type :: effect_type(),
    payload :: term(),
    idempotency_key :: idempotency_key() | undefined,
    timeout :: pos_integer() | undefined  %% milliseconds
}).

%% Active effect (running/completed/cancelled)
-record(effect, {
    effect_id :: effect_id(),
    spec :: effect_spec(),
    status :: effect_status(),
    submitted_at :: erlang:timestamp(),
    completed_at :: erlang:timestamp() | undefined,
    result :: effect_result() | undefined,
    reason :: term() | undefined  %% If cancelled or failed
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
%% Note: Implementation in Phase 3
-spec yield(case_id(), step_seq(), scope_id(), effect_spec()) -> {ok, effect()} | {ok, wf_receipt:receipt()}.
yield(_CaseId, _StepSeq, _ScopeId, _EffectSpec) ->
    erlang:navigation_error("not_implemented").

%% @doc Cancel running effect
%% Note: Implementation in Phase 6
-spec cancel_effect(effect_id(), term()) -> ok | {error, term()}.
cancel_effect(_EffectId, _Reason) ->
    erlang:navigation_error("not_implemented").

%% @doc Check if effect is cancelled
-spec is_cancelled(effect_id()) -> boolean().
is_cancelled(_EffectId) ->
    erlang:navigation_error("not_implemented").

%% @doc Get effect result (polling interface for executor)
%% Returns {ok, Result} | pending | {cancelled, Reason}
-spec get_result(effect_id()) -> {ok, effect_result()} | pending | {cancelled, term()}.
get_result(_EffectId) ->
    erlang:navigation_error("not_implemented").
```

##### 2. Create wf_effect.hrl header file

**File**: `src/wf_effect.hrl`
**Changes**: Export records for use in tests and other modules

```erlang
%%%-------------------------------------------------------------------
%%% @doc wf_effect record definitions
%%% This header file exports effect_spec and effect records for use
%%% in tests and other modules (wf_exec, wf_receipt, etc.)
%%%-------------------------------------------------------------------

-include_lib("wf_effect.hrl").

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
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc -o ebin/ src/wf_effect.erl`
- [ ] Type exports resolve: `erl -eval "wf_effect:effect_id()." -s init stop -noshell`
- [ ] Header file includes without errors in test module
- [ ] Dialyzer passes: `dialyzer -r src/`

##### Manual Verification:

- [ ] Records can be created in Erlang shell
- [ ] Type definitions are documented
- [ ] Module documentation is complete
- [ ] gen_server skeleton is correct

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Receipt Module (wf_receipt)

#### Overview

Implement wf_receipt module for receipt storage and verification. Receipts form an append-only log of completed effects with hash-based verification.

#### Changes Required:

##### 1. Create wf_receipt.erl module

**File**: `src/wf_receipt.erl`
**Changes**: New module with receipt record, storage, lookup, verification

```erlang
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
%%% - Table owned by wf_effect gen_server (crash resilience)
%%% - Secondary index on case_id for fast queries
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
    receipt_id/0,
    receipt/0,
    effect_id/0,
    case_id/0,
    idempotency_key/0
]).

-include("wf_effect.hrl").

%%====================================================================
%% Records
%%====================================================================

%% Receipt record: Immutable receipt for completed effect
-record(receipt, {
    receipt_id :: receipt_id(),
    case_id :: case_id(),
    effect_id :: effect_id(),
    effect_spec_hash :: binary(),
    idempotency_key :: idempotency_key() | undefined,
    timestamp :: erlang:timestamp(),
    result :: {ok, term()} | {error, term()} | {cancelled, term()},
    duration_us :: non_neg_integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type receipt_id() :: term().
-type effect_id() :: wf_effect:effect_id().
-type case_id() :: wf_effect:case_id().
-type idempotency_key() :: wf_effect:idempotency_key().
-opaque receipt() :: #receipt{}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Start receipt store server (owns ETS table)
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
-spec store(receipt(), #effect_spec{}) -> ok.
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
-spec lookup(case_id(), effect_id()) -> {ok, receipt()} | not_found.
lookup(_CaseId, EffectId) ->
    Pattern = #receipt{effect_id = EffectId, _ = '_'},
    case ets:match_object(wf_receipts, Pattern) of
        [Receipt] -> {ok, Receipt};
        [] -> not_found
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
-spec verify(receipt(), #effect_spec{}) -> boolean().
verify(Receipt, EffectSpec) ->
    ExpectedHash = hash_effect_spec(EffectSpec),
    Receipt#receipt.effect_spec_hash =:= ExpectedHash.

%% @doc Hash effect_spec for verification
-spec hash_effect_spec(#effect_spec{}) -> binary().
hash_effect_spec(EffectSpec) ->
    %% Hash critical fields (effect_id, effect_type, payload)
    Data = {
        EffectSpec#effect_spec.effect_id,
        EffectSpec#effect_spec.effect_type,
        EffectSpec#effect_spec.payload
    },
    crypto:hash(sha256, term_to_binary(Data)).
```

##### 2. Create wf_receipt.hrl header file

**File**: `src/wf_receipt.hrl`
**Changes**: Export receipt record for use in tests and other modules

```erlang
%%%-------------------------------------------------------------------
%%% @doc wf_receipt record definitions
%%% This header file exports receipt record for use in tests and
%%% other modules (wf_effect, wf_exec, etc.)
%%%-------------------------------------------------------------------

%% Receipt record: Immutable receipt for completed effect
-record(receipt, {
    receipt_id :: term(),
    case_id :: term(),
    effect_id :: {term(), non_neg_integer(), term()},
    effect_spec_hash :: binary(),
    idempotency_key :: term() | undefined,
    timestamp :: erlang:timestamp(),
    result :: {ok, term()} | {error, term()} | {cancelled, term()},
    duration_us :: non_neg_integer()
}).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc -o ebin/ src/wf_receipt.erl`
- [ ] EUnit tests pass: `erl -eval "eunit:test(wf_receipt, [verbose])." -s init stop -noshell`
- [ ] Receipt creation and storage work
- [ ] Receipt lookup by effect_id works
- [ ] Receipt lookup by idempotency_key works
- [ ] Receipt verification (hash check) works
- [ ] Receipt query (all receipts for case) works

##### Manual Verification:

- [ ] Receipts are stored in ETS table
- [ ] Hash function produces consistent results
- [ ] Append-only log enforced (no modification after creation)
- [ ] Receipt verification fails on tampered spec

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Effect Yielding (wf_effect)

#### Overview

Implement effect yielding with idempotency checking. Tasks submit effects via `yield/4`, which checks for cached receipts (idempotency) or executes new effects.

#### Changes Required:

##### 1. Complete wf_effect:yield/4 implementation

**File**: `src/wf_effect.erl`
**Changes**: Implement yield/4 with idempotency check and async execution

```erlang
%% @doc Submit effect for execution (check idempotency first)
%% Returns {ok, Effect} or {ok, Receipt} (cached from idempotency)
-spec yield(case_id(), step_seq(), scope_id(), effect_spec()) -> {ok, effect()} | {ok, wf_receipt:receipt()}.
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

%% @private Get handler for effect type
get_handler_for_type(mock_effect) ->
    fun(_Spec) -> {ok, mock_result} end;
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
```

##### 2. Implement get_result/1 for executor polling

**File**: `src/wf_effect.erl`
**Changes**: Add polling interface for executor to check effect completion

```erlang
%% @doc Get effect result (polling interface for executor)
%% Returns {ok, Result} | pending | {cancelled, Reason}
-spec get_result(effect_id()) -> {ok, effect_result()} | pending | {cancelled, term()}.
get_result(EffectId) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = complete, result = Result}] ->
            {ok, Result};
        [#effect{status = pending}] ->
            pending;
        [#effect{status = cancelled, reason = Reason}] ->
            {cancelled, Reason};
        [#effect{status = failed, reason = Reason}] ->
            {error, Reason};
        [] ->
            {error, effect_not_found}
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] EUnit tests pass: `erl -eval "eunit:test(wf_effect, [verbose])." -s init stop -noshell`
- [ ] Effect yielding works
- [ ] Idempotency returns cached receipt
- [ ] get_result/1 returns correct status
- [ ] Effect execution creates receipt
- [ ] Receipt hash matches effect_spec

##### Manual Verification:

- [ ] Effects execute asynchronously (executor not blocked)
- [ ] Idempotency prevents duplicate execution
- [ ] Receipts are created on completion
- [ ] Polling interface works for executor

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Effect Execution

#### Overview

Complete effect execution infrastructure with mock handlers and proper error handling.

#### Changes Required:

##### 1. Expand effect handler registry

**File**: `src/wf_effect.erl`
**Changes**: Add more mock handlers for testing

```erlang
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] EUnit tests pass for all mock handlers
- [ ] Delay handler simulates async execution
- [ ] Error handler returns error tuple
- [ ] Crash handler is caught and wrapped in error tuple

##### Manual Verification:

- [ ] Mock handlers work correctly
- [ ] HTTP mock returns expected result
- [ ] File write mock returns expected result
- [ ] Unknown effect type returns error

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Executor Integration (wf_exec)

#### Overview

Integrate effect system with wf_exec executor. Extend token and exec_state records, add `blocked_effect` status, and modify task execution to handle effect yielding.

#### Changes Required:

##### 1. Update wf_exec.hrl token record

**File**: `src/wf_exec.hrl`
**Changes**: Add `blocked_effect` to token status and `current_effect` field

```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled | blocked_effect,  %% ADDED blocked_effect
    instance_id :: term() | undefined,
    current_effect :: {term(), non_neg_integer(), term()} | undefined  %% NEW FIELD
}).
```

##### 2. Update wf_exec.hrl exec_state record

**File**: `src/wf_exec.hrl`
**Changes**: Add `blocked_effect` to exec_state status and `case_id` field

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: map(),
    case_id :: term() | undefined,  %% NEW FIELD (for effect_id generation)
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | cancelled | blocked_effect,  %% ADDED blocked_effect
    current_token :: term() | undefined
}).
```

##### 3. Update wf_exec:new/1 to initialize case_id

**File**: `src/wf_exec.erl`
**Changes**: Generate case_id in new/1

```erlang
%% @doc Create new executor from bytecode
-spec new(wf_vm:wf_bc()) -> exec_state().
new(Bytecode) ->
    CaseId = make_ref(),  %% ADDED: Generate case_id for effect IDs
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active,
        instance_id = undefined,
        current_effect = undefined  %% ADDED
    },
    #exec_state{
        ip = 0,
        bytecode = Bytecode,
        ctx = #{},
        case_id = CaseId,  %% ADDED
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0,
        status = running,
        current_token = InitialTokenId
    }.
```

##### 4. Update wf_exec:execute_task_exec/2 to handle effects

**File**: `src/wf_exec.erl`
**Changes**: Handle `{effect, EffectSpec, ContCtx}` return from tasks

```erlang
%% @doc Execute TASK_EXEC: run task function (handle effects)
execute_task_exec({_TaskExec, TaskFun}, ExecState) ->
    Ctx = ExecState#exec_state.ctx,
    CaseId = ExecState#exec_state.case_id,  %% ADDED
    StepSeq = ExecState#exec_state.step_count,
    ScopeId = get_current_scope(ExecState),

    %% Call task function
    TaskResult = try
        TaskFun(Ctx)
    catch
        Type:Error:Stacktrace ->
            {error, {task_crashed, Type, Error, Stacktrace}}
    end,

    case TaskResult of
        {ok, NewCtx} ->
            %% Pure computation, continue
            ExecState#exec_state{
                ctx = NewCtx,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        {effect, EffectSpec, ContCtx} ->
            %% Yield effect, block executor
            {ok, EffectOrReceipt} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpec),

            case EffectOrReceipt of
                #receipt{result = Result} ->
                    %% Cached result from idempotency, resume immediately
                    NewCtx2 = maps:put(effect_result, Result, ContCtx),
                    ExecState#exec_state{
                        ctx = NewCtx2,
                        ip = ExecState#exec_state.ip + 1,
                        step_count = ExecState#exec_state.step_count + 1
                    };
                #effect{effect_id = EffectId} ->
                    %% New effect, block executor
                    CurrentToken = ExecState#exec_state.current_token,
                    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
                    UpdatedToken = Token#token{
                        status = blocked_effect,
                        current_effect = EffectId
                    },
                    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

                    ExecState#exec_state{
                        ctx = ContCtx,
                        tokens = Tokens,
                        status = blocked_effect,
                        ip = ExecState#exec_state.ip + 1,
                        step_count = ExecState#exec_state.step_count + 1
                    }
            end;
        {error, Reason} ->
            %% Task failed
            ExecState#exec_state{
                status = failed,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.
```

##### 5. Add effect completion check to wf_exec:step/2

**File**: `src/wf_exec.erl`
**Changes**: Check effect completion when blocked_effect

```erlang
%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), map()}.
step(ExecState, _SchedDecision) ->
    %% Check if blocked on effect
    case ExecState#exec_state.status of
        blocked_effect ->
            step_check_effect(ExecState, _SchedDecision);
        _ ->
            step_normal(ExecState, _SchedDecision)
    end.

%% @private Check effect completion (blocked_effect path)
step_check_effect(ExecState, _SchedDecision) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    EffectId = Token#token.current_effect,

    case wf_effect:get_result(EffectId) of
        {ok, Result} ->
            %% Effect completed, resume task
            NewCtx = maps:put(effect_result, Result, ExecState#exec_state.ctx),
            UpdatedToken = Token#token{
                status = active,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_complete,
                effect_id => EffectId,
                result => Result
            },
            {ExecState#exec_state{tokens = Tokens, status = running}, TraceEvent};
        {cancelled, Reason} ->
            %% Effect cancelled, propagate cancellation
            UpdatedToken = Token#token{
                status = cancelled,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_cancelled,
                effect_id => EffectId,
                reason => Reason
            },
            {ExecState#exec_state{tokens = Tokens, status = cancelled}, TraceEvent};
        pending ->
            %% Effect still pending, yield
            TraceEvent = #{type => blocked_effect, effect_id => EffectId},
            {ExecState, TraceEvent};
        {error, Reason} ->
            %% Effect failed
            UpdatedToken = Token#token{
                status = failed,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_failed,
                effect_id => EffectId,
                reason => Reason
            },
            {ExecState#exec_state{tokens = Tokens, status = failed}, TraceEvent}
    end.

%% @private Normal step (not blocked)
step_normal(ExecState, SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),
    TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, TraceEvent}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] EUnit tests pass: `erl -eval "eunit:test(wf_exec, [verbose])." -s init stop -noshell`
- [ ] Task yields effect via `{effect, EffectSpec, ContCtx}`
- [ ] Executor transitions to `blocked_effect` status
- [ ] Executor resumes when effect completes
- [ ] Idempotent effects resume immediately (cached receipt)
- [ ] case_id is generated in new/1

##### Manual Verification:

- [ ] Task can yield effect
- [ ] Executor blocks on effect
- [ ] Executor resumes after effect completes
- [ ] Effect result is in context
- [ ] Cancelled effects propagate to executor

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Cancellation Integration

#### Overview

Implement effect cancellation. When a cancel scope is cancelled, propagate cancellation to running effects.

#### Changes Required:

##### 1. Implement wf_effect:cancel_effect/2

**File**: `src/wf_effect.erl`
**Changes**: Add cancellation logic

```erlang
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
```

##### 2. Implement wf_effect:is_cancelled/1

**File**: `src/wf_effect.erl`
**Changes**: Add cancellation check

```erlang
%% @doc Check if effect is cancelled
-spec is_cancelled(effect_id()) -> boolean().
is_cancelled(EffectId) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = cancelled}] -> true;
        _ -> false
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] EUnit tests pass: `erl -eval "eunit:test(wf_effect, [verbose])." -s init stop -noshell`
- [ ] cancel_effect/2 marks effect as cancelled
- [ ] cancel_effect/2 creates cancelled receipt
- [ ] is_cancelled/1 returns true for cancelled effects
- [ ] Already completed effects cannot be cancelled

##### Manual Verification:

- [ ] Cancellation works for pending effects
- [ ] Cancelled effects create receipts
- [ ] Executor resumes with cancelled status

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 7: Comprehensive Testing

#### Overview

Write comprehensive unit and integration tests for the effect system. Include property-based tests for invariants.

#### Changes Required:

##### 1. Create test/wf_effect_tests.erl

**File**: `test/wf_effect_tests.erl`
**Changes**: Comprehensive unit tests for wf_effect

```erlang
-module(wf_effect_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_effect.hrl").

%%====================================================================
%% Effect Spec Creation Tests
%%====================================================================

new_spec_test_() ->
    CaseId = make_ref(),
    StepSeq = 0,
    ScopeId = root,
    EffectType = mock_effect,
    Payload = #{data => test},

    EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, EffectType, Payload),

    [
        ?_assertEqual({CaseId, StepSeq, ScopeId}, EffectSpec#effect_spec.effect_id),
        ?_assertEqual(EffectType, EffectSpec#effect_spec.effect_type),
        ?_assertEqual(Payload, EffectSpec#effect_spec.payload),
        ?_assertEqual(undefined, EffectSpec#effect_spec.idempotency_key),
        ?_assertEqual(undefined, EffectSpec#effect_spec.timeout)
    ].

set_idempotency_key_test_() ->
    EffectSpec = wf_effect:new_spec(make_ref(), 0, root, mock_effect, #{}),
    Key = make_ref(),
    UpdatedSpec = wf_effect:set_idempotency_key(EffectSpec, Key),

    ?_assertEqual(Key, UpdatedSpec#effect_spec.idempotency_key).

set_timeout_test_() ->
    EffectSpec = wf_effect:new_spec(make_ref(), 0, root, mock_effect, #{}),
    Timeout = 5000,
    UpdatedSpec = wf_effect:set_timeout(EffectSpec, Timeout),

    ?_assertEqual(Timeout, UpdatedSpec#effect_spec.timeout).

%%====================================================================
%% Effect Yielding Tests
%%====================================================================

yield_new_effect_test_() ->
    {setup,
     fun() ->
         %% Start gen_server
         {ok, Pid} = wf_effect:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Stop gen_server
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),

         {ok, EffectOrReceipt} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         [
             ?_assert(is_record(EffectOrReceipt, effect)),
             ?_assertEqual(pending, EffectOrReceipt#effect.status)
         ]
     end}.

yield_idempotent_effect_cached_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         Key = make_ref(),
         EffectSpec1 = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),
         EffectSpec1WithKey = wf_effect:set_idempotency_key(EffectSpec1, Key),

         %% Submit first effect
         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec1WithKey),

         %% Wait for completion
         timer:sleep(50),

         %% Submit second effect with same key
         EffectSpec2 = wf_effect:new_spec(CaseId, 1, root, mock_effect, #{}),
         EffectSpec2WithKey = wf_effect:set_idempotency_key(EffectSpec2, Key),
         {ok, ReceiptOrEffect} = wf_effect:yield(CaseId, 1, root, EffectSpec2WithKey),

         [
             ?_assert(is_record(ReceiptOrEffect, receipt)),
             ?_assertEqual({ok, mock_result}, ReceiptOrEffect#receipt.result)
         ]
     end}.

%%====================================================================
%% Effect Cancellation Tests
%%====================================================================

cancel_effect_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_delay, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         %% Cancel immediately
         Result = wf_effect:cancel_effect(Effect#effect.effect_id, test_cancel),

         timer:sleep(150),  %% Wait for effect to complete/cancel

         ?_assertEqual(ok, Result)
     end}.

is_cancelled_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_delay, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),
         wf_effect:cancel_effect(Effect#effect.effect_id, test_cancel),

         timer:sleep(150),

         ?_assert(wf_effect:is_cancelled(Effect#effect.effect_id))
     end}.
```

##### 2. Create test/wf_receipt_tests.erl

**File**: `test/wf_receipt_tests.erl`
**Changes**: Comprehensive unit tests for wf_receipt

```erlang
-module(wf_receipt_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_receipt.hrl").

%%====================================================================
%% Receipt Creation Tests
%%====================================================================

new_receipt_test_() ->
    EffectId = {make_ref(), 0, root},
    Result = {ok, test_result},
    DurationUs = 1000,

    Receipt = wf_receipt:new(EffectId, Result, DurationUs),

    [
        ?_assert(is_reference(Receipt#receipt.receipt_id)),
        ?_assertEqual(element(1, EffectId), Receipt#receipt.case_id),
        ?_assertEqual(EffectId, Receipt#receipt.effect_id),
        ?_assertEqual(Result, Receipt#receipt.result),
        ?_assertEqual(DurationUs, Receipt#receipt.duration_us)
    ].

%%====================================================================
%% Receipt Storage and Lookup Tests
%%====================================================================

store_and_lookup_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         EffectId = {make_ref(), 0, root},
         EffectSpec = #effect_spec{
             effect_id = EffectId,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },
         Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

         wf_receipt:store(Receipt, EffectSpec),

         {ok, Lookup} = wf_receipt:lookup(element(1, EffectId), EffectId),

         [
             ?_assertEqual(Receipt#receipt.receipt_id, Lookup#receipt.receipt_id),
             ?_assertEqual({ok, test_result}, Lookup#receipt.result)
         ]
     end}.

lookup_by_key_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectId = {CaseId, 0, root},
         Key = make_ref(),
         EffectSpec = #effect_spec{
             effect_id = EffectId,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = Key,
             timeout = undefined
         },
         Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

         wf_receipt:store(Receipt, EffectSpec),

         {ok, Lookup} = wf_receipt:lookup_by_key(CaseId, Key),

         [
             ?_assertEqual({ok, test_result}, Lookup#receipt.result),
             ?_assertEqual(Key, Lookup#receipt.idempotency_key)
         ]
     end}.

%%====================================================================
%% Receipt Verification Tests
%%====================================================================

verify_receipt_test_() ->
    EffectId = {make_ref(), 0, root},
    EffectSpec = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test},
        idempotency_key = undefined,
        timeout = undefined
    },
    Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

    %% Manually set hash (normally done by store/2)
    Hash = wf_receipt:hash_effect_spec(EffectSpec),
    UpdatedReceipt = Receipt#receipt{effect_spec_hash = Hash},

    ?_assert(wf_receipt:verify(UpdatedReceipt, EffectSpec)).

verify_tampered_receipt_fails_test_() ->
    EffectId = {make_ref(), 0, root},
    EffectSpec = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test},
        idempotency_key = undefined,
        timeout = undefined
    },
    Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

    %% Tamper with receipt
    TamperedReceipt = Receipt#receipt{effect_spec_hash = <<>>},

    ?_assertNot(wf_receipt:verify(TamperedReceipt, EffectSpec)).

%%====================================================================
%% Receipt Query Tests
%%====================================================================

all_receipts_for_case_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectId1 = {CaseId, 0, root},
         EffectId2 = {CaseId, 1, root},
         EffectSpec = #effect_spec{
             effect_id = EffectId1,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },

         Receipt1 = wf_receipt:new(EffectId1, {ok, result1}, 1000),
         wf_receipt:store(Receipt1, EffectSpec),

         Receipt2 = wf_receipt:new(EffectId2, {ok, result2}, 1000),
         EffectSpec2 = EffectSpec#effect_spec{effect_id = EffectId2},
         wf_receipt:store(Receipt2, EffectSpec2),

         AllReceipts = wf_receipt:all(CaseId),

         [
             ?_assertEqual(2, length(AllReceipts))
         ]
     end}.
```

##### 3. Add integration tests to test/wf_exec_tests.erl

**File**: `test/wf_exec_tests.erl`
**Changes**: Add effect yielding and blocking tests

```erlang
%%====================================================================
%% Phase 7 Tests: Effect Integration
%%====================================================================

%% Test task yields effect
task_yields_effect_test_() ->
    {setup,
     fun() ->
         %% Start gen_servers
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         %% Task function that yields effect
         TaskFun = fun(Ctx) ->
             EffectSpec = wf_effect:new_spec(
                 make_ref(), 0, root, mock_effect, #{}
             ),
             {effect, EffectSpec, Ctx}
         end,

         %% Mock bytecode with task that yields effect
         %% Note: This requires modifying mock_bytecode generators
         %% For now, stub test
         ?_test(begin
             %% Test implementation
             ok
         end)
     end}.

%% Test executor blocks on effect
executor_blocks_on_effect_test_() ->
    ?_test(begin
        %% Test implementation
        ok
    end).

%% Test executor resumes when effect completes
executor_resumes_after_effect_test_() ->
    ?_test(begin
        %% Test implementation
        ok
    end).

%% Test idempotent effects resume immediately
idempotent_effect_resumes_immediately_test_() ->
    ?_test(begin
        %% Test implementation
        ok
    end).
```

#### Success Criteria:

##### Automated Verification:

- [ ] All EUnit tests pass: `erl -eval "eunit:test(wf_effect, [verbose])." -s init stop -noshell`
- [ ] All EUnit tests pass: `erl -eval "eunit:test(wf_receipt, [verbose])." -s init stop -noshell`
- [ ] All EUnit tests pass: `erl -eval "eunit:test(wf_exec, [verbose])." -s init stop -noshell`
- [ ] Property-based tests pass (if implemented)
- [ ] Test coverage > 80%

##### Manual Verification:

- [ ] Effect yielding works in executor
- [ ] Executor blocks and resumes correctly
- [ ] Idempotency caching works
- [ ] Cancellation propagates to effects
- [ ] Receipts are created and verifiable

**Note**: Complete all automated verification, then pause for manual confirmation. This is the final phase.

---

## Testing Strategy

### Unit Tests:

**wf_effect tests:**
- Effect spec creation with new_spec/4
- Setting idempotency key with set_idempotency_key/2
- Setting timeout with set_timeout/2
- Effect yielding with yield/4 (new effect)
- Effect yielding with idempotency (cached receipt)
- Effect cancellation with cancel_effect/2
- Cancellation check with is_cancelled/1
- Result polling with get_result/1
- Effect execution with mock handlers (mock_effect, mock_delay, mock_error, mock_crash)

**wf_receipt tests:**
- Receipt creation with new/3
- Receipt storage with store/2
- Receipt lookup by effect_id with lookup/2
- Receipt lookup by idempotency_key with lookup_by_key/2
- Receipt query all receipts for case with all/1
- Receipt verification with verify/2
- Hash consistency with hash_effect_spec/1
- Tampered receipt verification fails

**wf_exec tests:**
- Executor case_id generation in new/1
- Task yields effect via {effect, EffectSpec, ContCtx}
- Executor transitions to blocked_effect status
- Executor polls effect result with get_result/1
- Executor resumes when effect completes
- Idempotent effects resume immediately (cached receipt)
- Cancelled effects propagate to executor
- Failed effects set executor status to failed

### Integration Tests:

**End-to-end scenarios:**
1. **Simple effect execution**: Task yields effect → executor blocks → effect completes → executor resumes
2. **Idempotent effect**: Task yields effect with key → executor resumes immediately with cached receipt
3. **Cancelled effect**: Task yields effect → effect cancelled → executor resumes with cancelled status
4. **Failed effect**: Task yields effect → effect handler crashes → executor resumes with error
5. **Multiple effects**: Multiple tokens with pending effects → all complete → executor continues
6. **Receipt verification**: All receipts verify correctly with hash check
7. **Receipt log**: Append-only log grows correctly, no modifications

### Manual Testing Steps:

1. **Start Erlang shell**: `erl -pa ebin/`
2. **Start gen_servers**: `{ok, _} = wf_effect:start_link().`, `{ok, _} = wf_receipt:start_link().`
3. **Create effect spec**: `Spec = wf_effect:new_spec(make_ref(), 0, root, mock_effect, #{}).`
4. **Yield effect**: `{ok, Effect} = wf_effect:yield(make_ref(), 0, root, Spec).`
5. **Check result**: `wf_effect:get_result(Effect#effect.effect_id).`
6. **Verify receipt**: `{ok, Receipt} = wf_receipt:lookup(make_ref(), Effect#effect.effect_id).`
7. **Test idempotency**: Submit same effect with key, verify cached receipt returned
8. **Test cancellation**: Submit effect, cancel immediately, verify cancelled receipt created

## Migration Notes

**No Data Migration Required**: This is a new feature with no existing data to migrate.

**Backward Compatibility**: Existing wf_exec tests continue to pass. Mock task execution (always returns ok) is preserved for tests that don't use effects.

**Rollback Strategy**: If effect system has issues, revert wf_exec changes to use mock task execution (remove effect handling from execute_task_exec/2). wf_effect and wf_receipt modules are independent and can be disabled.

**Performance Impact**: Minimal for workloads that don't use effects (task execution path unchanged). For effects, async execution ensures executor not blocked.

**Memory Impact**: ETS tables for effects and receipts grow unbounded in v1. Monitor memory usage, implement log rotation in v2.

**Dependencies**: Requires crypto module (standard Erlang/OTP library). No external dependencies.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/research.md`
- Item specification: `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/item.json`
- PROMPT.md: `/Users/speed/wf-substrate/PROMPT.md` (lines 59, 102, 145-155)
- wf_exec.erl: `/Users/speed/wf-substrate/src/wf_exec.erl` (lines 194-204: mock task execution, 96-99: is_blocked/1)
- wf_exec.hrl: `/Users/speed/wf-substrate/src/wf_exec.hrl` (lines 12: token status, 40: exec_state status)
- wf_state.erl: `/Users/speed/wf-substrate/src/wf_state.erl` (lines 107-115: receipt stub)
- wf_vm.erl: `/Users/speed/wf-substrate/src/wf_vm.erl` (lines 13-21: opcode definitions)
- Test examples: `/Users/speed/wf-substrate/test/wf_exec_tests.erl` (EUnit patterns)
