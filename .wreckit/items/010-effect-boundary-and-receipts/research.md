# Research: Implement effect boundary and receipt system

**Date**: 2025-01-10
**Item**: 010-effect-boundary-and-receipts

## Research Question

Implement wf_effect.erl: the effect system boundary for external IO/tool calls. Tasks do not perform side effects directly. Instead, when a task needs external interaction, the executor yields {effect, EffectSpec, Continuation} where EffectSpec describes what to do and Continuation is the resumption point.

EffectSpec includes: effect_id (unique causal ID derived from case_id + step_seq + scope), effect_type (atom), payload (term), idempotency_key (optional, for at-most-once semantics), timeout (optional).

Effects are:
- Cancelable: if the cancel scope containing the effect is cancelled, the effect is marked cancelled and its result (if any) is discarded.
- Idempotent or at-most-once: if an idempotency_key is provided, duplicate effect submissions with the same key return the cached result from the receipt store.

Implement wf_receipt.erl: receipt records for completed effects. Each receipt contains: receipt_id, effect_id, effect_spec_hash (for verification), timestamp, result (ok/error tuple), duration_us. Receipts form an append-only log per case. Receipts can be queried by effect_id or idempotency_key.

Export from wf_effect: new_spec/4, yield/3, cancel_effect/2, is_cancelled/1.
Export from wf_receipt: new/3, store/2, lookup/2, lookup_by_key/2, all/1, verify/2 (check hash matches spec).

## Summary

This task involves implementing two critical modules for the workflow substrate: **wf_effect.erl** (effect system boundary for external IO/tool calls) and **wf_receipt.erl** (receipt records for completed effects with verification and caching). Together, these modules provide a clean separation between pure workflow execution and impure side effects, enabling deterministic replay, crash recovery, and at-most-once semantics.

The effect system implements a **yield/resume** pattern where tasks never perform IO directly. Instead, when a task needs external interaction, it yields an `{effect, EffectSpec, Continuation}` tuple. The executor suspends the task, submits the effect to wf_effect, and blocks until the effect completes. The effect is executed by external handlers (gen_servers, ports, or user-provided modules), and the result is resumed to the task via a receipt.

**Key Architectural Requirements**:
- **Separation of concerns**: Pure workflow execution vs impure effects
- **Causal tracking**: Each effect has unique ID derived from case_id + step_seq + scope
- **Cancelability**: Effects can be cancelled when their containing scope is cancelled
- **Idempotency**: Effects with idempotency_key return cached results from receipt store
- **Verification**: Receipts store hash of effect_spec for verification
- **Append-only log**: Receipts form immutable audit trail per case

**Integration Points**:
- **wf_exec** (item 005): Executor yields `{effect, EffectSpec, Continuation}` when task returns effect, blocks on effect result, resumes with receipt
- **wf_state** (item 006): State store persists receipts (via wf_receipt) for crash recovery
- **wf_cancel** (item 008): Cancellation propagates to running effects (cancel_effect/2)
- **wf_trace** (item 011): Tracing system logs effect submission/completion events

## Current State Analysis

### Existing Implementation

**Current State**: No `wf_effect.erl` or `wf_receipt.erl` modules exist yet. However, the executor has mock task execution that hints at effect handling:

**1. Executor Task Execution** (`/Users/speed/wf-substrate/src/wf_exec.erl:194-204`):

```erlang
%% @doc Execute TASK_EXEC: run task function (mock for now)
execute_task_exec({_TaskExec, _TaskName}, ExecState) ->
    %% Mock: task always succeeds, updates context
    NewCtx = maps:put(task_result, ok, ExecState#exec_state.ctx),
    ExecState#exec_state{
        ctx = NewCtx,
        ip = ExecState#exec_state.ip + 1,
        step_count = ExecState#exec_state.step_count + 1
    }.
```

**Analysis**:
- Current implementation mocks task execution (always returns ok)
- **Missing**: Effect yielding logic (task should return `{ok, NewCtx}` or `{effect, EffectSpec, Continuation}`)
- **Missing**: Effect blocking and resumption
- **Missing**: Receipt creation and storage

**2. Token Status** (`/Users/speed/wf-substrate/src/wf_exec.erl:28-34`):

```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled
}).
```

**Analysis**:
- Token status includes `active`, `complete`, `cancelled`
- **Missing**: `blocked_effect` status (when task is waiting for effect to complete)
- **Extension needed**: Add `current_effect_id` field to track pending effect

**3. State Store Receipt Stub** (`/Users/speed/wf-substrate/src/wf_state.erl:107-115`):

```erlang
%% Receipt record: Stub for item 010 integration
-record(receipt, {
    receipt_id :: term(),
    case_id :: case_id(),
    mutations :: [#mutation{}],
    timestamp :: erlang:timestamp(),
    state_before_hash :: binary(),
    state_after_hash :: binary()
}).
```

**Analysis**:
- wf_state has a stub receipt record (for state mutations, not effect receipts)
- **Different concern**: Effect receipts track effect completion, state receipts track state commits
- **No conflict**: Can coexist as separate receipt types

**4. No Effect Infrastructure**:
- No effect opcode in wf_vm.erl
- No effect yielding in wf_exec
- No receipt store
- No effect cancellation interface

### Specification Context

**From PROMPT.md:59, 102, 145-155**:

```
Module: wf_effect.erl
- effect system (tool calls / IO) boundary

Module: wf_receipt.erl
- receipts, hashing, causal ids

Types:
- effect_id() uniquely identifies effects (causal ID)
- receipt() for effects and commits

Pattern Semantics:
- task/2 can request external effects:
  {effect, Spec, ContCtx} means yield to wf_effect manager
  wf_effect executes Spec, returns Result
  reducer resumes continuation with Result, producing a receipt

All effects must:
- have unique causal ids
- be cancelable when supported
- be idempotent or have explicit "at most once" semantics via receipts
```

**From item.json:6** (item 010 specification):

```
EffectSpec includes: effect_id (unique causal ID derived from case_id + step_seq + scope),
effect_type (atom), payload (term), idempotency_key (optional, for at-most-once semantics),
timeout (optional).

Effects are:
- Cancelable: if the cancel scope containing the effect is cancelled, the effect is marked
  cancelled and its result (if any) is discarded.
- Idempotent or at-most-once: if an idempotency_key is provided, duplicate effect
  submissions with the same key return the cached result from the receipt store.

Receipt: receipt_id, effect_id, effect_spec_hash (for verification), timestamp, result
(ok/error tuple), duration_us. Receipts form an append-only log per case.
```

**Workflow Pattern Context**:

This implements the **Effect Pattern** from functional programming:
- **Pure core**: Workflow execution is deterministic and replayable
- **Impure boundary**: All side effects isolated at effect boundary
- **Causal tracking**: Each effect tagged with causal ID for traceability
- **Receipts**: Immutable audit trail of all effects executed

## Key Files

### Existing Files

- `/Users/speed/wf-substrate/src/wf_exec.erl:194-204` - **Task execution (mock)**
  - Lines 194-204: `execute_task_exec/2` mocks task execution
  - **Integration point**: Replace mock with effect yielding/resumption logic
  - **Extension needed**: Add `blocked_effect` status to token
  - **Extension needed**: Track pending effect_id in token or exec_state

- `/Users/speed/wf-substrate/src/wf_exec.erl:28-34` - **Token record**
  - Lines 28-34: `#token{}` record definition
  - **Extension needed**: Add `current_effect_id :: term() | undefined` field

- `/Users/speed/wf-substrate/src/wf_exec.erl:89-99` - **Executor status**
  - Lines 89-99: `is_blocked/1` checks if executor is blocked
  - **Extension needed**: Add `blocked_effect` to status enum
  - **Integration point**: Check effect completion on step if blocked

- `/Users/speed/wf-substrate/src/wf_state.erl:107-115` - **Receipt stub**
  - Lines 107-115: Receipt record for state mutations
  - **Note**: Effect receipts are separate concern
  - **Coexistence**: Can have both state receipts and effect receipts

- `/Users/speed/wf-substrate/src/wf_state.erl:117-127` - **State record**
  - Lines 117-127: `#state{}` record with ETS table reference
  - **Integration point**: Store receipt references in state (optional for v1)

- `/Users/speed/wf-substrate/src/wf_vm.erl:1-50` - **Opcode definitions**
  - Lines 13-21: `opcode()` type
  - **Missing**: No effect-specific opcode (effects are task-level, not bytecode-level)
  - **Note**: Effects are task-level yields, not opcodes

### Specification Files

- `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/item.json:1-14` - **Item specification**
  - Lines 6-7: EffectSpec structure (effect_id, effect_type, payload, idempotency_key, timeout)
  - Lines 6-7: Cancelable effects (cancelled when scope cancelled)
  - Lines 6-7: Idempotent effects (cached by idempotency_key)
  - Lines 6-7: Receipt structure (receipt_id, effect_id, effect_spec_hash, timestamp, result, duration_us)
  - Lines 6-7: wf_effect exports: new_spec/4, yield/3, cancel_effect/2, is_cancelled/1
  - Lines 6-7: wf_receipt exports: new/3, store/2, lookup/2, lookup_by_key/2, all/1, verify/2

- `/Users/speed/wf-substrate/PROMPT.md:59, 102, 145-155` - **Module specification**
  - Module: wf_effect.erl (effect boundary)
  - Module: wf_receipt.erl (receipts, hashing, causal IDs)
  - Pattern: {effect, Spec, ContCtx} yield from task
  - Requirements: unique causal IDs, cancelable, idempotent or at-most-once

### Files to Create

- `src/wf_effect.erl` - **Effect system boundary module** (primary deliverable)
  - `-record(effect_spec, {})` - Effect specification record
  - `-record(effect, {})` - Active effect record (running/completed/cancelled)
  - `-type effect_id()` - Causal ID type
  - `-type effect_type()` - Atom type for effect types
  - `new_spec/4` - Create new EffectSpec from components
  - `yield/3` - Yield effect from task (returns {effect, EffectSpec, Continuation})
  - `cancel_effect/2` - Cancel running effect
  - `is_cancelled/1` - Check if effect is cancelled
  - `execute/2` - Execute effect (internal, calls effect handler)

- `src/wf_receipt.erl` - **Receipt storage module** (primary deliverable)
  - `-record(receipt, {})` - Receipt record
  - `-type receipt_id()` - Receipt identifier type
  - `-type effect_id()` - Effect identifier (same as wf_effect)
  - `new/3` - Create new receipt from effect_id, result, duration
  - `store/2` - Store receipt in append-only log (ETS table)
  - `lookup/2` - Lookup receipt by effect_id
  - `lookup_by_key/2` - Lookup receipt by idempotency_key
  - `all/1` - Get all receipts for a case_id
  - `verify/2` - Verify receipt matches effect_spec (hash check)
  - `hash_effect_spec/1` - Hash effect_spec for verification (internal)

- `src/wf_effect_handler.erl` - **Effect handler behavior** (optional, for v1)
  - `-callback handle_effect/2` - Handle effect execution
  - Default handlers for common effects (http_call, file_write, etc.)

- `test/wf_effect_tests.erl` - **Effect system tests**
  - Test effect_spec creation
  - Test effect yielding
  - Test effect cancellation
  - Test idempotency (cached results)
  - Test effect execution (mock handler)
  - Test integration with wf_exec

- `test/wf_receipt_tests.erl` - **Receipt storage tests**
  - Test receipt creation
  - Test receipt storage and lookup
  - Test receipt lookup by idempotency_key
  - Test receipt verification (hash match)
  - Test append-only log (no modification)
  - Test receipt query (all receipts for case)

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Modules Needed**:
- `crypto` - For hashing effect_spec (crypto:hash/2)
- `ets` - For receipt storage (append-only log)
- `erlang:timestamp/0` - For receipt timestamps
- `erlang:unique_integer/0` - For effect_id generation (alternative to make_ref())

**Internal Module Dependencies**:
- **wf_exec (item 005)**: Executor yields effects from tasks
  - wf_exec calls `wf_effect:yield/3` when task returns effect
  - wf_exec blocks on effect result (`status = blocked_effect`)
  - wf_exec resumes task when receipt available
  - wf_exec provides case_id, step_seq, scope_id for effect_id generation

- **wf_state (item 006)**: State store for crash recovery
  - wf_state stores receipt references (optional for v1)
  - wf_state persists state including effect_id tracking
  - Receipts stored in separate ETS table (not in wf_state)

- **wf_cancel (item 008)**: Cancellation propagates to effects
  - wf_cancel calls `wf_effect:cancel_effect/2` when scope cancelled
  - wf_effect marks effect as cancelled
  - wf_exec discards cancelled effect results

- **wf_trace (item 011)**: Tracing logs effect events
  - wf_effect logs effect submission/completion
  - Receipt timestamps provide observability

**No Circular Dependencies**: wf_effect depends on wf_exec (for context), wf_cancel (for cancellation), but these are foundational modules. wf_receipt is independent. Safe to implement.

### Effect ID Generation

**From item.json:6**:
> "effect_id (unique causal ID derived from case_id + step_seq + scope)"

**Causal ID Design**:

```erlang
%% Option 1: Composite key
-effect_id() :: {case_id(), step_seq(), scope_id()}.

%% Option 2: Hash-based
-effect_id() :: binary().  %% Hash of {case_id, step_seq, scope_id}

%% Option 3: Sequential with prefix
-effect_id() :: {case_id(), pos_integer()}.
```

**Recommendation**: Option 1 (composite key). Provides traceability (can see case, step, scope from ID). Unique if components unique.

**Implementation**:
```erlang
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
```

### Effect Yield Pattern

**From PROMPT.md:145-150**:

```
task/2 can request external effects:
{effect, Spec, ContCtx} means yield to wf_effect manager
wf_effect executes Spec, returns Result
reducer resumes continuation with Result, producing a receipt
```

**Yield/Resume Protocol**:

1. **Task returns effect**:
   ```erlang
   %% Task function
   task(Ctx) ->
       case maps:get(user_id, Ctx, undefined) of
           undefined ->
               %% Need to fetch user, yield effect
               UserId = 123,
               {effect, wf_effect:new_spec(CaseId, StepSeq, ScopeId, http_get, #{url => "/users/123"}), Ctx};
           UserId ->
               %% User already fetched, continue
               {ok, Ctx#{user_id => UserId}}
       end.
   ```

2. **Executor detects effect**:
   ```erlang
   execute_task_exec({_TaskExec, TaskFun}, ExecState) ->
       Ctx = ExecState#exec_state.ctx,
       CaseId = ExecState#exec_state.case_id,
       StepSeq = ExecState#exec_state.step_count,
       ScopeId = get_current_scope(ExecState),

       case TaskFun(Ctx) of
           {ok, NewCtx} ->
               %% Pure computation, continue
               ExecState#exec_state{ctx = NewCtx, ip = IP + 1};
           {effect, EffectSpec, ContCtx} ->
               %% Yield effect, block executor
               {ok, Effect} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpec),
               ExecState#exec_state{
                   ctx = ContCtx,
                   status = blocked_effect,
                   current_effect = Effect#effect.effect_id,
                   ip = IP + 1
               }
       end.
   ```

3. **Executor resumes when effect completes**:
   ```erlang
   step(ExecState, _SchedDecision) when ExecState#exec_state.status =:= blocked_effect ->
       EffectId = ExecState#exec_state.current_effect,
       case wf_effect:get_result(EffectId) of
           {ok, Result} ->
               %% Effect completed, resume task
               %% Task logic resumes with result in context
               ExecState#exec_state{
                   status = running,
                   current_effect = undefined,
                   ctx = maps:put(effect_result, Result, ExecState#exec_state.ctx)
               };
           pending ->
               %% Effect still running, yield
               {ExecState, #{status => blocked_effect}};
           {cancelled, _Reason} ->
               %% Effect was cancelled, handle gracefully
               ExecState#exec_state{
                   status = cancelled,
                   current_effect = undefined
               }
       end.
   ```

### Idempotency and Caching

**From item.json:6**:
> "Idempotent or at-most-once: if an idempotency_key is provided, duplicate effect submissions with the same key return the cached result from the receipt store."

**Implementation**:

```erlang
%% Submit effect (with idempotency check)
-spec yield(case_id(), step_seq(), scope_id(), effect_spec()) -> {ok, effect()} | {ok, receipt()}.
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

execute_new_effect(EffectId, EffectSpec) ->
    %% Create active effect record
    Effect = #effect{
        effect_id = EffectId,
        spec = EffectSpec,
        status = pending,
        submitted_at = erlang:timestamp(),
        result = undefined
    },

    %% Store effect in ETS table
    ets:insert(wf_effects, Effect),

    %% Submit to effect handler (async)
    spawn_link(fun() ->
        Result = execute_effect_handler(EffectSpec),
        complete_effect(EffectId, Result)
    end),

    {ok, Effect}.
```

**Receipt Lookup by Key**:

```erlang
%% In wf_receipt.erl
-spec lookup_by_key(case_id(), idempotency_key()) -> {ok, receipt()} | not_found.
lookup_by_key(CaseId, IdempotencyKey) ->
    %% Scan receipts for matching key
    Pattern = #receipt{
        case_id = CaseId,
        idempotency_key = IdempotencyKey,
        _ = '_'
    },
    case ets:match_object(wf_receipts, Pattern) of
        [Receipt] -> {ok, Receipt};
        [] -> not_found
    end.
```

### Effect Cancellation

**From item.json:6**:
> "Cancelable: if the cancel scope containing the effect is cancelled, the effect is marked cancelled and its result (if any) is discarded."

**Implementation**:

```erlang
-spec cancel_effect(effect_id(), reason()) -> ok | {error, term()}.
cancel_effect(EffectId, Reason) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = pending} = Effect] ->
            %% Mark effect as cancelled
            CancelledEffect = Effect#effect{status = cancelled, reason = Reason},
            ets:insert(wf_effects, CancelledEffect),

            %% Create receipt (cancelled)
            Receipt = wf_receipt:new(EffectId, {cancelled, Reason}, 0),
            wf_receipt:store(Receipt),
            ok;
        [#effect{status = complete}] ->
            %% Effect already completed, cannot cancel
            {error, {effect_already_complete, EffectId}};
        [] ->
            {error, {effect_not_found, EffectId}}
    end.

-spec is_cancelled(effect_id()) -> boolean().
is_cancelled(EffectId) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = cancelled}] -> true;
        _ -> false
    end.
```

**Integration with wf_cancel**:

```erlang
%% In wf_cancel:cancel_activity/3
cancel_activity(CaseId, TaskId, Options) ->
    Token = find_token_for_task(CaseId, TaskId),

    %% Check if token has pending effect
    case Token#token.current_effect of
        undefined ->
            %% No effect, just cancel token
            mark_token_cancelled(Token);
        EffectId ->
            %% Cancel pending effect
            wf_effect:cancel_effect(EffectId, {activity_cancelled, TaskId}),
            mark_token_cancelled(Token)
    end.
```

### Receipt Storage and Verification

**Receipt Record**:

```erlang
-record(receipt, {
    receipt_id :: receipt_id(),
    case_id :: case_id(),
    effect_id :: effect_id(),
    effect_spec_hash :: binary(),           %% Hash of effect_spec for verification
    idempotency_key :: term() | undefined, %% For idempotency lookups
    timestamp :: erlang:timestamp(),
    result :: {ok, term()} | {error, term()} | {cancelled, term()},
    duration_us :: non_neg_integer()
}).
```

**Receipt Creation**:

```erlang
%% In wf_receipt.erl
-spec new(effect_id(), result(), duration_us()) -> receipt().
new(EffectId, Result, DurationUs) ->
    #receipt{
        receipt_id = make_ref(),
        case_id = element(1, EffectId),  %% Extract case_id from {CaseId, StepSeq, ScopeId}
        effect_id = EffectId,
        effect_spec_hash = undefined,  %% Filled by store/2
        idempotency_key = undefined,
        timestamp = erlang:timestamp(),
        result = Result,
        duration_us = DurationUs
    }.
```

**Receipt Storage with Hash**:

```erlang
-spec store(receipt(), effect_spec()) -> ok.
store(Receipt, EffectSpec) ->
    %% Hash effect_spec for verification
    EffectSpecHash = hash_effect_spec(EffectSpec),

    %% Attach hash to receipt
    ReceiptWithHash = Receipt#receipt{effect_spec_hash = EffectSpecHash},

    %% Store in append-only log
    ets:insert(wf_receipts, ReceiptWithHash),
    ok.
```

**Receipt Verification**:

```erlang
-spec verify(receipt(), effect_spec()) -> boolean().
verify(Receipt, EffectSpec) ->
    ExpectedHash = hash_effect_spec(EffectSpec),
    Receipt#receipt.effect_spec_hash =:= ExpectedHash.
```

**Hash Function**:

```erlang
-spec hash_effect_spec(effect_spec()) -> binary().
hash_effect_spec(EffectSpec) ->
    %% Serialize effect_spec to binary and hash
    Data = {
        EffectSpec#effect_spec.effect_id,
        EffectSpec#effect_spec.effect_type,
        EffectSpec#effect_spec.payload
    },
    crypto:hash(sha256, term_to_binary(Data)).
```

### Effect Handler Interface

**Effect Handler Behavior** (for v1, use simple function):

```erlang
%% Simple callback (v1)
-type effect_handler() :: fun((effect_spec()) -> {ok, term()} | {error, term()}).

%% Execute effect via handler
execute_effect_handler(EffectSpec) ->
    Handler = get_handler_for_type(EffectSpec#effect_spec.effect_type),
    Handler(EffectSpec).

%% Get handler for effect type
get_handler_for_type(http_get) ->
    fun(Spec) ->
        Url = maps:get(url, Spec#effect_spec.payload),
        %% Mock HTTP GET
        {ok, #{status => 200, body => <<"response">>}}
    end;
get_handler_for_type(file_write) ->
    fun(Spec) ->
        Path = maps:get(path, Spec#effect_spec.payload),
        Content = maps:get(content, Spec#effect_spec.payload),
        %% Mock file write
        {ok, bytes_written}
    end;
get_handler_for_type(EffectType) ->
    fun(_Spec) -> {error, {unknown_effect_type, EffectType}} end.
```

**Future Enhancement**: Gen_server behavior for effect handlers
```erlang
%% v2: Use gen_server behavior for effect handlers
-callback init(term()) -> {ok, State}.
-callback handle_effect(effect_spec(), State) -> {reply, Result, State}.
```

### ETS Tables for Effect Tracking

**Effect Table** (running/completed/cancelled effects):

```erlang
%% Table: wf_effects
%% Type: set
%% Key: effect_id
init() ->
    ets:new(wf_effects, [
        named_table,
        set,
        {keypos, #effect.effect_id},
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).
```

**Receipt Table** (append-only log):

```erlang
%% Table: wf_receipts
%% Type: ordered_set (ordered by timestamp for querying)
init() ->
    ets:new(wf_receipts, [
        named_table,
        ordered_set,
        {keypos, #receipt.receipt_id},
        public
        %% Note: ordered_set for append-only semantics
        %% Secondary index on case_id for fast queries
    ]),
    %% Create secondary index on case_id (bag allows multiple receipts per case)
    ets:add_compound_index(wf_receipts, [#receipt.case_id]).
```

**Table Ownership**: Own in gen_server (wf_effect_store, wf_receipt_store) for crash resilience.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Effect handler crash** | High | Effect handler may raise exception or crash. Mitigation: Wrap in try-catch, return {error, HandlerCrash}. Supervisor restart handler if gen_server. |
| **Receipt hash collision** | Low | SHA-256 collision is astronomically unlikely. Mitigation: Use crypto:hash/2 (SHA-256). Document hash algorithm. |
| **Idempotency key collision** | High | Different effects with same key return wrong cached result. Mitigation: Document key must be globally unique per effect type. Use composite keys {effect_type, unique_id}. |
| **Effect timeout deadlock** | High | Effect never completes, executor blocked forever. Mitigation: Support timeout field in effect_spec. Spawn monitor process, cancel effect on timeout. |
| **Cancellation race condition** | Medium | Effect completes after cancellation signal. Mitigation: Check effect status on completion. If cancelled, discard result. Atomic status update. |
| **Receipt log growth** | Medium | Append-only receipt log grows unbounded. Mitigation: Implement log rotation (future enhancement). For v1, accept growth. Document cleanup strategy. |
| **ETS table ownership** | Medium | If owner process crashes, tables deleted. Mitigation: Own tables in separate gen_server (wf_effect_store, wf_receipt_store). Restart on crash. |
| **Concurrent effect submission** | Low | Two effects submitted with same effect_id. Mitigation: effect_id derived from case_id + step_seq + scope (unique). Use sequential step_count. |
| **Effect handler blocking** | Medium | Handler may block indefinitely (e.g., network call). Mitigation: Use timeout in handler. Run in separate process, monitor for timeout. |
| **Receipt verification failure** | Medium | Receipt hash doesn't match effect_spec. Mitigation: Log verification failures. Return error to caller. Investigate tampering or corruption. |
| **Memory exhaustion (many effects)** | High | Thousands of pending effects consume memory. Mitigation: Limit concurrent effects. Queue when limit reached. |
| **Idempotency lookup performance** | Medium | Scanning all receipts for key is O(N). Mitigation: Secondary ETS index on idempotency_key. Use ets:match/2 with pattern. |
| **Effect result serialization** | Medium | Effect result may be non-serializable (pid, port). Mitigation: Require serializable results. Return error if non-serializable. Document constraints. |
| **Executor integration complexity** | High | Modifying executor to handle effects is large change. Mitigation: Incremental integration. First, stub effect handling. Then, add full yield/resume. |
| **Test coverage gaps** | Medium | Effect system has many edge cases (cancellation, timeout, idempotency). Mitigation: Comprehensive unit tests. Property-based tests for invariants. |
| **Receipt loss on crash** | High | Receipts not persisted if process crashes before ETS write. Mitigation: Write receipts atomically. Use gen_server for serialization. |
| **Effect handler deadlock** | Medium | Handler waits for executor, executor waits for handler. Mitigation: Async effect execution (spawn_link). Timeout on result. |
| **Scope tracking missing** | High | effect_id requires scope_id, but executor scope stack may be inconsistent. Mitigation: Pass current scope from executor. Validate scope_id exists. |

## Recommended Approach

**High-Level Strategy**: Implement wf_effect module with effect yielding, cancellation, and idempotency support. Implement wf_receipt module with receipt storage, lookup, and verification. Integrate with wf_exec to handle effect yielding/blocking/resumption. Use ETS tables for effect and receipt storage. Support async effect execution with timeout and cancellation.

### Phase 1: Types and Records (wf_effect)

**1. Define types and records** (`src/wf_effect.erl`):

```erlang
%% Effect specification (submitted by task)
-record(effect_spec, {
    effect_id :: effect_id(),
    effect_type :: effect_type(),
    payload :: term(),
    idempotency_key :: term() | undefined,
    timeout :: pos_integer() | undefined  %% milliseconds
}).

%% Active effect (running/completed/cancelled)
-record(effect, {
    effect_id :: effect_id(),
    spec :: effect_spec(),
    status :: pending | complete | cancelled,
    submitted_at :: erlang:timestamp(),
    completed_at :: erlang:timestamp() | undefined,
    result :: term() | undefined,
    reason :: term() | undefined  %% If cancelled
}).

%% Types
-type effect_id() :: {case_id(), step_seq(), scope_id()}.
-type effect_type() :: atom().
-type case_id() :: term().
-type step_seq() :: non_neg_integer().
-type scope_id() :: term().
-type idempotency_key() :: term().

%% Type exports
-export_type([
    effect_id/0,
    effect_type/0,
    effect_spec/0,
    effect/0
]).
```

**2. Test type compilation**

### Phase 2: Receipt Module (wf_receipt)

**1. Define receipt record** (`src/wf_receipt.erl`):

```erlang
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

-type receipt_id() :: term().
-type effect_id() :: wf_effect:effect_id().
-type case_id() :: term().
-type idempotency_key() :: term().
```

**2. Implement receipt creation**:

```erlang
-spec new(effect_id(), result(), duration_us()) -> receipt().
new(EffectId, Result, DurationUs) ->
    #receipt{
        receipt_id = make_ref(),
        case_id = element(1, EffectId),
        effect_id = EffectId,
        effect_spec_hash = <<>>,  %% Placeholder
        idempotency_key = undefined,
        timestamp = erlang:timestamp(),
        result = Result,
        duration_us = DurationUs
    }.
```

**3. Implement receipt storage**:

```erlang
-spec store(receipt(), effect_spec()) -> ok.
store(Receipt, EffectSpec) ->
    %% Hash effect_spec
    EffectSpecHash = hash_effect_spec(EffectSpec),

    %% Update receipt with hash and idempotency_key
    UpdatedReceipt = Receipt#receipt{
        effect_spec_hash = EffectSpecHash,
        idempotency_key = EffectSpec#effect_spec.idempotency_key
    },

    %% Store in ETS
    ets:insert(wf_receipts, UpdatedReceipt),
    ok.
```

**4. Implement receipt lookup**:

```erlang
-spec lookup(case_id(), effect_id()) -> {ok, receipt()} | not_found.
lookup(_CaseId, EffectId) ->
    Pattern = #receipt{effect_id = EffectId, _ = '_'},
    case ets:match_object(wf_receipts, Pattern) of
        [Receipt] -> {ok, Receipt};
        [] -> not_found
    end.

-spec lookup_by_key(case_id(), idempotency_key()) -> {ok, receipt()} | not_found.
lookup_by_key(CaseId, IdempotencyKey) ->
    Pattern = #receipt{case_id = CaseId, idempotency_key = IdempotencyKey, _ = '_'},
    case ets:match_object(wf_receipts, Pattern) of
        [Receipt] -> {ok, Receipt};
        [] -> not_found
    end.
```

**5. Implement receipt verification**:

```erlang
-spec verify(receipt(), effect_spec()) -> boolean().
verify(Receipt, EffectSpec) ->
    ExpectedHash = hash_effect_spec(EffectSpec),
    Receipt#receipt.effect_spec_hash =:= ExpectedHash.

-spec hash_effect_spec(effect_spec()) -> binary().
hash_effect_spec(EffectSpec) ->
    Data = {
        EffectSpec#effect_spec.effect_id,
        EffectSpec#effect_spec.effect_type,
        EffectSpec#effect_spec.payload
    },
    crypto:hash(sha256, term_to_binary(Data)).
```

**6. Test receipt module**

### Phase 3: Effect Yielding (wf_effect)

**1. Implement `new_spec/4`**:

```erlang
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
```

**2. Implement `yield/3`**:

```erlang
-spec yield(case_id(), step_seq(), scope_id(), effect_spec()) -> {ok, effect()} | {ok, receipt()}.
yield(CaseId, StepSeq, ScopeId, EffectSpec) ->
    EffectId = {CaseId, StepSeq, ScopeId},

    %% Check idempotency
    case EffectSpec#effect_spec.idempotency_key of
        undefined ->
            execute_new_effect(EffectId, EffectSpec);
        IdempotencyKey ->
            case wf_receipt:lookup_by_key(CaseId, IdempotencyKey) of
                {ok, Receipt} ->
                    {ok, Receipt};
                not_found ->
                    execute_new_effect(EffectId, EffectSpec)
            end
    end.

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

    ets:insert(wf_effects, Effect),

    %% Execute async
    spawn_link(fun() ->
        StartTime = erlang:monotonic_time(microsecond),
        Result = execute_effect_handler(EffectSpec),
        EndTime = erlang:monotonic_time(microsecond),
        DurationUs = EndTime - StartTime,
        complete_effect(EffectId, Result, DurationUs)
    end),

    {ok, Effect}.
```

**3. Implement `cancel_effect/2`**:

```erlang
-spec cancel_effect(effect_id(), term()) -> ok | {error, term()}.
cancel_effect(EffectId, Reason) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = pending} = Effect] ->
            CancelledEffect = Effect#effect{
                status = cancelled,
                reason = Reason,
                completed_at = erlang:timestamp()
            },
            ets:insert(wf_effects, CancelledEffect),

            %% Create cancelled receipt
            Receipt = wf_receipt:new(EffectId, {cancelled, Reason}, 0),
            wf_receipt:store(Receipt, CancelledEffect#effect.spec),
            ok;
        [#effect{status = complete}] ->
            {error, {effect_already_complete, EffectId}};
        [] ->
            {error, {effect_not_found, EffectId}}
    end.
```

**4. Implement `is_cancelled/1`**:

```erlang
-spec is_cancelled(effect_id()) -> boolean().
is_cancelled(EffectId) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = cancelled}] -> true;
        _ -> false
    end.
```

**5. Test effect yielding**

### Phase 4: Effect Execution

**1. Implement mock effect handler** (for v1):

```erlang
%% Mock handler for testing
execute_effect_handler(EffectSpec) ->
    Handler = get_handler_for_type(EffectSpec#effect_spec.effect_type),
    Handler(EffectSpec).

get_handler_for_type(mock_effect) ->
    fun(_Spec) -> {ok, mock_result} end;
get_handler_for_type(EffectType) ->
    fun(_Spec) -> {error, {unknown_effect_type, EffectType}} end.
```

**2. Implement `complete_effect/3`**:

```erlang
complete_effect(EffectId, Result, DurationUs) ->
    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = pending} = Effect] ->
            %% Mark effect complete
            CompletedEffect = Effect#effect{
                status = complete,
                result = Result,
                completed_at = erlang:timestamp()
            },
            ets:insert(wf_effects, CompletedEffect),

            %% Create receipt
            Receipt = wf_receipt:new(EffectId, Result, DurationUs),
            wf_receipt:store(Receipt, CompletedEffect#effect.spec),
            ok;
        [#effect{status = cancelled}] ->
            %% Effect was cancelled, discard result
            {cancelled, effect_cancelled};
        [] ->
            {error, effect_not_found}
    end.
```

**3. Test effect execution**

### Phase 5: Executor Integration (wf_exec)

**1. Extend token record** (`src/wf_exec.erl`):

```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled | blocked_effect,
    current_effect :: effect_id() | undefined  %% NEW FIELD
}).
```

**2. Add `blocked_effect` to exec_state status**:

```erlang
is_blocked(#exec_state{status = Status}) ->
    Status =:= blocked_effect orelse
    Status =:= blocked_join orelse
    Status =:= blocked_signal.
```

**3. Update `execute_task_exec/2` to handle effects**:

```erlang
execute_task_exec({_TaskExec, TaskFun}, ExecState) ->
    Ctx = ExecState#exec_state.ctx,
    CaseId = ExecState#exec_state.case_id,  %% Add case_id to exec_state
    StepSeq = ExecState#exec_state.step_count,
    ScopeId = get_current_scope(ExecState),

    case TaskFun(Ctx) of
        {ok, NewCtx} ->
            %% Pure computation
            ExecState#exec_state{
                ctx = NewCtx,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        {effect, EffectSpec, ContCtx} ->
            %% Yield effect
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
                    Token = maps:get(ExecState#exec_state.current_token, ExecState#exec_state.tokens),
                    UpdatedToken = Token#token{
                        status = blocked_effect,
                        current_effect = EffectId
                    },
                    Tokens = maps:put(ExecState#exec_state.current_token, UpdatedToken, ExecState#exec_state.tokens),

                    ExecState#exec_state{
                        ctx = ContCtx,
                        tokens = Tokens,
                        status = blocked_effect,
                        ip = ExecState#exec_state.ip + 1,
                        step_count = ExecState#exec_state.step_count + 1
                    }
            end
    end.
```

**4. Add effect completion check to `step/2`**:

```erlang
step(ExecState, _SchedDecision) when ExecState#exec_state.status =:= blocked_effect ->
    %% Check if effect completed
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    EffectId = Token#token.current_effect,

    case ets:lookup(wf_effects, EffectId) of
        [#effect{status = complete, result = Result}] ->
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
        [#effect{status = cancelled}] ->
            %% Effect cancelled, propagate cancellation
            UpdatedToken = Token#token{
                status = cancelled,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_cancelled,
                effect_id => EffectId
            },
            {ExecState#exec_state{tokens = Tokens, status = cancelled}, TraceEvent};
        [] ->
            %% Effect still pending, yield
            TraceEvent = #{type => blocked_effect, effect_id => EffectId},
            {ExecState, TraceEvent}
    end;

%% Normal step (not blocked)
step(ExecState, SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),
    TraceEvent = #{opcode => Opcode},
    {NewExecState, TraceEvent}.
```

**5. Add `case_id` to exec_state**:

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: ctx(),
    case_id :: case_id(),  %% NEW FIELD
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked_effect | blocked_join | blocked_signal | cancelled | failed,
    current_token :: term()
}).
```

**6. Test executor integration**

### Phase 6: Cancellation Integration

**1. Update wf_cancel to cancel effects** (item 008 integration):

```erlang
%% In wf_cancel:cancel_activity/3
cancel_activity(CaseId, TaskId, Options) ->
    Token = find_token_for_task(CaseId, TaskId),

    %% Cancel effect if pending
    case Token#token.current_effect of
        undefined ->
            mark_token_cancelled(Token);
        EffectId ->
            wf_effect:cancel_effect(EffectId, {activity_cancelled, TaskId}),
            mark_token_cancelled(Token)
    end.
```

**2. Test cancellation propagation**

### Phase 7: Comprehensive Testing

**1. Unit tests for wf_effect**:
- Test effect_spec creation
- Test effect yielding
- Test effect execution
- Test effect cancellation
- Test idempotency (cached results)
- Test effect timeout (if implemented)

**2. Unit tests for wf_receipt**:
- Test receipt creation
- Test receipt storage and lookup
- Test receipt lookup by idempotency_key
- Test receipt verification (hash match)
- Test receipt query (all receipts for case)
- Test hash collisions (unlikely, but test logic)

**3. Integration tests**:
- Test executor yields effect from task
- Test executor blocks on effect
- Test executor resumes when effect completes
- Test executor handles cached receipt (idempotency)
- Test cancellation propagates to effect
- Test cancelled effect result discarded

**4. Property-based tests**:
```erlang
%% Property: Receipts are append-only (no modification)
prop_receipts_append_only() ->
    ?FORALL({Receipt1, Receipt2}, {receipt_gen(), receipt_gen()},
        begin
            wf_receipt:store(Receipt1),
            wf_receipt:store(Receipt2),
            {ok, RetrievedReceipt} = wf_receipt:lookup(element(1, Receipt1)),
            Receipt1 =:= RetrievedReceipt
        end).

%% Property: Idempotency returns cached result
prop_idempotency_caching() ->
    ?FORALL(EffectSpec, effect_spec_gen(),
        begin
            Key = make_ref(),
            EffectSpec1 = EffectSpec#effect_spec{idempotency_key = Key},
            EffectSpec2 = EffectSpec#effect_spec{idempotency_key = Key},

            %% Submit effect twice
            {ok, Effect1} = wf_effect:yield(CaseId, 0, root, EffectSpec1),
            complete_effect(Effect1#effect.effect_id, {ok, result1}, 100),

            {ok, ReceiptOrEffect} = wf_effect:yield(CaseId, 1, root, EffectSpec2),

            %% Should return cached receipt
            is_record(ReceiptOrEffect, receipt) andalso
            ReceiptOrEffect#receipt.result =:= {ok, result1}
        end).
```

## Open Questions

1. **Effect ID Format**: Should effect_id be composite `{CaseId, StepSeq, ScopeId}` or hash-based?
   - **Recommendation**: Composite for v1 (traceable). Consider hash for v2 if size is concern.

2. **Effect Handler Registration**: How to register handlers for effect types?
   - **Option 1**: Module attribute (compile-time)
   - **Option 2**: Registration function (runtime)
   - **Recommendation**: Option 2 for flexibility. `wf_effect:register_handler(EffectType, HandlerFun)`.

3. **Effect Timeout Implementation**: Should timeout be enforced by effect system or handler?
   - **Recommendation**: Effect system enforces timeout. Spawn monitor process, cancel effect after timeout. Handler can check timeout internally, but system guarantees it.

4. **Receipt Persistence**: Should receipts be persisted to disk or kept in memory?
   - **Recommendation**: ETS in-memory for v1. Disk persistence for v2 (dets or custom). ETS survives process crashes if owned by separate gen_server.

5. **Concurrent Effect Submission**: Can multiple effects be submitted concurrently?
   - **Recommendation**: Yes, but executor is single-threaded. Only one effect per token. Multiple tokens can have pending effects. Track per-token effects.

6. **Effect Result Size Limits**: Should there be limits on effect result size?
   - **Recommendation**: Yes, enforce maximum size (e.g., 1MB). Large results should use references (e.g., file paths).

7. **Receipt Log Rotation**: How to handle append-only receipt log growth?
   - **Recommendation**: For v1, accept growth. For v2, implement log rotation (archive old receipts, delete after retention period).

8. **Effect Spec Payload Validation**: Should effect_spec payload be validated?
   - **Recommendation**: No validation for v1 (trust task). For v2, add per-effect-type validators (schema validation).

9. **Error Reporting**: How to report effect execution errors to task?
   - **Recommendation**: Include error in context (e.g., `#{effect_error => Error}`). Task checks for error and handles gracefully.

10. **Effect Retry Policy**: Should failed effects be retried?
    - **Recommendation**: No, task decides retry semantics. Task can resubmit effect with same idempotency_key (if idempotent).

11. **Effect Tracing**: Should effect submission/completion be traced?
    - **Recommendation**: Yes, produce trace events for observability. Use wf_trace (item 011) when available.

12. **Idempotency Key Format**: What format should idempotency_key use?
    - **Recommendation**: User-defined term. Document should be globally unique per effect type. Use composite keys {effect_type, unique_id}.

13. **Effect Handler Isolation**: Should effect handlers run in separate processes?
    - **Recommendation**: Yes, spawn_link for isolation. Handler crash doesn't crash executor. Supervisor restarts handler if gen_server.

14. **Receipt Verification Failures**: What to do if receipt verification fails?
    - **Recommendation**: Log error, return error to caller. Indicates tampering or corruption. Investigate immediately.

15. **Effect Spec Serialization**: Should effect_spec be serializable for persistence?
    - **Recommendation**: Yes, require serializable terms. Check with `erlang:is_term/1` or try `term_to_binary/1`.

16. **Case ID Generation**: Who generates case_id?
    - **Current**: Not defined in wf_exec
    - **Recommendation**: Add case_id field to exec_state, generated in `wf_exec:new/1` using `make_ref()`.

17. **Effect Ordering**: Should effects be ordered in receipt log?
    - **Recommendation**: Yes, use `ordered_set` ETS table. Ordered by receipt_id (timestamp). Enables chronological querying.

18. **Effect Handler Timeout**: How to enforce handler timeout?
    - **Recommendation**: Use `erlang:spawn_monitor/2` and `erlang:send_after/3`. Kill handler process on timeout, cancel effect.

19. **Memory Management**: How to limit memory usage for effects/receipts?
    - **Recommendation**: Set ETS table size limits. Use `ets:new/2` with `{max_entries, N}`. Reject new effects when limit reached.

20. **Effect Completion Notification**: How to notify executor when effect completes?
    - **Recommendation**: Executor polls effect status in `step/2` (simple). For v2, use gen_server call (async notification).
