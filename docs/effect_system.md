# Effect System Architecture and Usage

## Overview

The effect system provides a clean separation between pure workflow execution and impure side effects (IO/tool calls). It implements a yield/resume pattern where tasks never perform IO directly, ensuring deterministic replay and crash recovery.

## Architecture

### Components

1. **wf_effect** - Effect system boundary that manages effect lifecycle
2. **wf_receipt** - Receipt storage for completed effects (append-only audit trail)
3. **wf_exec** - Bytecode executor integrated with effect yielding/resuming

### Effect Lifecycle

```
Task → yield({effect, EffectSpec, ContCtx})
     → Executor submits effect to wf_effect
     → wf_effect executes effect asynchronously
     → Effect completes → wf_receipt stores receipt
     → Executor polls for result
     → Executor resumes task with result in context
```

## Effect Specification

### Creating an Effect Spec

```erlang
%% Create basic effect spec
EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, EffectType, Payload),

%% Add idempotency key (for at-most-once semantics)
EffectSpecWithKey = wf_effect:set_idempotency_key(EffectSpec, IdempotencyKey),

%% Add timeout (milliseconds)
EffectSpecWithTimeout = wf_effect:set_timeout(EffectSpec, Timeout),
```

### Effect Spec Structure

```erlang
-record(effect_spec, {
    effect_id,         %% {CaseId, StepSeq, ScopeId} - unique causal ID
    effect_type,       %% Atom identifying effect kind (e.g., http_get)
    payload,           %% Effect-specific data (map, record, etc.)
    idempotency_key,   %% Optional: User-provided key for caching
    timeout            %% Optional: Timeout in milliseconds
}).
```

## Task Functions

### Yielding Effects from Tasks

```erlang
%% Task function signature
fun(Ctx) -> {ok, NewCtx} | {effect, EffectSpec, ContCtx} | {error, Reason}

%% Example: Task that fetches user data
task_fetch_user(Ctx) ->
    case maps:get(user_id, Ctx, undefined) of
        undefined ->
            %% Need to fetch user, yield effect
            UserId = maps:get(requested_user_id, Ctx),
            Payload = #{url => "/users/" ++ integer_to_list(UserId)},
            EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, http_get, Payload),

            %% Yield effect, continuation context is Ctx
            {effect, EffectSpec, Ctx};
        UserId ->
            %% User already fetched, continue
            {ok, Ctx#{user_fetched => true}}
    end.
```

### Using Effect Results

After an effect completes, the result is available in the context under the key `effect_result`:

```erlang
%% After effect completes, check result
case maps:get(effect_result, Ctx) of
    {ok, UserData} ->
        %% Process user data
        {ok, Ctx#{user_data => UserData}};
    {error, Reason} ->
        %% Handle error
        {error, {user_fetch_failed, Reason}}
end.
```

## Idempotency

### Using Idempotency Keys

Idempotency keys ensure that duplicate effect submissions return cached results:

```erlang
%% Create effect with idempotency key
Key = {user_fetch, UserId},
EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, http_get, Payload),
EffectSpecWithKey = wf_effect:set_idempotency_key(EffectSpec, Key),

%% First submission: executes effect
{ok, Effect} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpecWithKey),

%% Wait for completion
timer:sleep(100),

%% Second submission: returns cached receipt immediately
{ok, Receipt} = wf_effect:yield(CaseId, StepSeq + 1, ScopeId, EffectSpecWithKey),
```

### Idempotency Key Format

Idempotency keys should be globally unique per effect type:

```erlang
%% Good: Composite key with effect type
Key = {http_get, "https://api.example.com/users/123"},

%% Good: UUID
Key = "550e8400-e29b-41d4-a716-446655440000",

%% Bad: Non-unique (will cause collisions)
Key = user_fetch,
```

## Cancellation

### Cancelling Effects

Effects can be cancelled when their containing scope is cancelled:

```erlang
%% Cancel pending effect
EffectId = {CaseId, StepSeq, ScopeId},
ok = wf_effect:cancel_effect(EffectId, {scope_cancelled, ScopeId}),

%% Check if effect is cancelled
true = wf_effect:is_cancelled(EffectId),
```

### Cancelled Receipts

Cancelled effects create receipts with `{cancelled, Reason}` results:

```erlang
%% After cancellation, effect result is {cancelled, Reason}
{cancelled, {scope_cancelled, ScopeId}} = wf_effect:get_result(EffectId),
```

## Receipts

### Receipt Structure

```erlang
-record(receipt, {
    receipt_id,        %% Unique identifier (reference)
    case_id,          %% Case ID for grouping
    effect_id,        %% Causal ID linking to effect
    effect_spec_hash, %% SHA-256 hash of effect_spec
    idempotency_key,  %% Idempotency key (if provided)
    timestamp,        %% Completion timestamp
    result,           %% {ok, term()} | {error, term()} | {cancelled, term()}
    duration_us       %% Execution duration in microseconds
}).
```

### Querying Receipts

```erlang
%% Lookup receipt by effect_id
{ok, Receipt} = wf_receipt:lookup(CaseId, EffectId),

%% Lookup receipt by idempotency_key
{ok, Receipt} = wf_receipt:lookup_by_key(CaseId, IdempotencyKey),

%% Get all receipts for a case
AllReceipts = wf_receipt:all(CaseId),

%% Verify receipt matches effect_spec
true = wf_receipt:verify(Receipt, EffectSpec),
```

## Effect Handlers

### Built-in Mock Handlers

The system includes mock handlers for testing:

- **mock_effect** - Returns `{ok, mock_result}`
- **mock_delay** - Returns `{ok, delayed_result}` after 100ms delay
- **mock_error** - Returns `{error, mock_error}`
- **mock_crash** - Crashes (wrapped as `{error, {handler_crashed, ...}}`)
- **http_get** - Mock HTTP GET returning status 200
- **file_write** - Mock file write returning bytes written

### Implementing Custom Handlers

Custom effect handlers can be added by extending `get_handler_for_type/1` in `wf_effect.erl`:

```erlang
get_handler_for_type(database_query) ->
    fun(Spec) ->
        Query = maps:get(query, Spec#effect_spec.payload),
        %% Execute database query
        case execute_query(Query) of
            {ok, Rows} -> {ok, #{rows => Rows}};
            {error, Reason} -> {error, Reason}
        end
    end;
get_handler_for_type(EffectType) ->
    fun(_Spec) -> {error, {unknown_effect_type, EffectType}} end.
```

## Executor Integration

### Executor Status

The executor has a `blocked_effect` status when waiting for effects:

```erlang
%% Executor state when blocked on effect
#exec_state{
    status = blocked_effect,
    current_token = TokenId,
    tokens = #{TokenId => #token{
        status = blocked_effect,
        current_effect = EffectId  %% Pending effect ID
    }}
}
```

### Polling for Effect Completion

The executor polls `wf_effect:get_result/1` to check for completion:

```erlang
%% In executor step function
case wf_effect:get_result(EffectId) of
    {ok, Result} ->
        %% Effect completed, resume task
        NewCtx = maps:put(effect_result, Result, ExecState#exec_state.ctx),
        ExecState#exec_state{status = running, ctx = NewCtx};
    {cancelled, Reason} ->
        %% Effect cancelled, propagate
        ExecState#exec_state{status = cancelled};
    {error, Reason} ->
        %% Effect failed
        ExecState#exec_state{status = failed};
    pending ->
        %% Effect still running, yield
        {ExecState, #{type => blocked_effect}}
end.
```

## Error Handling

### Effect Handler Errors

Effect handlers that crash are caught and wrapped:

```erlang
%% Handler crash
{error, {handler_crashed, error, Reason, Stacktrace}}
```

### Task Errors

Tasks that return errors fail the executor:

```erlang
%% Task returns error
{error, database_unavailable}

%% Executor transitions to failed status
#exec_state{status = failed}
```

## Best Practices

1. **Use Idempotency Keys** - For all effects that should be executed at-most-once (e.g., HTTP POST, database writes)

2. **Set Timeouts** - Prevent indefinite blocking by setting timeouts on long-running effects

3. **Handle Results** - Always check effect results before using them in task logic

4. **Unique IDs** - Ensure case_id, step_seq, and scope_id combine to form unique effect IDs

5. **Audit Trail** - Query receipts for debugging and auditing workflow execution

6. **Cancellation** - Always check `wf_effect:is_cancelled/1` before proceeding with dependent effects

7. **Error Propagation** - Design tasks to handle `{error, Reason}` results gracefully

## Future Enhancements (v2+)

- Gen_server-based effect handlers with supervision
- Receipt persistence to disk (dets or custom)
- Receipt log rotation and retention policies
- Timer-based effect timeout enforcement
- Automatic retry with exponential backoff
- Streaming effect results for large payloads
- Effect spec validation with schemas
- Dynamic effect handler registration
- Integration with wf_trace (item 011)
