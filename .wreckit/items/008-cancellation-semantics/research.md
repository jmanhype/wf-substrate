# Research: Implement structured cancellation semantics

**Date**: 2025-01-10
**Item**: 008-cancellation-semantics

## Research Question

Implement wf_cancel.erl: structured cancellation for three scopes:
1. Activity cancellation: cancel a single task. If the task is running (has yielded an effect), the effect is marked cancelled. If not yet started, the task is skipped. Produces a cancel_activity event.
2. Case cancellation: cancel an entire workflow instance. All active branches, pending effects, and running tasks are cancelled. The case transitions to a terminal cancelled state. Produces a cancel_case event.
3. Region cancellation: cancel a scoped subtree identified by cancel scope ID. Only tokens and tasks within the named scope are cancelled. Tokens outside the scope are unaffected. Produces cancel_region events for each affected item.

Cancellation propagation is O(scope size) — it walks only the tokens/tasks in the affected scope, not the entire workflow. Cancelled scopes produce structured cancel events with: scope_id, cancelled_tokens, cancelled_effects, timestamp. Cancellation does not corrupt unrelated scopes — verified by invariant checks.

Integrates with wf_exec (scope stack), wf_state (scope metadata), and wf_effect (effect cancellation). Export: cancel_activity/3, cancel_case/1, cancel_region/3, is_cancelled/2.

## Summary

This task involves implementing `wf_cancel.erl`, a structured cancellation system that supports three granularities of cancellation: **activity** (single task), **case** (entire workflow instance), and **region** (scoped subtree). The cancellation system must propagate cancellation signals efficiently (O(scope size), not O(workflow size)) while maintaining state consistency and producing structured audit events.

The implementation is **item 008** in the sequence and builds on existing infrastructure:
- **wf_exec** (item 005): Provides scope stack tracking via `CANCEL_SCOPE` opcodes (lines 477-510 of wf_exec.erl)
- **wf_state** (item 006): Provides scope metadata storage with `#scope{}` records containing status fields
- **wf_effect** (item 010): Provides effect cancellation interface (not yet implemented)

**Current State**: The executor has stub cancellation support with placeholder functions `is_scope_cancelled/2` and `propagate_cancellation/2` (wf_exec.erl:513-529). These stubs are marked with TODO comments to call `wf_cancel` when item 008 is implemented. The state store already supports scope tracking with `#scope{status := active | cancelled}` records (wf_state.erl:84-90).

**Key Architectural Constraints**:
1. **Efficient propagation**: Must walk only tokens/tasks in affected scope (O(scope_size)), not entire workflow
2. **State consistency**: Cancellation must not corrupt unrelated scopes
3. **Structured events**: Each cancellation produces audit events with scope_id, cancelled_tokens, cancelled_effects, timestamp
4. **Effect integration**: Must integrate with wf_effect for cancelling running effects

## Current State Analysis

### Existing Implementation

**Current State**: No `wf_cancel.erl` module exists yet. However, the infrastructure for cancellation is partially in place:

**1. Executor Cancellation Opcodes** (`/Users/speed/wf-substrate/src/wf_exec.erl:473-529`):

```erlang
%% @doc Execute CANCEL_SCOPE: enter or exit cancel scope
execute_cancel_scope({_CancelScope, {enter, ScopeId}}, ExecState) ->
    %% Push scope onto stack
    NewScopeStack = [ScopeId | ExecState#exec_state.scope_stack],
    ExecState#exec_state{
        scope_stack = NewScopeStack,
        ip = ExecState#exec_state.ip + 1,
        step_count = ExecState#exec_state.step_count + 1
    };

execute_cancel_scope({_CancelScope, {exit, ScopeId}}, ExecState) ->
    %% Pop scope from stack
    [_Top | Rest] = ExecState#exec_state.scope_stack,
    NewScopeStack = Rest,

    %% Check if scope is cancelled (stub for wf_cancel)
    case is_scope_cancelled(ScopeId, ExecState) of
        true ->
            %% Propagate cancellation to all tokens in scope
            Tokens = propagate_cancellation(ScopeId, ExecState#exec_state.tokens),
            ExecState#exec_state{
                scope_stack = NewScopeStack,
                tokens = Tokens,
                status = cancelled,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        false ->
            %% Normal exit
            ExecState#exec_state{
                scope_stack = NewScopeStack,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Check if scope is cancelled (stub for wf_cancel)
-spec is_scope_cancelled(term(), exec_state()) -> boolean().
is_scope_cancelled(_ScopeId, _ExecState) ->
    %% Stub: always false for now
    %% TODO: Call wf_cancel:is_cancelled/2 when item 008 is implemented
    false.

%% @doc Propagate cancellation to all tokens in scope (stub for wf_cancel)
-spec propagate_cancellation(term(), #{term() => #token{}}) -> #{term() => #token{}}.
propagate_cancellation(ScopeId, TokensMap) ->
    %% Stub: mark all tokens in scope as cancelled
    %% TODO: Call wf_cancel:propagate/2 when item 008 is implemented
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).
```

**Analysis**:
- Lines 477-510: `CANCEL_SCOPE` opcode handlers push/pop scopes from stack
- Lines 492-502: On scope exit, checks if cancelled (currently always false)
- Lines 513-517: `is_scope_cancelled/2` stub (always returns false)
- Lines 519-529: `propagate_cancellation/2` stub (marks tokens in scope as cancelled)
- **TODO comments** indicate where wf_cancel integration should happen

**2. State Store Scope Tracking** (`/Users/speed/wf-substrate/src/wf_state.erl:83-90, 121, 149, 545-549`):

```erlang
%% Scope record: Cancel scope
-record(scope, {
    scope_id :: scope_id(),
    parent_scope :: scope_id() | undefined,
    status :: active | cancelled,
    tokens :: [token_id()],                   %% Tokens in this scope
    entered_at :: erlang:timestamp()          %% Entry timestamp
}).

%% In state record:
scopes :: #{scope_id() => #scope{}},     %% Cancel scopes

%% Mutation types:
{cancel_scope, scope_id()} |               %% Cancel a scope

%% Apply mutation:
apply_single_mutation(#mutation{type = {cancel_scope, ScopeId}}, State) ->
    Scopes = State#state.scopes,
    Scope = maps:get(ScopeId, Scopes),
    UpdatedScope = Scope#scope{status = cancelled},
    State#state{scopes = Scopes#{ScopeId => UpdatedScope}}.
```

**Analysis**:
- Scope records have `status` field (active/cancelled)
- State store tracks scopes in a map
- `{cancel_scope, ScopeId}` mutation type exists
- Scope status can be updated atomically via commit protocol

**3. Token Status Tracking** (`/Users/speed/wf-substrate/src/wf_exec.erl:28-34`):

```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),                     %% Current scope membership
    value :: term(),
    status :: active | complete | cancelled   %% Token status
}).
```

**Analysis**:
- Tokens have `scope_id` field indicating which scope they belong to
- Tokens have `status` field (active/complete/cancelled)
- Scope membership tracked per token

**4. Cancellation Tests** (`/Users/speed/wf-substrate/test/wf_exec_tests.erl:183-238`):

```erlang
%% Test cancel scope enter and exit
cancel_scope_test_() ->
    Bytecode = mock_bytecode_cancel(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute CANCEL_SCOPE enter
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),
    %% Execute TASK_EXEC
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    %% Execute CANCEL_SCOPE exit
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    [
        ?_assertEqual(2, wf_exec:get_scope_stack_depth(ExecState1)),  %% [root, scope1]
        ?_assertEqual(1, wf_exec:get_scope_stack_depth(ExecState3)),  %% [root]
        ?_assertNot(wf_exec:is_done(ExecState3))  %% Still running
    ].

%% Test nested cancel scopes
nested_cancel_scope_test_() ->
    Bytecode = [
        {'CANCEL_SCOPE', {enter, scope1}},
        {'CANCEL_SCOPE', {enter, scope2}},
        {'TASK_EXEC', task_a},
        {'CANCEL_SCOPE', {exit, scope2}},
        {'CANCEL_SCOPE', {exit, scope1}},
        {'DONE'}
    ],
    ...
```

**Analysis**:
- Tests verify scope stack manipulation
- Tests verify nested scopes work correctly
- **No tests for actual cancellation** (stubs always return false)

### Integration Points

**1. wf_exec Integration** (item 005 - executor):

Current stub functions need implementation:
- `is_scope_cancelled/2` → should call `wf_cancel:is_cancelled/2`
- `propagate_cancellation/2` → should call `wf_cancel:propagate/2`

**2. wf_state Integration** (item 006 - state store):

Scope management functions exist:
- `wf_state:enter_scope/2` - Enter cancel scope
- `wf_state:exit_scope/2` - Exit cancel scope
- `wf_state:get_scope/2` - Get scope metadata
- Scope status can be updated via `{cancel_scope, ScopeId}` mutation

**3. wf_effect Integration** (item 010 - effect boundary):

Effect cancellation interface (not yet implemented):
- `wf_effect:cancel_effect/2` - Cancel a running effect
- `wf_effect:is_cancelled/1` - Check if effect is cancelled
- Effects yielded by tasks must be cancelable when scope is cancelled

**4. wf_vm Integration** (item 004 - bytecode):

Opcode already defined:
- `{cancel_scope, {enter | exit, term()}}` - CANCEL_SCOPE opcode (wf_vm.erl:19)

### Specification Context

**From PROMPT.md:57, 102, 117, 133**:

```
Module: wf_cancel.erl
- cancellation semantics (activity/case/region)

Types:
- scope_id() identifies cancellation scopes

Pattern Constructors:
- cancel(ScopeSpec, P) - scope/region cancellation wrapper

Semantics:
- cancel propagates cancellation signals to scoped subterms
```

**From item.json:6** (item 008 specification):

```
Three cancellation scopes:
1. Activity cancellation: cancel a single task. If the task is running (has yielded an
   effect), the effect is marked cancelled. If not yet started, the task is skipped.
   Produces a cancel_activity event.

2. Case cancellation: cancel an entire workflow instance. All active branches, pending
   effects, and running tasks are cancelled. The case transitions to a terminal cancelled
   state. Produces a cancel_case event.

3. Region cancellation: cancel a scoped subtree identified by cancel scope ID. Only tokens
   and tasks within the named scope are cancelled. Tokens outside the scope are unaffected.
   Produces cancel_region events for each affected item.

Cancellation propagation is O(scope size) — it walks only the tokens/tasks in the affected
scope, not the entire workflow. Cancelled scopes produce structured cancel events with:
scope_id, cancelled_tokens, cancelled_effects, timestamp. Cancellation does not corrupt
unrelated scopes — verified by invariant checks.
```

**From ARCHITECTURE.md:1260-1300** (module specification - inferred from pattern):

```
Role: Cancellation semantics for workflow execution.

Exports:
- cancel_activity/3: Cancel single task (CasePidOrId, TaskId, Options)
- cancel_case/1: Cancel entire case (CasePidOrId)
- cancel_region/3: Cancel scoped region (CasePidOrId, ScopeId, Options)
- is_cancelled/2: Check if token/effect cancelled (CasePidOrId, TokenOrEffectId)

Dependencies:
- wf_exec (scope stack)
- wf_state (scope metadata)
- wf_effect (effect cancellation)
```

## Key Files

### Existing Files

- `/Users/speed/wf-substrate/src/wf_exec.erl:473-529` - **Cancellation opcode handlers**
  - Lines 477-510: `execute_cancel_scope/2` handles CANCEL_SCOPE enter/exit
  - Lines 513-517: `is_scope_cancelled/2` stub (marked TODO for wf_cancel)
  - Lines 519-529: `propagate_cancellation/2` stub (marked TODO for wf_cancel)
  - **Integration point**: Replace stubs with wf_cancel calls

- `/Users/speed/wf-substrate/src/wf_state.erl:83-90, 121, 149, 545-549` - **Scope metadata storage**
  - Lines 83-90: `#scope{}` record definition with status field
  - Lines 121: `scopes` field in state record
  - Lines 149: `{cancel_scope, scope_id()}` mutation type
  - Lines 545-549: `apply_single_mutation` for cancel_scope
  - **Integration point**: Query/update scope status via wf_state API

- `/Users/speed/wf-substrate/src/wf_exec.erl:28-34` - **Token record with scope membership**
  - Lines 28-34: `#token{}` record with `scope_id` and `status` fields
  - **Integration point**: Token scope membership used for region cancellation

- `/Users/speed/wf-substrate/src/wf_vm.erl:19` - **Opcode definition**
  - Lines 19: `{cancel_scope, {enter | exit, term()}}` opcode type
  - **Already defined**, no changes needed

- `/Users/speed/wf-substrate/test/wf_exec_tests.erl:183-238` - **Cancellation tests**
  - Lines 183-209: Test scope enter/exit
  - Lines 211-238: Test nested scopes
  - **Missing**: Tests for actual cancellation (stubs always return false)

### Specification Files

- `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/item.json:1-14` - **Item specification**
  - Lines 6-7: Three cancellation scopes (activity, case, region)
  - Lines 6-7: Cancellation event structure (scope_id, cancelled_tokens, cancelled_effects, timestamp)
  - Lines 6-7: O(scope size) propagation requirement
  - Lines 6-7: Invariant checks requirement
  - Lines 6-7: Integration with wf_exec, wf_state, wf_effect
  - Lines 6-7: Export requirements: cancel_activity/3, cancel_case/1, cancel_region/3, is_cancelled/2

- `/Users/speed/wf-substrate/PROMPT.md:57, 102, 117, 133` - **Module specification**
  - Module: wf_cancel.erl
  - Pattern: cancel(ScopeSpec, P)
  - Semantics: cancel propagates cancellation to scoped subterms

### Files to Create

- `src/wf_cancel.erl` - **Cancellation semantics module** (primary deliverable)
  - `-type cancel_event()` - Cancel event record
  - `-type cancel_scope()` - Activity, case, or region
  - `cancel_activity/3` - Cancel single task
  - `cancel_case/1` - Cancel entire case
  - `cancel_region/3` - Cancel scoped region
  - `is_cancelled/2` - Check if token/effect cancelled
  - `propagate/2` - Propagate cancellation to scope (internal)
  - `create_cancel_event/4` - Create structured event (internal)

- `test/wf_cancel_tests.erl` - **Cancellation tests**
  - Test activity cancellation (task with effect)
  - Test activity cancellation (task not yet started)
  - Test case cancellation (all tokens cancelled)
  - Test region cancellation (only tokens in scope)
  - Test nested region cancellation
  - Test cancellation events structure
  - Test O(scope size) complexity
  - Test invariant preservation (unrelated scopes not corrupted)
  - Test integration with wf_exec
  - Test integration with wf_state

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Modules Needed**:
- `lists` - For walking token maps
- `maps` - For scope/token lookups
- `erlang:timestamp/0` - For event timestamps

**Internal Module Dependencies**:
- **wf_exec (item 005)**: Provides scope stack, token tracking
  - Executor calls `wf_cancel:is_cancelled/2` in `is_scope_cancelled/2`
  - Executor calls `wf_cancel:propagate/2` in `propagate_cancellation/2`
  - Executor passes token map and scope stack to wf_cancel

- **wf_state (item 006)**: Provides scope metadata storage
  - `wf_cancel` queries scope status via `wf_state:get_scope/2`
  - `wf_cancel` updates scope status via `{cancel_scope, ScopeId}` mutation
  - `wf_cancel` commits mutations atomically

- **wf_effect (item 010)**: Provides effect cancellation interface
  - `wf_cancel:cancel_activity/3` calls `wf_effect:cancel_effect/2` for running effects
  - `wf_cancel:is_cancelled/2` calls `wf_effect:is_cancelled/1` for effect status
  - **Not implemented yet**, stub or mock for v1

**No Circular Dependencies**: wf_cancel depends on wf_state (scopes) and wf_exec (tokens), but these are foundational. Safe to implement independently.

### Cancellation Semantics Design

**From item.json:6** - Three cancellation scopes:

**1. Activity Cancellation** (single task):
```erlang
cancel_activity(CasePidOrId, TaskId, Options) ->
    %% Find token executing task
    TokenId = find_token_for_task(CasePidOrId, TaskId),

    %% Check if task has yielded effect
    case get_current_effect(TokenId) of
        {ok, EffectId} ->
            %% Task running, cancel effect
            wf_effect:cancel_effect(EffectId, Options),
            mark_token_cancelled(TokenId);
        undefined ->
            %% Task not started, skip
            mark_token_cancelled(TokenId)
    end,

    %% Produce event
    Event = #cancel_activity{
        scope_id = TaskId,
        cancelled_tokens = [TokenId],
        cancelled_effects = [EffectId],
        timestamp = erlang:timestamp()
    },
    {ok, Event}.
```

**Key Points**:
- Task identified by TaskId (atomic task name or generated ID)
- If task yielded effect, effect is cancelled via wf_effect
- If task not yet started, token is marked cancelled (skipped)
- Produces `cancel_activity` event

**2. Case Cancellation** (entire workflow):
```erlang
cancel_case(CasePidOrId) ->
    %% Get all active tokens
    State = wf_state:get_state(CasePidOrId),
    Tokens = wf_state:get_tokens(State),
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],

    %% Cancel all tokens
    CancelledTokens = [mark_token_cancelled(T#token.token_id) || T <- ActiveTokens],

    %% Cancel all pending effects
    CancelledEffects = [cancel_effect_for_token(T) || T <- ActiveTokens],

    %% Mark case status as cancelled
    wf_state:buffer_mutation(State, {set_case_status, cancelled}),
    wf_state:commit(State),

    %% Produce event
    Event = #cancel_case{
        scope_id = CasePidOrId,
        cancelled_tokens = [T#token.token_id || T <- CancelledTokens],
        cancelled_effects = CancelledEffects,
        timestamp = erlang:timestamp()
    },
    {ok, Event}.
```

**Key Points**:
- Cancels all active tokens in case
- Cancels all pending effects yielded by tokens
- Marks case with terminal `cancelled` status
- Produces `cancel_case` event
- **Note**: Case runner must detect cancelled status and terminate

**3. Region Cancellation** (scoped subtree):
```erlang
cancel_region(CasePidOrId, ScopeId, Options) ->
    %% Get scope metadata
    State = wf_state:get_state(CasePidOrId),
    Scope = wf_state:get_scope(State, ScopeId),

    case Scope of
        undefined ->
            {error, {scope_not_found, ScopeId}};
        #scope{status = cancelled} ->
            {error, {already_cancelled, ScopeId}};
        #scope{} ->
            %% Find tokens in scope (O(tokens_in_scope))
            Tokens = wf_state:get_tokens(State),
            TokensInScope = [T || {_, T} <- maps:to_list(Tokens),
                                 T#token.scope_id =:= ScopeId],

            %% Mark tokens as cancelled
            CancelledTokens = [mark_token_cancelled(T#token.token_id) || T <- TokensInScope],

            %% Cancel effects yielded by tokens in scope
            CancelledEffects = [cancel_effect_for_token(T) || T <- TokensInScope],

            %% Update scope status to cancelled
            wf_state:buffer_mutation(State, {cancel_scope, ScopeId}),
            wf_state:commit(State),

            %% Produce event
            Event = #cancel_region{
                scope_id = ScopeId,
                cancelled_tokens = [T#token.token_id || T <- CancelledTokens],
                cancelled_effects = CancelledEffects,
                timestamp = erlang:timestamp()
            },
            {ok, Event}
    end.
```

**Key Points**:
- Only cancels tokens with `scope_id = ScopeId`
- Tokens outside scope are unaffected (invariant preservation)
- O(tokens_in_scope) complexity, not O(all_tokens)
- Produces `cancel_region` event
- Scope status updated atomically via commit protocol

### Cancel Event Structure

**From item.json:6**:

```erlang
-record(cancel_activity, {
    scope_id :: task_id(),                    %% Task ID
    cancelled_tokens :: [token_id()],
    cancelled_effects :: [effect_id()],
    timestamp :: erlang:timestamp()
}).

-record(cancel_case, {
    scope_id :: case_id(),                    %% Case ID
    cancelled_tokens :: [token_id()],
    cancelled_effects :: [effect_id()],
    timestamp :: erlang:timestamp()
}).

-record(cancel_region, {
    scope_id :: scope_id(),                   %% Scope ID
    cancelled_tokens :: [token_id()],
    cancelled_effects :: [effect_id()],
    timestamp :: erlang:timestamp()
}).

-type cancel_event() :: #cancel_activity{} | #cancel_case{} | #cancel_region{}.
```

**Event Fields**:
- `scope_id`: Identifier of cancelled scope (task_id, case_id, or scope_id)
- `cancelled_tokens`: List of token IDs cancelled
- `cancelled_effects`: List of effect IDs cancelled
- `timestamp`: When cancellation occurred

### Invariant Checks

**From item.json:6** ("Cancellation does not corrupt unrelated scopes — verified by invariant checks"):

**Invariants to Verify**:

1. **Scope Isolation**: Tokens in unrelated scopes remain active
   ```erlang
   verify_scope_isolation(State, CancelledScopeId) ->
       Tokens = wf_state:get_tokens(State),
       ActiveTokensOutsideScope = [T || {_, T} <- maps:to_list(Tokens),
                                       T#token.scope_id =/= CancelledScopeId,
                                       T#token.status =:= active],
       %% All tokens outside cancelled scope should still be active
       case ActiveTokensOutsideScope of
           [] -> ok;
           _ -> {error, {tokens_corrupted, ActiveTokensOutsideScope}}
       end.
   ```

2. **Scope Nesting**: Cancelling parent scope cancels child scopes
   ```erlang
   verify_scope_nesting(State, ParentScopeId) ->
       Scopes = wf_state:get_scopes(State),
       ChildScopes = [S || {_, S} <- maps:to_list(Scopes),
                           S#scope.parent_scope =:= ParentScopeId],
       %% All child scopes should be cancelled
       CancelledChildren = [S || S <- ChildScopes, S#scope.status =:= cancelled],
       case length(CancelledChildren) =:= length(ChildScopes) of
           true -> ok;
           false -> {error, {child_scopes_not_cancelled, ChildScopes}}
       end.
   ```

3. **No Orphaned Tokens**: All cancelled tokens belong to cancelled scope
   ```erlang
   verify_no_orphaned_tokens(State, CancelledScopeId) ->
       Tokens = wf_state:get_tokens(State),
       CancelledTokens = [T || {_, T} <- maps:to_list(Tokens),
                               T#token.status =:= cancelled],
       %% All cancelled tokens should have scope_id = CancelledScopeId
       OrphanedTokens = [T || T <- CancelledTokens,
                             T#token.scope_id =/= CancelledScopeId],
       case OrphanedTokens of
           [] -> ok;
           _ -> {error, {orphaned_cancelled_tokens, OrphanedTokens}}
       end.
   ```

**Invariant Enforcement**:
- Run invariants after each cancellation operation
- Return error if invariants violated
- Used in tests to verify correctness

### Integration with wf_exec

**Current stub functions** (wf_exec.erl:513-529):

```erlang
%% @doc Check if scope is cancelled (stub for wf_cancel)
-spec is_scope_cancelled(term(), exec_state()) -> boolean().
is_scope_cancelled(_ScopeId, _ExecState) ->
    %% Stub: always false for now
    %% TODO: Call wf_cancel:is_cancelled/2 when item 008 is implemented
    false.

%% @doc Propagate cancellation to all tokens in scope (stub for wf_cancel)
-spec propagate_cancellation(term(), #{term() => #token{}}) -> #{term() => #token{}}.
propagate_cancellation(ScopeId, TokensMap) ->
    %% Stub: mark all tokens in scope as cancelled
    %% TODO: Call wf_cancel:propagate/2 when item 008 is implemented
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).
```

**Required Changes**:

1. **Replace `is_scope_cancelled/2`**:
   ```erlang
   is_scope_cancelled(ScopeId, ExecState) ->
       %% Delegate to wf_cancel
       wf_cancel:is_cancelled(ExecState#exec_state.state, ScopeId).
   ```

2. **Replace `propagate_cancellation/2`**:
   ```erlang
   propagate_cancellation(ScopeId, TokensMap) ->
       %% Delegate to wf_cancel
       wf_cancel:propagate(ScopeId, TokensMap).
   ```

**Note**: wf_exec needs `state` field (wf_state:state()) to pass to wf_cancel. Currently, exec_state has inline state (ctx, tokens, scopes). This must be refactored to use wf_state (item 006 follow-up).

### Integration with wf_effect

**From item 010** (wf_effect - not implemented yet):

**Effect cancellation interface** (inferred from item.json:6):
```erlang
%% Cancel a running effect
-spec cancel_effect(effect_id(), options()) -> ok | {error, term()}.
cancel_effect(EffectId, Options) ->
    %% Mark effect as cancelled
    %% Discard result if already completed
    %% Unblock token waiting for effect
    ok.

%% Check if effect is cancelled
-spec is_cancelled(effect_id()) -> boolean().
is_cancelled(EffectId) ->
    %% Check effect status
    false.
```

**Integration in wf_cancel**:

```erlang
%% In cancel_activity/3
cancel_activity(CasePidOrId, TaskId, Options) ->
    TokenId = find_token_for_task(CasePidOrId, TaskId),

    case get_current_effect(TokenId) of
        {ok, EffectId} ->
            %% Task running, cancel effect
            case wf_effect:cancel_effect(EffectId, Options) of
                ok -> mark_token_cancelled(TokenId);
                {error, Reason} -> {error, {effect_cancel_failed, Reason}}
            end;
        undefined ->
            %% Task not started
            mark_token_cancelled(TokenId)
    end.
```

**Stub for v1** (wf_effect not implemented):
```erlang
%% Stub effect cancellation (wf_effect not implemented yet)
cancel_effect_for_token(Token) ->
    %% For v1, assume no effect yielded
    %% TODO: Integrate with wf_effect when item 010 implemented
    undefined.
```

### O(Scope Size) Complexity

**From item.json:6** ("Cancellation propagation is O(scope size)"):

**Current stub** (wf_exec.erl:519-529):
```erlang
propagate_cancellation(ScopeId, TokensMap) ->
    %% This is O(total_tokens), not O(scope_tokens)!
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).
```

**Problem**: Iterates all tokens, even if only one token in scope.

**Optimized approach**:
```erlang
propagate(ScopeId, TokensMap) ->
    %% Only iterate tokens in scope (O(scope_tokens))
    maps:filtermap(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId ->
                %% Token in scope, mark cancelled
                {true, Token#token{status = cancelled}};
            _OtherScopeId ->
                %% Token outside scope, unchanged
                false
        end
    end, TokensMap).
```

**Alternative** (if scope tracks token list):
```erlang
%% If scope#scope.tokens tracks tokens in scope
propagate(ScopeId, TokensMap) ->
    Scope = wf_state:get_scope(State, ScopeId),
    TokenIdsInScope = Scope#scope.tokens,

    %% Only update tokens in scope (O(scope_tokens))
    lists:foldl(fun(TokenId, AccTokens) ->
        Token = maps:get(TokenId, AccTokens),
        AccTokens#{TokenId => Token#token{status = cancelled}}
    end, TokensMap, TokenIdsInScope).
```

**Recommendation**: Add `tokens` field to `#scope{}` record to track tokens in scope (list of token_ids). Enables O(scope_tokens) cancellation without scanning all tokens.

### Event Production and Logging

**From item.json:6** ("Cancelled scopes produce structured cancel events"):

**Event Logging Strategy**:

1. **Create event record**:
   ```erlang
   create_cancel_event(ScopeType, ScopeId, CancelledTokens, CancelledEffects) ->
       Event = case ScopeType of
           activity -> #cancel_activity{...};
           case -> #cancel_case{...};
           region -> #cancel_region{...}
       end,
       Event.
   ```

2. **Log event** (for item 011 - tracing):
   ```erlang
   %% For v1, return event to caller
   %% For v2, send to wf_trace sink (item 011)
   ```

3. **Event format**:
   ```erlang
   {cancel_activity, Timestamp, ScopeId, CancelledTokens, CancelledEffects}
   {cancel_case, Timestamp, ScopeId, CancelledTokens, CancelledEffects}
   {cancel_region, Timestamp, ScopeId, CancelledTokens, CancelledEffects}
   ```

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **O(n) cancellation walks all tokens** | High | Current stub iterates all tokens. Mitigation: Add token list to scope record, cancel only tokens in scope. O(scope_tokens) complexity. Verify with performance tests. |
| **Scope tracking inconsistency** | High | Token scope_id may not match actual scope. Mitigation: Validate scope_id exists in state before cancellation. Add invariant checks. Property-based tests for scope consistency. |
| **Race conditions (concurrent cancellation)** | Medium | Multiple processes may cancel same scope simultaneously. Mitigation: Use wf_state commit protocol (atomic mutations). First cancellation wins, subsequent get {error, already_cancelled}. |
| **Effect cancellation not implemented** | Medium | wf_effect not implemented yet (item 010). Mitigation: Stub effect cancellation for v1. Add TODO to integrate when item 010 complete. |
| **Nested scope cancellation bugs** | High | Cancelling parent scope should cancel all child scopes. Mitigation: Recursive cancellation (walk scope tree). Validate invariants. Tests for deeply nested scopes. |
| **Case cancellation incomplete** | High | Some tokens may not be cancelled. Mitigation: Enumerate all active tokens before cancellation. Verify all tokens cancelled via invariant checks. |
| **Event structure mismatch** | Medium | Cancel events may not match tracing format (item 011). Mitigation: Use compatible event format (tuples with timestamp). Document structure clearly. |
| **State corruption from partial cancellation** | High | Crash mid-cancellation leaves partial state. Mitigation: Use wf_state commit protocol (atomic mutations). All-or-nothing semantics. Rollback on error. |
| **Token lookup performance** | Medium | Finding token for task ID may be O(n). Mitigation: Add task_id → token_id index (separate map). For v1, linear scan acceptable. |
| **Scope exit before cancellation** | Low | Token exits scope before cancellation processed. Mitigation: Check token current scope_id before cancelling. Skip if token already exited. |
| **Orphaned effects** | High | Effect not cancelled when token cancelled. Mitigation: Track effect→token mapping. Cancel all effects for cancelled tokens. Verify no pending effects post-cancellation. |
| **wf_exec integration missing state field** | High | wf_exec doesn't have wf_state field yet. Mitigation: Stub with inline state for v1. Add TODO to refactor when item 006 integration complete. |
| **Test coverage gaps** | Medium | Cancellation has many edge cases (nested scopes, partial cancellation). Mitigation: Comprehensive unit tests. Property-based tests for invariants. Integration tests with wf_exec. |
| **Performance regression** | Medium | Cancellation overhead may slow executor. Mitigation: Profile hot paths. Optimize token lookup (add index). Benchmark cancellation latency. |
| **Event log growth** | Low | Cancel events accumulate indefinitely. Mitigation: For v1, accept growth (events small). Future: event log rotation (item 011). |
| **Backward compatibility** | Low | Stub functions changed may break existing tests. Mitigation: Keep stub behavior as default (no cancellation). Add opt-in cancellation via options. |

## Recommended Approach

**High-Level Strategy**: Implement wf_cancel module with three cancellation functions (activity, case, region). Use wf_state for scope metadata and atomic commits. Integrate with wf_exec stub functions. Add comprehensive tests for correctness and invariants. Stub wf_effect integration (item 010 not yet implemented).

### Phase 1: Types and Records

**1. Define types and records** (`src/wf_cancel.erl`):
```erlang
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

%% Type exports
-export_type([
    cancel_event/0,
    cancel_scope/0
]).
```

**2. Define cancellation scope types**:
```erlang
-type cancel_scope() ::
    {activity, task_id()} |
    {case, case_id()} |
    {region, scope_id()}.

-type cancel_event() :: #cancel_activity{} | #cancel_case{} | #cancel_region{}.
```

**3. Test type compilation**

### Phase 2: Region Cancellation (Core)

**1. Implement `cancel_region/3`**:
```erlang
-spec cancel_region(case_id(), scope_id(), proplists:proplist()) ->
    {ok, cancel_event()} | {error, term()}.
cancel_region(CaseId, ScopeId, Options) ->
    %% Get state
    {ok, State} = wf_state:restore_from_ets(CaseId),

    %% Check scope exists and not already cancelled
    case wf_state:get_scope(State, ScopeId) of
        undefined ->
            {error, {scope_not_found, ScopeId}};
        #scope{status = cancelled} ->
            {error, {already_cancelled, ScopeId}};
        #scope{} = Scope ->
            %% Find tokens in scope
            Tokens = wf_state:get_tokens(State),
            TokensInScope = filter_tokens_in_scope(Tokens, ScopeId),

            %% Mark tokens as cancelled
            CancelledTokens = mark_tokens_cancelled(TokensInScope),

            %% Cancel effects (stub for wf_effect)
            CancelledEffects = cancel_effects_for_tokens(TokensInScope),

            %% Update scope status
            {ok, NewState} = wf_state:buffer_mutation(
                State,
                {cancel_scope, ScopeId}
            ),
            {ok, _State2, _Receipt} = wf_state:commit(NewState),

            %% Create event
            Event = #cancel_region{
                scope_id = ScopeId,
                cancelled_tokens = [T#token.token_id || T <- CancelledTokens],
                cancelled_effects = CancelledEffects,
                timestamp = erlang:timestamp()
            },
            {ok, Event}
    end.
```

**2. Implement helper functions**:
```erlang
%% Filter tokens by scope_id
filter_tokens_in_scope(TokensMap, ScopeId) ->
    [T || {_, T} <- maps:to_list(TokensMap), T#token.scope_id =:= ScopeId].

%% Mark tokens as cancelled
mark_tokens_cancelled(Tokens) ->
    [T#token{status = cancelled} || T <- Tokens].

%% Cancel effects for tokens (stub for wf_effect)
cancel_effects_for_tokens(Tokens) ->
    %% TODO: Integrate with wf_effect when item 010 implemented
    [].
```

**3. Test region cancellation**:
```erlang
cancel_region_test_() ->
    %% Create state with scope and tokens
    State = create_test_state_with_scope(),
    ScopeId = test_scope,

    %% Cancel region
    {ok, Event} = wf_cancel:cancel_region(State, ScopeId, []),

    %% Verify event structure
    ?assertEqual(ScopeId, Event#cancel_region.scope_id),
    ?assertEqual(2, length(Event#cancel_region.cancelled_tokens)),
    ?assertEqual(0, length(Event#cancel_region.cancelled_effects)).

cancel_region_preserves_other_scopes_test_() ->
    %% Create state with multiple scopes
    State = create_test_state_with_two_scopes(),
    ScopeId1 = scope1,
    ScopeId2 = scope2,

    %% Cancel scope1
    {ok, _Event} = wf_cancel:cancel_region(State, ScopeId1, []),

    %% Verify scope2 tokens still active (invariant)
    Tokens = wf_state:get_tokens(State),
    TokensInScope2 = [T || {_, T} <- maps:to_list(Tokens),
                         T#token.scope_id =:= scope2],
    ?assertEqual([active, active], [T#token.status || T <- TokensInScope2]).
```

### Phase 3: Activity Cancellation

**1. Implement `cancel_activity/3`**:
```erlang
-spec cancel_activity(case_id(), task_id(), proplists:proplist()) ->
    {ok, cancel_event()} | {error, term()}.
cancel_activity(CaseId, TaskId, Options) ->
    %% Get state
    {ok, State} = wf_state:restore_from_ets(CaseId),

    %% Find token executing task
    Tokens = wf_state:get_tokens(State),
    Token = find_token_for_task(Tokens, TaskId),

    case Token of
        undefined ->
            {error, {task_not_found, TaskId}};
        #token{} ->
            %% Check if task yielded effect
            case get_effect_for_token(Token) of
                {ok, EffectId} ->
                    %% Cancel effect
                    case wf_effect:cancel_effect(EffectId, Options) of
                        ok ->
                            CancelledToken = Token#token{status = cancelled},
                            Event = #cancel_activity{
                                scope_id = TaskId,
                                cancelled_tokens = [CancelledToken#token.token_id],
                                cancelled_effects = [EffectId],
                                timestamp = erlang:timestamp()
                            },
                            {ok, Event};
                        {error, Reason} ->
                            {error, {effect_cancel_failed, Reason}}
                    end;
                undefined ->
                    %% Task not started, skip
                    CancelledToken = Token#token{status = cancelled},
                    Event = #cancel_activity{
                        scope_id = TaskId,
                        cancelled_tokens = [CancelledToken#token.token_id],
                        cancelled_effects = [],
                        timestamp = erlang:timestamp()
                    },
                    {ok, Event}
            end
    end.
```

**2. Implement helper functions**:
```erlang
%% Find token by task ID (assumes task_id stored in token.value)
find_token_for_task(TokensMap, TaskId) ->
    Tokens = maps:values(TokensMap),
    case [T || T <- Tokens, T#token.value =:= TaskId] of
        [Token] -> Token;
        [] -> undefined
    end.

%% Get effect for token (stub for wf_effect)
get_effect_for_token(Token) ->
    %% TODO: Integrate with wf_effect when item 010 implemented
    undefined.
```

**3. Test activity cancellation**:
```erlang
cancel_activity_with_effect_test_() ->
    %% Create token with effect
    State = create_test_state_with_effect(),
    TaskId = task1,

    %% Cancel activity
    {ok, Event} = wf_cancel:cancel_activity(State, TaskId, []),

    %% Verify effect cancelled
    ?assertEqual(1, length(Event#cancel_activity.cancelled_effects)).

cancel_activity_without_effect_test_() ->
    %% Create token without effect
    State = create_test_state_without_effect(),
    TaskId = task2,

    %% Cancel activity
    {ok, Event} = wf_cancel:cancel_activity(State, TaskId, []),

    %% Verify no effects cancelled
    ?assertEqual(0, length(Event#cancel_activity.cancelled_effects)).
```

### Phase 4: Case Cancellation

**1. Implement `cancel_case/1`**:
```erlang
-spec cancel_case(case_id()) -> {ok, cancel_event()} | {error, term()}.
cancel_case(CaseId) ->
    %% Get state
    {ok, State} = wf_state:restore_from_ets(CaseId),

    %% Get all active tokens
    Tokens = wf_state:get_tokens(State),
    ActiveTokens = [T || {_, T} <- maps:to_list(Tokens),
                         T#token.status =:= active],

    %% Mark all tokens as cancelled
    CancelledTokens = [T#token{status = cancelled} || T <- ActiveTokens],

    %% Cancel all effects (stub)
    CancelledEffects = cancel_effects_for_tokens(ActiveTokens),

    %% Update case status (add set_case_status mutation)
    {ok, NewState} = wf_state:buffer_mutation(
        State,
        {set_case_status, cancelled}
    ),
    {ok, _State2, _Receipt} = wf_state:commit(NewState),

    %% Create event
    Event = #cancel_case{
        scope_id = CaseId,
        cancelled_tokens = [T#token.token_id || T <- CancelledTokens],
        cancelled_effects = CancelledEffects,
        timestamp = erlang:timestamp()
    },
    {ok, Event}.
```

**2. Add mutation type** (update wf_state.erl):
```erlang
%% In wf_state.erl mutation() type:
-type mutation() ::
    ...
    | {set_case_status, cancelled | done | failed}.

%% Apply mutation:
apply_single_mutation(#mutation{type = {set_case_status, Status}}, State) ->
    %% Add status field to state record or metadata
    State#state{status = Status}.
```

**3. Test case cancellation**:
```erlang
cancel_case_test_() ->
    %% Create state with multiple tokens
    State = create_test_state_with_multiple_tokens(),

    %% Cancel case
    {ok, Event} = wf_cancel:cancel_case(State),

    %% Verify all tokens cancelled
    ?assertEqual(3, length(Event#cancel_case.cancelled_tokens)).

cancel_case_terminal_state_test_() ->
    %% Cancel case
    {ok, Event} = wf_cancel:cancel_case(State),

    %% Verify case status is cancelled (terminal)
    ?assertEqual(cancelled, State#state.status).
```

### Phase 5: Integration with wf_exec

**1. Replace stub in wf_exec**:
```erlang
%% In wf_exec.erl
is_scope_cancelled(ScopeId, ExecState) ->
    %% Call wf_cancel
    wf_cancel:is_cancelled(ExecState#exec_state.state, ScopeId).

propagate_cancellation(ScopeId, TokensMap) ->
    %% Call wf_cancel
    wf_cancel:propagate(ScopeId, TokensMap).
```

**2. Add `propagate/2` to wf_cancel**:
```erlang
%% Propagate cancellation to tokens in scope
-spec propagate(scope_id(), #{token_id() => #token{}}) ->
    #{token_id() => #token{}}.
propagate(ScopeId, TokensMap) ->
    %% Mark tokens in scope as cancelled
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).
```

**3. Add `is_cancelled/2`**:
```erlang
%% Check if scope is cancelled
-spec is_cancelled(wf_state:state(), scope_id()) -> boolean().
is_cancelled(State, ScopeId) ->
    case wf_state:get_scope(State, ScopeId) of
        #scope{status = cancelled} -> true;
        _ -> false
    end.
```

**4. Test integration**:
```erlang
wf_exec_cancel_integration_test_() ->
    %% Create executor with cancel scope
    ExecState0 = create_executor_with_cancel_scope(),

    %% Cancel scope via wf_cancel
    {ok, Event} = wf_cancel:cancel_region(ExecState0, scope1, []),

    %% Step executor (triggers cancellation)
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),

    %% Verify executor detects cancellation
    ?assertEqual(cancelled, wf_exec:get_status(ExecState1)).
```

### Phase 6: Invariant Checks

**1. Implement invariant verification**:
```erlang
%% Verify scope isolation (unrelated scopes unaffected)
verify_scope_isolation(State, CancelledScopeId) ->
    Tokens = wf_state:get_tokens(State),
    ActiveTokensOutsideScope = [
        T || {_, T} <- maps:to_list(Tokens),
        T#token.scope_id =/= CancelledScopeId,
        T#token.status =:= active
    ],
    case ActiveTokensOutsideScope of
        [] -> ok;
        _ -> {error, {tokens_corrupted, ActiveTokensOutsideScope}}
    end.

%% Verify scope nesting (child scopes cancelled)
verify_scope_nesting(State, ParentScopeId) ->
    Scopes = wf_state:get_scopes(State),
    ChildScopes = [S || {_, S} <- maps:to_list(Scopes),
                        S#scope.parent_scope =:= ParentScopeId],
    CancelledChildren = [S || S <- ChildScopes,
                            S#scope.status =:= cancelled],
    case length(CancelledChildren) =:= length(ChildScopes) of
        true -> ok;
        false -> {error, {child_scopes_not_cancelled, ChildScopes}}
    end.

%% Verify no orphaned tokens
verify_no_orphaned_tokens(State, CancelledScopeId) ->
    Tokens = wf_state:get_tokens(State),
    CancelledTokens = [T || {_, T} <- maps:to_list(Tokens),
                            T#token.status =:= cancelled],
    OrphanedTokens = [T || T <- CancelledTokens,
                          T#token.scope_id =/= CancelledScopeId],
    case OrphanedTokens of
        [] -> ok;
        _ -> {error, {orphaned_cancelled_tokens, OrphanedTokens}}
    end.
```

**2. Run invariants in tests**:
```erlang
cancel_region_invariants_test_() ->
    State = create_test_state_with_nested_scopes(),

    %% Cancel parent scope
    {ok, _Event} = wf_cancel:cancel_region(State, parent_scope, []),

    %% Verify invariants
    ?assertEqual(ok, wf_cancel:verify_scope_isolation(State, parent_scope)),
    ?assertEqual(ok, wf_cancel:verify_scope_nesting(State, parent_scope)),
    ?assertEqual(ok, wf_cancel:verify_no_orphaned_tokens(State, parent_scope)).
```

### Phase 7: Performance Tests

**1. Benchmark cancellation**:
```erlang
%% Measure: O(scope_size) cancellation
bench_cancel_region_small() ->
    State = create_state_with_n_tokens_in_scope(10),
    {Time, _} = timer:tc(fun() ->
        wf_cancel:cancel_region(State, scope1, [])
    end),
    io:format("Cancel 10 tokens: ~p microsec~n", [Time]).

bench_cancel_region_large() ->
    State = create_state_with_n_tokens_in_scope(1000),
    {Time, _} = timer:tc(fun() ->
        wf_cancel:cancel_region(State, scope1, [])
    end),
    io:format("Cancel 1000 tokens: ~p microsec~n", [Time]).

%% Verify: O(scope_size), not O(total_tokens)
bench_cancel_region_isolation() ->
    %% Create state with 1000 tokens total, 10 in scope
    State = create_state_with_1000_tokens_10_in_scope(),
    {Time, _} = timer:tc(fun() ->
        wf_cancel:cancel_region(State, scope1, [])
    end),
    %% Should be fast (only 10 tokens cancelled)
    ?assert(Time < 1000).  %% < 1ms
```

### Phase 8: Comprehensive Testing

**1. Unit tests**:
- Test cancel_activity/3 (with effect, without effect)
- Test cancel_case/1 (all tokens cancelled)
- Test cancel_region/3 (tokens in scope only)
- Test is_cancelled/2 (scope status check)
- Test propagate/2 (token cancellation)

**2. Invariant tests**:
- Test scope isolation preserved
- Test scope nesting enforced
- Test no orphaned tokens
- Test state consistency after cancellation

**3. Integration tests**:
- Test wf_exec detects cancelled scopes
- Test wf_exec propagates cancellation
- Test case termination on cancellation

**4. Performance tests**:
- Benchmark small scope cancellation
- Benchmark large scope cancellation
- Verify O(scope_size) complexity

**5. Property-based tests**:
```erlang
%% Property: cancellation preserves invariants
prop_cancel_preserves_invariants() ->
    ?FORALL({State, ScopeId}, {state_gen(), scope_id_gen()},
        begin
            {ok, _Event} = wf_cancel:cancel_region(State, ScopeId, []),
            %% Verify invariants
            wf_cancel:verify_scope_isolation(State, ScopeId) =:= ok andalso
            wf_cancel:verify_no_orphaned_tokens(State, ScopeId) =:= ok
        end).

%% Property: case cancellation cancels all tokens
prop_case_cancellation_complete() ->
    ?FORALL(State, state_gen(),
        begin
            {ok, Event} = wf_cancel:cancel_case(State),
            Tokens = wf_state:get_tokens(State),
            ActiveTokens = [T || {_, T} <- maps:to_list(Tokens),
                             T#token.status =:= active],
            length(ActiveTokens) =:= 0
        end).
```

## Open Questions

1. **wf_state Integration**: Should wf_cancel use wf_state directly or accept state as parameter?
   - **Recommendation**: Accept wf_state:state() as parameter for flexibility. Caller manages state lifecycle. Enables testing without full wf_state infrastructure.

2. **Token Lookup by Task ID**: How to find token executing a specific task?
   - **Option 1**: Scan all tokens (O(n)), check token.value or IP
   - **Option 2**: Add task_id → token_id index in state
   - **Recommendation**: Option 1 for v1 (simple). Add index in v2 if performance issue.

3. **Effect Tracking**: How to track which effect a token is waiting for?
   - **Option 1**: Store effect_id in token record
   - **Option 2**: Separate token_id → effect_id map in state
   - **Recommendation**: Option 2 (separate map). Keeps token record small. Enables O(1) effect lookup. Add to wf_state in item 010 integration.

4. **Scope Token List**: Should scope record track tokens in scope (token list)?
   - **Pro**: Enables O(scope_tokens) cancellation without scanning all tokens
   - **Con**: Requires maintaining token list on token creation/movement
   - **Recommendation**: Add token list to scope record. Update list when tokens enter/exit scope (in wf_exec CANCEL_SCOPE handlers). Enables efficient cancellation.

5. **Nested Scope Cancellation**: Should cancelling parent scope recursively cancel child scopes?
   - **Option 1**: Only cancel parent scope (child scopes remain active)
   - **Option 2**: Recursively cancel all child scopes
   - **Recommendation**: Option 2 (recursive). Child scopes cannot execute if parent cancelled. Recursive cancellation cleans up state properly. Add depth limit to prevent infinite loops.

6. **Event Logging**: Where should cancel events be logged?
   - **Option 1**: Return event to caller (caller logs)
   - **Option 2**: Send to wf_trace sink (item 011)
   - **Recommendation**: Option 1 for v1 (return event). Add Option 2 in v2 when tracing implemented. Keep wf_cancel independent of tracing system.

7. **Error Handling**: Should cancellation fail if scope not found or already cancelled?
   - **Option 1**: Return error ({error, scope_not_found})
   - **Option 2**: Idempotent (return ok if already cancelled)
   - **Recommendation**: Option 1 (fail loudly). Cancellation is explicit operation. Caller should check scope exists before cancelling. Helps catch bugs early.

8. **wf_effect Integration**: How to handle wf_effect not implemented yet?
   - **Option 1**: Stub wf_effect calls (always return ok)
   - **Option 2**: Make effect cancellation optional
   - **Recommendation**: Option 1 (stub). Add TODO comments. When item 010 implemented, replace stubs with real calls. Keep API compatible.

9. **Case Status Field**: Where to store case status (cancelled/done/failed)?
   - **Option 1**: Add status field to #state{} record
   - **Option 2**: Add status field to #metadata{} record
   - **Recommendation**: Option 1 (status in state record). Case status is top-level state, not metadata. Simplifies status checks.

10. **Cancellation Idempotency**: Should cancel_region/3 be idempotent (ok if already cancelled)?
    - **Recommendation**: No (return error if already cancelled). Cancellation is one-way operation. Caller should check status before cancelling. Prevents silent failures.

11. **Scope Exit Cancellation**: When scope exits, should executor check if cancelled?
    - **Current**: executor checks on scope exit (wf_exec.erl:492-502)
    - **Question**: Should cancellation be checked on every step?
    - **Recommendation**: Current approach is fine (check on exit). Cancellation detected when scope exits. Add check on token creation (skip token if scope cancelled).

12. **Concurrent Cancellation**: What if two processes cancel same scope simultaneously?
    - **Risk**: Race condition on scope status update
    - **Mitigation**: wf_state commit protocol serializes mutations. First commit wins, second gets {error, already_cancelled}.
    - **Recommendation**: Document serialization semantics. Tests for concurrent cancellation (if supported).

13. **Effect Cancellation Timeout**: How long to wait for effect to cancel?
    - **Recommendation**: No timeout for v1 (assume immediate). Add timeout option in v2 if effects are slow to cancel. Document as known limitation.

14. **Cancellation Event Format**: Should events be records or tuples?
    - **Option 1**: Records (#cancel_activity{}, etc.)
    - **Option 2**: Tuples ({cancel_activity, Timestamp, ...})
    - **Recommendation**: Records (current design). Type-safe, documented fields. Convert to tuples for tracing (item 011).

15. **Scope ID Generation**: Who generates scope IDs?
    - **Current**: wf_exec generates scope IDs (make_ref()) in CANCEL_SCOPE enter
    - **Question**: Should wf_cancel generate IDs?
    - **Recommendation**: Current approach is fine. Executor generates IDs when entering scopes. wf_cancel uses provided IDs.

16. **Performance Baseline**: What is acceptable cancellation latency?
    - **Target**: < 1ms for small scope (< 10 tokens)
    - **Target**: < 100ms for large scope (< 10,000 tokens)
    - **Measurement**: Use timer:tc/1 to benchmark. Optimize if exceeds targets.

17. **Test State Helpers**: How to create test states with scopes/tokens?
    - **Recommendation**: Create helper module `wf_cancel_test_helpers.erl` with functions:
      - `create_state_with_n_tokens(N)` - Create state with N tokens
      - `create_state_with_scope(ScopeId, TokenIds)` - Create state with scope
      - `create_state_with_nested_scopes(ParentId, ChildId)` - Create nested scopes
    - Keeps test code clean and reusable.

18. **Stub Removal**: When to remove wf_exec stub functions?
    - **Current**: wf_exec has is_scope_cancelled/2 and propagate_cancellation/2 stubs
    - **Migration**: Replace with wf_cancel calls in Phase 5
    - **Recommendation**: Keep stubs as deprecated wrappers for backward compatibility. Remove in next major version.

19. **Scope Metadata Query**: How to query all scopes in state?
    - **Current**: wf_state:get_scope/2 gets single scope by ID
    - **Missing**: get all scopes, find child scopes
    - **Recommendation**: Add wf_state:get_scopes/1 to return scopes map. Enables recursive cancellation (walk child scopes).

20. **Cancellation Semantics Documentation**: Where to document cancellation semantics?
    - **Option 1**: Inline comments in wf_cancel.erl
    - **Option 2**: Separate docs/CANCELLATION.md
    - **Recommendation**: Both. Inline comments for API details. Separate doc for architectural decisions, semantics, examples.
