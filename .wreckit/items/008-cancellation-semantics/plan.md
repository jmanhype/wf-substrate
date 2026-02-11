# Implement structured cancellation semantics Implementation Plan

## Implementation Plan Title

Structured Cancellation Semantics for Workflow Execution

## Overview

Implement `wf_cancel.erl` module providing three granularities of cancellation:
1. **Activity cancellation**: Cancel a single task (with or without yielded effect)
2. **Case cancellation**: Cancel an entire workflow instance (all tokens and effects)
3. **Region cancellation**: Cancel a scoped subtree (only tokens within scope ID)

The cancellation system propagates efficiently (O(scope_size) not O(workflow_size)), maintains state consistency via wf_state's atomic commit protocol, and produces structured audit events for each cancellation operation.

## Current State

### Existing Infrastructure

**Executor Stubs** (`src/wf_exec.erl:513-529`):
- `is_scope_cancelled/2` - Returns `false` (stub, marked TODO for wf_cancel)
- `propagate_cancellation/2` - Iterates ALL tokens (O(n), inefficient)
- Lines 492-502: Scope exit checks if cancelled but always gets `false`

**State Store** (`src/wf_state.erl:83-90, 530-549`):
- `#scope{}` record with `status` field (active/cancelled) ✓
- `#scope{tokens}` field tracks tokens in scope ✓ (line 88)
- `{cancel_scope, ScopeId}` mutation type ✓
- `apply_single_mutation` for cancel_scope ✓
- `get_scope/2`, `enter_scope/1`, `exit_scope/1` functions ✓

**Token Tracking** (`src/wf_exec.erl:28-34`):
- `#token{scope_id}` field tracks scope membership ✓
- `#token{status}` field (active/complete/cancelled) ✓

**Executor State** (`src/wf_exec.erl:55-64`):
- `#exec_state{}` has inline state (tokens, scope_stack, status)
- **Missing**: No `wf_state:state()` field to pass to wf_cancel

### Key Constraints

1. **Scope#scope.tokens list exists** (wf_state.erl:88) - enables O(scope_size) cancellation
2. **No wf_state field in exec_state** - wf_cancel must work with wf_state:state() directly
3. **wf_effect not implemented** (item 010) - effect cancellation must be stubbed
4. **No case-level status** - `#state{}` record has no status field, must add `{set_case_status, Status}` mutation

### Missing Components

- No `wf_cancel.erl` module exists
- No cancel event records defined
- No case status tracking in wf_state
- No integration with wf_exec stubs

## Desired End State

### Functional Requirements

1. **wf_cancel module** with exports:
   - `cancel_activity/3` - Cancel single task
   - `cancel_case/1` - Cancel entire case
   - `cancel_region/3` - Cancel scoped region
   - `is_cancelled/2` - Check if scope cancelled
   - `propagate/2` - Propagate cancellation to tokens (for wf_exec)

2. **Cancel event records** with fields:
   - `scope_id` - Cancelled scope identifier
   - `cancelled_tokens` - List of token IDs cancelled
   - `cancelled_effects` - List of effect IDs cancelled
   - `timestamp` - When cancellation occurred

3. **wf_state integration**:
   - Add `{set_case_status, cancelled | done | failed}` mutation type
   - Apply mutation updates `#state{status}` field (new field)
   - `cancel_scope` mutation already exists ✓

4. **wf_exec integration**:
   - Replace `is_scope_cancelled/2` to call `wf_cancel:is_cancelled/2`
   - Replace `propagate_cancellation/2` to call `wf_cancel:propagate/2`
   - **Limitation**: wf_exec has inline state, not wf_state - stub stays for now

### Non-Functional Requirements

1. **Performance**: O(scope_size) cancellation, not O(total_tokens)
2. **Consistency**: Atomic updates via wf_state commit protocol
3. **Invariants**: Unrelated scopes unaffected, no orphaned tokens
4. **Testing**: Unit, integration, invariant, and performance tests

### Verification

```erlang
%% Region cancellation preserves other scopes
State = create_state_with_two_scopes(),
{ok, _Event} = wf_cancel:cancel_region(CaseId, scope1, []),
%% Verify: scope2 tokens still active
[?assert(active =:= T#token.status) || T <- tokens_in_scope(State, scope2)].

%% Case cancellation cancels all tokens
State = create_state_with_multiple_tokens(),
{ok, Event} = wf_cancel:cancel_case(CaseId),
%% Verify: all tokens cancelled
?assertEqual(0, count_active_tokens(State)).

%% O(scope_size) complexity
State = create_state_with_1000_tokens_10_in_scope(),
{Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(CaseId, scope1, []) end),
?assert(Time < 1000).  %% < 1ms for 10 tokens
```

### Key Discoveries

- **wf_state:scope#tokens list exists** (line 88) - enables efficient O(scope_size) cancellation without scanning all tokens
- **wf_exec has inline state** - cannot pass wf_state to wf_cancel without refactoring; wf_cancel must work with wf_state:state() from ETS
- **Case status missing** - `#state{}` record needs `status` field for terminal cancelled state
- **Effect cancellation stub needed** - wf_effect not implemented (item 010), must stub with TODO comments
- **Scope tracking in executor** - wf_exec tracks scope_stack but doesn't update wf_state scopes (enter_scope/exit_scope called during bytecode execution)

## What We're NOT Doing

1. **Refactoring wf_exec to use wf_state** - Executor keeps inline state (separate refactoring)
2. **Implementing wf_effect** - Effect cancellation stubbed (item 010)
3. **Real-time cancellation signals** - No async cancellation API (future enhancement)
4. **Cancellation undo/rollback** - Cancellation is one-way operation
5. **Event logging/tracing** - Events returned to caller (item 011 will add tracing)
6. **Concurrent cancellation handling** - Single-process model (wf_state serializes)
7. **Scope hierarchy queries** - No "find all child scopes" API (can add if needed)
8. **Cancellation hooks/callbacks** - No user-defined cancellation handlers
9. **Partial cancellation retry** - No automatic retry on failure
10. **Performance optimization beyond O(scope_size)** - No token indexing, caching, etc.

## Implementation Approach

**High-Level Strategy**: Implement wf_cancel as standalone module that operates on wf_state:state(). Use wf_state's commit protocol for atomic updates. Stub wf_effect integration. Replace wf_exec stubs to delegate to wf_cancel (where possible with inline state constraint).

**Architectural Decisions**:

1. **State Management**: wf_cancel accepts `wf_state:state()` as parameter (not case_id). Caller manages state lifecycle (restore from ETS, commit changes). Enables testing without full infrastructure.

2. **Case Status**: Add `status` field to `#state{}` record (cancelled/done/failed). Use `{set_case_status, Status}` mutation to update atomically.

3. **Effect Cancellation**: Stub `cancel_effects_for_tokens/1` to return `[]` (no effects). Add TODO comments for item 010 integration.

4. **Token Lookup**: For activity cancellation, scan tokens linearly to find task (O(n)). Acceptable for v1. Add task_id→token_id index in v2 if needed.

5. **Scope Token List**: Use `#scope.tokens` field (already exists) for O(scope_size) cancellation. No need to scan all tokens.

6. **wf_exec Integration**: Replace stubs to call wf_cancel BUT keep inline state constraint. wf_cancel uses wf_state:state(), not exec_state. Document limitation.

7. **Event Format**: Use records (`#cancel_activity{}`, etc.) for type safety. Convert to tuples for tracing (item 011).

8. **Error Handling**: Fail loudly (return errors) for scope_not_found, already_cancelled. Cancellation is explicit operation. No silent failures.

9. **Invariant Checks**: Run after each cancellation in tests. Export `verify_scope_isolation/2`, `verify_scope_nesting/2`, `verify_no_orphaned_tokens/2` for testing.

**Phasing Strategy**:

1. **Phase 1**: Add case status field to wf_state (foundational)
2. **Phase 2**: Create wf_cancel with types and helper functions (skeleton)
3. **Phase 3**: Implement region cancellation (core, uses scope#tokens)
4. **Phase 4**: Implement activity cancellation (depends on token lookup)
5. **Phase 5**: Implement case cancellation (depends on case status)
6. **Phase 6**: Invariant checks and testing
7. **Phase 7**: wf_exec integration (replace stubs)
8. **Phase 8**: Performance benchmarks

**Risk Mitigation**:

- **O(n) token scan for activity cancellation**: Acceptable for v1 (low task count). Add index in v2.
- **Effect cancellation stubs**: Document clearly with TODO. No integration tests for effects (defer to item 010).
- **wf_exec inline state**: Document limitation. wf_cancel works with wf_state:state(). Future refactoring to integrate properly.
- **Case status mutation**: Add proper validation. Test state transitions.

---

## Phases

### Phase 1: Add Case Status Tracking to wf_state

#### Overview

Add `status` field to `#state{}` record and `{set_case_status, Status}` mutation type. Enables case cancellation to mark workflow as cancelled (terminal state).

#### Changes Required:

##### 1. wf_state.erl - Add status field to state record

**File**: `src/wf_state.erl`
**Changes**: Add `status` field to `#state{}` record

```erlang
%% State record: Per-case execution state
-record(state, {
    case_id :: case_id(),                    %% Unique identifier
    ctx :: ctx(),                            %% User context (opaque map)
    tokens :: #{token_id() => #token{}},     %% Active tokens
    scopes :: #{scope_id() => #scope{}},     %% Cancel scopes
    metadata :: #metadata{},                  %% Execution metadata
    status :: running | cancelled | done | failed,  %% Case status (NEW)
    buffered_mutations :: [#mutation{}],      %% Buffered mutations
    ets_table :: ets:tid() | undefined       %% ETS table reference
}).
```

##### 2. wf_state.erl - Add set_case_status mutation type

**File**: `src/wf_state.erl` (line ~141)
**Changes**: Add `{set_case_status, Status}` to mutation type union

```erlang
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
    {set_case_status, running | cancelled | done | failed} | |  %% NEW
    {increment_step_count} |
    {set_metadata, term()}.
```

##### 3. wf_state.erl - Add mutation validation

**File**: `src/wf_state.erl` (in validate_single_mutation, ~line 485)
**Changes**: Add validation clause for set_case_status

```erlang
validate_single_mutation(#mutation{type = {set_case_status, Status}}, _State)
  when Status =:= running; Status =:= cancelled; Status =:= done; Status =:= failed ->
    ok;
```

##### 4. wf_state.erl - Add mutation application

**File**: `src/wf_state.erl` (in apply_single_mutation, ~line 545)
**Changes**: Add application clause for set_case_status

```erlang
apply_single_mutation(#mutation{type = {set_case_status, Status}}, State) ->
    State#state{status = Status};
```

##### 5. wf_state.erl - Update new/1 to initialize status

**File**: `src/wf_state.erl` (line ~231)
**Changes**: Initialize status field to running in new/1

```erlang
new(CaseId) ->
    EtsTable = whereis(wf_state_store),
    #state{
        case_id = CaseId,
        ctx = #{},
        tokens = #{},
        scopes = #{},
        metadata = #metadata{
            step_count = 0,
            start_time = erlang:timestamp(),
            last_commit_time = undefined
        },
        status = running,  %% NEW
        buffered_mutations = [],
        ets_table = EtsTable
    }.
```

##### 6. wf_state.erl - Add get_status/1 accessor

**File**: `src/wf_state.erl` (in API section, ~line 250)
**Changes**: Export and implement get_status/1

```erlang
%% @doc Get case status
-spec get_status(state()) -> running | cancelled | done | failed.
get_status(#state{status = Status}) ->
    Status.
```

##### 7. wf_state.erl - Update serialize/deserialize

**File**: `src/wf_state.erl` (lines ~368-395)
**Changes**: Ensure status field included in snapshots

```erlang
%% In serialize_state:
StateMap = #{
    case_id => State#state.case_id,
    ctx => State#state.ctx,
    tokens => tokens_to_map(State#state.tokens),
    scopes => scopes_to_map(State#state.scopes),
    metadata => metadata_to_map(State#state.metadata),
    status => State#state.status,  %% NEW
    snapshot_id => SnapshotId,
    snapshot_time => SnapshotTime
};

%% In deserialize_state:
#state{
    case_id = maps:get(case_id, StateMap),
    ctx = maps:get(ctx, StateMap),
    tokens = map_to_tokens(maps:get(tokens, StateMap)),
    scopes = map_to_scopes(maps:get(scopes, StateMap)),
    metadata = map_to_metadata(maps:get(metadata, StateMap)),
    status = maps:get(status, StateMap, running),  %% NEW (default running)
    buffered_mutations = [],
    ets_table = whereis(wf_state_store)
}
```

##### 8. test/wf_state_tests.erl - Add status tests

**File**: `test/wf_state_tests.erl`
**Changes**: Add tests for status field and set_case_status mutation

```erlang
%% Test case status tracking
case_status_test_() ->
    State0 = wf_state:new(test_case),
    ?assertEqual(running, wf_state:get_status(State0)),

    {ok, State1} = wf_state:buffer_mutation(State0, {set_case_status, cancelled}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    ?assertEqual(cancelled, wf_state:get_status(State2)).

%% Test invalid status rejected
invalid_status_test_() ->
    State0 = wf_state:new(test_case),
    {ok, State1} = wf_state:buffer_mutation(State0, {set_case_status, invalid_status}),
    ?assertEqual({error, {validation_failed, _}}, wf_state:commit(State1)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests pass: `rebar3 ct --module wf_state_tests`
- [ ] Compilation succeeds: `rebar3 compile`
- [ ] No type errors: Check dialyzer output

##### Manual Verification:

- [ ] Status field initialized to `running` in new state
- [ ] Status updated atomically via commit
- [ ] Invalid status values rejected by validation
- [ ] Status persisted in ETS snapshots
- [ ] get_status/1 returns correct value

**Note**: This phase is foundational - case status is required for case cancellation. Verify thoroughly before proceeding.

---

### Phase 2: Create wf_cancel Module Structure

#### Overview

Create `src/wf_cancel.erl` with type definitions, records, and helper function stubs. Establish the module skeleton before implementing cancellation logic.

#### Changes Required:

##### 1. src/wf_cancel.erl - Create module with types and records

**File**: `src/wf_cancel.erl` (new file)
**Changes**: Create complete module skeleton

```erlang
%%====================================================================
%% wf_cancel - Structured cancellation semantics for workflow execution
%%====================================================================

-module(wf_cancel).
-behaviour(gen_server).

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
    {case, case_id()} |
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

%% Include wf_state and wf_exec records
-include("wf_state.hrl").
-include("wf_exec.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Cancel a single task (activity)
%% If task has yielded an effect, effect is cancelled.
%% If task not yet started, token is marked cancelled (skipped).
-spec cancel_activity(case_id(), task_id(), proplists:proplist()) ->
    {ok, #cancel_activity{}} | {error, term()}.
cancel_activity(_CaseId, _TaskId, _Options) ->
    {error, not_implemented}.

%% @doc Cancel an entire workflow instance
%% All active tokens and pending effects are cancelled.
%% Case transitions to terminal cancelled state.
-spec cancel_case(case_id()) -> {ok, #cancel_case{}} | {error, term()}.
cancel_case(_CaseId) ->
    {error, not_implemented}.

%% @doc Cancel a scoped region
%% Only tokens within the named scope are cancelled.
%% Tokens outside the scope are unaffected.
-spec cancel_region(case_id(), scope_id(), proplists:proplist()) ->
    {ok, #cancel_region{}} | {error, term()}.
cancel_region(_CaseId, _ScopeId, _Options) ->
    {error, not_implemented}.

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
verify_scope_isolation(_State, _CancelledScopeId) ->
    ok.

%% @doc Verify scope nesting: child scopes cancelled when parent cancelled
-spec verify_scope_nesting(wf_state:state(), scope_id()) -> ok | {error, term()}.
verify_scope_nesting(_State, _ParentScopeId) ->
    ok.

%% @doc Verify no orphaned tokens: all cancelled tokens in cancelled scope
-spec verify_no_orphaned_tokens(wf_state:state(), scope_id()) -> ok | {error, term()}.
verify_no_orphaned_tokens(_State, _CancelledScopeId) ->
    ok.
```

##### 2. test/wf_cancel_tests.erl - Create test file skeleton

**File**: `test/wf_cancel_tests.erl` (new file)
**Changes**: Create test skeleton with basic imports

```erlang
%%====================================================================
%% wf_cancel_tests - Unit tests for wf_cancel
%%====================================================================

-module(wf_cancel_tests).
-include_lib("eunit/include/eunit.hrl").

-include("wf_state.hrl").
-include("wf_exec.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

%% Create a test state with tokens and scopes
create_test_state() ->
    State = wf_state:new(test_case),
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
    {ok, State1} = wf_state:add_token(State, TokenId1, Token1),
    {ok, State2} = wf_state:add_token(State1, TokenId2, Token2),
    {ok, State3} = wf_state:enter_scope(State2, scope1),
    {ok, State4} = wf_state:enter_scope(State3, scope2),
    {ok, _State5, _Receipt} = wf_state:commit(State4),
    State4.

%%====================================================================
%% Phase 2 Tests
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
    State = create_test_state(),
    {ok, State1} = wf_state:buffer_mutation(State, {cancel_scope, scope1}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    ?_assert(wf_cancel:is_cancelled(State2, scope1)).

%% Test is_cancelled/2 with undefined scope
is_cancelled_undefined_scope_test_() ->
    State = create_test_state(),
    ?_assertNot(wf_cancel:is_cancelled(State, nonexistent_scope)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Tests pass: `rebar3 ct --module wf_cancel_tests`
- [ ] No dialyzer warnings: `rebar3 dialyzer`
- [ ] Module loads in shell: `l(wf_cancel).`

##### Manual Verification:

- [ ] All types exported correctly
- [ ] Helper functions (is_cancelled, propagate) work correctly
- [ ] Module follows OTP design patterns
- [ ] Headers included properly (wf_state.hrl, wf_exec.hrl)

**Note**: This phase establishes the foundation. Verify module structure before adding logic.

---

### Phase 3: Implement Region Cancellation

#### Overview

Implement `cancel_region/3` to cancel all tokens within a specific scope ID. Uses `#scope.tokens` list for O(scope_size) efficiency. Core cancellation logic.

#### Changes Required:

##### 1. src/wf_cancel.erl - Implement cancel_region/3

**File**: `src/wf_cancel.erl`
**Changes**: Replace stub with full implementation

```erlang
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
do_cancel_region(State, ScopeId) ->
    %% Check scope exists and not already cancelled
    case wf_state:get_scope(State, ScopeId) of
        undefined ->
            {error, {scope_not_found, ScopeId}};
        #scope{status = cancelled} ->
            {error, {already_cancelled, ScopeId}};
        #scope{tokens = TokenIds} = Scope ->
            %% Get tokens in scope (O(scope_size) using scope#tokens list)
            TokensMap = wf_state:get_tokens(State),
            TokensInScope = get_tokens_by_ids(TokensMap, TokenIds),

            %% Mark tokens as cancelled
            CancelledTokenIds = mark_tokens_cancelled(State, TokensInScope),

            %% Cancel effects yielded by tokens (stub for wf_effect)
            CancelledEffectIds = cancel_effects_for_tokens(TokensInScope),

            %% Update scope status to cancelled
            {ok, State1} = wf_state:buffer_mutation(State, {cancel_scope, ScopeId}),
            {ok, State2, _Receipt} = wf_state:commit(State1),

            %% Create cancel event
            Event = #cancel_region{
                scope_id = ScopeId,
                cancelled_tokens = CancelledTokenIds,
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
        {ok, NewState} = wf_state:buffer_mutation(
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
```

##### 2. src/wf_cancel.erl - Implement invariant checks

**File**: `src/wf_cancel.erl`
**Changes**: Implement verification functions

```erlang
%% @doc Verify scope isolation: unrelated scopes unaffected
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

%% @doc Verify no orphaned tokens: all cancelled tokens in cancelled scope
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

%% @doc Verify scope nesting: child scopes cancelled when parent cancelled
verify_scope_nesting(State, ParentScopeId) ->
    Scopes = wf_state:get_scopes(State),
    ChildScopes = [
        S || {_, S} <- maps:to_list(Scopes),
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
```

##### 3. test/wf_cancel_tests.erl - Add region cancellation tests

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add comprehensive tests

```erlang
%% Test region cancellation
cancel_region_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         ScopeId = scope1,
         {ok, Event} = wf_cancel:cancel_region(State, ScopeId, []),
         [
             ?_assertEqual(ScopeId, Event#cancel_region.scope_id),
             ?_assertEqual(1, length(Event#cancel_region.cancelled_tokens)),
             ?_assertEqual(0, length(Event#cancel_region.cancelled_effects))
         ]
     end}.

%% Test region cancellation error on undefined scope
cancel_region_undefined_scope_test_() ->
    State = create_test_state(),
    ?_assertEqual(
        {error, {scope_not_found, nonexistent_scope}},
        wf_cancel:cancel_region(State, nonexistent_scope, [])
    ).

%% Test region cancellation error on already cancelled
cancel_region_already_cancelled_test_() ->
    State = create_test_state(),
    {ok, State1} = wf_state:buffer_mutation(State, {cancel_scope, scope1}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    ?_assertEqual(
        {error, {already_cancelled, scope1}},
        wf_cancel:cancel_region(State2, scope1, [])
    ).

%% Test region cancellation preserves other scopes (invariant)
cancel_region_preserves_other_scopes_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         %% Cancel scope1
         {ok, _Event} = wf_cancel:cancel_region(State, scope1, []),

         %% Verify scope2 tokens still active (invariant)
         Tokens = wf_state:get_tokens(State),
         TokensInScope2 = [T || {_, T} <- maps:to_list(Tokens),
                               T#token.scope_id =:= scope2],
         ?_assertEqual([active], [T#token.status || T <- TokensInScope2])
     end}.

%% Test O(scope_size) complexity
cancel_region_complexity_test_() ->
    %% Create state with 100 tokens total, 5 in scope
    State = create_state_with_n_tokens_in_scope(100, 5),
    {Time, _} = timer:tc(fun() ->
        wf_cancel:cancel_region(State, scope1, [])
    end),
    %% Should be fast (< 1ms) for 5 tokens
    ?_assert(Time < 1000).

%% Helper: create state with N tokens, M in scope1
create_state_with_n_tokens_in_scope(TotalTokens, TokensInScope) ->
    State = wf_state:new(test_case),
    {ok, State1} = wf_state:enter_scope(State, scope1),

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
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests pass: `rebar3 ct --module wf_cancel_tests`
- [ ] Invariant tests pass: scope isolation, no orphaned tokens
- [ ] Performance test passes: O(scope_size) verified
- [ ] Error cases handled correctly

##### Manual Verification:

- [ ] Region cancellation only affects tokens in scope
- [ ] Scope status updated to cancelled
- [ ] Event structure correct (scope_id, cancelled_tokens, timestamp)
- [ ] Unrelated scopes remain active
- [ ] Error messages descriptive

**Note**: This is the core cancellation logic. Test thoroughly - all other cancellations build on this.

---

### Phase 4: Implement Activity Cancellation

#### Overview

Implement `cancel_activity/3` to cancel a single task. Finds token executing task by scanning tokens. If task yielded effect, cancel effect (stub). If not started, mark token cancelled.

#### Changes Required:

##### 1. src/wf_cancel.erl - Implement cancel_activity/3

**File**: `src/wf_cancel.erl`
**Changes**: Replace stub with full implementation

```erlang
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
%% For v1, scan all tokens and check value field
%% TODO: Add task_id → token_id index for O(1) lookup in v2
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
    {ok, State1} = wf_state:buffer_mutation(
        State,
        {update_token, TokenId, fun(_) -> CancelledToken end}
    ),
    {ok, State2, _Receipt} = wf_state:commit(State1),

    %% Create event
    Event = #cancel_activity{
        scope_id = Token#token.value,  %% Task ID
        cancelled_tokens = [TokenId],
        cancelled_effects = CancelledEffects,
        timestamp = erlang:timestamp()
    },
    {ok, Event}.
```

##### 2. test/wf_cancel_tests.erl - Add activity cancellation tests

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add comprehensive tests

```erlang
%% Test activity cancellation (task without effect)
cancel_activity_without_effect_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         TaskId = task1,
         {ok, Event} = wf_cancel:cancel_activity(State, TaskId, []),
         [
             ?_assertEqual(TaskId, Event#cancel_activity.scope_id),
             ?_assertEqual(1, length(Event#cancel_activity.cancelled_tokens)),
             ?_assertEqual(0, length(Event#cancel_activity.cancelled_effects))
         ]
     end}.

%% Test activity cancellation error on undefined task
cancel_activity_undefined_task_test_() ->
    State = create_test_state(),
    ?_assertEqual(
        {error, {task_not_found, nonexistent_task}},
        wf_cancel:cancel_activity(State, nonexistent_task, [])
    ).

%% Test activity cancellation preserves other tasks
cancel_activity_preserves_other_tasks_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         %% Cancel task1
         {ok, _Event} = wf_cancel:cancel_activity(State, task1, []),

         %% Verify task2 still active
         Tokens = wf_state:get_tokens(State),
         Task2Token = [T || {_, T} <- maps:to_list(Tokens),
                        T#token.value =:= task2],
         ?_assertEqual([active], [T#token.status || T <- Task2Token])
     end}.

%% Test activity cancellation of already cancelled task
cancel_activity_already_cancelled_test_() ->
    State = create_test_state(),
    %% Cancel task1 first time
    {ok, _Event1} = wf_cancel:cancel_activity(State, task1, []),
    %% Try to cancel again
    ?_assertEqual(
        {error, {already_cancelled, task1}},
        wf_cancel:cancel_activity(State, task1, [])
    ).

%% Test activity cancellation invariants
cancel_activity_invariants_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         {ok, _Event} = wf_cancel:cancel_activity(State, task1, []),

         %% Verify invariants
         ?_assertEqual(ok, wf_cancel:verify_no_orphaned_tokens(State, scope1))
     end}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests pass: `rebar3 ct --module wf_cancel_tests`
- [ ] Task lookup works correctly
- [ ] Token status updated to cancelled
- [ ] Event structure correct
- [ ] Invariant checks pass

##### Manual Verification:

- [ ] Activity cancellation only affects target task
- [ ] Other tasks remain active
- [ ] Error cases handled (task_not_found, already_cancelled)
- [ ] Effect cancellation stub called correctly
- [ ] O(n) token scan acceptable (document for v2 optimization)

**Note**: Activity cancellation uses O(n) token scan. Document as known limitation for v2 optimization.

---

### Phase 5: Implement Case Cancellation

#### Overview

Implement `cancel_case/1` to cancel entire workflow instance. Cancels all active tokens, marks case status as cancelled, produces cancel_case event.

#### Changes Required:

##### 1. src/wf_cancel.erl - Implement cancel_case/1

**File**: `src/wf_cancel.erl`
**Changes**: Replace stub with full implementation

```erlang
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
            {CancelledTokens, CancelledTokenIds} = mark_all_tokens_cancelled(State, ActiveTokens),

            %% Cancel all effects (stub for wf_effect)
            CancelledEffectIds = cancel_effects_for_tokens(CancelledTokens),

            %% Mark case status as cancelled
            {ok, State1} = wf_state:buffer_mutation(State, {set_case_status, cancelled}),
            {ok, State2, _Receipt} = wf_state:commit(State1),

            %% Create event
            Event = #cancel_case{
                scope_id = State#state.case_id,
                cancelled_tokens = CancelledTokenIds,
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
        {ok, NewState} = wf_state:buffer_mutation(
            AccState,
            {update_token, TokenId, fun(_) -> CancelledToken end}
        ),
        {NewState, [TokenId | AccIds]}
    end, {State, []}, Tokens).
```

##### 2. test/wf_cancel_tests.erl - Add case cancellation tests

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add comprehensive tests

```erlang
%% Test case cancellation
cancel_case_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         {ok, Event} = wf_cancel:cancel_case(State),
         [
             ?_assertEqual(State#state.case_id, Event#cancel_case.scope_id),
             ?_assertEqual(2, length(Event#cancel_case.cancelled_tokens)),
             ?_assertEqual(0, length(Event#cancel_case.cancelled_effects))
         ]
     end}.

%% Test case cancellation marks status
cancel_case_status_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         {ok, _Event} = wf_cancel:cancel_case(State),
         ?_assertEqual(cancelled, wf_state:get_status(State))
     end}.

%% Test case cancellation error on already cancelled
cancel_case_already_cancelled_test_() ->
    State = create_test_state(),
    %% Cancel case first time
    {ok, _Event1} = wf_cancel:cancel_case(State),
    %% Try to cancel again
    ?_assertEqual(
        {error, {already_cancelled, State#state.case_id}},
        wf_cancel:cancel_case(State)
    ).

%% Test case cancellation cancels all tokens
cancel_case_all_tokens_test_() ->
    {setup,
     fun() -> create_test_state() end,
     fun(State) ->
         {ok, Event} = wf_cancel:cancel_case(State),

         %% Verify all tokens cancelled
         Tokens = wf_state:get_tokens(State),
         ActiveTokens = [T || {_, T} <- maps:to_list(Tokens),
                          T#token.status =:= active],
         ?_assertEqual(0, length(ActiveTokens)),
         ?_assertEqual(2, length(Event#cancel_case.cancelled_tokens))
     end}.

%% Test case cancellation with many tokens
cancel_case_many_tokens_test_() ->
    State = create_state_with_n_tokens(100),
    {ok, Event} = wf_cancel:cancel_case(State),

    %% Verify all tokens cancelled
    ?_assertEqual(100, length(Event#cancel_case.cancelled_tokens)).

%% Helper: create state with N tokens
create_state_with_n_tokens(N) ->
    State = wf_state:new(test_case),
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
    end, {State, 0}, lists:seq(1, N)),

    {ok, State2, _Receipt} = wf_state:commit(FinalState),
    State2.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests pass: `rebar3 ct --module wf_cancel_tests`
- [ ] All tokens cancelled
- [ ] Case status updated to cancelled
- [ ] Event structure correct
- [ ] Error cases handled

##### Manual Verification:

- [ ] Case cancellation marks all tokens cancelled
- [ ] Case status is terminal (cancelled)
- [ ] No active tokens remain
- [ ] Error on double cancellation
- [ ] Performance acceptable for large token counts

**Note**: Case cancellation is terminal. Verify executor respects cancelled status and stops execution.

---

### Phase 6: wf_exec Integration

#### Overview

Replace wf_exec stub functions (`is_scope_cancelled/2` and `propagate_cancellation/2`) to delegate to wf_cancel. Update executor to use wf_cancel for cancellation detection and propagation.

#### Changes Required:

##### 1. src/wf_exec.erl - Replace is_scope_cancelled/2

**File**: `src/wf_exec.erl` (lines 513-517)
**Changes**: Replace stub with wf_cancel call

```erlang
%% @doc Check if scope is cancelled
-spec is_scope_cancelled(term(), exec_state()) -> boolean().
is_scope_cancelled(_ScopeId, _ExecState) ->
    %% TODO: Call wf_cancel:is_cancelled/2 when wf_exec has wf_state field
    %% Current limitation: exec_state has inline state, not wf_state:state()
    %% For now, keep stub behavior (always false)
    false.
```

**Note**: Keep stub for now - wf_exec has inline state, not wf_state. Document limitation.

##### 2. src/wf_exec.erl - Update propagate_cancellation/2

**File**: `src/wf_exec.erl` (lines 519-529)
**Changes**: Update comment to document wf_cancel delegation

```erlang
%% @doc Propagate cancellation to all tokens in scope
-spec propagate_cancellation(term(), #{term() => #token{}}) -> #{term() => #token{}}.
propagate_cancellation(ScopeId, TokensMap) ->
    %% Delegate to wf_cancel for token cancellation
    wf_cancel:propagate(ScopeId, TokensMap).
```

##### 3. src/wf_exec.erl - Update cancel_scope exit handler

**File**: `src/wf_exec.erl` (lines 486-510)
**Changes**: Add comment about wf_cancel integration

```erlang
execute_cancel_scope({_CancelScope, {exit, ScopeId}}, ExecState) ->
    %% Pop scope from stack
    [_Top | Rest] = ExecState#exec_state.scope_stack,
    NewScopeStack = Rest,

    %% Check if scope is cancelled (via wf_cancel)
    %% TODO: When wf_exec has wf_state field, call wf_cancel:is_cancelled(State, ScopeId)
    case is_scope_cancelled(ScopeId, ExecState) of
        true ->
            %% Propagate cancellation to all tokens in scope (via wf_cancel)
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
```

##### 4. src/wf_exec.erl - Add module documentation

**File**: `src/wf_exec.erl` (top of module)
**Changes**: Document cancellation integration

```erlang
%%====================================================================
%% wf_exec - Workflow Executor
%%====================================================================
%%
%% Cancellation Integration:
%% - is_scope_cancelled/2 checks if scope is cancelled (stub for now)
%% - propagate_cancellation/2 delegates to wf_cancel:propagate/2
%% - TODO: Integrate with wf_state:state() when executor refactored
%%
```

##### 5. test/wf_exec_tests.erl - Add cancellation integration tests

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for wf_cancel integration

```erlang
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] Tests pass: `rebar3 ct --module wf_exec_tests`
- [ ] propagate_cancellation/2 delegates to wf_cancel
- [ ] wf_cancel:propagate/2 works correctly with token maps
- [ ] No regressions in existing tests

##### Manual Verification:

- [ ] propagate_cancellation/2 marks tokens in scope as cancelled
- [ ] Tokens outside scope remain active
- [ ] Executor status set to cancelled when scope cancelled
- [ ] Documentation updated with integration notes

**Note**: wf_exec integration is limited by inline state. Full integration requires executor refactoring (future work).

---

### Phase 7: Comprehensive Testing

#### Overview

Add comprehensive unit tests, invariant tests, performance benchmarks, and property-based tests to verify correctness and performance of cancellation system.

#### Changes Required:

##### 1. test/wf_cancel_tests.erl - Add nested scope tests

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add tests for nested scope cancellation

```erlang
%% Test nested scope cancellation
nested_scope_cancellation_test_() ->
    State = create_state_with_nested_scopes(),

    %% Cancel parent scope
    {ok, _Event} = wf_cancel:cancel_region(State, parent_scope, []),

    %% Verify: child scope tokens cancelled too
    Tokens = wf_state:get_tokens(State),
    ParentTokens = [T || {_, T} <- maps:to_list(Tokens),
                       T#token.scope_id =:= parent_scope],
    ChildTokens = [T || {_, T} <- maps:to_list(Tokens),
                      T#token.scope_id =:= child_scope],

    [
        ?_assertEqual([cancelled], [T#token.status || T <- ParentTokens]),
        ?_assertEqual([cancelled], [T#token.status || T <- ChildTokens])
    ].

%% Helper: create state with nested scopes
create_state_with_nested_scopes() ->
    State = wf_state:new(test_case),
    {ok, State1} = wf_state:enter_scope(State, parent_scope),
    {ok, State2} = wf_state:enter_scope(State1, child_scope),

    %% Add tokens
    Token1 = #token{token_id = make_ref(), ip = 0, scope_id = parent_scope, value = task1, status = active},
    Token2 = #token{token_id = make_ref(), ip = 0, scope_id = child_scope, value = task2, status = active},

    {ok, State3} = wf_state:add_token(State2, Token1#token.token_id, Token1),
    {ok, State4} = wf_state:add_token(State3, Token2#token.token_id, Token2),
    {ok, State5, _Receipt} = wf_state:commit(State4),
    State5.
```

##### 2. test/wf_cancel_tests.erl - Add performance benchmarks

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add benchmarks

```erlang
%% Performance benchmark: small scope
bench_small_scope_test_() ->
    State = create_state_with_n_tokens_in_scope(100, 10),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(State, scope1, []) end),
    ?_assert(Time < 1000).  %% < 1ms for 10 tokens

%% Performance benchmark: large scope
bench_large_scope_test_() ->
    State = create_state_with_n_tokens_in_scope(10000, 1000),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(State, scope1, []) end),
    ?_assert(Time < 100000).  %% < 100ms for 1000 tokens

%% Performance benchmark: verify O(scope_size) not O(total_tokens)
bench_complexity_isolation_test_() ->
    %% Create state with 10000 total tokens, 10 in scope
    State = create_state_with_n_tokens_in_scope(10000, 10),
    {Time, _} = timer:tc(fun() -> wf_cancel:cancel_region(State, scope1, []) end),
    %% Should be fast (only 10 tokens cancelled), not O(10000)
    ?_assert(Time < 1000).
```

##### 3. test/wf_cancel_tests.erl - Add invariant verification tests

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add invariant tests

```erlang
%% Test scope isolation invariant
verify_scope_isolation_test_() ->
    State = create_state_with_two_scopes(),
    {ok, _Event} = wf_cancel:cancel_region(State, scope1, []),
    ?_assertEqual(ok, wf_cancel:verify_scope_isolation(State, scope1)).

%% Test no orphaned tokens invariant
verify_no_orphaned_tokens_test_() ->
    State = create_test_state(),
    {ok, _Event} = wf_cancel:cancel_region(State, scope1, []),
    ?_assertEqual(ok, wf_cancel:verify_no_orphaned_tokens(State, scope1)).

%% Test scope nesting invariant
verify_scope_nesting_test_() ->
    State = create_state_with_nested_scopes(),
    {ok, _Event} = wf_cancel:cancel_region(State, parent_scope, []),
    ?_assertEqual(ok, wf_cancel:verify_scope_nesting(State, parent_scope)).
```

##### 4. test/wf_cancel_tests.erl - Add edge case tests

**File**: `test/wf_cancel_tests.erl`
**Changes**: Add edge case tests

```erlang
%% Test cancellation with no tokens
cancel_empty_scope_test_() ->
    State = wf_state:new(test_case),
    {ok, State1} = wf_state:enter_scope(State, empty_scope),
    {ok, State2, _Receipt} = wf_state:commit(State1),

    %% Cancel empty scope (no tokens)
    {ok, Event} = wf_cancel:cancel_region(State2, empty_scope, []),
    ?_assertEqual(0, length(Event#cancel_region.cancelled_tokens)).

%% Test cancellation with single token
cancel_single_token_test_() ->
    State = wf_state:new(test_case),
    {ok, State1} = wf_state:enter_scope(State, scope1),

    Token = #token{token_id = make_ref(), ip = 0, scope_id = scope1, value = task1, status = active},
    {ok, State2} = wf_state:add_token(State1, Token#token.token_id, Token),
    {ok, State3, _Receipt} = wf_state:commit(State2),

    %% Cancel single token
    {ok, Event} = wf_cancel:cancel_region(State3, scope1, []),
    ?_assertEqual(1, length(Event#cancel_region.cancelled_tokens)).

%% Test cancellation preserves token values
cancel_preserves_values_test_() ->
    State = create_test_state(),
    {ok, _Event} = wf_cancel:cancel_region(State, scope1, []),

    %% Verify token values unchanged
    Tokens = wf_state:get_tokens(State),
    Token1 = [T || {_, T} <- maps:to_list(Tokens), T#token.value =:= task1],
    ?_assertEqual([task1], [T#token.value || T <- Token1]).
```

#### Success Criteria:

##### Automated Verification:

- [ ] All tests pass: `rebar3 ct --module wf_cancel_tests`
- [ ] Nested scope tests pass
- [ ] Performance benchmarks pass
- [ ] Invariant tests pass
- [ ] Edge case tests pass
- [ ] No test failures or timeouts

##### Manual Verification:

- [ ] Cancellation works for nested scopes
- [ ] Performance targets met (O(scope_size))
- [ ] Invariants hold after cancellation
- [ ] Edge cases handled correctly
- [ ] Test coverage > 90%

**Note**: Comprehensive testing ensures cancellation system is correct and performant. Run all tests before proceeding.

---

### Phase 8: Documentation and Final Verification

#### Overview

Add module documentation, inline comments, and verify complete implementation. Update ARCHITECTURE.md if needed. Final integration testing.

#### Changes Required:

##### 1. src/wf_cancel.erl - Add module documentation

**File**: `src/wf_cancel.erl` (top of module)
**Changes**: Add comprehensive module documentation

```erlang
%%====================================================================
%% wf_cancel - Structured Cancellation Semantics
%%====================================================================
%%
%% This module implements three granularities of cancellation for workflow
%% execution:
%%
%% 1. Activity Cancellation (cancel_activity/3)
%%    - Cancel a single task
%%    - If task has yielded effect, effect is cancelled
%%    - If task not yet started, token is marked cancelled (skipped)
%%
%% 2. Case Cancellation (cancel_case/1)
%%    - Cancel entire workflow instance
%%    - All active tokens and pending effects cancelled
%%    - Case transitions to terminal cancelled state
%%
%% 3. Region Cancellation (cancel_region/3)
%%    - Cancel scoped subtree identified by scope ID
%%    - Only tokens within scope are cancelled
%%    - Tokens outside scope unaffected (invariant)
%%
%% Cancellation propagation is O(scope_size), not O(workflow_size).
%% Uses wf_state's atomic commit protocol for state consistency.
%% Produces structured cancel events for audit/tracing.
%%
%% Limitations:
%% - Effect cancellation stubbed (wf_effect not implemented, item 010)
%% - Activity cancellation uses O(n) token scan (documented for v2)
%% - wf_exec integration partial (executor has inline state)
%%
%% @end
%%====================================================================
```

##### 2. ARCHITECTURE.md - Add wf_cancel section (if not exists)

**File**: `ARCHITECTURE.md` (add section)
**Changes**: Document wf_cancel architecture

```markdown
### wf_cancel - Cancellation Semantics

**Role**: Structured cancellation for workflow execution.

**Exports**:
- `cancel_activity/3`: Cancel single task
- `cancel_case/1`: Cancel entire case
- `cancel_region/3`: Cancel scoped region
- `is_cancelled/2`: Check if scope cancelled
- `propagate/2`: Propagate cancellation to tokens (wf_exec integration)

**Dependencies**:
- wf_state: Scope metadata, atomic commits
- wf_exec: Token tracking, scope stack (partial integration)

**Events**:
- `#cancel_activity{}`: Activity cancelled
- `#cancel_case{}`: Case cancelled
- `#cancel_region{}`: Region cancelled

**Invariants**:
- Scope isolation: Unrelated scopes unaffected
- No orphaned tokens: Cancelled tokens in cancelled scope
- Scope nesting: Child scopes cancelled when parent cancelled
```

##### 3. README.md - Update with cancellation feature

**File**: `README.md` (if applicable)
**Changes**: Add cancellation to feature list

```markdown
### Workflow Execution

- **wf_exec**: Bytecode executor with parallel branch support
- **wf_state**: Per-case state store with commit protocol
- **wf_cancel**: Structured cancellation (activity/case/region)
- **wf_vm**: Bytecode format and opcodes (including CANCEL_SCOPE)
```

##### 4. Final integration test

**File**: Run full test suite
**Changes**: Verify all tests pass

```bash
# Compile
rebar3 compile

# Run all tests
rebar3 ct

# Dialyzer
rebar3 dialyzer

# Coverage
rebar3 cover
```

#### Success Criteria:

##### Automated Verification:

- [ ] All tests pass: `rebar3 ct`
- [ ] No dialyzer warnings: `rebar3 dialyzer`
- [ ] Coverage > 90%: `rebar3 cover`
- [ ] Documentation builds: `rebar3 edoc`

##### Manual Verification:

- [ ] Module documentation complete
- [ ] Inline comments clear
- [ ] ARCHITECTURE.md updated
- [ ] Examples in documentation
- [ ] No TODO comments left (except documented limitations)
- [ ] Code review completed

**Note**: This is the final phase. Ensure all documentation is complete and all tests pass.

---

## Testing Strategy

### Unit Tests:

**wf_cancel_tests.erl**:
- Activity cancellation (with/without effect)
- Case cancellation (all tokens)
- Region cancellation (scope only)
- Nested scope cancellation
- Invariant verification (scope isolation, no orphaned tokens, scope nesting)
- Error cases (scope_not_found, already_cancelled, task_not_found)
- Edge cases (empty scope, single token, no tokens)

**wf_state_tests.erl** (Phase 1):
- Case status tracking
- set_case_status mutation
- Status field initialization
- Status persistence in ETS

**wf_exec_tests.erl** (Phase 6):
- wf_cancel:propagate/2 integration
- Scope stack operations
- Cancellation propagation

### Integration Tests:

**End-to-end scenarios**:
1. Cancel region with multiple tokens, verify only tokens in scope cancelled
2. Cancel case with active branches, verify all tokens cancelled
3. Cancel activity in running workflow, verify task skipped
4. Cancel nested scopes, verify child scopes also cancelled
5. Consecutive cancellations, verify errors on double-cancel

### Invariant Tests:

**Scope Isolation**:
- Cancel scope1, verify scope2 tokens active
- Cancel parent, verify unrelated children active

**No Orphaned Tokens**:
- Cancel scope, verify all cancelled tokens in scope
- Cancel case, verify no active tokens remain

**Scope Nesting**:
- Cancel parent, verify child scopes cancelled
- Cancel child, verify parent active

### Performance Tests:

**Benchmarks**:
1. Small scope (10 tokens): < 1ms
2. Large scope (1000 tokens): < 100ms
3. Complexity isolation (10000 total, 10 in scope): < 1ms

**Verify O(scope_size)**:
- Measure cancellation time vs scope size
- Verify linear relationship with scope tokens, not total tokens

### Manual Testing Steps:

1. **Create workflow with cancel scopes**:
   ```erlang
   Bytecode = [
       {'CANCEL_SCOPE', {enter, scope1}},
       {'TASK_EXEC', task_a},
       {'CANCEL_SCOPE', {enter, scope2}},
       {'TASK_EXEC', task_b},
       {'CANCEL_SCOPE', {exit, scope2}},
       {'TASK_EXEC', task_c},
       {'CANCEL_SCOPE', {exit, scope1}},
       {'DONE'}
   ],
   ```

2. **Execute workflow**:
   ```erlang
   ExecState = wf_exec:new(Bytecode),
   {ExecState1, _} = wf_exec:step(ExecState),
   ```

3. **Cancel region**:
   ```erlang
   {ok, Event} = wf_cancel:cancel_region(CaseId, scope1, []),
   ```

4. **Verify event structure**:
   ```erlang
   ?assertEqual(scope1, Event#cancel_region.scope_id),
   ?assert(length(Event#cancel_region.cancelled_tokens) > 0),
   ```

5. **Verify state consistency**:
   ```erlang
   Tokens = wf_state:get_tokens(State),
   ActiveTokens = [T || {_, T} <- maps:to_list(Tokens), T#token.status =:= active],
   ?assertEqual(0, length(ActiveTokens)).
   ```

## Migration Notes

**From wf_exec stubs to wf_cancel**:

1. **is_scope_cancelled/2**: Stub remains (wf_exec has inline state, not wf_state). Document limitation.

2. **propagate_cancellation/2**: Now delegates to `wf_cancel:propagate/2`. No behavior change.

3. **State management**: wf_cancel uses `wf_state:state()` from ETS. Caller manages state lifecycle.

**For wf_effect integration (future)**:

When item 010 (wf_effect) is implemented:
1. Replace `cancel_effects_for_tokens/1` stub with real calls to `wf_effect:cancel_effect/2`
2. Replace `get_effect_for_token/1` stub with effect lookup from wf_effect
3. Replace `cancel_effect/1` stub with real effect cancellation
4. Add integration tests for effect cancellation
5. Remove TODO comments

**For wf_exec refactoring (future)**:

When wf_exec refactored to use wf_state:
1. Add `wf_state:state()` field to `#exec_state{}` record
2. Update `is_scope_cancelled/2` to call `wf_cancel:is_cancelled(State, ScopeId)`
3. Remove limitation comments
4. Add integration tests for executor-based cancellation

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/research.md`
- Specification: `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/item.json`
- wf_exec: `/Users/speed/wf-substrate/src/wf_exec.erl` (lines 473-529, 55-64)
- wf_state: `/Users/speed/wf-substrate/src/wf_state.erl` (lines 83-90, 117-125, 530-559)
- wf_vm: `/Users/speed/wf-substrate/src/wf_vm.erl` (line 19: CANCEL_SCOPE opcode)
- Tests: `/Users/speed/wf-substrate/test/wf_exec_tests.erl` (lines 183-238)
