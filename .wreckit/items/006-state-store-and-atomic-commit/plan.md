# Implement state store with atomic commit protocol Implementation Plan

## Implementation Plan Title

State Store with Atomic Commit: Per-Case Transactional State Management for Workflow Execution

## Overview

This implementation creates `wf_state.erl`, a per-case state store with atomic commit protocol that provides transactional state management for workflow execution. The state store manages all mutable execution state (context, tokens, scopes, metadata) with atomic commit semantics to ensure crash recovery and consistency.

The module implements a **buffer-apply-commit** protocol: mutations are buffered during execution quanta, validated for consistency at commit boundaries, applied atomically (all-or-nothing), and produce immutable receipts. This ensures that a crash mid-execution never leaves partially-applied state—either all mutations in a quantum commit or none do.

**Critical Design Requirement** (from PROMPT.md:29-33): "State updates are buffered during a reduction quantum and only applied at commit boundaries. This ensures that a crash mid-quantum does not leave partial state." The atomic commit protocol is foundational for fault tolerance and crash recovery.

## Current State

**Existing State**: No `wf_state.erl` module exists yet. The project has:

1. **Executor implementation** (`/Users/speed/wf-substrate/src/wf_exec.erl:1-666`):
   - Contains `#exec_state{}` record (lines 55-66) with inline state management
   - State fields: `ip`, `bytecode`, `ctx`, `tokens`, `branch_map`, `join_counters`, `scope_stack`, `step_count`, `status`, `current_token`
   - All state mutations are direct record updates (no buffering)
   - No atomic commit protocol
   - No receipt generation
   - State is ephemeral (lost on process crash)

2. **Type definitions** (`/Users/speed/wf-substrate/src/wf_vm.erl:1-50`):
   - Defines `wf_bc()`, `opcode()`, `join_policy()`, `loop_policy()`, `mi_policy()` types
   - No state or mutation types defined yet

3. **Test infrastructure** (`/Users/speed/wf-substrate/test/wf_exec_tests.erl:1-305`):
   - Tests access exec_state fields directly (e.g., `wf_exec:get_ctx/1`, `wf_exec:get_step_count/1`)
   - No state store tests exist

**Blocking Dependencies**:
1. **wf_receipt.erl** (item 010) - Not implemented yet, but receipts are core to state store protocol
2. **wf_exec.erl** (item 005) - Currently implemented, but must be refactored to use wf_state

**Current State Management Pattern** (from wf_exec.erl):
```erlang
%% Current pattern: Direct record updates (no buffering)
execute_task_exec({_TaskExec, _TaskName}, ExecState) ->
    Ctx = ExecState#exec_state.ctx,
    case TaskFun(Ctx) of
        {ok, NewCtx} ->
            %% Direct mutation - no atomic commit
            ExecState#exec_state{ctx = NewCtx, ip = IP + 1, step_count = StepCount + 1};
        {effect, EffectSpec, ContCtx} ->
            %% Direct mutation - no receipt
            ExecState#exec_state{ctx = ContCtx, status = blocked_effect, ...}
    end.
```

**Problem**: Current implementation violates PROMPT.md:29-33 requirement ("State updates are buffered... only applied at commit boundaries"). Every opcode handler directly mutates exec_state with no transactional semantics.

## Desired End State

**Specification**: A fully implemented state store module with atomic commit protocol:

1. **wf_state.erl** module with:
   - State record (`#state{}`) containing: case_id, ctx, tokens, scopes, metadata, buffered_mutations, ets_table
   - Mutation record (`#mutation{}`) capturing: mutation_id, type, timestamp
   - Token record (`#token{}`) from wf_exec: token_id, ip, scope_id, value, status
   - Scope record (`#scope{}`) for cancel scopes: scope_id, parent_scope, status, tokens, entered_at
   - Metadata record (`#metadata{}`) for execution stats: step_count, start_time, last_commit_time
   - Type specs: `-type state()`, `-type mutation()`, `-type ctx()`, `-type token_id()`, `-type scope_id()`, `-type case_id()`

2. **Core API functions**:
   - `new/1`: Create new state store with initial ctx
   - `get_ctx/1`, `put_ctx/2`: Context access
   - `get_tokens/1`, `add_token/3`, `remove_token/2`: Token management
   - `enter_scope/2`, `exit_scope/2`, `get_scope/2`: Scope management
   - `buffer_mutation/2`: Buffer mutation during quantum
   - `commit/1`: Atomic commit (validate → apply → receipt)
   - `rollback/1`: Discard buffered mutations
   - `snapshot/1`, `restore/2`: Snapshot/restore for debugging

3. **ETS persistence**:
   - ETS table (`wf_state_store`) for crash resilience
   - State persisted on every commit
   - State restorable on process crash
   - Table owned by separate gen_server (wf_state_store) for survival

4. **Receipt integration** (stub for item 010):
   - Receipt record capturing mutations, timestamps, state hashes
   - Receipt produced on every commit
   - Receipt stored in append-only log
   - TODO: Integrate with wf_receipt module when item 010 implemented

5. **Verification criteria**:
   - `rebar3 compile` succeeds with no warnings
   - `rebar3 eunit` passes all state store tests
   - Dialyzer analysis passes with no type errors
   - Atomic commit verified: crash mid-quantum leaves no partial state
   - Rollback verified: discarded mutations don't affect state
   - ETS persistence verified: state survives process crash
   - Receipt generation verified: every commit produces receipt

### Key Discoveries:

- **From PROMPT.md:29-33** (Critical Requirement): "State updates are buffered during a reduction quantum and only applied at commit boundaries via commit/1. This ensures that a crash mid-quantum does not leave partial state. The commit protocol: buffer mutations during steps -> validate consistency -> apply atomically -> produce receipt." This is the foundational invariant that must be preserved.

- **From ARCHITECTURE.md:1519-1537** (ETS Decision): Use ETS table for state persistence. ETS provides fast concurrent access and survives process crashes (if owned by separate process). Alternative: gen_server (slower, message passing). Process dictionary not suitable (lost on crash, not shareable).

- **From ARCHITECTURE.md:1260-1278** (Module Spec): wf_state exports: new/1, atomic_commit/2, get_ctx/1, set_ctx/2, get_tokens/1, set_tokens/2. Protocol: Buffer mutations → Validate → Apply → Produce receipt.

- **From item.json:6** (Required Exports): new/1, get_ctx/1, put_ctx/2, add_token/3, remove_token/2, get_tokens/1, enter_scope/2, exit_scope/2, get_scope/2, buffer_mutation/2, commit/1, rollback/1, snapshot/1, restore/2. Must include -type state() and -type mutation() specs.

- **Current wf_exec Pattern** (wf_exec.erl:55-283): All state mutations are direct record updates. Example: `ExecState#exec_state{ctx = NewCtx, ip = IP + 1}`. This violates atomic commit requirement. Must be refactored to use wf_state API.

- **Token Record Structure** (wf_exec.erl:29-35): `#token{token_id, ip, scope_id, value, status}`. This structure must be preserved in wf_state for compatibility.

- **wf_receipt Dependency** (item 010): Receipt module not implemented yet. Decision: Create stub receipt record in wf_state for v1. Add TODO to integrate with proper wf_receipt module when item 010 is complete.

- **State Components** (ARCHITECTURE.md:1118-1157): State includes ctx (user context), tokens (active token map), scopes (cancel scope map), metadata (step count, timestamps). State is per-case (identified by case_id).

## What We're NOT Doing

- **NOT implementing wf_exec integration in this item** (deferred to item 005 refactoring): wf_state will be implemented standalone. Refactoring wf_exec to use wf_state is a separate task.
- **NOT implementing full wf_receipt module** (item 010): Will create stub receipt record for v1. Full receipt storage and verification is item 010.
- **NOT implementing wf_case_runner** (item 012): State store will be usable by any process. Case runner integration is separate.
- **NOT implementing cancellation semantics** (item 008): State store provides scope storage, but cancellation propagation logic is separate.
- **NOT implementing distributed state replication**: State is local to a single node. Multi-node replication is future work.
- **NOT implementing state compression or optimization**: State snapshots are complete. Optimization is future work.
- **NOT implementing query APIs for historical state**: Only current state is queryable. Historical receipts are stored but not indexed.
- **NOT implementing state migration for schema changes**: State format is version 1.0. Migration logic is future work.
- **NOT implementing performance monitoring**: No telemetry or metrics collection. Monitoring is external (OS tools, observer).
- **NOT implementing transaction isolation across processes**: State is single-writer (case runner). Concurrent access is reads-only.

## Implementation Approach

**High-Level Strategy**: Implement wf_state standalone first (independent of wf_exec), using stub wf_receipt for receipt generation. Use ETS table owned by separate gen_server (wf_state_store) for crash resilience. Follow test-driven development where each function has unit tests covering success and error paths.

**Architectural Principles**:
1. **Transactional Semantics**: All state changes go through buffer → validate → apply → receipt protocol
2. **Crash Resilience**: State persisted in ETS table owned by separate process
3. **Immutable Receipts**: Every commit produces immutable receipt capturing mutations
4. **Type Safety**: Full -type and -spec declarations, Dialyzer-clean
5. **Pure Functional Core**: State updates are functional transformations (no in-place mutation)

**Key Design Decisions**:

1. **State Structure**: `#state{case_id, ctx, tokens, scopes, metadata, buffered_mutations, ets_table}`. State is per-case identified by unique case_id (ref). State components match wf_exec's exec_state structure for compatibility.

2. **Mutation Types**: Union type of 10 mutations: set_ctx, update_ctx, add_token, remove_token, update_token, enter_scope, exit_scope, cancel_scope, increment_step_count, set_metadata. Each mutation has dedicated apply function.

3. **Buffer Strategy**: Mutations buffered in list (reversed order for prepend performance). Cleared on commit/rollback. No limit for v1 (trust quanta sizing). Warning if buffer > 1000 mutations.

4. **Validation Checks**: Token IDs unique, scope IDs unique, scope nesting consistent (exit matches enter), token exists before update/remove, context is valid map. Strict validation for v1 (catch errors early).

5. **ETS Ownership**: Separate gen_server (wf_state_store) owns ETS table. Table survives case runner crashes. State written on commit, read on crash recovery, deleted on case completion.

6. **Receipt Structure** (stub): `#receipt{receipt_id, case_id, mutations, timestamp, state_before_hash, state_after_hash}`. Simple record for v1. Will integrate with wf_receipt (item 010) later.

7. **Snapshot/Restore**: Use term_to_binary/binary_to_term. Filter out non-serializable fields (buffered_mutations, ets_table). Document limitations (pids, ports, refs not serializable).

8. **Error Handling**: validate_mutations/2 returns {error, Reason}. apply_mutations/2 wrapped in try-catch. On exception, rollback completely (return original state). Commit fails atomically.

9. **Scope Stack Tracking**: Maintain explicit scope stack in state (separate from parent_scope links). Enables O(1) "current scope" lookup. parent_scope links used for validation/debugging.

10. **Token Status Tracking**: Keep status in token record (current pattern from wf_exec). Enables O(1) status lookup. Filter tokens by status when needed.

---

## Phases

### Phase 1: Core Types and Records

#### Overview

Define all types and records for the state store, establishing the foundation for all state operations. This phase creates the "data model" for transactional state management.

#### Changes Required:

##### 1. State Record and Core Types

**File**: `src/wf_state.erl`
**Changes**: Create new module with record and type definitions

```erlang
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
    restore/2
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

%% Mutation record: Single state mutation
-record(mutation, {
    id :: mutation_id(),                      %% Unique mutation ID
    type :: mutation(),                       %% Mutation type and data
    timestamp :: erlang:timestamp()           %% When buffered
}).

%% Token record: Logical thread of execution
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

%% Receipt record: Stub for item 010 integration
-record(receipt, {
    receipt_id :: term(),
    case_id :: case_id(),
    mutations :: [#mutation{}],
    timestamp :: erlang:timestamp(),
    state_before_hash :: binary(),
    state_after_hash :: binary()
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with no warnings
- [ ] Dialyzer passes with no type errors
- [ ] Type specs are complete for all records
- [ ] All types exported with -export_type attribute

##### Manual Verification:

- [ ] Record definitions match wf_exec's exec_state structure
- [ ] Token record structure preserved from wf_exec
- [ ] Mutation type covers all required operations
- [ ] Module documentation explains atomic commit protocol

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: State Creation and Basic Accessors

#### Overview

Implement state creation (`new/1`) and basic accessor functions (`get_ctx/1`, `get_tokens/1`, `get_scope/2`). This phase provides the foundation for state initialization and read operations.

#### Changes Required:

##### 1. State Creation Function

**File**: `src/wf_state.erl`
**Changes**: Add new/1 function

```erlang
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
    State = #state{
        case_id = CaseId,
        ctx = InitialCtx,
        tokens = #{},
        scopes = #{RootScopeId => RootScope},
        metadata = Metadata,
        buffered_mutations = [],
        ets_table = undefined
    },
    %% Persist to ETS
    case ets:lookup(wf_state_store, CaseId) of
        [] ->
            ets:insert(wf_state_store, State);
        [_] ->
            error({case_id_already_exists, CaseId})
    end,
    {ok, State}.
```

##### 2. Basic Accessor Functions

**File**: `src/wf_state.erl`
**Changes**: Add accessor functions

```erlang
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes new state creation tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] new/1 creates state with empty tokens map
- [ ] new/1 creates state with root scope initialized
- [ ] get_ctx/1 returns initial context
- [ ] get_tokens/1 returns empty map for new state
- [ ] get_scope/2 returns root scope for 'root' key
- [ ] get_scope/2 returns undefined for non-existent scope

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Mutation Buffering

#### Overview

Implement mutation buffering infrastructure (`buffer_mutation/2`) and state update functions (`put_ctx/2`, `add_token/3`, `remove_token/2`, `enter_scope/2`, `exit_scope/2`). These functions buffer mutations without applying them immediately.

#### Changes Required:

##### 1. Mutation Buffering

**File**: `src/wf_state.erl`
**Changes**: Add buffer_mutation/2 and warning logic

```erlang
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
```

##### 2. Context Update Functions

**File**: `src/wf_state.erl`
**Changes**: Add put_ctx/2

```erlang
%% @doc Update context (buffers mutation)
-spec put_ctx(state(), ctx()) -> {ok, state()}.
put_ctx(State, NewCtx) when is_map(NewCtx) ->
    Mutation = {set_ctx, NewCtx},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.
```

##### 3. Token Management Functions

**File**: `src/wf_state.erl`
**Changes**: Add add_token/3, remove_token/2

```erlang
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
```

##### 4. Scope Management Functions

**File**: `src/wf_state.erl`
**Changes**: Add enter_scope/2, exit_scope/2

```erlang
%% @doc Enter cancel scope (buffers mutation)
-spec enter_scope(state(), scope_id()) -> {ok, state()}.
enter_scope(State, ScopeId) ->
    %% Parent scope is current scope (root for top-level)
    ParentScope = get_current_scope(State),
    Mutation = {enter_scope, ScopeId, ParentScope},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @doc Exit cancel scope (buffers mutation)
-spec exit_scope(state(), scope_id()) -> {ok, state()}.
exit_scope(State, ScopeId) ->
    Mutation = {exit_scope, ScopeId},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.

%% @private Get current scope (root if empty stack)
-spec get_current_scope(state()) -> scope_id().
get_current_scope(#state{scopes = Scopes}) ->
    %% For now, return root scope
    %% In future, maintain explicit scope stack
    case maps:is_key(root, Scopes) of
        true -> root;
        false -> undefined
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes mutation buffering tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] buffer_mutation/2 prepends mutation to buffered_mutations list
- [ ] Multiple buffer_mutation calls accumulate mutations in reverse order
- [ ] put_ctx/2 buffers set_ctx mutation
- [ ] add_token/3 buffers add_token mutation
- [ ] remove_token/2 buffers remove_token mutation
- [ ] enter_scope/2 buffers enter_scope mutation with parent scope
- [ ] exit_scope/2 buffers exit_scope mutation
- [ ] Warning logged when buffer exceeds 1000 mutations
- [ ] Buffered mutations are NOT applied to state yet

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Mutation Validation

#### Overview

Implement mutation validation logic (`validate_mutations/2`) that checks consistency before applying mutations. This phase ensures that invalid mutations are rejected before state is modified.

#### Changes Required:

##### 1. Validation Function

**File**: `src/wf_state.erl`
**Changes**: Add validate_mutations/2

```erlang
%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate mutations before applying
-spec validate_mutations([#mutation{}], state()) -> ok | {error, term()}.
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

validate_single_mutation(_Mutation, _State) ->
    {error, unknown_mutation_type}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes validation tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] validate_mutations/2 returns ok for valid mutation sequence
- [ ] validate_mutations/2 returns {error, Reasons} for invalid mutations
- [ ] set_ctx with map validates ok
- [ ] set_ctx with non-map returns error
- [ ] add_token with duplicate ID returns error
- [ ] remove_token with non-existent ID returns error
- [ ] update_token with non-existent ID returns error
- [ ] enter_scope with duplicate ID returns error
- [ ] exit_scope with non-existent ID returns error
- [ ] cancel_scope with non-existent ID returns error
- [ ] increment_step_count always validates ok
- [ ] Multiple errors returned as list

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Mutation Application

#### Overview

Implement mutation application logic (`apply_mutations/2`, `apply_single_mutation/2`) that applies validated mutations to state atomically. This phase ensures all mutations are applied in a single transaction.

#### Changes Required:

##### 1. Apply Mutations Function

**File**: `src/wf_state.erl`
**Changes**: Add apply_mutations/2 and apply_single_mutation/2

```erlang
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes mutation application tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] apply_mutations/2 applies all mutations in order
- [ ] set_ctx mutation updates ctx field
- [ ] add_token mutation adds token to tokens map
- [ ] remove_token mutation removes token from tokens map
- [ ] update_token mutation updates existing token
- [ ] enter_scope mutation adds scope to scopes map
- [ ] exit_scope mutation removes scope from scopes map
- [ ] cancel_scope mutation updates scope status to cancelled
- [ ] increment_step_count mutation increments step_count
- [ ] set_metadata mutation replaces metadata record
- [ ] Mutations applied atomically (all succeed or all fail)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Atomic Commit and Receipt Generation

#### Overview

Implement atomic commit protocol (`commit/1`) that validates mutations, applies them atomically, persists to ETS, and produces receipt. Also implement rollback (`rollback/1`) to discard buffered mutations.

#### Changes Required:

##### 1. Commit Function

**File**: `src/wf_state.erl`
**Changes**: Add commit/1

```erlang
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
```

##### 2. Rollback Function

**File**: `src/wf_state.erl`
**Changes**: Add rollback/1

```erlang
%% @doc Rollback buffered mutations (discard without applying)
-spec rollback(state()) -> state().
rollback(State) ->
    State#state{buffered_mutations = []}.
```

##### 3. State Hashing Function

**File**: `src/wf_state.erl`
**Changes**: Add hash_state/1

```erlang
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes commit and rollback tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] commit/1 returns {ok, NewState, Receipt} for valid mutations
- [ ] commit/1 returns {error, validation_failed} for invalid mutations
- [ ] commit/1 applies all mutations to state
- [ ] commit/1 persists state to ETS table
- [ ] commit/1 produces receipt with mutations, timestamps, hashes
- [ ] commit/1 clears buffered_mutations after successful commit
- [ ] commit/1 updates last_commit_time in metadata
- [ ] rollback/1 clears buffered_mutations without applying
- [ ] rollback/1 does not modify state fields
- [ ] commit/1 fails atomically if exception during apply
- [ ] Receipt contains state_before_hash and state_after_hash

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 7: ETS Persistence (gen_server)

#### Overview

Implement gen_server to own ETS table for crash resilience. State persisted on commit, restorable on crash. Table survives case runner process crashes.

#### Changes Required:

##### 1. gen_server Implementation

**File**: `src/wf_state.erl`
**Changes**: Add gen_server callbacks and start_link/0

```erlang
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
    ets:new(wf_state_store, [
        named_table,
        set,
        {keypos, #state.case_id},
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
```

##### 2. Add ETS Table Reference to State

**File**: `src/wf_state.erl`
**Changes**: Update new/1 to set ets_table field

```erlang
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
```

##### 3. Add State Restoration Function

**File**: `src/wf_state.erl`
**Changes**: Add restore_from_ets/1

```erlang
%% @doc Restore state from ETS (for crash recovery)
-spec restore_from_ets(case_id()) -> {ok, state()} | {error, not_found}.
restore_from_ets(CaseId) ->
    case ets:lookup(wf_state_store, CaseId) of
        [State] -> {ok, State};
        [] -> {error, not_found}
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes ETS persistence tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] start_link/0 creates gen_server process
- [ ] gen_server creates wf_state_store ETS table
- [ ] ETS table is public with read_concurrency and write_concurrency
- [ ] new/1 inserts state into ETS table
- [ ] commit/1 updates state in ETS table
- [ ] restore_from_ets/1 loads state from ETS
- [ ] ETS table survives gen_server restart
- [ ] State persists across process crashes

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 8: Snapshot and Restore

#### Overview

Implement snapshot (`snapshot/1`) and restore (`restore/2`) functions for debugging and replay. Snapshots serialize state to binary, restore deserializes binary to state.

#### Changes Required:

##### 1. Snapshot Function

**File**: `src/wf_state.erl`
**Changes**: Add snapshot/1

```erlang
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
```

##### 2. Restore Function

**File**: `src/wf_state.erl`
**Changes**: Add restore/2

```erlang
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
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` passes snapshot and restore tests
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] snapshot/1 serializes state to binary
- [ ] snapshot/1 excludes buffered_mutations from binary
- [ ] snapshot/1 excludes ets_table from binary
- [ ] restore/2 deserializes binary to state
- [ ] restore/2 validates case_id matches
- [ ] restore/2 returns error for invalid binary
- [ ] Snapshot and restore round-trip preserves state
- [ ] Restored state has empty buffered_mutations
- [ ] Restored state has current ets_table reference

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 9: Test Suite

#### Overview

Create comprehensive test suite for wf_state module. Tests cover all functions, success paths, error paths, and edge cases.

#### Changes Required:

##### 1. Test Module Structure

**File**: `test/wf_state_tests.erl`
**Changes**: Create new test module

```erlang
-module(wf_state_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%% Test state creation
new_test_() ->
    InitialCtx = #{key => value},
    {ok, State} = wf_state:new(InitialCtx),
    [
        ?_assertEqual(InitialCtx, wf_state:get_ctx(State)),
        ?_assertEqual(#{}, wf_state:get_tokens(State)),
        ?_assertMatch(#scope{scope_id = root}, wf_state:get_scope(State, root)),
        ?_assertEqual(undefined, wf_state:get_scope(State, non_existent))
    ].

%% Test mutation buffering
buffer_mutation_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    State1 = wf_state:buffer_mutation(State0, {set_ctx, #{new => ctx}}),
    State2 = wf_state:buffer_mutation(State1, {increment_step_count}),
    [
        ?_assertEqual(2, length(State2#state.buffered_mutations)),
        ?_assertEqual(#{}, wf_state:get_ctx(State2))  %% Not applied yet
    ].

%% Test atomic commit
commit_test_() ->
    InitialCtx = #{counter => 0},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1, _Receipt} = wf_state:put_ctx(State0, #{counter => 1}),
    {ok, State2, Receipt} = wf_state:commit(State1),
    [
        ?_assertEqual(#{counter => 1}, wf_state:get_ctx(State2)),
        ?_assertEqual([], State2#state.buffered_mutations),
        ?_assertMatch(#receipt{}, Receipt)
    ].

%% Test validation errors
commit_validation_error_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    %% Try to remove non-existent token
    {ok, State1} = wf_state:remove_token(State0, non_existent_token),
    Result = wf_state:commit(State1),
    [
        ?_assertMatch({error, {validation_failed, _}}, Result)
    ].

%% Test rollback
rollback_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:put_ctx(State0, #{new => ctx}),
    State2 = wf_state:rollback(State1),
    [
        ?_assertEqual([], State2#state.buffered_mutations),
        ?_assertEqual(#{}, wf_state:get_ctx(State2))  %% Original ctx unchanged
    ].

%% Test token management
token_management_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    TokenId = make_ref(),
    Token = #token{
        token_id = TokenId,
        ip = 0,
        scope_id = root,
        value = undefined,
        status = active
    },
    {ok, State1} = wf_state:add_token(State0, TokenId, Token),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    Tokens = wf_state:get_tokens(State2),
    [
        ?_assert(maps:is_key(TokenId, Tokens)),
        ?_assertEqual(Token, maps:get(TokenId, Tokens))
    ].

%% Test scope management
scope_management_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:enter_scope(State0, my_scope),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    Scope = wf_state:get_scope(State2, my_scope),
    [
        ?_assertMatch(#scope{scope_id = my_scope}, Scope),
        ?_assertEqual(root, Scope#scope.parent_scope),
        ?_assertEqual(active, Scope#scope.status)
    ].

%% Test snapshot and restore
snapshot_restore_test_() ->
    InitialCtx = #{key => value},
    {ok, State0} = wf_state:new(InitialCtx),
    {ok, State1} = wf_state:put_ctx(State0, #{new => ctx}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    Binary = wf_state:snapshot(State2),
    {ok, State3} = wf_state:restore(Binary, State2#state.case_id),
    [
        ?_assertEqual(State2#state.ctx, State3#state.ctx),
        ?_assertEqual(State2#state.tokens, State3#state.tokens),
        ?_assertEqual([], State3#state.buffered_mutations)
    ].

%% Test ETS persistence
ets_persistence_test_() ->
    InitialCtx = #{},
    {ok, State0} = wf_state:new(InitialCtx),
    CaseId = State0#state.case_id,
    {ok, State1} = wf_state:put_ctx(State0, #{persisted => true}),
    {ok, State2, _Receipt} = wf_state:commit(State1),
    %% Restore from ETS
    {ok, State3} = wf_state:restore_from_ets(CaseId),
    [
        ?_assertEqual(State2#state.ctx, State3#state.ctx),
        ?_assertEqual(State2#state.tokens, State3#state.tokens)
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit -m wf_state_tests` passes all tests (100% success)
- [ ] Code coverage > 90% for wf_state.erl
- [ ] Dialyzer passes

##### Manual Verification:

- [ ] All state creation tests pass
- [ ] All mutation buffering tests pass
- [ ] All commit tests pass (success and error paths)
- [ ] All rollback tests pass
- [ ] All token management tests pass
- [ ] All scope management tests pass
- [ ] All snapshot/restore tests pass
- [ ] All ETS persistence tests pass
- [ ] Edge cases covered (empty state, large buffers, duplicate IDs)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

## Testing Strategy

### Unit Tests:

- **State Creation**: Test new/1 creates valid state with empty tokens, root scope, zero step count
- **Basic Accessors**: Test get_ctx/1, get_tokens/1, get_scope/2 return correct values
- **Mutation Buffering**: Test buffer_mutation/2 accumulates mutations without applying
- **Context Updates**: Test put_ctx/2 buffers set_ctx mutation
- **Token Management**: Test add_token/3, remove_token/2 buffer mutations
- **Scope Management**: Test enter_scope/2, exit_scope/2 buffer mutations
- **Validation**: Test validate_mutations/2 catches all invalid inputs (duplicate IDs, missing IDs, wrong types)
- **Mutation Application**: Test apply_mutations/2 applies all mutations correctly
- **Atomic Commit**: Test commit/1 validates, applies, persists, produces receipt
- **Rollback**: Test rollback/1 discards buffered mutations
- **Snapshot/Restore**: Test snapshot/1 serializes state, restore/2 deserializes
- **ETS Persistence**: Test state persists to ETS, restorable from ETS
- **Error Handling**: Test commit fails atomically on exception
- **Edge Cases**: Test empty state, large buffers (> 1000), nested scopes

### Integration Tests:

- **End-to-End Workflow**: Create state, buffer mutations, commit, verify receipt
- **Crash Recovery**: Commit state, simulate crash, restore from ETS, verify state matches
- **Multiple Commits**: Sequential commits, each producing receipt
- **Validation Failure**: Buffer invalid mutation, commit fails, state unchanged
- **Rollback After Failure**: Validation fails, rollback discards buffer, new commit succeeds

### Manual Testing Steps:

1. Start wf_state gen_server: `wf_state:start_link()`
2. Create state: `{ok, State0} = wf_state:new(#{})`
3. Buffer mutations: `{ok, State1} = wf_state:put_ctx(State0, #{test => 1})`
4. Commit mutations: `{ok, State2, Receipt} = wf_state:commit(State1)`
5. Verify context: `#{test := 1} = wf_state:get_ctx(State2)`
6. Verify receipt: `#receipt{mutations = Mutations} = Receipt`
7. Test rollback: `{ok, State3} = wf_state:put_ctx(State2, #{test => 2})`, `State4 = wf_state:rollback(State3)`
8. Verify context unchanged: `#{test := 1} = wf_state:get_ctx(State4)`
9. Test snapshot: `Binary = wf_state:snapshot(State4)`
10. Test restore: `{ok, State5} = wf_state:restore(Binary, State4#state.case_id)`
11. Verify restored state: `#{test := 1} = wf_state:get_ctx(State5)`

## Migration Notes

**This is a new module** - no existing data migration required.

**Future Integration**: After item 005 (wf_exec) refactoring, executor will delegate state operations to wf_state. Current inline state management in exec_state will be replaced with wf_state API calls.

**Receipt Migration**: Stub receipt record will be replaced with proper wf_receipt module (item 010). Migration path:
1. v1: Stub receipt in wf_state
2. v2: Integrate wf_receipt:store/1 and wf_receipt:lookup/1
3. v3: Remove stub, use wf_receipt exclusively

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/006-state-store-and-atomic-commit/research.md`
- PROMPT.md: `/Users/speed/wf-substrate/PROMPT.md:29-33` (Atomic commit requirement)
- ARCHITECTURE.md: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1260-1278` (State store spec)
- ARCHITECTURE.md: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1519-1537` (ETS decision)
- ARCHITECTURE.md: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1118-1157` (Execution state types)
- wf_exec.erl: `/Users/speed/wf-substrate/src/wf_exec.erl:55-66` (exec_state record)
- wf_exec.erl: `/Users/speed/wf-substrate/src/wf_exec.erl:29-35` (token record)
