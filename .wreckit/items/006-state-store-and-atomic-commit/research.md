# Research: Implement state store with atomic commit protocol

**Date**: 2025-01-10
**Item**: 006-state-store-and-atomic-commit

## Research Question

Implement wf_state.erl: per-case state store with atomic commit protocol. The state store holds: ctx() (user-provided context map, opaque to the engine), token tracking (which tokens are active, where they are), scope metadata (cancel scopes, their status), and execution metadata (step count, timestamps).

State updates are buffered during a reduction quantum and only applied at commit boundaries via commit/1. This ensures that a crash mid-quantum does not leave partial state. The commit protocol: buffer mutations during steps -> validate consistency -> apply atomically -> produce receipt.

All mutations produce receipts (wf_receipt records) capturing what changed, when, and why. Receipts form an append-only log.

Export: new/1 (initial ctx), get_ctx/1, put_ctx/2, add_token/3, remove_token/2, get_tokens/1, enter_scope/2, exit_scope/2, get_scope/2, buffer_mutation/2, commit/1, rollback/1, snapshot/1, restore/2.

Include -type state() and -type mutation() specs.

## Summary

This task involves implementing `wf_state.erl`, a per-case state store with atomic commit protocol that provides transactional state management for workflow execution. The state store manages all mutable execution state (context, tokens, scopes, metadata) with atomic commit semantics to ensure crash recovery and consistency.

The module implements a **buffer-apply-commit** protocol: mutations are buffered during execution quanta, validated for consistency at commit boundaries, applied atomically (all-or-nothing), and produce immutable receipts. This ensures that a crash mid-execution never leaves partially-applied state—either all mutations in a quantum commit or none do.

**Critical Design Requirement** (from PROMPT.md:29-33): "State updates are buffered during a reduction quantum and only applied at commit boundaries. This ensures that a crash mid-quantum does not leave partial state." The atomic commit protocol is foundational for fault tolerance and crash recovery.

**Key Architectural Context** (from ARCHITECTURE.md and existing code):
- This is **item 006** in the implementation sequence
- State store is used by `wf_exec` (executor) during execution
- Integrates with `wf_receipt` (item 010) for mutation receipts
- Provides crash recovery foundation for per-case gen_statem processes
- Current `wf_exec` implementation keeps state in `exec_state` record (no persistence yet)

The state store must support:
1. **Context management**: User-provided opaque map flowing through workflow
2. **Token tracking**: Active tokens with positions (IP), scope membership, status
3. **Scope metadata**: Cancel scopes with nesting and status
4. **Execution metadata**: Step count, timestamps, statistics
5. **Atomic commit**: Buffer → validate → apply → receipt protocol
6. **Rollback**: Discard buffered mutations without applying
7. **Snapshot/restore**: Capture and restore complete state (for debugging/replay)

## Current State Analysis

### Existing Implementation

**Current State**: No `wf_state.erl` module exists yet. The project has:

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

### Specification Context

**From ARCHITECTURE.md:1260-1278** - State Store Module Specification:
```
Role: Per-case state store with atomic commit protocol.

Exports:
- new/1: Create new state store
- atomic_commit/2: Commit mutations atomically
- get_ctx/1, set_ctx/2: Context access
- get_tokens/1, set_tokens/2: Token tracking

Dependencies: None

Key Functions:
-spec atomic_commit(State :: state(), Mutations :: [mutation()]) ->
    {ok, NewState :: state(), Receipt :: receipt()} | {error, Reason}.

Protocol: Buffer mutations → Validate → Apply → Produce receipt
```

**From ARCHITECTURE.md:1519-1537** - State Store Design Decision:
```
**ETS** (chosen):
- Fast concurrent access
- Persistent across process crashes
- Standard OTP approach

**gen_server** (alternative):
- Slower (message passing)
- Serializes access
- More control over updates

**Process Dictionary** (not suitable):
- Lost on process crash
- Not shareable across processes
- Discouraged in OTP

**Decision**: ETS for performance and crash resilience.
```

**From PROMPT.md:29-33** - The Critical Requirement:
> "State updates are buffered during a reduction quantum and only applied at commit boundaries via commit/1. This ensures that a crash mid-quantum does not leave partial state. The commit protocol: buffer mutations during steps -> validate consistency -> apply atomically -> produce receipt."

**From item.json:6** - Item 006 Specification:
> "Implement wf_state.erl: per-case state store with atomic commit protocol. The state store holds: ctx() (user-provided context map, opaque to the engine), token tracking (which tokens are active, where they are), scope metadata (cancel scopes, their status), and execution metadata (step count, timestamps)."

> "Export: new/1 (initial ctx), get_ctx/1, put_ctx/2, add_token/3, remove_token/2, get_tokens/1, enter_scope/2, exit_scope/2, get_scope/2, buffer_mutation/2, commit/1, rollback/1, snapshot/1, restore/2."

> "Include -type state() and -type mutation() specs."

### Integration Points

**Where wf_state fits in architecture**:

1. **Executor Integration** (`wf_exec` - item 005):
   - Executor currently manages state inline in `exec_state` record
   - Must be refactored to delegate state operations to `wf_state`
   - Executor's `step/2` should buffer mutations, then `commit/1` at quantum end
   - Example: Each opcode calls `wf_state:buffer_mutation/2`, then `wf_state:commit/1` after N steps

2. **Receipt Integration** (`wf_receipt` - item 010):
   - Every commit produces a `wf_receipt` record
   - Receipts form append-only log per case
   - Receipts contain: mutation list, timestamp, hash, result
   - Used for auditing, replay, crash recovery

3. **Cancellation Integration** (`wf_cancel` - item 008):
   - Cancellation scopes stored in state store
   - Scope metadata includes: scope_id, parent_scope, status (active/cancelled), tokens_in_scope
   - `wf_cancel:propagate/2` updates scope status atomically

4. **Per-Case Process** (`wf_case_runner` - item 012):
   - Each case runner holds a `wf_state` instance
   - State persists across crashes (ETS table)
   - State restored on process restart

## Key Files

### Specification Files

- `/Users/speed/wf-substrate/PROMPT.md:29-33` - **Atomic commit requirement**. Specifies:
  - Buffer mutations during reduction quantum
  - Apply only at commit boundaries
  - Crash mid-quantum leaves no partial state
  - Buffer → validate → apply → receipt protocol

- `/Users/speed/wf-substrate/.wreckit/items/006-state-store-and-atomic-commit/item.json:1-14` - **This item's specification**. Defines:
  - Module: `wf_state.erl`
  - State components: ctx, tokens, scopes, execution metadata
  - Exported API: 13 functions (new, get_ctx, put_ctx, add_token, remove_token, etc.)
  - Type specs: state(), mutation()

- `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1260-1278` - **State store module spec**. Defines:
  - Lines 1260-1278: Module responsibilities and key functions
  - Lines 1519-1537: ETS vs gen_server vs process dictionary decision
  - Lines 1118-1157: Execution state types (ctx, tokens, scopes)

- `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:210-226` - **exec_state structure**. Current pattern:
  - Lines 210-226: exec_state record definition (must be refactored to use wf_state)

### Current Implementation Files

- `/Users/speed/wf-substrate/src/wf_exec.erl:1-666` - **Executor with inline state management**. Analysis:
  - Lines 55-66: exec_state record (ctx, tokens, scopes, metadata)
  - Lines 79-102: new/1 creates exec_state with inline initialization
  - Lines 105-127: Accessor functions (get_ip, get_ctx, get_step_count, set_ctx, get_scope_stack_depth)
  - Lines 142-283: step/2 with direct state mutations (no buffering)
  - **Key issue**: All mutations are direct record updates, violating atomic commit requirement

- `/Users/speed/wf-substrate/src/wf_vm.erl:1-50` - **Type definitions**. Analysis:
  - Lines 10-21: opcode() type (used in mutation type)
  - Lines 24-40: Policy types (join_policy, loop_policy, mi_policy)
  - **Missing**: No state() or mutation() types defined

- `/Users/speed/wf-substrate/test/wf_exec_tests.erl:1-305` - **Executor tests**. Analysis:
  - Tests access state directly via wf_exec accessor functions
  - No state store tests exist
  - Tests will need refactoring when wf_state integrated

### Dependency Files (Not Yet Implemented)

- **Item 010**: `src/wf_receipt.erl` - Receipt storage (item 010)
  - Receipt record definition
  - Store, lookup, verify functions
  - State store produces receipts on commit

- **Item 008**: `src/wf_cancel.erl` - Cancellation semantics
  - Uses wf_state for scope storage
  - Calls wf_state:enter_scope/2, exit_scope/2

- **Item 012**: `src/wf_case_runner.erl` - Per-case gen_statem
  - Holds wf_state instance
  - Restores state on crash

### Files to Create

- `src/wf_state.erl` - **State store module** (primary deliverable)
  - `-record(state, {...})` - State record
  - `-record(mutation, {...})` - Mutation record
  - `-type state()` - State type spec
  - `-type mutation()` - Mutation type spec
  - `new/1` - Create new state store
  - `get_ctx/1`, `put_ctx/2` - Context access
  - `get_tokens/1`, `add_token/3`, `remove_token/2` - Token management
  - `enter_scope/2`, `exit_scope/2`, `get_scope/2` - Scope management
  - `buffer_mutation/2` - Buffer mutation during quantum
  - `commit/1` - Atomic commit (validate → apply → receipt)
  - `rollback/1` - Discard buffered mutations
  - `snapshot/1`, `restore/2` - Snapshot/restore for debugging

- `test/wf_state_tests.erl` - **State store tests**
  - Test state creation and initialization
  - Test context mutations and commit
  - Test token mutations (add, remove, update)
  - Test scope management (enter, exit, nesting)
  - Test atomic commit (all-or-nothing semantics)
  - Test rollback (discard buffered mutations)
  - Test snapshot/restore
  - Test receipt generation
  - Test crash recovery (ETS persistence)

- `include/wf_state.hrl` - **Record definitions** (optional)
  - State record definition
  - Mutation record definition
  - Included by wf_state, wf_exec, tests

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Applications Needed**:
- `stdlib` - For maps, lists, proplists
- `kernel` - For basic types
- `ets` - For state persistence (ETS table for crash resilience)

**Internal Module Dependencies**:
- **wf_receipt.erl** (item 010): State store produces receipts on commit
- **wf_exec.erl** (item 005): Executor consumes state store API
- **wf_cancel.erl** (item 008): Cancellation uses scope management
- **wf_vm.erl** (item 004): Uses policy types (join_policy, loop_policy, mi_policy)

**No Circular Dependencies**: State store is foundational (no dependencies on executor, scheduler, etc.). Safe to implement independently.

### State Store Design

**From ARCHITECTURE.md:1519-1537** - ETS vs gen_server vs process dictionary:

**Decision**: Use **ETS table** for state persistence.

**Rationale**:
1. **Crash Resilience**: ETS tables survive process crashes (if owned by separate process)
2. **Performance**: Fast concurrent access (no message passing overhead)
3. **OTP Standard**: Well-understood pattern for stateful processes
4. **Shared Access**: Multiple processes can access same ETS table (useful for debugging)

**Alternative considered**: gen_server
- **Pros**: Serializes access, more control over updates, explicit state
- **Cons**: Slower (message passing), single point of contention
- **Verdict**: Acceptable for simpler cases, but ETS chosen for performance

**Not suitable**: Process dictionary
- **Lost on crash**: Violates crash recovery requirement
- **Not shareable**: Cannot inspect state from other processes
- **OTP anti-pattern**: Explicitly discouraged in OTP design principles

### state() Type Design

**Components** (from item.json:6 and ARCHITECTURE.md:1118-1157):

```erlang
-record(state, {
    case_id :: case_id(),                    %% Unique identifier
    ctx :: ctx(),                            %% User context (opaque map)
    tokens :: #{token_id() => #token{}},     %% Active tokens
    scopes :: #{scope_id() => #scope{}},     %% Cancel scopes
    metadata :: #metadata{},                  %% Execution metadata
    buffered_mutations :: [mutation()],        %% Buffered mutations
    ets_table :: ets:tid() | undefined      %% ETS table reference
}).

-type ctx() :: map().

-type token_id() :: term().
-type scope_id() :: term().
-type case_id() :: term().

%% Token record (from wf_exec.erl:29-35)
-record(token, {
    token_id :: token_id(),
    ip :: non_neg_integer(),
    scope_id :: scope_id(),
    value :: term(),
    status :: active | complete | cancelled
}).

%% Scope record (cancel scopes)
-record(scope, {
    scope_id :: scope_id(),
    parent_scope :: scope_id() | undefined,
    status :: active | cancelled,
    tokens :: [token_id()],                   %% Tokens in this scope
    entered_at :: erlang:timestamp()         %% Entry timestamp
}).

%% Metadata record
-record(metadata, {
    step_count :: non_neg_integer(),
    start_time :: erlang:timestamp(),
    last_commit_time :: erlang:timestamp() | undefined
}).
```

**Key Design Decisions**:

1. **case_id field**: Unique identifier for state instance. Used for ETS table key.

2. **ctx field**: User-provided map, opaque to engine. Engine treats as black box, only reads/writes entire map.

3. **tokens field**: Map of token_id → token record. Enables O(1) token lookup.

4. **scopes field**: Map of scope_id → scope record. Enables O(1) scope lookup.

5. **buffered_mutations field**: List of mutations accumulated during quantum. Cleared on commit/rollback.

6. **ets_table field**: Reference to ETS table (owned by separate process). Enables crash recovery.

### mutation() Type Design

**Mutation types** (from item.json:6 exports and executor operations):

```erlang
-type mutation() ::
    {set_ctx, ctx()} |
    {update_ctx, fun((ctx()) -> ctx())} |
    {add_token, token_id(), #token{}} |
    {remove_token, token_id()} |
    {update_token, token_id(), fun((#token{}) -> #token{})} |
    {enter_scope, scope_id(), parent_scope_id()} |
    {exit_scope, scope_id()} |
    {cancel_scope, scope_id()} |
    {increment_step_count} |
    {set_metadata, term()}.

%% Mutation application record
-record(mutation, {
    id :: mutation_id(),                      %% Unique mutation ID
    type :: mutation(),                       %% Mutation type and data
    timestamp :: erlang:timestamp()            %% When buffered
}).

-type mutation_id() :: ref().
```

**Mutation Categories**:

1. **Context mutations**: set_ctx, update_ctx
2. **Token mutations**: add_token, remove_token, update_token
3. **Scope mutations**: enter_scope, exit_scope, cancel_scope
4. **Metadata mutations**: increment_step_count, set_metadata

**Mutation Application**:
- Each mutation type has apply function
- Applied atomically in commit/1
- If any mutation fails, all rolled back

### Atomic Commit Protocol

**From PROMPT.md:29-33** and item.json:6:

**Protocol Stages**:

1. **Buffer Stage** (during quantum):
   ```erlang
   %% Executor calls buffer_mutation for each state change
   State0 = wf_state:new(CaseId, InitialCtx),
   State1 = wf_state:buffer_mutation(State0, {set_ctx, NewCtx}),
   State2 = wf_state:buffer_mutation(State1, {add_token, TokenId, Token}),
   %% ...more mutations...
   ```

2. **Validate Stage** (commit start):
   ```erlang
   %% Validate consistency before applying
   case validate_mutations(BufferedMutations, State) of
       ok -> proceed_to_apply();
       {error, Reason} -> rollback(State)
   end.
   ```

3. **Apply Stage** (all-or-nothing):
   ```erlang
   %% Apply mutations atomically
   try
       NewState = apply_mutations(BufferedMutations, State),
       persist_to_ets(NewState),
       {ok, NewState, Receipt}
   catch
       Error:Reason -> rollback(State), {error, Reason}
   end.
   ```

4. **Receipt Stage** (commit complete):
   ```erlang
   Receipt = #wf_receipt{
       receipt_id = make_ref(),
       mutations = BufferedMutations,
       timestamp = erlang:timestamp(),
       state_hash = hash_state(NewState)
   },
   wf_receipt:store(Receipt),
   {ok, NewState, Receipt}.
   ```

**Validation Checks**:
- Token IDs unique (no duplicate add_token)
- Scope IDs unique (no duplicate enter_scope with same ID)
- Scope nesting consistent (exit_scope matches enter_scope)
- Token exists before update_token/remove_token
- Context is valid map

**Rollback Behavior**:
- Discard buffered mutations
- Restore state to pre-commit state
- Return original state (unchanged)

### ETS Integration

**From ARCHITECTURE.md:1519-1537** (ETS chosen):

**ETS Table Design**:
```erlang
%% Table: wf_state_store
%% Type: set
%% Key: case_id
%% Value: #state{} record (complete state snapshot)

init() ->
    ets:new(wf_state_store, [
        named_table,
        set,
        {keypos, #state.case_id},
        public,  %% Allow external inspection for debugging
        {read_concurrency, true},
        {write_concurrency, true}
    ]).
```

**Persistence Strategy**:
1. **Initialize ETS table** in wf_state:start_link/0 (gen_server)
2. **Insert state** on wf_state:new/1
3. **Update state** on commit/1 (write complete snapshot)
4. **Lookup state** on crash recovery (by case_id)
5. **Delete state** on case completion

**Crash Recovery**:
```erlang
%% Case runner restart
init([CaseId]) ->
    case ets:lookup(wf_state_store, CaseId) of
        [{CaseId, State}] ->
            %% Restore state from ETS
            {ok, State};
        [] ->
            %% No state found (first start)
            {ok, wf_state:new(CaseId, #{})}
    end.
```

**Alternative**: ETS table owned by separate gen_server (wf_state_store).
- **Pros**: Table survives case runner crashes
- **Cons**: Additional process overhead
- **Verdict**: Use gen_server for ETS ownership (standard OTP pattern)

### API Function Design

**From item.json:6** - Required exports:

1. **new/1**: Create new state store
   ```erlang
   -spec new(ctx()) -> {ok, state()}.
   new(InitialCtx) ->
       CaseId = make_ref(),
       State = #state{
           case_id = CaseId,
           ctx = InitialCtx,
           tokens = #{},
           scopes = #{root => #scope{scope_id = root, status = active}},
           metadata = #metadata{step_count = 0, start_time = timestamp()},
           buffered_mutations = []
       },
       ets:insert(wf_state_store, State),
       {ok, State}.
   ```

2. **get_ctx/1**, **put_ctx/2**: Context access
   ```erlang
   -spec get_ctx(state()) -> ctx().
   get_ctx(State) -> State#state.ctx.

   -spec put_ctx(state(), ctx()) -> {ok, state()}.
   put_ctx(State, NewCtx) ->
       Mutation = {set_ctx, NewCtx},
       NewState = buffer_mutation(State, Mutation),
       {ok, NewState}.
   ```

3. **add_token/3**, **remove_token/2**, **get_tokens/1**: Token management
   ```erlang
   -spec add_token(state(), token_id(), #token{}) -> {ok, state()}.
   add_token(State, TokenId, Token) ->
       Mutation = {add_token, TokenId, Token},
       NewState = buffer_mutation(State, Mutation),
       {ok, NewState}.

   -spec remove_token(state(), token_id()) -> {ok, state()}.
   remove_token(State, TokenId) ->
       Mutation = {remove_token, TokenId},
       NewState = buffer_mutation(State, Mutation),
       {ok, NewState}.

   -spec get_tokens(state()) -> #{token_id() => #token{}}.
   get_tokens(State) -> State#state.tokens.
   ```

4. **enter_scope/2**, **exit_scope/2**, **get_scope/2**: Scope management
   ```erlang
   -spec enter_scope(state(), scope_id()) -> {ok, state()}.
   enter_scope(State, ScopeId) ->
       ParentScope = get_current_scope(State),
       Mutation = {enter_scope, ScopeId, ParentScope},
       NewState = buffer_mutation(State, Mutation),
       {ok, NewState}.

   -spec exit_scope(state(), scope_id()) -> {ok, state()}.
   exit_scope(State, ScopeId) ->
       Mutation = {exit_scope, ScopeId},
       NewState = buffer_mutation(State, Mutation),
       {ok, NewState}.

   -spec get_scope(state(), scope_id()) -> #scope{} | undefined.
   get_scope(State, ScopeId) -> maps:get(ScopeId, State#state.scopes, undefined).
   ```

5. **buffer_mutation/2**: Buffer mutation during quantum
   ```erlang
   -spec buffer_mutation(state(), mutation()) -> state().
   buffer_mutation(State, Mutation) ->
       MutationRecord = #mutation{
           id = make_ref(),
           type = Mutation,
           timestamp = erlang:timestamp()
       },
       State#state{
           buffered_mutations = [MutationRecord | State#state.buffered_mutations]
       }.
   ```

6. **commit/1**: Atomic commit (buffer → validate → apply → receipt)
   ```erlang
   -spec commit(state()) ->
       {ok, state(), wf_receipt:receipt()} | {error, term()}.
   commit(State) ->
       Mutations = lists:reverse(State#state.buffered_mutations),
       case validate_mutations(Mutations, State) of
           ok ->
               AppliedState = apply_mutations(Mutations, State),
               ets:insert(wf_state_store, AppliedState),
               Receipt = create_receipt(Mutations, AppliedState),
               NewState = AppliedState#state{buffered_mutations = []},
               {ok, NewState, Receipt};
           {error, Reason} ->
               {error, Reason}
       end.
   ```

7. **rollback/1**: Discard buffered mutations
   ```erlang
   -spec rollback(state()) -> state().
   rollback(State) ->
       State#state{buffered_mutations = []}.
   ```

8. **snapshot/1**, **restore/2**: Snapshot/restore for debugging
   ```erlang
   -spec snapshot(state()) -> binary().
   snapshot(State) ->
       %% Serialize state to binary (for debugging/replay)
       term_to_binary(State).

   -spec restore(binary()) -> {ok, state()} | {error, term()}.
   restore(Binary) ->
       try binary_to_term(Binary) of
           State -> {ok, State}
       catch
           _:_ -> {error, invalid_snapshot}
       end.
   ```

### Integration with wf_exec

**Current wf_exec pattern** (from `/Users/speed/wf-substrate/src/wf_exec.erl:55-283`):
- State is inline in exec_state record
- Direct mutations: `ExecState#exec_state{ctx = NewCtx, ip = IP + 1}`

**Required refactoring**:
1. **Replace inline state with wf_state**:
   ```erlang
   -record(exec_state, {
       ip :: non_neg_integer(),
       bytecode :: wf_vm:wf_bc(),
       state :: wf_state:state(),  %% Delegated to wf_state
       status :: running | done | blocked
   }).
   ```

2. **Refactor opcode handlers**:
   ```erlang
   %% BEFORE (direct mutation)
   execute_task_exec(_Opcode, ExecState) ->
       {ok, NewCtx} = TaskFun(ExecState#exec_state.ctx),
       ExecState#exec_state{ctx = NewCtx, ip = IP + 1}.

   %% AFTER (buffered mutation)
   execute_task_exec(_Opcode, ExecState) ->
       {ok, NewCtx} = TaskFun(wf_state:get_ctx(ExecState#exec_state.state)),
       {ok, NewState} = wf_state:put_ctx(ExecState#exec_state.state, NewCtx),
       ExecState#exec_state{state = NewState, ip = IP + 1}.
   ```

3. **Commit at quantum boundaries**:
   ```erlang
   run_loop(ExecState0, Quanta, Count) when Count >= Quanta ->
       %% Quantum exhausted, commit mutations
       {ok, NewState, _Receipt} = wf_state:commit(ExecState0#exec_state.state),
       {yield, ExecState0#exec_state{state = NewState}};
   ```

### Receipt Integration

**From item 010** (wf_receipt - not implemented yet):

**Receipt structure** (from ARCHITECTURE.md:859-867):
```erlang
-record(receipt, {
    receipt_id :: receipt_id(),
    effect_id :: effect_id(),
    effect_spec_hash :: binary(),
    timestamp :: erlang:timestamp(),
    result :: {ok, term()} | {error, term()} | cancelled,
    duration_us :: non_neg_integer()
}).
```

**State store receipt extension**:
```erlang
-record(state_receipt, {
    receipt_id :: receipt_id(),
    case_id :: case_id(),
    mutations :: [mutation()],
    timestamp :: erlang:timestamp(),
    state_before_hash :: binary(),
    state_after_hash :: binary()
}).
```

**Receipt production**:
- Every commit produces receipt
- Receipt includes: mutations applied, timestamps, state hashes
- Receipts stored in append-only log (for audit/replay)

### Patterns to Follow

**From existing codebase**:

1. **Record-based state** (wf_exec.erl:55-66):
   - Use records for structured state
   - Document each field with type spec
   - Keep state immutable (functional updates)

2. **Accessor functions** (wf_exec.erl:105-127):
   - Provide getter/setter functions for each field
   - Hide implementation details (encapsulation)
   - Enable refactoring without breaking API

3. **Type specs** (wf_vm.erl:10-40):
   - Export types with `-export_type([...])`
   - Use `-type` for type aliases
   - Document types with comments

4. **Test patterns** (wf_exec_tests.erl:25-94):
   - Unit test each function in isolation
   - Use EUnit test generators
   - Test success and error paths

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **ETS table ownership complexity** | High | ETS table tied to process owner. If owner crashes, table deleted. Mitigation: Own ETS table in separate gen_server (wf_state_store) that survives case runner crashes. Document ownership clearly. |
| **Atomic commit implementation bugs** | High | Validate mutations before applying. If apply fails, rollback must be guaranteed. Mitigation: Use try-catch, ensure rollback path always succeeds. Add comprehensive tests for all failure modes. |
| **Mutation validation incomplete** | High | Invalid mutations could corrupt state. Mitigation: Comprehensive validation checks (token IDs unique, scope nesting consistent, context is map). Add property-based tests to verify invariants. |
| **Buffer overflow (too many mutations)** | Medium | Long-running quantum could buffer thousands of mutations. Mitigation: Add max_buffer_size limit. Force commit if buffer exceeds threshold. Document recommended quanta size. |
| **State serialization issues** | Medium | snapshot/restore uses term_to_binary/binary_to_term. May fail for complex terms (pids, refs). Mitigation: Filter out non-serializable fields (buffered_mutations, ets_table). Document snapshot limitations. |
| **Performance degradation (ETS writes)** | Medium | Every commit writes complete state snapshot to ETS. Large state = slow writes. Mitigation: Use ETS write_concurrency. Benchmark performance. Consider differential writes (delta-only) if needed. |
| **Memory leaks (buffered mutations not cleared)** | High | If commit/1 fails without rollback, buffered_mutations list grows unbounded. Mitigation: Ensure rollback clears buffer. Add buffer size monitoring in tests. Assert buffer empty after quantum. |
| **Race conditions (concurrent access)** | Medium | Multiple processes accessing same ETS table. Mitigation: Use ETS read_concurrency/write_concurrency. Document serialization semantics. Add tests for concurrent access. |
| **Receipt log growth** | Medium | Append-only receipt log grows unbounded. Mitigation: Implement receipt log rotation or archiving (future enhancement). For v1, accept growth and document as known limitation. |
| **Scope nesting validation bugs** | High | enter_scope/exit_scope must be properly nested. Mismatch causes corruption. Mitigation: Validate scope stack consistency. Track parent_scope references. Add tests for deeply nested scopes. |
| **Token lifecycle bugs** | High | Tokens must be added before referenced, removed after completion. Mitigation: Validate token existence in update_token/remove_token. Track token status (active/complete/cancelled). Add property tests for token invariants. |
| **Integration with wf_exec complexity** | High | Refactoring wf_exec to use wf_state is large change. Mitigation: Incremental migration. First, implement wf_state standalone. Second, add wf_state as optional backend. Third, make wf_state required. Run tests at each step. |
| **wf_receipt not implemented yet** | Medium | State store depends on wf_receipt for receipt generation. Mitigation: Stub wf_receipt functions for v1. Use simple record structure. Add TODO to integrate with item 010 when implemented. |
| **Test coverage gaps** | Medium | State store has 13 exported functions, many edge cases. Mitigation: Comprehensive unit tests for each function. Property-based tests for invariants (buffer size, token count, scope depth). Integration tests with wf_exec. |

## Recommended Approach

**High-Level Strategy**: Implement wf_state standalone first (independent of wf_exec), using mock wf_receipt for receipt generation. Then incrementally integrate with wf_exec by adding state store as optional backend, then making it required. Use ETS table owned by separate gen_server for crash resilience.

### Phase 1: Foundation (types, records, new/1)

**1. Define types and records** (`src/wf_state.erl`):
```erlang
%% State record
-record(state, {
    case_id :: case_id(),
    ctx :: ctx(),
    tokens :: #{token_id() => #token{}},
    scopes :: #{scope_id() => #scope{}},
    metadata :: #metadata{},
    buffered_mutations :: [#mutation{}],
    ets_table :: ets:tid() | undefined
}).

%% Mutation record
-record(mutation, {
    id :: mutation_id(),
    type :: mutation(),
    timestamp :: erlang:timestamp()
}).

%% Scope record
-record(scope, {
    scope_id :: scope_id(),
    parent_scope :: scope_id() | undefined,
    status :: active | cancelled,
    tokens :: [token_id()],
    entered_at :: erlang:timestamp()
}).

%% Metadata record
-record(metadata, {
    step_count :: non_neg_integer(),
    start_time :: erlang:timestamp(),
    last_commit_time :: erlang:timestamp() | undefined
}).

%% Type exports
-export_type([
    state/0,
    mutation/0,
    ctx/0,
    token_id/0,
    scope_id/0,
    case_id/0
]).
```

**2. Implement new/1**:
```erlang
-spec new(ctx()) -> {ok, state()}.
new(InitialCtx) ->
    CaseId = make_ref(),
    RootScope = #scope{
        scope_id = root,
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
        scopes = #{root => RootScope},
        metadata = Metadata,
        buffered_mutations = [],
        ets_table = undefined
    },
    ets:insert(wf_state_store, State),
    {ok, State}.
```

**3. Create ETS table**:
```erlang
%% gen_server init
init([]) ->
    ets:new(wf_state_store, [
        named_table,
        set,
        {keypos, #state.case_id},
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    {ok, #{}}.
```

**4. Implement basic accessors**:
- get_ctx/1
- get_tokens/1
- get_scope/2

**5. Test basic state creation and access**

### Phase 2: Buffering and Mutations (buffer_mutation/2)

**1. Implement mutation type**:
```erlang
-type mutation() ::
    {set_ctx, ctx()} |
    {update_ctx, fun((ctx()) -> ctx())} |
    {add_token, token_id(), #token{}} |
    {remove_token, token_id()} |
    {update_token, token_id(), fun((#token{}) -> #token{})} |
    {enter_scope, scope_id(), scope_id()} |
    {exit_scope, scope_id()} |
    {cancel_scope, scope_id()} |
    {increment_step_count} |
    {set_metadata, term()}.
```

**2. Implement buffer_mutation/2**:
```erlang
-spec buffer_mutation(state(), mutation()) -> state().
buffer_mutation(State, Mutation) ->
    MutationRecord = #mutation{
        id = make_ref(),
        type = Mutation,
        timestamp = erlang:timestamp()
    },
    State#state{
        buffered_mutations = [MutationRecord | State#state.buffered_mutations]
    }.
```

**3. Implement put_ctx/2** (uses buffering):
```erlang
-spec put_ctx(state(), ctx()) -> {ok, state()}.
put_ctx(State, NewCtx) ->
    Mutation = {set_ctx, NewCtx},
    NewState = buffer_mutation(State, Mutation),
    {ok, NewState}.
```

**4. Test mutation buffering**

### Phase 3: Atomic Commit (commit/1)

**1. Implement validate_mutations/2**:
```erlang
validate_mutations(Mutations, State) ->
    %% Validate token uniqueness
    %% Validate scope nesting
    %% Validate context type
    %% Validate token existence before update/remove
    ok.
```

**2. Implement apply_mutations/2**:
```erlang
apply_mutations(Mutations, State) ->
    lists:foldl(fun(Mutation, AccState) ->
        apply_single_mutation(Mutation, AccState)
    end, State, Mutations).
```

**3. Implement apply_single_mutation/2** (one clause per mutation type):
```erlang
apply_single_mutation(#mutation{type = {set_ctx, NewCtx}}, State) ->
    State#state{ctx = NewCtx};

apply_single_mutation(#mutation{type = {add_token, TokenId, Token}}, State) ->
    Tokens = State#state.tokens,
    State#state{tokens = Tokens#{TokenId => Token}};

apply_single_mutation(#mutation{type = {enter_scope, ScopeId, ParentScope}}, State) ->
    Scopes = State#state.scopes,
    NewScope = #scope{
        scope_id = ScopeId,
        parent_scope = ParentScope,
        status = active,
        tokens = [],
        entered_at = erlang:timestamp()
    },
    State#state{scopes = Scopes#{ScopeId => NewScope}}.

%% ...more handlers...
```

**4. Implement commit/1**:
```erlang
-spec commit(state()) -> {ok, state(), receipt()} | {error, term()}.
commit(State) ->
    Mutations = lists:reverse(State#state.buffered_mutations),
    case validate_mutations(Mutations, State) of
        ok ->
            AppliedState = apply_mutations(Mutations, State),
            ets:insert(wf_state_store, AppliedState),
            Receipt = create_receipt(Mutations, State, AppliedState),
            NewState = AppliedState#state{buffered_mutations = []},
            {ok, NewState, Receipt};
        {error, Reason} ->
            {error, Reason}
    end.
```

**5. Implement rollback/1**:
```erlang
-spec rollback(state()) -> state().
rollback(State) ->
    State#state{buffered_mutations = []}.
```

**6. Test commit and rollback**

### Phase 4: Token and Scope Management

**1. Implement token functions**:
- add_token/3
- remove_token/2
- update_token/3 (internal)

**2. Implement scope functions**:
- enter_scope/2
- exit_scope/2
- get_scope/2
- cancel_scope/2 (internal)

**3. Test token lifecycle**:
- Add token → commit → token exists
- Remove token → commit → token removed
- Update token → commit → token updated
- Duplicate token ID → validation error

**4. Test scope management**:
- Enter scope → commit → scope exists
- Exit scope → commit → scope removed
- Nested scopes → parent references correct
- Cancel scope → tokens marked cancelled

### Phase 5: Snapshot and Restore

**1. Implement snapshot/1**:
```erlang
-spec snapshot(state()) -> binary().
snapshot(State) ->
    %% Filter out non-serializable fields
    CleanState = State#state{
        buffered_mutations = [],  %% Don't snapshot buffered mutations
        ets_table = undefined      %% Don't snapshot ETS reference
    },
    term_to_binary(CleanState).
```

**2. Implement restore/2**:
```erlang
-spec restore(binary()) -> {ok, state()} | {error, term()}.
restore(Binary) ->
    try binary_to_term(Binary) of
        State -> {ok, State}
    catch
        _:_ -> {error, invalid_snapshot}
    end.
```

**3. Test snapshot/restore**:
- Snapshot state → restore → state matches
- Restore invalid binary → error
- Snapshot with buffered mutations → mutations discarded

### Phase 6: Receipt Integration (Stub)

**1. Create wf_receipt stub** (item 010 not implemented):
```erlang
%% Simple receipt record for now
-record(receipt, {
    receipt_id :: term(),
    case_id :: case_id(),
    mutations :: [mutation()],
    timestamp :: erlang:timestamp()
}).

%% Stub functions
create_receipt(Mutations, StateBefore, StateAfter) ->
    #receipt{
        receipt_id = make_ref(),
        case_id = StateAfter#state.case_id,
        mutations = Mutations,
        timestamp = erlang:timestamp()
    }.
```

**2. Integrate receipt generation into commit/1**

**3. Test receipt generation**

**4. TODO**: Integrate with proper wf_receipt module (item 010)

### Phase 7: ETS Persistence

**1. Create wf_state_store gen_server**:
```erlang
-module(wf_state_store).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(wf_state_store, [
        named_table,
        set,
        {keypos, #state.case_id},
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    {ok, #{}}.
```

**2. Update wf_state to use ETS**:
- Write state on commit/1
- Read state on crash recovery
- Delete state on case completion

**3. Test crash recovery**:
- Start case → commit mutations → simulate crash
- Restart case → restore state from ETS
- Assert state matches pre-crash state

### Phase 8: Integration with wf_exec

**1. Refactor exec_state record**:
```erlang
%% BEFORE
-record(exec_state, {
    ip,
    ctx,
    tokens,
    ...
}).

%% AFTER
-record(exec_state, {
    ip,
    bytecode,
    state :: wf_state:state(),  %% Delegated
    status
}).
```

**2. Refactor accessor functions**:
- `wf_exec:get_ctx/1` → `wf_state:get_ctx/1`
- `wf_exec:set_ctx/2` → `wf_state:put_ctx/2`
- etc.

**3. Refactor opcode handlers**:
- Replace direct mutations with wf_state calls
- Example: `ExecState#exec_state{ctx = NewCtx}` → `wf_state:put_ctx(State, NewCtx)`

**4. Add commit at quantum boundaries**:
- In `wf_exec:run/3`, commit after Quanta steps
- Handle commit errors (rollback)

**5. Migrate tests**:
- Update `wf_exec_tests` to use wf_state API
- Add state store tests to existing test suite

**6. Test integration**:
- Run all wf_exec tests with wf_state backend
- Assert execution produces same results as before
- Measure performance overhead

### Phase 9: Comprehensive Testing

**1. Unit tests**:
- Test each wf_state function in isolation
- Test mutation validation (valid/invalid cases)
- Test atomic commit (all-or-nothing)
- Test rollback (buffer cleared)
- Test snapshot/restore

**2. Property-based tests**:
- Generate random mutation sequences
- Assert invariants preserved:
  - Token IDs unique
  - Scope nesting consistent
  - Context is always map
  - Step count non-negative
- Assert commit produces receipt
- Assert rollback discards mutations

**3. Integration tests**:
- Test wf_exec with wf_state backend
- Test complete workflows (seq, par, xor, loop)
- Test crash recovery (ETS persistence)

**4. Performance tests**:
- Measure commit latency (time to apply N mutations)
- Measure ETS write throughput
- Measure memory usage (state size + buffer)
- Profile hot paths

## Open Questions

1. **wf_receipt Integration**: Should state store implement its own receipt structure or wait for item 010?
   - **Recommendation**: Implement stub receipt record for v1. Add TODO to integrate with wf_receipt when item 010 is complete. Receipt stub should be compatible with future wf_receipt structure.

2. **ETS Table Ownership**: Should ETS table be owned by wf_state gen_server or separate wf_state_store gen_server?
   - **Recommendation**: Separate wf_state_store gen_server. Keeps table alive even if all state processes crash. Standard OTP pattern for long-lived ETS tables.

3. **State Serialization Format**: Should snapshot/1 use term_to_binary or custom format?
   - **Recommendation**: Use term_to_binary for v1 (simple, works for most cases). Add TODO for custom format if complex terms (pids, ports) cause issues. Document that ETS table reference not serialized.

4. **Mutation Buffer Limit**: Should there be max_buffer_size limit?
   - **Recommendation**: No limit for v1 (trust quanta sizing). Add warning if buffer > 1000 mutations. Future enhancement: force commit if buffer exceeds threshold.

5. **Validation Strictness**: How strict should validate_mutations/2 be?
   - **Recommendation**: Strict validation for v1 (check all invariants). Catch errors early rather than allowing corrupt state. Validation errors should cause rollback with clear error message.

6. **Scope Stack Tracking**: Should state maintain explicit scope stack or infer from parent_scope links?
   - **Recommendation**: Maintain explicit scope stack in state (separate from parent_scope links). Enables O(1) "current scope" lookup. parent_scope links used for validation/debugging.

7. **Token Status Tracking**: Should token status be in token record or separate map?
   - **Recommendation**: Keep status in token record (current pattern from wf_exec). Enables O(1) status lookup. Filter tokens by status when needed.

8. **Commit Frequency**: How often should executor commit state?
   - **Recommendation**: Commit after each quantum (N reductions). Default quanta = 100. Configurable via options. Commits too frequent = overhead; too rare = crash recovery gap.

9. **Concurrent Access**: Should state store support concurrent reads/writes from multiple processes?
   - **Recommendation**: Support concurrent reads (ETS read_concurrency). Serialize writes (gen_server call). For v1, single writer (case runner) is typical case. Document serialization semantics.

10. **State Size Monitoring**: Should state store monitor memory usage?
    - **Recommendation**: Add state_size/1 utility function (returns byte size). For v1, monitoring is external responsibility (OS tools, observer). Future enhancement: add telemetry/limits.

11. **Receipt Storage**: Where should receipts be stored?
    - **Recommendation**: Use wf_receipt:store/1 when item 10 implemented. For v1, store receipts in ETS table (wf_receipts). Separate table from state store.

12. **Backwards Compatibility**: How to handle old state formats after schema changes?
    - **Recommendation**: Use state version field. Add migration logic in restore/2. For v1, assume version 1.0. Document versioning strategy.

13. **Error Handling in apply_mutations/2**: What if one mutation fails mid-apply?
    - **Recommendation**: Wrap apply_mutations in try-catch. If exception, rollback completely (return original state). Log error for debugging. Commit fails atomically.

14. **Test Double Strategy**: How to test wf_state without wf_exec?
    - **Recommendation**: Standalone unit tests with direct wf_state API. Mock wf_receipt. Use mock mutations. No dependency on wf_exec for core functionality tests.

15. **Performance Baseline**: What is acceptable commit latency?
    - **Recommendation**: Target < 1ms for 100 mutations. Benchmark during implementation. If slower, investigate ETS write overhead or mutation application logic. Optimize hot paths.
