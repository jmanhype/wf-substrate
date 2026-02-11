# Research: Implement multiple instance patterns

**Date**: 2025-01-10
**Item**: 009-multiple-instance-support

## Research Question

Implement wf_mi.erl: multiple instance patterns from the workflow pattern catalog.

Instance creation modes:
1. Fixed count: N instances determined at compile time (mi/2 with {fixed, N} config).
2. Runtime count: N determined at runtime from ctx() (mi/2 with {dynamic, Fun} config where Fun :: ctx() -> pos_integer()).
3. With synchronization barrier: all instances must complete before continuation (join policy: wait_all).
4. Without synchronization: instances are fire-and-forget, continuation proceeds immediately.

Join policies for MI completion:
- wait_all: block until every instance completes.
- wait_n: block until N of M instances complete, cancel remainder.
- first_complete: block until first instance completes, cancel all others (discriminator pattern for MI).

Instance spawning: each instance gets a unique instance_id, its own token, and a copy of the body bytecode (or shared reference with instance-local state). Instance collection: completed instances report results to a collector that evaluates the join policy.

Integrated with wf_exec: MI_SPAWN opcode creates instance entries in the branch map, JOIN_WAIT handles MI join policies. Integrates with wf_cancel for cancelling remaining instances when join is satisfied.

Export: spawn_instances/4, collect_result/3, check_join/2, cancel_remaining/2.

## Summary

This task involves implementing `wf_mi.erl`, a multiple instance (MI) pattern system that supports spawning multiple concurrent instances of a workflow fragment with configurable synchronization semantics. Multiple instance patterns are a fundamental workflow pattern (WP16 - Multi-Instance) from the workflow pattern catalog, enabling parallel execution of the same activity/sequence with different data contexts.

The implementation requires four instance creation modes (fixed count, runtime dynamic count, with sync barrier, without sync) and three join policies (wait_all, wait_n, first_complete). Each instance gets a unique ID, its own token, and executes the same body bytecode. Instances report results to a collector that evaluates join policies and decides whether to cancel remaining instances.

**Key Architectural Context**:
- This is **item 009** in the implementation sequence
- Builds on existing parallel execution infrastructure (`PAR_FORK`/`JOIN_WAIT` in wf_exec)
- Extends the `branch_map` concept to track multiple instances
- Reuses `join_counter` records for MI join policies
- Integrates with `wf_cancel` (item 008) for cancelling remaining instances
- No external dependencies (pure Erlang/OTP)

**Current State**: The executor has `PAR_FORK` (spawn N branches to different IPs) and `JOIN_WAIT` (wait for branch completion) infrastructure. However, this spawns different branches to different target IPs. Multiple instance patterns need to spawn N instances of the SAME body bytecode at the same IP, with per-instance state. The `mi_policy()` type is already defined in `wf_vm.erl:38-40` but no handler exists yet.

## Current State Analysis

### Existing Implementation

**1. Parallel Execution Infrastructure** (`/Users/speed/wf-substrate/src/wf_exec.erl:259-323`):

The executor already has parallel fork/join support:

```erlang
%% @doc Execute PAR_FORK: spawn N tokens, track in branch_map
execute_par_fork({_ParFork, TargetIPs}, ExecState) ->
    BranchId = make_ref(),
    JoinId = make_ref(),
    NumBranches = length(TargetIPs),

    %% Spawn N tokens (each to different IP)
    NewTokens = lists:map(fun(IP) ->
        TokenId = make_ref(),
        Token = #token{
            token_id = TokenId,
            ip = IP,              %% DIFFERENT IPs for PAR_FORK
            scope_id = ScopeId,
            value = undefined,
            status = active
        },
        {TokenId, Token}
    end, TargetIPs),

    %% Create branch info
    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = [TokenId || {TokenId, _Token} <- NewTokens],
        join_id = JoinId,
        targets = TargetIPs
    },
    BranchMap = maps:put(BranchId, BranchInfo, BranchMap0),

    %% Create join counter
    JoinCounter = #join_counter{
        join_id = JoinId,
        completed = 0,
        required = NumBranches,
        policy = all,               %% PAR_FORK always uses 'all'
        results = []
    },
    JoinCounters = maps:put(JoinId, JoinCounter, JoinCounters0),
    ...
```

**Analysis**:
- `PAR_FORK` spawns tokens to **different target IPs** (parallel branches)
- Each branch is a different workflow fragment
- Join policy is hardcoded to `all` (wait for all branches)
- MI patterns need to spawn instances to the **same IP** (same body, different data)

**2. Join Counter Infrastructure** (`/Users/speed/wf-substrate/src/wf_exec.erl:44-51, 325-363, 381-393`):

```erlang
-record(join_counter, {
    join_id :: term(),
    completed :: non_neg_integer(),
    required :: non_neg_integer(),
    policy :: wf_vm:join_policy(),      %% all | first_n | n_of_m | first_complete | sync_merge
    results :: [term()]
}).

%% Join wait handler
execute_join_wait({_JoinWait, Policy}, ExecState) ->
    JoinId = find_active_join(ExecState),
    JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),

    case JoinCounter#join_counter.completed >= JoinCounter#join_counter.required of
        true ->
            %% Join satisfied, merge results
            ...
        false ->
            %% Block until join satisfied
            ExecState#exec_state{status = blocked_join, ...}
    end.

%% Result merging
merge_results(Results, all) ->
    {merged, Results};
merge_results(Results, {first_n, _N}) ->
    {merged, Results};
merge_results(Results, first_complete) ->
    {first_complete, hd(Results)};
merge_results(Results, sync_merge) ->
    {sync_merged, lists:foldl(fun merge_state/2, #{}, Results)}.
```

**Analysis**:
- Join policies already support: `all`, `first_n`, `first_complete`, `sync_merge`
- MI join policies (`wait_all`, `wait_n`) can map to existing policies
- `wait_all` → `all` policy
- `wait_n` → `{first_n, N}` policy
- `first_complete` → `first_complete` policy
- **Missing**: Infrastructure to cancel remaining instances when `wait_n` or `first_complete` satisfied

**3. Branch Tracking** (`/Users/speed/wf-substrate/src/wf_exec.erl:36-42, 575-597`):

```erlang
-record(branch_info, {
    branch_id :: term(),
    tokens :: [term()],           %% List of token IDs in this branch
    join_id :: term(),            %% Associated join counter
    targets :: [non_neg_integer()] %% Target IPs (for PAR_FORK)
}).

%% Find branch for a token
find_branch_for_token(TokenId, BranchMap) ->
    case [BranchId || {BranchId, #branch_info{tokens = Tokens}} <- maps:to_list(BranchMap),
                      lists:member(TokenId, Tokens)] of
        [BranchId | _] -> {ok, BranchId};
        [] -> error
    end.

%% Increment join counter when token completes
increment_join_counter(BranchId, ExecState, ResultValue) ->
    BranchInfo = maps:get(BranchId, ExecState#exec_state.branch_map),
    JoinId = BranchInfo#branch_info.join_id,

    JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),
    UpdatedJoinCounter = JoinCounter#join_counter{
        completed = JoinCounter#join_counter.completed + 1,
        results = [ResultValue | JoinCounter#join_counter.results]
    },
    maps:put(JoinId, UpdatedJoinCounter, ExecState#exec_state.join_counters).
```

**Analysis**:
- Branches track token IDs and associated join
- When token completes, executor finds its branch and increments join counter
- **Reusable for MI**: MI instances can use same branch/join infrastructure
- **Extension needed**: Track `instance_id` for each token

**4. MI Policy Type** (`/Users/speed/wf-substrate/src/wf_vm.erl:18, 37-40`):

```erlang
-type opcode() ::
    ...
    {atom(), mi_policy()} |                   %% MI_SPAWN with policy
    ...

%% Multiple instance policies
-type mi_policy() ::
    {fixed, pos_integer()} |                 %% Fixed N instances
    {dynamic, pos_integer(), pos_integer()}.  %% {min, max} runtime range
```

**Analysis**:
- `mi_policy()` type already defined in wf_vm
- `MI_SPAWN` opcode type already declared
- **Missing**: `execute_mi_spawn` handler in wf_exec.erl
- **Missing**: wf_mi.erl module implementation

**5. Cancellation Infrastructure** (`/Users/speed/wf-substrate/src/wf_exec.erl:519-529, 008-cancellation-semantics/research.md:26-1365`):

From item 008 research (cancellation semantics):

```erlang
%% Stub functions (to be replaced with wf_cancel calls)
is_scope_cancelled(_ScopeId, _ExecState) ->
    false.

propagate_cancellation(ScopeId, TokensMap) ->
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).
```

**Analysis**:
- Cancellation marks tokens as `cancelled` status
- wf_cancel (item 008) provides `cancel_activity/3`, `cancel_case/1`, `cancel_region/3`
- **For MI**: Need to cancel remaining instances when join satisfied (wait_n, first_complete)
- Can use wf_cancel to cancel individual instance tokens
- Or use internal token cancellation (set status to cancelled)

### Integration Points

**1. wf_exec (executor) - Primary Integration**:

Current opcode dispatch (`/Users/speed/wf-substrate/src/wf_exec.erl:186-208`):

```erlang
execute_opcode({ParFork, Targets}, ExecState) when ParFork =:= 'PAR_FORK'; ParFork =:= par_fork ->
    execute_par_fork({ParFork, Targets}, ExecState);
execute_opcode({JoinWait, _Policy}, ExecState) when JoinWait =:= 'JOIN_WAIT'; JoinWait =:= join_wait ->
    execute_join_wait({JoinWait, _Policy}, ExecState);
...
execute_opcode(_Opcode, _ExecState) ->
    error({unimplemented_opcode, _Opcode}).
```

**Missing**:
```erlang
execute_opcode({MiSpawn, Policy}, ExecState) when MiSpawn =:= 'MI_SPAWN'; MiSpawn =:= mi_spawn ->
    execute_mi_spawn({MiSpawn, Policy}, ExecState),
```

**2. wf_cancel (cancellation) - Secondary Integration**:

From item 008 research, wf_cancel provides:
- `cancel_activity/3` - Cancel single task (can cancel instance tokens)
- `cancel_case/1` - Cancel entire case
- `cancel_region/3` - Cancel scoped region

**For MI**:
- When `wait_n` satisfied, cancel remaining M-N instances
- When `first_complete` satisfied, cancel all other instances
- Use `wf_cancel:cancel_activity/3` to cancel each remaining token
- Or directly set token status to `cancelled` (simpler, internal)

**3. wf_state (state store) - Instance Tracking**:

Current state (`/Users/speed/wf-substrate/src/wf_state.erl:117-127`):

```erlang
-record(state, {
    case_id :: case_id(),
    ctx :: ctx(),
    tokens :: #{token_id() => #token{}},    %% Already tracks tokens
    scopes :: #{scope_id() => #scope{}},
    metadata :: #metadata{},
    status :: running | cancelled | done | failed,
    buffered_mutations :: [#mutation{}],
    ets_table :: ets:tid() | undefined
}).
```

**Analysis**:
- State already tracks tokens (can track instance tokens)
- **No changes needed**: MI instances are just tokens with instance_id metadata
- Instance metadata stored in token `value` field or new `instance_id` field

### Specification Context

**From PROMPT.md:9, 58, 90, 118**:

```
Module: wf_mi.erl
- multiple instance semantics (fixed, dynamic)

Types:
- instance_id() identifies MI instances
- mi_policy() = {fixed, N} | {dynamic, Min, Max}

Pattern Constructors:
- mi(Policy, P) - multiple instance wrapper

Semantics:
- mi spawns N instances of P, each with unique instance_id
- Instances execute concurrently (each gets a token)
- Join policy determines when MI completes (wait_all/wait_n/first_complete)
```

**From item.json:6** (item 009 specification):

```
Instance creation modes:
1. Fixed count: N instances determined at compile time (mi/2 with {fixed, N} config).
2. Runtime count: N determined at runtime from ctx() (mi/2 with {dynamic, Fun} config where Fun :: ctx() -> pos_integer()).
3. With synchronization barrier: all instances must complete before continuation (join policy: wait_all).
4. Without synchronization: instances are fire-and-forget, continuation proceeds immediately.

Join policies for MI completion:
- wait_all: block until every instance completes.
- wait_n: block until N of M instances complete, cancel remainder.
- first_complete: block until first instance completes, cancel all others (discriminator pattern for MI).

Instance spawning: each instance gets a unique instance_id, its own token, and a copy of the body bytecode (or shared reference with instance-local state). Instance collection: completed instances report results to a collector that evaluates the join policy.
```

**Workflow Pattern Context** (from van der Aalst workflow patterns):

**WP16 - Multi-Instance**:
- **Multi-Instance (Synchronous)**: N instances execute concurrently, synchronization barrier waits for all
- **Multi-Instance (Asynchronous)**: N instances execute concurrently, no synchronization (fire-and-forget)
- **Multi-Instance with N-out-of-M**: Wait for N of M instances, cancel remainder
- **Discriminator (Multi-Instance variant)**: Wait for first completion, cancel others

## Key Files

### Existing Files

- `/Users/speed/wf-substrate/src/wf_exec.erl:259-323` - **PAR_FORK handler**
  - Lines 259-323: `execute_par_fork/2` spawns parallel branches to different IPs
  - **Pattern to follow**: Branch spawning, join counter creation, branch map tracking
  - **Difference**: MI spawns instances to same IP (not different IPs)

- `/Users/speed/wf-substrate/src/wf_exec.erl:325-393` - **JOIN_WAIT handler and result merging**
  - Lines 325-363: `execute_join_wait/2` blocks until join satisfied
  - Lines 381-393: `merge_results/2` merges results per policy
  - **Reusable for MI**: MI join policies map to existing join policies

- `/Users/speed/wf-substrate/src/wf_exec.erl:36-42, 575-597` - **Branch and join tracking**
  - Lines 36-42: `#branch_info{}` record definition
  - Lines 575-582: `find_branch_for_token/2` finds branch for token
  - Lines 584-597: `increment_join_counter/3` updates join counter on completion
  - **Reusable**: MI instances use same infrastructure

- `/Users/speed/wf-substrate/src/wf_exec.erl:28-34` - **Token record**
  - Lines 28-34: `#token{}` record with token_id, ip, scope_id, value, status
  - **Extension needed**: Add `instance_id` field to track MI instances

- `/Users/speed/wf-substrate/src/wf_vm.erl:18, 37-40` - **MI policy type definition**
  - Lines 18: Opcode type includes `{atom(), mi_policy()}`
  - Lines 37-40: `mi_policy()` type defined as `{fixed, N} | {dynamic, Min, Max}`
  - **Already defined**, no changes needed

- `/Users/speed/wf-substrate/src/wf_exec.erl:186-208` - **Opcode dispatch**
  - Lines 186-208: `execute_opcode/2` dispatches to opcode handlers
  - **Missing**: `execute_mi_spawn/2` handler (throws `unimplemented_opcode`)

- `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/research.md:26-1365` - **Cancellation semantics**
  - Lines 26-1365: Complete research on wf_cancel implementation
  - **Integration**: Use wf_cancel to cancel remaining MI instances when join satisfied

### Specification Files

- `/Users/speed/wf-substrate/.wreckit/items/009-multiple-instance-support/item.json:1-14` - **Item specification**
  - Lines 6-7: Four instance creation modes (fixed, dynamic, with sync, without sync)
  - Lines 6-7: Three join policies (wait_all, wait_n, first_complete)
  - Lines 6-7: Instance spawning (unique instance_id, token, bytecode copy)
  - Lines 6-7: Integration with wf_exec (MI_SPAWN opcode), wf_cancel (cancel remaining)
  - Lines 6-7: Export requirements: spawn_instances/4, collect_result/3, check_join/2, cancel_remaining/2

- `/Users/speed/wf-substrate/PROMPT.md:9, 58, 90, 118` - **Module specification**
  - Lines 9: wf_mi.erl in module list
  - Lines 58: mi(Policy, P) pattern constructor
  - Lines 90: Multiple instance patterns in deliverable requirements
  - Lines 118: mi spawns N instances with unique instance_id

### Files to Create

- `src/wf_mi.erl` - **Multiple instance semantics module** (primary deliverable)
  - `-type mi_join_policy()` - wait_all, wait_n, first_complete
  - `-type mi_instance()` - Instance metadata (instance_id, token_id, status, result)
  - `-record(mi_config)` - MI configuration (policy, join_policy, body_ip, scope_id)
  - `spawn_instances/4` - Spawn N instances from MI config
  - `collect_result/3` - Collect instance result and update join state
  - `check_join/2` - Check if join policy satisfied
  - `cancel_remaining/2` - Cancel remaining instances when join satisfied
  - `eval_instance_count/2` - Evaluate instance count (fixed or dynamic)

- `src/wf_exec.erl` - **Add MI_SPAWN handler**
  - Add `execute_mi_spawn/2` function
  - Add `instance_id` field to `#token{}` record (optional, can use value field)
  - Update `execute_opcode/2` to dispatch MI_SPAWN
  - Reuse branch/join infrastructure for instance tracking

- `test/wf_mi_tests.erl` - **Multiple instance tests**
  - Test fixed count MI (N instances spawned, all complete)
  - Test dynamic count MI (runtime evaluation)
  - Test MI with wait_all (synchronization barrier)
  - Test MI without sync (fire-and-forget)
  - Test MI with wait_n (N of M complete, remainder cancelled)
  - Test MI with first_complete (first wins, others cancelled)
  - Test instance result collection and merging
  - Test integration with wf_exec
  - Test integration with wf_cancel (cancelling remaining instances)

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Modules Needed**:
- `lists` - For instance spawning and result collection
- `maps` - For instance tracking (instance_id -> instance metadata)
- `make_ref()` - For generating unique instance IDs

**Internal Module Dependencies**:
- **wf_exec (item 005)**: Executor that runs MI_SPAWN opcode
  - wf_exec calls `wf_mi:spawn_instances/4` in MI_SPAWN handler
  - wf_exec calls `wf_mi:collect_result/3` when instance token completes
  - wf_exec calls `wf_mi:check_join/2` in JOIN_WAIT handler
  - wf_exec provides instance tracking (tokens, branch_map, join_counters)

- **wf_cancel (item 008)**: Cancellation for remaining instances
  - wf_mi calls `wf_cancel:cancel_activity/3` to cancel remaining instances
  - Or wf_mi directly sets token status to cancelled (internal cancellation)
  - **Recommendation**: Internal cancellation (simpler, no wf_cancel dependency for v1)

- **wf_state (item 006)**: State persistence (optional for v1)
  - MI instances tracked in exec_state (in-memory)
  - wf_state persists tokens if needed for crash recovery
  - **No direct dependency** for v1 (wf_exec manages instance lifecycle)

- **wf_vm (item 004)**: Bytecode types
  - wf_vm defines `mi_policy()` type
  - wf_vm defines `MI_SPAWN` opcode type
  - **No changes needed** to wf_vm (types already defined)

**No Circular Dependencies**: wf_mi depends on wf_exec (executor) and wf_cancel (cancellation). These are foundational modules. Safe to implement independently.

### MI Spawning Strategy

**Key Difference from PAR_FORK**:

PAR_FORK (`wf_exec.erl:260-278`):
```erlang
%% Spawn N tokens to DIFFERENT IPs
NewTokens = lists:map(fun(IP) ->
    TokenId = make_ref(),
    Token = #token{
        token_id = TokenId,
        ip = IP,              %% DIFFERENT IPs
        ...
    }
end, TargetIPs).
```

MI_SPAWN (proposed):
```erlang
%% Spawn N tokens to SAME IP (body), with different instance_id
NewTokens = lists:map(fun(InstanceNum) ->
    TokenId = make_ref(),
    InstanceId = {instance, make_ref()},
    Token = #token{
        token_id = TokenId,
        ip = BodyIP,         %% SAME IP for all instances
        instance_id = InstanceId,  %% NEW FIELD: unique per instance
        value = #{instance_num => InstanceNum, instance_id => InstanceId},
        ...
    }
end, lists:seq(1, N)).
```

**Bytecode Sharing**:
- **Option 1**: All instances share bytecode reference (memory efficient)
- **Option 2**: Each instance gets bytecode copy (isolated state)
- **Recommendation**: Option 1 (shared bytecode). Instances have separate `ctx()` and `value` fields, so state is isolated. Bytecode is read-only at runtime.

**Instance ID Generation**:
```erlang
%% Unique instance ID
InstanceId = {mi_instance, make_ref()},

%% Or sequential with scope uniqueness
InstanceId = {ScopeId, InstanceNum},
```

**Recommendation**: Use `{instance, make_ref()}` for uniqueness. Sequential IDs require scope tracking (more complex).

### Instance State Tracking

**Token Extension** (`wf_exec.erl:28-34`):

Current token record:
```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled
}).
```

**Option 1**: Add `instance_id` field
```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled,
    instance_id :: term() | undefined  %% NEW FIELD
}).
```

**Option 2**: Use `value` field for instance metadata
```erlang
Token = #token{
    ...
    value = #{instance_num => N, instance_id => InstanceId, mi_config => Config}
}
```

**Recommendation**: Option 1 (add field). Cleaner separation of concerns. `value` is for user data, `instance_id` is for system metadata.

### Join Policy Mapping

**MI Join Policies** → **Existing wf_exec Join Policies**:

| MI Join Policy | wf_exec Join Policy | Behavior |
| -------------- | ------------------ | --------- |
| `wait_all` | `all` | Block until all M instances complete |
| `wait_n` | `{first_n, N}` | Block until N of M complete, cancel remainder |
| `first_complete` | `first_complete` | Block until first completes, cancel others |

**Implementation**:
```erlang
%% In wf_mi:spawn_instances/4
JoinPolicy = case MIJoinPolicy of
    wait_all -> all;
    {wait_n, N} -> {first_n, N};
    first_complete -> first_complete
end,

JoinCounter = #join_counter{
    join_id = JoinId,
    completed = 0,
    required = RequiredInstances,  %% M for wait_all, N for wait_n/first_complete
    policy = JoinPolicy,
    results = []
}
```

**Fire-and-Forget** (no synchronization):
- Do not create join counter
- Do not block executor
- Instances run independently
- **Use case**: Logging, notification, side-effect-only workflows

### Dynamic Instance Count Evaluation

**From item.json:6**:
> "Runtime count: N determined at runtime from ctx() (mi/2 with {dynamic, Fun} config where Fun :: ctx() -> pos_integer())."

**Implementation**:
```erlang
%% In wf_mi:eval_instance_count/2
eval_instance_count({fixed, N}, _Ctx) ->
    {ok, N};

eval_instance_count({dynamic, Min, Max}, Ctx) when is_function(Fun) ->
    %% Call user function with context
    case Fun(Ctx) of
        N when N >= Min, N =< Max ->
            {ok, N};
        N when N < Min ->
            {error, {below_minimum, N, Min}};
        N when N > Max ->
            {error, {above_maximum, N, Max}}
    end.
```

**When to Evaluate**:
- At **compile time** (when wf_mi:mi/2 is called): Fixed count
- At **runtime** (when MI_SPAWN opcode executed): Dynamic count

**Recommendation**: Evaluate dynamic count at **runtime** (in MI_SPAWN handler). This allows context to evolve before MI execution.

### Result Collection and Merging

**Instance Completion**:

When instance token completes (`wf_exec.erl:536-573`):
```erlang
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Increment join counter if part of MI
    {UpdatedTokens2, UpdatedJoinCounters} = case get_instance_id(CurrentToken, ExecState) of
        {ok, InstanceId, MIConfig} ->
            %% Token is MI instance, collect result
            wf_mi:collect_result(InstanceId, UpdatedToken#token.value, ExecState);
        undefined ->
            %% Not an MI instance, use existing branch logic
            {Tokens, ExecState#exec_state.join_counters}
    end,
    ...
```

**Result Merging** (from `wf_exec.erl:381-393`):

Already implemented:
```erlang
merge_results(Results, all) ->
    {merged, Results};           %% All results collected
merge_results(Results, {first_n, _N}) ->
    {merged, Results};           %% First N results
merge_results(Results, first_complete) ->
    {first_complete, hd(Results)};  %% First result only
merge_results(Results, sync_merge) ->
    {sync_merged, lists:foldl(fun merge_state/2, #{}, Results)}.
```

**For MI**:
- Use `merge_results/2` for result aggregation
- `wait_all` → `{merged, AllResults}`
- `wait_n` → `{merged, FirstNResults}`
- `first_complete` → `{first_complete, FirstResult}`
- Fire-and-forget → No result merging (instances discarded)

### Cancellation of Remaining Instances

**When Join Satisfied Early** (`wait_n`, `first_complete`):

**Option 1**: Use wf_cancel (item 008)
```erlang
cancel_remaining(CompletedInstanceIds, AllInstanceIds) ->
    RemainingIds = AllInstanceIds -- CompletedInstanceIds,

    %% Cancel each remaining instance via wf_cancel
    lists:foreach(fun(InstanceId) ->
        TokenId = get_token_id_for_instance(InstanceId),
        wf_cancel:cancel_activity(CaseId, TokenId, [])
    end, RemainingIds).
```

**Option 2**: Internal cancellation (simpler, no wf_cancel dependency)
```erlang
cancel_remaining(ExecState, RemainingInstanceIds) ->
    %% Set remaining tokens to cancelled status
    Tokens = ExecState#exec_state.tokens,
    CancelledTokens = lists:foldl(fun(InstanceId, AccTokens) ->
        TokenId = get_token_id_for_instance(InstanceId, AccTokens),
        Token = maps:get(TokenId, AccTokens),
        AccTokens#{TokenId => Token#token{status = cancelled}}
    end, Tokens, RemainingInstanceIds),

    ExecState#exec_state{tokens = CancelledTokens}.
```

**Recommendation**: Option 2 (internal) for v1. Simpler, no cross-module dependency. Add Option 1 in v2 for consistency with wf_cancel semantics.

**Cancellation Detection**:
- Cancelled tokens should not increment join counters
- Cancelled tokens should be removed from active tokens
- Executor should skip cancelled tokens in token selection

### Instance Lifecycle

**Full Lifecycle**:

1. **Spawn** (`execute_mi_spawn/2`):
   - Evaluate instance count (fixed or dynamic)
   - Create N tokens with unique instance_ids
   - Create branch info tracking all instance tokens
   - Create join counter (unless fire-and-forget)
   - Store MI config in instance metadata

2. **Execute** (normal token execution):
   - Each instance token executes body bytecode
   - Instances run concurrently (executor scheduler selects tokens)
   - Instance state isolated (separate ctx() per token)

3. **Complete** (`execute_done/2`):
   - Instance token marked complete
   - Result collected via `wf_mi:collect_result/3`
   - Join counter incremented

4. **Check Join** (`execute_join_wait/2`):
   - Check if join policy satisfied
   - If satisfied: merge results, cancel remaining instances
   - If not satisfied: block executor

5. **Cancel** (early termination):
   - Remaining instances marked cancelled
   - Cancelled instances removed from active tokens
   - Join counter not incremented for cancelled instances

### Compiler Integration (Future)

**Pattern Constructor** (wf_core.erl or wf_term.erl):
```erlang
%% mi/2: Multiple instance pattern constructor
mi(Policy, Body) ->
    #mi{
        policy = Policy,           %% {fixed, N} | {dynamic, Min, Max}
        join_policy = wait_all,    %% Default wait_all
        body = Body
    }.
```

**Compiler** (wf_compile.erl):
```erlang
%% Compile MI pattern to bytecode
compile(#mi{policy = Policy, join_policy = JoinPolicy, body = Body}, NextIP) ->
    BodyBytecode = compile(Body),
    BodyIP = NextIP + 1,  %% Body starts after MI_SPAWN

    [
        {'MI_SPAWN', {Policy, JoinPolicy, BodyIP}},
        BodyBytecode,
        {'JOIN_WAIT', JoinPolicy},
        {'SEQ_NEXT', NextIP + length(BodyBytecode) + 2}
    ].
```

**Out of Scope for v1**: Compiler integration is separate item (wf_compile). For v1, focus on runtime execution (wf_mi + wf_exec handler).

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Token record modification** | High | Adding `instance_id` field changes token structure. Mitigation: Add field with default `undefined`, backward compatible. Update all token construction sites. |
| **Instance ID collisions** | Medium | `make_ref()` is globally unique, but custom IDs might collide. Mitigation: Use `{instance, make_ref()}` format for uniqueness. Document ID format. |
| **Dynamic count evaluation failure** | High | User function may crash or return invalid value. Mitigation: Wrap in try/catch, validate range (Min <= N <= Max). Return error on failure. |
| **Join policy mismatch** | High | MI join policies (wait_n) must map correctly to wf_exec policies ({first_n, N}). Mitigation: Test each mapping. Document policy equivalence. |
| **Cancellation deadlock** | High | Cancelling instances while they're executing may cause deadlock. Mitigation: Cancellation is cooperative (tokens check status on each step). Cancelled tokens gracefully exit. |
| **Memory exhaustion (unbounded instances)** | High | Dynamic count with high Max may spawn too many instances. Mitigation: Enforce absolute maximum (e.g., 1000 instances). Add configuration option. |
| **Result collection ordering** | Medium | Instance completion order may be non-deterministic. Mitigation: Sort results by instance_id for stable ordering. Use lists:keysort/2. |
| **Fire-and-forget instance tracking** | Medium | Instances without join may be orphaned (no cleanup). Mitigation: Fire-and-forget instances still tracked in tokens map. Cleaned up on case completion. |
| **Concurrent instance execution** | Low | Executor is single-threaded, so no race conditions. Mitigation: No mitigation needed (cooperative scheduling). |
| **Bytecode sharing bugs** | Medium | Shared bytecode reference may cause state leakage. Mitigation: Bytecode is read-only. Each instance has separate ctx() and token.value. Verify with tests. |
| **Performance degradation (many instances)** | Medium | Spawning 1000+ instances may slow executor. Mitigation: Profile instance spawn overhead. Use ETS for instance tracking if slow. |
| **Integration with wf_cancel** | Medium | wf_cancel (item 008) may not be implemented yet. Mitigation: Use internal cancellation for v1. Add wf_cancel integration in v2. |
| **Test coverage gaps** | Medium | MI has many combinations (fixed/dynamic × wait_all/wait_n/first_complete × sync/async). Mitigation: Property-based tests for all combinations. |
| **Instance result type mismatch** | Low | Different instances may return different result types. Mitigation: Document that results should be homogeneous. Use tagged tuples for type safety. |
| **Scope stack handling** | Medium | Instances inherit scope stack from parent. Mitigation: Copy scope stack on spawn. Each instance gets independent scope stack. |
| **Loop + MI interaction** | High | MI inside loop may spawn exponential instances. Mitigation: Validate total instance count. Add safety limit (e.g., 10000). |

## Recommended Approach

**High-Level Strategy**: Implement wf_mi module with instance spawning, result collection, and join checking. Add MI_SPAWN handler to wf_exec that reuses existing branch/join infrastructure. Use internal cancellation for v1 (add wf_cancel integration in v2). Add instance_id field to token record. Test all MI policy combinations with property-based tests.

### Phase 1: Types and Records

**1. Define types and records** (`src/wf_mi.erl`):

```erlang
%% MI join policies
-type mi_join_policy() ::
    wait_all |
    {wait_n, pos_integer()} |
    first_complete |
    none.  %% Fire-and-forget

%% Instance metadata
-record(mi_instance, {
    instance_id :: term(),
    token_id :: term(),
    instance_num :: pos_integer(),
    status :: active | complete | cancelled,
    result :: term() | undefined
}).

%% MI configuration
-record(mi_config, {
    policy :: wf_vm:mi_policy(),           %% {fixed, N} | {dynamic, Min, Max}
    join_policy :: mi_join_policy(),
    body_ip :: non_neg_integer(),
    scope_id :: term(),
    instance_ids :: [term()]  %% List of instance IDs
}).

%% Type exports
-export_type([
    mi_join_policy/0,
    mi_instance/0,
    mi_config/0
]).
```

**2. Test type compilation**

### Phase 2: Instance Spawning

**1. Implement `spawn_instances/4** (`src/wf_mi.erl`):

```erlang
%% Spawn N instances from MI configuration
-spec spawn_instances(wf_vm:mi_policy(), mi_join_policy(),
                    non_neg_integer(), term()) ->
    {ok, [#mi_instance{}], #mi_config{}}.
spawn_instances(MIPolicy, MIJoinPolicy, BodyIP, ScopeId) ->
    %% Evaluate instance count
    {ok, NumInstances} = eval_instance_count(MIPolicy, #{}),

    %% Spawn N instances
    Instances = lists:map(fun(N) ->
        InstanceId = {instance, make_ref()},
        TokenId = make_ref(),

        Instance = #mi_instance{
            instance_id = InstanceId,
            token_id = TokenId,
            instance_num = N,
            status = active,
            result = undefined
        },
        Instance
    end, lists:seq(1, NumInstances)),

    %% Create MI config
    Config = #mi_config{
        policy = MIPolicy,
        join_policy = MIJoinPolicy,
        body_ip = BodyIP,
        scope_id = ScopeId,
        instance_ids = [I#mi_instance.instance_id || I <- Instances]
    },

    {ok, Instances, Config}.
```

**2. Implement `eval_instance_count/2**:

```erlang
%% Evaluate instance count (fixed or dynamic)
-spec eval_instance_count(wf_vm:mi_policy(), map()) -> {ok, pos_integer()} | {error, term()}.
eval_instance_count({fixed, N}, _Ctx) ->
    {ok, N};

eval_instance_count({dynamic, _Min, _Max} = Policy, Ctx) ->
    %% For v1, use fixed count (dynamic evaluation in v2)
    %% TODO: Call user function in v2
    {ok, 1}.
```

**3. Test instance spawning**:

```erlang
spawn_fixed_instances_test_() ->
    MIPolicy = {fixed, 3},
    MIJoinPolicy = wait_all,
    BodyIP = 10,
    ScopeId = root,

    {ok, Instances, Config} = wf_mi:spawn_instances(MIPolicy, MIJoinPolicy, BodyIP, ScopeId),

    ?assertEqual(3, length(Instances)),
    ?assertEqual(3, length(Config#mi_config.instance_ids)).

spawn_dynamic_instances_test_() ->
    MIPolicy = {dynamic, 1, 10},
    MIJoinPolicy = wait_all,
    BodyIP = 10,
    ScopeId = root,

    {ok, Instances, _Config} = wf_mi:spawn_instances(MIPolicy, MIJoinPolicy, BodyIP, ScopeId),

    ?assertEqual(1, length(Instances)).  %% v1 returns 1
```

### Phase 3: MI_SPAWN Opcode Handler

**1. Add `instance_id` field to token** (`src/wf_exec.erl`):

```erlang
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled,
    instance_id :: term() | undefined  %% NEW FIELD
}).
```

**2. Implement `execute_mi_spawn/2** (`src/wf_exec.erl`):

```erlang
%% @doc Execute MI_SPAWN: spawn N instances of body
execute_mi_spawn({'MI_SPAWN', {MIPolicy, MIJoinPolicy, BodyIP}}, ExecState) ->
    %% Spawn instances
    {ok, Instances, MIConfig} = wf_mi:spawn_instances(
        MIPolicy, MIJoinPolicy, BodyIP, get_current_scope(ExecState)
    ),

    %% Create tokens for each instance
    ScopeId = get_current_scope(ExecState),
    CurrentToken = ExecState#exec_state.current_token,
    NewTokens = lists:map(fun(Instance) ->
        Token = #token{
            token_id = Instance#mi_instance.token_id,
            ip = BodyIP,  %% All instances execute same body
            scope_id = ScopeId,
            value = #{instance_id => Instance#mi_instance.instance_id,
                      instance_num => Instance#mi_instance.instance_num},
            status = active,
            instance_id = Instance#mi_instance.instance_id
        },
        {Instance#mi_instance.token_id, Token}
    end, Instances),

    %% Add tokens to state
    TokensMap0 = ExecState#exec_state.tokens,
    TokensMap = lists:foldl(fun({TokenId, Token}, Acc) ->
        maps:put(TokenId, Token, Acc)
    end, TokensMap0, NewTokens),

    %% Create branch info (tracks all instances)
    BranchId = make_ref(),
    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = [TokenId || {TokenId, _Token} <- NewTokens],
        join_id = undefined,  %% Set below if sync required
        targets = [BodyIP]  %% Same IP for all instances
    },
    BranchMap0 = ExecState#exec_state.branch_map,
    BranchMap = maps:put(BranchId, BranchInfo, BranchMap0),

    %% Create join counter (if sync required)
    {JoinCounters, NextIP} = case MIJoinPolicy of
        none ->
            %% Fire-and-forget, no join
            {ExecState#exec_state.join_counters, ExecState#exec_state.ip + 1};
        _SyncPolicy ->
            JoinId = make_ref(),
            NumInstances = length(Instances),
            Required = case MIJoinPolicy of
                wait_all -> NumInstances;
                {wait_n, N} -> N;
                first_complete -> 1
            end,
            JoinCounter = #join_counter{
                join_id = JoinId,
                completed = 0,
                required = Required,
                policy = mi_join_to_exec_join(MIJoinPolicy),
                results = []
            },
            JoinCounters0 = ExecState#exec_state.join_counters,
            JoinCounters1 = maps:put(JoinId, JoinCounter, JoinCounters0),

            %% Update branch info with join_id
            BranchMap2 = maps:put(BranchId,
                BranchInfo#branch_info{join_id = JoinId}, BranchMap),

            {JoinCounters1, ExecState#exec_state.ip + 1}
    end,

    %% Remove current token (replaced by instance tokens)
    TokensMapFinal = maps:remove(CurrentToken, TokensMap),

    %% Select next token to execute
    NextToken = select_next_token(TokensMapFinal),

    ExecState#exec_state{
        ip = NextIP,
        tokens = TokensMapFinal,
        branch_map = BranchMap,
        join_counters = JoinCounters,
        step_count = ExecState#exec_state.step_count + 1,
        current_token = NextToken
    }.

%% Convert MI join policy to wf_exec join policy
mi_join_to_exec_join(wait_all) -> all;
mi_join_to_exec_join({wait_n, _N}) -> {first_n, _N};
mi_join_to_exec_join(first_complete) -> first_complete;
mi_join_to_exec_join(none) -> none.
```

**3. Update `execute_opcode/2` to dispatch MI_SPAWN**:

```erlang
execute_opcode({MiSpawn, Policy}, ExecState)
  when MiSpawn =:= 'MI_SPAWN'; MiSpawn =:= mi_spawn ->
    execute_mi_spawn({MiSpawn, Policy}, ExecState);
```

**4. Test MI_SPAWN handler**:

```erlang
mi_spawn_fixed_test_() ->
    Bytecode = [
        {'MI_SPAWN', {{fixed, 2}, wait_all, 2}},  %% Spawn 2 instances of body at IP 2
        {'TASK_EXEC', task},                           %% Body
        {'DONE'},
        {'JOIN_WAIT', wait_all}
    ],
    ExecState0 = wf_exec:new(Bytecode),

    %% Execute MI_SPAWN
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),

    %% Verify 2 instance tokens created
    Tokens = ExecState1#exec_state.tokens,
    ?assertEqual(2, map_size(Tokens)).

mi_spawn_fire_and_forget_test_() ->
    Bytecode = [
        {'MI_SPAWN', {{fixed, 2}, none, 2}},  %% No join
        {'TASK_EXEC', task},
        {'DONE'}
    ],
    ExecState0 = wf_exec:new(Bytecode),

    %% Execute MI_SPAWN
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),

    %% Verify no join counter created
    JoinCounters = ExecState1#exec_state.join_counters,
    ?assertEqual(0, map_size(JoinCounters)).
```

### Phase 4: Result Collection

**1. Implement `collect_result/3** (`src/wf_mi.erl`):

```erlang
%% Collect instance result and update join state
-spec collect_result(term(), term(), exec_state()) ->
    {exec_state(), boolean()}.
collect_result(InstanceId, Result, ExecState) ->
    %% Find instance in branch_map
    BranchMap = ExecState#exec_state.branch_map,
    {ok, BranchId} = find_branch_for_instance(InstanceId, BranchMap),

    %% Increment join counter
    JoinId = (maps:get(BranchId, BranchMap))#branch_info.join_id,

    case JoinId of
        undefined ->
            %% Fire-and-forget, no join
            {ExecState, false};
        _JoinId ->
            JoinCounters = ExecState#exec_state.join_counters,
            JoinCounter = maps:get(JoinId, JoinCounters),
            UpdatedJoinCounter = JoinCounter#join_counter{
                completed = JoinCounter#join_counter.completed + 1,
                results = [Result | JoinCounter#join_counter.results]
            },
            UpdatedJoinCounters = maps:put(JoinId, UpdatedJoinCounter, JoinCounters),

            %% Check if join satisfied
            JoinSatisfied = UpdatedJoinCounter#join_counter.completed >= UpdatedJoinCounter#join_counter.required,
            {ExecState#exec_state{join_counters = UpdatedJoinCounters}, JoinSatisfied}
    end.
```

**2. Update `execute_done/2` to detect MI instances** (`src/wf_exec.erl`):

```erlang
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Check if token is MI instance
    {UpdatedTokens2, UpdatedJoinCounters, JoinSatisfied} = case Token#token.instance_id of
        undefined ->
            %% Not an MI instance, use existing logic
            {Tokens, ExecState#exec_state.join_counters, false};
        InstanceId ->
            %% MI instance, collect result
            {TokensWithResult, JoinSat} = wf_mi:collect_result(
                InstanceId, UpdatedToken#token.value, ExecState#exec_state{tokens = Tokens}
            ),
            {TokensWithResult#exec_state.tokens, TokensWithResult#exec_state.join_counters, JoinSat}
    end,

    %% If join satisfied and MI join policy is wait_n or first_complete, cancel remaining
    FinalExecState = case JoinSatisfied andalso Token#token.instance_id =/= undefined of
        true -> wf_mi:cancel_remaining(InstanceId, ExecState#exec_state{tokens = UpdatedTokens2, join_counters = UpdatedJoinCounters});
        false -> ExecState#exec_state{tokens = UpdatedTokens2, join_counters = UpdatedJoinCounters}
    end,

    ... rest of execute_done logic
```

**3. Test result collection**:

```erlang
collect_result_test_() ->
    %% Create MI with wait_all
    Instances = [
        #mi_instance{instance_id = i1, token_id = t1, status = active},
        #mi_instance{instance_id = i2, token_id = t2, status = active}
    ],

    %% First instance completes
    {ExecState1, JoinSatisfied1} = wf_mi:collect_result(i1, result1, ExecState0),

    ?assertEqual(1, get_join_counter_completed(ExecState1)),
    ?assertEqual(false, JoinSatisfied1),

    %% Second instance completes
    {ExecState2, JoinSatisfied2} = wf_mi:collect_result(i2, result2, ExecState1),

    ?assertEqual(2, get_join_counter_completed(ExecState2)),
    ?assertEqual(true, JoinSatisfied2).
```

### Phase 5: Cancellation of Remaining Instances

**1. Implement `cancel_remaining/2** (`src/wf_mi.erl`):

```erlang
%% Cancel remaining instances when join satisfied early
-spec cancel_remaining(term(), exec_state()) -> exec_state().
cancel_remaining(CompletedInstanceId, ExecState) ->
    %% Find branch info
    BranchMap = ExecState#exec_state.branch_map,
    {ok, BranchId} = find_branch_for_instance(CompletedInstanceId, BranchMap),
    BranchInfo = maps:get(BranchId, BranchMap),

    %% Find remaining active instances
    AllInstanceIds = get_instance_ids_from_branch(BranchInfo),
    ActiveInstanceIds = lists:filter(fun(InstanceId) ->
        InstanceId =/= CompletedInstanceId
    end, AllInstanceIds),

    %% Cancel remaining instances
    Tokens = ExecState#exec_state.tokens,
    CancelledTokens = lists:foldl(fun(InstanceId, AccTokens) ->
        TokenId = get_token_id_for_instance(InstanceId, AccTokens),
        Token = maps:get(TokenId, AccTokens),
        AccTokens#{TokenId => Token#token{status = cancelled}}
    end, Tokens, ActiveInstanceIds),

    ExecState#exec_state{tokens = CancelledTokens}.
```

**2. Test cancellation**:

```erlang
cancel_remaining_test_() ->
    %% Create MI with wait_n (wait for 2 of 3)
    ExecState0 = create_mi_state({fixed, 3}, {wait_n, 2}),

    %% First 2 instances complete
    {ExecState1, _} = wf_mi:collect_result(i1, result1, ExecState0),
    {ExecState2, JoinSat} = wf_mi:collect_result(i2, result2, ExecState1),

    ?assertEqual(true, JoinSat),

    %% Cancel remaining instances
    ExecState3 = wf_mi:cancel_remaining(i2, ExecState2),

    %% Verify i3 cancelled
    Token3 = get_token_for_instance(i3, ExecState3),
    ?assertEqual(cancelled, Token3#token.status).
```

### Phase 6: Comprehensive Testing

**1. Unit tests**:
- Test spawn_instances/4 (fixed, dynamic)
- Test eval_instance_count/2 (fixed, dynamic validation)
- Test collect_result/3 (join counter increment, join satisfaction)
- Test check_join/2 (all join policies)
- Test cancel_remaining/2 (remaining instances cancelled)

**2. Integration tests**:
- Test MI_SPAWN opcode in wf_exec
- Test fixed count MI with wait_all
- Test dynamic count MI with wait_all
- Test MI with wait_n (N of M, remainder cancelled)
- Test MI with first_complete (first wins, others cancelled)
- Test MI fire-and-forget (no join)
- Test instance isolation (separate ctx())

**3. Property-based tests**:
```erlang
%% Property: MI spawns correct number of instances
prop_mi_instance_count() ->
    ?FORALL({N, MIPolicy}, {pos_integer(), mi_policy_gen()},
        begin
            {ok, Instances, _Config} = wf_mi:spawn_instances(MIPolicy, wait_all, 10, root),
            ExpectedN = case MIPolicy of
                {fixed, N} -> N;
                {dynamic, _, _} -> 1  %% v1 returns 1
            end,
            length(Instances) =:= ExpectedN
        end).

%% Property: Join satisfied after required instances complete
prop_mi_join_satisfied() ->
    ?FORALL({M, N}, {int(2, 10), int(1, 10)},
        begin
            MIJoinPolicy = {wait_n, N},
            Required = min(N, M),
            %% Complete Required instances
            ExecState = complete_n_instances(Required, M, MIJoinPolicy),
            %% Check join satisfied
            wf_mi:check_join(ExecState) =:= {ok, satisfied}
        end).
```

**4. Performance tests**:
- Benchmark MI spawn overhead (10, 100, 1000 instances)
- Benchmark result collection overhead
- Verify O(N) complexity (not O(N²))

## Open Questions

1. **Token instance_id Field**: Should `instance_id` be added to `#token{}` record or stored in `value` field?
   - **Recommendation**: Add `instance_id` field. Cleaner separation of metadata vs user data. Backward compatible (default undefined).

2. **Dynamic Count Evaluation**: When should dynamic count be evaluated (compile time or runtime)?
   - **Recommendation**: Runtime (in MI_SPAWN handler). Allows context to evolve before MI. Compile-time evaluation loses flexibility.

3. **Bytecode Sharing**: Should instances share bytecode reference or each get a copy?
   - **Recommendation**: Share reference (memory efficient). Bytecode is read-only. State isolation via separate ctx() and token.value.

4. **Instance ID Format**: Should instance IDs be `{instance, make_ref()}` or sequential with scope?
   - **Recommendation**: `{instance, make_ref()}` for uniqueness. Sequential IDs require scope tracking (more complex).

5. **Fire-and-Forget Tracking**: How to track fire-and-forget instances (no join)?
   - **Recommendation**: Still track in tokens map. Cleaned up on case completion. No special handling needed.

6. **Result Ordering**: Should instance results be ordered by instance_num or completion order?
   - **Recommendation**: Completion order (current behavior). Sort by instance_num if stable ordering needed (add option).

7. **Cancellation Integration**: Should wf_mi use wf_cancel or internal cancellation?
   - **Recommendation**: Internal for v1 (simpler). Add wf_cancel integration in v2 for consistency.

8. **Maximum Instance Limit**: Should there be a hard limit on instance count?
   - **Recommendation**: Yes, enforce absolute maximum (e.g., 1000). Prevent memory exhaustion. Configurable via options.

9. **Instance Result Type**: Should results be homogeneous or allow heterogeneous types?
   - **Recommendation**: Document homogeneous expectation. Use tagged tuples for type safety if needed.

10. **Scope Stack Handling**: Do instances inherit parent scope stack or get independent copies?
    - **Recommendation**: Copy scope stack on spawn. Each instance has independent scope stack. Prevents scope interference.

11. **Loop + MI Safety**: How to prevent exponential instance explosion (MI inside loop)?
    - **Recommendation**: Validate total instance count. Add safety limit (e.g., 10000 instances per case).

12. **Instance Metadata Storage**: Where to store MI config (branch_info, separate map, token value)?
    - **Recommendation**: Extend `branch_info` record with `mi_config` field. Keeps instance tracking localized.

13. **Error Handling**: What happens if instance crashes (raises exception)?
    - **Recommendation**: Mark instance as failed (not complete). Join counter counts failed instances toward completion. Configurable (ignore failures or require all success).

14. **MI Compiler Integration**: When to implement wf_compile support for MI pattern?
    - **Recommendation**: Separate item (wf_compile). For v1, focus on runtime execution. Compiler integration is future work.

15. **Performance Baseline**: What is acceptable MI spawn overhead?
    - **Target**: < 1ms for 10 instances, < 100ms for 1000 instances
    - **Measurement**: Use `timer:tc/1` to benchmark. Optimize if exceeds targets.

16. **Test State Helpers**: How to create test states with MI instances?
    - **Recommendation**: Create helper module `wf_mi_test_helpers.erl` with functions:
      - `create_mi_state(N, Policy)` - Create state with N instances
      - `complete_n_instances(N, M, Policy)` - Complete N of M instances
      - `get_join_counter_completed/1` - Helper to check join counter

17. **Join Policy Mismatch**: How to ensure MI join policies map correctly to wf_exec policies?
    - **Recommendation**: Document mapping clearly. Add tests for each policy (wait_all, wait_n, first_complete). Verify behavior matches specification.

18. **Instance Lifecycle Events**: Should MI produce trace events (spawn, complete, cancel)?
    - **Recommendation**: Yes, produce events for observability. Use wf_trace (item 011) when available. For v1, return events to caller.

19. **Backward Compatibility**: How to handle existing tokens without `instance_id` field?
    - **Recommendation**: Default `instance_id` to `undefined`. Check `instance_id =/= undefined` to detect MI instances. Backward compatible.

20. **State Persistence**: Should MI instances be persisted to wf_state for crash recovery?
    - **Recommendation**: Yes, persist tokens (including instance_id) via wf_state. Enables crash recovery. Add in v2 (wf_state integration).
