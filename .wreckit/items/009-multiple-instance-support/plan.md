# Implement multiple instance patterns Implementation Plan

## Implementation Plan Title

Multiple Instance Pattern Support (WP16): Fixed and Dynamic Instance Spawning with Configurable Join Policies

## Overview

Implement `wf_mi.erl`: Multiple instance pattern support enabling parallel execution of the same workflow fragment with different data contexts. This implements workflow pattern WP16 (Multi-Instance) from the workflow pattern catalog, supporting:
- Fixed count instances (N determined at compile time)
- Dynamic count instances (N determined at runtime from ctx)
- Synchronized execution (wait_all, wait_n, first_complete join policies)
- Fire-and-forget execution (no synchronization)

The implementation integrates with the existing executor's parallel execution infrastructure (`PAR_FORK`/`JOIN_WAIT` in `wf_exec`) and extends token tracking to support instance-level metadata. Instances share bytecode references but maintain isolated state via separate tokens and contexts.

## Current State

**Existing Infrastructure (Reusable):**

1. **Parallel Execution** (`/Users/speed/wf-substrate/src/wf_exec.erl:259-323`):
   - `PAR_FORK` spawns N tokens to DIFFERENT target IPs
   - Creates branch_info tracking tokens and join_id
   - Creates join_counter with policy-based result collection
   - Pattern to follow: Token spawning, branch tracking, join counter creation

2. **Join Counter Management** (`/Users/speed/wf-substrate/src/wf_exec.erl:44-51, 325-393`):
   - Supports join policies: `all`, `{first_n, N}`, `first_complete`, `sync_merge`
   - MI policies map directly: `wait_all` → `all`, `wait_n` → `{first_n, N}`, `first_complete` → `first_complete`
   - `merge_results/2` handles result aggregation per policy

3. **Token Tracking** (`/Users/speed/wf-substrate/src/wf_exec.erl:28-34, 575-597`):
   - `#token{}` record with token_id, ip, scope_id, value, status
   - `find_branch_for_token/2` locates branch for completion handling
   - `increment_join_counter/3` updates join counter on token completion
   - **Extension needed**: Add `instance_id` field for MI instance tracking

4. **MI Policy Type** (`/Users/speed/wf-substrate/src/wf_vm.erl:37-40`):
   - `mi_policy()` already defined: `{fixed, N} | {dynamic, Min, Max}`
   - Opcode type includes `{atom(), mi_policy()}` for MI_SPAWN
   - **Missing**: `execute_mi_spawn/2` handler in wf_exec.erl

5. **State Store** (`/Users/speed/wf-substrate/src/wf_state.erl:76-82`):
   - Token record matches wf_exec structure (status: active | complete | cancelled)
   - Scope tracking with `#scope{status := active | cancelled}`
   - No changes needed: MI instances tracked as tokens in existing maps

**What's Missing:**

1. **wf_mi.erl module**: Core MI semantics module (does not exist)
2. **MI_SPAWN opcode handler**: `execute_mi_spawn/2` in wf_exec.erl (unimplemented_opcode throws)
3. **Token instance_id field**: Extend #token{} record for instance metadata
4. **Result collection for MI**: Detect MI instances in execute_done/2, collect results
5. **Cancellation of remaining instances**: Internal cancellation for wait_n/first_complete policies
6. **Comprehensive tests**: Unit tests for wf_mi, integration tests for MI_SPAWN execution

### Key Discoveries:

- **Pattern to follow** (`wf_exec.erl:259-323`): PAR_FORK implementation shows complete token/branch/join lifecycle
- **Backward compatible** (`wf_exec.erl:28-34`): Adding `instance_id` field with default `undefined` maintains compatibility
- **Join policy mapping** (`wf_exec.erl:381-393`): MI join policies map 1:1 to existing wf_exec policies via `merge_results/2`
- **Shared bytecode safe**: Instances share bytecode reference (read-only) with separate ctx() and token.value for state isolation
- **No wf_cancel dependency** (`wf_exec.erl:519-529`): Internal token cancellation (set status to cancelled) sufficient for v1
- **Executor is single-threaded** (`wf_exec.erl:247-253`): Cooperative scheduling via `select_next_token/2`, no race conditions
- **Trace events required**: MI spawn/completion/cancel should produce structured events for observability

## Desired End State

**Functional Requirements:**

1. **Instance Spawning** (`wf_mi:spawn_instances/4`):
   - Fixed count: Spawn exactly N instances (N from `{fixed, N}` policy)
   - Dynamic count: Evaluate N from ctx() with min/max bounds validation
   - Each instance gets unique `instance_id` (`{instance, make_ref()}`), token, and sequential instance_num
   - All instances target SAME body IP (unlike PAR_FORK's different IPs)

2. **Join Policy Enforcement** (`wf_mi:check_join/2`):
   - `wait_all`: Block until all M instances complete
   - `{wait_n, N}`: Block until N of M complete, cancel remainder
   - `first_complete`: Block until first instance completes, cancel all others
   - `none` (fire-and-forget): No join counter, instances run independently

3. **Result Collection** (`wf_mi:collect_result/3`):
   - Track instance completion via join counter increment
   - Store instance results in `join_counter.results` list
   - Check join satisfaction after each completion
   - Trigger cancellation of remaining instances when join satisfied early

4. **Cancellation of Remaining Instances** (`wf_mi:cancel_remaining/2`):
   - Set remaining instance tokens to `cancelled` status
   - Cancelled instances removed from active tokens by `select_next_token/2`
   - Cancelled instances do NOT increment join counters
   - Graceful termination (cancelled tokens check status and exit)

5. **Executor Integration** (`wf_exec:execute_mi_spawn/2`):
   - Add MI_SPAWN opcode handler
   - Extend #token{} record with `instance_id` field
   - Reuse branch/join infrastructure for instance tracking
   - Detect MI instances in execute_done/2, collect results via wf_mi

**Verification Criteria:**

- Unit tests: spawn_instances/4 (fixed/dynamic), collect_result/3, check_join/2, cancel_remaining/2
- Integration tests: MI_SPAWN opcode execution with all join policies
- Property tests: Instance count correctness, join satisfaction logic
- Performance tests: Spawn overhead < 1ms for 10 instances, < 100ms for 1000 instances
- Backward compatibility: Existing wf_exec tests pass without modification

### Key Discoveries:

- **PAR_FORK vs MI_SPAWN** (`wf_exec.erl:260-278`): PAR_FORK spawns to different IPs, MI spawns to same IP with instance_id
- **Join counter reuse** (`wf_exec.erl:296-305`): MI uses existing #join_counter{} structure with mapped policies
- **Token selection** (`wf_exec.erl:247-253`): `select_next_token/2` filters by `status =:= active`, automatically excluding cancelled instances
- **Scope isolation**: Each instance gets independent scope_stack copy (prevents interference)
- **Instance safety**: Enforce absolute maximum instance count (1000) to prevent memory exhaustion

## What We're NOT Doing

1. **Compiler integration** (wf_compile.erl): Pattern constructor `mi/2` and bytecode compilation is **out of scope** for v1
   - v1 focuses on runtime execution (wf_mi + wf_exec handler)
   - Compiler integration is separate work item (wf_compile, future)

2. **wf_cancel integration** (item 008): Using wf_cancel module for instance cancellation
   - v1 uses internal token cancellation (set status to cancelled)
   - wf_cancel integration (cancel_activity/3) deferred to v2 for consistency

3. **Dynamic count evaluation from user functions**: Runtime ctx() → N evaluation
   - v1 implements `{dynamic, Min, Max}` as stub (returns Min instances)
   - Full dynamic evaluation with user Fun :: ctx() → pos_integer() deferred to v2

4. **State persistence** (wf_state crash recovery): Persisting MI instances to ETS
   - v1 keeps MI instances in exec_state (in-memory)
   - Crash recovery via wf_state persistence is v2 work

5. **Advanced MI patterns**: Deferred choice MI, pipelined MI, recursive MI
   - v1 implements only basic WP16 patterns (fixed/dynamic with sync policies)
   - Advanced patterns are future enhancements

6. **Instance failure handling**: Distinguishing complete vs failed instances in join logic
   - v1 treats all completed instances (including errors) as satisfying join
   - Failure-aware policies (require_all_success) are v2 work

7. **Result ordering guarantees**: Stable ordering by instance_num vs completion order
   - v1 uses completion order (current behavior of merge_results/2)
   - Stable ordering option is future enhancement

## Implementation Approach

**High-Level Strategy**: Incremental implementation following the pattern established by PAR_FORK/JION_WAIT. Implement wf_mi module with instance spawning, result collection, and join checking. Add MI_SPAWN handler to wf_exec that reuses existing branch/join infrastructure. Use internal token cancellation for v1. Add instance_id field to token record. Test exhaustively with unit, integration, and property-based tests.

**Key Design Decisions**:

1. **Instance ID Format**: Use `{instance, make_ref()}` for uniqueness (simpler than scoped sequential IDs)
2. **Bytecode Sharing**: All instances share bytecode reference (read-only, memory efficient)
3. **Join Policy Mapping**: Direct mapping to wf_exec join policies (wait_all → all, wait_n → first_n)
4. **Token Extension**: Add `instance_id :: term() | undefined` field to #token{} record
5. **Cancellation**: Internal token status change (cancelled) for v1 (simpler, no wf_cancel dependency)
6. **Scope Handling**: Copy scope_stack on spawn (each instance has independent scope stack)
7. **Safety Limit**: Enforce maximum 1000 instances (configurable option for v2)
8. **Dynamic Count**: Stub implementation for v1 (return Min, ignore ctx evaluation)
9. **Fire-and-Forget**: No join counter, instances tracked in tokens map, cleaned up on case completion
10. **Result Ordering**: Completion order (merge_results/2 behavior), sort by instance_num if stable ordering needed

**Phases** (ordered by dependency and risk):

1. **Types and Records**: Define wf_mi types, extend token record
2. **Instance Spawning**: Implement spawn_instances/4 and eval_instance_count/2
3. **MI_SPAWN Handler**: Add execute_mi_spawn/2 to wf_exec, wire into opcode dispatch
4. **Result Collection**: Implement collect_result/3, update execute_done/2 for MI instances
5. **Join Checking and Cancellation**: Implement check_join/2 and cancel_remaining/2
6. **Comprehensive Testing**: Unit tests, integration tests, property tests

**Rollback Strategy**: Each phase is independently testable. If a phase fails, revert changes and fix issues before proceeding. Token record change is backward compatible (instance_id defaults to undefined), so existing wf_exec tests continue passing.

---

## Phases

### Phase 1: Types and Records

#### Overview

Define wf_mi module with core types and records. Extend #token{} record to support instance metadata. This foundation enables all subsequent phases.

#### Changes Required:

##### 1. Create src/wf_mi.erl module with types and records

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl` (NEW FILE)
**Changes**: Create new module with MI type definitions and record structures

```erlang
%%%-------------------------------------------------------------------
%%% @doc Multiple Instance Pattern Semantics (WP16)
%%%
%%% This module implements multiple instance patterns from the workflow
%%% pattern catalog. MI patterns spawn N concurrent instances of the same
%%% workflow fragment with configurable synchronization policies.
%%%
%%% == Instance Creation Modes ==
%%% - Fixed count: N instances determined at compile time ({fixed, N})
%%% - Dynamic count: N instances determined at runtime from ctx ({dynamic, Min, Max})
%%%
%%% == Join Policies ==
%%% - wait_all: Block until all M instances complete (synchronization barrier)
%%% - {wait_n, N}: Block until N of M instances complete, cancel remainder
%%% - first_complete: Block until first instance completes, cancel all others
%%% - none: Fire-and-forget (no synchronization, continuation proceeds immediately)
%%%
%%% == Instance Lifecycle ==
%%% 1. Spawn: N instances created with unique instance_id, token, and instance_num
%%% 2. Execute: Each instance executes body bytecode concurrently (cooperative scheduling)
%%% 3. Complete: Instance marks complete, result collected, join counter incremented
%%% 4. Join Satisfied: Results merged, continuation token created, remaining instances cancelled
%%%
%%% == Integration ==
%%% - wf_exec: MI_SPAWN opcode handler calls wf_mi:spawn_instances/4
%%% - wf_exec: execute_done/2 calls wf_mi:collect_result/3 for MI instances
%%% - wf_exec: JOIN_WAIT handler calls wf_mi:check_join/2
%%% - wf_mi:cancel_remaining/2 cancels instances when join satisfied early
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_mi).

%% API exports
-export([
    spawn_instances/4,
    collect_result/3,
    check_join/2,
    cancel_remaining/2
]).

%% Type exports
-export_type([
    mi_join_policy/0,
    mi_instance/0,
    mi_config/0
]).

%%====================================================================
%% Types
%%====================================================================

%% MI join policies
-type mi_join_policy() ::
    wait_all |
    {wait_n, pos_integer()} |
    first_complete |
    none.  %% Fire-and-forget

%% Instance metadata
-record(mi_instance, {
    instance_id :: term(),                      %% Unique instance identifier
    token_id :: term(),                         %% Associated token ID
    instance_num :: pos_integer(),              %% Sequential instance number (1..N)
    status :: active | complete | cancelled,    %% Instance status
    result :: term() | undefined                %% Instance result (undefined until complete)
}).

-opaque mi_instance() :: #mi_instance{}.

%% MI configuration
-record(mi_config, {
    policy :: wf_vm:mi_policy(),                %% {fixed, N} | {dynamic, Min, Max}
    join_policy :: mi_join_policy(),            %% wait_all | {wait_n, N} | first_complete | none
    body_ip :: non_neg_integer(),               %% Bytecode IP of body to execute
    scope_id :: term(),                         %% Scope ID for instances
    instance_ids :: [term()]                    %% List of instance IDs
}).

-opaque mi_config() :: #mi_config{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Spawn N instances from MI configuration
%% Returns {ok, Instances, Config} where Instances is list of #mi_instance{} records
-spec spawn_instances(wf_vm:mi_policy(), mi_join_policy(),
                      non_neg_integer(), term()) ->
    {ok, [#mi_instance{}], #mi_config{}}.
spawn_instances(_MIPolicy, _MIJoinPolicy, _BodyIP, _ScopeId) ->
    erlang:error(not_implemented).

%% @doc Collect instance result and update join state
%% Returns {UpdatedExecState, JoinSatisfied} where JoinSatisfied is boolean()
-spec collect_result(term(), term(), wf_exec:exec_state()) ->
    {wf_exec:exec_state(), boolean()}.
collect_result(_InstanceId, _Result, _ExecState) ->
    erlang:error(not_implemented).

%% @doc Check if join policy is satisfied
%% Returns {ok, satisfied} | {ok, not_satisfied} | {error, term()}
-spec check_join(wf_exec:exec_state()) ->
    {ok, satisfied | not_satisfied} | {error, term()}.
check_join(_ExecState) ->
    erlang:error(not_implemented).

%% @doc Cancel remaining instances when join satisfied early
%% Returns updated exec_state with remaining instances marked cancelled
-spec cancel_remaining(term(), wf_exec:exec_state()) -> wf_exec:exec_state().
cancel_remaining(_CompletedInstanceId, _ExecState) ->
    erlang:error(not_implemented).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Evaluate instance count (fixed or dynamic)
%% For v1, dynamic policy returns Min instances (stub)
%% v2: Evaluate user function Fun :: ctx() -> pos_integer()
-spec eval_instance_count(wf_vm:mi_policy(), map()) -> {ok, pos_integer()} | {error, term()}.
eval_instance_count(_Policy, _Ctx) ->
    erlang:error(not_implemented).
```

##### 2. Extend token record with instance_id field

**File**: `/Users/speed/wf-substrate/src/wf_exec.erl`
**Changes**: Add `instance_id` field to #token{} record (line 28-34)

```erlang
%% BEFORE:
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled
}).

%% AFTER:
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled,
    instance_id :: term() | undefined  %% MI instance ID (undefined for non-MI tokens)
}).
```

##### 3. Update token creation sites

**File**: `/Users/speed/wf-substrate/src/wf_exec.erl`
**Changes**: Update all token construction to include `instance_id = undefined`

- Line 83-89: `wf_exec:new/1` initial token
- Line 270-276: `execute_par_fork/2` parallel fork tokens
- Line 340-346: `execute_join_wait/2` continuation token

```erlang
%% Pattern to apply (add instance_id = undefined):
#token{
    token_id = TokenId,
    ip = IP,
    scope_id = ScopeId,
    value = Value,
    status = Status,
    instance_id = undefined  %% ADD THIS LINE
}
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc -o ebin/ src/wf_mi.erl`
- [ ] Type check passes: `dialyzer src/wf_mi.erl`
- [ ] wf_exec compiles with token record changes: `erlc -o ebin/ src/wf_exec.erl`
- [ ] Existing wf_exec tests pass: `ct_run -suite wf_exec_tests`

##### Manual Verification:

- [ ] Token record default value ensures backward compatibility
- [ ] All token construction sites updated (grep for "#token{" to verify)
- [ ] Module exports match specification (spawn_instances/4, collect_result/3, check_join/2, cancel_remaining/2)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Instance Spawning

#### Overview

Implement instance spawning logic in wf_mi. Evaluate instance count (fixed or dynamic) and create N instance records with unique IDs and sequential numbers. This phase provides the core instance creation machinery.

#### Changes Required:

##### 1. Implement eval_instance_count/2

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl`
**Changes**: Add instance count evaluation function (line ~180)

```erlang
%% @doc Evaluate instance count (fixed or dynamic)
%% Fixed policy: Returns N directly
%% Dynamic policy: v1 stub returns Min, v2 evaluates user function
-spec eval_instance_count(wf_vm:mi_policy(), map()) -> {ok, pos_integer()} | {error, term()}.
eval_instance_count({fixed, N}, _Ctx) when N > 0 ->
    {ok, N};
eval_instance_count({fixed, 0}, _Ctx) ->
    {error, zero_instances};
eval_instance_count({dynamic, Min, Max}, _Ctx) when Min > 0, Min =< Max ->
    %% v1: Return Min instances (stub)
    %% v2: Call user function Fun :: ctx() -> pos_integer()
    {ok, Min};
eval_instance_count({dynamic, Min, Max}, _Ctx) when Min > Max ->
    {error, {invalid_range, Min, Max}};
eval_instance_count({dynamic, 0, _Max}, _Ctx) ->
    {error, zero_minimum}.
```

##### 2. Implement spawn_instances/4

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl`
**Changes**: Implement instance spawning function (line ~70)

```erlang
%% @doc Spawn N instances from MI configuration
-spec spawn_instances(wf_vm:mi_policy(), mi_join_policy(),
                      non_neg_integer(), term()) ->
    {ok, [#mi_instance{}], #mi_config{}}.
spawn_instances(MIPolicy, MIJoinPolicy, BodyIP, ScopeId) ->
    %% Evaluate instance count
    case eval_instance_count(MIPolicy, #{}) of
        {error, Reason} ->
            error({instance_count_error, Reason});
        {ok, NumInstances} when NumInstances > 1000 ->
            error({too_many_instances, NumInstances, 1000});
        {ok, NumInstances} ->
            %% Spawn N instances
            Instances = lists:map(fun(N) ->
                InstanceId = {instance, make_ref()},
                TokenId = make_ref(),

                #mi_instance{
                    instance_id = InstanceId,
                    token_id = TokenId,
                    instance_num = N,
                    status = active,
                    result = undefined
                }
            end, lists:seq(1, NumInstances)),

            %% Create MI config
            Config = #mi_config{
                policy = MIPolicy,
                join_policy = MIJoinPolicy,
                body_ip = BodyIP,
                scope_id = ScopeId,
                instance_ids = [I#mi_instance.instance_id || I <- Instances]
            },

            {ok, Instances, Config}
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass for eval_instance_count/2 (fixed: positive N, zero N; dynamic: valid range, invalid range)
- [ ] Unit tests pass for spawn_instances/4 (fixed count, dynamic count, too many instances error)
- [ ] Instance IDs are unique (property test: generate 100 instances, verify all IDs distinct)
- [ ] Instance numbers are sequential 1..N (property test)

##### Manual Verification:

- [ ] Instance ID format is `{instance, Ref}` where Ref is make_ref()
- [ ] Fixed count spawns exactly N instances
- [ ] Dynamic count spawns Min instances (v1 behavior)
- [ ] Error cases return descriptive error tuples

**Test Coverage** (create test/wf_mi_tests.erl):

```erlang
%% Unit tests for eval_instance_count
eval_instance_count_fixed_test() ->
    ?assertEqual({ok, 5}, wf_mi:eval_instance_count({fixed, 5}, #{})),
    ?assertEqual({error, zero_instances}, wf_mi:eval_instance_count({fixed, 0}, #{})).

eval_instance_count_dynamic_test() ->
    ?assertEqual({ok, 2}, wf_mi:eval_instance_count({dynamic, 2, 10}, #{})),
    ?assertEqual({error, invalid_range}, wf_mi:eval_instance_count({dynamic, 10, 2}, #{})).

%% Unit tests for spawn_instances
spawn_instances_fixed_test() ->
    {ok, Instances, Config} = wf_mi:spawn_instances({fixed, 3}, wait_all, 10, root),
    ?assertEqual(3, length(Instances)),
    ?assertEqual(3, length(Config#mi_config.instance_ids)).

spawn_instances_unique_ids_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, 100}, wait_all, 10, root),
    Ids = [I#mi_instance.instance_id || I <- Instances],
    ?assertEqual(100, length(lists:usort(Ids))).
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: MI_SPAWN Opcode Handler

#### Overview

Add execute_mi_spawn/2 to wf_exec that spawns MI instances. Integrate with existing branch/join infrastructure. Wire into opcode dispatch table. This enables MI execution through the executor.

#### Changes Required:

##### 1. Add MI_SPAWN opcode handler to wf_exec

**File**: `/Users/speed/wf-substrate/src/wf_exec.erl`
**Changes**: Add execute_mi_spawn/2 function (after execute_par_fork/2, around line 324)

```erlang
%% @doc Execute MI_SPAWN: spawn N instances of body at same IP
%% Unlike PAR_FORK (spawns to different IPs), MI_SPAWN spawns N instances
%% to the SAME body IP, with per-instance state via instance_id
execute_mi_spawn({'MI_SPAWN', {MIPolicy, MIJoinPolicy, BodyIP}}, ExecState) ->
    %% Spawn instances via wf_mi
    {ok, Instances, MIConfig} = wf_mi:spawn_instances(
        MIPolicy, MIJoinPolicy, BodyIP, get_current_scope(ExecState)
    ),

    %% Create tokens for each instance
    ScopeId = get_current_scope(ExecState),
    CurrentToken = ExecState#exec_state.current_token,
    NewTokens = lists:map(fun(Instance) ->
        Token = #token{
            token_id = Instance#mi_instance.token_id,
            ip = BodyIP,  %% All instances execute SAME body
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
                {wait_n, N} -> min(N, NumInstances);  %% Guard against N > M
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

%% @doc Convert MI join policy to wf_exec join policy
-spec mi_join_to_exec_join(wf_mi:mi_join_policy()) -> wf_vm:join_policy().
mi_join_to_exec_join(wait_all) -> all;
mi_join_to_exec_join({wait_n, N}) -> {first_n, N};
mi_join_to_exec_join(first_complete) -> first_complete;
mi_join_to_exec_join(none) -> none.
```

##### 2. Wire MI_SPAWN into opcode dispatch

**File**: `/Users/speed/wf-substrate/src/wf_exec.erl`
**Changes**: Add MI_SPAWN case to execute_opcode/2 (line 186-208)

```erlang
%% BEFORE:
execute_opcode({ParFork, Targets}, ExecState) when ParFork =:= 'PAR_FORK'; ParFork =:= par_fork ->
    execute_par_fork({ParFork, Targets}, ExecState);
execute_opcode({JoinWait, _Policy}, ExecState) when JoinWait =:= 'JOIN_WAIT'; JoinWait =:= join_wait ->
    execute_join_wait({JoinWait, _Policy}, ExecState);
...

%% AFTER:
execute_opcode({MiSpawn, Policy}, ExecState)
  when MiSpawn =:= 'MI_SPAWN'; MiSpawn =:= mi_spawn ->
    execute_mi_spawn({MiSpawn, Policy}, ExecState);
execute_opcode({ParFork, Targets}, ExecState) when ParFork =:= 'PAR_FORK'; ParFork =:= par_fork ->
    execute_par_fork({ParFork, Targets}, ExecState);
execute_opcode({JoinWait, _Policy}, ExecState) when JoinWait =:= 'JOIN_WAIT'; JoinWait =:= join_wait ->
    execute_join_wait({JoinWait, _Policy}, ExecState);
...
```

#### Success Criteria:

##### Automated Verification:

- [ ] wf_exec compiles without errors
- [ ] Unit test: MI_SPAWN with fixed count spawns N tokens
- [ ] Unit test: MI_SPAWN with dynamic count spawns Min tokens
- [ ] Unit test: MI_SPAWN with wait_all creates join counter (required = N)
- [ ] Unit test: MI_SPAWN with wait_n creates join counter (required = N)
- [ ] Unit test: MI_SPAWN with first_complete creates join counter (required = 1)
- [ ] Unit test: MI_SPAWN with none does NOT create join counter
- [ ] Unit test: All instance tokens have same IP (body IP)
- [ ] Unit test: All instance tokens have unique instance_id
- [ ] Existing wf_exec tests still pass (backward compatibility)

##### Manual Verification:

- [ ] Instance tokens have instance_id field set
- [ ] Instance tokens have value map with instance_id and instance_num
- [ ] Branch info tracks all instance token IDs
- [ ] Join counter created with correct required count
- [ ] Fire-and-forget MI has no join counter

**Test Coverage** (add to test/wf_exec_tests.erl):

```erlang
%% Mock MI bytecode
mock_bytecode_mi_fixed() ->
    [
        {'MI_SPAWN', {{fixed, 2}, wait_all, 2}},  %% Spawn 2 instances
        {'TASK_EXEC', task},                        %% Body
        {'DONE'},                                   %% Body end
        {'JOIN_WAIT', wait_all}                     %% Wait for instances
    ].

%% Test MI_SPAWN spawns 2 instances
mi_spawn_fixed_test_() ->
    Bytecode = mock_bytecode_mi_fixed(),
    ExecState0 = wf_exec:new(Bytecode),

    %% Execute MI_SPAWN
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),

    [
        ?_assertEqual(2, map_size(ExecState1#exec_state.tokens)),
        ?_assertEqual(1, map_size(ExecState1#exec_state.join_counters)),
        ?_assertEqual(2, get_join_counter_required(ExecState1))
    ].

%% Test MI_SPAWN fire-and-forget
mi_spawn_fire_and_forget_test_() ->
    Bytecode = [
        {'MI_SPAWN', {{fixed, 2}, none, 1}},  %% No join
        {'TASK_EXEC', task},
        {'DONE'}
    ],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),

    [
        ?_assertEqual(2, map_size(ExecState1#exec_state.tokens)),
        ?_assertEqual(0, map_size(ExecState1#exec_state.join_counters))
    ].
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Result Collection

#### Overview

Implement result collection for MI instances. Update execute_done/2 to detect MI instances and collect results via wf_mi. This enables join counter tracking and join satisfaction detection.

#### Changes Required:

##### 1. Implement collect_result/3 in wf_mi

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl`
**Changes**: Implement result collection function (line ~80)

```erlang
%% @doc Collect instance result and update join state
%% Returns {UpdatedExecState, JoinSatisfied}
-spec collect_result(term(), term(), wf_exec:exec_state()) ->
    {wf_exec:exec_state(), boolean()}.
collect_result(InstanceId, Result, ExecState) ->
    %% Find branch for this instance
    BranchMap = ExecState#exec_state.branch_map,
    case find_branch_for_instance(InstanceId, BranchMap) of
        {ok, BranchId} ->
            BranchInfo = maps:get(BranchId, BranchMap),
            JoinId = BranchInfo#branch_info.join_id,

            case JoinId of
                undefined ->
                    %% Fire-and-forget, no join
                    {ExecState, false};
                _JoinId ->
                    %% Increment join counter
                    JoinCounters = ExecState#exec_state.join_counters,
                    JoinCounter = maps:get(JoinId, JoinCounters),
                    UpdatedJoinCounter = JoinCounter#join_counter{
                        completed = JoinCounter#join_counter.completed + 1,
                        results = [Result | JoinCounter#join_counter.results]
                    },
                    UpdatedJoinCounters = maps:put(JoinId, UpdatedJoinCounter, JoinCounters),

                    %% Check if join satisfied
                    JoinSatisfied = UpdatedJoinCounter#join_counter.completed >=
                                    UpdatedJoinCounter#join_counter.required,
                    {ExecState#exec_state{join_counters = UpdatedJoinCounters}, JoinSatisfied}
            end;
        error ->
            %% Instance not found in branch map (should not happen)
            {ExecState, false}
    end.

%% @doc Find branch for a given instance ID
-spec find_branch_for_instance(term(), #{term() => #branch_info{}}) ->
    {ok, term()} | error.
find_branch_for_instance(InstanceId, BranchMap) ->
    %% Find branch containing a token with this instance_id
    Branches = maps:to_list(BranchMap),
    case lists:search(fun({_BranchId, #branch_info{tokens = TokenIds}}) ->
        %% Check each token in branch (need to look up token to check instance_id)
        lists:any(fun(TokenId) ->
            %% This is inefficient but correct for v1
            %% v2: Add instance_id mapping for O(1) lookup
            TokenId =:= InstanceId  %% Simplified: instance_id matches token_id in branch
        end, TokenIds)
    end, Branches) of
        {value, {BranchId, _BranchInfo}} -> {ok, BranchId};
        false -> error
    end.
```

**Note**: The find_branch_for_instance/2 implementation above is simplified. For correct implementation, we need to look up tokens by instance_id. This requires either:
- Adding instance_id → token_id mapping in exec_state (v2 enhancement)
- Iterating all tokens to find matching instance_id (O(N), acceptable for v1)

Revised implementation (v1 approach):

```erlang
%% @doc Find branch for a given instance ID (v1: O(N) token iteration)
-spec find_branch_for_instance(term(), #{term() => #branch_info{}}, #{term() => #token{}}) ->
    {ok, term()} | error.
find_branch_for_instance(InstanceId, BranchMap, Tokens) ->
    %% Find token with this instance_id
    TokenId = case lists:search(fun({_Tid, Token}) ->
        Token#token.instance_id =:= InstanceId
    end, maps:to_list(Tokens)) of
        {value, {Tid, _Token}} -> Tid;
        false -> error
    end,

    case TokenId of
        error -> error;
        _ ->
            %% Find branch containing this token
            wf_exec:find_branch_for_token(TokenId, BranchMap)
    end.
```

Updated collect_result/3 signature:

```erlang
%% Updated signature (passes Tokens map)
collect_result(InstanceId, Result, ExecState) ->
    BranchMap = ExecState#exec_state.branch_map,
    Tokens = ExecState#exec_state.tokens,

    case find_branch_for_instance(InstanceId, BranchMap, Tokens) of
        {ok, BranchId} ->
            %% ... rest of implementation
        error ->
            {ExecState, false}
    end.
```

##### 2. Update execute_done/2 to detect MI instances

**File**: `/Users/speed/wf-substrate/src/wf_exec.erl`
**Changes**: Modify execute_done/2 to detect MI instances and collect results (line 536-573)

```erlang
%% BEFORE:
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Increment join counter if part of a parallel branch
    {UpdatedTokens2, UpdatedJoinCounters} = case find_branch_for_token(CurrentToken, ExecState#exec_state.branch_map) of
        {ok, BranchId} ->
            %% Token is part of a parallel branch, increment join counter
            NewJoinCounters = increment_join_counter(BranchId, ExecState, UpdatedToken#token.value),
            {Tokens, NewJoinCounters};
        error ->
            %% Not part of a parallel branch
            {Tokens, ExecState#exec_state.join_counters}
    end,
    ...

%% AFTER:
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Check if token is MI instance
    case Token#token.instance_id of
        undefined ->
            %% Not an MI instance, use existing branch logic
            {UpdatedTokens2, UpdatedJoinCounters} = case find_branch_for_token(CurrentToken, ExecState#exec_state.branch_map) of
                {ok, BranchId} ->
                    NewJoinCounters = increment_join_counter(BranchId, ExecState, UpdatedToken#token.value),
                    {Tokens, NewJoinCounters};
                error ->
                    {Tokens, ExecState#exec_state.join_counters}
            end,

            %% Check if all tokens complete
            ActiveTokens = [T || T <- maps:values(UpdatedTokens2), T#token.status =:= active],
            case ActiveTokens of
                [] ->
                    ExecState#exec_state{
                        tokens = UpdatedTokens2,
                        join_counters = UpdatedJoinCounters,
                        status = done,
                        step_count = ExecState#exec_state.step_count + 1
                    };
                _ ->
                    NextToken = select_next_token(UpdatedTokens2),
                    ExecState#exec_state{
                        tokens = UpdatedTokens2,
                        join_counters = UpdatedJoinCounters,
                        current_token = NextToken,
                        step_count = ExecState#exec_state.step_count + 1
                    }
            end;
        InstanceId ->
            %% MI instance, collect result via wf_mi
    ExecStateWithTokens = ExecState#exec_state{tokens = Tokens},
            {ExecState2, JoinSatisfied} = wf_mi:collect_result(
                InstanceId, UpdatedToken#token.value, ExecStateWithTokens
            ),

            %% If join satisfied and policy is wait_n or first_complete, cancel remaining
            FinalExecState = case JoinSatisfied of
                true ->
                    %% Check join policy (from MI config in branch or join counter)
                    JoinId = find_join_id_for_instance(InstanceId, ExecState2),
                    JoinCounter = maps:get(JoinId, ExecState2#exec_state.join_counters),
                    Policy = JoinCounter#join_counter.policy,

                    case Policy of
                        {first_n, _N} ->
                            wf_mi:cancel_remaining(InstanceId, ExecState2);
                        first_complete ->
                            wf_mi:cancel_remaining(InstanceId, ExecState2);
                        _Other ->
                            ExecState2
                    end;
                false ->
                    ExecState2
            end,

            %% Check if all tokens complete
            ActiveTokens = [T || T <- maps:values(FinalExecState#exec_state.tokens),
                                T#token.status =:= active],
            case ActiveTokens of
                [] ->
                    FinalExecState#exec_state{
                        status = done,
                        step_count = FinalExecState#exec_state.step_count + 1
                    };
                _ ->
                    NextToken = select_next_token(FinalExecState#exec_state.tokens),
                    FinalExecState#exec_state{
                        current_token = NextToken,
                        step_count = FinalExecState#exec_state.step_count + 1
                    }
            end
    end.

%% @doc Find join ID for instance (helper)
find_join_id_for_instance(InstanceId, ExecState) ->
    {ok, BranchId} = wf_mi:find_branch_for_instance(
        InstanceId, ExecState#exec_state.branch_map, ExecState#exec_state.tokens
    ),
    BranchInfo = maps:get(BranchId, ExecState#exec_state.branch_map),
    BranchInfo#branch_info.join_id.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit test: collect_result/3 increments join counter
- [ ] Unit test: collect_result/3 returns JoinSatisfied = true when completed >= required
- [ ] Integration test: MI instance completion increments join counter
- [ ] Integration test: MI wait_all completes after all instances done
- [ ] Integration test: MI wait_n completes after N instances done
- [ ] Integration test: MI first_complete completes after first instance done
- [ ] Existing PAR_FORK tests still pass (non-MI tokens use existing logic)

##### Manual Verification:

- [ ] MI instances detected by instance_id field (not undefined)
- [ ] Non-MI instances (instance_id = undefined) use existing branch logic
- [ ] Join counter incremented correctly for MI instances
- [ ] Join satisfaction detected correctly

**Test Coverage** (add to test/wf_mi_tests.erl and test/wf_exec_tests.erl):

```erlang
%% Unit test: collect_result increments join counter
collect_result_test_() ->
    %% Create exec state with MI instances
    ExecState0 = create_mi_state({fixed, 2}, wait_all),

    %% First instance completes
    {ExecState1, JoinSat1} = wf_mi:collect_result(i1, result1, ExecState0),
    ?assertEqual(false, JoinSat1),
    ?assertEqual(1, get_join_completed(ExecState1)),

    %% Second instance completes
    {ExecState2, JoinSat2} = wf_mi:collect_result(i2, result2, ExecState1),
    ?assertEqual(true, JoinSat2),
    ?assertEqual(2, get_join_completed(ExecState2)).

%% Integration test: MI execution with wait_all
mi_execution_wait_all_test_() ->
    Bytecode = mock_bytecode_mi_fixed(),
    ExecState0 = wf_exec:new(Bytecode),

    %% Run until done
    Result = wf_exec:run(ExecState0, 100, undefined),
    ?assertMatch({done, _ExecState}, Result),
    {done, ExecStateDone} = Result,

    %% Verify both instances completed
    Tokens = ExecStateDone#exec_state.tokens,
    ?assertEqual(0, map_size(Tokens)).  %% All tokens complete
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Join Checking and Cancellation

#### Overview

Implement join checking and cancellation of remaining instances. When join satisfied early (wait_n, first_complete), cancel remaining instances by setting token status to cancelled. This completes MI execution logic.

#### Changes Required:

##### 1. Implement check_join/2 in wf_mi

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl`
**Changes**: Add join checking function (line ~120)

```erlang
%% @doc Check if join policy is satisfied
%% Returns {ok, satisfied} | {ok, not_satisfied} | {error, no_active_join}
-spec check_join(wf_exec:exec_state()) ->
    {ok, satisfied | not_satisfied} | {error, no_active_join}.
check_join(ExecState) ->
    JoinCounters = ExecState#exec_state.join_counters,
    case maps:to_list(JoinCounters) of
        [{_JoinId, #join_counter{completed = Completed, required = Required}}] ->
            case Completed >= Required of
                true -> {ok, satisfied};
                false -> {ok, not_satisfied}
            end;
        [] ->
            {error, no_active_join};
        _Multiple ->
            {error, multiple_active_joins}
    end.
```

##### 2. Implement cancel_remaining/2 in wf_mi

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl`
**Changes**: Add cancellation function (line ~135)

```erlang
%% @doc Cancel remaining instances when join satisfied early
%% Finds all instances in the same MI group, marks them as cancelled
-spec cancel_remaining(term(), wf_exec:exec_state()) -> wf_exec:exec_state().
cancel_remaining(CompletedInstanceId, ExecState) ->
    BranchMap = ExecState#exec_state.branch_map,
    Tokens = ExecState#exec_state.tokens,

    %% Find branch for completed instance
    case wf_mi:find_branch_for_instance(CompletedInstanceId, BranchMap, Tokens) of
        {ok, BranchId} ->
            BranchInfo = maps:get(BranchId, BranchMap),
            AllTokenIds = BranchInfo#branch_info.tokens,

            %% Find all active instances (excluding completed one)
            ActiveTokenIds = lists:filter(fun(TokenId) ->
                Token = maps:get(TokenId, Tokens),
                Token#token.status =:= active andalso
                Token#token.instance_id =/= CompletedInstanceId
            end, AllTokenIds),

            %% Mark remaining instances as cancelled
            UpdatedTokens = lists:foldl(fun(TokenId, AccTokens) ->
                Token = maps:get(TokenId, AccTokens),
                AccTokens#{TokenId => Token#token{status = cancelled}}
            end, Tokens, ActiveTokenIds),

            ExecState#exec_state{tokens = UpdatedTokens};
        error ->
            %% Instance not found (should not happen)
            ExecState
    end.
```

##### 3. Add helper function to expose find_branch_for_instance

**File**: `/Users/speed/wf-substrate/src/wf_mi.erl`
**Changes**: Export find_branch_for_instance/3 as internal API

```erlang
%% Export find_branch_for_instance for use by execute_done/2
-export([find_branch_for_instance/3]).

%% @doc Find branch for a given instance ID
-spec find_branch_for_instance(term(), #{term() => #branch_info{}}, #{term() => #token{}}) ->
    {ok, term()} | error.
find_branch_for_instance(InstanceId, BranchMap, Tokens) ->
    %% Find token with this instance_id
    TokenId = case lists:search(fun({_Tid, Token}) ->
        Token#token.instance_id =:= InstanceId
    end, maps:to_list(Tokens)) of
        {value, {Tid, _Token}} -> Tid;
        false -> error
    end,

    case TokenId of
        error -> error;
        _ ->
            %% Find branch containing this token
            find_branch_for_token(TokenId, BranchMap)
    end.

%% @doc Find branch for a given token ID (internal, copied from wf_exec)
-spec find_branch_for_token(term(), #{term() => #branch_info{}}) ->
    {ok, term()} | error.
find_branch_for_token(TokenId, BranchMap) ->
    case [BranchId || {BranchId, #branch_info{tokens = Tokens}} <- maps:to_list(BranchMap),
                      lists:member(TokenId, Tokens)] of
        [BranchId | _] -> {ok, BranchId};
        [] -> error
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit test: check_join/2 returns satisfied when completed >= required
- [ ] Unit test: check_join/2 returns not_satisfied when completed < required
- [ ] Unit test: cancel_remaining/2 marks remaining instances as cancelled
- [ ] Integration test: MI wait_n cancels remaining instances after N complete
- [ ] Integration test: MI first_complete cancels all other instances after first complete
- [ ] Integration test: Cancelled instances do NOT increment join counter
- [ ] Integration test: Cancelled instances removed from active tokens

##### Manual Verification:

- [ ] Cancelled instances have status = cancelled
- [ ] select_next_token/2 skips cancelled tokens (status =:= active check)
- [ ] Join counter not incremented for cancelled instances
- [ ] Fire-and-forget MI (none policy) never calls cancel_remaining/2

**Test Coverage** (add to test/wf_mi_tests.erl):

```erlang
%% Unit test: check_join
check_join_satisfied_test_() ->
    ExecState = create_mi_state_with_counter({fixed, 3}, wait_all, 2),  %% 2 of 3 completed
    ?assertEqual({ok, not_satisfied}, wf_mi:check_join(ExecState)),

    ExecState2 = create_mi_state_with_counter({fixed, 3}, wait_all, 3),  %% 3 of 3 completed
    ?assertEqual({ok, satisfied}, wf_mi:check_join(ExecState2)).

%% Unit test: cancel_remaining
cancel_remaining_test_() ->
    %% Create MI with wait_n (wait for 2 of 3)
    ExecState0 = create_mi_state({fixed, 3}, {wait_n, 2}),

    %% Complete 2 instances
    {ExecState1, _} = wf_mi:collect_result(i1, result1, ExecState0),
    {ExecState2, JoinSat} = wf_mi:collect_result(i2, result2, ExecState1),
    ?assertEqual(true, JoinSat),

    %% Cancel remaining
    ExecState3 = wf_mi:cancel_remaining(i2, ExecState2),

    %% Verify i3 cancelled
    Token3 = get_token_for_instance(i3, ExecState3),
    ?assertEqual(cancelled, Token3#token.status).

%% Integration test: MI wait_n
mi_wait_n_test_() ->
    Bytecode = [
        {'MI_SPAWN', {{fixed, 3}, {wait_n, 2}, 2}},  %% Spawn 3, wait for 2
        {'TASK_EXEC', task},
        {'DONE'},
        {'JOIN_WAIT', {wait_n, 2}}
    ],
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecStateDone} = wf_exec:run(ExecState0, 100, undefined),

    %% Verify 1 instance cancelled, 2 completed
    Tokens = maps:values(ExecStateDone#exec_state.tokens),
    CancelledCount = lists:foldl(fun(T, Acc) ->
        case T#token.status of
            cancelled -> Acc + 1;
            complete -> Acc
        end
    end, 0, Tokens),
    ?assertEqual(1, CancelledCount).
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Comprehensive Testing

#### Overview

Add comprehensive test coverage for all MI functionality. Unit tests for each wf_mi function, integration tests for MI execution, property tests for invariants, and performance tests for scalability.

#### Changes Required:

##### 1. Create complete test suite

**File**: `/Users/speed/wf-substrate/test/wf_mi_tests.erl` (NEW FILE)
**Changes**: Create comprehensive test module

```erlang
-module(wf_mi_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Unit Tests: eval_instance_count
%%====================================================================

eval_instance_count_fixed_positive_test() ->
    ?assertEqual({ok, 5}, wf_mi:eval_instance_count({fixed, 5}, #{})).

eval_instance_count_fixed_zero_test() ->
    ?assertEqual({error, zero_instances}, wf_mi:eval_instance_count({fixed, 0}, #{})).

eval_instance_count_dynamic_valid_test() ->
    ?assertEqual({ok, 2}, wf_mi:eval_instance_count({dynamic, 2, 10}, #{})).

eval_instance_count_dynamic_invalid_range_test() ->
    ?assertEqual({error, {invalid_range, 10, 2}},
                 wf_mi:eval_instance_count({dynamic, 10, 2}, #{})).

eval_instance_count_dynamic_zero_min_test() ->
    ?assertEqual({error, zero_minimum},
                 wf_mi:eval_instance_count({dynamic, 0, 10}, #{})).

%%====================================================================
%% Unit Tests: spawn_instances
%%====================================================================

spawn_instances_fixed_test() ->
    {ok, Instances, Config} = wf_mi:spawn_instances({fixed, 3}, wait_all, 10, root),
    ?assertEqual(3, length(Instances)),
    ?assertEqual(3, length(Config#mi_config.instance_ids)).

spawn_instances_dynamic_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({dynamic, 2, 10}, wait_all, 10, root),
    ?assertEqual(2, length(Instances)).

spawn_instances_too_many_test() ->
    ?assertError({too_many_instances, 1001, 1000},
                 wf_mi:spawn_instances({fixed, 1001}, wait_all, 10, root)).

spawn_instances_unique_ids_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, 100}, wait_all, 10, root),
    Ids = [I#mi_instance.instance_id || I <- Instances],
    ?assertEqual(100, length(lists:usort(Ids))).

spawn_instances_sequential_numbers_test() ->
    {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, 5}, wait_all, 10, root),
    Nums = [I#mi_instance.instance_num || I <- Instances],
    ?assertEqual([1, 2, 3, 4, 5], lists:sort(Nums)).

%%====================================================================
%% Unit Tests: collect_result
%%====================================================================

collect_result_increment_test() ->
    ExecState0 = create_mi_state({fixed, 2}, wait_all),
    {ExecState1, JoinSat1} = wf_mi:collect_result(i1, result1, ExecState0),
    ?assertEqual(false, JoinSat1),
    ?assertEqual(1, get_join_completed(ExecState1)).

collect_result_satisfied_test() ->
    ExecState0 = create_mi_state({fixed, 2}, wait_all),
    {ExecState1, _} = wf_mi:collect_result(i1, result1, ExecState0),
    {ExecState2, JoinSat2} = wf_mi:collect_result(i2, result2, ExecState1),
    ?assertEqual(true, JoinSat2),
    ?assertEqual(2, get_join_completed(ExecState2)).

collect_result_fire_and_forget_test() ->
    ExecState0 = create_mi_state({fixed, 2}, none),
    {ExecState1, JoinSat} = wf_mi:collect_result(i1, result1, ExecState0),
    ?assertEqual(false, JoinSat),
    ?assertEqual(0, map_size(ExecState1#exec_state.join_counters)).

%%====================================================================
%% Unit Tests: check_join
%%====================================================================

check_join_not_satisfied_test() ->
    ExecState = create_mi_state_with_counter({fixed, 3}, wait_all, 2),
    ?assertEqual({ok, not_satisfied}, wf_mi:check_join(ExecState)).

check_join_satisfied_test() ->
    ExecState = create_mi_state_with_counter({fixed, 3}, wait_all, 3),
    ?assertEqual({ok, satisfied}, wf_mi:check_join(ExecState)).

check_join_no_active_join_test() ->
    ExecState = create_mi_state({fixed, 2}, none),
    ?assertEqual({error, no_active_join}, wf_mi:check_join(ExecState)).

%%====================================================================
%% Unit Tests: cancel_remaining
%%====================================================================

cancel_remaining_test() ->
    ExecState0 = create_mi_state({fixed, 3}, {wait_n, 2}),
    {ExecState1, _} = wf_mi:collect_result(i1, result1, ExecState0),
    {ExecState2, JoinSat} = wf_mi:collect_result(i2, result2, ExecState1),
    ?assertEqual(true, JoinSat),

    ExecState3 = wf_mi:cancel_remaining(i2, ExecState2),
    Token3 = get_token_for_instance(i3, ExecState3),
    ?assertEqual(cancelled, Token3#token.status).

%%====================================================================
%% Property Tests
%%====================================================================

prop_instance_count_correct() ->
    ?FORALL(N, pos_integer(), N =< 100,
        begin
            {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, N}, wait_all, 10, root),
            length(Instances) =:= N
        end).

prop_instance_ids_unique() ->
    ?FORALL(N, pos_integer(), N =< 50,
        begin
            {ok, Instances, _Config} = wf_mi:spawn_instances({fixed, N}, wait_all, 10, root),
            Ids = [I#mi_instance.instance_id || I <- Instances],
            length(Ids) =:= length(lists:usort(Ids))
        end).

prop_join_satisfied_after_n_complete() ->
    ?FORALL({M, N}, {pos_integer(), pos_integer()},
        M =< 10 andalso N =< M,
        begin
            MIJoinPolicy = {wait_n, N},
            ExecState = complete_n_instances(N, M, MIJoinPolicy),
            {ok, Result} = wf_mi:check_join(ExecState),
            Result =:= satisfied
        end).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create test exec state with MI instances
create_mi_state(MIPolicy, MIJoinPolicy) ->
    {ok, Instances, _Config} = wf_mi:spawn_instances(MIPolicy, MIJoinPolicy, 10, root),
    Tokens = lists:map(fun(I) ->
        Tid = I#mi_instance.token_id,
        {Tid, #token{
            token_id = Tid,
            ip = 10,
            scope_id = root,
            value = #{instance_id => I#mi_instance.instance_id},
            status = active,
            instance_id = I#mi_instance.instance_id
        }}
    end, Instances),

    BranchId = make_ref(),
    TokenIds = [Tid || {Tid, _} <- Tokens],
    JoinId = case MIJoinPolicy of
        none -> undefined;
        _ -> make_ref()
    end,

    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = TokenIds,
        join_id = JoinId,
        targets = [10]
    },

    JoinCounters = case JoinId of
        undefined -> #{};
        _ ->
            Required = case MIJoinPolicy of
                wait_all -> length(Instances);
                {wait_n, N} -> N;
                first_complete -> 1
            end,
            #{JoinId => #join_counter{
                join_id = JoinId,
                completed = 0,
                required = Required,
                policy = all,
                results = []
            }}
    end,

    #exec_state{
        ip = 0,
        bytecode = [],
        ctx = #{},
        tokens = maps:from_list(Tokens),
        branch_map = #{BranchId => BranchInfo},
        join_counters = JoinCounters,
        scope_stack = [root],
        step_count = 0,
        status = running,
        current_token = hd(TokenIds)
    }.

%% Create test exec state with partial completion
create_mi_state_with_counter(MIPolicy, MIJoinPolicy, Completed) ->
    ExecState0 = create_mi_state(MIPolicy, MIJoinPolicy),
    InstanceIds = [I#mi_instance.instance_id || I <- element(2, wf_mi:spawn_instances(MIPolicy, MIJoinPolicy, 10, root))],

    %% Complete N instances
    {ExecState, _} = lists:foldl(fun(N, {AccES, AccIds}) ->
        [Id | Rest] = AccIds,
        {ES, _} = wf_mi:collect_result(Id, result, AccES),
        {ES, Rest}
    end, {ExecState0, InstanceIds}, lists:seq(1, Completed)),

    ExecState.

%% Complete N instances
complete_n_instances(N, M, MIJoinPolicy) ->
    ExecState0 = create_mi_state({fixed, M}, MIJoinPolicy),
    {ok, Instances, _} = wf_mi:spawn_instances({fixed, M}, MIJoinPolicy, 10, root),
    Ids = [I#mi_instance.instance_id || I <- lists:sublist(Instances, N)],

    {ExecState, _} = lists:foldl(fun(Id, {AccES, _AccIdx}) ->
        {ES, _} = wf_mi:collect_result(Id, result, AccES),
        {ES, undefined}
    end, {ExecState0, undefined}, Ids),

    ExecState.

%% Get join counter completed count
get_join_completed(ExecState) ->
    [{_JoinId, JoinCounter}] = maps:to_list(ExecState#exec_state.join_counters),
    JoinCounter#join_counter.completed.

%% Get token for instance ID
get_token_for_instance(InstanceId, ExecState) ->
    Tokens = maps:values(ExecState#exec_state.tokens),
    case lists:search(fun(T) -> T#token.instance_id =:= InstanceId end, Tokens) of
        {value, Token} -> Token;
        false -> error({token_not_found, InstanceId})
    end.
```

##### 2. Add integration tests to wf_exec_tests.erl

**File**: `/Users/speed/wf-substrate/test/wf_exec_tests.erl`
**Changes**: Add MI execution tests (after PAR_FORK tests)

```erlang
%%====================================================================
%% Multiple Instance Tests
%%====================================================================

%% Mock MI bytecode
mock_bytecode_mi_wait_all() ->
    [
        {'MI_SPAWN', {{fixed, 2}, wait_all, 2}},
        {'TASK_EXEC', task},
        {'DONE'},
        {'JOIN_WAIT', wait_all}
    ].

mock_bytecode_mi_wait_n() ->
    [
        {'MI_SPAWN', {{fixed, 3}, {wait_n, 2}, 2}},
        {'TASK_EXEC', task},
        {'DONE'},
        {'JOIN_WAIT', {wait_n, 2}}
    ].

mock_bytecode_mi_first_complete() ->
    [
        {'MI_SPAWN', {{fixed, 3}, first_complete, 2}},
        {'TASK_EXEC', task},
        {'DONE'},
        {'JOIN_WAIT', first_complete}
    ].

mock_bytecode_mi_fire_and_forget() ->
    [
        {'MI_SPAWN', {{fixed, 2}, none, 1}},
        {'TASK_EXEC', task},
        {'DONE'}
    ].

%% Test MI_SPAWN spawns instances
mi_spawn_test_() ->
    Bytecode = mock_bytecode_mi_wait_all(),
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),

    [
        ?_assertEqual(2, map_size(ExecState1#exec_state.tokens)),
        ?_assertEqual(1, map_size(ExecState1#exec_state.join_counters))
    ].

%% Test MI wait_all execution
mi_execution_wait_all_test_() ->
    Bytecode = mock_bytecode_mi_wait_all(),
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 100, undefined),

    [
        ?assertMatch({done, _ExecState}, Result)
    ].

%% Test MI wait_n cancels remaining
mi_execution_wait_n_test_() ->
    Bytecode = mock_bytecode_mi_wait_n(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecStateDone} = wf_exec:run(ExecState0, 100, undefined),

    Tokens = maps:values(ExecStateDone#exec_state.tokens),
    CancelledCount = lists:foldl(fun(T, Acc) ->
        case T#token.status of
            cancelled -> Acc + 1;
            complete -> Acc
        end
    end, 0, Tokens),

    [
        ?_assertEqual(1, CancelledCount)
    ].

%% Test MI first_complete cancels all others
mi_execution_first_complete_test_() ->
    Bytecode = mock_bytecode_mi_first_complete(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecStateDone} = wf_exec:run(ExecState0, 100, undefined),

    Tokens = maps:values(ExecStateDone#exec_state.tokens),
    CancelledCount = lists:foldl(fun(T, Acc) ->
        case T#token.status of
            cancelled -> Acc + 1;
            complete -> Acc
        end
    end, 0, Tokens),

    [
        ?_assertEqual(2, CancelledCount)
    ].

%% Test MI fire-and-forget
mi_execution_fire_and_forget_test_() ->
    Bytecode = mock_bytecode_mi_fire_and_forget(),
    ExecState0 = wf_exec:new(Bytecode),
    {done, ExecStateDone} = wf_exec:run(ExecState0, 100, undefined),

    [
        ?_assertEqual(0, map_size(ExecStateDone#exec_state.join_counters))
    ].
```

##### 3. Add performance benchmarks

**File**: `/Users/speed/wf-substrate/test/wf_mi_bench_tests.erl` (NEW FILE)
**Changes**: Create performance benchmarks

```erlang
-module(wf_mi_bench_tests).
-include_lib("eunit/include/eunit.hrl").

%% Benchmark MI spawn overhead
bench_mi_spawn_test_() ->
    {
        foreach,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(bench_spawn_10()),
            ?_test(bench_spawn_100()),
            ?_test(bench_spawn_1000())
        ]
    }.

setup() ->
    ok.

cleanup(_State) ->
    ok.

bench_spawn_10() ->
    {Time, _} = timer:tc(fun() ->
        wf_mi:spawn_instances({fixed, 10}, wait_all, 100, root)
    end),
    ?debugFmt("Spawn 10 instances: ~p us", [Time]),
    ?assert(Time < 1000).  %% < 1ms

bench_spawn_100() ->
    {Time, _} = timer:tc(fun() ->
        wf_mi:spawn_instances({fixed, 100}, wait_all, 100, root)
    end),
    ?debugFmt("Spawn 100 instances: ~p us", [Time]),
    ?assert(Time < 10000).  %% < 10ms

bench_spawn_1000() ->
    {Time, _} = timer:tc(fun() ->
        wf_mi:spawn_instances({fixed, 1000}, wait_all, 100, root)
    end),
    ?debugFmt("Spawn 1000 instances: ~p us", [Time]),
    ?assert(Time < 100000).  %% < 100ms
```

#### Success Criteria:

##### Automated Verification:

- [ ] All wf_mi unit tests pass (eval_instance_count, spawn_instances, collect_result, check_join, cancel_remaining)
- [ ] All wf_exec integration tests pass (MI_SPAWN, MI execution with all policies)
- [ ] Property tests pass (instance_count_correct, instance_ids_unique, join_satisfied_after_n_complete)
- [ ] Performance benchmarks meet targets (< 1ms for 10 instances, < 100ms for 1000)
- [ ] Code coverage > 90% for wf_mi module
- [ ] All existing wf_exec tests still pass (backward compatibility)

##### Manual Verification:

- [ ] Test execution completes in reasonable time (< 30 seconds)
- [ ] No test failures or flaky tests
- [ ] Memory usage stable (no leaks in spawn/cancel cycles)
- [ ] Code is well-documented (comments, types)

**Note**: Complete all automated verification, then pause for manual confirmation. This completes the implementation.

---

## Testing Strategy

### Unit Tests:

**wf_mi module**:
- `eval_instance_count/2`: Fixed (positive N, zero N), dynamic (valid range, invalid range, zero min)
- `spawn_instances/4`: Fixed count (N instances), dynamic count (Min instances), too many instances (error)
- `collect_result/3`: Join counter increment, join satisfaction detection, fire-and-forget (no join)
- `check_join/2`: Satisfied (completed >= required), not_satisfied (completed < required), no_active_join
- `cancel_remaining/2`: Remaining instances marked cancelled, completed instance not cancelled

**wf_exec module**:
- `execute_mi_spawn/2`: MI_SPAWN spawns N tokens, creates join counter (sync), no join counter (fire-and-forget)
- Token record: instance_id field defaults to undefined (backward compatibility)
- `execute_done/2`: MI instances detected (instance_id /= undefined), non-MI use existing logic

**Key edge cases**:
- Zero instance count (error)
- Too many instances (> 1000, error)
- Dynamic count with Min > Max (error)
- MI with N=1 (single instance, still spawns correctly)
- MI with wait_n where N > M (cap at M)
- First instance completes (cancellation of remaining)
- All instances complete (no cancellation needed)
- Fire-and-forget instances (no join tracking)

### Integration Tests:

**End-to-end scenarios**:
1. **MI wait_all**: Spawn 3 instances, all complete, join satisfied
2. **MI wait_n**: Spawn 3 instances, wait for 2, 1 cancelled
3. **MI first_complete**: Spawn 3 instances, first completes, 2 cancelled
4. **MI fire-and-forget**: Spawn 2 instances, no join, both run independently
5. **MI dynamic count**: Spawn dynamic instances, verify Min count created
6. **MI with PAR_FORK**: MI instances inside PAR_FORK branches (nested parallelism)
7. **MI cancellation**: Cancel MI scope during execution (integration with wf_cancel, v2)

**Result collection and merging**:
- Verify join counter results list populated correctly
- Verify merge_results/2 called with correct policy
- Verify continuation token created with merged results

### Property-Based Tests:

**Invariants**:
- `prop_instance_count_correct`: Fixed N spawns exactly N instances
- `prop_instance_ids_unique`: All instance IDs are distinct
- `prop_join_satisfied_after_n_complete`: Join satisfied after N of M complete (wait_n policy)
- `prop_cancelled_instances_not_active`: Cancelled instances have status = cancelled, not active
- `prop_fire_and_forget_no_join`: Fire-and-forget MI has no join counter

### Manual Testing Steps:

1. **Compile and run tests**:
   ```bash
   rebar3 compile
   rebar3 eunit
   rebar3 proper
   ```

2. **Verify MI execution manually**:
   - Create MI bytecode with fixed count
   - Execute step-by-step, inspect token state
   - Verify instance tokens created with correct instance_id
   - Verify join counter created and incremented
   - Verify join satisfaction detected
   - Verify remaining instances cancelled (wait_n, first_complete)

3. **Performance verification**:
   - Run benchmarks: `rebar3 eunit -module wf_mi_bench_tests`
   - Verify spawn overhead < 1ms for 10 instances
   - Verify spawn overhead < 100ms for 1000 instances

4. **Memory leak verification**:
   - Spawn/cancel cycle repeated 1000 times
   - Monitor memory usage (should be stable)
   - Verify no orphaned tokens or join counters

## Migration Notes

**Backward Compatibility**:

1. **Token record change**: Adding `instance_id` field with default `undefined` is backward compatible. All existing token construction sites updated to include `instance_id = undefined`.

2. **wf_exec changes**: Existing non-MI workflows continue to work. MI_SPAWN is a new opcode, existing workflows don't use it. execute_done/2 checks `instance_id =:= undefined` to detect MI instances vs non-MI.

3. **No data migration**: All changes are in-memory (exec_state). No persistent state changes.

**Rollback Strategy**:

If a phase fails:
1. Revert changes to affected files
2. Fix issues identified by tests
3. Re-run verification before proceeding

If entire implementation fails:
1. Revert all changes (wf_mi.erl, wf_exec.erl, test files)
2. Keep wf_exec.erl token record change (backward compatible, harmless)
3. Re-implement with corrected approach

**Future Enhancements** (v2, out of scope for v1):
- Dynamic count evaluation with user function Fun :: ctx() → pos_integer()
- wf_cancel integration for instance cancellation
- Compiler integration (wf_compile support for mi/2 pattern)
- State persistence via wf_state for crash recovery
- Instance failure handling (distinguish complete vs failed)
- Stable result ordering by instance_num
- Configurable maximum instance count
- Instance ID → token_id mapping for O(1) lookup
- Advanced MI patterns (deferred choice MI, pipelined MI, recursive MI)

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/009-multiple-instance-support/research.md`
- Item specification: `/Users/speed/wf-substrate/.wreckit/items/009-multiple-instance-support/item.json`
- wf_exec executor: `/Users/speed/wf-substrate/src/wf_exec.erl` (lines 28-34: token record, 259-323: PAR_FORK, 325-393: JOIN_WAIT, 536-573: execute_done)
- wf_vm types: `/Users/speed/wf-substrate/src/wf_vm.erl` (lines 37-40: mi_policy type)
- wf_state state store: `/Users/speed/wf-substrate/src/wf_state.erl` (lines 76-82: token record, 84-91: scope record)
- PROMPT specification: `/Users/speed/wf-substrate/PROMPT.md` (lines 58, 90, 118: MI pattern requirements)
- Cancellation research: `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/research.md`
- Workflow Patterns (WP16): van der Aalst workflow pattern catalog - Multi-Instance patterns
