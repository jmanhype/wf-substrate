# Operational Semantics of wf_substrate

## Table of Contents

1. [Introduction](#1-introduction)
2. [Execution State Definition](#2-execution-state-definition)
3. [Reduction Rule Notation](#3-reduction-rule-notation)
4. [Kernel Primitive Reduction Rules](#4-kernel-primitive-reduction-rules)
5. [Structural Rules](#5-structural-rules)
6. [Examples](#6-examples)
7. [References](#7-references)

---

## 1. Introduction

wf_substrate executes workflow patterns compiled to bytecode. This document specifies the **small-step operational semantics** of the 9 kernel primitives using inference rules. Each rule defines how one execution state reduces to another.

### 1.1 Scope

This document covers **kernel primitives only**:

1. **task** - Execute a function (pure or effect-yielding)
2. **seq** - Sequential composition (left then right)
3. **par** - Parallel fork (spawn all branches)
4. **xor** - Exclusive choice (enable one branch, disable others)
5. **join** - Join synchronization (wait-all, wait-n, first-complete policies)
6. **loop** - Structured loop (while, until, count policies)
7. **defer** - Deferred evaluation (race on external event)
8. **cancel** - Cancel scope (enter, propagate, exit)
9. **mi** - Multiple instances (spawn N, collect results, apply join policy)

**Derived patterns** (wf_core.erl) expand to kernel primitives, so their semantics are derived from these rules.

### 1.2 Assumptions

- **Bytecode execution**: Rules assume bytecode compilation (wf_compile) has resolved all structural decisions
- **Explicit state**: Execution state is explicit (IP, tokens, scopes), not implicit in AST structure
- **Determinism**: Under deterministic scheduler, same term + same ctx → same trace
- **Atomic operations**: Each reduction rule is atomic (no interleaving)

### 1.3 Related Documents

- **docs/ARCHITECTURE.md**: System architecture and bytecode design
- **PROMPT.md:97-136**: Kernel types and primitives definition
- **Item 002 plan.md**: Pattern term algebra types

---

## 2. Execution State Definition

### 2.1 Configuration

A **configuration** is a pair ⟨P, σ⟩ where:

- **P** is the current program (bytecode instruction at current IP)
- **σ** is the execution state

We write reductions as: ⟨P, σ⟩ → ⟨P', σ'⟩

### 2.2 Execution State σ

The execution state σ is a tuple:

```
σ = (ctx, tokens, scopes, ip, branch_map, join_counters, step_count)
```

**Components**:

| Component | Type | Description |
|-----------|------|-------------|
| **ctx** | ctx() | User-provided context map (opaque to engine) |
| **tokens** | set(token) | Active tokens (logical threads of execution) |
| **scopes** | map(scope_id, scope_metadata) | Cancel scopes with nesting |
| **ip** | non_neg_integer() | Instruction pointer (index into bytecode) |
| **branch_map** | map(branch_id, branch_info) | Active parallel branches |
| **join_counters** | map(join_id, join_counter) | Join synchronization state |
| **step_count** | non_neg_integer() | Number of reductions executed |

### 2.3 Token Structure

Each **token** represents a logical thread of execution:

```
token = {token_id, ip, scope_id, value}
```

**Fields**:

- **token_id**: Unique identifier (auto-generated, e.g., ref)
- **ip**: Current instruction pointer for this token
- **scope_id**: Active cancellation scope
- **value**: Accumulated result from completed operations
- **status**: active | complete | cancelled (implicit in rules)

**Invariant**: Number of active tokens tracks logical parallelism.

### 2.4 Scope Structure

**Cancel scopes** track nesting and cancellation status:

```
scope_metadata = #{status := active | cancelled, parent => scope_id}
```

**Fields**:

- **status**: Current status of the scope
- **parent**: Parent scope ID (for nesting)

**Scopes Nest Properly**: If scope A encloses scope B, then A is the parent of B. Scopes never cross.

### 2.5 Branch Map

The **branch_map** tracks active parallel branches:

```
branch_info = #{tokens := [token_id()], targets := [ip()], join_id := join_id()}
```

**Fields**:

- **tokens**: List of active tokens in this branch
- **targets**: IP targets for each branch
- **join_id**: Associated join point (where branches merge)

### 2.6 Join Counters

**Join counters** track synchronization state:

```
join_counter = #{completed := N, required := M, policy := join_policy(), results := [term()]}
```

**Fields**:

- **completed**: Number of branches that have completed
- **required**: Number required for join policy
- **policy**: Join policy (all, first_n, n_of_m, first_complete)
- **results**: Accumulated results from completed branches

### 2.7 Initial State

For a compiled bytecode B and initial context Ctx:

```
σ₀ = (Ctx, {token₀}, {root ⇒ #{status => active}}, 0, {}, {}, 0)
```

Where token₀ = {auto_gen(), 0, root, undefined}

### 2.8 Terminal States

A configuration is **terminal** when no further reductions are possible:

- **Done**: No active tokens, all branches complete, join counters empty
- **Cancelled**: All tokens marked cancelled
- **Blocked**: Waiting for external signal or effect result (not terminal, just suspended)

---

## 3. Reduction Rule Notation

### 3.1 Inference Rule Format

Rules are written in the form:

```
premise
---------
conclusion
```

Or with multiple premises:

```
premise₁    premise₂    premise₃
----------------------------------
           conclusion
```

**Example**:

```
⟨TASK_EXEC a, (ctx, tokens, scopes, ip, ...)⟩
    where task_a(ctx) = {ok, ctx'}
---------------------------------------------------------------
⟨next_opcode, (ctx', tokens, scopes, ip+1, ...)⟩
```

### 3.2 Configuration Reduction

A **reduction step** transforms one configuration to another:

```
⟨P, σ⟩ → ⟨P', σ'⟩
```

We write **→*** for zero or more reductions:
```
⟨P, σ⟩ →* ⟨P', σ'⟩
```

### 3.3 Rule Names

Each rule has a descriptive name:

- **TASK-EXEC**: Execute a pure task
- **TASK-EFFECT**: Task yields effect
- **SEQ-LEFT**: Start sequence (execute left)
- **SEQ-RIGHT**: Continue sequence (left done, execute right)
- **PAR-FORK**: Fork parallel branches
- **XOR-CHOOSE**: Choose exclusive branch
- **JOIN-ALL**: Wait for all branches
- **LOOP-WHILE**: Check while condition
- **DEFER-SELECT**: External event selects branch
- **CANCEL-PROPAGATE**: Propagate cancellation
- **MI-SPAWN-FIXED**: Spawn fixed number of instances

### 3.4 Notation Legend

| Symbol | Meaning |
|--------|---------|
| ⟨P, σ⟩ | Configuration (program P, state σ) |
| → | Single reduction step |
| →* | Zero or more reduction steps |
| ctx[i] | Context value after i reductions |
| ip[i] | Instruction pointer after i reductions |
| token ∈ tokens | Token is member of token set |
| σ' | Updated state (prime notation) |
| ... | Other state components unchanged |

---

## 4. Kernel Primitive Reduction Rules

### 4.1 Task Execution

Tasks are the leaf operations of workflows. They can be **pure** (return {ok, ctx'}) or **effect-yielding** (return {effect, Spec, ContCtx}).

#### TASK-EXEC (Pure Task)

Execute a pure task function that doesn't yield effects.

```
⟨TASK_EXEC name, (ctx, tokens, scopes, ip, ...)⟩
    where task_fun(ctx) = {ok, ctx'}
---------------------------------------------------------------
⟨next_opcode, (ctx', tokens, scopes, ip+1, ...)⟩
```

**Preconditions**:
- Current opcode is TASK_EXEC
- Task function returns {ok, ctx'}

**Postconditions**:
- Context updated to ctx'
- IP advanced to next opcode
- Token remains active
- No side effects performed

**Informal**: Execute the task function, update context with result, advance to next instruction.

#### TASK-EFFECT (Effect-Yielding Task)

Task yields control to effect boundary.

```
⟨TASK_EXEC name, (ctx, tokens, scopes, ip, ...)⟩
    where task_fun(ctx) = {effect, EffectSpec, ContCtx}
---------------------------------------------------------------
{effect, EffectSpec, ContCtx, (ctx, tokens', scopes, ip, ...)}
```

**Preconditions**:
- Current opcode is TASK_EXEC
- Task function returns {effect, EffectSpec, ContCtx}

**Postconditions**:
- Executor yields effect specification
- Continuation context ContCtx saved for resumption
- State marked as waiting_effect (tokens' updated)
- IP does NOT advance (will resume after effect completes)

**Informal**: Task cannot complete without external effect. Yield to wf_effect manager, wait for result.

#### TASK-ERROR (Task Failure)

```
⟨TASK_EXEC name, (ctx, tokens, scopes, ip, ...)⟩
    where task_fun(ctx) = {error, Reason}
---------------------------------------------------------------
⟨ERROR, (ctx, tokens', scopes, ip, ...)⟩
```

**Preconditions**:
- Task function returns {error, Reason}

**Postconditions**:
- Token marked as failed
- Error propagated according to scope rules
- Execution may terminate or error handler invoked

**Informal**: Task failed. Propagate error to parent scope or terminate case.

### 4.2 Sequential Composition (seq)

Sequential composition executes left branch, then right branch. The right branch starts only after left completes.

#### SEQ-ENTER

```
⟨SEQ_ENTER, (ctx, tokens, scopes, ip, ...)⟩
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens, scopes', ip+1, ...)⟩
```

**Preconditions**: None

**Postconditions**:
- New sequence scope pushed onto scope stack
- scopes' = scopes ∪ {new_scope_id ⇒ #{status => active, parent ⇒ current_scope}}
- IP advanced to first instruction of left branch

**Informal**: Enter a sequence scope. Implicitly saves return address.

#### SEQ-NEXT (Left Complete)

Left branch complete, continue to right.

```
⟨SEQ_NEXT, (ctx, tokens, scopes, ip, branch_map, ...)⟩
    where token ∈ tokens, token.ip = ip, token.status = complete
    and branch_map[seq_id].right_ip = RightIP
---------------------------------------------------------------
⟨right_opcode, (ctx, tokens, scopes, RightIP, branch_map', ...)⟩
```

**Preconditions**:
- Left branch token marked complete
- Right branch IP available from branch_map

**Postconditions**:
- Token's IP updated to RightIP
- Token's status reset to active
- branch_map' removes seq_id entry
- Execution continues at right branch

**Informal**: Left done. Move to right. Update token position.

#### SEQ-DONE (Both Complete)

Both left and right complete. Sequence terminates.

```
⟨SEQ_NEXT, (ctx, tokens, scopes, ip, ...)⟩
    where left_token.complete, right_token.complete
    and branch_map[seq_id].tokens = [left_token, right_token]
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- Both left and right tokens complete
- Branch map has both tokens

**Postconditions**:
- Both tokens removed from active set
- New continuation token created (or IP advanced)
- Sequence scope removed from scope stack
- Execution continues after sequence

**Informal**: Both branches done. Merge results, continue.

### 4.3 Parallel Fork (par)

Parallel fork spawns N branches that execute concurrently (logically, not necessarily in parallel).

#### PAR-FORK

Spawn N parallel branches.

```
⟨PAR_FORK, (ctx, tokens, scopes, ip, branch_map, ...)⟩
    where PAR_FORK has N branch targets [ip₁, ip₂, ..., ipₙ]
    and N ≥ 2
---------------------------------------------------------------
⟨PAR_FORK, (ctx, tokens', scopes, ip, branch_map', ...)⟩
```

**Preconditions**:
- N ≥ 2 (parallel fork requires at least 2 branches)

**Postconditions**:
- tokens' = original tokens ∪ {token₁, ..., tokenₙ} where:
  - tokenᵢ = {idᵢ, ipᵢ, scope_id, undefined}
- branch_map'[par_id] = #{tokens => [id₁, ..., idₙ], targets => [ip₁, ..., ipₙ], join_id => JoinID}
- IP does NOT advance (waiting for branches to complete)

**Informal**: Create N new tokens, one per branch. Track them in branch_map. All branches active.

#### PAR-COMPLETE (One Branch Completes)

One branch completes. Update join counter.

```
⟨DONE, (ctx, tokens, scopes, ip, branch_map, join_counters, ...)⟩
    where token ∈ tokens, token.branch_id = BranchID
    and branch_map[BranchID].join_id = JoinID
    and join_counters[JoinID] = #{completed := K, required := N}
---------------------------------------------------------------
⟨next, (ctx, tokens', scopes, ip, branch_map', join_counters', ...)⟩
```

**Preconditions**:
- One token completes
- Associated join point exists

**Postconditions**:
- tokens' = tokens \ {token} (remove completed token)
- join_counters'[JoinID] = join_counters[JoinID] ∪ {completed := K+1}
- branch_map'[BranchID] = branch_map[BranchID] ∪ {tokens => tokens - [token.id]}

**Informal**: Branch done. Increment join counter. Remove branch token. Check if join ready.

#### PAR-JOIN (All Branches Complete)

All branches complete. Trigger join.

```
⟨JOIN_CHECK, (ctx, tokens, scopes, ip, branch_map, join_counters, ...)⟩
    where join_counters[JoinID].completed = join_counters[JoinID].required
---------------------------------------------------------------
⟨JOIN_WAIT, (ctx, tokens', scopes, ip, branch_map', join_counters', ...)⟩
```

**Preconditions**:
- Completed count equals required count

**Postconditions**:
- All branch tokens removed
- Single continuation token created (or IP advanced)
- Join counter removed
- Branch map entry removed

**Informal**: All branches done. Merge results, continue to join instruction.

### 4.4 Exclusive Choice (xor)

Exclusive choice selects ONE branch, disabling others. Unlike par (which spawns all branches), xor spawns only the selected branch.

#### XOR-CHOOSE

Select one branch, disable others.

```
⟨XOR_CHOOSE, (ctx, tokens, scopes, ip, ...)⟩
    where scheduler selects branch_i from [branch₁, ..., branchₙ]
    and XOR_CHOOSE has branch targets [ip₁, ip₂, ..., ipₙ]
---------------------------------------------------------------
⟨branch_opcode_i, (ctx, tokens', scopes, ip_i, ...)⟩
```

**Preconditions**:
- Scheduler chooses one branch (deterministic or nondeterministic policy)
- N ≥ 2 branches available

**Postconditions**:
- tokens' contains only ONE token for selected branch
- Other branches are NEVER spawned (not cancelled, just never created)
- IP set to ipᵢ (selected branch target)
- Execution continues in selected branch only

**Informal**: Pick one branch, execute it. Other branches never run. No cleanup needed.

**Key Difference from par**:
- par spawns ALL branches, cancels unselected ones
- xor spawns ONLY selected branch (others never created)

### 4.5 Join Synchronization (join)

Join policies determine when parallel branches can continue. Different policies wait for different numbers of branches.

#### JOIN-ALL (Wait for All Branches)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = #{completed := N, required := N, policy := all}
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, join_counters', ...)⟩
```

**Preconditions**:
- All N branches have completed (completed = required)
- Policy is 'all'

**Postconditions**:
- Join counter removed (join_counters' = join_counters \ {join_id})
- All branch tokens merged into continuation
- Execution continues to next opcode
- Results from all N branches collected

**Informal**: Wait for everyone. Then merge all results and continue.

#### JOIN-FIRST-N (Wait for N Branches)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = #{completed := K, required := N, policy := {first_n, M}}
    and K ≥ M
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, join_counters', ...)⟩
```

**Preconditions**:
- M or more branches have completed (K ≥ M)
- Policy is {first_n, M}

**Postconditions**:
- Join counter removed
- First M branch results collected
- Remaining (N - K) branches cancelled
- Continuation token created

**Informal**: Wait for M branches. Cancel the rest. Continue with M results.

#### JOIN-FIRST-COMPLETE (Wait for First Branch)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = #{completed := 1, required := N, policy := first_complete}
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- At least 1 branch completed
- Policy is first_complete

**Postconditions**:
- Join counter removed
- First completed branch result used
- All other (N - 1) branches cancelled
- Continuation with single result

**Informal**: First one wins. Cancel everyone else. Continue immediately.

#### JOIN-N-OF-M (Wait for N out of M)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = #{completed := K, required := M, policy := {n_of_m, N, M}}
    and K ≥ N
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- N or more branches completed (K ≥ N)
- Policy is {n_of_m, N, M} where 1 ≤ N ≤ M

**Postconditions**:
- Join counter removed
- First N branch results collected
- Remaining (M - K) branches cancelled
- Continuation with N results

**Informal**: Need N out of M total. Once N done, cancel rest, continue.

#### JOIN-SYNC-MERGE (Full Synchronization)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = #{completed := N, required := N, policy := sync_merge}
    and all branch results are compatible for merge
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- All N branches completed
- Policy is sync_merge (special case of 'all' with state merge)

**Postconditions**:
- All branch results merged into single context
- Conflicts resolved (last-write-wins or merge function)
- Continuation with merged context

**Informal**: Wait for all, then merge their state changes into one context.

### 4.6 Structured Loop (loop)

Loops repeat body until exit condition is satisfied. Three policies: while (check first), until (check after), count (execute N times).

#### LOOP-WHILE (Check Condition First)

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = while
    and condition(ctx) = true
---------------------------------------------------------------
⟨body_opcode, (ctx, tokens, scopes, body_ip, ...)⟩
```

**Condition true** → execute body.

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = while
    and condition(ctx) = false
---------------------------------------------------------------
⟨exit_opcode, (ctx, tokens, scopes, exit_ip, ...)⟩
```

**Condition false** → exit loop.

**Preconditions**:
- Loop policy is 'while'
- Condition function checks ctx

**Postconditions** (true):
- IP jumps to body_ip
- Loop body executes

**Postconditions** (false):
- IP jumps to exit_ip
- Loop terminates

**Informal**: Check condition first. If true, run body. If false, exit.

#### LOOP-UNTIL (Execute Body, Then Check)

```
⟨LOOP_BACK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = until
    and body_ip is loop start
---------------------------------------------------------------
⟨body_opcode, (ctx, tokens, scopes, body_ip, ...)⟩
```

Always execute body first.

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = until
    and condition(ctx) = false
---------------------------------------------------------------
⟨LOOP_BACK, (ctx, tokens, scopes, ip, ...)⟩
```

**Condition false** → repeat (jump back to LOOP_BACK).

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = until
    and condition(ctx) = true
---------------------------------------------------------------
⟨exit_opcode, (ctx, tokens, scopes, exit_ip, ...)⟩
```

**Condition true** → exit.

**Informal**: Execute body unconditionally. Then check condition. If false, repeat. If true, exit.

#### LOOP-COUNT (Execute N Times)

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = {count, N}
    and counter > 0
---------------------------------------------------------------
⟨body_opcode, (ctx, tokens, scopes, body_ip, counter' = counter - 1, ...)⟩
```

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where loop_policy = {count, N}
    and counter = 0
---------------------------------------------------------------
⟨exit_opcode, (ctx, tokens, scopes, exit_ip, ...)⟩
```

**Preconditions**:
- Loop policy is {count, N}
- Counter tracks remaining iterations

**Postconditions** (counter > 0):
- Execute body
- Decrement counter

**Postconditions** (counter = 0):
- Exit loop

**Informal**: Run body N times. Counter tracks iterations. Zero → exit.

### 4.7 Deferred Evaluation (defer)

Deferred choice races branches on external events. First external trigger wins.

#### DEFER-WAIT

All branches pending, waiting for external event.

```
⟨DEFER_ENTER, (ctx, tokens, scopes, ip, ...)⟩
    where DEFER_ENTER has N branch targets [ip₁, ..., ipₙ]
---------------------------------------------------------------
⟨DEFER_WAIT, (ctx, tokens', scopes, ip, defer_map, ...)⟩
```

**Preconditions**:
- N ≥ 2 branches (defer requires multiple options)

**Postconditions**:
- State marked as waiting_signal
- defer_map = #{branches => [{ip₁, cont₁}, ..., {ipₙ, contₙ}], pending => all}
- No branches execute yet
- IP does NOT advance

**Informal**: Set up race. Wait for external signal to select branch.

#### DEFER-SELECT (External Event Arrives)

External event selects one branch.

```
{signal, EventId, Payload}
    where defer_map.branches = [{ip₁, cont₁}, ..., {ipₙ, contₙ}]
    and scheduler maps EventId to branch_i
---------------------------------------------------------------
⟨branch_opcode_i, (ctx, tokens'', scopes, ip_i, defer_map', ...)⟩
```

**Preconditions**:
- External signal received
- Scheduler maps EventId to branch_i (via signal handler)

**Postconditions**:
- Selected branch executes with Payload
- Other branches cancelled
- defer_map removed
- IP set to ip_i

**Informal**: External event picks winner. Losers cancelled. Winner executes with event data.

**Example**: User approval (approve/reject buttons). First click wins, other option disabled.

### 4.8 Cancel Scope (cancel)

Cancellation propagates to all tokens within a scope. Three scope levels: activity (single task), case (entire workflow), region (scoped subtree).

#### CANCEL-ENTER

Enter cancellation scope.

```
⟨CANCEL_SCOPE(enter), (ctx, tokens, scopes, ip, ...)⟩
    where CANCEL_SCOPE has scope_id = ScopeId
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens, scopes', ip, ...)⟩
```

**Preconditions**:
- ScopeId is valid term

**Postconditions**:
- scopes' = scopes ∪ {ScopeId ⇒ #{status => active, parent ⇒ current_scope}}
- Current scope set to ScopeId for all new tokens in region
- Scope stack pushed

**Informal**: Enter a cancel region. Track it in scope stack. Enables targeted cancellation.

#### CANCEL-PROPAGATE

Propagate cancellation to all tokens in scope.

```
{cancel, ScopeId}
    where scopes[ScopeId] = #{status := active}
    and tokens_in_scope = {t | t ∈ tokens, t.scope_id = ScopeId}
---------------------------------------------------------------
⟨current, (ctx, tokens', scopes', ip, ...)⟩
```

**Preconditions**:
- Cancel signal received for ScopeId
- Scope is currently active

**Postconditions**:
- scopes'[ScopeId] = #{status := cancelled}
- For all t ∈ tokens_in_scope: t.status = cancelled
- Cancellation propagates to nested scopes recursively
- Time complexity: O(size of scope) - walk only tokens in scope

**Informal**: Mark scope and all its tokens as cancelled. Recursive on nested scopes.

#### CANCEL-NESTED (Nested Scope Propagation)

```
{cancel, ParentScopeId}
    where scopes[ParentScopeId].nested_scopes = [Child₁, ..., Childₙ]
---------------------------------------------------------------
{cancel, Child₁} ... {cancel, Childₙ}
```

**Preconditions**:
- Parent scope cancelled
- Has nested child scopes

**Postconditions**:
- Cancel signal sent to all nested scopes
- Recursive application of CANCEL-PROPAGATE

**Informal**: Cancel parent → cancel all children (transitive).

#### CANCEL-EXIT

Exit cancellation scope.

```
⟨CANCEL_SCOPE(exit), (ctx, tokens, scopes, ip, ...)⟩
    where current_scope = ScopeId
    and scopes[ScopeId].parent = ParentScopeId
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens, scopes', ip, ...)⟩
```

**Preconditions**:
- Exiting current scope
- Parent scope exists

**Postconditions**:
- scopes' = scopes \ {ScopeId} (remove from scope stack)
- Current scope restored to ParentScopeId
- Tokens in scope revert to parent scope_id
- Scope stack popped

**Informal**: Leave cancel region. Restore parent scope. Clean up scope metadata.

#### CANCEL-EFFECT (Cancel In-Flight Effects)

```
{cancel, ScopeId}
    where effect ∈ pending_effects
    and effect.scope_id = ScopeId
---------------------------------------------------------------
wf_effect:cancel(effect.effect_id)
```

**Preconditions**:
- Scope cancelled
- Has pending effects in scope

**Postconditions**:
- wf_effect:cancel/2 called for each effect
- Receipt records cancellation
- Effect handler notified

**Informal**: Stop in-flight effects in cancelled scope. Mark receipts as cancelled.

### 4.9 Multiple Instances (mi)

Multiple instances spawns N copies of a workflow pattern, then joins results.

#### MI-SPAWN-FIXED

Spawn fixed N instances.

```
⟨MI_SPAWN, (ctx, tokens, scopes, ip, ...)⟩
    where MI_SPAWN has policy = {fixed, N}
    and N > 0
---------------------------------------------------------------
⟨MI_SPAWN, (ctx, tokens', scopes, ip, mi_map, ...)⟩
```

**Preconditions**:
- N > 0 (must spawn at least one instance)

**Postconditions**:
- tokens' = tokens ∪ {instance_token₁, ..., instance_tokenₙ}
- instance_tokenᵢ = {idᵢ, instance_ipᵢ, scope_id, undefined}
- mi_map[mi_id] = #{instances => [id₁, ..., idₙ], policy => {fixed, N}, join_policy => JoinPolicy}
- Each instance gets unique instance_id

**Informal**: Create N identical instances. Track them in mi_map.

#### MI-SPAWN-DYNAMIC

Spawn dynamic N instances (N computed from ctx).

```
⟨MI_SPAWN, (ctx, tokens, scopes, ip, ...)⟩
    where MI_SPAWN has policy = {dynamic, Min, Max}
    and N = compute_instance_count(ctx, Min, Max)
    and Min ≤ N ≤ Max
---------------------------------------------------------------
⟨MI_SPAWN, (ctx, tokens', scopes, ip, mi_map, ...)⟩
```

**Preconditions**:
- Instance count function computes N from context
- N must be within [Min, Max] bounds

**Postconditions**:
- Spawn N instances (like MI-SPAWN-FIXED)
- N logged for audit

**Informal**: Compute N from ctx (e.g., "process all items in list"). Spawn N instances.

#### MI-COLLECT

One instance completes.

```
⟨DONE, (ctx, tokens, scopes, ip, mi_map, ...)⟩
    where instance_token ∈ tokens
    and instance_token.instance_id = InstanceID
    and mi_map[mi_id].instances = [InstanceID | Others]
---------------------------------------------------------------
⟨next, (ctx, tokens', scopes, ip, mi_map', ...)⟩
```

**Preconditions**:
- Instance completes
- Instance tracked in mi_map

**Postconditions**:
- tokens' = tokens \ {instance_token}
- mi_map'[mi_id] = mi_map[mi_id] ∪ {
    instances => Others,
    results => [instance_result | mi_map[mi_id].results]
  }
- Result collected

**Informal**: Instance done. Remove its token. Collect result. Check if all done.

#### MI-JOIN

Apply join policy to instance results.

```
⟨MI_JOIN, (ctx, tokens, scopes, ip, mi_map, ...)⟩
    where mi_map[mi_id].instances = []
    and mi_map[mi_id].join_policy = JoinPolicy
    and JoinPolicy satisfied by mi_map[mi_id].results
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- All instances complete (instances list empty)
- Join policy condition satisfied

**Postconditions**:
- Join policy applied to results (all, first_n, first_complete, n_of_m)
- Single continuation token created
- mi_map entry removed
- Execution continues

**Join Policies** (same as par join):
- **all**: Collect all results, continue when N complete
- **wait_n** (first_n): Continue when N complete, cancel rest
- **first_complete**: Continue when first complete, cancel rest
- **n_of_m**: Continue when N out of M complete

**Informal**: All instances done. Apply join policy to merge/select results. Continue.

---

## 5. Structural Rules

These rules define invariants that hold across all reductions.

### 5.1 Context Rule (CTX)

Context flows through reductions unchanged unless explicitly modified.

```
⟨P, (ctx, ...)⟩ → ⟨P', (ctx', ...)⟩
    where ctx' = ctx unless P modifies ctx
```

**Invariant**: Context is immutable except where task explicitly transforms it.

**Examples**:
- SEQ: ctx flows through left, then right (left may modify ctx, right sees modified ctx)
- PAR: ctx copied to each branch, results merged on join
- XOR: ctx flows to selected branch only

### 5.2 Scope Rule (SCOPE)

Cancel scopes nest properly. Parent scope contains child scope.

```
enter(ScopeParent)
enter(ScopeChild)  ⇒  ScopeParent.status = active ⇒ ScopeChild.parent = ScopeParent
exit(ScopeChild)   ⇒  ScopeChild removed, ScopeParent restored
```

**Invariant**: Scopes never cross. If A encloses B, A must exit after B.

**Formal**:
```
scopes[ScopeA].parent = ScopeB ⇒ scopes[ScopeB].parent ≠ ScopeA
```

**Implication**: Scope nesting forms a tree, not a graph.

### 5.3 Branch Rule (BRANCH)

Parallel branches are isolated. Each branch has its own token and IP.

```
par([P₁, ..., Pₙ]) ⇒ n tokens, each with independent IP
```

**Invariant**: Branches don't share state except through ctx (merged on join).

**Formal**:
```
tokenᵢ.ip ≠ tokenⱼ.ip for i ≠ j (in same par)
tokenᵢ.scope_id = tokenⱼ.scope_id (same cancel scope)
```

**Implication**: Parallel branches execute independently but share cancellation scope.

### 5.4 Token Rule (TOKEN)

Tokens are created on fork/choice, destroyed on join/error.

```
FORK:  1 token → N tokens (par, mi)
XOR:   1 token → 1 token (selected branch only)
JOIN:  N tokens → 1 token (or 0 if all cancelled)
ERROR: N tokens → 0 tokens (case terminates)
```

**Invariant**: Number of active tokens tracks logical parallelism.

**Formal**:
```
|tokens| ≥ 1 while case is active
|tokens| = 0 in terminal state (done/cancelled/failed)
```

**Implication**: Executor can detect completion by checking if tokens set is empty.

### 5.5 Determinism Rule (DETERM)

Under deterministic scheduler, same term + same ctx → same trace.

```
scheduler_policy = deterministic ⇒
⟨P, σ⟩ →* ⟨P', σ'⟩ is unique (no nondeterministic choices)
```

**Invariant**: Deterministic scheduler produces unique reduction sequence.

**Formal**:
```
∀σ₁, σ₂: if ctx₁ = ctx₂ and scheduler = deterministic
then trace₁ = trace₂ (identical opcode sequence)
```

**Implication**: Replay is possible by recording scheduler choices (or using deterministic policy).

### 5.6 Progress Rule (PROGRESS)

Unless blocked (waiting for effect/signal), some action is always enabled.

```
¬blocked(σ) ⇒ ∃action: enabled(action, σ)
```

**Invariant**: No deadlock unless waiting for external event.

**Formal**:
```
blocked(σ) ⇔ waiting_effect(σ) ∨ waiting_signal(σ) ∨ cancelled(σ)
```

**Implication**: Executor can always make progress unless waiting externally or cancelled.

---

## 6. Examples

### 6.1 Sequence Example

**Term**: `seq(task(a), task(b))`

**Bytecode**:
```
IP: Opcode
0:  SEQ_ENTER
1:  TASK_EXEC a
2:  SEQ_NEXT
3:  TASK_EXEC b
4:  DONE
```

**Execution Trace**:

**Step 0**:
```
State: σ₀ = (ctx₀, {token₀@ip=0}, ...)
⟨TASK_EXEC a, (ctx₀, {token₀@ip=1}, ...)⟩
→ TASK-EXEC
```

**Step 1**:
```
State: σ₁ = (ctx₁, {token₀@ip=2}, ...)  [task a executed, ctx updated]
⟨SEQ_NEXT, (ctx₁, {token₀@ip=2}, ...)⟩
→ SEQ-NEXT (left complete, advance to right)
```

**Step 2**:
```
State: σ₂ = (ctx₁, {token₀@ip=3}, ...)  [IP updated to right branch]
⟨TASK_EXEC b, (ctx₁, {token₀@ip=3}, ...)⟩
→ TASK-EXEC
```

**Step 3**:
```
State: σ₃ = (ctx₂, {token₀@ip=4}, ...)  [task b executed]
⟨DONE, (ctx₂, {token₀@ip=4}, ...)⟩
→ Terminal (done)
```

**Result**: Context transformed ctx₀ → ctx₁ → ctx₂. Both tasks executed sequentially.

### 6.2 Parallel Example

**Term**: `par([task(a), task(b)])`

**Bytecode**:
```
IP: Opcode
0:  PAR_FORK [1, 3]
1:  TASK_EXEC a
2:  DONE
3:  TASK_EXEC b
4:  DONE
5:  JOIN_WAIT all
```

**Execution Trace**:

**Step 0**:
```
State: σ₀ = (ctx₀, {token₀@ip=0}, ...)
⟨PAR_FORK, (ctx₀, {token₀@ip=0}, ...)⟩
→ PAR-FORK (spawn 2 tokens)
```

**Step 1**:
```
State: σ₁ = (ctx₀, {token₁@ip=1, token₂@ip=3}, branch_map[par₁] = ...)
[token₁ starts at IP=1, token₂ starts at IP=3]
```

**Step 2**:
```
⟨TASK_EXEC a, (ctx₀, {token₁@ip=1, token₂@ip=3}, ...)⟩
→ TASK-EXEC (token₁ complete)
```

**Step 3**:
```
⟨TASK_EXEC b, (ctx₀, {token₁@ip=2, token₂@ip=3}, ...)⟩
→ TASK-EXEC (token₂ complete)
```

**Step 4**:
```
State: σ₄ = (ctx₀, {token₁@ip=2, token₂@ip=4}, join_counters[join₁] = #{completed => 2})
⟨JOIN_WAIT, ..., join_counters[join₁] = #{completed => 2, required => 2}⟩
→ JOIN-ALL (both complete, continue)
```

**Result**: Both tasks executed (order may vary). Results merged on join.

### 6.3 Loop Example

**Term**: `loop({count, 3}, task(a))`

**Bytecode**:
```
IP: Opcode
0:  LOOP_CHECK count=3
1:  TASK_EXEC a
2:  LOOP_BACK
3:  DONE (exit)
```

**Execution Trace**:

**Step 0** (counter=3):
```
⟨LOOP_CHECK, (ctx₀, {token₀@ip=0}, counter=3, ...)⟩
→ LOOP-COUNT (3 > 0, execute body)
```

**Step 1**:
```
⟨TASK_EXEC a, (ctx₀, {token₀@ip=1}, counter'=2, ...)⟩
→ TASK-EXEC
```

**Step 2**:
```
⟨LOOP_BACK, (ctx₁, {token₀@ip=2}, counter=2, ...)⟩
→ Jump to IP=0
```

**Step 3** (counter=2):
```
⟨LOOP_CHECK, (ctx₁, {token₀@ip=0}, counter'=2, ...)⟩
→ LOOP-COUNT (2 > 0, execute body)
[... task a executes, counter decremented to 1 ...]
```

**Step N** (counter=1):
```
[task a executes, counter decremented to 0]
```

**Step N+1** (counter=0):
```
⟨LOOP_CHECK, (ctx₃, {token₀@ip=0}, counter'=0, ...)⟩
→ LOOP-COUNT (0 = 0, exit)
```

**Step N+2**:
```
⟨DONE, (ctx₃, {token₀@ip=3}, ...)⟩
→ Terminal
```

**Result**: Task a executed 3 times. Counter: 3 → 2 → 1 → 0.

### 6.4 Cancel Example

**Term**: `cancel(scope₁, par([task(a), task(b)]))`

**Bytecode**:
```
IP: Opcode
0:  CANCEL_SCOPE enter scope₁
1:  PAR_FORK [2, 4]
2:  TASK_EXEC a
3:  DONE
4:  TASK_EXEC b
5:  DONE
6:  JOIN_WAIT all
7:  CANCEL_SCOPE exit scope₁
```

**Execution Trace**:

**Step 0**:
```
⟨CANCEL_SCOPE enter, (ctx₀, {token₀@ip=0}, scopes = {}, ...)⟩
→ CANCEL-ENTER
```

**Step 1**:
```
State: scopes = {scope₁ ⇒ #{status => active}}
⟨PAR_FORK, (ctx₀, {token₀@ip=1}, scopes, ...)⟩
→ PAR-FORK (spawn tokens in scope₁)
```

**Step 2**:
```
State: tokens = {token₁@ip=2&scope=scope₁, token₂@ip=4&scope=scope₁}
[Both tokens in scope₁]
```

**External Signal**: `{cancel, scope₁}`

**Step 3**:
```
⟨CANCEL-PROPAGATE, scope₁⟩
→ Mark all tokens in scope₁ as cancelled
```

**Step 4**:
```
State: tokens = {token₁@status=cancelled, token₂@status=cancelled}
scopes = {scope₁ ⇒ #{status => cancelled}}
[In-flight effects cancelled]
```

**Step 5**:
```
⟨CANCEL_SCOPE exit, (ctx₀, {...}, scopes, ...)⟩
→ CANCEL-EXIT
```

**Result**: Both tasks cancelled. Scope cleaned up. Case terminates with cancelled status.

---

## 7. References

### Specification Documents

- **PROMPT.md**: `/Users/speed/wf-substrate/PROMPT.md:97-136`
  - Lines 97-104: Kernel types (ctx, proc, exec, scope_id, case_id, receipt)
  - Lines 106-125: Pattern algebra definition (9 kernel primitives)
  - Lines 127-136: Semantics overview

### Type Definitions

- **Item 002 research.md**: Pattern term algebra types
  - Lines 107-157: Kernel types (ctx, scope_id, task_fun, join_policy, etc.)
  - Lines 266-287: wf_term() union type

### Module Specifications

- **Item 004**: Bytecode compiler and opcodes
- **Item 005**: Executor and exec_state structure
- **Item 008**: Cancellation semantics (scopes)
- **Item 009**: Multiple instances

### Companion Documents

- **docs/ARCHITECTURE.md**: System architecture and bytecode design rationale
- **Item 002 plan.md**: Pattern algebra implementation details

### Formal Semantics Background

- **Operational Semantics**: Small-step reduction rules (Plotkin)
- **Inference Rules**: Natural deduction notation
- **Process Algebra**: CSP, CCS (for parallel composition semantics)

---

**Document Version**: 1.0
**Last Updated**: 2025-01-10
**Author**: Agent swarm (20-agent coordination)
**Status**: Formal specification for executor implementation (item 005)
