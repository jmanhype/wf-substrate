# Write architecture and formal semantics documentation Implementation Plan

## Implementation Plan Title

Documentation Foundation: Architecture and Formal Semantics for Workflow Pattern Substrate

## Overview

This implementation plan creates two foundational documentation files that establish the theoretical and architectural foundation for the entire wf_substrate project. These documents serve as the authoritative specification for the bytecode VM design, execution semantics, and module architecture. They are written BEFORE implementation of items 004-020, guiding all subsequent development work.

**Key Deliverables:**
1. **docs/ARCHITECTURE.md** - System architecture, module dependency graph, bytecode VM design rationale, compilation pipeline, supervision tree layout, and effect boundary model
2. **docs/SEMANTICS.md** - Small-step operational semantics with inference rules for all 9 kernel primitives

## Current State

**Project Status**: Only the basic rebar3 scaffold exists (item 001 complete). No workflow logic, pattern algebra, compiler, or executor exists yet.

**Existing Files**:
- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration (OTP 26+, no external dependencies)
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file with auto-discovery
- `/Users/speed/wf-substrate/src/wf_substrate_app.erl:1-43` - Application callback module
- `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:1-51` - Empty top-level supervisor
- `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder public API (commented exports)
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:1-16` - Empty EUnit test suite
- `/Users/speed/wf-substrate/docs/README.md:1-22` - Documentation placeholder noting that ARCHITECTURE.md and SEMANTICS.md are planned

**Key Constraints Discovered**:
1. **No Implementation Exists**: Documentation must be based entirely on PROMPT.md specification and item overviews, not existing code
2. **Documentation-First Approach**: This documentation defines the target architecture that items 004-020 will implement
3. **Bytecode VM Decision**: Must document WHY bytecode VM is chosen over AST interpretation or continuation network (per PROMPT.md:157-177)
4. **Specification Authority**: All documentation must align with PROMPT.md:1-375 as the authoritative source

## Desired End State

Two comprehensive Markdown files in `/Users/speed/wf-substrate/docs/` that:

1. **ARCHITECTURE.md** (approximately 800-1200 lines):
   - Module dependency graph showing relationships between 16 planned modules
   - Runtime strategy choice section explaining bytecode VM vs alternatives
   - Compilation pipeline flow: term → validate → compile → bytecode → execute
   - Supervision tree layout: wf_substrate_sup → wf_case_sup → per-case runners
   - Effect boundary model: how tasks yield effects, how receipts work
   - Type system overview summarizing kernel types from item 002
   - Diagrams using ASCII art for clarity

2. **SEMANTICS.md** (approximately 1000-1500 lines):
   - Execution state definition: formal model of σ = (ctx, tokens, scopes, ip, ...)
   - Inference-rule notation explanation: premise/conclusion format
   - Reduction rules for all 9 kernel primitives:
     - Task execution (pure and effect-yielding)
     - Sequential composition (seq: left then right)
     - Parallel fork (par: spawn all branches)
     - Exclusive choice (xor: enable one, disable others)
     - Join synchronization (wait-all, wait-n, first-complete policies)
     - Structured loop (while, until, count policies)
     - Deferred evaluation (race on external event)
     - Cancel scope (enter, propagate, exit)
     - Multiple instances (spawn N, collect results, apply join policy)
   - Structural rules for context, scope, branch, and token management
   - Example traces showing step-by-step execution

**Verification Criteria**:
- [ ] Both files exist in `/Users/speed/wf-substrate/docs/`
- [ ] Files are valid Markdown with proper formatting
- [ ] All sections from item.json overview are covered
- [ ] Content aligns with PROMPT.md specification
- [ ] All 9 kernel primitives have reduction rules in SEMANTICS.md
- [ ] Module dependency graph in ARCHITECTURE.md matches planned modules
- [ ] Bytecode VM rationale is clearly explained
- [ ] Compilation pipeline is documented with examples
- [ ] Supervision tree layout is specified with ASCII diagram

### Key Discoveries:

- **PROMPT.md:97-136** defines kernel types (ctx, proc, exec, scope_id, case_id, receipt) and lists 9 kernel primitives - this is the primary source for SEMANTICS.md
- **PROMPT.md:157-177** explicitly requires choosing between bytecode VM or continuation network and documenting the decision - this must be a dedicated section in ARCHITECTURE.md
- **PROMPT.md:145-156** describes the effect boundary pattern - tasks return `{effect, EffectSpec, ContCtx}` instead of performing IO directly
- **PROMPT.md:179-198** specifies OTP supervision tree requirements - wf_substrate_sup supervises wf_case_sup which supervises per-case runners
- **Item 002 research.md:107-287** provides detailed type definitions that should be referenced in ARCHITECTURE.md type system overview
- **Items 004-012 overviews** describe bytecode opcodes, exec_state structure, state store, scheduler policies, cancellation, multiple instances, effects, tracing, and supervision - these provide concrete details for both documents

## What We're NOT Doing

- **NOT implementing any code** - this is pure documentation only
- **NOT writing docs/PATTERNS.md** - that's item 017
- **NOT writing docs/TESTING.md** - that's item 014-016
- **NOT writing docs/OPERATIONS.md** - that's item 020
- **NOT implementing derived pattern semantics** - only kernel primitives get reduction rules
- **NOT writing executable examples** - those are item 018
- **NOT specifying low-level implementation details** - keep documentation at architectural/semantic level, not implementation level
- **NOT creating visual diagrams with external tools** - use ASCII art only for portability

## Implementation Approach

**High-Level Strategy**: Create two comprehensive, specification-aligned documentation files in sequence. First write ARCHITECTURE.md to establish the system architecture and design decisions, then write SEMANTICS.md to formalize the execution semantics.

**Documentation Philosophy**:
- Use "will" and "shall" language for future implementation (since code doesn't exist yet)
- Mark sections as "specified" or "planned" rather than "implemented"
- Base all content strictly on PROMPT.md and item overviews
- Use formal notation for inference rules (ASCII art, no LaTeX required)
- Provide concrete examples to illustrate abstract concepts
- Include diagrams using ASCII art for dependency graphs, compilation pipeline, supervision tree

**Risk Mitigation**:
- Since no implementation exists, documentation is inherently speculative - mitigate by referencing specification sources (PROMPT.md:line numbers, item overviews)
- Avoid implementation-specific details that might change - focus on "what" and "why", not "how"
- When multiple interpretations exist, choose the one that best satisfies stated goals (O(1) dispatch, no AST interpretation)
- Document assumptions explicitly when specification is ambiguous

**File Organization**:
- Use clear section headers and table of contents
- Keep each document focused (ARCHITECTURE on structure/design, SEMANTICS on formal rules)
- Use consistent terminology across both documents
- Cross-reference between documents where needed

---

## Phases

### Phase 1: ARCHITECTURE.md - System Architecture Document

#### Overview

Create docs/ARCHITECTURE.md with comprehensive system architecture documentation. This document will serve as the blueprint for all subsequent implementation work (items 004-020). It explains what modules exist, how they relate, why bytecode VM is chosen, and how the system fits together.

#### Changes Required:

##### 1. Create docs/ARCHITECTURE.md

**File**: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md`
**Changes**: Create new comprehensive architecture document with the following sections:

```markdown
# Architecture of wf_substrate

## Table of Contents
1. Overview
2. Module Dependency Graph
3. Runtime Strategy Choice
4. Compilation Pipeline
5. Supervision Tree Layout
6. Effect Boundary Model
7. Type System Overview
8. Module Responsibilities
9. Design Alternatives
10. References

## 1. Overview
[Brief introduction: wf_substrate is a pure Erlang/OTP workflow pattern substrate that compiles workflow patterns to bytecode for O(1) execution. It implements 43 workflow patterns with 9 kernel primitives.]

## 2. Module Dependency Graph

### 2.1 Dependency Diagram

[ASCII art diagram showing:]
wf_term (foundation)
  ↓
wf_core (derived patterns)
  ↓
wf_compile (bytecode compiler)
  ↓
wf_vm (bytecode definition)
  ↓
wf_exec (executor)

wf_state (state store) ← used by wf_exec
wf_sched (scheduler) ← used by wf_exec
wf_cancel (cancellation) ← used by wf_exec
wf_mi (multiple instances) ← used by wf_exec
wf_effect (effect boundary) ← used by wf_exec
wf_receipt (receipts) ← used by wf_effect
wf_trace (tracing) ← used by wf_exec
wf_validate (validation) ← standalone

### 2.2 Module Table

[Table listing all 16 modules with brief descriptions]

## 3. Runtime Strategy Choice

### 3.1 The Problem: AST Interpretation Overhead

[Explain O(depth) dispatch problem: "case NodeType of" in hot loop]

### 3.2 Strategy Comparison Table

| Approach | Dispatch Complexity | Per-Step Overhead | Tracing Difficulty |
|----------|-------------------|-------------------|-------------------|
| AST Interpretation | O(depth) tree walk | High (node dispatch) | Hard (AST traversal) |
| Bytecode VM | O(1) instruction fetch | Low (flat opcode list) | Easy (IP sequence) |
| Continuation Network | O(1) closure call | Medium (closure alloc) | Medium (call stack) |

### 3.3 Why Bytecode VM?

[Decision rationale:]
- O(1) instruction fetch: IP register points to next opcode
- No per-step "case NodeType of": all structural decisions resolved at compile time
- Explicit exec_state: IP, branch map, join counters, scope stack are explicit
- Easy tracing: IP and opcode sequence provide clear execution trace
- Better optimization: compile-time analysis can optimize bytecode

[Tradeoffs:]
- Compilation overhead: acceptable for long-running workflows
- Debugging complexity: mitigated by tracing and readable opcode names

### 3.4 Alternative: Continuation Network

[Brief description of continuation network approach and why bytecode VM was chosen instead]

## 4. Compilation Pipeline

### 4.1 Pipeline Stages

[ASCII flowchart:]
```
wf_term() (user-authored pattern term)
    ↓
wf_validate:structural_check/1 (well-formedness)
    ↓
wf_compile:compile/1 (single-pass compilation)
    ↓
wf_bc() (flat list of opcodes)
    ↓
wf_exec:new/1 (create executor)
    ↓
wf_exec:step/2 (execute in quanta)
```

### 4.2 Stage Details

#### Stage 1: term (wf_term())
[Example: seq(par([task(a), task(b)]), task(c))]

#### Stage 2: validate
[Checks: well_formed, structural invariants, bounded]

#### Stage 3: compile
[Process: single recursive pass, emit opcodes with labels, resolve labels to integers]

#### Stage 4: bytecode
[Example bytecode: [{SEQ_ENTER, 0}, {PAR_FORK, [2, 5]}, {TASK_EXEC, a}, ...]]

#### Stage 5: execute
[Process: wf_exec runs tight loop over bytecode, quanta-based execution]

### 4.3 Concrete Compilation Example

[Show full compilation of seq(par([task(a), task(b)]), task(c)) with source → bytecode mapping]

## 5. Supervision Tree Layout

### 5.1 Tree Structure

[ASCII diagram:]
```
wf_substrate_sup (one_for_one)
├── wf_case_sup (simple_one_for_one, dynamic)
│   ├── case_runner_1 (gen_statem)
│   ├── case_runner_2 (gen_statem)
│   └── case_runner_N (gen_statem)
├── wf_effect_sup (optional, simple_one_for_one)
│   ├── effect_worker_1
│   └── effect_worker_N
└── wf_trace_sink (optional, gen_server)
```

### 5.2 Per-Case Runner (gen_statem)

**States**: initializing, running, waiting_effect, waiting_signal, cancelled, done

**Data**: exec_state, step_quanta, trace_level, effect_handler

**Messages**: {signal, Signal}, {effect_result, EffectId, Result}, {cancel, Reason}, {cancel_region, ScopeId}

**Timeouts**: state_timeout for overall case timeout

### 5.3 Fault Tolerance

[Explain restart strategies, error handling, crash recovery]

## 6. Effect Boundary Model

### 6.1 The Problem: Pure Substrate, Impure World

[Explain: wf_substrate is pure, but tasks need external IO (HTTP, database, etc.)]

### 6.2 Effect Boundary Pattern

**Traditional approach** (wrong):
```erlang
task(do_http_request) ->
    httpc:request(...).  % Side effect!
```

**Effect boundary approach** (correct):
```erlang
task(do_http_request) ->
    {effect, #{
        effect_type => http_request,
        payload => #{url => ..., method => get}
    }, ContinuationCtx}.
```

### 6.3 EffectSpec Structure

```erlang
{effect, EffectSpec, Continuation}
where EffectSpec = #{
    effect_id := unique_id(),      % Derived from case_id + step_seq + scope
    effect_type := atom(),          % e.g., http_request, file_write
    payload := term(),              % Operation-specific data
    idempotency_key => term(),      % Optional, for at-most-once semantics
    timeout => timeout()            % Optional timeout
}
```

### 6.4 Receipt Structure

```erlang
#receipt{
    receipt_id = unique_id(),
    effect_id = effect_id(),
    effect_spec_hash = hash(),
    timestamp = erlang:timestamp(),
    result = {ok, term()} | {error, term()} | cancelled,
    duration_us = non_neg_integer()
}
```

### 6.5 Effect Lifecycle

[ASCII flowchart:]
```
Task → {effect, Spec, ContCtx}
       ↓
wf_effect executes Spec
       ↓
Effect Result (ok/error/cancelled)
       ↓
Executor resumes with Result
       ↓
Receipt produced
```

### 6.6 Cancellation of Effects

- In-flight effects cancelled when cancel scope containing them is cancelled
- wf_effect:cancel_effect/2 cancels pending effect
- Receipt records cancellation for audit trail

### 6.7 Idempotency and At-Most-Once

- idempotency_key provides deduplication
- Duplicate effects with same key return cached receipt result
- Enables safe retry after crash

## 7. Type System Overview

### 7.1 Kernel Types

[Summarize types from item 002 research.md:107-157]
- ctx() - user context map
- scope_id() - cancellation scope identifier
- case_id() - workflow instance identifier
- receipt() - effect receipt
- task_fun() - task function signature
- join_policy() - join synchronization policy
- loop_policy() - loop exit condition
- mi_policy() - multiple instance policy

### 7.2 Pattern Algebra Type

[Show wf_term() union type with 9 constructors from item 002 plan.md:266-287]

### 7.3 Bytecode Types

- wf_bc() - flat list of opcodes
- opcode() - union of 12 opcodes (SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, LOOP_CHECK, CANCEL_SCOPE, MI_SPAWN, EFFECT_YIELD, TASK_EXEC, DONE)

### 7.4 Execution State Types

- exec_state() - executor state record (from item 005)
- state() - case state store (from item 006)
- sched_state() - scheduler state (from item 007)

## 8. Module Responsibilities

### 8.1 Foundation Modules

**wf_term.erl** (item 002)
- Pattern algebra AST with 9 kernel constructors
- Smart constructors with validation
- Structural validation (well_formed/1)

**wf_core.erl** (item 002)
- Derived patterns (simple_merge, synchronizing_merge, discriminator, n_out_of_m)
- Each derived pattern is a macro/function returning wf_term()

### 8.2 Compilation Modules

**wf_compile.erl** (item 004)
- Compile wf_term() to wf_bc() bytecode
- Single recursive pass over AST
- Label resolution to integer addresses
- Opcode emission

**wf_vm.erl** (item 004)
- Bytecode definition (wf_bc() and opcode() types)
- Opcode specifications and operands

**wf_exec.erl** (item 005)
- Bytecode executor hot loop
- exec_state record (IP, branch map, join counters, scope stack, tokens)
- step/2 for single reduction, run/3 for quanta-based execution

### 8.3 Runtime Support Modules

**wf_state.erl** (item 006)
- Per-case state store
- Atomic commit protocol (buffer → validate → apply → receipt)
- Context and token tracking
- Scope metadata

**wf_sched.erl** (item 007)
- Pluggable scheduler policies
- Deterministic (stable ordering)
- Nondeterministic (random with log)
- Replay (feed recorded log)

**wf_cancel.erl** (item 008)
- Cancellation semantics
- Activity/case/region scopes
- O(scope size) propagation

**wf_mi.erl** (item 009)
- Multiple instances support
- Fixed and dynamic count modes
- Join policies for instance results

**wf_effect.erl** (item 010)
- Effect boundary implementation
- EffectSpec creation and execution
- Effect cancellation

**wf_receipt.erl** (item 010)
- Receipt storage and retrieval
- Idempotency key lookup
- Receipt verification

**wf_trace.erl** (item 011)
- Structured tracing
- Trace event logging
- Replay log support

**wf_validate.erl** (item 013)
- Structural validation
- Bounded model checking
- Soundness checks

### 8.4 OTP Integration Modules

**wf_substrate_app.erl** (item 012)
- Application behavior callback

**wf_substrate_sup.erl** (item 012)
- Top-level supervisor

**wf_case_sup.erl** (item 012)
- Dynamic supervisor for per-case runners

**wf_case_runner.erl** (item 012)
- gen_statem per-case runner

**wf_substrate.erl** (item 012)
- Public API module

## 9. Design Alternatives

### 9.1 Bytecode VM vs Continuation Network

[Comparison table and rationale for bytecode VM choice]

### 9.2 Per-Case Process vs Single Global Executor

[Rationale: per-case gen_statem processes for isolation and supervision]

### 9.3 Effect Handling: In-Process vs Out-of-Process

[Discussion: both options supported via effect_handler module]

## 10. References

- PROMPT.md:1-375 - Authoritative specification
- Item 002 research.md - Pattern term algebra research
- Items 004-012 item.json - Module specifications
- docs/SEMANTICS.md - Formal operational semantics
```

#### Success Criteria:

##### Manual Verification:

- [ ] File exists at `/Users/speed/wf-substrate/docs/ARCHITECTURE.md`
- [ ] File is valid Markdown (no syntax errors)
- [ ] All sections from Table of Contents are present
- [ ] Module dependency graph shows all 16 planned modules
- [ ] Bytecode VM rationale section explains O(1) vs O(depth) dispatch
- [ ] Compilation pipeline section includes concrete example
- [ ] Supervision tree section includes ASCII diagram
- [ ] Effect boundary section includes EffectSpec and Receipt structures
- [ ] Type system overview references item 002 types
- [ ] Module responsibilities section covers all 16 modules with references to item numbers
- [ ] No implementation-specific details that don't exist yet
- [ ] All content aligns with PROMPT.md specification
- [ ] "will" and "shall" language used for future implementation

**Note**: Complete manual verification, then proceed to Phase 2.

---

### Phase 2: SEMANTICS.md - Formal Operational Semantics

#### Overview

Create docs/SEMANTICS.md with small-step operational semantics for all 9 kernel primitives. This document formalizes the execution model using inference-rule notation, providing precise mathematical specification of how each primitive behaves.

#### Changes Required:

##### 1. Create docs/SEMANTICS.md

**File**: `/Users/speed/wf-substrate/docs/SEMANTICS.md`
**Changes**: Create new comprehensive semantics document with the following sections:

```markdown
# Operational Semantics of wf_substrate

## Table of Contents
1. Introduction
2. Execution State Definition
3. Reduction Rule Notation
4. Kernel Primitive Reduction Rules
5. Structural Rules
6. Examples
7. References

## 1. Introduction

wf_substrate executes workflow patterns compiled to bytecode. This document specifies the small-step operational semantics of the 9 kernel primitives using inference rules. Each rule defines how one execution state reduces to another.

**Scope**: This document covers kernel primitives only. Derived patterns (wf_core.erl) expand to kernel primitives, so their semantics are derived.

## 2. Execution State Definition

### 2.1 Configuration

A configuration is a pair ⟨P, σ⟩ where:
- **P** is the current program (bytecode instruction at IP)
- **σ** is the execution state

### 2.2 Execution State σ

The execution state σ is a tuple:
```
σ = (ctx, tokens, scopes, ip, branch_map, join_counters, step_count)
```

Where:
- **ctx**: ctx() - user-provided context map (opaque to engine)
- **tokens**: set of active tokens, each token = {token_id, ip, scope_id, value}
- **scopes**: map of scope_id → scope_metadata (cancel scopes, nesting)
- **ip**: instruction pointer (integer index into bytecode)
- **branch_map**: map tracking active parallel branches (branch_id → token_set)
- **join_counters**: map of join_id → {completed, required, policy}
- **step_count**: non_neg_integer() - number of reductions executed

### 2.3 Token Structure

Each token represents a logical thread of execution:
```
token = {token_id, ip, scope_id, value}
```

- **token_id**: unique identifier (auto-generated)
- **ip**: current instruction pointer for this token
- **scope_id**: active cancellation scope
- **value**: accumulated result from completed operations

### 2.4 Scope Structure

Cancel scopes track nesting and cancellation status:
```
scope_metadata = #{status := active | cancelled, parent => scope_id}
```

### 2.5 Initial State

For a compiled bytecode B and initial context Ctx:
```
σ₀ = (Ctx, {token₀}, {}, 0, {}, {}, 0)
```
where token₀ = {auto_gen(), 0, root, undefined}

### 2.6 Terminal States

- **Done**: No active tokens, all branches complete
- **Cancelled**: All tokens marked cancelled
- **Blocked**: Waiting for external signal or effect result

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

### 3.2 Configuration Reduction

A reduction step transforms one configuration to another:
```
⟨P, σ⟩ → ⟨P', σ'⟩
```

### 3.3 Rule Names

Each rule has a descriptive name:
- **TASK-EXEC**: Execute a pure task
- **TASK-EFFECT**: Task yields effect
- **SEQ-LEFT**: Start sequence (execute left)
- **SEQ-RIGHT**: Continue sequence (left done, execute right)
- **PAR-FORK**: Fork parallel branches
- **PAR-COMPLETE**: Branch completes, update join counter
- **XOR-CHOOSE**: Choose exclusive branch
- **JOIN-ALL**: Wait for all branches
- **JOIN-FIRST-N**: Wait for N branches
- **JOIN-FIRST-COMPLETE**: Wait for first branch
- **LOOP-WHILE**: Check while condition
- **LOOP-UNTIL**: Execute body, check until condition
- **LOOP-COUNT**: Decrement counter
- **DEFER-WAIT**: All branches pending
- **DEFER-SELECT**: External event selects branch
- **CANCEL-ENTER**: Enter cancel scope
- **CANCEL-PROPAGATE**: Propagate cancellation
- **CANCEL-EXIT**: Exit cancel scope
- **MI-SPAWN-FIXED**: Spawn fixed number of instances
- **MI-SPAWN-DYNAMIC**: Spawn dynamic instances
- **MI-COLLECT**: Collect instance result
- **MI-JOIN**: Apply join policy

## 4. Kernel Primitive Reduction Rules

### 4.1 Task Execution

#### TASK-EXEC (Pure Task)

Execute a pure task function that doesn't yield effects.

```
⟨TASK_EXEC, (ctx, tokens, scopes, ip, ...)⟩
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

#### TASK-EFFECT (Effect-Yielding Task)

Task yields control to effect boundary.

```
⟨TASK_EXEC, (ctx, tokens, scopes, ip, ...)⟩
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
- State marked as waiting_effect

#### TASK-ERROR (Task Failure)

```
⟨TASK_EXEC, (ctx, tokens, scopes, ip, ...)⟩
    where task_fun(ctx) = {error, Reason}
---------------------------------------------------------------
⟨ERROR, (ctx, tokens', scopes, ip, ...)⟩
```

**Postconditions**:
- Token marked as failed
- Error propagated according to scope rules

### 4.2 Sequential Composition (seq)

#### SEQ-ENTER

```
⟨SEQ_ENTER, (ctx, tokens, scopes, ip, ...)⟩
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens, scopes, ip+1, ...)⟩
```

Enters sequence scope. Return address implicitly next opcode after SEQ_NEXT.

#### SEQ-NEXT

Left branch complete, continue to right.

```
⟨SEQ_NEXT, (ctx, tokens, scopes, ip, ...)⟩
    where left_token is complete
---------------------------------------------------------------
⟨right_opcode, (ctx, tokens, scopes, right_ip, ...)⟩
```

**Preconditions**:
- Left branch token marked complete
- Right branch IP available from branch map

#### SEQ-DONE

Both left and right complete.

```
⟨SEQ_NEXT, (ctx, tokens, scopes, ip, ...)⟩
    where left_token complete, right_token complete
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

### 4.3 Parallel Fork (par)

#### PAR-FORK

Spawn N parallel branches.

```
⟨PAR_FORK, (ctx, tokens, scopes, ip, ...)⟩
    where PAR_FORK has N branch targets [ip₁, ip₂, ..., ipₙ]
---------------------------------------------------------------
⟨PAR_FORK, (ctx, tokens', scopes, ip, branch_map', ...)⟩
```

**Preconditions**:
- N ≥ 2 (parallel fork requires at least 2 branches)

**Postconditions**:
- tokens' = original tokens plus N new tokens:
  - token₁ = {id₁, ip₁, scope_id, undefined}
  - token₂ = {id₂, ip₂, scope_id, undefined}
  - ...
  - tokenₙ = {idₙ, ipₙ, scope_id, undefined}
- branch_map' tracks: {par_id, [token₁, ..., tokenₙ], [ip₁, ..., ipₙ]}

#### PAR-COMPLETE

One branch completes.

```
⟨DONE, (ctx, tokens, scopes, ip, branch_map, ...)⟩
    where token ∈ tokens completes for par_id
    and branch_map[par_id] = {active, [token, others], targets}
---------------------------------------------------------------
⟨next, (ctx, tokens, scopes, ip, branch_map', ...)⟩
```

**Postconditions**:
- branch_map'[par_id] = {active, [others], targets}
- If others is empty, all branches complete, trigger join

### 4.4 Exclusive Choice (xor)

#### XOR-CHOOSE

Select one branch, disable others.

```
⟨XOR_CHOOSE, (ctx, tokens, scopes, ip, ...)⟩
    where scheduler selects branch_i from [branch₁, ..., branchₙ]
---------------------------------------------------------------
⟨branch_opcode_i, (ctx, tokens', scopes, branch_ip_i, ...)⟩
```

**Preconditions**:
- Scheduler chooses one branch (deterministic or nondeterministic policy)

**Postconditions**:
- tokens' contains only one token for selected branch
- Other branches are never spawned (not cancelled, just never created)

### 4.5 Join Synchronization (join)

#### JOIN-ALL (Wait for All Branches)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = {completed, N, all}
    and completed = N
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- All N branches have completed
- Policy is 'all'

**Postconditions**:
- Join counter removed
- Execution continues to next opcode
- All branch tokens merged into continuation

#### JOIN-FIRST-N (Wait for N Branches)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = {completed, N_required, {first_n, K}}
    and completed ≥ K
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- K or more branches have completed
- Policy is {first_n, K}

**Postconditions**:
- Join counter removed
- Remaining (N - completed) branches cancelled
- First K branch results collected

#### JOIN-FIRST-COMPLETE (Wait for First Branch)

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = {1, N, first_complete}
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Preconditions**:
- At least 1 branch completed
- Policy is first_complete

**Postconditions**:
- Join counter removed
- All other branches cancelled
- First completed branch result used

#### JOIN-N-OF-M

```
⟨JOIN_WAIT, (ctx, tokens, scopes, ip, join_counters, ...)⟩
    where join_counters[join_id] = {completed, N, {n_of_m, K, M}}
    and completed ≥ K
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

### 4.6 Structured Loop (loop)

#### LOOP-WHILE

Check condition, execute body if true, exit if false.

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where condition(ctx) = true
---------------------------------------------------------------
⟨body_opcode, (ctx, tokens, scopes, body_ip, ...)⟩
```

**Condition true** → execute body.

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where condition(ctx) = false
---------------------------------------------------------------
⟨exit_opcode, (ctx, tokens, scopes, exit_ip, ...)⟩
```

**Condition false** → exit loop.

#### LOOP-UNTIL

Execute body, then check condition.

```
⟨LOOP_BACK, (ctx, tokens, scopes, ip, ...)⟩
    where body_ip is loop start
---------------------------------------------------------------
⟨body_opcode, (ctx, tokens, scopes, body_ip, ...)⟩
```

Always execute body first.

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where condition(ctx) = false
---------------------------------------------------------------
⟨LOOP_BACK, (ctx, tokens, scopes, ip, ...)⟩
```

**Condition false** → repeat (jump back).

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where condition(ctx) = true
---------------------------------------------------------------
⟨exit_opcode, (ctx, tokens, scopes, exit_ip, ...)⟩
```

**Condition true** → exit.

#### LOOP-COUNT

Execute body N times.

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where counter > 0
---------------------------------------------------------------
⟨body_opcode, (ctx, tokens, scopes, body_ip, counter' = counter - 1, ...)⟩
```

```
⟨LOOP_CHECK, (ctx, tokens, scopes, ip, ...)⟩
    where counter = 0
---------------------------------------------------------------
⟨exit_opcode, (ctx, tokens, scopes, exit_ip, ...)⟩
```

### 4.7 Deferred Evaluation (defer)

#### DEFER-WAIT

All branches pending, waiting for external event.

```
⟨DEFER_ENTER, (ctx, tokens, scopes, ip, ...)⟩
    where DEFER_ENTER has N branch targets
---------------------------------------------------------------
⟨DEFER_WAIT, (ctx, tokens', scopes, ip, defer_map, ...)⟩
```

**Postconditions**:
- State marked as waiting_signal
- defer_map tracks N pending branches
- No branches execute until external signal arrives

#### DEFER-SELECT

External event selects one branch.

```
{signal, EventId, Payload}
    where defer_map has branches [branch₁, ..., branchₙ]
    and scheduler selects branch_i based on EventId
---------------------------------------------------------------
⟨branch_opcode_i, (ctx, tokens'', scopes, branch_ip_i, ...)⟩
```

**Preconditions**:
- External signal received
- Scheduler maps EventId to branch_i

**Postconditions**:
- Selected branch executes with Payload
- Other branches cancelled
- defer_map removed

### 4.8 Cancel Scope (cancel)

#### CANCEL-ENTER

Enter cancellation scope.

```
⟨CANCEL_SCOPE, (ctx, tokens, scopes, ip, ...)⟩
    where CANCEL_SCOPE has scope_id
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens, scopes', ip, ...)⟩
```

**Postconditions**:
- scopes' = scopes ∪ {scope_id ⇒ #{status => active, parent ⇒ current_scope}}
- Current scope set to scope_id for all tokens in this region

#### CANCEL-PROPAGATE

Propagate cancellation to all tokens in scope.

```
{cancel, ScopeId}
    where scopes[ScopeId] = #{status => active}
    and tokens_in_scope = {t | t.scope_id = ScopeId}
---------------------------------------------------------------
⟨current, (ctx, tokens', scopes', ip, ...)⟩
```

**Preconditions**:
- Cancel signal received for ScopeId

**Postconditions**:
- scopes'[ScopeId] = #{status => cancelled}
- For all t ∈ tokens_in_scope: t.status = cancelled
- Cancellation propagates to nested scopes recursively
- Time complexity: O(size of scope)

#### CANCEL-EXIT

Exit cancellation scope.

```
⟨CANCEL_SCOPE(exit), (ctx, tokens, scopes, ip, ...)⟩
    where current_scope = ScopeId
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens, scopes', ip, ...)⟩
```

**Postconditions**:
- scopes' = scopes \ {ScopeId} (remove from scope stack)
- Current scope restored to parent scope
- Tokens in scope revert to parent scope_id

### 4.9 Multiple Instances (mi)

#### MI-SPAWN-FIXED

Spawn fixed N instances.

```
⟨MI_SPAWN, (ctx, tokens, scopes, ip, ...)⟩
    where MI_SPAWN has policy {fixed, N}
    and N > 0
---------------------------------------------------------------
⟨MI_SPAWN, (ctx, tokens', scopes, ip, mi_map, ...)⟩
```

**Postconditions**:
- tokens' = original tokens plus N instance tokens:
  - instance_token_i = {id_i, instance_ip_i, scope_id, undefined}
- mi_map tracks: {mi_id, [instance_token₁, ..., instance_tokenₙ], join_policy}

#### MI-SPAWN-DYNAMIC

Spawn dynamic N instances (N computed from ctx).

```
⟨MI_SPAWN, (ctx, tokens, scopes, ip, ...)⟩
    where MI_SPAWN has policy {dynamic, Min, Max}
    and N = compute_instance_count(ctx, Min, Max)
    and Min ≤ N ≤ Max
---------------------------------------------------------------
⟨MI_SPAWN, (ctx, tokens', scopes, ip, mi_map, ...)⟩
```

**Preconditions**:
- Instance count function computes N from context
- N must be within [Min, Max] bounds

#### MI-COLLECT

One instance completes.

```
⟨DONE, (ctx, tokens, scopes, ip, mi_map, ...)⟩
    where instance_token completes for mi_id
    and mi_map[mi_id] = {active, [token, others], policy, results}
---------------------------------------------------------------
⟨next, (ctx, tokens, scopes, ip, mi_map', ...)⟩
```

**Postconditions**:
- mi_map'[mi_id] = {active, [others], policy, results ++ [instance_result]}

#### MI-JOIN

Apply join policy to instance results.

```
⟨MI_JOIN, (ctx, tokens, scopes, ip, mi_map, ...)⟩
    where mi_map[mi_id] = {active, [], policy, results}
    and all instances complete
---------------------------------------------------------------
⟨next_opcode, (ctx, tokens', scopes, ip+1, ...)⟩
```

**Join Policies**:
- **all**: Collect all results, continue when N instances complete
- **wait_n**: Continue when N instances complete, cancel rest
- **first_complete**: Continue when first instance completes, cancel rest
- **n_of_m**: Continue when N out of M complete

## 5. Structural Rules

### 5.1 Context Rule

Context flows through reductions unchanged unless explicitly modified.

```
⟨P, (ctx, ...)⟩ → ⟨P', (ctx', ...)⟩
    where ctx' = ctx unless P modifies ctx
```

### 5.2 Scope Rule

Cancel scopes nest properly. Parent scope contains child scope.

```
enter(ScopeParent)
enter(ScopeChild)  ⇒  ScopeParent is parent of ScopeChild
exit(ScopeChild)   ⇒  ScopeParent is current scope
```

**Invariant**: Scopes never cross. If A encloses B, A must exit after B.

### 5.3 Branch Rule

Parallel branches are isolated. Each branch has its own token and IP.

```
par([P₁, ..., Pₙ]) ⇒ n tokens, each with independent IP
```

Branches don't share state except through ctx (which is merged on join).

### 5.4 Token Rule

Tokens are created on fork/choice, destroyed on join/error.

```
FORK:  1 token → N tokens
JOIN:  N tokens → 1 token (or 0 if all cancelled)
XOR:   1 token → 1 token (selected branch)
```

**Invariant**: Number of active tokens tracks logical parallelism.

### 5.5 Determinism Rule

Under deterministic scheduler, same term + same ctx → same trace.

```
scheduler_policy = deterministic ⇒
⟨P, σ⟩ →* ⟨P', σ'⟩ is unique
```

## 6. Examples

### 6.1 Sequence Example

**Term**: `seq(task(a), task(b))`

**Bytecode**:
```
0: SEQ_ENTER
1: TASK_EXEC a
2: SEQ_NEXT
3: TASK_EXEC b
4: DONE
```

**Execution Trace**:
```
Step 0: ⟨TASK_EXEC a, (ctx₀, {token₀}, ..., ip=1)⟩
        → TASK-EXEC
Step 1: ⟨SEQ_NEXT, (ctx₁, {token₀}, ..., ip=2)⟩
        → SEQ-NEXT (left complete)
Step 2: ⟨TASK_EXEC b, (ctx₁, {token₀}, ..., ip=3)⟩
        → TASK-EXEC
Step 3: ⟨DONE, (ctx₂, {token₀}, ..., ip=4)⟩
        → terminal state
```

### 6.2 Parallel Example

**Term**: `par([task(a), task(b)])`

**Bytecode**:
```
0: PAR_FORK [1, 3]
1: TASK_EXEC a
2: DONE
3: TASK_EXEC b
4: DONE
5: JOIN_WAIT all
```

**Execution Trace**:
```
Step 0: ⟨PAR_FORK, (ctx₀, {token₀}, ..., ip=0)⟩
        → PAR-FORK (spawn 2 tokens: token₁@ip=1, token₂@ip=3)
Step 1: ⟨TASK_EXEC a, (ctx₀, {token₁, token₂}, ..., ip=1)⟩
        → TASK-EXEC (token₁ complete)
Step 2: ⟨TASK_EXEC b, (ctx₀, {token₁, token₂}, ..., ip=3)⟩
        → TASK-EXEC (token₂ complete)
Step 3: ⟨JOIN_WAIT, (ctx₀, {token₁, token₂}, ..., join_counters={join_id, 2, all})⟩
        → JOIN-ALL (both complete, continue)
Step 4: ⟨next, (ctx₂, {token₀}, ..., ip=next)⟩
        → merge results, continue
```

### 6.3 Loop Example

**Term**: `loop({count, 3}, task(a))`

**Bytecode**:
```
0: LOOP_CHECK count=3
1: TASK_EXEC a
2: LOOP_BACK
3: DONE (exit)
```

**Execution Trace**:
```
Step 0: ⟨LOOP_CHECK, (ctx₀, {token₀}, ..., counter=3, ip=0)⟩
        → LOOP-COUNT (counter > 0, execute body)
Step 1: ⟨TASK_EXEC a, (ctx₀, {token₀}, ..., ip=1)⟩
        → TASK-EXEC
Step 2: ⟨LOOP_BACK, (ctx₁, {token₀}, ..., counter=3, ip=2)⟩
        → jump to ip=0
Step 3: ⟨LOOP_CHECK, (ctx₁, {token₀}, ..., counter'=2, ip=0)⟩
        → LOOP-COUNT (2 > 0, execute body)
[... repeats for counter=2, counter=1 ...]
Step N: ⟨LOOP_CHECK, (ctx₃, {token₀}, ..., counter'=0, ip=0)⟩
        → LOOP-COUNT (counter = 0, exit)
Step N+1: ⟨DONE, (ctx₃, {token₀}, ..., ip=3)⟩
        → terminal state
```

## 7. References

- PROMPT.md:97-136 - Kernel types and primitives definition
- PROMPT.md:127-136 - Semantics overview
- docs/ARCHITECTURE.md - System architecture and bytecode design
- Item 002 plan.md - Pattern term algebra types
- Items 004-009 - Bytecode and execution details
```

#### Success Criteria:

##### Manual Verification:

- [ ] File exists at `/Users/speed/wf-substrate/docs/SEMANTICS.md`
- [ ] File is valid Markdown (no syntax errors)
- [ ] All sections from Table of Contents are present
- [ ] Execution state definition includes all components (ctx, tokens, scopes, ip, branch_map, join_counters, step_count)
- [ ] Reduction rule notation is explained clearly
- [ ] All 9 kernel primitives have reduction rules:
  - [ ] Task (TASK-EXEC, TASK-EFFECT, TASK-ERROR)
  - [ ] Sequential composition (SEQ-ENTER, SEQ-NEXT, SEQ-DONE)
  - [ ] Parallel fork (PAR-FORK, PAR-COMPLETE)
  - [ ] Exclusive choice (XOR-CHOOSE)
  - [ ] Join synchronization (JOIN-ALL, JOIN-FIRST-N, JOIN-FIRST-COMPLETE, JOIN-N-OF-M)
  - [ ] Structured loop (LOOP-WHILE, LOOP-UNTIL, LOOP-COUNT)
  - [ ] Deferred evaluation (DEFER-WAIT, DEFER-SELECT)
  - [ ] Cancel scope (CANCEL-ENTER, CANCEL-PROPAGATE, CANCEL-EXIT)
  - [ ] Multiple instances (MI-SPAWN-FIXED, MI-SPAWN-DYNAMIC, MI-COLLECT, MI-JOIN)
- [ ] Each rule has clear preconditions and postconditions
- [ ] Structural rules section covers context, scope, branch, and token invariants
- [ ] Examples section shows step-by-step execution traces
- [ ] All inference rules use consistent ASCII notation
- [ ] Content aligns with PROMPT.md semantics description
- [ ] No implementation-specific details (only semantic specification)

**Note**: Complete manual verification, then proceed to Phase 3.

---

### Phase 3: Review and Refinement

#### Overview

Review both documentation files for consistency, completeness, clarity, and alignment with specifications. Make any necessary corrections before considering the task complete.

#### Changes Required:

##### 1. Consistency Check

**Verification Actions**:
1. Cross-reference ARCHITECTURE.md with PROMPT.md:
   - [ ] Module dependency graph matches PROMPT.md:49-65 (16 modules)
   - [ ] Bytecode VM rationale aligns with PROMPT.md:157-177
   - [ ] Compilation pipeline matches PROMPT.md:157-166
   - [ ] Supervision tree matches PROMPT.md:179-198
   - [ ] Effect boundary matches PROMPT.md:145-156

2. Cross-reference SEMANTICS.md with PROMPT.md:
   - [ ] Kernel primitives list matches PROMPT.md:110-118 (9 primitives)
   - [ ] Reduction rules align with PROMPT.md:127-136 semantics description
   - [ ] Execution state definition matches PROMPT.md:97-104 kernel types

3. Cross-reference both docs with item 002 (pattern algebra):
   - [ ] Type definitions match item 002 research.md:107-157
   - [ ] wf_term() union type matches item 002 plan.md:266-287

4. Cross-reference both docs with items 004-012:
   - [ ] Opcodes match item 004 overview (12 opcodes)
   - [ ] exec_state structure matches item 005 overview
   - [ ] State store description matches item 006
   - [ ] Scheduler policies match item 007
   - [ ] Cancellation scopes match item 008
   - [ ] Multiple instances match item 009
   - [ ] Effect boundary matches item 010
   - [ ] Tracing matches item 011
   - [ ] Supervision tree matches item 012

##### 2. Completeness Check

**ARCHITECTURE.md Checklist**:
- [ ] All 16 modules listed in dependency graph
- [ ] All 12 opcodes documented
- [ ] All 3 scheduler policies described
- [ ] All 3 cancel scopes (activity, case, region) mentioned
- [ ] All join policies (all, first_n, n_of_m, sync_merge, first_complete) covered
- [ ] All loop policies (while, until, count) covered
- [ ] All MI modes (fixed, dynamic) and join policies covered
- [ ] Compilation pipeline has concrete example
- [ ] Supervision tree has ASCII diagram
- [ ] Effect boundary has EffectSpec and Receipt structures

**SEMANTICS.md Checklist**:
- [ ] Execution state fully defined (all 7 components)
- [ ] All 9 kernel primitives have reduction rules
- [ ] All join policy variants have rules
- [ ] All loop policy variants have rules
- [ ] All cancel scope operations have rules
- [ ] All MI operations have rules
- [ ] Structural rules section present
- [ ] Examples section has 3 concrete traces

##### 3. Clarity Check

**Readability Verification**:
- [ ] Read ARCHITECTURE.md from perspective of implementer (items 004-020)
  - Can you understand what module to implement and how it fits?
  - Is the bytecode VM rationale convincing?
  - Is the compilation pipeline clear?
- [ ] Read ARCHITECTURE.md from perspective of user (wf_substrate API users)
  - Can you understand the high-level architecture?
  - Is the effect boundary model clear?
  - Is the supervision tree understandable?
- [ ] Read SEMANTICS.md from perspective of implementer
  - Are the reduction rules unambiguous?
  - Are the pre/post conditions clear?
  - Can you implement an executor from these rules?
- [ ] Check for jargon without definition
- [ ] Check for ambiguous phrasing ("should", "could", "might" → replace with "must", "will", "shall")
- [ ] Check for missing examples where concept is complex

##### 4. Future-Proofing Check

**Language Verification**:
- [ ] Future implementation uses "will" or "shall" (not "is" or "does")
- [ ] Sections marked as "Planned" or "Specified" where appropriate
- [ ] Assumptions documented explicitly
- [ ] Areas of ambiguity noted (e.g., "This specification assumes X")
- [ ] Room left for implementation details to differ
- [ ] No commitment to specific implementation tactics (only strategy)

##### 5. Refinement Actions

If any check fails, make corrections:

1. **Add missing content** (e.g., missing opcode, missing reduction rule)
2. **Clarify ambiguous sections** (add examples, rephrase)
3. **Fix inconsistencies** (align documents with PROMPT.md)
4. **Add cross-references** (link between ARCHITECTURE.md and SEMANTICS.md)
5. **Fix formatting** (Markdown syntax, ASCII diagrams)

#### Success Criteria:

##### Manual Verification:

- [ ] All consistency checks pass
- [ ] All completeness checks pass
- [ ] All clarity checks pass
- [ ] All future-proofing checks pass
- [ ] Both documents are coherent and aligned
- [ ] Documents are ready to guide implementation work (items 004-020)
- [ ] No open questions or unresolved ambiguities remain

**Note**: This is the final phase. Once all checks pass, the implementation is complete.

---

## Testing Strategy

This is a documentation-only task with no executable code. Testing consists of manual verification and review.

### Manual Verification Steps:

**For ARCHITECTURE.md**:
1. Read through entire document for flow and clarity
2. Check all ASCII diagrams render correctly
3. Verify all section references are valid
4. Cross-check module list with PROMPT.md:49-65
5. Cross-check opcodes with item 004
6. Cross-check supervision tree with item 012
7. Verify effect boundary matches item 010

**For SEMANTICS.md**:
1. Read through entire document for mathematical correctness
2. Verify all inference rules have premises and conclusions
3. Check that all 9 kernel primitives are covered
4. Verify reduction rules are internally consistent
5. Check examples follow the rules they illustrate
6. Verify notation is used consistently
7. Check that pre/post conditions are specified for each rule

**Integration Verification**:
1. Verify both documents reference each other appropriately
2. Check terminology is consistent between documents
3. Verify type names match between documents
4. Check opcode names match between documents
5. Verify no contradictions exist between documents

### Review Criteria:

**Completeness**:
- All required sections present
- All kernel primitives covered
- All modules documented
- All opcodes listed

**Correctness**:
- Aligns with PROMPT.md specification
- Aligns with item overviews
- No technical errors
- No contradictions

**Clarity**:
- Readable by target audience (implementers and users)
- Examples provided for complex concepts
- Diagrams aid understanding
- No undefined jargon

**Consistency**:
- Terminology used consistently
- Notation used consistently
- Cross-references accurate
- Both documents aligned

## Migration Notes

Not applicable - this is new documentation with no existing data or systems to migrate.

## References

- **PROMPT.md**: `/Users/speed/wf-substrate/PROMPT.md:1-375` - Authoritative specification
  - Lines 44-66: Deliverables (modules, documentation, patterns)
  - Lines 97-136: Kernel types and semantics
  - Lines 145-156: Effect boundary pattern
  - Lines 157-177: Runtime strategy choice
  - Lines 178-198: OTP design requirements
- **Item 002 research.md**: `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/research.md:1-485` - Pattern term algebra types
- **Item 002 plan.md**: `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/plan.md:1-1747` - Implementation plan
- **Items 004-012**: Module specifications in respective item.json files
- **Item 003 item.json**: `/Users/speed/wf-substrate/.wreckit/items/003-architecture-and-semantics-docs/item.json:1-14` - This task's specification
