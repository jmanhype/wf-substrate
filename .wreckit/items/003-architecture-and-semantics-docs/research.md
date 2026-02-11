# Research: Write architecture and formal semantics documentation

**Date**: 2025-01-10
**Item**: 003-architecture-and-semantics-docs

## Research Question

Write docs/ARCHITECTURE.md covering: module dependency graph (wf_term -> wf_compile -> wf_exec, wf_state, wf_sched, wf_effect, wf_cancel, wf_mi, wf_trace, wf_receipt, wf_validate), runtime strategy choice explaining the bytecode VM approach (why not direct AST interpretation — O(depth) dispatch vs O(1) instruction fetch), compilation pipeline (term -> validate -> compile -> bytecode -> execute), supervision tree layout, and the effect boundary model.

Write docs/SEMANTICS.md with small-step operational semantics (reduction rules) for every kernel primitive: task execution, sequential composition (seq completes left then right), parallel fork (par spawns all branches), exclusive choice (xor enables one branch, disables others), join synchronization (wait-all, wait-n, first-complete policies), structured loop (check condition, execute body or exit), deferred evaluation, cancel scope (enter scope, propagate cancel, exit scope), and multiple instances (spawn N, collect results, apply join policy). Each rule should be written in inference-rule style with clear pre/post conditions on the execution state.

## Summary

This task involves creating two foundational documentation files that establish the theoretical and architectural foundation for the entire wf_substrate project. The documentation must be written **before** implementation of items 004-020, serving as the authoritative specification for the bytecode VM design, execution semantics, and module architecture.

**Key Finding**: This is a **documentation-first** task. The project currently has only the rebar3 scaffold (item 001 complete) and planned but unimplemented pattern algebra (item 002 has research/plan but no code). Items 004-020 are all in "idea" state with no implementation. Therefore, this documentation must be based entirely on:
1. The authoritative specification in PROMPT.md (lines 1-375)
2. The research and plan documents from item 002
3. The item.json overviews from items 004-020

The documentation will define:
- **ARCHITECTURE.md**: System architecture, module dependency graph, compilation pipeline, bytecode VM design rationale, supervision tree, and effect boundary model
- **SEMANTICS.md**: Small-step operational semantics with inference rules for all 9 kernel primitives (task, seq, par, xor, join, loop, defer, cancel, mi)

Both documents must be precise, formal, and implementation-agnostic enough to guide the upcoming implementation work while remaining consistent with the PROMPT.md specification.

## Current State Analysis

### Existing Implementation

**Current State**: The project has only the basic rebar3 scaffold from item 001. No workflow logic, pattern algebra, compiler, or executor exists yet.

**Existing Files**:
- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration with OTP 26+, warnings_as_errors, Dialyzer, no external dependencies
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file with auto-discovery enabled
- `/Users/speed/wf-substrate/src/wf_substrate_app.erl:1-43` - Application callback module
- `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:1-51` - Empty top-level supervisor
- `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder public API (commented exports)
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:1-16` - Empty EUnit test suite
- `/Users/speed/wf-substrate/docs/README.md:1-22` - Documentation placeholder noting that ARCHITECTURE.md and SEMANTICS.md are planned

**Planned But Unimplemented** (from item 002 research.md):
- `wf_term.erl` - Pattern term algebra with 9 kernel constructors (item 002)
- `wf_core.erl` - Derived patterns (item 002)
- `wf_compile.erl` - Bytecode compiler (item 004)
- `wf_vm.erl` - Bytecode definition (item 004)
- `wf_exec.erl` - Executor/reducer hot loop (item 005)
- `wf_state.erl` - State store with atomic commit (item 006)
- `wf_sched.erl` - Scheduler policies (item 007)
- `wf_cancel.erl` - Cancellation semantics (item 008)
- `wf_mi.erl` - Multiple instances (item 009)
- `wf_effect.erl` - Effect boundary (item 010)
- `wf_receipt.erl` - Receipt system (item 010)
- `wf_trace.erl` - Tracing and replay (item 011)
- `wf_validate.erl` - Validation backend (item 013)

**Key Constraint**: Since no implementation exists, the documentation cannot reference actual code implementations. It must describe the **intended** design based on the specification and item overviews.

### Specification Context

The authoritative specification is in `/Users/speed/wf-substrate/PROMPT.md`:

**Lines 97-136**: Kernel Types and Semantics
- Defines ctx(), proc(), exec(), scope_id(), case_id(), receipt() types
- Lists 9 kernel primitives (task, seq, par, xor, join, loop, defer, cancel, mi)
- States: "IMPORTANT: define explicit representation of 'in-progress' state in exec() so the reducer doesn't need to interpret AST each step" (line 137)

**Lines 157-177**: Runtime Strategy Choice
- Explicitly requires choosing between "Strategy S1: Bytecode VM" or "Strategy S2: Continuation Network"
- Bytecode VM: Compile to opcodes, tight loop over exec_state record, no per-step AST dispatch
- Continuation Network: Compile to closure network, reducer stepping frames
- **Decision Point**: This documentation must document WHY bytecode VM is chosen (or choose one)

**Lines 49-65**: Module List
- Authoritative list of 16 modules to implement
- Role assignments for 20-agent swarm (lines 319-341)

**Lines 106-125**: Pattern Algebra Definition
- Authoritative definition of kernel primitives with type signatures
- Derived patterns (simple_merge, synchronizing_merge, discriminator, n_out_of_m)

### Dependency Analysis

From item overviews (items 004-020 item.json files):

**Module Dependency Graph** (must be documented in ARCHITECTURE.md):
```
wf_term (foundation)
  ↓
wf_compile (depends on wf_term)
  ↓
wf_vm (bytecode definition, depends on wf_compile output)
  ↓
wf_exec (executor, depends on wf_vm bytecode)

wf_state (state store, used by wf_exec)
wf_sched (scheduler, used by wf_exec)
wf_cancel (cancellation, used by wf_exec)
wf_mi (multiple instances, used by wf_exec)
wf_effect (effect boundary, used by wf_exec)
wf_receipt (receipts, used by wf_effect)
wf_trace (tracing, used by wf_exec)
wf_validate (validation, standalone but uses wf_term)
wf_core (derived patterns, depends on wf_term)
```

## Key Files

### Specification Files

- `/Users/speed/wf-substrate/PROMPT.md:1-375` - **Primary authoritative specification**. All documentation must be consistent with this file. Key sections:
  - Lines 44-66: Deliverables A (modules), B (documentation), C (patterns)
  - Lines 97-136: Kernel types and term algebra
  - Lines 157-177: Runtime strategy choice (bytecode VM vs continuation network)
  - Lines 178-198: OTP design requirements (supervision tree, per-case runner)
  - Lines 242-268: Pattern implementation mapping table

- `/Users/speed/wf-substrate/.wreckit/items/003-architecture-and-semantics-docs/item.json:1-14` - This item's specification, defining exactly what ARCHITECTURE.md and SEMANTICS.md must cover

### Research and Planning Files

- `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/research.md:1-485` - Research on pattern term algebra. Contains:
  - Lines 107-157: Detailed type definitions (ctx, scope_id, case_id, receipt, task_fun, join_policy, etc.)
  - Lines 266-287: wf_term() union type with 9 constructors
  - These types should be referenced in ARCHITECTURE.md

- `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/plan.md:1-1747` - Implementation plan for pattern algebra. Contains:
  - Lines 135-303: Detailed type specifications with documentation
  - These types are the foundation for SEMANTICS.md reduction rules

### Item Overviews (Future Modules)

- `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/item.json:1-14` - Opcodes: SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, LOOP_CHECK, CANCEL_SCOPE, MI_SPAWN, EFFECT_YIELD, TASK_EXEC, DONE

- `/Users/speed/wf-substrate/.wreckit/items/005-executor-hot-loop/item.json:1-14` - exec_state record: instruction pointer (IP), branch tracking map, join counters, scope stack, token set, reduction counter. Quanta-based execution.

- `/Users/speed/wf-substrate/.wreckit/items/006-state-store-and-atomic-commit/item.json:1-14` - State store holds: ctx(), token tracking, scope metadata, execution metadata. Atomic commit protocol: buffer mutations -> validate -> apply -> produce receipt.

- `/Users/speed/wf-substrate/.wreckit/items/007-scheduler-policies/item.json:1-14` - Three policies: deterministic (stable ordering), nondeterministic (random with log), replay (feed recorded log).

- `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/item.json:1-14` - Three scopes: activity (single task), case (entire workflow), region (scoped subtree). O(scope size) propagation.

- `/Users/speed/wf-substrate/.wreckit/items/009-multiple-instance-support/item.json:1-14` - Instance modes: fixed count, runtime count. Join policies: wait_all, wait_n, first_complete.

- `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/item.json:1-14` - Effect boundary: tasks yield {effect, EffectSpec, Continuation}. Effects are cancelable, idempotent, or at-most-once. Receipts form append-only log.

- `/Users/speed/wf-substrate/.wreckit/items/011-tracing-and-replay/item.json:1-14` - Trace events: step_seq, opcode, state_before, state_after, timestamp, scope, branch_id. Trace levels: none, min, full.

- `/Users/speed/wf-substrate/.wreckit/items/012-otp-supervision-tree/item.json:1-14` - Supervision hierarchy: wf_substrate_sup -> wf_case_sup (dynamic) -> per-case runners (gen_statem). Public API: new_case/3, signal/2, cancel/1, etc.

- `/Users/speed/wf-substrate/.wreckit/items/013-validation-backend/item.json:1-14` - (Not read yet, but validation backend mentioned in PROMPT.md)

### Existing Placeholder

- `/Users/speed/wf-substrate/docs/README.md:1-22` - Notes that ARCHITECTURE.md and SEMANTICS.md are planned but not yet created. This file will be replaced/complemented by the new documentation.

## Technical Considerations

### Documentation Structure

**ARCHITECTURE.md Structure** (based on item.json requirements):
1. **Module Dependency Graph**: Visual or textual representation of how modules depend on each other
2. **Runtime Strategy Choice**: Explanation of bytecode VM approach vs AST interpretation
   - Why bytecode VM: O(1) instruction fetch vs O(depth) dispatch
   - No per-step "case NodeType of ..." in hot loop
   - All structural decisions resolved at compile time
3. **Compilation Pipeline**: term -> validate -> compile -> bytecode -> execute
4. **Supervision Tree Layout**: wf_substrate_sup -> wf_case_sup -> per-case runners
5. **Effect Boundary Model**: How tasks yield effects, how receipts work

**SEMANTICS.md Structure** (based on item.json requirements):
1. **Execution State Definition**: Formal definition of exec_state, tokens, scopes
2. **Small-Step Reduction Rules**: Inference-rule style for each primitive:
   - Task execution
   - Sequential composition (seq)
   - Parallel fork (par)
   - Exclusive choice (xor)
   - Join synchronization (wait-all, wait-n, first-complete)
   - Structured loop (check condition, execute body or exit)
   - Deferred evaluation (defer)
   - Cancel scope (enter, propagate, exit)
   - Multiple instances (spawn N, collect results, apply join policy)
3. **Pre/Post Conditions**: Clear specification of execution state before and after each reduction

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Internal Module Dependencies** (to document in ARCHITECTURE.md):
- wf_term: Foundation, no dependencies
- wf_core: Depends on wf_term
- wf_compile: Depends on wf_term
- wf_vm: Defines bytecode format, depends on wf_compile output structure
- wf_exec: Depends on wf_vm bytecode, wf_state, wf_sched, wf_cancel, wf_mi, wf_effect, wf_trace
- wf_state: Standalone state store
- wf_sched: Standalone scheduler policies
- wf_cancel: Depends on wf_state (scope metadata)
- wf_mi: Depends on wf_exec (MI_SPAWN opcode)
- wf_effect: Standalone effect boundary
- wf_receipt: Depends on wf_effect
- wf_trace: Standalone tracing
- wf_validate: Depends on wf_term

### Patterns to Follow

**Formal Semantics Notation** (for SEMANTICS.md):
- Use inference-rule style: $\frac{precondition}{postcondition}$
- Example: $\frac{\langle seq(P,Q), \sigma \rangle \rightarrow \langle P, \sigma \rangle}{\langle seq(P,Q), \sigma \rangle \rightarrow \langle Q, \sigma' \rangle}$ when P completes
- Document execution state $\sigma = (ctx, tokens, scopes, ip, \dots)$
- Use LaTeX or ASCII art for inference rules

**Documentation Conventions** (from PROMPT.md and item 002 plan):
- Use `-type` and `-spec` notation for type specifications
- Use tagged tuple notation for data structures: `{tag, arg1, arg2}`
- Use atom notation for opcodes and policies: `SEQ_ENTER`, `all`, `while`
- Use function reference notation: `module:function/arity`

**Architecture Diagram Notation**:
- Use ASCII art or PlantUML for dependency graphs
- Use indentation for supervision tree hierarchy
- Use arrows for data flow: term -> compile -> bytecode -> execute

### Bytecode VM Design Rationale

**Why Bytecode VM** (PROMPT.md:157-177 and item 004/005 overviews):

**Advantages over AST Interpretation**:
1. **O(1) instruction fetch** vs O(depth) dispatch: Bytecode is a flat list, executor maintains IP register. AST interpretation requires tree traversal per step.
2. **No per-step "case NodeType of"**: All structural decisions resolved at compile time. Hot loop is flat opcode dispatch.
3. **Explicit exec_state**: Execution state (IP, branch map, join counters, scope stack) is explicit, not implicit in AST structure.
4. **Easier optimization**: Compile-time analysis can optimize bytecode (e.g., constant folding, jump target resolution).
5. **Better tracing/debugging**: IP and opcode sequence provide clear execution trace.

**Disadvantages**:
1. **Compilation overhead**: Need to compile term to bytecode before execution (acceptable tradeoff for long-running workflows)
2. **Debugging complexity**: Need to understand bytecode, not just AST (mitigated by tracing)

**Comparison with Continuation Network** (PROMPT.md:168-172):
- Continuation network also avoids AST dispatch by precomputing closures
- More functional style, but harder to inspect/trace
- Bytecode VM chosen for explicitness and easier observability

### Compilation Pipeline

**From PROMPT.md:157-166 and item 004 overview**:

1. **term (wf_term())**: User-authored pattern term, e.g., `seq(par([task(a), task(b)]), task(c))`
2. **validate**: Run `wf_validate:structural_check/1` to verify term is well-formed (branch counts, policy validity, scope nesting)
3. **compile**: Run `wf_compile:compile/1` which does:
   - Single recursive pass over term
   - Emit opcode list with label targets
   - Resolve labels to integer addresses
   - Return `wf_bc()` (flat list of `{opcode, operands}` tuples)
4. **bytecode**: Compiled bytecode, e.g., `[{SEQ_ENTER, 0}, {PAR_FORK, [2, 5]}, {TASK_EXEC, a}, ...]`
5. **execute**: Run `wf_exec:new/1` to create exec_state, then `wf_exec:step/2` in quanta

**Opcodes** (from item 004):
- `SEQ_ENTER`: Begin sequence scope
- `SEQ_NEXT`: Advance to next step in sequence
- `PAR_FORK`: Fork N parallel branches with label targets
- `JOIN_WAIT`: Block until join condition met
- `XOR_CHOOSE`: Exclusive choice point with branch labels
- `LOOP_BACK`: Jump back to loop head
- `LOOP_CHECK`: Evaluate loop condition, exit or continue
- `CANCEL_SCOPE`: Enter/exit cancel region with scope ID
- `MI_SPAWN`: Spawn multiple instances per config
- `EFFECT_YIELD`: Yield control for external effect
- `TASK_EXEC`: Execute a task (leaf operation)
- `DONE`: Terminate execution path

### Effect Boundary Model

**From PROMPT.md:145-156 and item 010 overview**:

**Effect Boundary Pattern**:
- Tasks do NOT perform side effects directly
- When a task needs external interaction, it returns `{effect, EffectSpec, ContCtx}`
- Executor yields to `wf_effect` manager
- `wf_effect` executes EffectSpec, returns Result
- Executor resumes continuation with Result, produces receipt

**EffectSpec Structure** (item 010):
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

**Receipt Structure** (item 010):
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

**Cancellation of Effects**:
- If cancel scope containing effect is cancelled, effect is marked cancelled
- In-flight effects are cancelled via wf_effect:cancel_effect/2
- Receipts record cancellation for audit trail

### Supervision Tree Layout

**From PROMPT.md:179-198 and item 012 overview**:

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

**Per-Case Runner** (gen_statem):
- **States**: initializing, running, waiting_effect, waiting_signal, cancelled, done
- **Data**: exec_state, step_quanta, trace_level, effect_handler
- **Messages**: {signal, Signal}, {effect_result, EffectId, Result}, {cancel, Reason}, {cancel_region, ScopeId}
- **Timeouts**: state_timeout for overall case timeout

**Public API** (wf_substrate.erl, from PROMPT.md:273-282):
- `new_case(ProcTerm, InitCtx, Options) -> {ok, CasePid, CaseId}`
- `signal(CasePidOrId, Msg) -> ok`
- `cancel(CasePidOrId) -> ok`
- `cancel_region(CasePidOrId, RegionSpec) -> ok`
- `await(CasePidOrId, Timeout) -> {ok, ResultCtx} | {error, Reason} | timeout`
- `status(CasePidOrId) -> #{state := running|completed|cancelled|failed, steps := N, ...}`
- `trace(CasePidOrId, FromSeq, ToSeq) -> [TraceEvent]`
- `validate(ProcTerm, Options) -> ok | {error, [ValidationIssue]}`

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Documentation precedes implementation** | High | The documentation is speculative since no implementation exists. Mitigation: Base documentation strictly on PROMPT.md specification and item overviews. Use "will" and "shall" language for future work. Mark sections as "planned" or "specified" rather than "implemented". |
| **Inconsistency with future implementation** | High | Documentation might describe features that change during implementation. Mitigation: Keep documentation at the right level of abstraction. Describe semantics and architecture, not low-level implementation details. Update documentation when implementation diverges. |
| **Ambiguity in PROMPT.md** | Medium | The specification may have ambiguous or contradictory requirements. Mitigation: Document assumptions explicitly. When multiple interpretations exist, choose the one that best satisfies the stated goals (O(1) dispatch, no AST interpretation). |
| **Bytecode VM vs Continuation Network choice** | Medium | PROMPT.md says "choose one" but doesn't mandate which. Mitigation: Choose bytecode VM and document rationale clearly (explicit state, easier tracing, O(1) dispatch). Acknowledge continuation network as alternative. |
| **Formal semantics notation** | Low | Inference rules may be unfamiliar to some readers. Mitigation: Provide both formal notation and explanatory text. Use ASCII art if LaTeX is not available. Include examples. |
| **Module dependency changes** | Low | Planned dependencies might change during implementation. Mitigation: Document dependencies as "planned" or "specified". Note that actual implementation may differ. |
| **Omission of kernel primitives** | Medium | Documentation might miss reduction rules for some primitives. Mitigation: Use checklist of 9 kernel primitives from PROMPT.md:110-118. Verify all are covered in SEMANTICS.md. |
| **Effect boundary underspecification** | Medium | PROMPT.md is brief on effect system. Mitigation: Use item 010 overview as primary source. Document what's specified and mark areas as "to be detailed in implementation". |

## Recommended Approach

**High-Level Strategy**: Create two comprehensive, specification-aligned documentation files that serve as the authoritative architectural and semantic foundation for all subsequent implementation work (items 004-020).

### Phase 1: ARCHITECTURE.md - System Architecture and Design

**1. Module Dependency Graph Section**
- Create visual/graphical representation of module dependencies
- Use ASCII art or PlantUML
- Show wf_term as foundation, wf_compile/wf_vm as intermediate, wf_exec as consumer
- Show cross-cutting concerns: wf_state, wf_sched, wf_cancel, wf_effect, wf_trace
- Add table explaining each module's responsibility

**2. Runtime Strategy Choice Section**
- Title: "Why Bytecode VM?"
- Compare AST interpretation vs bytecode VM
- Table: "Dispatch Complexity" showing O(depth) vs O(1)
- Explain "No Per-Step 'case NodeType of'" requirement from PROMPT.md:27
- Show explicit exec_state structure (IP, branch map, join counters, scope stack)
- Explain compilation overhead vs runtime performance tradeoff
- Acknowledge continuation network as alternative and explain why bytecode VM is chosen

**3. Compilation Pipeline Section**
- Flowchart: term -> validate -> compile -> bytecode -> execute
- Detail each stage:
  - **term**: wf_term() from item 002, show example
  - **validate**: wf_validate checks (well_formed, structural, bounded)
  - **compile**: wf_compile single-pass recursive compilation, label resolution
  - **bytecode**: wf_bc() flat list, show example opcode sequence
  - **execute**: wf_exec stepper, quanta-based execution
- Show concrete example: Compile `seq(par([task(a), task(b)]), task(c))` to bytecode

**4. Supervision Tree Layout Section**
- ASCII tree: wf_substrate_sup -> wf_case_sup -> case_runner_N
- Explain each level's responsibility
- Detail per-case runner states (initializing, running, waiting_effect, etc.)
- Show message flow: external signals -> case_runner -> executor -> effect handler
- Include fault tolerance: restart strategies, error handling

**5. Effect Boundary Model Section**
- Explain problem: Tasks need external IO but substrate is pure
- Show pattern: Task returns `{effect, EffectSpec, ContCtx}` instead of doing IO
- Detail EffectSpec structure (effect_id, type, payload, options)
- Detail receipt structure (receipt_id, effect_id, result, timestamp)
- Explain lifecycle: yield -> execute -> resume -> receipt
- Explain cancellation: how in-flight effects are cancelled
- Explain idempotency: how idempotency_key provides at-most-once semantics

**6. Type System Overview Section**
- Summarize types from item 002 research.md
- Show kernel types: ctx(), scope_id(), case_id(), receipt()
- Show policy types: join_policy(), loop_policy(), mi_policy()
- Show wf_term() union type with 9 constructors
- Reference wf_term.erl as authoritative source

### Phase 2: SEMANTICS.md - Small-Step Operational Semantics

**1. Execution State Definition Section**
- Define formal execution state $\sigma = (ctx, tokens, scopes, ip, \dots)$
- Define token structure: each token has position (IP) and value
- Define scope structure: cancel scopes with nesting
- Define branch tracking: for par/xor branches
- Define join counters: for join policies
- Use mathematical notation or Erlang record notation

**2. Reduction Rule Notation Section**
- Explain inference-rule style: $\frac{precondition}{postcondition}$
- Define configuration: $\langle P, \sigma \rangle$ (program term, execution state)
- Define reduction: $\langle P, \sigma \rangle \rightarrow \langle P', \sigma' \rangle$
- Provide legend for all notation used

**3. Kernel Primitive Reduction Rules**

For each of the 9 primitives, provide:

**Task Execution** (PROMPT.md:110):
- Rule for task start: Create effect or execute pure function
- Rule for task completion: Update ctx, advance to next instruction
- Rule for task with effect: Yield to effect boundary
- Show both pure task and effect-yielding task

**Sequential Composition** (seq, PROMPT.md:111):
- Rule SEQ-LEFT: $\frac{\langle seq(P,Q), \sigma \rangle \rightarrow \langle P, \sigma \rangle}{\dots}$
- Rule SEQ-RIGHT: When P completes, continue with Q
- Show IP manipulation: SEQ_ENTER pushes return address, SEQ_NEXT pops it

**Parallel Fork** (par, PROMPT.md:112):
- Rule PAR-FORK: Split execution into N branches
- Each branch gets its own token with separate IP
- All branches execute concurrently (logically)
- Show branch map creation: tracking N active branches

**Exclusive Choice** (xor, PROMPT.md:113):
- Rule XOR-CHOOSE: Select one branch, disable others
- Only selected branch gets token
- Unselected branches are never executed (not just cancelled)
- Show branch selection policy (deterministic or nondeterministic)

**Join Synchronization** (join, PROMPT.md:114):
- Rule JOIN-ALL: Wait for all N branches to complete
- Rule JOIN-FIRST-N: Wait for N branches, cancel remainder
- Rule JOIN-FIRST-COMPLETE: Wait for first branch, cancel all others
- Show join counter: increment on completion, check condition
- Show cancellation of unselected branches

**Structured Loop** (loop, PROMPT.md:115):
- Rule LOOP-WHILE: Check condition, execute body if true, exit if false
- Rule LOOP-UNTIL: Execute body, check condition, repeat if false, exit if true
- Rule LOOP-COUNT: Execute body N times, decrement counter
- Show IP manipulation: LOOP_BACK jumps to loop head

**Deferred Evaluation** (defer, PROMPT.md:116):
- Rule DEFER-WAIT: All branches are pending, waiting for external event
- Rule DEFER-SELECT: External event selects one branch, others are cancelled
- Show race semantics: first external trigger wins

**Cancel Scope** (cancel, PROMPT.md:117):
- Rule CANCEL-ENTER: Push scope ID onto scope stack
- Rule CANCEL-PROPAGATE: When cancel signal received, mark all tokens in scope as cancelled
- Rule CANCEL-EXIT: Pop scope ID from scope stack
- Show O(scope size) cancellation: walk only tokens in scope
- Show cancellation of in-flight effects

**Multiple Instances** (mi, PROMPT.md:118):
- Rule MI-SPAWN-FIXED: Create N instances from fixed config
- Rule MI-SPAWN-DYNAMIC: Create N instances from runtime ctx() evaluation
- Rule MI-COLLECT: Each instance reports result to collector
- Rule MI-JOIN: Apply join policy (wait_all, wait_n, first_complete)
- Show instance tracking: each instance has unique instance_id

**4. Structural Rules Section**
- Rule CONTEXT: How ctx() flows through reductions
- Rule SCOPE: How cancel scopes nest and propagate
- Rule BRANCH: How parallel branches are isolated
- Rule TOKEN: How tokens are created, destroyed, and merged

**5. Examples Section**
- Trace execution of simple workflow: `seq(task(a), task(b))`
- Trace execution of parallel workflow: `par([task(a), task(b)])`
- Trace execution of loop: `loop({count, 3}, task(a))`
- Show state evolution at each step

### Phase 3: Review and Refinement

**1. Consistency Check**
- Verify ARCHITECTURE.md aligns with PROMPT.md
- Verify SEMANTICS.md rules align with PROMPT.md semantics description
- Verify both docs align with item 002 type definitions
- Verify both docs align with item 004-012 overviews

**2. Completeness Check**
- Checklist: All 9 kernel primitives have reduction rules
- Checklist: All join policies (all, first_n, n_of_m, sync_merge) covered
- Checklist: All loop policies (while, until, count) covered
- Checklist: All cancel scopes (activity, case, region) covered
- Checklist: All MI modes (fixed, dynamic) and join policies covered

**3. Clarity Check**
- Read docs from perspective of implementer (items 004-020)
- Read docs from perspective of user (wf_substrate API users)
- Add examples for complex concepts
- Add diagrams for complex flows (compilation, execution)

**4. Future-Proofing**
- Mark sections as "Planned" or "To Be Implemented" where appropriate
- Use "will" language for future work
- Note areas where specification is ambiguous and document assumptions
- Leave room for implementation details to differ

## Open Questions

1. **Bytecode VM vs Continuation Network**: PROMPT.md:157-177 says "choose one" but doesn't mandate which. Should we document bytecode VM as the chosen approach, or document both as options?
   - **Recommendation**: Document bytecode VM as the chosen approach with clear rationale. This aligns with item 004 (bytecode compiler) and item 005 (executor hot loop) overviews which explicitly describe bytecode VM. Mention continuation network as an alternative in a "Design Alternatives" section.

2. **Formal Semantics Notation**: Should we use LaTeX mathematical notation (requiring LaTeX tooling) or ASCII art for inference rules?
   - **Recommendation**: Use ASCII art for portability (Markdown renders everywhere). Include LaTeX notation in comments or as optional alternative. Example: Use "(premise)/(conclusion)" format with ASCII arrows.

3. **Level of Detail for exec_state**: SEMANTICS.md must define execution state, but the exact structure isn't finalized. How detailed should we be?
   - **Recommendation**: Define exec_state abstractly as a tuple (ctx, tokens, scopes, ip, branch_map, join_counters, step_count). Note that the exact Erlang record structure will be defined in wf_exec.erl (item 005). Keep semantics at the level of "what" not "how".

4. **Effect System Semantics**: PROMPT.md:145-156 is brief on effects. Item 010 has more detail but is also high-level. How detailed should SEMANTICS.md be on effect reduction rules?
   - **Recommendation**: Provide high-level reduction rule for effect yield: Task -> {effect, Spec} -> EffectExecution -> Resume. Don't specify exact effect protocol or EffectSpec fields. Note that detailed effect semantics will be in item 010.

5. **Cancellation Semantics**: How are cancelled tasks represented in the execution state? Do they leave tokens, are they removed from branch map, or marked as cancelled?
   - **Recommendation**: Define cancelled tasks as having tokens marked with cancelled status. Cancellation propagates by marking tokens, not removing them (removal would break branch tracking). Document this clearly in SEMANTICS.md cancel scope rules.

6. **Nondeterminism in Scheduler**: Should SEMANTICS.md define how scheduler choices are made, or treat scheduler as a nondeterministic oracle?
   - **Recommendation**: Treat scheduler as a nondeterministic choice function choose(EnabledActions) -> SelectedAction. Document that wf_sched (item 007) will implement specific policies (deterministic, nondeterministic, replay). Keep semantics abstract: "scheduler selects one enabled action".

7. **Validation Backend Role**: ARCHITECTURE.md should explain wf_validate (item 013). Is this part of compilation pipeline or a separate testing tool?
   - **Recommendation**: Document wf_validate as having two roles: (1) Structural validation in compilation pipeline (fast checks), (2) Bounded model checking as testing tool (deep checks). Make this distinction clear in ARCHITECTURE.md.

8. **Trace Event Semantics**: Should SEMANTICS.md include trace emission as part of reduction rules, or treat tracing as orthogonal to semantics?
   - **Recommendation**: Treat tracing as orthogonal. Reduction rules define the minimal semantics. Tracing is an observation mechanism that doesn't affect semantics. Mention this in SEMANTICS.md introduction.

9. **Error Handling in Semantics**: How should errors (task failure, effect timeout) be represented in reduction rules?
   - **Recommendation**: Define error as a special token state or ctx field. Errors propagate like cancellations but with different semantics. Provide reduction rules for error propagation. Note that full error handling will be detailed in implementation.

10. **Atomic Commit Protocol**: ARCHITECTURE.md should explain atomic commit (item 006). Is this part of exec_state semantics or an implementation detail?
    - **Recommendation**: Treat atomic commit as an implementation detail for crash recovery. SEMANTICS.md can assume state updates are atomic. ARCHITECTURE.md should explain the commit protocol as part of state store design, not as part of core semantics.

11. **Receipt Semantics**: Are receipts part of the execution state or an external log?
    - **Recommendation**: Receipts are an external append-only log, not part of core execution state. They are produced by state transitions but don't affect subsequent reductions. Document this in ARCHITECTURE.md effect boundary section.

12. **Module Dependency Cycles**: Are there any planned circular dependencies (e.g., wf_exec depends on wf_cancel, wf_cancel depends on wf_state, wf_state used by wf_exec)?
    - **Recommendation**: Document that wf_exec is the central module coordinating all others. Dependencies radiate from wf_exec, not circular. If actual implementation has cycles, document them and explain why they're acceptable (e.g., callback interfaces).

13. **Documentation Format**: Should these be single Markdown files or split into multiple files (e.g., ARCHITECTURE.md, COMPILATION.md, EXECUTION.md)?
    - **Recommendation**: Keep as single ARCHITECTURE.md and SEMANTICS.md as specified in item.json. Use clear section headers and table of contents. If docs grow too large, note that they can be split in future (e.g., SEMANTICS.md could split into KERNEL_SEMANTICS.md and DERIVED_SEMANTICS.md).

14. **Examples in SEMANTICS.md**: Should reduction rules include concrete examples with specific wf_term() structures?
    - **Recommendation**: Yes, include examples section at end of SEMANTICS.md. Show step-by-step execution of simple workflows. This makes formal semantics more accessible. Reference examples that will be implemented in items 018 (example workflows).

15. **Derived Patterns in SEMANTICS.md**: Should SEMANTICS.md cover derived patterns (simple_merge, discriminator, etc.) or only kernel primitives?
    - **Recommendation**: Focus on kernel primitives (9 of them). Briefly mention that derived patterns are macros that expand to kernel primitives, so their semantics are derived. Detailed derived pattern semantics can be in docs/PATTERNS.md (item 017).
