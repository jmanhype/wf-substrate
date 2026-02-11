# Architecture of wf_substrate

## Table of Contents

1. [Overview](#1-overview)
2. [Module Dependency Graph](#2-module-dependency-graph)
3. [Runtime Strategy Choice](#3-runtime-strategy-choice)
4. [Compilation Pipeline](#4-compilation-pipeline)
5. [Supervision Tree Layout](#5-supervision-tree-layout)
6. [Effect Boundary Model](#6-effect-boundary-model)
7. [Type System Overview](#7-type-system-overview)
8. [Module Responsibilities](#8-module-responsibilities)
9. [Design Alternatives](#9-design-alternatives)
10. [References](#10-references)

---

## 1. Overview

wf_substrate is a **pure Erlang/OTP workflow pattern substrate** that compiles workflow patterns to bytecode for O(1) execution. It implements the 43 workflow patterns (control-flow focus) using only 9 kernel primitives, providing a foundational control substrate for building reliable, observable, and testable workflow systems.

**Key Design Principles:**

- **Compiled Execution**: Workflow patterns compile to bytecode, avoiding per-step AST interpretation overhead
- **Explicit State**: Execution state is explicit (IP, branch map, join counters), not implicit in tree structure
- **Structured Cancellation**: Three scope levels (activity, case, region) with O(scope size) propagation
- **Effect Boundary**: Pure substrate with explicit effect boundaries for external IO
- **Observability**: Every reduction step produces structured trace events for replay and debugging
- **OTP Integration**: Full supervision tree, per-case gen_statem processes, fault tolerance

**What This Document Covers:**

This document specifies the system architecture, module organization, compilation pipeline, supervision tree, and effect boundary model. It serves as the authoritative blueprint for implementation work (items 004-020).

**Companion Documents:**

- `docs/SEMANTICS.md`: Small-step operational semantics with inference rules
- `docs/PATTERNS.md`: Mapping of 43 patterns to kernel terms and bytecode (future, item 017)
- `docs/TESTING.md`: Test strategy and validation approaches (future, item 014-016)
- `docs/OPERATIONS.md`: Deployment, monitoring, and failure handling (future, item 020)

---

## 2. Module Dependency Graph

### 2.1 Dependency Diagram

The wf_substrate system consists of 16 modules organized in a layered dependency graph:

```
Foundation Layer (No Dependencies)
├── wf_term.erl          [Pattern algebra AST - 9 kernel constructors]
│
Pattern Library Layer
├── wf_core.erl          [Derived patterns - depends on wf_term]
│
Compilation Layer
├── wf_compile.erl       [Bytecode compiler - depends on wf_term]
└── wf_vm.erl            [Bytecode definition - depends on wf_compile output]
│
Execution Layer
└── wf_exec.erl          [Executor hot loop - depends on wf_vm bytecode]
    │
    ├── wf_state.erl     [State store - used by wf_exec]
    ├── wf_sched.erl     [Scheduler policies - used by wf_exec]
    ├── wf_cancel.erl    [Cancellation - used by wf_exec]
    ├── wf_mi.erl        [Multiple instances - used by wf_exec]
    ├── wf_effect.erl    [Effect boundary - used by wf_exec]
    └── wf_trace.erl     [Tracing - used by wf_exec]
        │
        └── wf_receipt.erl  [Receipts - used by wf_effect]
│
Validation Layer
└── wf_validate.erl      [Validation backend - depends on wf_term]
│
OTP Integration Layer
├── wf_substrate.erl     [Public API]
├── wf_substrate_app.erl [Application callback]
├── wf_substrate_sup.erl [Top-level supervisor]
├── wf_case_sup.erl      [Dynamic case supervisor]
└── wf_case_runner.erl   [Per-case gen_statem]
```

### 2.2 Dependency Flow

**Compilation Flow:**
```
User authors wf_term() → wf_compile compiles → wf_bc() bytecode → wf_exec executes
```

**Execution Flow:**
```
wf_exec coordinates:
  ├─ wf_state for persistence
  ├─ wf_sched for action selection
  ├─ wf_cancel for cancellation
  ├─ wf_mi for instance management
  ├─ wf_effect for external effects
  └─ wf_trace for observability
```

### 2.3 Module Table

| Module | Role | Dependencies | Item |
|--------|------|--------------|------|
| `wf_term.erl` | Pattern algebra AST (9 kernel constructors) | None | 002 |
| `wf_core.erl` | Derived patterns (simple_merge, discriminator, etc.) | wf_term | 002 |
| `wf_compile.erl` | Compile wf_term() to wf_bc() bytecode | wf_term | 004 |
| `wf_vm.erl` | Bytecode definition and opcode types | wf_compile (output structure) | 004 |
| `wf_exec.erl` | Bytecode executor hot loop | wf_vm, wf_state, wf_sched, wf_cancel, wf_mi, wf_effect, wf_trace | 005 |
| `wf_state.erl` | Per-case state store with atomic commit | None | 006 |
| `wf_sched.erl` | Scheduling policies (deterministic, nondeterministic, replay) | None | 007 |
| `wf_cancel.erl` | Cancellation semantics (activity/case/region scopes) | wf_state | 008 |
| `wf_mi.erl` | Multiple instances support (fixed/dynamic) | wf_exec | 009 |
| `wf_effect.erl` | Effect boundary implementation | wf_receipt | 010 |
| `wf_receipt.erl` | Receipt storage and idempotency | None | 010 |
| `wf_trace.erl` | Structured tracing and replay log | None | 011 |
| `wf_validate.erl` | Structural validation and bounded model checking | wf_term | 013 |
| `wf_substrate.erl` | Public API module | All above | 012 |
| `wf_substrate_app.erl` | Application behavior callback | OTP | 012 |
| `wf_substrate_sup.erl` | Top-level supervisor | OTP | 012 |
| `wf_case_sup.erl` | Dynamic case supervisor | OTP | 012 |
| `wf_case_runner.erl` | Per-case gen_statem process | wf_exec | 012 |

---

## 3. Runtime Strategy Choice

### 3.1 The Problem: AST Interpretation Overhead

A naive workflow engine might interpret the AST at every step:

```erlang
%% WRONG: Per-step "case NodeType of" dispatch
execute_step({seq, Left, Right}, State) ->
    case execute_step(Left, State) of
        {done, NewState} -> execute_step(Right, NewState);
        {continue, _} -> ...
    end;
execute_step({par, Branches}, State) ->
    %% Tree walk at every step
    ...
```

**Problems with AST Interpretation:**

1. **O(depth) Dispatch**: Each step requires walking the AST to find the current node
2. **Per-Step Pattern Matching**: "case NodeType of" at every reduction step
3. **Implicit State**: Execution position is implicit in AST structure, not explicit
4. **Hard to Trace**: No clear instruction pointer or execution sequence
5. **Cannot Optimize**: Structural decisions re-evaluated at runtime

**PROMPT.md Requirement** (lines 23-27):

> "Runtime must not repeatedly interpret node types in a slow dispatch loop. Runtime must operate on a compiled executable form (bytecode + direct reducer OR compiled continuation network). Avoid per-step 'case NodeType of ...' in the hot loop."

### 3.2 Strategy Comparison

| Approach | Dispatch Complexity | Per-Step Overhead | Tracing Difficulty | Optimization Potential |
|----------|-------------------|-------------------|-------------------|----------------------|
| **AST Interpretation** | O(depth) tree walk | High (node dispatch + pattern match) | Hard (AST traversal path) | None (structure frozen) |
| **Bytecode VM** | O(1) instruction fetch | Low (flat opcode list) | Easy (IP sequence) | High (compile-time analysis) |
| **Continuation Network** | O(1) closure call | Medium (closure allocation) | Medium (call stack) | Medium (partial evaluation) |

### 3.3 Why Bytecode VM?

wf_substrate chooses **Strategy S1: Bytecode VM**. The decision rationale:

#### 3.3.1 O(1) Instruction Fetch

Bytecode is a flat list of instructions. The executor maintains an instruction pointer (IP) register:

```erlang
%% Bytecode: Flat list
[{SEQ_ENTER, 0},
 {PAR_FORK, [2, 5, 8]},
 {TASK_EXEC, task_a},
 {DONE},
 ...]

%% Executor: IP points to next instruction
#exec_state{ip = 3, bytecode = Bytecode}
%% Fetch: element(3, Bytecode) = {TASK_EXEC, task_a}
```

**vs AST Interpretation**: O(depth) tree walk to find current node at every step.

#### 3.3.2 No Per-Step "case NodeType of"

All structural decisions are resolved at compile time:

```erlang
%% COMPILE TIME: wf_compile.erl
compile({seq, Left, Right}) ->
    [{SEQ_ENTER, 0},
     ...Left bytecode...,   %% Jump target for left
     {SEQ_NEXT, right_label},
     {label, right_label},
     ...Right bytecode...].

%% RUNTIME: wf_exec.erl
%% No "case NodeType of" - just execute opcode
execute_opcode({SEQ_ENTER, _}, State) ->
    State#exec_state{scope_stack = [current_scope | State#exec_state.scope_stack]};
execute_opcode({SEQ_NEXT, TargetIP}, State) ->
    State#exec_state{ip = TargetIP};
```

#### 3.3.3 Explicit exec_state

Execution state is explicit and flat:

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),           %% Instruction pointer
    bytecode :: wf_bc(),               %% Compiled bytecode
    ctx :: ctx(),                      %% User context
    tokens :: #{token_id() => token()},%% Active tokens
    branch_map :: #{branch_id() => branch_info()}, %% Parallel branches
    join_counters :: #{join_id() => join_counter()}, %% Join state
    scope_stack :: [scope_id()],       %% Cancellation scope stack
    step_count :: non_neg_integer()    %% Reduction counter
}).
```

**vs AST Interpretation**: Execution position implicit in recursive structure, requiring tree traversal to determine "where we are".

#### 3.3.4 Easy Tracing and Debugging

IP and opcode sequence provide clear execution trace:

```erlang
%% Trace Event
#trace_event{
    step_seq = 42,
    opcode = {PAR_FORK, [2, 5, 8]},
    ip_before = 3,
    ip_after = 4,
    state_before = #{...},
    state_after = #{...}
}
```

**vs AST Interpretation**: Must trace AST path and node addresses, harder to reconstruct execution flow.

#### 3.3.5 Compile-Time Optimization

Bytecode enables optimization passes (future enhancement):

- **Constant Folding**: Evaluate pure tasks at compile time
- **Jump Target Resolution**: Convert labels to integer addresses
- **Dead Code Elimination**: Remove unreachable branches
- **Opcode Fusion**: Combine sequences into single instructions

### 3.4 Tradeoffs

**Compilation Overhead**:
- Must compile wf_term() to bytecode before execution
- **Acceptable** for long-running workflows (compile once, execute many times)
- **Mitigated** by fast single-pass compiler (wf_compile:compile/1)

**Debugging Complexity**:
- Developers must understand bytecode, not just AST
- **Mitigated** by:
  - Human-readable opcode names (SEQ_ENTER, PAR_FORK, etc.)
  - Tracing shows opcode sequence
  - Source mapping (wf_term → bytecode) for debugging

**Memory Usage**:
- Bytecode is additional data structure
- **Mitigated** by compact representation (tagged tuples, no duplication)

### 3.5 Alternative: Continuation Network

**Strategy S2** (PROMPT.md:168-172) compiles wf_term() to a network of closures:

```erlang
%% Continuation network approach
compile({seq, Left, Right}) ->
    fun(Ctx) ->
        LeftCont = compile(Left),
        case LeftCont(Ctx) of
            {done, NewCtx} -> (compile(Right))(NewCtx);
            {continue, _} -> ...
        end
    end.
```

**Advantages:**
- Functional style, more "Erlang-ish"
- O(1) closure calls (no AST dispatch)

**Disadvantages:**
- Harder to inspect and trace (call stack vs flat instruction sequence)
- Closure allocation overhead
- Less explicit execution state
- Harder to optimize (cannot analyze closure network as easily)

**Why Bytecode VM Was Chosen:**

1. **Observability**: IP and opcode sequence provide clear execution trace
2. **Explicit State**: exec_state is explicit, not buried in closure chain
3. **Optimization Potential**: Easier to analyze and optimize flat bytecode
4. **Debugging**: Opcodes are human-readable, closure chain is not
5. **Spec Alignment**: Matches PROMPT.md:162-166 description of "tight loop over exec_state"

---

## 4. Compilation Pipeline

### 4.1 Pipeline Stages

```
┌─────────────────────────────────────────────────────────────┐
│ Stage 1: term (wf_term())                                   │
│ User-authored pattern term                                   │
│ Example: seq(par([task(a), task(b)]), task(c))              │
└────────────────────┬────────────────────────────────────────┘
                     ↓ validate
┌────────────────────┴────────────────────────────────────────┐
│ Stage 2: wf_validate:structural_check/1                     │
│ Checks: well_formed, structural invariants, bounded         │
└────────────────────┬────────────────────────────────────────┘
                     ↓ compile
┌────────────────────┴────────────────────────────────────────┐
│ Stage 3: wf_compile:compile/1                               │
│ Single recursive pass over AST, emit opcodes with labels    │
└────────────────────┬────────────────────────────────────────┘
                     ↓ resolve labels
┌────────────────────┴────────────────────────────────────────┐
│ Stage 4: bytecode (wf_bc())                                 │
│ Flat list of {opcode, operands} tuples                     │
│ Example: [{SEQ_ENTER,0}, {PAR_FORK,[2,5]}, {TASK_EXEC,a}, ..]│
└────────────────────┬────────────────────────────────────────┘
                     ↓ execute
┌────────────────────┴────────────────────────────────────────┐
│ Stage 5: wf_exec:new/1 → wf_exec:step/2                    │
│ Execute bytecode in quanta, update exec_state               │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 Stage Details

#### Stage 1: term (wf_term())

**Purpose**: User-authored workflow pattern using kernel constructors.

**Example**:
```erlang
Term = seq(
    par([
        task(verify_payment, #{function => fun verify_payment/1}),
        task(check_inventory, #{function => fun check_inventory/1})
    ]),
    task(ship_order, #{function => fun ship_order/1})
).
```

**Type** (from item 002):
```erlang
-type wf_term() ::
    {task, atom(), task_metadata()} |
    {seq, wf_term(), wf_term()} |
    {par, [wf_term()]} |
    {xor, [wf_term()]} |
    {join, join_policy(), [wf_term()]} |
    {loop, loop_policy(), wf_term()} |
    {defer, [wf_term()]} |
    {cancel, cancel_scope(), wf_term()} |
    {mi, mi_policy(), wf_term()}.
```

#### Stage 2: validate

**Purpose**: Ensure term is well-formed before compilation.

**Module**: `wf_validate:structural_check/1` (fast path) or `wf_validate:bounded_check/2` (deep validation)

**Checks**:
- **Well-formed**: Branch counts (par, xor, defer require ≥ 2), policy validity
- **Structural**: Cancel scopes properly nested, no orphan joins
- **Bounded** (optional): No deadlock, option to complete (item 013)

**Example**:
```erlang
case wf_validate:structural_check(Term) of
    ok -> wf_compile:compile(Term);
    {error, Issues} -> {error, validation_failed, Issues}
end.
```

#### Stage 3: compile

**Purpose**: Transform wf_term() AST into flat bytecode.

**Module**: `wf_compile:compile/1`

**Process**:
1. Single recursive pass over wf_term() tree
2. Emit opcode list with label targets (e.g., `{label, loop_start}`)
3. Return unresolved bytecode (labels present)

**Algorithm** (simplified):
```erlang
compile({seq, Left, Right}) ->
    LeftCode = compile(Left),
    RightCode = compile(Right),
    RightLabel = make_label(),
    [
        {SEQ_ENTER, 0},
        LeftCode,
        {SEQ_NEXT, RightLabel},
        {label, RightLabel},
        RightCode
    ];

compile({par, Branches}) ->
    BranchCodes = [compile(B) || B <- Branches],
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),
    [
        {PAR_FORK, BranchLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, BranchCodes, BranchLabels),
        {label, JoinLabel},
        {JOIN_WAIT, all}
    ].
```

#### Stage 4: bytecode

**Purpose**: Resolved, executable bytecode.

**Type**:
```erlang
-type wf_bc() :: [opcode()].
-type opcode() ::
    {SEQ_ENTER, non_neg_integer()} |
    {SEQ_NEXT, non_neg_integer()} |
    {PAR_FORK, [non_neg_integer()]} |
    {JOIN_WAIT, join_policy()} |
    {XOR_CHOOSE, [non_neg_integer()]} |
    {LOOP_BACK, non_neg_integer()} |
    {LOOP_CHECK, loop_policy()} |
    {CANCEL_SCOPE, scope_id()} |
    {MI_SPAWN, mi_policy()} |
    {EFFECT_YIELD, term()} |
    {TASK_EXEC, atom()} |
    {DONE}.
```

**Label Resolution**:
- Replace `{label, Label}` with integer index
- Replace label references with integer targets

**Example**:
```erlang
%% Before resolution:
[
    {PAR_FORK, [label_a, label_b]},
    {label, label_a},
    {TASK_EXEC, task_a},
    {DONE},
    {label, label_b},
    {TASK_EXEC, task_b},
    {DONE}
]

%% After resolution:
[
    {PAR_FORK, [2, 5]},  %% label_a → IP 2, label_b → IP 5
    {TASK_EXEC, task_a},
    {DONE},
    {TASK_EXEC, task_b},
    {DONE}
]
```

#### Stage 5: execute

**Purpose**: Run bytecode to completion or effect boundary.

**Module**: `wf_exec`

**Process**:
1. `wf_exec:new/1`: Create exec_state from bytecode
2. `wf_exec:step/2`: Execute one reduction (return updated exec_state)
3. `wf_exec:run/3`: Execute N quanta (return {yield, State} | {done, State} | {effect, ...})

**Example**:
```erlang
{ok, Bytecode} = wf_compile:compile(Term),
ExecState0 = wf_exec:new(Bytecode, InitialCtx),
case wf_exec:run(ExecState0, 100) of  %% 100 reductions
    {done, ExecState1} ->
        {ok, ExecState1#exec_state.ctx};
    {effect, EffectSpec, Continuation, ExecState1} ->
        %% Handle effect
        wf_effect:execute(EffectSpec, Continuation);
    {yield, ExecState1} ->
        %% Continue later
        wf_exec:run(ExecState1, 100)
end.
```

### 4.3 Concrete Compilation Example

**Source Term**:
```erlang
seq(
    par([
        task(a, #{function => fun task_a/1}),
        task(b, #{function => fun task_b/1})
    ]),
    task(c, #{function => fun task_c/1})
)
```

**Compilation Steps**:

1. **Compile seq(Left, Right)**:
   - Emit `SEQ_ENTER`
   - Compile Left (par)
   - Emit `SEQ_NEXT` with label to Right
   - Emit label
   - Compile Right (task c)

2. **Compile par([task a, task b])**:
   - Emit `PAR_FORK` with labels for branches
   - Emit label for branch 1
   - Compile task a
   - Emit `DONE`
   - Emit label for branch 2
   - Compile task b
   - Emit `DONE`

3. **Compile task X**:
   - Emit `TASK_EXEC` with task name

**Unresolved Bytecode**:
```erlang
[
    {SEQ_ENTER, 0},
    {PAR_FORK, [label_branch1, label_branch2]},
    {label, label_branch1},
    {TASK_EXEC, a},
    {DONE},
    {label, label_branch2},
    {TASK_EXEC, b},
    {DONE},
    {SEQ_NEXT, label_right},
    {label, label_right},
    {TASK_EXEC, c},
    {DONE}
]
```

**Resolved Bytecode** (with IP indices):
```erlang
%% IP:  Opcode
0:   {SEQ_ENTER, 0}
1:   {PAR_FORK, [3, 6]}        %% label_branch1 → 3, label_branch2 → 6
2:   {SKIP}                    %% placeholder (PAR_FORK jumps to branches)
3:   {TASK_EXEC, a}            %% label_branch1
4:   {DONE}
5:   {SKIP}                    %% end of branch 1
6:   {TASK_EXEC, b}            %% label_branch2
7:   {DONE}
8:   {SKIP}                    %% end of branch 2, join point
9:   {SEQ_NEXT, 11}            %% label_right → 11
10:  {SKIP}                    %% placeholder
11:  {TASK_EXEC, c}            %% label_right
12:  {DONE}
```

**Execution Trace** (simplified):
```
IP=0:  SEQ_ENTER → push scope
IP=1:  PAR_FORK → spawn 2 tokens (IP=3 and IP=6)
IP=3:  TASK_EXEC a → execute task a
IP=4:  DONE → branch 1 complete
IP=6:  TASK_EXEC b → execute task b
IP=7:  DONE → branch 2 complete
IP=8:  Join point → both branches done, continue
IP=9:  SEQ_NEXT → advance to right branch
IP=11: TASK_EXEC c → execute task c
IP=12: DONE → terminal state
```

---

## 5. Supervision Tree Layout

### 5.1 Tree Structure

```
wf_substrate_sup (one_for_one)
│
├─ wf_case_sup (simple_one_for_one, dynamic)
│  ├─ case_runner_1 (gen_statem)  %% Case instance #1
│  ├─ case_runner_2 (gen_statem)  %% Case instance #2
│  └─ case_runner_N (gen_statem)  %% Case instance #N
│
├─ wf_effect_sup (simple_one_for_one, optional)
│  ├─ effect_worker_1 (gen_server)
│  └─ effect_worker_N (gen_server)
│
└─ wf_trace_sink (gen_server, optional)
```

### 5.2 Component Responsibilities

#### 5.2.1 wf_substrate_sup

**Role**: Top-level supervisor (one_for_one strategy).

**Children**:
- `wf_case_sup`: Dynamic supervisor for per-case runners
- `wf_effect_sup`: Optional supervisor for effect workers
- `wf_trace_sink`: Optional trace event sink

**Restart Strategy**: `one_for_one` (if child crashes, restart only that child)

#### 5.2.2 wf_case_sup

**Role**: Dynamic supervisor for per-case runners.

**Strategy**: `simple_one_for_one` (each case is independent child)

**Child Spec**:
```erlang
#{id => CaseId,
  start => {wf_case_runner, start_link, [CaseId, Bytecode, Ctx, Options]},
  restart => transient,  %% Don't restart if case completes normally
  shutdown => 5000,
  type => worker,
  modules => [wf_case_runner]}
```

**Lifecycle**:
- `start_case(CaseId, Bytecode, Ctx, Options)`: Start new case runner
- Case runner exits when done/cancelled/failed
- No automatic restart (transient)

#### 5.2.3 wf_case_runner

**Role**: Per-case gen_statem process managing execution.

**States**:
- `initializing`: Setting up exec_state
- `running`: Executing reductions in quanta
- `waiting_effect`: Yielded at effect boundary, waiting for result
- `waiting_signal`: Waiting for external signal (defer pattern)
- `cancelled`: Cancellation in progress
- `done`: Terminal state (success/cancelled/failed)

**Data**:
```erlang
#{case_id := case_id(),
  exec_state := exec_state(),
  step_quanta := pos_integer(),   %% Reductions per tick
  trace_level := none | min | full,
  effect_handler := module(),
  start_time := erlang:timestamp(),
  steps_executed := non_neg_integer()}
```

**Messages**:
```erlang
%% External signals
{signal, Msg}                       %% Send signal to case
{cancel, Reason}                    %% Cancel entire case
{cancel_region, ScopeId}            %% Cancel specific scope

%% Effect responses
{effect_result, EffectId, Result}   %% Effect completed

%% Internal timeouts
state_timeout                        %% Case timeout
```

**State Transitions**:
```
initializing → running (exec_state created)
running → waiting_effect (effect yield)
running → waiting_signal (defer pattern)
running → done (terminal state)
waiting_effect → running (effect result received)
waiting_signal → running (signal received)
any → cancelled (cancel received)
cancelled → done (cleanup complete)
```

**Example**:
```erlang
%% running state
running({cast, {signal, Msg}}, Data) ->
    ExecState = maps:get(exec_state, Data),
    %% Inject signal into context
    NewCtx = maps:put(signal, Msg, ExecState#exec_state.ctx),
    NewExecState = ExecState#exec_state{ctx = NewCtx},
    {next_state, running, Data#{exec_state => NewExecState},
     [{state_timeout, 0, step}]};

running(state_timeout, Data) ->
    %% Execute one quantum
    ExecState = maps:get(exec_state, Data),
    Quanta = maps:get(step_quanta, Data, 100),
    case wf_exec:run(ExecState, Quanta) of
        {done, NewExecState} ->
            {next_state, done, Data#{exec_state => NewExecState}};
        {effect, EffectSpec, Continuation, NewExecState} ->
            %% Yield to effect handler
            wf_effect:execute(EffectSpec, Continuation, self()),
            {next_state, waiting_effect, Data#{exec_state => NewExecState}};
        {yield, NewExecState} ->
            %% Continue later
            {next_state, running, Data#{exec_state => NewExecState},
             [{state_timeout, 0, step}]}
    end.
```

#### 5.2.4 wf_effect_sup (Optional)

**Role**: Supervisor for effect worker processes.

**Use Case**: Out-of-process effect execution (isolated from case runner).

**Strategy**: `simple_one_for_one`

**Children**: Effect workers (one per in-flight effect)

**Alternative**: In-process effect execution (effect_handler module callback).

#### 5.2.5 wf_trace_sink (Optional)

**Role**: Centralized trace event aggregation.

**Messages**:
```erlang
{trace_event, CaseId, TraceEvent}
```

**Storage**: ETS table, disk log, or external telemetry system.

### 5.3 Fault Tolerance

#### 5.3.1 Case Runner Crash

**Scenario**: Case runner crashes due to bug or unhandled error.

**Recovery**:
- `wf_case_sup` detects crash (simple_one_for_one)
- If case is transient, no restart (intentional termination)
- If crash is unexpected, logs error and exits
- No automatic restart (cases are one-shot)

#### 5.3.2 Effect Worker Crash

**Scenario**: Effect worker crashes during external operation.

**Recovery**:
- `wf_effect_sup` restarts effect worker
- Effect marked as failed in case runner
- Receipt records failure
- Case runner handles error (retry or abort)

#### 5.3.3 Supervisor Shutdown

**Scenario**: Entire wf_substrate application shutting down.

**Strategy**:
- `wf_substrate_sup` shuts down children in reverse order
- `wf_case_sup` stops all case runners (graceful shutdown)
- Each case runner saves state (via `wf_state:atomic_commit/1`)
- Effects cancelled cleanly

---

## 6. Effect Boundary Model

### 6.1 The Problem: Pure Substrate, Impure World

wf_substrate is designed as a **pure functional substrate**:

- Workflow transformations are pure functions on context
- No side effects in workflow logic
- Deterministic execution (replayable)

**But** workflows need to interact with the external world:

- HTTP requests
- Database queries
- File I/O
- Message publishing
- Third-party API calls

**The Dilemma**: How to perform impure operations without breaking purity?

### 6.2 Effect Boundary Pattern

**Wrong Approach** (direct side effects):
```erlang
task(send_email, #{function => fun(Ctx) ->
    %% BAD: Side effect in task!
    smtp:send(<<"user@example.com">>, <<"Hello">>),
    {ok, Ctx}
end}).
```

**Problems**:
- Task is not pure (cannot replay/test in isolation)
- Cannot cancel in-flight email
- No audit trail
- No idempotency guarantees

**Correct Approach** (effect boundary):
```erlang
task(send_email, #{function => fun(Ctx) ->
    %% GOOD: Return effect specification
    {effect, #{
        effect_id => wf_effect:generate_id(Ctx, step_seq, scope),
        effect_type => send_email,
        payload => #{
            to => <<"user@example.com">>,
            subject => <<"Hello">>,
            body => <<"...">>
        },
        idempotency_key => maps:get(email_id, Ctx),
        timeout => 30000
    }, Ctx}
end}).
```

### 6.3 EffectSpec Structure

```erlang
-effect_spec() :: #{
    effect_id := effect_id(),        %% Unique identifier
    effect_type := atom(),            %% Kind of effect (http_request, db_query, etc.)
    payload := term(),                %% Operation-specific data
    idempotency_key => term(),        %% Optional, for at-most-once semantics
    timeout => timeout()              %% Optional timeout
}.
```

**Fields**:

- **effect_id**: Unique ID derived from case_id + step_seq + scope (ensures causality)
- **effect_type**: Atom identifying effect kind (e.g., `http_request`, `db_query`, `send_email`)
- **payload**: Effect-specific data (URL, query params, email details, etc.)
- **idempotency_key** (optional): Key for deduplication (e.g., email_id, payment_id)
- **timeout** (optional): Milliseconds, default from effect_handler config

### 6.4 Receipt Structure

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

**Purpose**: Append-only log of all executed effects.

**Properties**:
- **Immutable**: Receipts never change once written
- **Append-only**: New receipts added to end of log
- **Hashed**: `effect_spec_hash` links receipt to original EffectSpec
- **Causal**: effect_id ties receipt to workflow step
- **Auditable**: Full history of external interactions

### 6.5 Effect Lifecycle

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Task Returns Effect Spec                                 │
│    task(Ctx) -> {effect, EffectSpec, ContCtx}               │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌────────────────────┴────────────────────────────────────────┐
│ 2. Executor Yields                                          │
│    wf_exec:step/2 returns {effect, EffectSpec, Continuation}│
│    Case runner enters waiting_effect state                  │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌────────────────────┴────────────────────────────────────────┐
│ 3. Effect Handler Executes                                  │
│    wf_effect:execute(EffectSpec, Continuation, CasePid)    │
│    - Executes external operation (HTTP, DB, etc.)           │
│    - May spawn worker process or call callback              │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌────────────────────┴────────────────────────────────────────┐
│ 4. Effect Result Returned                                   │
│    Case runner receives {effect_result, EffectId, Result}   │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌────────────────────┴────────────────────────────────────────┐
│ 5. Executor Resumes                                         │
│    Continuation executed with Result                         │
│    Receipt produced and stored                               │
└─────────────────────────────────────────────────────────────┘
```

### 6.6 Effect Handler Module

**Purpose**: Pluggable effect execution backend.

**Interface**:
```erlang
-callback execute(EffectSpec :: effect_spec(), EffectId :: effect_id()) ->
    {ok, Result :: term()} | {error, Reason :: term()} | timeout.

-callback cancel(EffectId :: effect_id()) ->
    ok | {error, not_found}.
```

**Built-in Handler** (wf_effect): Simple in-process execution.

**Custom Handler**: User can provide module for:
- HTTP client with retry logic
- Database connection pooling
- Message broker integration
- Third-party API clients

### 6.7 Cancellation of Effects

**Scenario**: Cancel scope triggered while effect in-flight.

**Mechanism**:
1. `wf_cancel:propagate/2` marks scope as cancelled
2. Case runner checks pending effects for cancelled scopes
3. For each in-flight effect in cancelled scope:
   - Call `wf_effect:cancel/2` (or effect_handler:cancel/1)
   - Mark effect as cancelled
   - Receipt records `cancelled` result
4. Executor resumes with cancellation

**Effect Handler Requirements**:
- Must implement `cancel/1` callback
- Must attempt to stop in-flight operation (e.g., HTTP timeout)
- Must not execute already-cancelled effects
- Should record cancellation in receipt

### 6.8 Idempotency and At-Most-Once Semantics

**Problem**: After crash and replay, effects might execute twice.

**Solution**: idempotency_key enables deduplication.

**Mechanism**:
1. Task returns `EffectSpec` with `idempotency_key`
2. Before executing effect, `wf_receipt:check/2` checks if key exists
3. If key found:
   - Return cached receipt result (don't execute effect again)
   - Skip execution, return receipt directly
4. If key not found:
   - Execute effect
   - Store receipt with idempotency_key
   - Return result

**Example** (payment processing):
```erlang
task(process_payment, #{function => fun(Ctx) ->
    PaymentId = maps:get(payment_id, Ctx),
    {effect, #{
        effect_id => wf_effect:generate_id(Ctx, step_seq, scope),
        effect_type => payment_charge,
        payload => #{
            amount => maps:get(amount, Ctx),
            currency => <<"USD">>
        },
        idempotency_key => PaymentId,  %% Prevent duplicate charges
        timeout => 30000
    }, Ctx}
end}).
```

**Replay Scenario**:
1. First run: Executes payment, stores receipt with `idempotency_key = payment_123`
2. Crash and restart
3. Replay reaches same step
4. `wf_receipt:check(payment_123)` finds existing receipt
5. Returns cached result (doesn't charge again)

### 6.9 Effect Semantics

**Effect Categories**:

1. **Idempotent**: Safe to retry (e.g., HTTP GET, read-only DB query)
   - No idempotency_key needed
   - Receipt optional (for audit)

2. **At-Most-Once**: Must execute exactly once (e.g., payment charge, email send)
   - Requires idempotency_key
   - Receipt required for deduplication

3. **Cancelable**: Can be stopped mid-execution (e.g., long-running process)
   - Effect handler must implement cancel/1
   - Receipt records cancellation

---

## 7. Type System Overview

### 7.1 Kernel Types

These types form the foundation of the workflow substrate:

```erlang
%% Context: User-provided state flowing through workflow
-type ctx() :: map().

%% Scope ID: Cancellation scope identifier
-type scope_id() :: term().

%% Case ID: Workflow instance identifier
-type case_id() :: term().

%% Receipt: Effect execution record
-type receipt() :: #receipt{}.

%% Task Function: Pure function transforming context
-type task_fun() :: fun((ctx()) ->
    {ok, ctx()} |
    {error, term()} |
    {effect, effect_spec(), ctx()}
).

%% Task Metadata: Map containing function and optional fields
-type task_metadata() :: #{
    function := task_fun(),
    timeout => timeout(),
    retry => non_neg_integer(),
    description => binary()
}.

%% Join Policies: Synchronization strategies
-type join_policy() ::
    all |                      %% Wait for all branches
    sync_merge |               %% Full synchronization with state merge
    {first_n, pos_integer()} | %% Wait for N branches
    {n_of_m, pos_integer(), pos_integer()} | %% Wait for N out of M
    first_complete.            %% Wait for first branch only

%% Loop Policies: Exit conditions
-type loop_policy() ::
    while |                    %% While condition true
    until |                    %% Until condition true
    {count, non_neg_integer()}. %% Execute N times

%% Multiple Instance Policies
-type mi_policy() ::
    {fixed, pos_integer()} |              %% Fixed N instances
    {dynamic, pos_integer(), pos_integer()}. %% Min..Max instances (ctx-dependent)

%% Cancellation Scope Specification
-type cancel_scope() ::
    scope_id() |
    {scope_id(), [cancel_option()]}.

-type cancel_option() ::
    {timeout, timeout()} |
    {notify, pid()}.
```

### 7.2 Pattern Algebra Type

The pattern algebra is a closed union type with 9 kernel constructors:

```erlang
-type wf_term() ::
    {task, atom(), task_metadata()} |       %% Execute task function
    {seq, wf_term(), wf_term()} |           %% Sequential composition
    {par, [wf_term()]} |                    %% Parallel fork (AND-split)
    {xor, [wf_term()]} |                    %% Exclusive choice (XOR-split)
    {join, join_policy(), [wf_term()]} |    %% Join with policy
    {loop, loop_policy(), wf_term()} |      %% Structured loop
    {defer, [wf_term()]} |                  %% Deferred choice (race)
    {cancel, cancel_scope(), wf_term()} |   %% Cancel scope wrapper
    {mi, mi_policy(), wf_term()}.           %% Multiple instances
```

**Derived patterns** (wf_core.erl) are macros/functions that return wf_term():
- `simple_merge/2`: XOR converging to single continuation
- `synchronizing_merge/2`: PAR with full synchronization
- `discriminator/2`: First-complete join with cancellation
- `n_out_of_m/3`: N out of M join policy

### 7.3 Bytecode Types

```erlang
%% Bytecode: Flat list of opcodes
-type wf_bc() :: [opcode()].

%% Opcode: Single instruction
-type opcode() ::
    {SEQ_ENTER, non_neg_integer()} |      %% Enter sequence scope
    {SEQ_NEXT, non_neg_integer()} |       %% Advance to next in sequence
    {PAR_FORK, [non_neg_integer()]} |     %% Fork N parallel branches
    {JOIN_WAIT, join_policy()} |          %% Block until join condition met
    {XOR_CHOOSE, [non_neg_integer()]} |   %% Choose exclusive branch
    {LOOP_BACK, non_neg_integer()} |      %% Jump back to loop head
    {LOOP_CHECK, loop_policy()} |         %% Check loop condition
    {CANCEL_SCOPE, scope_id()} |          %% Enter/exit cancel region
    {MI_SPAWN, mi_policy()} |             %% Spawn multiple instances
    {EFFECT_YIELD, term()} |              %% Yield for external effect
    {TASK_EXEC, atom()} |                 %% Execute task
    {DONE}.                               %% Terminate execution path
```

### 7.4 Execution State Types

```erlang
%% Executor State
-record(exec_state, {
    ip :: non_neg_integer(),              %% Instruction pointer
    bytecode :: wf_bc(),                  %% Compiled bytecode
    ctx :: ctx(),                         %% User context
    tokens :: #{token_id() => token()},   %% Active tokens
    branch_map :: #{branch_id() => branch_info()}, %% Parallel branches
    join_counters :: #{join_id() => join_counter()}, %% Join state
    scope_stack :: [scope_id()],          %% Cancel scope stack
    step_count :: non_neg_integer()       %% Reduction counter
}).

%% Token: Logical thread of execution
-record(token, {
    token_id :: token_id(),
    ip :: non_neg_integer(),              %% Current instruction pointer
    scope_id :: scope_id(),               %% Active cancel scope
    value :: term()                       %% Accumulated result
}).

%% Branch Info: Parallel branch tracking
-record(branch_info, {
    branch_id :: branch_id(),
    tokens :: [token_id()],               %% Active tokens in branch
    join_id :: join_id(),                 %% Associated join point
    targets :: [non_neg_integer()]        %% IP targets for branches
}).

%% Join Counter: Join synchronization state
-record(join_counter, {
    join_id :: join_id(),
    completed :: non_neg_integer(),       %% Number of completed branches
    required :: non_neg_integer(),        %% Required for join policy
    policy :: join_policy(),              %% Join policy
    results :: [term()]                   %% Accumulated results
}).
```

---

## 8. Module Responsibilities

### 8.1 Foundation Modules

#### wf_term.erl (Item 002)

**Role**: Pattern algebra AST with 9 kernel constructors.

**Exports**:
- Constructors: `task/2`, `seq/2`, `par/1`, `xor/1`, `join/2`, `loop/2`, `defer/1`, `cancel/2`, `mi/2`
- Validation: `well_formed/1`
- Smart constructors with invariant checking

**Dependencies**: None (foundation module)

**Key Functions**:
```erlang
-spec task(Name :: atom(), Metadata :: task_metadata()) -> wf_term().
-spec seq(Left :: wf_term(), Right :: wf_term()) -> wf_term().
-spec par(Branches :: [wf_term()]) -> wf_term().  %% Requires length >= 2
-spec well_formed(wf_term()) -> ok | {error, [validation_error()]}.
```

#### wf_core.erl (Item 002)

**Role**: Derived pattern library built from kernel primitives.

**Exports**:
- `simple_merge/2`: XOR choice converging to single continuation
- `synchronizing_merge/2`: PAR with full synchronization
- `discriminator/2`: First-complete join with cancellation
- `n_out_of_m/3`: N out of M join policy

**Dependencies**: wf_term

**Key Functions**:
```erlang
-spec simple_merge([wf_term()], wf_term()) -> wf_term().
-spec discriminator([wf_term()], wf_term()) -> wf_term().
-spec n_out_of_m(N :: pos_integer(), [wf_term()], wf_term()) -> wf_term().
```

### 8.2 Compilation Modules

#### wf_compile.erl (Item 004)

**Role**: Compile wf_term() to wf_bc() bytecode.

**Exports**:
- `compile/1`: Transform term to bytecode
- `resolve_labels/1`: Convert label references to integer targets

**Dependencies**: wf_term

**Key Functions**:
```erlang
-spec compile(wf_term()) -> {ok, wf_bc()} | {error, Reason}.
```

**Algorithm**:
- Single recursive pass over AST
- Emit opcode list with labels
- Resolve labels to integer addresses
- Validate no unresolved labels remain

#### wf_vm.erl (Item 004)

**Role**: Bytecode definition and opcode types.

**Exports**: Type definitions only (no functions)

**Dependencies**: None (types only)

**Types**: `wf_bc()`, `opcode()`, `join_policy()`, `loop_policy()`, `mi_policy()`

### 8.3 Execution Modules

#### wf_exec.erl (Item 005)

**Role**: Bytecode executor hot loop.

**Exports**:
- `new/1`: Create executor from bytecode
- `step/2`: Execute single reduction
- `run/3`: Execute N quanta
- `is_done/1`, `is_blocked/1`: State queries

**Dependencies**: wf_vm, wf_state, wf_sched, wf_cancel, wf_mi, wf_effect, wf_trace

**Key Functions**:
```erlang
-spec new(wf_bc()) -> exec_state().
-spec step(exec_state(), sched_decision()) -> {exec_state(), trace_event()}.
-spec run(exec_state(), pos_integer()) ->
    {done, exec_state()} |
    {effect, effect_spec(), continuation(), exec_state()} |
    {yield, exec_state()}.
```

#### wf_state.erl (Item 006)

**Role**: Per-case state store with atomic commit protocol.

**Exports**:
- `new/1`: Create new state store
- `atomic_commit/2`: Commit mutations atomically
- `get_ctx/1`, `set_ctx/2`: Context access
- `get_tokens/1`, `set_tokens/2`: Token tracking

**Dependencies**: None

**Key Functions**:
```erlang
-spec atomic_commit(State :: state(), Mutations :: [mutation()]) ->
    {ok, NewState :: state(), Receipt :: receipt()} | {error, Reason}.
```

**Protocol**: Buffer mutations → Validate → Apply → Produce receipt

#### wf_sched.erl (Item 007)

**Role**: Pluggable scheduler policies.

**Exports**:
- `select_action/2`: Choose next enabled action
- Policies: `deterministic`, `nondeterministic`, `replay`

**Dependencies**: None

**Key Functions**:
```erlang
-spec select_action(EnabledActions :: [action()], Policy :: sched_policy()) ->
    {ok, action()} | {error, no_enabled_actions}.

-type sched_policy() :: deterministic | nondeterministic | {replay, choice_log()}.
```

**Deterministic**: Stable ordering (e.g., by token_id, then IP)
**Nondeterministic**: Random selection with logging
**Replay**: Use logged choices

#### wf_cancel.erl (Item 008)

**Role**: Cancellation semantics and propagation.

**Exports**:
- `propagate/2`: Propagate cancellation to scope
- `is_cancelled/2`: Check if scope/token is cancelled
- Scopes: `activity`, `case`, `region`

**Dependencies**: wf_state

**Key Functions**:
```erlang
-spec propagate(ScopeId :: scope_id(), State :: state()) -> {ok, NewState :: state()}.
-spec is_cancelled(ScopeId :: scope_id(), State :: state()) -> boolean().
```

**Propagation**: O(scope size) - mark all tokens in scope as cancelled

#### wf_mi.erl (Item 009)

**Role**: Multiple instances support.

**Exports**:
- `spawn_instances/2`: Create N instances
- `collect_result/2`: Gather instance result
- `join/2`: Apply join policy to instance results

**Dependencies**: wf_exec

**Key Functions**:
```erlang
-spec spawn_instances(mi_policy(), wf_term()) -> {ok, [instance_id()]}.
-spec join(join_policy(), [instance_result()]) -> {ok, term()} | {error, Reason}.
```

#### wf_effect.erl (Item 010)

**Role**: Effect boundary implementation.

**Exports**:
- `execute/3`: Execute effect specification
- `cancel/2`: Cancel in-flight effect
- `generate_id/3`: Create unique effect ID

**Dependencies**: wf_receipt

**Key Functions**:
```erlang
-spec execute(EffectSpec :: effect_spec(), Continuation, CasePid :: pid()) ->
    {ok, Result :: term()} | {error, Reason}.

-spec cancel(EffectId :: effect_id()) -> ok | {error, not_found}.
```

#### wf_receipt.erl (Item 010)

**Role**: Receipt storage and retrieval.

**Exports**:
- `store/1`: Store receipt
- `lookup/1`: Retrieve receipt by ID
- `check/2`: Check idempotency key

**Dependencies**: None

**Key Functions**:
```erlang
-spec store(Receipt :: receipt()) -> ok.
-spec lookup(ReceiptId :: receipt_id()) -> {ok, Receipt :: receipt()} | {error, not_found}.
-spec check(IdempotencyKey :: term()) -> {ok, Receipt :: receipt()} | error.
```

#### wf_trace.erl (Item 011)

**Role**: Structured tracing and replay log.

**Exports**:
- `emit/2`: Emit trace event
- `get_trace/2`: Retrieve trace events
- `replay/2`: Replay from trace log

**Dependencies**: None

**Key Functions**:
```erlang
-spec emit(CaseId :: case_id(), Event :: trace_event()) -> ok.
-spec get_trace(CaseId :: case_id(), Range :: {From, To}) -> [trace_event()].
-spec replay(CaseId :: case_id(), TraceLog :: [trace_event()]) -> {ok, term()}.
```

### 8.4 Validation Modules

#### wf_validate.erl (Item 013)

**Role**: Structural validation and bounded model checking.

**Exports**:
- `structural_check/1`: Fast structural validation
- `bounded_check/2`: Bounded state exploration
- `soundness_check/1`: Prove soundness properties

**Dependencies**: wf_term

**Key Functions**:
```erlang
-spec structural_check(wf_term()) -> ok | {error, [validation_issue()]}.
-spec bounded_check(wf_term(), Depth :: pos_integer()) ->
    {ok, [property()]} | {error, Reason}.

-type property() :: {option_to_complete, boolean()} | {no_deadlock, boolean()}.
```

### 8.5 OTP Integration Modules

#### wf_substrate.erl (Item 012)

**Role**: Public API module.

**Exports**:
- `new_case/3`: Start new workflow case
- `signal/2`: Send signal to case
- `cancel/1`, `cancel_region/2`: Cancellation
- `await/2`: Wait for completion
- `status/1`: Query case status
- `trace/2`: Retrieve trace
- `validate/1`: Validate workflow term

**Dependencies**: All modules

**Key Functions**:
```erlang
-spec new_case(ProcTerm :: wf_term(), InitCtx :: ctx(), Options :: proplists:proplist()) ->
    {ok, CasePid :: pid(), CaseId :: case_id()}.

-spec await(CasePidOrId :: pid() | case_id(), Timeout :: timeout()) ->
    {ok, ResultCtx :: ctx()} | {error, Reason} | timeout.
```

#### wf_substrate_app.erl (Item 012)

**Role**: Application behavior callback.

**Dependencies**: OTP

#### wf_substrate_sup.erl (Item 012)

**Role**: Top-level supervisor.

**Dependencies**: OTP

#### wf_case_sup.erl (Item 012)

**Role**: Dynamic case supervisor.

**Dependencies**: OTP

#### wf_case_runner.erl (Item 012)

**Role**: Per-case gen_statem process.

**Dependencies**: wf_exec, wf_effect, wf_trace

**Implementation**: See section 5.2.3

---

## 9. Design Alternatives

### 9.1 Bytecode VM vs Continuation Network

**Bytecode VM** (chosen):
- Explicit exec_state with IP
- Flat opcode list
- Easy tracing and debugging
- Compile-time optimization potential

**Continuation Network** (alternative):
- Functional closure network
- O(1) closure calls
- More Erlang-ish style
- Harder to inspect and trace

**Decision**: Bytecode VM for observability and explicit state.

### 9.2 Per-Case Process vs Single Global Executor

**Per-Case Process** (chosen):
- Each case is separate gen_statem
- Isolation and fault tolerance
- OTP supervision integration
- Easier to reason about

**Single Global Executor** (alternative):
- One process executes all cases
- Lower overhead
- Harder to isolate failures
- Complex scheduling

**Decision**: Per-case processes for OTP integration and fault tolerance.

### 9.3 Effect Handling: In-Process vs Out-of-Process

**In-Process** (default):
- Effect handler is callback module
- Case runner calls effect_handler:execute/2 directly
- Lower overhead
- Simpler implementation

**Out-of-Process** (optional):
- Effect workers supervised by wf_effect_sup
- Isolated from case runner
- Better fault tolerance
- Higher overhead

**Decision**: Support both via pluggable effect_handler module.

### 9.4 State Store: ETS vs gen_server vs Process Dictionary

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

---

## 10. References

### Specification Documents

- **PROMPT.md**: `/Users/speed/wf-substrate/PROMPT.md:1-375`
  - Lines 44-66: Deliverables (modules, documentation, patterns)
  - Lines 97-136: Kernel types and primitives
  - Lines 145-156: Effect boundary pattern
  - Lines 157-177: Runtime strategy choice (bytecode VM vs continuation network)
  - Lines 179-198: OTP design requirements

### Research and Planning Documents

- **Item 002 research.md**: Pattern term algebra research
  - Lines 107-157: Detailed type definitions
  - Lines 266-287: wf_term() union type

- **Item 002 plan.md**: Implementation plan for pattern algebra
  - Lines 135-303: Type specifications with documentation

### Module Specifications

- **Item 004**: Bytecode compiler and VM definition
- **Item 005**: Executor hot loop
- **Item 006**: State store and atomic commit
- **Item 007**: Scheduler policies
- **Item 008**: Cancellation semantics
- **Item 009**: Multiple instances
- **Item 010**: Effect boundary and receipts
- **Item 011**: Tracing and replay
- **Item 012**: OTP supervision tree
- **Item 013**: Validation backend

### Companion Documents

- **docs/SEMANTICS.md**: Small-step operational semantics with inference rules
- **docs/PATTERNS.md**: Pattern mapping (future, item 017)
- **docs/TESTING.md**: Test strategy (future, item 014-016)
- **docs/OPERATIONS.md**: Operations guide (future, item 020)

### Standards and Best Practices

- **Erlang/OTP Design Principles**: Supervision trees, gen_statem, fault tolerance
- **Workflow Patterns**: van der Aalst et al., "Workflow Patterns" (43 patterns)
- **Operational Semantics**: Small-step reduction rules, inference rule notation

---

**Document Version**: 1.0
**Last Updated**: 2025-01-10
**Author**: Agent swarm (20-agent coordination)
**Status**: Implementation specification for items 004-020
