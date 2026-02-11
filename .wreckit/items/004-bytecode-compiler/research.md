# Research: Implement bytecode compiler

**Date**: 2025-01-10
**Item**: 004-bytecode-compiler

## Research Question

Implement wf_compile.erl: compile wf_term() AST into wf_bc() bytecode (flat instruction list). Opcodes: SEQ_ENTER (begin sequence scope), SEQ_NEXT (advance to next step in sequence), PAR_FORK (fork N parallel branches with label targets), JOIN_WAIT (block until join condition met — policy: all/n/first), XOR_CHOOSE (exclusive choice point with branch labels), LOOP_BACK (jump back to loop head), LOOP_CHECK (evaluate loop condition, exit or continue), CANCEL_SCOPE (enter/exit cancel region with scope ID), MI_SPAWN (spawn multiple instances per config), EFFECT_YIELD (yield control for external effect), TASK_EXEC (execute a task — the leaf operation), DONE (terminate execution path).

Output is a flat list of {opcode, operands} tuples with integer label targets for jumps/forks. The compiler performs a single recursive pass over the AST, emitting instructions and resolving labels. Include -spec for compile/1 :: wf_term() -> wf_bc(). Validate that compiled output has no unresolved labels. No per-step AST dispatch at runtime — all structural decisions are resolved at compile time.

## Summary

This task involves implementing the bytecode compiler (`wf_compile.erl`) that transforms the workflow pattern AST (`wf_term()`) into a flat list of bytecode instructions (`wf_bc()`). The compiler is the critical bridge between authoring (pattern terms) and execution (bytecode VM), resolving all structural decisions at compile time to enable O(1) instruction dispatch at runtime.

The compiler must implement a **single recursive pass** over the `wf_term()` tree, emitting opcodes for each of the 9 kernel primitives (task, seq, par, xor, join, loop, defer, cancel, mi). Key challenges include label management (generating unique labels for jump targets and resolving them to integer addresses), branch tracking (for parallel and exclusive choice), and validation (ensuring no unresolved labels remain in final bytecode).

**Key Architectural Context** (from ARCHITECTURE.md):
- This is **item 004** in the implementation sequence
- Depends on `wf_term.erl` (item 002) for AST types
- Produces `wf_bc()` bytecode consumed by `wf_exec.erl` (item 005)
- Part of the **compilation pipeline**: term → validate → compile → bytecode → execute
- Bytecode VM strategy chosen to avoid per-step "case NodeType of" dispatch (PROMPT.md:23-27)

**Critical Constraint**: The compiler must resolve ALL structural decisions at compile time. Runtime execution (wf_exec) should NOT walk the AST or dispatch on node types. Every control flow decision (branch selection, loop back, join targets) must be encoded as integer label targets in the bytecode.

## Current State Analysis

### Existing Implementation

**Current State**: No implementation exists yet. The project has only the rebar3 scaffold (item 001) and documentation (items 002-003). Items 004-020 are all in "idea" state with no code.

**Existing Files** (from item 001):
- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration ready, no external dependencies
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file with auto-discovery
- `/Users/speed/wf-substrate/src/wf_substrate_app.erl:1-43` - Application callback
- `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:1-51` - Empty supervisor
- `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder API
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:1-16` - Empty test suite

**Documentation Available**:
- `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1-1592` - Complete architecture specification including compilation pipeline (section 4), bytecode VM design rationale (section 3), and module responsibilities (section 8)
- `/Users/speed/wf-substrate/docs/SEMANTICS.md:1-1348` - Complete operational semantics with reduction rules for all kernel primitives, examples of execution traces showing IP advancement
- `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/research.md:1-485` - Pattern term algebra research with type definitions

**Planned But Unimplemented** (dependencies for this item):
- `wf_term.erl` - Pattern term algebra (item 002)
- `wf_core.erl` - Derived patterns (item 002)

**Implementation Status**: This is the **first implementation item** after the foundation (item 002) and documentation (item 003). The compiler cannot be implemented until `wf_term.erl` exists with the `wf_term()` type definition.

### Specification Context

The authoritative specification is in `/Users/speed/wf-substrate/PROMPT.md`:

**Lines 23-27**: The Critical Constraint
> "Runtime must not repeatedly interpret node types in a slow dispatch loop. Runtime must operate on a compiled executable form (bytecode + direct reducer OR compiled continuation network). Avoid per-step 'case NodeType of ...' in the hot loop."

**Lines 162-166**: Bytecode VM Strategy
```
Strategy S1: Bytecode VM
- Compile wf_term() to wf_bc() list of opcodes
- Opcodes are pattern primitives (SEQ_ENTER, PAR_FORK, JOIN_WAIT, etc.)
- wf_exec runs a tight loop executing opcodes and mutating exec_state
- No per-step AST dispatch
```

**Lines 106-125**: Pattern Algebra Definition
- Defines the 9 kernel constructors that the compiler must handle
- Task: `task(Name, Fun)` where Fun is `fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, EffectSpec, ContCtx})`
- Sequential: `seq(P, Q)`
- Parallel: `par(ListOfP)`
- Exclusive: `xor(ListOfP)`
- Join: `join(Policy, ListOfP)` where Policy includes all/first/n_of_m/sync_merge
- Loop: `loop(Policy, P)` where Policy determines exit condition
- Deferred: `defer(ListOfP)`
- Cancel: `cancel(ScopeSpec, P)`
- Multiple Instance: `mi(Policy, P)`

**From ARCHITECTURE.md:309-590**: Compilation Pipeline Details
- Stage 3 (compile) is the responsibility of `wf_compile:compile/1`
- Single recursive pass over AST
- Emit opcode list with label targets
- Resolve labels to integer addresses
- Example compilation of `seq(par([task(a), task(b)]), task(c))` shown in detail (lines 508-589)

**From SEMANTICS.md**: Opcode Semantics
Each opcode has defined small-step semantics that the compiler must produce correctly:
- `SEQ_ENTER`: Push scope, advance to next instruction (SEMANTICS.md:311-327)
- `SEQ_NEXT`: Left complete, advance to right branch (SEMANTICS.md:329-375)
- `PAR_FORK`: Spawn N tokens, one per branch (SEMANTICS.md:377-448)
- `JOIN_WAIT`: Block until join policy satisfied (SEMANTICS.md:481-591)
- `XOR_CHOOSE`: Select ONE branch, others never spawned (SEMANTICS.md:449-480)
- `LOOP_BACK`: Jump to loop head (SEMANTICS.md:663-695)
- `LOOP_CHECK`: Evaluate condition, exit or continue (SEMANTICS.md:595-662)
- `CANCEL_SCOPE`: Enter/exit cancel region (SEMANTICS.md:748-861)
- `MI_SPAWN`: Spawn N instances (SEMANTICS.md:862-968)
- `TASK_EXEC`: Execute task function or yield effect (SEMANTICS.md:238-306)
- `DONE`: Terminate execution path (SEMANTICS.md:1088-1090)

## Key Files

### Specification Files

- `/Users/speed/wf-substrate/PROMPT.md:1-375` - **Primary specification**. Key sections:
  - Lines 23-27: No per-step AST dispatch requirement
  - Lines 106-125: Pattern algebra definition (9 kernel primitives)
  - Lines 162-166: Bytecode VM strategy (wf_bc() list of opcodes)
  - Lines 97-104: Kernel types (ctx, scope_id, case_id, receipt)

- `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/item.json:1-14` - **This item's specification**. Defines:
  - Module: `wf_compile.erl`
  - Input: `wf_term()` AST
  - Output: `wf_bc()` flat instruction list
  - Opcodes: SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, LOOP_CHECK, CANCEL_SCOPE, MI_SPAWN, EFFECT_YIELD, TASK_EXEC, DONE
  - Requirement: Single recursive pass, label resolution, validation
  - Spec: `-spec compile/1 :: wf_term() -> wf_bc()`

### Documentation Files

- `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:309-590` - **Compilation pipeline specification**. Section 4 details:
  - Lines 310-341: Pipeline stages diagram (term → validate → compile → bytecode → execute)
  - Lines 392-429: Compilation algorithm sketch for seq and par
  - Lines 507-589: Concrete compilation example showing label resolution

- `/Users/speed/wf-substrate/docs/SEMANTICS.md:238-1090` - **Opcode semantics**. Defines reduction rules for:
  - Lines 238-306: Task execution (TASK_EXEC, EFFECT_YIELD)
  - Lines 307-375: Sequential composition (SEQ_ENTER, SEQ_NEXT)
  - Lines 376-448: Parallel fork (PAR_FORK)
  - Lines 449-480: Exclusive choice (XOR_CHOOSE)
  - Lines 481-591: Join synchronization (JOIN_WAIT with all/first_n/n_of_m policies)
  - Lines 592-695: Structured loop (LOOP_CHECK, LOOP_BACK)
  - Lines 697-748: Deferred evaluation (not explicitly an opcode, part of execution)
  - Lines 749-861: Cancel scope (CANCEL_SCOPE)
  - Lines 862-968: Multiple instances (MI_SPAWN)

- `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/research.md:106-287` - **Type definitions**. Defines:
  - Lines 107-157: Kernel types (ctx, scope_id, case_id, receipt, task_fun, task_metadata)
  - Lines 132-156: Join policies (all, sync_merge, first_n, n_of_m, first_complete)
  - Lines 157-266: wf_term() union type with 9 constructors

### Files to Create

- `src/wf_compile.erl` - **Bytecode compiler** (primary deliverable)
  - `compile/1`: Main entry point, compiles wf_term() to wf_bc()
  - `compile_seq/2`: Compile seq(P, Q) with SEQ_ENTER/SEQ_NEXT
  - `compile_par/1`: Compile par([P1, ..., Pn]) with PAR_FORK and labels
  - `compile_xor/1`: Compile xor([P1, ..., Pn]) with XOR_CHOOSE and labels
  - `compile_join/2`: Compile join(Policy, [P1, ..., Pn]) with JOIN_WAIT
  - `compile_loop/2`: Compile loop(Policy, P) with LOOP_CHECK/LOOP_BACK
  - `compile_defer/1`: Compile defer([P1, ..., Pn]) (may not need dedicated opcode)
  - `compile_cancel/2`: Compile cancel(Scope, P) with CANCEL_SCOPE
  - `compile_mi/2`: Compile mi(Policy, P) with MI_SPAWN
  - `compile_task/2`: Compile task(Name, Metadata) with TASK_EXEC or EFFECT_YIELD
  - `resolve_labels/1`: Convert labels to integer IP addresses
  - `validate_bytecode/1`: Check no unresolved labels remain

- `src/wf_vm.erl` - **Bytecode definition** (secondary deliverable)
  - Type definitions: `-type wf_bc() :: [opcode()].`
  - Type definitions: `-type opcode() :: {SEQ_ENTER, non_neg_integer()} | ...`
  - Type definitions for join_policy(), loop_policy(), mi_policy() (reused from wf_term)

- `test/wf_compile_tests.erl` - **Compiler tests**
  - Test compilation of each kernel primitive
  - Test label resolution
  - Test validation (unresolved labels detection)
  - Test complex nested workflows
  - Test derived patterns (simple_merge, discriminator, etc.)

### Existing Dependencies (from item 002)

**Note**: These files don't exist yet but are planned per item 002 research.md:

- `src/wf_term.erl` - Pattern term algebra (item 002)
  - Lines 107-157: Kernel type definitions needed by compiler
  - Lines 147-156: `wf_term()` union type with 9 constructors
  - Smart constructors for validation (compiler can use raw constructors)

- `src/wf_core.erl` - Derived patterns (item 002)
  - Not strictly needed by compiler (derived patterns are just wf_term() trees)
  - Compiler tests should compile derived patterns to verify they work

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Applications Needed**:
- `stdlib` - For lists, maps, proplists, sets, ordsets (label management)
- `kernel` - For basic types and processes

**Internal Module Dependencies**:
- **wf_term.erl** (item 002): Compiler MUST import `wf_term()` type and all 9 constructor tags
- **wf_core.erl** (item 002): Optional, for testing derived patterns compile correctly
- **wf_vm.erl** (this item, co-delivered): Compiler produces bytecode matching wf_vm opcode types
- **wf_validate.erl** (item 013): Future enhancement, compiler may call `wf_validate:structural_check/1` before compilation

**No Circular Dependencies**: Compiler is purely functional (wf_term → wf_bc), no process spawning or side effects.

### Bytecode Format Design

**From ARCHITECTURE.md:1097-1116** and **item.json**, the bytecode types are:

```erlang
%% Bytecode: Flat list of opcodes
-type wf_bc() :: [opcode()].

%% Opcode: Single instruction with operands
-type opcode() ::
    {SEQ_ENTER, non_neg_integer()} |      %% Begin sequence scope (operand is placeholder for future use)
    {SEQ_NEXT, non_neg_integer()} |       %% Advance to next in sequence (operand is target IP)
    {PAR_FORK, [non_neg_integer()]} |     %% Fork N parallel branches (operand is list of target IPs)
    {JOIN_WAIT, join_policy()} |          %% Block until join condition met (operand is policy)
    {XOR_CHOOSE, [non_neg_integer()]} |   %% Choose exclusive branch (operand is list of target IPs)
    {LOOP_BACK, non_neg_integer()} |      %% Jump back to loop head (operand is target IP)
    {LOOP_CHECK, loop_policy()} |         %% Check loop condition (operand is policy)
    {CANCEL_SCOPE, {enter | exit, scope_id()}} | %% Enter/exit cancel region
    {MI_SPAWN, mi_policy()} |             %% Spawn multiple instances (operand is policy)
    {EFFECT_YIELD, effect_spec()} |      %% Yield for external effect (operand is effect spec)
    {TASK_EXEC, atom()} |                 %% Execute task (operand is task name)
    {DONE}.                               %% Terminate execution path (no operands)

%% Join policies (from wf_term.erl)
-type join_policy() ::
    all |                      %% Wait for all branches
    sync_merge |               %% Full synchronization with state merge
    {first_n, pos_integer()} | %% Wait for N branches
    {n_of_m, pos_integer(), pos_integer()} | %% Wait for N out of M
    first_complete.            %% Wait for first branch only

%% Loop policies (from wf_term.erl)
-type loop_policy() ::
    while |                    %% While condition true
    until |                    %% Until condition true
    {count, non_neg_integer()}. %% Execute N times

%% Multiple instance policies (from wf_term.erl)
-type mi_policy() ::
    {fixed, pos_integer()} |              %% Fixed N instances
    {dynamic, pos_integer(), pos_integer()}. %% Min..Max instances (ctx-dependent)
```

**Key Design Decision**: Opcodes are **tagged tuples**, not records. Rationale:
- Immutable equality (pattern matching works)
- Lightweight (no record overhead)
- Easy to emit in lists
- Consistent with wf_term() representation

### Label Management Strategy

**Problem**: Compiler must emit jump targets before knowing their IP addresses.

**Example** (from ARCHITECTURE.md:540-576):
```erlang
%% Compiling seq(par([task(a), task(b)]), task(c))

%% First pass: emit with symbolic labels
[
    {SEQ_ENTER, 0},
    {PAR_FORK, [label_branch1, label_branch2]},  %% Labels not resolved yet
    {label, label_branch1},                        %% Label marker
    {TASK_EXEC, a},
    {DONE},
    {label, label_branch2},
    {TASK_EXEC, b},
    {DONE},
    {SEQ_NEXT, label_right},                       %% Another label
    {label, label_right},
    {TASK_EXEC, c},
    {DONE}
]
```

**Solution**: Two-pass algorithm (but single recursive AST pass):

1. **Compilation Pass**: Recursive walk of wf_term(), emitting opcodes with labels
   - Generate unique labels using `erlang:make_ref()` or counter
   - Emit `{label, Label}` markers at target positions
   - Emit label references in operands (e.g., `{PAR_FORK, [Label1, Label2]}`)

2. **Resolution Pass**: Single linear scan of opcode list
   - Build map: `#{Label => IP}` for each `{label, Label}` instruction
   - Replace label references with integer IPs
   - Remove `{label, Label}` markers from final bytecode

**Complexity**:
- Compilation: O(N) where N = number of AST nodes (single recursive pass)
- Resolution: O(M) where M = number of opcodes (single linear scan)
- Total: O(N + M) which is O(N) since M ≤ c×N for some constant c

**Alternative**: Single-pass with backpatching (more complex, not recommended for first implementation).

### Compilation Algorithm for Each Primitive

**From ARCHITECTURE.md:403-429** and SEMANTICS.md reduction rules:

#### Task (TASK_EXEC)

**Simple Case** (pure task, no effect):
```erlang
compile_task({task, Name, _Metadata}) ->
    [{TASK_EXEC, Name}].
```

**Effect-Yielding Case**: Compiler cannot detect if task yields effects at compile time (depends on runtime ctx()). The compiler always emits `TASK_EXEC`. The executor (wf_exec) will handle effect yielding at runtime (SEMANTICS.md:265-287).

**Validation**: Ensure Name is an atom.

#### Sequence (seq)

**Algorithm**:
```erlang
compile_seq({seq, Left, Right}) ->
    LeftCode = compile(Left),
    RightLabel = make_label(),
    RightCode = compile(Right),
    [
        {SEQ_ENTER, 0},
        LeftCode,
        {SEQ_NEXT, RightLabel},
        {label, RightLabel},
        RightCode
    ].
```

**Semantics** (from SEMANTICS.md:307-375):
- SEQ_ENTER: Push sequence scope onto scope stack
- Execute Left bytecode
- SEQ_NEXT: When Left completes, jump to RightLabel
- Execute Right bytecode
- Implicit DONE when Right completes

**Label Usage**: RightLabel allows SEQ_NEXT to jump over Left code to Right code.

#### Parallel (par)

**Algorithm**:
```erlang
compile_par({par, Branches}) when length(Branches) >= 2 ->
    BranchCodes = [compile(B) || B <- Branches],
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),
    [
        {PAR_FORK, BranchLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, BranchCodes, BranchLabels),
        {label, JoinLabel},
        {JOIN_WAIT, all}  %% or specific join policy
    ].
```

**Semantics** (from SEMANTICS.md:376-448):
- PAR_FORK: Spawn N tokens, one per branch
- Each branch starts at its label
- Each branch ends with DONE (token marks itself complete)
- All branches converge at JoinLabel
- JOIN_WAIT: Block until all branches complete (or policy satisfied)

**Label Usage**: BranchLabels for each branch start, JoinLabel for convergence point.

**Critical**: Each branch MUST end with DONE to signal completion. Join counter increments on each DONE.

#### Exclusive Choice (xor)

**Algorithm**:
```erlang
compile_xor({xor, Alternatives}) when length(Alternatives) >= 2 ->
    AltCodes = [compile(A) || A <- Alternatives],
    AltLabels = [make_label() || _ <- Alternatives],
    [
        {XOR_CHOOSE, AltLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, AltCodes, AltLabels)
    ].
```

**Semantics** (from SEMANTICS.md:449-480):
- XOR_CHOOSE: Scheduler selects ONE branch at runtime
- Only selected branch executes
- Unselected branches are NEVER spawned (not cancelled, just never created)
- No join needed (only one branch runs)

**Label Usage**: AltLabels for each alternative start.

**Difference from par**: par has JOIN_WAIT, xor does not. par spawns all branches, xor spawns only one.

#### Join (join)

**Algorithm**:
```erlang
compile_join({join, Policy, Branches}) when length(Branches) >= 2 ->
    BranchCodes = [compile(B) || B <- Branches],
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),
    [
        {PAR_FORK, BranchLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, BranchCodes, BranchLabels),
        {label, JoinLabel},
        {JOIN_WAIT, Policy}
    ].
```

**Semantics** (from SEMANTICS.md:481-591):
- Compile same as par (spawn all branches)
- JOIN_WAIT with different policies:
  - `all`: Wait for all N branches
  - `{first_n, M}`: Wait for M branches, cancel remainder
  - `{n_of_m, N, M}`: Wait for N out of M, cancel remainder
  - `first_complete`: Wait for first branch, cancel all others
  - `sync_merge`: Wait for all, merge their state changes

**Label Usage**: Same as par (BranchLabels, JoinLabel).

**Difference from par**: join is explicit about policy. par is implicit `join(all, ...)`.

#### Loop (loop)

**Algorithm for count policy**:
```erlang
compile_loop({loop, {count, N}, Body}) ->
    BodyCode = compile(Body),
    LoopHeadLabel = make_label(),
    ExitLabel = make_label(),
    [
        {label, LoopHeadLabel},
        {LOOP_CHECK, {count, N}},
        BodyCode,
        {LOOP_BACK, LoopHeadLabel},
        {label, ExitLabel}
    ].
```

**Algorithm for while policy**:
```erlang
compile_loop({loop, while, Body}) ->
    BodyCode = compile(Body),
    LoopHeadLabel = make_label(),
    ExitLabel = make_label(),
    %% While: check condition FIRST, then execute body if true
    [
        {label, LoopHeadLabel},
        {LOOP_CHECK, while},
        BodyCode,
        {LOOP_BACK, LoopHeadLabel},
        {label, ExitLabel}
    ].
```

**Algorithm for until policy**:
```erlang
compile_loop({loop, until, Body}) ->
    BodyCode = compile(Body),
    LoopHeadLabel = make_label(),
    ExitLabel = make_label(),
    %% Until: execute body FIRST, then check condition
    [
        {label, LoopHeadLabel},
        BodyCode,
        {LOOP_CHECK, until},
        {LOOP_BACK, LoopHeadLabel},
        {label, ExitLabel}
    ].
```

**Semantics** (from SEMANTICS.md:592-695):
- **count**: LOOP_CHECK decrements counter, exit if zero, execute body if > 0
- **while**: LOOP_CHECK evaluates condition, exit if false, execute body if true
- **until**: Execute body unconditionally, LOOP_CHECK checks condition, exit if true, repeat if false
- **LOOP_BACK**: Jump back to LoopHeadLabel
- No DONE needed (loop terminates via LOOP_CHECK)

**Label Usage**: LoopHeadLabel for loop body start, ExitLabel for loop exit.

**Critical**: LOOP_BACK must jump to LoopHeadLabel, not to beginning of bytecode (supports nested loops).

#### Cancel (cancel)

**Algorithm**:
```erlang
compile_cancel({cancel, ScopeId, Body}) ->
    BodyCode = compile(Body),
    [
        {CANCEL_SCOPE, {enter, ScopeId}},
        BodyCode,
        {CANCEL_SCOPE, {exit, ScopeId}}
    ].
```

**Semantics** (from SEMANTICS.md:749-861):
- CANCEL_SCOPE enter: Push scope onto scope stack, all new tokens inherit this scope_id
- CANCEL_SCOPE exit: Pop scope from stack, tokens revert to parent scope
- If cancel signal received during execution, all tokens in scope are marked cancelled
- Cancellation propagates to nested scopes recursively

**Label Usage**: None needed (enter/exit are linear, no jumps).

**Validation**: Ensure ScopeId is a valid term (atom, integer, ref, etc.).

#### Multiple Instances (mi)

**Algorithm**:
```erlang
compile_mi({mi, Policy, Body}) ->
    BodyCode = compile(Body),
    InstanceLabel = make_label(),
    JoinLabel = make_label(),
    [
        {MI_SPAWN, Policy},
        {label, InstanceLabel},
        BodyCode,
        {DONE},
        {label, JoinLabel},
        {JOIN_WAIT, all}  %% Join policy could be parameterized
    ].
```

**Semantics** (from SEMANTICS.md:862-968):
- MI_SPAWN: Spawn N instances (fixed count or dynamic from ctx())
- Each instance executes Body independently
- Each instance terminates with DONE
- Join collects all instance results (or first N, or first complete)
- Join policy is same as join (all, first_n, n_of_m, first_complete)

**Label Usage**: InstanceLabel for body start, JoinLabel for collection point.

**Open Question**: Should mi spawn instances sequentially in bytecode or is MI_SPAWN sufficient? Research assumes MI_SPAWN opcode handles spawning all instances, and Body is template for each instance.

#### Deferred Choice (defer)

**Algorithm**:
```erlang
compile_defer({defer, Alternatives}) when length(Alternatives) >= 2 ->
    AltCodes = [compile(A) || A <- Alternatives],
    AltLabels = [make_label() || _ <- Alternatives],
    [
        {DEFER_ENTER, AltLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, AltCodes, AltLabels),
        {DEFER_WAIT}
    ].
```

**Issue**: Item.json opcode list does NOT include DEFER_ENTER or DEFER_WAIT.

**Resolution**: Deferred choice may be implemented at runtime without dedicated opcodes:
- Compile as par with special handling in executor
- Or compile as xor with external event selection
- Research assumes defer will be handled by xor or par with runtime semantics

**Recommendation**: Omit defer compilation in first implementation, or compile to xor with comment that runtime will handle external event selection.

### Label Resolution Algorithm

**Two-Pass Approach**:

**Pass 1: Compilation** (recursive AST walk)
```erlang
compile(Term) ->
    {BytecodeWithLabels, _LabelCount} = compile_pass(Term, 0),
    ResolvedBytecode = resolve_labels(BytecodeWithLabels),
    case validate_bytecode(ResolvedBytecode) of
        ok -> {ok, ResolvedBytecode};
        {error, Reasons} -> {error, {validation_failed, Reasons}}
    end.

compile_pass({seq, Left, Right}, LabelCount) ->
    {LeftCode, LabelCount1} = compile_pass(Left, LabelCount),
    RightLabel = {label, LabelCount1},
    {RightCode, LabelCount2} = compile_pass(Right, LabelCount1 + 1),
    Bytecode = [
        {SEQ_ENTER, 0},
        LeftCode,
        {SEQ_NEXT, RightLabel},
        RightLabel,
        RightCode
    ],
    {Bytecode, LabelCount2};
%% ... similar for other primitives
```

**Pass 2: Resolution** (linear scan)
```erlang
resolve_labels(BytecodeWithLabels) ->
    %% Build label map: #{Label => IP}
    LabelMap = build_label_map(BytecodeWithLabels, 0, #{}),
    %% Replace label references with IPs
    Resolved = replace_labels(BytecodeWithLabels, LabelMap, 0, []),
    %% Remove {label, Label} markers
    lists:filter(fun({label, _}) -> false; (_) -> true end, Resolved).

build_label_map([], _IP, Map) -> Map;
build_label_map([{label, Label} | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map#{Label => IP});
build_label_map([_Opcode | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map).

replace_labels([], _LabelMap, _IP, Acc) -> lists:reverse(Acc);
replace_labels([{label, _} | Rest], LabelMap, IP, Acc) ->
    %% Skip label markers (they'll be filtered out)
    replace_labels(Rest, LabelMap, IP + 1, Acc);
replace_labels([Opcode | Rest], LabelMap, IP, Acc) ->
    ResolvedOpcode = resolve_opcode_labels(Opcode, LabelMap),
    replace_labels(Rest, LabelMap, IP + 1, [ResolvedOpcode | Acc]).

resolve_opcode_labels({SEQ_NEXT, Label}, LabelMap) when is_reference(Label); is_atom(Label) ->
    {SEQ_NEXT, maps:get(Label, LabelMap)};
resolve_opcode_labels({PAR_FORK, Labels}, LabelMap) when is_list(Labels) ->
    {PAR_FORK, [maps:get(L, LabelMap) || L <- Labels]};
resolve_opcode_labels({XOR_CHOOSE, Labels}, LabelMap) when is_list(Labels) ->
    {XOR_CHOOSE, [maps:get(L, LabelMap) || L <- Labels]};
resolve_opcode_labels({LOOP_BACK, Label}, LabelMap) when is_reference(Label); is_atom(Label) ->
    {LOOP_BACK, maps:get(Label, LabelMap)};
%% ... similar for other opcodes with label operands
resolve_opcode_labels(Opcode, _LabelMap) ->
    Opcode.
```

**Validation**:
```erlang
validate_bytecode(Bytecode) ->
    case [L || Op <- Bytecode, has_unresolved_label(Op), L <- [extract_label(Op)]] of
        [] -> ok;
        Unresolved -> {error, {unresolved_labels, Unresolved}}
    end.

has_unresolved_label({_, Label}) when is_reference(Label); is_atom(Label) -> true;
has_unresolved_label({_, Labels}) when is_list(Labels) ->
    lists:any(fun(L) when is_reference(L); is_atom(L) -> true; (_) -> false end, Labels);
has_unresolved_label(_) -> false.
```

### Error Handling

**Compilation Errors**:
- Invalid term structure (e.g., par with < 2 branches)
- Invalid join policy
- Invalid loop policy
- Invalid scope_id type

**Strategy**: Compiler should **validate** during compilation and throw `badarg` with descriptive message:
```erlang
compile_par({par, Branches}) when length(Branches) < 2 ->
    error({badarg, {invalid_branch_count, par, length(Branches), 2}});
compile_par({par, Branches}) ->
    %% Normal compilation
    ...
```

**Alternative**: Return `{error, Reason}` instead of throwing. Research recommends throwing for contract violations (Erlang convention of "let it crash") but returning for validation errors.

**Recommendation**: Use both:
- Throw `badarg` for contract violations (e.g., par with 0 branches)
- Return `{error, Reason}` for structural issues (e.g., unresolved labels after compilation)

### Patterns to Follow

**From existing codebase** (item 001):
- Use `-spec` for all exported functions
- Use `-type` and `-opaque` for type definitions
- Use `-callback` for behaviours (not applicable here)
- Follow naming conventions: `module:function/arity`

**From Erlang/OTP conventions**:
- Use tagged tuples for data structures (already specified)
- Use pattern matching in function heads for validation
- Use guards for type checks (`is_atom`, `is_list`, `is_reference`)
- Use `error/1` or `error/2` for fatal errors
- Use lists:foldl/foldr for accumulations
- Use maps for key-value stores (label map)

**From ARCHITECTURE.md documentation**:
- Compile all 9 kernel primitives (item 004 responsibility)
- Single recursive pass over AST (performance requirement)
- Emit flat opcode list (output format)
- Resolve labels to integers (validation requirement)
- No per-step AST dispatch at runtime (architectural constraint)

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Dependency on wf_term.erl** | High | Compiler cannot be implemented until item 002 (wf_term) is complete. Mitigation: Implement wf_term first (item 002), ensure wf_term() type matches specification exactly. Create shared header file (.hrl) if types are shared across modules. |
| **Label Collision** | Medium | Using `make_ref()` for labels guarantees uniqueness (no collision). Using counter requires careful increment management. Mitigation: Use `erlang:make_ref()` for labels (simpler, safer). Counter approach is faster but riskier. |
| **Infinite Recursion on Malformed Terms** | Medium | Compiler assumes wf_term() is well-formed (e.g., no cycles). Malformed terms could cause infinite recursion. Mitigation: Add depth limit or visited set to detect cycles. Let wf_validate:structural_check/1 (item 013) catch this before compilation. |
| **Unresolved Labels After Resolution** | High | Resolution algorithm assumes all labels are defined. Undefined labels cause `maps:get` crash. Mitigation: Validate after resolution, check for unresolved labels, return `{error, {unresolved_labels, Labels}}` instead of crashing. |
| **Incorrect Label References** | High | Compiler might emit label reference that doesn't exist (e.g., typo in label name). Mitigation: Use make_ref() for labels (cannot typo), or compile with atom labels and validate all atoms in map. |
| **Bytecode Size Explosion** | Medium | Complex workflows could generate very long bytecode (linear in AST size). Mitigation: This is expected and acceptable (bytecode is still flat O(1) dispatch). Future optimization: peephole optimizer to merge redundant opcodes. |
| **Nested Scope Complexity** | Medium | Deeply nested par/seq/loop structures are hard to compile correctly. Mitigation: Test thoroughly with nested patterns. Use unit tests for each nesting level (1, 2, 3 deep). |
| **Join Policy Validation** | Medium | Compiler must validate join policies are correct (e.g., {n_of_m, N, M} where N ≤ M). Mitigation: Add validation function `validate_join_policy/1`, call during compilation. |
| **Loop Condition Evaluation** | Low | LOOP_CHECK opcode requires runtime evaluation of condition. Compiler cannot know if loop will terminate. Mitigation: This is executor's responsibility (wf_exec). Compiler only emits opcodes, doesn't check for infinite loops. |
| **Effect Yield Detection** | Low | Compiler cannot detect if TASK_EXEC will yield effect at runtime. Mitigation: This is runtime decision. Compiler always emits TASK_EXEC. Executor handles effect yielding (SEMANTICS.md:265-287). |
| **Defer Implementation Uncertainty** | Medium | item.json doesn't list DEFER_* opcodes, but defer is a kernel primitive. Mitigation: Omit defer compilation in v1, or compile to xor with runtime semantics. Clarify in implementation plan. |
| **Test Coverage Gaps** | High | Without comprehensive tests, bugs in label resolution or opcode emission could go undetected. Mitigation: Write EUnit tests for all 9 primitives, nested patterns, edge cases (empty seq, single branch par, etc.). Property-based tests for label resolution. |

## Recommended Approach

**High-Level Strategy**: Implement compiler incrementally, starting with simple opcodes (task, seq) and progressing to complex ones (par, xor, loop). Use a two-pass compilation algorithm (emit labels → resolve labels). Validate thoroughly with unit tests before moving to next primitive.

### Phase 1: Foundation (wf_vm.erl and types)

**1. Create wf_vm.erl** (bytecode definition module):
   - Define `-type wf_bc() :: [opcode()].`
   - Define `-type opcode() ::` union type with all 12 opcodes
   - Define `-type join_policy() ::` (reuse from wf_term or redefine)
   - Define `-type loop_policy() ::` (reuse from wf_term or redefine)
   - Define `-type mi_policy() ::` (reuse from wf_term or redefine)
   - Export all types (for use by wf_compile and wf_exec)
   - Add module docs explaining bytecode format

**2. Create shared types header** (optional):
   - If types are shared between wf_term.erl and wf_vm.erl, create `include/wf_types.hrl`
   - Include in both modules: `-include_lib("wf_types.hrl").`
   - Avoid duplication and ensure consistency

**3. Add -spec declarations**:
   - Every function in wf_compile gets full -spec
   - Use types defined in wf_vm.erl
   - Enable Dialyzer validation

### Phase 2: Label Management Infrastructure

**1. Implement label generation**:
```erlang
%% Generate unique label using ref (safest)
make_label() -> {label, erlang:make_ref()}.

%% Or using counter (faster but requires state)
%% Not recommended for first implementation
```

**2. Implement label resolution**:
```erlang
-spec resolve_labels([unresolved_opcode()]) -> {ok, wf_bc()} | {error, Reason}.
resolve_labels(BytecodeWithLabels) ->
    LabelMap = build_label_map(BytecodeWithLabels),
    Resolved = replace_labels(BytecodeWithLabels, LabelMap),
    Clean = lists:filter(fun({label, _}) -> false; (_) -> true end, Resolved),
    {ok, Clean}.
```

**3. Implement validation**:
```erlang
-spec validate_bytecode(wf_bc()) -> ok | {error, [unresolved_label()]}.
validate_bytecode(Bytecode) ->
    Unresolved = [L || Op <- Bytecode, is_label(Op), L <- [extract_label(Op)]],
    case Unresolved of
        [] -> ok;
        _ -> {error, {unresolved_labels, Unresolved}}
    end.
```

**4. Test label resolution**:
```erlang
label_resolution_test() ->
    Bytecode = [
        {PAR_FORK, [{label, ref1}, {label, ref2}]},
        {label, ref1},
        {TASK_EXEC, a},
        {DONE},
        {label, ref2},
        {TASK_EXEC, b},
        {DONE}
    ],
    {ok, Resolved} = resolve_labels(Bytecode),
    ?assertEqual(0, count_labels(Resolved)).
```

### Phase 3: Simple Primitives (task, seq)

**1. Implement compile/1** (entry point):
```erlang
-spec compile(wf_term()) -> {ok, wf_bc()} | {error, Reason}.
compile(Term) ->
    try
        BytecodeWithLabels = compile_term(Term),
        Resolved = resolve_labels(BytecodeWithLabels),
        case validate_bytecode(Resolved) of
            ok -> {ok, Resolved};
            {error, Reason} -> {error, Reason}
        end
    catch
        throw:Reason -> {error, Reason};
        error:Reason -> {error, Reason}
    end.
```

**2. Implement compile_task/2**:
```erlang
compile_task({task, Name, _Metadata}) when is_atom(Name) ->
    [{TASK_EXEC, Name}].
```

**3. Implement compile_seq/2**:
```erlang
compile_seq({seq, Left, Right}) ->
    LeftCode = compile_term(Left),
    RightLabel = make_label(),
    RightCode = compile_term(Right),
    [
        {SEQ_ENTER, 0},
        LeftCode,
        {SEQ_NEXT, RightLabel},
        RightLabel,
        RightCode
    ].
```

**4. Test simple primitives**:
```erlang
compile_task_test() ->
    Term = wf_term:task(my_task, #{function => fun my_task/1}),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{TASK_EXEC, my_task}], Bytecode).

compile_seq_test() ->
    Term = wf_term:seq(
        wf_term:task(a, #{function => fun a/1}),
        wf_term:task(b, #{function => fun b/1})
    ),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{SEQ_ENTER, 0}, {TASK_EXEC, a}, {SEQ_NEXT, _}, {TASK_EXEC, b}], Bytecode).
```

### Phase 4: Parallel and Exclusive Choice (par, xor)

**1. Implement compile_par/1**:
```erlang
compile_par({par, Branches}) when length(Branches) >= 2 ->
    BranchCodes = [compile_term(B) || B <- Branches],
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

**2. Implement compile_xor/1**:
```erlang
compile_xor({xor, Alternatives}) when length(Alternatives) >= 2 ->
    AltCodes = [compile_term(A) || A <- Alternatives],
    AltLabels = [make_label() || _ <- Alternatives],
    [
        {XOR_CHOOSE, AltLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, AltCodes, AltLabels)
    ].
```

**3. Test parallel primitives**:
```erlang
compile_par_test() ->
    Term = wf_term:par([
        wf_term:task(a, #{function => fun a/1}),
        wf_term:task(b, #{function => fun b/1})
    ]),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{PAR_FORK, [_]}, _, _, _, _, {JOIN_WAIT, all}], Bytecode),
    ?assertEqual(0, count_labels(Bytecode)).

compile_xor_test() ->
    Term = wf_term:xor([
        wf_term:task(a, #{function => fun a/1}),
        wf_term:task(b, #{function => fun b/1})
    ]),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{XOR_CHOOSE, [_]}, _, _, _, _], Bytecode),
    ?assertEqual(0, count_labels(Bytecode)).
```

### Phase 5: Loop and Join (loop, join)

**1. Implement compile_loop/2**:
```erlang
compile_loop({loop, Policy, Body}) ->
    BodyCode = compile_term(Body),
    LoopHeadLabel = make_label(),
    ExitLabel = make_label(),
    [
        {label, LoopHeadLabel},
        {LOOP_CHECK, Policy},
        BodyCode,
        {LOOP_BACK, LoopHeadLabel},
        {label, ExitLabel}
    ].
```

**2. Implement compile_join/2**:
```erlang
compile_join({join, Policy, Branches}) when length(Branches) >= 2 ->
    %% Same as par but with explicit policy
    BranchCodes = [compile_term(B) || B <- Branches],
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),
    [
        {PAR_FORK, BranchLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, BranchCodes, BranchLabels),
        {label, JoinLabel},
        {JOIN_WAIT, Policy}
    ].
```

**3. Test loop and join**:
```erlang
compile_loop_test() ->
    Term = wf_term:loop({count, 3},
        wf_term:task(a, #{function => fun a/1})
    ),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{label, _}, {LOOP_CHECK, {count, 3}}, _, {LOOP_BACK, _}, _], Bytecode).

compile_join_test() ->
    Term = wf_term:join({first_n, 1}, [
        wf_term:task(a, #{function => fun a/1}),
        wf_term:task(b, #{function => fun b/1})
    ]),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([_, {JOIN_WAIT, {first_n, 1}}], lists:last(Bytecode)).
```

### Phase 6: Cancel and Multiple Instances (cancel, mi)

**1. Implement compile_cancel/2**:
```erlang
compile_cancel({cancel, ScopeId, Body}) ->
    BodyCode = compile_term(Body),
    [
        {CANCEL_SCOPE, {enter, ScopeId}},
        BodyCode,
        {CANCEL_SCOPE, {exit, ScopeId}}
    ].
```

**2. Implement compile_mi/2**:
```erlang
compile_mi({mi, Policy, Body}) ->
    BodyCode = compile_term(Body),
    [
        {MI_SPAWN, Policy},
        BodyCode,
        {DONE},
        {JOIN_WAIT, all}  %% Or specific policy
    ].
```

**3. Test cancel and mi**:
```erlang
compile_cancel_test() ->
    Term = wf_term:cancel(my_scope,
        wf_term:task(a, #{function => fun a/1})
    ),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{CANCEL_SCOPE, {enter, my_scope}}, {TASK_EXEC, a}, {CANCEL_SCOPE, {exit, my_scope}}], Bytecode).

compile_mi_test() ->
    Term = wf_term:mi({fixed, 3},
        wf_term:task(a, #{function => fun a/1})
    ),
    {ok, Bytecode} = wf_compile:compile(Term),
    ?assertMatch([{MI_SPAWN, {fixed, 3}}, _, {DONE}, {JOIN_WAIT, all}], Bytecode).
```

### Phase 7: Defer (defer)

**Decision Point**: item.json doesn't list DEFER_* opcodes. Options:
1. Omit defer compilation (treat as not implemented)
2. Compile to xor (runtime handles external events)
3. Add DEFER_ENTER/DEFER_WAIT opcodes (deviates from spec)

**Recommendation**: Omit defer in v1, add comment that it's future work. Or compile to xor with documentation note.

### Phase 8: Integration and Testing

**1. Comprehensive testing**:
   - Test each primitive in isolation
   - Test nested combinations (seq(par), par(seq), loop(seq)), etc.
   - Test label resolution (deeply nested labels)
   - Test validation (malformed bytecode detection)
   - Test derived patterns (simple_merge, discriminator, n_out_of_m)

**2. Property-based tests**:
   - Generate random wf_term() terms within bounds
   - Compile and validate no unresolved labels
   - Assert bytecode is flat list (no nested lists)
   - Assert all opcodes are valid

**3. Compilation verification**:
   - Manually inspect bytecode for simple workflows
   - Compare with ARCHITECTURE.md:507-589 example
   - Verify IP addresses are correct

**4. Documentation**:
   - Add module docs explaining compilation algorithm
   - Add function docs with examples
   - Document label resolution strategy
   - Note limitations (defer, error recovery)

### Phase 9: Preparation for Next Items

**1. Ensure wf_exec can consume bytecode** (item 005):
   - Verify bytecode format matches wf_exec expectations
   - Test with mock executor (simple IP advancement)
   - Document executor interface

**2. Ensure wf_validate can check terms** (item 013):
   - Compiler may call wf_validate:structural_check/1 before compilation
   - Or leave validation to caller (separation of concerns)

**3. Performance profiling** (optional):
   - Benchmark compilation of large workflows (100+ tasks)
   - Profile label resolution performance
   - Check for bottlenecks (list concatenation, map operations)

**Rationale**: This phased approach isolates simple primitives first (task, seq), then complex ones (par, xor, loop). Testing at each phase catches bugs early. Label resolution infrastructure is built before complex primitives that need it most.

## Open Questions

1. **wf_term.erl Availability**: Item 002 (wf_term) must be implemented before this item can start. Has item 002 been completed? If not, should we implement wf_term types as part of this item?
   - **Recommendation**: Item 002 must be completed first. The compiler depends on `wf_term()` type definition. If wf_term doesn't exist, create placeholder types in wf_compile to unblock development, then switch to real wf_term when available.

2. **Label Generation Strategy**: Should we use `erlang:make_ref()` for labels (safe, slower) or integer counter (faster, riskier)?
   - **Recommendation**: Use `erlang:make_ref()` for v1. It's simpler and guarantees uniqueness. Counter approach requires state management (label counter accumulator passed through all compile functions). Optimize to counter if profiling shows ref creation is bottleneck.

3. **Error Handling Style**: Should compiler throw `badarg` for invalid terms or return `{error, Reason}`?
   - **Recommendation**: Hybrid approach. Throw for contract violations (par with 0 branches, invalid atoms). Return `{error, Reason}` for structural issues (unresolved labels after resolution, depth exceeded). This follows Erlang "let it crash" philosophy while providing structured errors for recoverable issues.

4. **Defer Opcode**: item.json doesn't list DEFER_ENTER or DEFER_WAIT opcodes, but defer is a kernel primitive. How should we compile defer?
   - **Recommendation**: Compile defer to xor (XOR_CHOOSE) with documentation that runtime will handle external event selection. Defer semantics (race on external events) are similar to xor (exclusive choice), just that choice is made by external signal, not scheduler. Omit dedicated defer opcodes in v1.

5. **MI_SPAWN Semantics**: Does MI_SPAWN opcode spawn all instances immediately, or does it emit a template that executor replicates?
   - **Recommendation**: MI_SPAWN spawns all instances immediately (operand is policy). Executor creates N execution contexts from the same body bytecode. Each instance executes independently with its own token. All instances converge at join point. This matches SEMANTICS.md:862-968.

6. **EFFECT_YIELD in Compiler**: Should compiler detect if task yields effect and emit EFFECT_YIELD instead of TASK_EXEC?
   - **Recommendation**: No. Compiler cannot determine at compile time if task will yield effect (depends on runtime ctx()). Always emit TASK_EXEC. Executor (wf_exec) will detect effect yield at runtime (task returns `{effect, Spec, ContCtx}`) and handle appropriately (SEMANTICS.md:265-287). EFFECT_YIELD opcode might not be needed at all (executor handles effect yield implicitly).

7. **CANCEL_SCOPE Operand Format**: Should CANCEL_SCOPE be `{CANCEL_SCOPE, {enter, ScopeId}}` and `{CANCEL_SCOPE, {exit, ScopeId}}` (two opcodes) or `{CANCEL_SCOPE, ScopeId}` with implicit enter/exit from position?
   - **Recommendation**: Use `{CANCEL_SCOPE, {enter, ScopeId}}` and `{CANCEL_SCOPE, {exit, ScopeId}}` format (two opcodes). This makes enter/exit explicit in bytecode, matching SEMANTICS.md:749-861 which shows CANCEL_ENTER and CANCEL_EXIT as separate rules. Alternative is single opcode with direction flag, but two-opcode approach is clearer.

8. **DONE Operand**: Should DONE have an operand (e.g., result value) or is it just a marker?
   - **Recommendation**: DONE has no operands (`{DONE}` with no second element). It's a marker that terminates execution path for a token. Executor removes token from active set when DONE is reached. Result values are accumulated in token state, not DONE operand.

9. **Label Counter State**: If we use counter-based labels (not refs), how do we pass counter through recursive compilation?
   - **Recommendation**: Use `compile_term/2` with accumulator: `compile_term(Term, LabelCount) -> {Bytecode, NewLabelCount}`. Every compile function takes current counter and returns incremented counter. This is pure functional approach (no state). Alternative is process dictionary (not recommended).

10. **Bytecode Verification**: Should compiler verify bytecode properties (e.g., all branches terminate with DONE, labels are reachable) or just check for unresolved labels?
    - **Recommendation**: Check only for unresolved labels in v1 (minimal validation). Advanced verification (termination, reachability) is better suited for wf_validate (item 013) which can do bounded model checking. Compiler's job is to compile, not verify soundness.

11. **Performance Optimization**: Should compiler do peephole optimization (e.g., remove redundant SEQ_ENTER/SEQ_NEXT, merge consecutive TASK_EXEC)?
    - **Recommendation**: No optimization in v1. Emit straightforward bytecode. Optimization can be added later as separate pass (wf_optimize module). Premature optimization adds complexity and risks. Focus on correctness first.

12. **Nested Parallel Forks**: When compiling `par([par([a, b]), par([c, d])])`, should labels be scoped to avoid collisions?
    - **Recommendation**: Labels are globally unique (via make_ref() or global counter), so no collision risk. Each fork generates fresh labels. Inner par's labels are distinct from outer par's labels. Resolution pass replaces all labels globally.

13. **Join Policy Validation**: Should compiler validate that join policies are well-formed (e.g., {n_of_m, N, M} where N ≤ M)?
    - **Recommendation**: Yes, add `validate_join_policy/1` function that checks policy structure. Call during compile_join/2. Throw `badarg` if invalid. This catches errors early (at compile time, not runtime).

14. **Source Mapping**: Should compiler emit source map (wf_term node → bytecode IP) for debugging and error reporting?
    - **Recommendation**: Not in v1. Source mapping is useful for debugging (which AST node produced which opcode) but adds complexity. Add in future if debugging becomes difficult. Can reconstruct from IP and opcode without explicit map.

15. **Compilation Failure Messages**: When compilation fails, how much context should error messages provide?
    - **Recommendation**: Provide structured errors with location information. Example: `{error, {invalid_branch_count, {par, 1}, 2}}` meaning "par has 1 branch, needs 2". Include offending node in error for easier debugging.
