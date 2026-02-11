# Implement bytecode compiler Implementation Plan

## Implementation Plan Title

Bytecode Compiler for Workflow Pattern Term Algebra

## Overview

Implement wf_compile.erl to transform wf_term() AST nodes into flat wf_bc() bytecode instruction lists. The compiler performs a single recursive pass over the AST, emitting opcodes with symbolic labels, then resolves all labels to integer IP addresses. This enables O(1) instruction dispatch at runtime without per-step AST interpretation, satisfying the critical architectural constraint from PROMPT.md:23-27.

The compiler handles all 9 kernel primitives (task, seq, par, xor, join, loop, defer, cancel, mi) and emits 12 opcodes (SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, LOOP_CHECK, CANCEL_SCOPE, MI_SPAWN, EFFECT_YIELD, TASK_EXEC, DONE). Label resolution uses a two-pass approach: emit labels during compilation, then scan bytecode to build label map and replace references.

## Current State

**What exists**:
- Rebar3 scaffold (item 001) with empty app structure
- Complete architecture documentation (ARCHITECTURE.md)
- Complete operational semantics (SEMANTICS.md)
- Item 002 (wf_term) plan exists but implementation is not complete
- No compiler implementation exists yet

**What's missing**:
- wf_compile.erl - bytecode compiler module
- wf_vm.erl - bytecode type definitions
- wf_compile_tests.erl - comprehensive test suite
- All opcode emission logic for 9 kernel primitives
- Label generation and resolution infrastructure
- Validation for compiled bytecode

**Critical dependencies**:
- wf_term.erl (item 002) MUST be completed first
- wf_core.erl derived patterns should exist for integration testing
- Compiler cannot start until wf_term() type is available

**Key constraints discovered**:
1. **No per-step AST dispatch at runtime** (PROMPT.md:23-27) - all structural decisions must be resolved at compile time
2. **Single recursive pass** requirement (ARCHITECTURE.md:392-429) - compilation must be O(N) where N is AST size
3. **Label resolution validation** - final bytecode MUST have no unresolved labels
4. **Opcode format** - tagged tuples, not records (ARCHITECTURE.md:1097-1116)
5. **Defer opcode** - not listed in item.json, will omit or compile to xor in v1

## Desired End State

**Functional requirements**:
- wf_compile:compile/1 accepts wf_term() and returns {ok, wf_bc()} or {error, Reason}
- All 9 kernel primitives compile correctly with proper opcodes
- Label resolution produces integer IP addresses with no unresolved labels
- Bytecode validation ensures structural correctness
- Compiler emits flat opcode lists (no nested structures)

**Non-functional requirements**:
- Compilation is O(N) single pass over AST (label resolution is separate O(M) pass)
- All functions have complete -spec declarations for Dialyzer
- Code coverage > 90% for compiler modules
- Compilation handles deeply nested structures (3+ levels)
- Error messages are descriptive and structured

**Verification criteria**:
- `rebar3 compile` succeeds with zero warnings
- `rebar3 dialyzer` passes with no type errors
- `rebar3 eunit` passes all compiler tests (100% success rate)
- Manual inspection of compiled bytecode matches ARCHITECTURE.md:507-589 example
- Label resolution tests verify 0% unresolved labels in valid bytecode
- Nested pattern tests (seq(par), par(seq), loop(par(seq))) all compile correctly

### Key Discoveries:

- **Dependency on item 002 is hard blocker**: From `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/item.json:5`, item 002 is still in "researched" state. Compiler CANNOT proceed until wf_term() type exists with all 9 constructors. Decision: Implement placeholder types in wf_vm.erl first (pure type definitions), then require item 002 completion before full compiler implementation.

- **Label generation via make_ref()**: From research.md:688-689, using `erlang:make_ref()` is safer than counter-based labels. Counter approach requires state threading through all compile functions (LabelCount accumulator). make_ref() guarantees uniqueness without state management. Decision: Use make_ref() for v1 implementation.

- **Defer opcode missing from spec**: From item.json:6, defer is a kernel primitive but item.json doesn't list DEFER_* opcodes. From research.md:549-556, defer may be compiled to xor or par. Decision: Omit defer compilation in v1, document as TODO. Add compile_defer/1 stub that returns {error, not_implemented}.

- **CANCEL_SCOPE format ambiguity**: From research.md:1049-1050, CANCEL_SCOPE could be single opcode or two opcodes. SEMANTICS.md:749-861 shows CANCEL_ENTER and CANCEL_EXIT as separate rules. Decision: Use two-opcode format `{CANCEL_SCOPE, {enter, ScopeId}}` and `{CANCEL_SCOPE, {exit, ScopeId}}` for clarity.

- **EFFECT_YIELD may not be needed**: From research.md:1046-1047, compiler cannot detect if task yields effect (runtime decision). Executor handles effect yield implicitly when task returns {effect, Spec, ContCtx}. Decision: Omit EFFECT_YIELD opcode from compiler output. Always emit TASK_EXEC. Executor handles effect detection at runtime.

- **wf_vm.erl should define types**: From ARCHITECTURE.md:1097-1116, bytecode types are well-defined. wf_compile should NOT define types internally. Decision: Create wf_vm.erl as bytecode type definition module. wf_compile imports types from wf_vm. Separation of concerns: wf_vm defines what bytecode is, wf_compile produces bytecode.

## What We're NOT Doing

- **NOT implementing executor (wf_exec)**: That's item 005. Compiler only produces bytecode, doesn't execute it.
- **NOT implementing validator (wf_validate)**: That's item 013. Compiler does minimal validation (branch counts, policy formats), not bounded model checking.
- **NOT implementing defer compilation**: Defer opcode not in spec, semantics unclear. Omit in v1.
- **NOT doing bytecode optimization**: No peephole optimizer, no dead code elimination. Emit straightforward bytecode.
- **NOT using counter-based labels**: Use make_ref() for simplicity and safety.
- **NOT emitting EFFECT_YIELD opcodes**: Executor handles effect yield detection implicitly.
- **NOT creating source maps**: Debugging aids can wait. IP + opcode is sufficient for v1.
- **NOT implementing error recovery**: Compiler fails-fast on invalid input. No partial compilation.
- **NOT handling circular AST references**: Assume well-formed input. Cycle detection is wf_validate's job (item 013).

## Implementation Approach

**High-level strategy**: Incremental implementation starting with simple opcodes (task, seq) and progressing to complex ones (par, xor, loop, join, cancel, mi). Use two-pass compilation: emit opcodes with symbolic labels (pass 1), then resolve labels to integer IPs (pass 2). Validate thoroughly at each phase before proceeding.

**Label management**: Use `erlang:make_ref()` for unique labels. Emit `{label, Label}` markers at target positions. Build label map by scanning bytecode and recording IP of each label marker. Replace label references with integer IPs using maps:get/2. Filter out label markers to produce final bytecode.

**Error handling**: Throw `badarg` for contract violations (par with < 2 branches, invalid policy formats). Return `{error, Reason}` for structural issues (unresolved labels after resolution, nested too deep). This follows Erlang "let it crash" philosophy while providing structured errors.

**Testing strategy**: Test each primitive in isolation with unit tests. Test nested combinations (seq(par), par(seq), loop(seq)). Test label resolution with mock bytecode. Test derived patterns from wf_core. Use property-based tests for label resolution correctness.

**Key design decisions**:
1. **wf_vm.erl first**: Define bytecode types before compiler implementation
2. **make_ref() labels**: Safe, simple, no state threading
3. **Two-pass compilation**: Emit labels, then resolve (simpler than single-pass with backpatching)
4. **Omit defer**: Not in spec, semantics unclear, defer to future work
5. **No EFFECT_YIELD**: Executor handles effect detection at runtime
6. **Two-opcode CANCEL_SCOPE**: Explicit enter/exit for clarity
7. **Minimal validation in compiler**: Branch counts, policy formats only. Deep validation in item 013.

---

## Phases

### Phase 1: Bytecode Type Definitions (wf_vm.erl)

#### Overview

Create bytecode type definition module that wf_compile and wf_exec will both depend on. This module defines wf_bc(), opcode(), and all policy types. No compilation logic, just type definitions and exports.

#### Changes Required:

##### 1. Create src/wf_vm.erl module

**File**: `src/wf_vm.erl`
**Changes**: New module defining bytecode types

```erlang
%%%-------------------------------------------------------------------
%%% @doc Bytecode type definitions for workflow pattern VM
%%%
%%% This module defines the bytecode format (wf_bc()) and all opcode types.
%%% Bytecode is a flat list of opcodes produced by wf_compile and
%%% consumed by wf_exec. No per-step AST dispatch occurs at runtime.
%%%
%%% Opcodes are tagged tuples (not records) for:
%%% - Immutable equality (pattern matching works)
%%% - Lightweight representation (no record overhead)
%%% - Easy emission in lists during compilation
%%% - Consistency with wf_term() tagged tuple representation
%%%
%%% From ARCHITECTURE.md:1097-1116 and PROMPT.md:162-166
%%% @end

-module(wf_vm).

%% Export all types for use by wf_compile and wf_exec
-export_type([
    wf_bc/0,
    opcode/0,
    join_policy/0,
    loop_policy/0,
    mi_policy/0,
    label/0
]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

%% Bytecode: Flat list of opcodes
%% From ARCHITECTURE.md:1100
-type wf_bc() :: [opcode()].

%% Opcode: Single instruction with operands
%% From item.json:6 and ARCHITECTURE.md:1103-1115
-type opcode() ::
    {SEQ_ENTER, non_neg_integer()} |              %% Begin sequence scope (operand reserved)
    {SEQ_NEXT, non_neg_integer()} |              %% Advance to next in sequence (target IP)
    {PAR_FORK, [non_neg_integer()]} |            %% Fork N parallel branches (target IPs)
    {JOIN_WAIT, join_policy()} |                  %% Block until join condition met
    {XOR_CHOOSE, [non_neg_integer()]} |          %% Choose exclusive branch (target IPs)
    {LOOP_BACK, non_neg_integer()} |              %% Jump back to loop head (target IP)
    {LOOP_CHECK, loop_policy()} |                 %% Check loop condition (policy operand)
    {CANCEL_SCOPE, {enter | exit, term()} | term()} | %% Enter/exit cancel region (scope_id)
    {MI_SPAWN, mi_policy()} |                     %% Spawn multiple instances (policy operand)
    {TASK_EXEC, atom()} |                         %% Execute task (task name operand)
    {DONE}.                                       %% Terminate execution path (no operands)

%% Join policies
%% From PROMPT.md:114 and wf_term.erl (item 002)
-type join_policy() ::
    all |                        %% Wait for all branches
    sync_merge |                 %% Full synchronization with state merge
    {first_n, pos_integer()} |  %% Wait for N branches (1 <= N <= total)
    {n_of_m, pos_integer(), pos_integer()} | %% Wait for N out of M (1 <= N <= M)
    first_complete.              %% Wait for first branch only

%% Loop policies
%% From PROMPT.md:115 and wf_term.erl (item 002)
-type loop_policy() ::
    while |                     %% While condition true
    until |                     %% Until condition true
    {count, non_neg_integer()}. %% Execute N times

%% Multiple instance policies
%% From PROMPT.md:118 and wf_term.erl (item 002)
-type mi_policy() ::
    {fixed, pos_integer()} |              %% Fixed N instances
    {dynamic, pos_integer(), pos_integer()}. %% Min..Max instances (ctx-dependent)

%% Label type for unresolved bytecode
%% Labels are refs generated by make_ref() during compilation,
%% then resolved to integer IPs in second pass
-type label() :: {label, reference()}.
```

##### 2. Update wf_substrate.app.src modules list

**File**: `src/wf_substrate.app.src`
**Changes**: Add wf_vm to modules list

```erlang
{modules, [wf_vm]},
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with zero warnings
- [ ] `rebar3 dialyzer` passes with no type errors (wf_vm types are well-formed)
- [ ] Module compiles with `erl -type` enabled

##### Manual Verification:

- [ ] Module documentation is clear about purpose (bytecode type definitions only)
- [ ] All exported types are documented with references to specs
- [ ] Type definitions match ARCHITECTURE.md:1097-1116 exactly
- [ ] No compilation logic is present in wf_vm.erl

**Note**: This phase creates the foundation for both compiler and executor. Types must be correct before proceeding.

---

### Phase 2: Compiler Skeleton and Label Infrastructure (wf_compile.erl)

#### Overview

Create wf_compile.erl module with main entry point (compile/1), label generation utilities, label resolution infrastructure, and bytecode validation. Implement compile/1 to dispatch based on wf_term() type, but leave primitive compilation stubs for next phases.

#### Changes Required:

##### 1. Create src/wf_compile.erl skeleton

**File**: `src/wf_compile.erl`
**Changes**: New module with label management and main entry point

```erlang
%%%-------------------------------------------------------------------
%%% @doc Bytecode compiler for workflow pattern terms
%%%
%%% This module compiles wf_term() AST into wf_bc() bytecode using a
%%% single recursive pass over the AST, emitting opcodes with symbolic
%%% labels, then resolving all labels to integer IP addresses.
%%%
%%% Compilation pipeline (from ARCHITECTURE.md:309-340):
%%%   1. Validate term (optional, via wf_validate:structural_check/1)
%%%   2. Single recursive pass over AST, emit opcodes with labels
%%%   3. Resolve labels to integer IPs (linear scan)
%%%   4. Validate bytecode (no unresolved labels)
%%%   5. Return {ok, wf_bc()}
%%%
%%% Label management:
%%%   - Generate unique labels using erlang:make_ref()
%%%   - Emit {label, Label} markers at target positions
%%%   - Build label map: #{Label => IP}
%%%   - Replace label references with integer IPs
%%%   - Remove label markers from final bytecode
%%%
%%% From PROMPT.md:52 (wf_compile.erl) and item 004 specification
%%% @end

-module(wf_compile).

%% Export main compilation function
-export([compile/1]).

%% Import types from wf_vm and wf_term (when available)
-include_lib("wf_vm.hrl").  %% Will import types from wf_vm

%%%===================================================================
%%% Types
%%%===================================================================

%% Unresolved opcode: May contain label references
%% During compilation, opcodes may have label operands (refs).
%% After resolution, all labels are replaced with integer IPs.
-type unresolved_opcode() ::
    {SEQ_ENTER, non_neg_integer()} |
    {SEQ_NEXT, label() | non_neg_integer()} |
    {PAR_FORK, [label() | non_neg_integer()]} |
    {JOIN_WAIT, wf_vm:join_policy()} |
    {XOR_CHOOSE, [label() | non_neg_integer()]} |
    {LOOP_BACK, label() | non_neg_integer()} |
    {LOOP_CHECK, wf_vm:loop_policy()} |
    {CANCEL_SCOPE, {enter | exit, term()} | term()} |
    {MI_SPAWN, wf_vm:mi_policy()} |
    {TASK_EXEC, atom()} |
    {DONE} |
    {label, reference()}.  %% Label marker (removed after resolution)

%% Unresolved bytecode: May contain labels and label markers
-type unresolved_bytecode() :: [unresolved_opcode()].

%% Compilation error reasons
-type compile_error() ::
    {invalid_term, term()} |
    {invalid_branch_count, {atom(), non_neg_integer(), non_neg_integer()}} |
    {invalid_policy, term()} |
    {unresolved_labels, [label()]} |
    {too_deep, non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Compile wf_term() AST into wf_bc() bytecode
%%%
%%% Performs single recursive pass over AST, emitting opcodes with
%%% symbolic labels, then resolves all labels to integer IP addresses.
%%%
%%% From item.json:6 and ARCHITECTURE.md:392-429
%%%
%%% @param Term Workflow pattern term (wf_term())
%%% @return {ok, Bytecode} on success, {error, Reason} on failure
%%% @end
-spec compile(wf_term:wf_term()) -> {ok, wf_vm:wf_bc()} | {error, compile_error()}.
compile(Term) ->
    try
        %% Pass 1: Compile AST to bytecode with labels
        UnresolvedBytecode = compile_term(Term),

        %% Pass 2: Resolve labels to integer IPs
        ResolvedBytecode = resolve_labels(UnresolvedBytecode),

        %% Pass 3: Validate no unresolved labels remain
        case validate_bytecode(ResolvedBytecode) of
            ok ->
                {ok, ResolvedBytecode};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        throw:Reason ->
            {error, Reason};
        error:Reason ->
            {error, {invalid_term, Reason}}
    end.

%%%===================================================================
%%% Internal Functions - Compilation
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Compile wf_term() to unresolved bytecode (with labels)
%%%
%%% This is the main recursive compilation function that dispatches
%%% on the term type and calls the appropriate compile_* function.
%%% Each compile_* function returns a list of unresolved opcodes.
%%%
%%% Stub implementations in this phase, filled in later phases.
%%% @end
-spec compile_term(wf_term:wf_term()) -> unresolved_bytecode().
compile_term({task, _Name, _Metadata} = Term) ->
    compile_task(Term);

compile_term({seq, _Left, _Right} = Term) ->
    compile_seq(Term);

compile_term({par, _Branches} = Term) ->
    compile_par(Term);

compile_term({xor, _Alternatives} = Term) ->
    compile_xor(Term);

compile_term({join, _Policy, _Branches} = Term) ->
    compile_join(Term);

compile_term({loop, _Policy, _Body} = Term) ->
    compile_loop(Term);

compile_term({defer, _Alternatives} = Term) ->
    compile_defer(Term);

compile_term({cancel, _ScopeId, _Body} = Term) ->
    compile_cancel(Term);

compile_term({mi, _Policy, _Body} = Term) ->
    compile_mi(Term);

compile_term(InvalidTerm) ->
    error({badarg, {invalid_term, InvalidTerm}}).

%% Stub implementations (will be filled in later phases)

compile_task({task, Name, _Metadata}) when is_atom(Name) ->
    %% Phase 3: Emit TASK_EXEC opcode
    [{TASK_EXEC, Name}].

compile_seq({seq, Left, Right}) ->
    %% Phase 3: Emit SEQ_ENTER, Left bytecode, SEQ_NEXT with label, Right bytecode
    error({not_implemented, seq}).

compile_par({par, Branches}) ->
    %% Phase 4: Emit PAR_FORK with branch labels, each branch with DONE, JOIN_WAIT
    error({not_implemented, par}).

compile_xor({xor, Alternatives}) ->
    %% Phase 4: Emit XOR_CHOOSE with alt labels, each alt with DONE
    error({not_implemented, xor}).

compile_join({join, Policy, Branches}) ->
    %% Phase 5: Emit PAR_FORK, branches, JOIN_WAIT with policy
    error({not_implemented, join}).

compile_loop({loop, Policy, Body}) ->
    %% Phase 5: Emit LOOP_CHECK, body, LOOP_BACK, exit label
    error({not_implemented, loop}).

compile_defer({defer, _Alternatives}) ->
    %% Defer not implemented in v1 (not in spec)
    throw({error, {not_implemented, defer}}).

compile_cancel({cancel, ScopeId, Body}) ->
    %% Phase 6: Emit CANCEL_SCOPE enter, body, CANCEL_SCOPE exit
    error({not_implemented, cancel}).

compile_mi({mi, Policy, Body}) ->
    %% Phase 6: Emit MI_SPAWN, body, DONE, JOIN_WAIT
    error({not_implemented, mi}).

%%%===================================================================
%%% Internal Functions - Label Management
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Generate a unique label using make_ref()
%%%
%%% Labels are unique references that will be resolved to integer IPs
%%% in the second pass. Using make_ref() guarantees uniqueness without
%%% requiring state threading through all compile functions.
%%%
%%% @return Label tuple
%%% @end
-spec make_label() -> wf_vm:label().
make_label() ->
    {label, erlang:make_ref()}.

%%%-------------------------------------------------------------------
%%% @doc Build label map from unresolved bytecode
%%%
%%% Scans bytecode and records the IP address of each {label, Label}
%%% marker. Returns a map from label reference to integer IP.
%%%
%%% From research.md:587-601
%%%
%%% @param Bytecode Unresolved bytecode with label markers
%%% @return Label map #{Label => IP}
%%% @end
-spec build_label_map(unresolved_bytecode()) -> #{reference() => non_neg_integer()}.
build_label_map(Bytecode) ->
    build_label_map(Bytecode, 0, #{}).

build_label_map([], _IP, Map) ->
    Map;
build_label_map([{label, Label} | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map#{Label => IP});
build_label_map([_Opcode | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map).

%%%-------------------------------------------------------------------
%%% @doc Resolve all labels in bytecode to integer IPs
%%%
%%% Replaces label references in opcode operands with integer IPs using
%%% the label map. Removes {label, Label} markers from final bytecode.
%%%
%%% From research.md:603-622
%%%
%%% @param Bytecode Unresolved bytecode
%%% @return Resolved bytecode (wf_vm:wf_bc())
%%% @end
-spec resolve_labels(unresolved_bytecode()) -> wf_vm:wf_bc().
resolve_labels(Bytecode) ->
    LabelMap = build_label_map(Bytecode),
    Resolved = replace_labels(Bytecode, LabelMap),
    %% Remove label markers
    lists:filter(fun({label, _}) -> false; (_) -> true end, Resolved).

%% @doc Replace label references in opcodes with integer IPs
-spec replace_labels(unresolved_bytecode(), #{reference() => non_neg_integer()}) -> unresolved_bytecode().
replace_labels(Bytecode, LabelMap) ->
    lists:map(fun(Opcode) -> resolve_opcode_labels(Opcode, LabelMap) end, Bytecode).

%% @doc Resolve labels in a single opcode
-spec resolve_opcode_labels(unresolved_opcode(), #{reference() => non_neg_integer()}) -> wf_vm:opcode().
resolve_opcode_labels({SEQ_NEXT, {label, Ref}}, LabelMap) ->
    {SEQ_NEXT, maps:get(Ref, LabelMap)};
resolve_opcode_labels({PAR_FORK, Labels}, LabelMap) when is_list(Labels) ->
    {PAR_FORK, [resolve_label(L, LabelMap) || L <- Labels]};
resolve_opcode_labels({XOR_CHOOSE, Labels}, LabelMap) when is_list(Labels) ->
    {XOR_CHOOSE, [resolve_label(L, LabelMap) || L <- Labels]};
resolve_opcode_labels({LOOP_BACK, {label, Ref}}, LabelMap) ->
    {LOOP_BACK, maps:get(Ref, LabelMap)};
resolve_opcode_labels(Opcode, _LabelMap) ->
    %% No labels to resolve (or already resolved)
    Opcode.

%% @doc Resolve a single label (if it's a label ref)
-spec resolve_label(label() | non_neg_integer(), #{reference() => non_neg_integer()}) -> non_neg_integer().
resolve_label({label, Ref}, LabelMap) ->
    maps:get(Ref, LabelMap);
resolve_label(IP, _LabelMap) when is_integer(IP) ->
    IP.

%%%===================================================================
%%% Internal Functions - Validation
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc Validate that bytecode has no unresolved labels
%%%
%%% Scans bytecode and ensures no label references remain. All labels
%%% should have been resolved to integer IPs.
%%%
%%% From research.md:624-636
%%%
%%% @param Bytecode Bytecode to validate
%%% @return ok if valid, {error, {unresolved_labels, Labels}} if invalid
%%% @end
-spec validate_bytecode(wf_vm:wf_bc()) -> ok | {error, {unresolved_labels, [wf_vm:label()]}}.
validate_bytecode(Bytecode) ->
    Unresolved = [L || Op <- Bytecode, is_unresolved(Op), L <- [extract_label(Op)]],
    case Unresolved of
        [] -> ok;
        _ -> {error, {unresolved_labels, Unresolved}}
    end.

%% @doc Check if opcode contains unresolved label
-spec is_unresolved(wf_vm:opcode()) -> boolean().
is_unresolved({_, {label, _Ref}}) -> true;
is_unresolved({_, Labels}) when is_list(Labels) ->
    lists:any(fun({label, _}) -> true; (_) -> false end, Labels);
is_unresolved(_) -> false.

%% @doc Extract label from unresolved opcode
-spec extract_label(wf_vm:opcode()) -> wf_vm:label().
extract_label({_, {label, _} = Label}) -> Label;
extract_label({_, [Label | _]}) when element(1, Label) =:= label -> Label.
```

##### 2. Update wf_substrate.app.src modules list

**File**: `src/wf_substrate.app.src`
**Changes**: Add wf_compile to modules list

```erlang
{modules, [wf_vm, wf_compile]},
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with zero warnings
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Module compiles and exports compile/1 function
- [ ] make_label/0 generates unique refs (test with multiple calls)

##### Manual Verification:

- [ ] Module documentation clearly explains two-pass compilation approach
- [ ] Label resolution code matches research.md:587-622
- [ ] Error handling uses both throws and returns appropriately
- [ ] Stub compile_* functions indicate which phase implements them

**Note**: This phase creates the infrastructure but doesn't compile valid workflows yet. Stubs will be filled in phases 3-6.

---

### Phase 3: Simple Primitives (task, seq)

#### Overview

Implement compilation for task (simplest) and seq (requires label management). These are the foundational primitives. task emits TASK_EXEC, seq emits SEQ_ENTER/SEQ_NEXT with label for right branch.

#### Changes Required:

##### 1. Implement compile_task/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_task/1 implementation

```erlang
compile_task({task, Name, _Metadata}) when is_atom(Name) ->
    %% TASK_EXEC: Execute task at runtime
    %% From SEMANTICS.md:238-306
    %% Compiler cannot detect if task yields effect (runtime decision)
    %% Always emit TASK_EXEC, executor handles effect yield
    [{TASK_EXEC, Name}].
```

##### 2. Implement compile_seq/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_seq/1 implementation

```erlang
compile_seq({seq, Left, Right}) ->
    %% SEQ: Execute Left, then Right
    %% From SEMANTICS.md:307-375 and ARCHITECTURE.md:405-415
    %%
    %% Bytecode structure:
    %%   SEQ_ENTER: Push sequence scope
    %%   [Left bytecode]: Execute left branch
    %%   SEQ_NEXT Label: When left completes, jump to right
    %%   Label: Target for SEQ_NEXT
    %%   [Right bytecode]: Execute right branch
    %%
    %% Label allows SEQ_NEXT to jump over left code to right code
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

##### 3. Create basic test file

**File**: `test/wf_compile_tests.erl`
**Changes**: New test module

```erlang
%%%-------------------------------------------------------------------
%%% @doc Unit tests for wf_compile bytecode compiler
%%%
%%% Tests compilation of each kernel primitive, label resolution,
%%% and validation of compiled bytecode.
%%% @end

-module(wf_compile_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Helpers
%%%===================================================================

%% Mock task metadata
mock_task_fun() ->
    fun(_Ctx) -> {ok, #{}} end.

mock_task_metadata() ->
    #{function => mock_task_fun()}.

%%%===================================================================
%%% Phase 3: Task and Sequence Tests
%%%===================================================================

compile_task_test_() ->
    {"Compile single task",
     fun() ->
         Term = {task, my_task, mock_task_metadata()},
         {ok, Bytecode} = wf_compile:compile(Term),
         ?assertEqual([{TASK_EXEC, my_task}], Bytecode)
     end}.

compile_seq_test_() ->
    {"Compile sequence of two tasks",
     fun() ->
         Term = {seq,
                 {task, task_a, mock_task_metadata()},
                 {task, task_b, mock_task_metadata()}},
         {ok, Bytecode} = wf_compile:compile(Term),
         ?assertMatch([{SEQ_ENTER, 0}, {TASK_EXEC, task_a},
                       {SEQ_NEXT, _}, {TASK_EXEC, task_b}],
                      lists:flatten(Bytecode)),
         %% Verify SEQ_NEXT target is integer (resolved label)
         {_, SEQ_NEXT_IP} = lists:nth(3, lists:flatten(Bytecode)),
         ?assert(is_integer(SEQ_NEXT_IP))
     end}.
```

##### 4. Update wf_substrate.app.src to include test module

**File**: `src/wf_substrate.app.src`
**Changes**: No changes needed (EUnit auto-discovers test modules)

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes compile_task_test_ (task emits TASK_EXEC)
- [ ] `rebar3 eunit` passes compile_seq_test_ (seq emits SEQ_ENTER, SEQ_NEXT, both tasks)
- [ ] `rebar3 compile` succeeds with zero warnings
- [ ] `rebar3 dialyzer` passes

##### Manual Verification:

- [ ] Manual inspection of seq bytecode shows SEQ_NEXT operand is integer (not label)
- [ ] Label resolution correctly maps RightLabel to IP of right branch
- [ ] compile_term/1 dispatches correctly for task and seq nodes

**Note**: Task is simplest (single opcode). Seq introduces label management. Next phases add complexity (multiple branches, loops).

---

### Phase 4: Parallel and Exclusive Choice (par, xor)

#### Overview

Implement compilation for par (parallel fork with join) and xor (exclusive choice). Both emit fork opcodes with multiple branch labels. par also emits JOIN_WAIT, xor does not (only one branch runs).

#### Changes Required:

##### 1. Implement compile_par/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_par/1 implementation

```erlang
compile_par({par, Branches}) when length(Branches) >= 2 ->
    %% PAR: Fork N parallel branches, wait for all at JOIN_WAIT
    %% From SEMANTICS.md:376-448 and research.md:336-363
    %%
    %% Bytecode structure:
    %%   PAR_FORK [Label1, ..., LabelN]: Spawn N tokens
    %%   Label1: Branch 1 start
    %%   [Branch1 bytecode]: Execute branch 1
    %%   DONE: Branch 1 complete (join counter++)
    %%   ...
    %%   LabelN: Branch N start
    %%   [BranchN bytecode]: Execute branch N
    %%   DONE: Branch N complete (join counter++)
    %%   JoinLabel: Convergence point
    %%   JOIN_WAIT all: Block until all branches complete
    %%
    %% Each branch MUST end with DONE to signal completion
    %% JOIN_WAIT blocks until join counter reaches N
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

##### 2. Implement compile_xor/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_xor/1 implementation

```erlang
compile_xor({xor, Alternatives}) when length(Alternatives) >= 2 ->
    %% XOR: Exclusive choice, select ONE branch at runtime
    %% From SEMANTICS.md:449-480 and research.md:365-388
    %%
    %% Bytecode structure:
    %%   XOR_CHOOSE [Label1, ..., LabelN]: Scheduler picks ONE
    %%   Label1: Alt 1 start
    %%   [Alt1 bytecode]: Execute alt 1 (only if selected)
    %%   DONE: Alt 1 complete
    %%   ...
    %%   LabelN: Alt N start
    %%   [AltN bytecode]: Execute alt N (only if selected)
    %%   DONE: Alt N complete
    %%   NO JOIN WAIT: Only one branch runs, no synchronization
    %%
    %% Unselected branches are NEVER spawned (not cancelled)
    %% No join needed (only one token exists)
    AltCodes = [compile_term(A) || A <- Alternatives],
    AltLabels = [make_label() || _ <- Alternatives],
    [
        {XOR_CHOOSE, AltLabels},
        lists:zipwith(fun(Code, Label) ->
            [{label, Label}, Code, {DONE}]
        end, AltCodes, AltLabels)
    ].
```

##### 3. Add tests for par and xor

**File**: `test/wf_compile_tests.erl`
**Changes**: Add test cases

```erlang
compile_par_test_() ->
    {"Compile parallel fork of two tasks",
     fun() ->
         Term = {par, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         ?assertMatch([{PAR_FORK, [_Label1, _Label2]}, _, _, _, _, {JOIN_WAIT, all}],
                      FlatBytecode),
         %% Verify PAR_FORK operand is list of integers (resolved labels)
         {_, ForkLabels} = lists:nth(1, FlatBytecode),
         ?assertEqual(2, length(ForkLabels)),
         ?assert(lists:all(fun is_integer/1, ForkLabels))
     end}.

compile_xor_test_() ->
    {"Compile exclusive choice of two tasks",
     fun() ->
         Term = {xor, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         ?assertMatch([{XOR_CHOOSE, [_Label1, _Label2]}, _, _, _, _],
                      FlatBytecode),
         %% Verify XOR_CHOOSE operand is list of integers
         {_, ChooseLabels} = lists:nth(1, FlatBytecode),
         ?assertEqual(2, length(ChooseLabels)),
         ?assert(lists:all(fun is_integer/1, ChooseLabels)),
         %% Verify no JOIN_WAIT present
         ?assertNot(lists:keymember(JOIN_WAIT, 1, FlatBytecode))
     end}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes compile_par_test_ (par emits PAR_FORK, branches, JOIN_WAIT)
- [ ] `rebar3 eunit` passes compile_xor_test_ (xor emits XOR_CHOOSE, branches, no JOIN_WAIT)
- [ ] All branch labels resolve to integers
- [ ] `rebar3 compile` succeeds

##### Manual Verification:

- [ ] Manual inspection of par bytecode shows PAR_FORK with 2 integer targets
- [ ] Each branch ends with DONE
- [ ] JOIN_WAIT is last opcode (after all branches)
- [ ] xor bytecode has no JOIN_WAIT

**Note**: par spawns all branches and joins them. xor spawns only one branch and has no join. Key difference is presence of JOIN_WAIT.

---

### Phase 5: Loop and Join (loop, join)

#### Overview

Implement compilation for loop (requires LOOP_CHECK and LOOP_BACK with exit label) and join (explicit join policy). Loop is tricky because it needs backward jump. Join is similar to par but with explicit policy operand.

#### Changes Required:

##### 1. Implement compile_loop/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_loop/1 implementation

```erlang
compile_loop({loop, Policy, Body}) ->
    %% LOOP: Repeated execution until condition satisfied
    %% From SEMANTICS.md:592-695 and research.md:421-479
    %%
    %% Bytecode structure (same for all policies):
    %%   LoopHeadLabel: Loop entry point
    %%   LOOP_CHECK Policy: Check condition (exit or continue)
    %%   [Body bytecode]: Execute loop body
    %%   LOOP_BACK LoopHeadLabel: Jump back to entry
    %%   ExitLabel: Loop exit point (after LOOP_BACK)
    %%
    %% Policy determines when LOOP_CHECK exits:
    %%   - {count, N}: Decrement, exit if 0, continue if > 0
    %%   - while: Check condition FIRST, exit if false
    %%   - until: Execute body, check condition AFTER, exit if true
    %%
    %% Compiler emits same structure for all policies.
    %% Runtime semantics differ based on policy operand.
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

##### 2. Implement compile_join/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_join/1 implementation

```erlang
compile_join({join, Policy, Branches}) when length(Branches) >= 2 ->
    %% JOIN: Explicit join with policy
    %% From SEMANTICS.md:481-591 and research.md:390-419
    %%
    %% Bytecode structure (same as par, but with explicit policy):
    %%   PAR_FORK [Label1, ..., LabelN]: Spawn N tokens
    %%   Label1: Branch 1 start
    %%   [Branch1 bytecode]: Execute branch 1
    %%   DONE: Branch 1 complete
    %%   ...
    %%   LabelN: Branch N start
    %%   [BranchN bytecode]: Execute branch N
    %%   DONE: Branch N complete
    %%   JoinLabel: Convergence point
    %%   JOIN_WAIT Policy: Block until policy satisfied
    %%
    %% Difference from par: par is implicit join(all, ...),
    %% join has explicit policy (all/first_n/n_of_m/sync_merge/first_complete)
    %%
    %% Policy validation: Compiler validates policy format
    validate_join_policy(Policy),
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

%% @private Validate join policy format
%% Throws badarg if policy is invalid
-spec validate_join_policy(wf_vm:join_policy()) -> ok.
validate_join_policy(all) -> ok;
validate_join_policy(sync_merge) -> ok;
validate_join_policy(first_complete) -> ok;
validate_join_policy({first_n, N}) when is_integer(N), N > 0 -> ok;
validate_join_policy({n_of_m, N, M}) when is_integer(N), is_integer(M), N > 0, M >= N -> ok;
validate_join_policy(InvalidPolicy) ->
    error({badarg, {invalid_join_policy, InvalidPolicy}}).
```

##### 3. Add tests for loop and join

**File**: `test/wf_compile_tests.erl`
**Changes**: Add test cases

```erlang
compile_loop_count_test_() ->
    {"Compile count loop",
     fun() ->
         Term = {loop, {count, 3}, {task, task_a, mock_task_metadata()}},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         ?assertMatch([_, {LOOP_CHECK, {count, 3}}, {TASK_EXEC, task_a},
                       {LOOP_BACK, _}, _], FlatBytecode),
         %% Verify LOOP_BACK operand is integer (backward jump)
         {_, LOOP_BACK_IP} = lists:nth(4, FlatBytecode),
         ?assert(is_integer(LOOP_BACK_IP))
     end}.

compile_join_first_n_test_() ->
    {"Compile join with first_n policy",
     fun() ->
         Term = {join, {first_n, 1}, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         ?assertMatch([_, _, _, _, _, _, {JOIN_WAIT, {first_n, 1}}],
                      lists:reverse(FlatBytecode))
     end}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes compile_loop_count_test_ (loop emits LOOP_CHECK, LOOP_BACK)
- [ ] `rebar3 eunit` passes compile_join_first_n_test_ (join emits PAR_FORK, JOIN_WAIT with policy)
- [ ] LOOP_BACK operand is integer (backward jump to loop head)
- [ ] validate_join_policy/1 throws badarg for invalid policies

##### Manual Verification:

- [ ] Loop bytecode shows LOOP_BACK jumping to LOOP_CHECK (not beginning of bytecode)
- [ ] Join bytecode has JOIN_WAIT with explicit policy (not hardcoded all)
- [ ] Invalid join policies (e.g., {first_n, 0}, {n_of_m, 5, 3}) throw badarg

**Note**: Loop requires backward jump (LOOP_BACK). Join is par with explicit policy. Both validate policy format before compilation.

---

### Phase 6: Cancel and Multiple Instances (cancel, mi)

#### Overview

Implement compilation for cancel (enter/exit scope) and mi (spawn instances with join). Cancel doesn't use labels (linear). Mi is similar to par but spawns instances instead of branches.

#### Changes Required:

##### 1. Implement compile_cancel/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_cancel/1 implementation

```erlang
compile_cancel({cancel, ScopeId, Body}) ->
    %% CANCEL: Region cancellation wrapper
    %% From SEMANTICS.md:749-861 and research.md:481-502
    %%
    %% Bytecode structure:
    %%   CANCEL_SCOPE {enter, ScopeId}: Push scope onto stack
    %%   [Body bytecode]: Execute body (all tokens inherit scope)
    %%   CANCEL_SCOPE {exit, ScopeId}: Pop scope from stack
    %%
    %% If cancel signal received during body execution,
    %% all tokens in scope are marked cancelled.
    %% Cancellation propagates to nested scopes recursively.
    %%
    %% No labels needed (linear execution).
    %% Two-opcode format makes enter/exit explicit.
    BodyCode = compile_term(Body),
    [
        {CANCEL_SCOPE, {enter, ScopeId}},
        BodyCode,
        {CANCEL_SCOPE, {exit, ScopeId}}
    ].
```

##### 2. Implement compile_mi/1 in wf_compile.erl

**File**: `src/wf_compile.erl`
**Changes**: Fill in compile_mi/1 implementation

```erlang
compile_mi({mi, Policy, Body}) ->
    %% MI: Multiple instances (parallel instances with join)
    %% From SEMANTICS.md:862-968 and research.md:504-531
    %%
    %% Bytecode structure:
    %%   MI_SPAWN Policy: Spawn N instances
    %%   [Body bytecode]: Template for each instance
    %%   DONE: Each instance terminates here
    %%   JOIN_WAIT all: Collect all instance results
    %%
    %% Policy determines instance count:
    %%   - {fixed, N}: Spawn exactly N instances
    %%   - {dynamic, Min, Max}: Spawn Min..Max based on ctx()
    %%
    %% Each instance executes Body independently with its own token.
    %% All instances converge at JOIN_WAIT.
    %%
    %% Alternative design: MI_SPAWN spawns instances and includes
    %% body bytecode as template. Executor replicates bytecode for
    %% each instance. This is more efficient than emitting N copies.
    validate_mi_policy(Policy),
    BodyCode = compile_term(Body),
    [
        {MI_SPAWN, Policy},
        BodyCode,
        {DONE},
        {JOIN_WAIT, all}
    ].

%% @private Validate MI policy format
%% Throws badarg if policy is invalid
-spec validate_mi_policy(wf_vm:mi_policy()) -> ok.
validate_mi_policy({fixed, N}) when is_integer(N), N > 0 -> ok;
validate_mi_policy({dynamic, Min, Max}) when is_integer(Min), is_integer(Max), Min > 0, Max >= Min -> ok;
validate_mi_policy(InvalidPolicy) ->
    error({badarg, {invalid_mi_policy, InvalidPolicy}}).
```

##### 3. Add tests for cancel and mi

**File**: `test/wf_compile_tests.erl`
**Changes**: Add test cases

```erlang
compile_cancel_test_() ->
    {"Compile cancel scope",
     fun() ->
         Term = {cancel, my_scope, {task, task_a, mock_task_metadata()}},
         {ok, Bytecode} = wf_compile:compile(Term),
         ?assertMatch([{CANCEL_SCOPE, {enter, my_scope}}, {TASK_EXEC, task_a},
                       {CANCEL_SCOPE, {exit, my_scope}}], Bytecode)
     end}.

compile_mi_test_() ->
    {"Compile multiple instances",
     fun() ->
         Term = {mi, {fixed, 3}, {task, task_a, mock_task_metadata()}},
         {ok, Bytecode} = wf_compile:compile(Term),
         ?assertMatch([{MI_SPAWN, {fixed, 3}}, {TASK_EXEC, task_a}, {DONE},
                       {JOIN_WAIT, all}], Bytecode)
     end}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes compile_cancel_test_ (cancel emits enter/exit opcodes)
- [ ] `rebar3 eunit` passes compile_mi_test_ (mi emits MI_SPAWN, body, DONE, JOIN_WAIT)
- [ ] validate_mi_policy/1 throws badarg for invalid policies

##### Manual Verification:

- [ ] Cancel bytecode has CANCEL_SCOPE enter and exit opcodes (linear, no labels)
- [ ] Mi bytecode has MI_SPAWN followed by body template, DONE, JOIN_WAIT
- [ ] Invalid mi policies (e.g., {fixed, 0}, {dynamic, 5, 3}) throw badarg

**Note**: Cancel is linear (no labels). Mi spawns instances then joins them. Both validate policy format.

---

### Phase 7: Integration Testing and Nested Patterns

#### Overview

Test complex nested workflows to ensure label resolution works correctly across multiple levels. Test derived patterns from wf_core. Ensure compiler handles all kernel primitives and combinations.

#### Changes Required:

##### 1. Add comprehensive integration tests

**File**: `test/wf_compile_tests.erl`
**Changes**: Add nested pattern tests

```erlang
compile_seq_par_test_() ->
    {"Compile seq(par([a, b]), c)",
     fun() ->
         Term = {seq,
                 {par, [
                     {task, task_a, mock_task_metadata()},
                     {task, task_b, mock_task_metadata()}
                 ]},
                 {task, task_c, mock_task_metadata()}},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         %% Should have: SEQ_ENTER, PAR_FORK, 2 branches with DONE, JOIN_WAIT, SEQ_NEXT, task_c
         ?assertMatch([{SEQ_ENTER, 0}, {PAR_FORK, [_]}, _, _, _, _, {JOIN_WAIT, all},
                       {SEQ_NEXT, _}, {TASK_EXEC, task_c}], FlatBytecode)
     end}.

compile_par_seq_test_() ->
    {"Compile par([seq(a, b), seq(c, d)])",
     fun() ->
         Term = {par, [
             {seq, {task, task_a, mock_task_metadata()}, {task, task_b, mock_task_metadata()}},
             {seq, {task, task_c, mock_task_metadata()}, {task, task_d, mock_task_metadata()}}
         ]},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         %% Should have: PAR_FORK, 2 sequences, each with SEQ_ENTER/SEQ_NEXT, DONE, DONE, JOIN_WAIT
         ?assertEqual(2, length([Op || {DONE} <- FlatBytecode])),
         ?assert(lists:keymember(JOIN_WAIT, 1, FlatBytecode))
     end}.

compile_loop_par_test_() ->
    {"Compile loop({count, 2}, par([a, b]))",
     fun() ->
         Term = {loop, {count, 2},
                  {par, [
                      {task, task_a, mock_task_metadata()},
                      {task, task_b, mock_task_metadata()}
                  ]}},
         {ok, Bytecode} = wf_compile:compile(Term),
         FlatBytecode = lists:flatten(Bytecode),
         %% Should have: LOOP_CHECK, PAR_FORK, 2 branches with DONE, JOIN_WAIT, LOOP_BACK
         ?assertMatch([_, {LOOP_CHECK, {count, 2}}, {PAR_FORK, [_]}, _, _, _, _, {JOIN_WAIT, all},
                       {LOOP_BACK, _}, _], FlatBytecode)
     end}.
```

##### 2. Add label resolution tests

**File**: `test/wf_compile_tests.erl`
**Changes**: Add label resolution tests

```erlang
label_resolution_test_() ->
    {"Verify all labels resolved in complex workflow",
     fun() ->
         Term = {loop, {count, 3},
                  {par, [
                      {seq, {task, task_a, mock_task_metadata()},
                             {task, task_b, mock_task_metadata()}},
                      {seq, {task, task_c, mock_task_metadata()},
                             {task, task_d, mock_task_metadata()}}
                  ]}},
         {ok, Bytecode} = wf_compile:compile(Term),
         %% Verify no label markers remain
         ?assertNot(lists:any(fun({label, _}) -> true; (_) -> false end, Bytecode)),
         %% Verify all label operands are integers
         ?assert(lists:all(fun
             ({_, {label, _}}) -> false;
             ({_, Labels}) when is_list(Labels) ->
                 lists:all(fun is_integer/1, Labels);
             (_) -> true
         end, Bytecode))
     end}.
```

##### 3. Add validation tests

**File**: `test/wf_compile_tests.erl`
**Changes**: Add validation tests

```erlang
validate_branch_count_test_() ->
    {"Validate par requires at least 2 branches",
     fun() ->
         %% Single branch par should fail
         Term = {par, [{task, task_a, mock_task_metadata()}]},
         ?assertMatch({error, _}, wf_compile:compile(Term))
     end}.

validate_join_policy_test_() ->
    {"Validate join policy format",
     fun() ->
         %% Invalid n_of_m where N > M should fail
         Term = {join, {n_of_m, 5, 3}, [
             {task, task_a, mock_task_metadata()},
             {task, task_b, mock_task_metadata()}
         ]},
         ?assertThrow({badarg, {invalid_join_policy, _}},
                      wf_compile:compile(Term))
     end}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] All nested pattern tests pass
- [ ] Label resolution tests verify 0% label markers in bytecode
- [ ] Validation tests reject invalid inputs
- [ ] Code coverage > 90% for wf_compile.erl
- [ ] `rebar3 eunit` passes all tests

##### Manual Verification:

- [ ] Manually inspect bytecode for seq(par) example matches ARCHITECTURE.md:507-589
- [ ] Label resolution correctly handles 10+ labels in complex workflows
- [ ] Nested loops (loop within loop) have correct LOOP_BACK targets

**Note**: This phase validates the entire compiler. All primitives work, labels resolve correctly, validation catches errors.

---

### Phase 8: Documentation and Final Verification

#### Overview

Add comprehensive module documentation, function docs with examples, and verify entire implementation matches specifications. Ensure compiler is production-ready.

#### Changes Required:

##### 1. Complete wf_compile.erl documentation

**File**: `src/wf_compile.erl`
**Changes**: Add complete -moduledoc and -doc comments

```erlang
%%%-------------------------------------------------------------------
%%% @doc Bytecode compiler for workflow pattern terms
%%%
%%% This module compiles wf_term() AST into wf_bc() bytecode using a
%%% single recursive pass over the AST, emitting opcodes with symbolic
%%% labels, then resolving all labels to integer IP addresses.
%%%
%%% == Compilation Pipeline ==
%%%
%%% From ARCHITECTURE.md:309-340:
%%%   1. Validate term (optional, via wf_validate:structural_check/1)
%%%   2. Single recursive pass over AST, emit opcodes with labels
%%%   3. Resolve labels to integer IPs (linear scan)
%%%   4. Validate bytecode (no unresolved labels)
%%%   5. Return {ok, wf_bc()}
%%%
%%% == Label Management ==
%%%
%%% Labels are generated using erlang:make_ref() to guarantee uniqueness.
%%% During compilation, opcodes may reference labels (e.g., {SEQ_NEXT, Label}).
%%% In the resolution pass, labels are replaced with integer IP addresses.
%%%
%%% Example compilation of seq(par([task(a), task(b)]), task(c)):
%%%
%%%   Unresolved bytecode:
%%%     [{SEQ_ENTER, 0},
%%%      {PAR_FORK, [Label1, Label2]},
%%%      {label, Label1}, {TASK_EXEC, a}, {DONE},
%%%      {label, Label2}, {TASK_EXEC, b}, {DONE},
%%%      {SEQ_NEXT, Label3},
%%%      {label, Label3}, {TASK_EXEC, c}]
%%%
%%%   Resolved bytecode (Label1=2, Label2=5, Label3=8):
%%%     [{SEQ_ENTER, 0},
%%%      {PAR_FORK, [2, 5]},
%%%      {TASK_EXEC, a}, {DONE},
%%%      {TASK_EXEC, b}, {DONE},
%%%      {SEQ_NEXT, 8},
%%%      {TASK_EXEC, c}]
%%%
%%% == Opcode Emission ==
%%%
%%% Each kernel primitive emits specific opcodes:
%%%   - task/2: TASK_EXEC
%%%   - seq/2: SEQ_ENTER, SEQ_NEXT with label
%%%   - par/1: PAR_FORK with branch labels, DONE per branch, JOIN_WAIT all
%%%   - xor/1: XOR_CHOOSE with alt labels, DONE per alt (no join)
%%%   - join/2: PAR_FORK with branch labels, DONE per branch, JOIN_WAIT Policy
%%%   - loop/2: LOOP_CHECK, body, LOOP_BACK with label, exit label
%%%   - defer/1: NOT IMPLEMENTED (not in spec)
%%%   - cancel/2: CANCEL_SCOPE enter, body, CANCEL_SCOPE exit
%%%   - mi/2: MI_SPAWN, body, DONE, JOIN_WAIT all
%%%
%%% == Error Handling ==
%%%
%%% Compiler throws badarg for contract violations:
%%%   - par with < 2 branches
%%%   - xor with < 2 alternatives
%%%   - join with < 2 branches
%%%   - Invalid policy format
%%%
%%% Compiler returns {error, Reason} for structural issues:
%%%   - Unresolved labels after resolution
%%%
%%% == Dependencies ==
%%%
%%% - wf_term.erl (item 002): wf_term() type definition
%%% - wf_vm.erl (item 004): wf_bc() and opcode() type definitions
%%% - wf_validate.erl (item 013): Structural validation (optional)
%%%
%%% @see wf_vm:opcode/0 for opcode definitions
%%% @see wf_term:wf_term/0 for AST type definition
%%% @end

-module(wf_compile).
...
```

##### 2. Complete wf_vm.erl documentation

**File**: `src/wf_vm.erl`
**Changes**: Add complete -moduledoc with opcode descriptions

```erlang
%%%-------------------------------------------------------------------
%%% @doc Bytecode type definitions for workflow pattern VM
%%%
%%% This module defines the bytecode format (wf_bc()) and all opcode types.
%%% Bytecode is a flat list of opcodes produced by wf_compile and
%%% consumed by wf_exec. No per-step AST dispatch occurs at runtime.
%%%
%%% == Bytecode Format ==
%%%
%%% Bytecode is a flat list of tagged tuples. Each tuple represents
%%% one instruction. Opcodes are not records to ensure:
%%%   - Immutable equality (pattern matching works)
%%%   - Lightweight representation (no record overhead)
%%%   - Easy emission in lists during compilation
%%%
%%% == Opcodes ==
%%%
%%% SEQ_ENTER: Begin sequence scope (operand reserved for future use)
%%% SEQ_NEXT: Advance to next in sequence (operand is target IP)
%%% PAR_FORK: Fork N parallel branches (operand is list of target IPs)
%%% JOIN_WAIT: Block until join condition met (operand is policy)
%%% XOR_CHOOSE: Choose exclusive branch (operand is list of target IPs)
%%% LOOP_BACK: Jump back to loop head (operand is target IP)
%%% LOOP_CHECK: Check loop condition (operand is policy)
%%% CANCEL_SCOPE: Enter/exit cancel region (operand is scope_id or {enter|exit, scope_id})
%%% MI_SPAWN: Spawn multiple instances (operand is policy)
%%% TASK_EXEC: Execute task (operand is task name)
%%% DONE: Terminate execution path (no operands)
%%%
%%% From ARCHITECTURE.md:1097-1116 and PROMPT.md:162-166
%%% @end

-module(wf_vm).
...
```

##### 3. Update wf_substrate.app.src

**File**: `src/wf_substrate.app.src`
**Changes**: Ensure all modules listed

```erlang
{modules, [wf_vm, wf_compile, wf_substrate_app, wf_substrate_sup, wf_substrate]},
```

##### 4. Run final verification

**File**: `test/wf_compile_tests.erl`
**Changes**: Add final integration test

```erlang
compiler_integration_test_() ->
    {"Verify complete compiler with complex workflow",
     fun() ->
         %% Complex workflow: loop(seq(par([a, b]), c), cancel(scope, xor([d, e])))
         Term = {loop, {count, 2},
                  {seq,
                   {par, [
                       {task, task_a, mock_task_metadata()},
                       {task, task_b, mock_task_metadata()}
                   ]},
                   {cancel, my_scope,
                    {xor, [
                        {task, task_c, mock_task_metadata()},
                        {task, task_d, mock_task_metadata()}
                    ]}}
                  }},
         {ok, Bytecode} = wf_compile:compile(Term),
         %% Verify bytecode is flat list
         ?assert(is_list(Bytecode)),
         %% Verify no nested lists
         ?assertNot(lists:any(fun is_list/1, Bytecode)),
         %% Verify no label markers remain
         ?assertNot(lists:any(fun({label, _}) -> true; (_) -> false end, Bytecode)),
         %% Verify all opcodes are valid
         ?assert(lists:all(fun
             ({SEQ_ENTER, _}) -> true;
             ({SEQ_NEXT, _}) -> true;
             ({PAR_FORK, _}) -> true;
             ({JOIN_WAIT, _}) -> true;
             ({XOR_CHOOSE, _}) -> true;
             ({LOOP_BACK, _}) -> true;
             ({LOOP_CHECK, _}) -> true;
             ({CANCEL_SCOPE, _}) -> true;
             ({MI_SPAWN, _}) -> true;
             ({TASK_EXEC, _}) -> true;
             ({DONE}) -> true;
             (_) -> false
         end, Bytecode))
     end}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with zero warnings
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] `rebar3 eunit` passes all tests (100% success rate)
- [ ] Code coverage > 90% for wf_compile.erl and wf_vm.erl
- [ ] `rebar3 xref` shows no undefined function calls
- [ ] Documentation is complete with examples

##### Manual Verification:

- [ ] Module docs clearly explain compilation pipeline
- [ ] Function docs have -spec and examples
- [ ] Bytecode output matches ARCHITECTURE.md:507-589 example
- [ ] All 9 kernel primitives compile correctly
- [ ] Label resolution works for deeply nested workflows
- [ ] Error messages are descriptive

**Note**: This is the final phase. All phases complete, compiler is production-ready.

---

## Testing Strategy

### Unit Tests:

Each compile_* function tested in isolation:
- **compile_task**: Verify TASK_EXEC emitted, name correct
- **compile_seq**: Verify SEQ_ENTER, SEQ_NEXT with label, both branches
- **compile_par**: Verify PAR_FORK with branch labels, each branch ends with DONE, JOIN_WAIT all
- **compile_xor**: Verify XOR_CHOOSE with alt labels, each alt ends with DONE, no JOIN_WAIT
- **compile_join**: Verify PAR_FORK, branches, JOIN_WAIT with policy
- **compile_loop**: Verify LOOP_CHECK, LOOP_BACK with label, exit label
- **compile_cancel**: Verify CANCEL_SCOPE enter/exit, body between them
- **compile_mi**: Verify MI_SPAWN, body, DONE, JOIN_WAIT all

### Integration Tests:

Nested combinations verify label resolution:
- **seq(par)**: SEQ_ENTER, PAR_FORK, branches, JOIN_WAIT, SEQ_NEXT, right branch
- **par(seq)**: PAR_FORK, sequences with SEQ_ENTER/SEQ_NEXT, DONE per seq, JOIN_WAIT
- **loop(par)**: LOOP_CHECK, PAR_FORK, branches, JOIN_WAIT, LOOP_BACK
- **cancel(xor)**: CANCEL_SCOPE enter, XOR_CHOOSE, alts, CANCEL_SCOPE exit

### Label Resolution Tests:

Verify label management:
- **Unique labels**: make_ref() generates unique refs
- **Label map build**: Correctly maps labels to IPs
- **Label replacement**: All label refs replaced with integers
- **Label removal**: {label, Ref} markers removed from final bytecode
- **Unresolved detection**: validate_bytecode catches unresolved labels

### Validation Tests:

Verify error handling:
- **Branch count**: par/xor/join with < 2 branches throws badarg
- **Policy format**: Invalid join/mi/loop policies throw badarg
- **Label resolution**: Unresolved labels after resolution return error

### Property-Based Tests:

Generate random workflows:
- Random wf_term() within depth limit
- Compile and verify no unresolved labels
- Assert bytecode is flat list (no nested structures)
- Assert all opcodes are valid opcode() type

### Manual Testing Steps:

1. **Compile simple workflow**: seq(task(a), task(b)) and inspect bytecode
2. **Compile parallel workflow**: par([task(a), task(b)]) and verify PAR_FORK targets
3. **Compile loop workflow**: loop({count, 3}, task(a)) and verify LOOP_BACK target
4. **Compile complex nested workflow**: seq(par([a, b]), loop({count, 2}, c)) and verify all labels resolved
5. **Verify error handling**: Attempt to compile par([a]) (single branch) and verify badarg thrown

## Migration Notes

No migration needed. This is a green-field implementation. wf_term() from item 002 must exist first. Compiler depends on type definitions from wf_term.erl and wf_vm.erl.

## References

- Primary specification: `/Users/speed/wf-substrate/PROMPT.md:23-27, 97-125, 162-166`
- Architecture: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:309-590, 1097-1116`
- Semantics: `/Users/speed/wf-substrate/docs/SEMANTICS.md:238-1090`
- Research: `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/research.md`
- Item 002 (dependency): `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/`
- Item 002 PRD: `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/prd.json`
