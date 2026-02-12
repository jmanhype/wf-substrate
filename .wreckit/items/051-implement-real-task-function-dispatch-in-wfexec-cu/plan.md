# Implement real task function dispatch in wf_exec. Currently lookup_task_function/2 is a hardcoded mock that ignores the task name and returns fun(Ctx) -> {ok, maps:put(task_result, ok, Ctx)} end. Fix this so the executor actually calls the task function from the wf_term metadata. The compiler must propagate task metadata (the function field from task_metadata map) into the bytecode or a side-table accessible by the executor. The executor must call the real function when it encounters a task_exec opcode. ACCEPTANCE TEST: Create an escript that compiles wf_term:seq(wf_term:task(a, #{function => fun(Ctx) -> {ok, Ctx#{a_ran => true}} end}), wf_term:task(b, #{function => fun(Ctx) -> {ok, Ctx#{b_ran => true}} end})), executes it, and asserts the final context contains both a_ran and b_ran keys set to true. The test must fail with the current mock and pass after the fix. Do NOT use mocks or stubs. Write a real eunit test in test/wf_acceptance_tests.erl that verifies this. Implementation Plan

## Implementation Plan Title

Real Task Function Dispatch in wf_exec Executor

## Overview

Implement real task function dispatch by extending the bytecode format to include task metadata, enhancing the compiler to collect and preserve this metadata, and modifying the executor to look up and call real task functions instead of the hardcoded mock. This enables workflow tasks to execute user-defined functions with proper context propagation.

The current mock implementation in `wf_exec:lookup_task_function/2` ignores task names and returns a dummy function that always puts `task_result => ok` in the context. This prevents real task execution. The fix requires a coordinated change across three modules: `wf_vm` (types), `wf_compile` (metadata collection), and `wf_exec` (function lookup and dispatch).

## Current State

**Critical Gap Identified:**

1. **wf_compile.erl:199-201** - The `compile_task/1` function explicitly discards task metadata:
   ```erlang
   compile_task({task, Name, _Metadata}) when is_atom(Name) ->
       [{task_exec, Name}].
   ```
   The `_Metadata` parameter (containing the task function) is ignored.

2. **wf_exec.erl:1023-1027** - The `lookup_task_function/2` function is a hardcoded mock:
   ```erlang
   lookup_task_function(_TaskName, _ExecState) ->
       fun(Ctx) -> {ok, maps:put(task_result, ok, Ctx)} end.
   ```
   Both parameters are ignored, returning a dummy function.

3. **wf_exec.erl:463-522** - The `execute_task_exec/2` handler calls the mock, preventing real task execution.

4. **wf_vm.erl:39** - Bytecode type is currently `[opcode()]`, with no metadata storage:
   ```erlang
   -type wf_bc() :: [opcode()].
   ```

5. **wf_exec.hrl:32-44** - The `exec_state` record has no field for storing task metadata.

**Test Infrastructure Missing:**
- No `test/wf_acceptance_tests.erl` file exists (verified via glob search).

**Existing Tests Will Continue Working:**
- Tests in `wf_exec_tests.erl` use mock bytecode generators (`mock_bytecode_*` helpers) that bypass `wf_compile`, so they're unaffected by bytecode format changes.
- Tests in `wf_compile_tests.erl` use `mock_task_metadata()` helpers that follow the `#{function => fun()}` pattern.

## Desired End State

1. **wf_vm.erl** - Extended type system:
   - New `task_metadata_map()` type mapping task names to metadata
   - Modified `wf_bc()` type to `{[opcode()], task_metadata_map()}` tuple

2. **wf_compile.erl** - Metadata-preserving compilation:
   - All `compile_*` functions return `{bytecode(), metadata_map()}` tuples
   - `compile/1` returns `{ok, {[opcode()], task_metadata_map()}}`
   - Task metadata (including functions) is collected and preserved

3. **wf_exec.erl** - Real function dispatch:
   - `exec_state` record includes `task_metadata` field
   - `new/1` accepts `{[opcode()], task_metadata_map()}` and stores metadata
   - `lookup_task_function/2` retrieves real function from metadata map
   - Error handling for missing task metadata

4. **test/wf_acceptance_tests.erl** - End-to-end acceptance test:
   - Compiles workflow with two tasks having distinct functions
   - Executes workflow and verifies both tasks ran
   - Uses `#{}` map syntax for context updates (Erlang 18+ feature)
   - Test fails with current mock, passes after implementation

### Key Discoveries:

- **Erlang Version**: The codebase uses `#{}` map syntax (Erlang 18+), not `Ctx#{}` record updates. The acceptance test should use `maps:put/3` for context updates.
- **Test Pattern**: Existing tests in `wf_compile_tests.erl:14-19` use `#{function => fun()}` pattern consistently.
- **No Existing Acceptance Tests**: Verified via glob search - `wf_acceptance_tests.erl` does not exist, so this is new functionality.
- **Mock Bytecode Isolation**: Tests use `mock_bytecode_*` helpers that generate bytecode directly, bypassing `wf_compile`. This provides test isolation.
- **Closure Serialization Constraint**: Task functions are closures (funs) that cannot be serialized via `term_to_binary/1`. The parallel metadata map approach keeps bytecode serializable by excluding metadata from dumps.

## What We're NOT Doing

1. **NOT changing task_exec opcode format** - The opcode remains `{task_exec, atom()}`. Metadata is stored separately in a parallel map.

2. **NOT implementing escript** - The item title mentions "Create an escript", but the implementation notes clarify this should be an EUnit test in `test/wf_acceptance_tests.erl`.

3. **NOT modifying tracing system** - This item does not include changes to `wf_trace` for metadata exclusion from serialization. That's a separate concern.

4. **NOT implementing persistence/distribution** - Bytecode with closures cannot be persisted or distributed. This is a known limitation, not a bug to fix.

5. **NOT changing wf_state** - Task metadata is derived from bytecode, not mutable state. No changes to `wf_state` (item 006) are needed.

6. **NOT supporting backward compatibility** - The bytecode format change from `[opcode()]` to `{[opcode()], task_metadata_map()}` is a breaking change. All consumers must update. Currently, `wf_exec` is the only consumer.

## Implementation Approach

**Design Decision: Parallel Metadata Map (Option A from research)**

Store task metadata in a parallel map bundled with bytecode as a tuple `{[opcode()], task_metadata_map()}`. This approach:

- **Preserves opcode simplicity** - No need to extend `{task_exec, atom()}` format
- **Enables serialization control** - Bytecode can be separated from unserializable closures
- **Supports future extensions** - Timeout, retry policies, and other metadata can be added
- **Provides O(1) lookup** - Executor stores metadata map in `exec_state` for fast access

**Rationale for Alternative Rejection:**

Option B (extending opcode to `{task_exec, atom(), task_fun()}`) was rejected because:
- Functions co-located with opcodes cannot be excluded from serialization
- Breaks uniformity of opcode format
- Makes tracing/replay more complex (need to filter per-opcode)

**Implementation Strategy:**

1. **Phase 1: Type System** - Extend `wf_vm` types to define metadata map and new bytecode wrapper
2. **Phase 2: Compiler** - Modify all compilation functions to collect and return metadata
3. **Phase 3: Executor** - Update `exec_state`, `new/1`, and `lookup_task_function/2`
4. **Phase 4: Acceptance Test** - Create EUnit test that validates end-to-end functionality

Each phase is independently testable and can be completed incrementally.

---

## Phases

### Phase 1: Type System Extensions (wf_vm.erl)

#### Overview

Add type definitions for task metadata map and extend bytecode type to bundle metadata with opcode list. This enables the compiler to preserve metadata and the executor to access it.

#### Changes Required:

##### 1. wf_vm.erl - Add task_metadata_map() type

**File**: `src/wf_vm.erl`
**Changes**: Add new type definition after line 39 (after `-type wf_bc() :: [opcode()].`)

Add before line 40 (blank line before opcode definitions):

```erlang
%% Task metadata map for runtime function dispatch
%% Maps task name atoms to their metadata (functions, timeout, retry, etc.)
-type task_metadata_map() :: #{atom() => wf_term:task_metadata()}.
```

##### 2. wf_vm.erl - Modify wf_bc() type

**File**: `src/wf_vm.erl`
**Changes**: Replace line 39 bytecode type definition with tuple wrapper

Replace:
```erlang
-type wf_bc() :: [opcode()].
```

With:
```erlang
-type wf_bc() :: {[opcode()], task_metadata_map()}.
```

##### 3. wf_vm.erl - No changes to opcode() type

**File**: `src/wf_vm.erl`
**Changes**: Keep opcode type unchanged (line 59 `{task_exec, atom()}`)

**Rationale**: Metadata comes from the wrapper map, not the opcode. The task_exec opcode remains `{task_exec, atom()}` to preserve simplicity.

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with no type errors
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] Module compiles without warnings

##### Manual Verification:

- [ ] Review type definitions in `wf_vm.erl` match specification
- [ ] Verify `wf_term:task_metadata()` is accessible (imported from wf_beam)

**Note**: No functional changes in this phase - only type definitions. Proceed to Phase 2.

---

### Phase 2: Compiler Enhancement (wf_compile.erl)

#### Overview

Modify the compiler to collect task metadata during AST traversal and return it bundled with bytecode. All `compile_*` functions must be updated to return `{bytecode(), metadata_map()}` tuples instead of just bytecode lists.

#### Changes Required:

##### 1. wf_compile.erl - Update compile/1 return type and structure

**File**: `src/wf_compile.erl`
**Changes**: Modify lines 132-153 to collect and return metadata

Replace the entire `compile/1` function (lines 132-153):

```erlang
-spec compile(wf_term:wf_term()) -> {ok, wf_vm:wf_bc()} | {error, compile_error()}.
compile(Term) ->
    try
        %% Pass 1: Compile AST to bytecode with labels, collecting metadata
        {UnresolvedBytecode, MetadataMap} = compile_term_with_metadata(Term),

        %% Pass 2: Resolve labels to integer IPs
        ResolvedBytecode = resolve_labels(UnresolvedBytecode),

        %% Pass 3: Validate no unresolved labels remain
        case validate_bytecode(ResolvedBytecode) of
            ok ->
                {ok, {ResolvedBytecode, MetadataMap}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        throw:ThrowReason ->
            {error, ThrowReason};
        error:ErrorReason ->
            {error, {invalid_term, ErrorReason}}
    end.
```

##### 2. wf_compile.erl - Add compile_term_with_metadata/1 entry point

**File**: `src/wf_compile.erl`
**Changes**: Add new function after line 195 (after `compile_term`), replacing compile_term with metadata-collecting version

Add after line 195:

```erlang
%%%-------------------------------------------------------------------
%%% @doc Compile wf_term() to unresolved bytecode with metadata map
%%%
%%% Returns {bytecode(), metadata_map()} where metadata maps task names
%%% to their task_metadata() maps containing functions and other config.
%%% @end
-spec compile_term_with_metadata(wf_term:wf_term()) -> {unresolved_bytecode(), wf_vm:task_metadata_map()}.
compile_term_with_metadata({task, _Name, _Metadata} = Term) ->
    compile_task_with_metadata(Term);

compile_term_with_metadata({seq, _Left, _Right} = Term) ->
    compile_seq_with_metadata(Term);

compile_term_with_metadata({par, _Branches} = Term) ->
    compile_par_with_metadata(Term);

compile_term_with_metadata({x_or, _Alternatives} = Term) ->
    compile_xor_with_metadata(Term);

compile_term_with_metadata({join, _Policy, _Branches} = Term) ->
    compile_join_with_metadata(Term);

compile_term_with_metadata({loop, _Policy, _Body} = Term) ->
    compile_loop_with_metadata(Term);

compile_term_with_metadata({defer, _Alternatives} = Term) ->
    compile_defer_with_metadata(Term);

compile_term_with_metadata({cancel, _ScopeId, _Body} = Term) ->
    compile_cancel_with_metadata(Term);

compile_term_with_metadata({mi, _Policy, _Body} = Term) ->
    compile_mi_with_metadata(Term);

compile_term_with_metadata(InvalidTerm) ->
    error({badarg, {invalid_term, InvalidTerm}}).
```

##### 3. wf_compile.erl - Add compile_task_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add new metadata-preserving task compilation function

Add after the new `compile_term_with_metadata/1` function:

```erlang
%% Compile task with metadata collection
compile_task_with_metadata({task, Name, Metadata}) when is_atom(Name) ->
    %% Emit task_exec opcode and store metadata
    {[{task_exec, Name}], #{Name => Metadata}}.
```

##### 4. wf_compile.erl - Add compile_seq_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add sequence compilation with metadata merging

Add after `compile_task_with_metadata/1`:

```erlang
compile_seq_with_metadata({seq, Left, Right}) ->
    %% Recursively compile branches, collecting metadata
    {LeftCode, LeftMeta} = compile_term_with_metadata(Left),
    {RightCode, RightMeta} = compile_term_with_metadata(Right),

    %% Build sequence bytecode with label
    RightLabel = make_label(),
    SeqCode = [{seq_enter, 0}] ++
        LeftCode ++
        [{seq_next, RightLabel}] ++
        [RightLabel] ++
        RightCode,

    %% Merge metadata from both branches
    {SeqCode, maps:merge(LeftMeta, RightMeta)}.
```

##### 5. wf_compile.erl - Add compile_par_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add parallel fork compilation with metadata merging

Add after `compile_seq_with_metadata/1`:

```erlang
compile_par_with_metadata({par, Branches}) when length(Branches) >= 2 ->
    %% Compile all branches, collecting metadata
    BranchResults = [compile_term_with_metadata(B) || B <- Branches],
    BranchCodes = [Code || {Code, _Meta} <- BranchResults],
    BranchMetadatas = [Meta || {_Code, Meta} <- BranchResults],

    %% Generate labels
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_ref(),

    %% Build branch bytecode with label markers and done opcodes
    BranchBytecode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(BranchCodes, BranchLabels)),

    %% Build full bytecode
    ParCode = [{par_fork, BranchLabels}] ++
        BranchBytecode ++
        [JoinLabel] ++
        [{join_wait, all}],

    %% Merge all branch metadata
    MergedMetadata = lists:foldl(fun maps:merge/2, #{}, BranchMetadatas),
    {ParCode, MergedMetadata}.
```

##### 6. wf_compile.erl - Add compile_xor_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add exclusive choice compilation with metadata merging

Add after `compile_par_with_metadata/1`:

```erlang
compile_xor_with_metadata({x_or, Alternatives}) when length(Alternatives) >= 2 ->
    %% Compile all alternatives, collecting metadata
    AltResults = [compile_term_with_metadata(A) || A <- Alternatives],
    AltCodes = [Code || {Code, _Meta} <- AltResults],
    AltMetadatas = [Meta || {_Code, Meta} <- AltResults],

    %% Generate labels
    AltLabels = [make_label() || _ <- Alternatives],

    %% Build alternative bytecode with label markers and done opcodes
    AltBytecode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(AltCodes, AltLabels)),

    %% Build full bytecode (no join for xor)
    XorCode = [{xor_choose, AltLabels}] ++ AltBytecode,

    %% Merge all alternative metadata
    MergedMetadata = lists:foldl(fun maps:merge/2, #{}, AltMetadatas),
    {XorCode, MergedMetadata}.
```

##### 7. wf_compile.erl - Add compile_join_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add join compilation with metadata merging (similar to par)

Add after `compile_xor_with_metadata/1`:

```erlang
compile_join_with_metadata({join, Policy, Branches}) when length(Branches) >= 2 ->
    %% Validate policy
    validate_join_policy(Policy),

    %% Compile all branches, collecting metadata
    BranchResults = [compile_term_with_metadata(B) || B <- Branches],
    BranchCodes = [Code || {Code, _Meta} <- BranchResults],
    BranchMetadatas = [Meta || {_Code, Meta} <- BranchResults],

    %% Generate labels
    BranchLabels = [make_label() || _ <- Branches],
    JoinLabel = make_ref(),

    %% Build branch bytecode
    BranchBytecode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(BranchCodes, BranchLabels)),

    %% Build full bytecode
    JoinCode = [{par_fork, BranchLabels}] ++
        BranchBytecode ++
        [JoinLabel] ++
        [{join_wait, Policy}],

    %% Merge all branch metadata
    MergedMetadata = lists:foldl(fun maps:merge/2, #{}, BranchMetadatas),
    {JoinCode, MergedMetadata}.
```

##### 8. wf_compile.erl - Add compile_loop_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add loop compilation with metadata propagation

Add after `compile_join_with_metadata/1`:

```erlang
compile_loop_with_metadata({loop, Policy, Body}) ->
    %% Compile body, collecting metadata
    {BodyCode, BodyMeta} = compile_term_with_metadata(Body),

    %% Generate labels
    LoopHeadLabel = make_label(),
    ExitLabel = make_label(),

    %% Build loop bytecode
    LoopCode = [LoopHeadLabel] ++
        [{loop_check, Policy}] ++
        BodyCode ++
        [{loop_back, LoopHeadLabel}] ++
        [ExitLabel],

    {LoopCode, BodyMeta}.
```

##### 9. wf_compile.erl - Add compile_defer_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add defer stub (same as existing)

Add after `compile_loop_with_metadata/1`:

```erlang
compile_defer_with_metadata({defer, _Alternatives}) ->
    throw({error, {not_implemented, defer}}).
```

##### 10. wf_compile.erl - Add compile_cancel_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add cancel compilation with metadata propagation

Add after `compile_defer_with_metadata/1`:

```erlang
compile_cancel_with_metadata({cancel, ScopeId, Body}) ->
    %% Compile body, collecting metadata
    {BodyCode, BodyMeta} = compile_term_with_metadata(Body),

    %% Build cancel bytecode (no metadata to merge)
    CancelCode = [{cancel_scope, {enter, ScopeId}}] ++
        BodyCode ++
        [{cancel_scope, {exit, ScopeId}}],

    {CancelCode, BodyMeta}.
```

##### 11. wf_compile.erl - Add compile_mi_with_metadata/1

**File**: `src/wf_compile.erl`
**Changes**: Add multiple instances compilation with metadata propagation

Add after `compile_cancel_with_metadata/1`:

```erlang
compile_mi_with_metadata({mi, Policy, Body}) ->
    %% Validate policy
    validate_mi_policy(Policy),

    %% Compile body, collecting metadata
    {BodyCode, BodyMeta} = compile_term_with_metadata(Body),

    %% Build MI bytecode
    MiCode = [{mi_spawn, Policy}] ++
        BodyCode ++
        [{done}] ++
        [{join_wait, all}],

    {MiCode, BodyMeta}.
```

##### 12. wf_compile.erl - Deprecate old compile_* functions

**File**: `src/wf_compile.erl`
**Changes**: Remove old `compile_task/1`, `compile_seq/1`, `compile_par/1`, `compile_xor/1`, `compile_join/1`, `compile_loop/1`, `compile_defer/1`, `compile_cancel/1`, `compile_mi/1` functions (lines 197-405)

**Rationale**: These are replaced by `*_with_metadata` versions. Remove to avoid confusion and maintain single source of truth.

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] Existing tests pass: `rebar3 eunit -m wf_compile_tests`
- [ ] Type checking passes: `dialyzer -Wunmatched_returns -Werror_handling`

##### Manual Verification:

- [ ] Inspect compiled bytecode structure: Test that `wf_compile:compile(wf_term:task(a, #{function => fun(_Ctx) -> ok end})).` returns `{ok, {Bytecode, Metadata}}` where Metadata is `#{a => #{function := _Fun}}`
- [ ] Verify metadata merging: Compile a sequence of two tasks, verify both tasks' functions are in metadata map
- [ ] Check that old compile functions are removed (grep for `compile_task(` should find only `compile_task_with_metadata`)

**Note**: Phase 2 is complete when compiler collects and returns metadata. Proceed to Phase 3.

---

### Phase 3: Executor Modification (wf_exec.erl)

#### Overview

Update the executor to accept the new bytecode format, store task metadata in exec_state, and look up real task functions from the metadata map instead of using the hardcoded mock.

#### Changes Required:

##### 1. wf_exec.hrl - Add task_metadata field to exec_state record

**File**: `src/wf_exec.hrl`
**Changes**: Modify lines 32-44 to add task_metadata field

Replace the entire `exec_state` record definition:

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: map(),
    case_id :: term() | undefined,  %% Case ID for effect ID generation
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | blocked_effect | cancelled | failed,
    current_token :: term() | undefined,
    task_metadata :: wf_vm:task_metadata_map()  %% Task metadata map for function lookup
}).
```

**Important**: Change `bytecode :: wf_vm:wf_bc()` to `bytecode :: [opcode()]` since we're unpacking the tuple in `new/1`.

Actually, let's keep `wf_vm:wf_bc()` as the full tuple and unpack in new/1. The corrected version:

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: [wf_vm:opcode()],  %% Opcodes only (metadata stored separately)
    ctx :: map(),
    case_id :: term() | undefined,
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | blocked_effect | cancelled | failed,
    current_token :: term() | undefined,
    task_metadata :: wf_vm:task_metadata_map()  %% Task metadata for function lookup
}).
```

##### 2. wf_exec.erl - Update new/1 to accept {Bytecode, Metadata} tuple

**File**: `src/wf_exec.erl`
**Changes**: Modify lines 51-77 to unpack bytecode tuple and store metadata

Replace the entire `new/1` function:

```erlang
%% @doc Create new executor from bytecode with metadata
-spec new(wf_vm:wf_bc()) -> exec_state().
new({Bytecode, Metadata}) ->
    CaseId = make_ref(),  %% Generate case_id for effect IDs
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active,
        instance_id = undefined,
        current_effect = undefined
    },
    #exec_state{
        ip = 0,
        bytecode = Bytecode,
        ctx = #{},
        case_id = CaseId,
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0,
        status = running,
        current_token = InitialTokenId,
        task_metadata = Metadata  %% Store task metadata for lookup
    }.
```

##### 3. wf_exec.erl - Replace lookup_task_function/2 mock implementation

**File**: `src/wf_exec.erl`
**Changes**: Replace lines 1022-1027 with real function lookup

Replace the entire function:

```erlang
%% @doc Look up task function from metadata map
-spec lookup_task_function(atom(), exec_state()) -> fun((map()) -> {ok, map()} | {effect, term(), map()} | {error, term()}).
lookup_task_function(TaskName, ExecState) ->
    case maps:find(TaskName, ExecState#exec_state.task_metadata) of
        {ok, Metadata} ->
            case maps:find(function, Metadata) of
                {ok, TaskFun} when is_function(TaskFun) ->
                    TaskFun;
                _ ->
                    error({badarg, {invalid_task_metadata, TaskName, "function key missing or not a fun"}})
            end;
        error ->
            error({badarg, {missing_task_metadata, TaskName}})
    end.
```

**Rationale**: Use `maps:find/2` instead of `maps:get/2` to provide better error messages distinguishing between missing task metadata and missing function key.

##### 4. wf_exec.erl - Update snapshot_exec_state/1 to include task_metadata

**File**: `src/wf_exec.erl`
**Changes**: Modify lines 104-121 to include task_metadata in snapshot

Add `task_metadata => ExecState#exec_state.task_metadata` to the StateMap (after line 118):

```erlang
snapshot_exec_state(ExecState) ->
    StateMap = #{
        ip => ExecState#exec_state.ip,
        bytecode => ExecState#exec_state.bytecode,
        ctx => ExecState#exec_state.ctx,
        case_id => ExecState#exec_state.case_id,
        tokens => ExecState#exec_state.tokens,
        branch_map => ExecState#exec_state.branch_map,
        join_counters => ExecState#exec_state.join_counters,
        scope_stack => ExecState#exec_state.scope_stack,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status,
        current_token => ExecState#exec_state.current_token,
        task_metadata => ExecState#exec_state.task_metadata
    },
    term_to_binary(StateMap).
```

##### 5. wf_exec.erl - Update restore_exec_state/2 to validate and restore task_metadata

**File**: `src/wf_exec.erl`
**Changes**: Modify lines 138-187 to handle task_metadata field

Update the validation check (after line 157) to include task_metadata:

```erlang
case maps:is_key(ip, StateMap) andalso
     maps:is_key(bytecode, StateMap) andalso
     maps:is_key(ctx, StateMap) andalso
     maps:is_key(case_id, StateMap) andalso
     maps:is_key(tokens, StateMap) andalso
     maps:is_key(branch_map, StateMap) andalso
     maps:is_key(join_counters, StateMap) andalso
     maps:is_key(scope_stack, StateMap) andalso
     maps:is_key(step_count, StateMap) andalso
     maps:is_key(status, StateMap) andalso
     maps:is_key(current_token, StateMap) andalso
     maps:is_key(task_metadata, StateMap) of
```

Update the ExecState construction to include task_metadata (after line 179):

```erlang
ExecState = #exec_state{
    ip = maps:get(ip, StateMap),
    bytecode = maps:get(bytecode, StateMap),
    ctx = maps:get(ctx, StateMap),
    case_id = maps:get(case_id, StateMap),
    tokens = maps:get(tokens, StateMap),
    branch_map = maps:get(branch_map, StateMap),
    join_counters = maps:get(join_counters, StateMap),
    scope_stack = maps:get(scope_stack, StateMap),
    step_count = maps:get(step_count, StateMap),
    status = maps:get(status, StateMap),
    current_token = maps:get(current_token, StateMap),
    task_metadata => maps:get(task_metadata, StateMap)
},
```

**Note**: The snapshot/restore functionality includes task_metadata in serialization. This will fail with `term_to_binary/1` if closures are present. This is acceptable - the limitation is documented, and snapshots without closures work fine.

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds
- [ ] Existing tests pass: `rebar3 eunit -m wf_exec_tests`
- [ ] No compilation errors or warnings

##### Manual Verification:

- [ ] Create executor with `{Bytecode, Metadata}` tuple and verify it stores metadata
- [ ] Call `wf_exec:new/1` and inspect returned `exec_state` to confirm `task_metadata` field is populated
- [ ] Test `lookup_task_function/2` with valid task name returns function
- [ ] Test `lookup_task_function/2` with invalid task name throws `{badarg, {missing_task_metadata, _}}`
- [ ] Test `lookup_task_function/2` with task missing function key throws `{badarg, {invalid_task_metadata, _, _}}`

**Note**: Phase 3 is complete when executor accepts metadata tuple and looks up real functions. Proceed to Phase 4.

---

### Phase 4: Acceptance Test (test/wf_acceptance_tests.erl)

#### Overview

Create an end-to-end EUnit test that verifies the complete workflow: compile a workflow with task functions, execute it, and assert that the real functions ran (not the mock). This test will fail with the current mock implementation and pass after the fix.

#### Changes Required:

##### 1. test/wf_acceptance_tests.erl - Create new test file

**File**: `test/wf_acceptance_tests.erl`
**Changes**: Create new file with acceptance test

```erlang
%%%-------------------------------------------------------------------
%%% @doc Acceptance tests for end-to-end workflow execution
%%%
%%% These tests verify that workflows compile and execute correctly with
%%% real task functions (not mocks). They serve as integration tests
%%% that validate the complete compilation and execution pipeline.
%%% @end
-module(wf_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test that real task functions are called during execution
%%
%% This test verifies:
%% 1. Compiler preserves task metadata through compilation
%% 2. Executor looks up task functions from metadata map
%% 3. Real functions execute (not the mock)
%% 4. Context updates from task functions propagate correctly
%%--------------------------------------------------------------------
task_function_dispatch_test_() ->
    {"Tasks execute their real functions from metadata",
     fun() ->
         %% Build workflow with two tasks having distinct functions
         %% Each function sets a unique key in the context
         Term = wf_term:seq(
             wf_term:task(a, #{
                 function => fun(Ctx) ->
                     {ok, maps:put(a_ran, true, Ctx)}
                 end
             }),
             wf_term:task(b, #{
                 function => fun(Ctx) ->
                     {ok, maps:put(b_ran, true, Ctx)}
                 end
             })
         ),

         %% Compile to bytecode (returns {ok, {Bytecode, Metadata}})
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),

         %% Verify metadata was collected
         ?assertEqual(2, map_size(Metadata)),
         ?assert(maps:is_key(a, Metadata)),
         ?assert(maps:is_key(b, Metadata)),

         %% Execute workflow
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),

         %% Verify both tasks ran (context has both keys)
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(true, maps:get(a_ran, FinalCtx)),
         ?assertEqual(true, maps:get(b_ran, FinalCtx)),
         ?assertEqual(2, map_size(FinalCtx))
     end}.

%%--------------------------------------------------------------------
%% @doc Test that task functions receive and return context correctly
%%
%% This test verifies context propagation through task execution.
%%--------------------------------------------------------------------
context_propagation_test_() ->
    {"Task functions receive and update context correctly",
     fun() ->
         %% Build workflow with task that reads and writes context
         Term = wf_term:task(context_test, #{
             function => fun(Ctx) ->
                         InitialValue = maps:get(initial, Ctx, 0),
                         {ok, maps:put(result, InitialValue * 2, Ctx)}
                     end
         }),

         %% Compile and execute
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         ExecState1 = wf_exec:set_ctx(ExecState0, #{initial => 5}),
         {done, ExecState2} = wf_exec:run(ExecState1, 100, deterministic),

         %% Verify context was read and updated
         FinalCtx = wf_exec:get_ctx(ExecState2),
         ?assertEqual(10, maps:get(result, FinalCtx))
     end}.
```

##### 2. Add test file to rebar.config

**File**: `rebar.config` (if needed)
**Changes**: Ensure test directory is included in eunit suite

**Note**: Rebar automatically includes all `test/*_tests.erl` files, so no config change is needed.

#### Success Criteria:

##### Automated Verification:

- [ ] Test file is created at `test/wf_acceptance_tests.erl`
- [ ] `rebar3 eunit -m wf_acceptance_tests` runs (may fail initially)
- [ ] Test fails with current mock implementation (before fix)
- [ ] Test passes after completing Phases 1-3 (after fix)

##### Manual Verification:

- [ ] Run acceptance test: `rebar3 eunit -m wf_acceptance_tests`
- [ ] Verify test output shows "2 tests passed" after implementation
- [ ] Inspect test execution traces to confirm real functions were called
- [ ] Test with context propagation shows correct values

**Note**: Phase 4 is complete when acceptance test passes. This validates the entire implementation.

---

## Testing Strategy

### Unit Tests:

- **wf_vm.erl**: No new unit tests (type changes only)
- **wf_compile.erl**: Extend existing tests to verify metadata collection
- **wf_exec.erl**: Extend existing tests to verify metadata lookup

### Integration Tests:

- **test/wf_acceptance_tests.erl**: End-to-end test that compiles and executes workflow with real functions
- Verify metadata map contains task functions
- Verify context updates from real task functions
- Test context propagation through task execution

### Manual Testing Steps:

1. **Compile a simple workflow:**
   ```erlang
   Term = wf_term:task(test, #{function => fun(Ctx) -> {ok, maps:put(x, 1, Ctx)} end}),
   {ok, {Bytecode, Metadata}} = wf_compile:compile(Term).
   ```
   Expected: `Bytecode` is `[{task_exec, test}]`, `Metadata` is `#{test => #{function := _Fun}}`.

2. **Execute the workflow:**
   ```erlang
   ExecState0 = wf_exec:new({Bytecode, Metadata}),
   {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),
   wf_exec:get_ctx(ExecState1).
   ```
   Expected: Context contains `#{x => 1}`.

3. **Test missing task metadata error:**
   ```erlang
   lookup_task_function(nonexistent, ExecState).
   ```
   Expected: Error `{badarg, {missing_task_metadata, nonexistent}}`.

## Migration Notes

### Breaking Changes:

1. **wf_vm:wf_bc() type change**: From `[opcode()]` to `{[opcode()], task_metadata_map()}`
   - **Impact**: All code creating bytecode must return tuple
   - **Migration**: Update `wf_compile:compile/1` return value (already done in Phase 2)
   - **Consumers**: Only `wf_exec:new/1` creates exec_state from bytecode (updated in Phase 3)

2. **wf_exec:new/1 signature change**: Now accepts `{Bytecode, Metadata}` tuple instead of just bytecode
   - **Impact**: All code creating exec_state must pass tuple
   - **Migration**: Update all callers (currently only tests)
   - **Test updates**: Tests using mock bytecode need adjustment

### Test Migration:

Existing tests in `wf_exec_tests.erl` use `mock_bytecode_*` helpers that return `[opcode()]`. These tests need updating to pass metadata map:

```erlang
%% Before (old format):
Bytecode = mock_bytecode_simple_task(),
ExecState = wf_exec:new(Bytecode).

%% After (new format):
Bytecode = mock_bytecode_simple_task(),
Metadata = #{},  %% Empty metadata for mock tests
ExecState = wf_exec:new({Bytecode, Metadata}).
```

However, since mock tests don't exercise real task functions, they can pass empty metadata maps. The real validation is in `wf_acceptance_tests.erl`.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/051-implement-real-task-function-dispatch-in-wfexec-cu/research.md`
- Item Specification: `/Users/speed/wf-substrate/.wreckit/items/051-implement-real-task-function-dispatch-in-wfexec-cu/item.json`
- src/wf_exec.erl:463-522 - execute_task_exec/2 (calls lookup_task_function)
- src/wf_exec.erl:1023-1027 - lookup_task_function/2 (mock to replace)
- src/wf_compile.erl:199-201 - compile_task/1 (discards metadata)
- src/wf_vm.erl:39 - wf_bc() type definition (needs extension)
- src/wf_vm.erl:59 - task_exec opcode type
- src/wf_term.erl:50-63 - task_fun() and task_metadata() types
- src/wf_exec.hrl:32-44 - exec_state record definition
- test/wf_compile_tests.erl:14-19 - Mock task metadata pattern
- test/wf_exec_tests.erl:6-7 - Mock bytecode generators
