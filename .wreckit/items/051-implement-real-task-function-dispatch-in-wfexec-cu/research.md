# Research: Implement real task function dispatch in wf_exec. Currently lookup_task_function/2 is a hardcoded mock that ignores the task name and returns fun(Ctx) -> {ok, maps:put(task_result, ok, Ctx)} end. Fix this so the executor actually calls the task function from the wf_term metadata. The compiler must propagate task metadata (the function field from task_metadata map) into the bytecode or a side-table accessible by the executor. The executor must call the real function when it encounters a task_exec opcode. ACCEPTANCE TEST: Create an escript that compiles wf_term:seq(wf_term:task(a, #{function => fun(Ctx) -> {ok, Ctx#{a_ran => true}} end}), wf_term:task(b, #{function => fun(Ctx) -> {ok, Ctx#{b_ran => true}} end})), executes it, and asserts the final context contains both a_ran and b_ran keys set to true. The test must fail with the current mock and pass after the fix. Do NOT use mocks or stubs. Write a real eunit test in test/wf_acceptance_tests.erl that verifies this.

**Date**: 2025-01-11
**Item**: 051-implement-real-task-function-dispatch-in-wfexec-cu

## Research Question

How can the wf_exec executor dispatch to real task functions from wf_term metadata instead of using a hardcoded mock? The compiler must preserve task function metadata through compilation to bytecode, and the executor must look up and call these functions at runtime.

## Summary

The current implementation has a **critical gap**: wf_compile:compile_task/1 at line 199-201 emits only `{task_exec, Name}` opcodes, discarding the task metadata (including the function). Meanwhile, wf_exec:execute_task_exec/2 at line 463-522 calls a mock `lookup_task_function/2` (lines 1023-1027) that returns a hardcoded function `fun(Ctx) -> {ok, maps:put(task_result, ok, Ctx)} end`, completely ignoring the task name.

**The solution requires three coordinated changes**:

1. **Compiler enhancement** (wf_compile.erl): Extend bytecode format to include task metadata alongside task_exec opcodes. Two viable approaches:
   - **Option A**: Store metadata in a parallel map (e.g., `wf_vm:wf_bc()` becomes `{bytecode(), metadata_map()}`)
   - **Option B**: Extend task_exec opcode to `{task_exec, atom(), task_fun()}` (inline function storage)

2. **Executor modification** (wf_exec.erl): Replace mock `lookup_task_function/2` with real lookup that retrieves the function from the compiled metadata structure and calls it.

3. **Type system updates** (wf_vm.erl): Extend type definitions to support the new metadata storage approach.

4. **Acceptance test** (test/wf_acceptance_tests.erl): Create eunit test that compiles a workflow with two tasks having distinct functions, executes it, and verifies both functions ran (checking context has both `a_ran` and `b_ran` keys).

**Key insight from item 005-executor-hot-loop research**: The system was designed for task functions to be stored in bytecode metadata as module:function references. However, since closures (funs) cannot be serialized as module:function references, we must store the funs directly in an in-memory side-table.

## Current State Analysis

### Existing Implementation

**wf_compile.erl:199-201** - `compile_task/1` currently discards metadata:
```erlang
compile_task({task, Name, _Metadata}) when is_atom(Name) ->
    %% Phase 3: Emit task_exec opcode
    [{task_exec, Name}].
```
The `_Metadata` variable is explicitly ignored (prefixed with underscore), and only the task name atom is emitted in the opcode.

**wf_exec.erl:1023-1027** - `lookup_task_function/2` is a hardcoded mock:
```erlang
lookup_task_function(_TaskName, _ExecState) ->
    %% Mock: task always succeeds with ok result
    %% In production, would look up from bytecode metadata
    fun(Ctx) -> {ok, maps:put(task_result, ok, Ctx)} end.
```
Both parameters are ignored, returning a mock function that always puts `task_result => ok` in the context.

**wf_exec.erl:463-522** - `execute_task_exec/2` calls the mock:
```erlang
execute_task_exec({_TaskExec, _TaskName}, ExecState) ->
    %% Mock: look up task function and execute it
    TaskFun = lookup_task_function(_TaskName, ExecState),
    Ctx = ExecState#exec_state.ctx,
    ...
    case TaskFun(Ctx) of
        {ok, NewCtx} -> ...
```
The function returned by the mock is called with the current context.

**wf_vm.erl:59** - Current opcode definition:
```erlang
{task_exec, atom()} |                         %% Execute task (task name operand)
```
Only stores the task name atom, with no metadata field.

**wf_term.erl:58-63** - Task metadata structure:
```erlang
-type task_metadata() :: #{
    function := task_fun(),
    timeout => timeout(),
    retry => non_neg_integer(),
    description => binary()
}.
```
The `function` key is required and contains a `task_fun()`, which is a closure matching `fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, term(), ctx()})`.

## Key Files

- **src/wf_exec.erl:463-522** - `execute_task_exec/2` opcode handler that currently calls mock lookup
- **src/wf_exec.erl:1023-1027** - `lookup_task_function/2` mock implementation (must be replaced)
- **src/wf_exec.erl:32-44** - `exec_state()` record definition (may need metadata field)
- **src/wf_compile.erl:199-201** - `compile_task/1` that discards task metadata
- **src/wf_compile.erl:133-153** - `compile/1` main compilation function (needs to collect metadata)
- **src/wf_vm.erl:59** - `task_exec` opcode type definition (needs extension)
- **src/wf_vm.erl:39** - `wf_bc()` bytecode type definition (may need wrapper)
- **src/wf_term.erl:50-63** - `task_fun()` and `task_metadata()` type definitions (closure signatures)
- **test/wf_compile_tests.erl:14-19** - Mock task metadata helper pattern
- **test/wf_exec_tests.erl:6-7** - Mock bytecode generators (show current expectations)
- **test/wf_acceptance_tests.erl** - [TO BE CREATED] Acceptance test file for end-to-end verification

## Technical Considerations

### Dependencies

- **wf_term.erl** (item 002): Provides `task_metadata()` type with required `function => fun()` key
- **wf_vm.erl** (item 004): Defines bytecode format - must extend to store metadata
- **wf_compile.erl** (item 004): Compiler - must preserve and propagate metadata
- **wf_exec.erl** (item 005): Executor - must lookup and call real functions
- **EUnit**: Testing framework for acceptance test

### Patterns to Follow

**Metadata storage pattern**: Looking at wf_trace.erl:18, metadata is stored as maps. However, task functions are closures that cannot be serialized. This suggests an in-memory side-table approach.

**Current test patterns** (from wf_compile_tests.erl:14-19):
```erlang
mock_task_metadata() ->
    #{function => mock_task_fun()}.
```
Tests already use the `#{function => fun()}` pattern. The acceptance test should follow this structure.

**Opcode dispatch pattern** (from wf_exec.erl:416-439):
Each opcode has a dedicated execute_* function that takes the opcode tuple and exec_state, returns updated exec_state. The task_exec handler should follow this pattern.

**Compiler two-pass pattern** (from wf_compile.erl:136-146):
```erlang
%% Pass 1: Compile AST to bytecode with labels
UnresolvedBytecode = compile_term(Term),

%% Pass 2: Resolve labels to integer IPs
ResolvedBytecode = resolve_labels(UnresolvedBytecode),
```
Metadata collection could be a parallel pass or integrated into existing passes.

### Design Decision: Metadata Storage Approach

Two viable approaches with trade-offs:

**Option A: Parallel Metadata Map** (RECOMMENDED)
- Change `wf_vm:wf_bc()` from `[opcode()]` to `{bytecode(), task_metadata_map()}`
- Compiler builds metadata map during compilation: `#{task_name => task_metadata()}`
- Executor stores metadata map in `exec_state` record
- `lookup_task_function/2` retrieves function from metadata map
- **Pros**: Clean separation, extends opcode format minimally, metadata map can grow for other needs
- **Cons**: Requires changing bytecode wrapper type, updating all bytecode consumers

**Option B: Extended task_exec Opcode**
- Change opcode from `{task_exec, atom()}` to `{task_exec, atom(), task_fun()}`
- Compiler inlines function directly in opcode
- Executor extracts function from opcode tuple
- **Pros**: No bytecode wrapper changes, functions co-located with their opcodes
- **Cons**: Functions cannot be serialized (breaks tracing/replay if bytecode is dumped), opcodes become less uniform

**Recommendation**: Use Option A (parallel metadata map) because:
1. It preserves bytecode serializability (metadata map can be excluded from dumps)
2. It follows the pattern of other metadata in the system (wf_trace uses metadata maps)
3. It allows future extensions (storing timeout, retry policies alongside functions)
4. The metadata map can be stored in exec_state for O(1) lookup during execution

### Design Decision: Function Serialization

Task functions are Erlang closures (funs) that **cannot be serialized** via `term_to_binary/1`. This has implications:

- **Tracing/Replay**: Trace events cannot include full bytecode if metadata map contains funs. Solution: Trace system should exclude metadata map from serialization or store only task names.
- **Distribution**: Bytecode with closures cannot be sent across nodes. This is acceptable - wf_compile is intended to run locally before execution.
- **Persistence**: Bytecode cannot be persisted to disk with closures. Acceptable - recompilation is cheap.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| -------- | ----------------- | ---------------- |
| **Closure serialization breaks tracing** | High | Ensure wf_trace excludes metadata map from binary serialization. Store only task names in traces. |
| **Breaking existing bytecode consumers** | Medium | Change wf_vm:wf_bc() type conservatively. Add metadata map as optional second element of tuple: `{wf_vm:wf_bc(), optional_metadata_map()}` for backward compatibility during transition. |
| **Performance regression from map lookups** | Low | O(1) map lookup is negligible compared to function call overhead. No optimization needed. |
| **Type system mismatch** | Medium | Ensure all modules importing wf_vm:wf_bc() are updated. Update -export_type lists. |
| **Test failures from mock dependency** | Low | Current tests use mock_bytecode_* helpers that bypass wf_compile. These will continue working. New acceptance test validates real compilation path. |

## Recommended Approach

### Phase 1: Type System Extensions (wf_vm.erl)

1. Add new type for task metadata map:
```erlang
-type task_metadata_map() :: #{atom() => wf_term:task_metadata()}.
```

2. Extend bytecode type to wrapper:
```erlang
-type wf_bc() :: {[opcode()], task_metadata_map()}.
```
This bundles bytecode with metadata map.

3. Update opcode type to remain unchanged (task_exec still just has name):
```erlang
{task_exec, atom()}
```
Metadata comes from the wrapper map, not the opcode.

### Phase 2: Compiler Enhancement (wf_compile.erl)

1. Modify `compile/1` to collect metadata during compilation:
```erlang
compile(Term) ->
    {UnresolvedBytecode, MetadataMap} = compile_term_with_metadata(Term),
    ResolvedBytecode = resolve_labels(UnresolvedBytecode),
    {ok, {ResolvedBytecode, MetadataMap}}.
```

2. Add `compile_term_with_metadata/1` that returns `{bytecode(), metadata_map()}`:
```erlang
compile_term_with_metadata({task, Name, Metadata}) ->
    {[{task_exec, Name}], #{Name => Metadata}};
compile_term_with_metadata({seq, Left, Right}) ->
    {LeftCode, LeftMeta} = compile_term_with_metadata(Left),
    {RightCode, RightMeta} = compile_term_with_metadata(Right),
    SeqCode = [{seq_enter, 0}] ++ LeftCode ++ [{seq_next, make_label()}] ++ RightCode,
    {SeqCode, maps:merge(LeftMeta, RightMeta)}.
```
Pattern: Recursively compile, merge metadata maps.

3. Update all `compile_*` functions to return `{unresolved_bytecode(), metadata_map()}`.

### Phase 3: Executor Modification (wf_exec.erl)

1. Add `task_metadata` field to `exec_state` record (wf_exec.hrl:32-44):
```erlang
-record(exec_state, {
    ...
    task_metadata :: #{atom() => wf_term:task_metadata()}
}).
```

2. Update `new/1` to extract and store metadata from bytecode wrapper:
```erlang
new({Bytecode, Metadata}) ->
    ExecState = #exec_state{
        bytecode = Bytecode,
        task_metadata = Metadata,
        ...
    }.
```

3. Replace `lookup_task_function/2` implementation:
```erlang
lookup_task_function(TaskName, ExecState) ->
    Metadata = maps:get(TaskName, ExecState#exec_state.task_metadata),
    maps:get(function, Metadata).
```
Retrieves metadata map for task, extracts function field.

### Phase 4: Acceptance Test (test/wf_acceptance_tests.erl)

Create new test file with end-to-end test:
```erlang
-module(wf_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").

task_function_dispatch_test_() ->
    {"Tasks execute their real functions from metadata",
     fun() ->
         %% Build workflow with two tasks having distinct functions
         Term = wf_term:seq(
             wf_term:task(a, #{function => fun(Ctx) -> {ok, maps:put(a_ran, true, Ctx)} end}),
             wf_term:task(b, #{function => fun(Ctx) -> {ok, maps:put(b_ran, true, Ctx)} end})
         ),

         %% Compile to bytecode
         {ok, {Bytecode, _Metadata}} = wf_compile:compile(Term),

         %% Execute workflow
         ExecState0 = wf_exec:new({Bytecode, _Metadata}),
         {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),

         %% Verify both tasks ran
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(true, maps:get(a_ran, FinalCtx)),
         ?assertEqual(true, maps:get(b_ran, FinalCtx))
     end}.
```

Test verifies:
1. Compiler preserves task functions through metadata map
2. Executor looks up and calls real functions
3. Both functions execute and update context correctly

## Open Questions

1. **Backward compatibility**: Should we support both `{[opcode()]}` and `{[opcode()], metadata_map()}` bytecode formats during transition, or break all consumers? Recommendation: Break all consumers (wf_exec is the only consumer).

2. **Error handling**: What should happen if a task name in bytecode has no entry in metadata map? Recommendation: Throw `{badarg, {missing_task_metadata, TaskName}}` from `lookup_task_function/2`.

3. **Metadata persistence**: Should `wf_state` (item 006) persist task metadata map alongside execution state? Recommendation: No, metadata is derived from bytecode, not mutable state. Recompile on restart.

4. **Function serialization limits**: Since closures cannot be serialized, should we require task functions to be `{module(), atom()}` pairs instead of funs? Recommendation: No, keep funs for flexibility. Document that bytecode with metadata cannot be distributed or persisted.

5. **Test file naming**: Should the acceptance test be in `test/wf_acceptance_tests.erl` or `test/wf_task_dispatch_tests.erl`? Recommendation: Use `wf_acceptance_tests.erl` as specified in item.json title.
