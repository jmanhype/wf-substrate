# Research: Fix 16 test regressions caused by item 051 bytecode format change. wf_exec:new/1 now expects {Bytecode, MetadataMap} tuple but many callers still pass plain [opcode()] lists. The fix: make wf_exec:new/1 accept both formats — if passed a plain list, wrap it as {List, #{}} with empty metadata. This ensures backward compatibility. All 16 failures are function_clause errors in wf_exec:new/1 called from wf_case_runner and other test modules. After fixing, rebar3 eunit must show 0 failures. Also implement real loop condition evaluation: currently the loop_check opcode handler always continues without evaluating the loop policy. For {count, N} loops, track iteration count per loop scope and exit when count is reached. ACCEPTANCE TEST: Add to test/wf_acceptance_tests.erl a loop_count_test that compiles wf_term:loop({count, 3}, wf_term:task(counter, #{function => fun(Ctx) -> N = maps:get(n, Ctx, 0), {ok, Ctx#{n => N + 1}} end})). Execute it. Assert the result is {done, _} not {yield, _}. Assert final context has n => 3.

**Date**: 2025-01-11
**Item**: 054-fix-16-test-regressions-caused-by-item-051-bytecod

## Research Question

How do we fix 16 test regressions caused by item 051's bytecode format change while also implementing real loop condition evaluation for {count, N} loop policies?

## Summary

Item 051 changed the bytecode format from `[opcode()]` to `{[opcode()], task_metadata_map()}` to support real task function dispatch. However, this change broke **16 existing test modules** that still pass plain opcode lists to `wf_exec:new/1`, causing `function_clause` errors when the function pattern matches on `{Bytecode, Metadata}` tuple.

The fix requires **two coordinated changes**:

1. **Backward compatibility for wf_exec:new/1**: Modify the function to accept both formats:
   - If passed `{Bytecode, Metadata}` tuple, use it directly
   - If passed plain `[opcode()]` list, wrap it as `{List, #{}}` with empty metadata
   - This ensures old tests continue working while new code can leverage metadata

2. **Real loop condition evaluation**: The current `evaluate_loop_condition/2` implementation (wf_exec.erl:836-856) has incomplete logic:
   - `{count, N}` loops: Uses a context-based counter but initializes it incorrectly (defaults to N, should start at N and decrement)
   - `while` loops: Always returns true (mock implementation)
   - `until` loops: Always returns false after one iteration (mock implementation)
   - The fix requires tracking loop iteration count **per loop scope** (not globally in context) to handle nested loops correctly

## Current State Analysis

### Existing Implementation

**wf_exec.erl:52-78** - `new/1` only accepts tuple format:
```erlang
-spec new(wf_vm:wf_bc()) -> exec_state().
new({Bytecode, Metadata}) ->
    CaseId = make_ref(),
    ...
    #exec_state{
        bytecode = Bytecode,
        task_metadata = Metadata,
        ...
    }.
```
The function clause pattern matches on `{Bytecode, Metadata}` tuple. If passed a plain list, this causes a `function_clause` error at runtime.

**wf_compile.erl:132-153** - Compiler now returns tuple format:
```erlang
compile(Term) ->
    ...
    {ok, {ResolvedBytecode, MetadataMap}}.
```
The compiler was updated in item 051 to return `{Bytecode, Metadata}` tuples.

**Test modules passing plain lists** (causing function_clause errors):
- `wf_test_seq.erl:76-77, 97-98, 108-109, 120-121, 139-140, 150-151` - Calls `wf_exec:new(Bytecode)` where Bytecode is a plain list
- `wf_test_par.erl:84, 99, 113, 128, 146, 164, 176, 195` - Same pattern
- `wf_test_xor.erl:62, 77, 90, 104, 107, 117, 130, 145` - Same pattern
- `wf_test_join.erl:63, 75, 90, 104, 115, 130, 144, 159, 175, 187, 197, 210, 220, 239` - Same pattern
- `wf_test_term.erl:72, 82, 102, 110` - Same pattern
- `wf_test_cancel.erl:80, 98, 118, 135, 154, 173, 191, 211` - Same pattern
- `wf_test_mi.erl:76, 88, 102, 117, 135, 153, 169, 186, 196` - Same pattern
- `wf_case_runner.erl:121` - `ExecState0 = wf_exec:new(Bytecode)` where Bytecode comes from `start_link/2` parameter typed as `wf_vm:wf_bc()`

All these callers use mock bytecode generators that return plain lists like:
```erlang
mock_bytecode_seq_2_tasks() ->
    [
        {seq_enter, 0},
        {task_exec, task_a},
        {seq_next, 3},
        {task_exec, task_b},
        {done}
    ].
```

**wf_exec.erl:836-856** - `evaluate_loop_condition/2` has incomplete implementation:
```erlang
evaluate_loop_condition({count, N}, ExecState) ->
    %% Check counter stored in context
    Counter = maps:get(loop_counter, ExecState#exec_state.ctx, N),
    case Counter > 0 of
        true ->
            %% Decrement counter after checking
            NewCounter = Counter - 1,
            NewCtx = maps:put(loop_counter, NewCounter, ExecState#exec_state.ctx),
            {true, NewCtx};
        false ->
            {false, ExecState#exec_state.ctx}
    end;
```

**Problems with current implementation**:
1. **Counter initialization**: Uses `maps:get(loop_counter, Ctx, N)` which defaults to N if key doesn't exist. This means the loop runs N+1 times (N, N-1, ..., 1, 0) before exiting at Counter=0. Should initialize to N and exit when Counter reaches 0.
2. **Global counter storage**: Stores loop counter in context (`loop_counter` key), which breaks for **nested loops**. If inner loop decrements counter, outer loop's counter is corrupted.
3. **Mock implementations**: `while` always continues, `until` always exits after first iteration.

**wf_exec.erl:805-825** - `execute_loop_check/2` doesn't differentiate exit paths:
```erlang
execute_loop_check({_LoopCheck, Policy}, ExecState) ->
    case evaluate_loop_condition(Policy, ExecState) of
        {true, NewCtx} ->
            %% Condition satisfied, continue to body
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,  %% Advances to body
                ctx = NewCtx,
                step_count = ExecState#exec_state.step_count + 1
            };
        {false, NewCtx} ->
            %% Condition not satisfied, exit loop
            %% For now, we still advance to body
            %% LOOP_BACK will handle the actual looping
            %% In production, compiler would emit explicit exit label
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,  %% BUG: Should jump to exit, not body
                ctx = NewCtx,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.
```

**Bug**: When `evaluate_loop_condition` returns `{false, NewCtx}`, the executor still advances IP to body instead of jumping to loop exit. The comment acknowledges this ("In production, compiler would emit explicit exit label") but it's not implemented.

## Key Files

- **src/wf_exec.erl:52-78** - `new/1` function that needs backward compatibility wrapper
- **src/wf_exec.erl:805-825** - `execute_loop_check/2` that needs exit path fix
- **src/wf_exec.erl:836-856** - `evaluate_loop_condition/2` that needs per-scope counter tracking
- **src/wf_compile.erl:132-153** - `compile/1` returning new tuple format
- **src/wf_vm.erl:44** - `wf_bc()` type definition: `{[opcode()], task_metadata_map()}`
- **src/wf_case_runner.erl:69-121** - API and init code passing bytecode to executor
- **test/wf_acceptance_tests.erl:1-87** - Needs new `loop_count_test/0` acceptance test
- **test/wf_test_seq.erl** - 6 test cases passing plain bytecode lists
- **test/wf_test_par.erl** - 8 test cases passing plain bytecode lists
- **test/wf_test_xor.erl** - 8 test cases passing plain bytecode lists
- **test/wf_test_join.erl** - 14 test cases passing plain bytecode lists
- **test/wf_test_cancel.erl** - 8 test cases passing plain bytecode lists
- **test/wf_test_mi.erl** - 8 test cases passing plain bytecode lists
- **test/wf_test_term.erl** - 4 test cases passing plain bytecode lists

## Technical Considerations

### Dependencies

- **wf_vm.erl** (item 004): Defines `wf_bc()` type as `{[opcode()], task_metadata_map()}` - type definition is correct
- **wf_compile.erl** (item 051): Compiler returns tuple format - already implemented correctly
- **wf_exec.erl** (item 005): Executor needs backward compatibility and loop fixes
- **wf_term.erl** (item 002): Provides `loop/2` constructor for acceptance test

### Patterns to Follow

**Backward compatibility pattern** (observed in other modules):
When changing API contracts, provide wrapper functions that detect input format and normalize it. Similar to how Erlang's `file:read_file/1` accepts both string and binary paths.

**Per-scope state tracking pattern**:
The executor already tracks per-scope state via `scope_stack` field in `exec_state`. For loop counters, we should track them in a map keyed by scope ID or loop head IP, not in the global context.

**Test mock pattern**:
Existing test modules use mock bytecode generators that return plain lists. This is intentional - they test executor logic in isolation without depending on compiler. The fix must preserve this pattern.

### Loop Counter Storage Design

Two viable approaches:

**Option A: Per-scope counter map in exec_state** (RECOMMENDED)
- Add `loop_counters :: #{term() => non_neg_integer()}` field to `exec_state` record
- Key = loop head IP or scope ID (both unique identifiers)
- On `loop_check`, initialize counter if not exists: `maps:get(LoopIP, Counters, N)`
- On exit, clean up counter entry
- **Pros**: Clean separation from user context, handles nested loops correctly
- **Cons**: Requires modifying exec_state record

**Option B: Context-based counter with unique keys**
- Store counters in context as `{loop_counter, LoopIP} => Count` tuples
- More complex key structure to avoid collisions
- **Pros**: No record changes
- **Cons**: Pollutes user context with executor bookkeeping, more complex key management

**Recommendation**: Use Option A (per-scope counter map) for clean separation.

### Loop Exit Path Design

Current compiler (wf_compile.erl:314-329) emits:
```erlang
LoopCode = [LoopHeadLabel] ++
    [{loop_check, Policy}] ++
    BodyCode ++
    [{loop_back, LoopHeadLabel}] ++
    [ExitLabel],
```

The structure is: `loop_check -> body -> loop_back -> (jump to loop_check)`.

When `loop_check` evaluates to false, the executor should **skip to ExitLabel**, not advance to body. However, the current implementation doesn't have ExitLabel's IP available at runtime.

**Solutions**:
1. **Modify bytecode format**: Emit `loop_check` with `{exit_ip, ExitIP}` operand - requires compiler change
2. **Use IP arithmetic**: Calculate exit IP as `current_ip + body_length + 2` - fragile
3. **Add loop_exit opcode**: Emit explicit exit marker after loop - cleanest but requires compiler change

**For item 054 scope**: Use solution #2 (IP arithmetic) as quick fix. For proper fix, item 052 should implement solution #3.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| -------- | ----------------- | ---------------- |
| **Function clause errors in production** | High | Implement backward compatibility wrapper immediately. All test modules will start passing. |
| **Loop counter corruption in nested loops** | Medium | Use per-scope counter map (Option A) instead of context storage. |
| **Incorrect loop exit logic causing infinite loops** | High | Implement exit path calculation or add loop_exit opcode. Verify with acceptance test. |
| **Performance regression from type checking** | Low | Type check (is_list/1, is_tuple/1) is O(1) and negligible. |
| **Breaking real workflow execution** | Medium | Test with wf_acceptance_tests to ensure compiled workflows still work. |
| **Acceptance test flapping** | Low | Ensure loop count test verifies exact iteration count (n=3) and {done, _} result. |

## Recommended Approach

### Phase 1: Backward Compatibility for wf_exec:new/1

Modify `wf_exec.erl:52-78` to detect and normalize input format:

```erlang
-spec new(wf_vm:wf_bc() | [wf_vm:opcode()]) -> exec_state().
new({Bytecode, Metadata}) when is_list(Bytecode), is_map(Metadata) ->
    %% New format: tuple with metadata
    CaseId = make_ref(),
    InitialTokenId = make_ref(),
    ...
    #exec_state{
        bytecode = Bytecode,
        task_metadata = Metadata,
        ...
    };
new(Bytecode) when is_list(Bytecode) ->
    %% Old format: plain list (backward compatibility)
    %% Wrap with empty metadata map
    new({Bytecode, #{}}).
```

This accepts both formats:
- New code: `wf_exec:new({Bytecode, Metadata})` - uses first clause
- Old tests: `wf_exec:new(BytecodeList)` - uses second clause, wraps in tuple

**Test verification**: Run `rebar3 eunit`. All 16 failing tests should now pass.

### Phase 2: Per-Scope Loop Counter Tracking

1. **Add loop counters to exec_state record** (wf_exec.hrl):
```erlang
-record(exec_state, {
    ...
    loop_counters :: #{term() => non_neg_integer()}
}).
```

2. **Initialize empty map in new/1**:
```erlang
loop_counters = #{},
```

3. **Modify evaluate_loop_condition to use per-scope counters**:
```erlang
evaluate_loop_condition({count, N}, ExecState) ->
    %% Use current IP as unique loop identifier
    LoopIP = ExecState#exec_state.ip,
    Counters = ExecState#exec_state.loop_counters,

    %% Initialize counter on first check (defaults to N)
    Counter = maps:get(LoopIP, Counters, N),

    case Counter > 0 of
        true ->
            %% Decrement and continue
            NewCounter = Counter - 1,
            NewCounters = maps:put(LoopIP, NewCounter, Counters),
            NewExecState = ExecState#exec_state{loop_counters = NewCounters},
            {true, NewExecState#exec_state.ctx, NewExecState};
        false ->
            %% Exit loop
            {false, ExecState#exec_state.ctx, ExecState}
    end;
```

Note: Return `{bool(), ctx(), exec_state()}` triple to pass updated exec_state back.

4. **Update execute_loop_check to handle exec_state return**:
```erlang
execute_loop_check({_LoopCheck, Policy}, ExecState) ->
    case evaluate_loop_condition(Policy, ExecState) of
        {true, NewCtx, NewExecState} ->
            %% Continue to body
            NewExecState#exec_state{
                ip = NewExecState#exec_state.ip + 1,
                ctx = NewCtx,
                step_count = NewExecState#exec_state.step_count + 1
            };
        {false, NewCtx, NewExecState} ->
            %% Exit loop: calculate exit IP
            %% Skip to instruction after loop_back (end of loop)
            %% This is a heuristic - proper fix requires loop_exit opcode
            ExitIP = calculate_loop_exit(ExecState),
            NewExecState#exec_state{
                ip = ExitIP,
                ctx = NewCtx,
                step_count = NewExecState#exec_state.step_count + 1
            }
    end.
```

5. **Implement calculate_loop_exit/1**:
```erlang
calculate_loop_exit(ExecState) ->
    %% Find next loop_back opcode and exit to IP after it
    %% Scan forward from current IP for loop_back
    Bytecode = ExecState#exec_state.bytecode,
    CurrentIP = ExecState#exec_state.ip,
    find_loop_back_exit(Bytecode, CurrentIP + 1, 0).

find_loop_back_exit(Bytecode, IP, Depth) ->
    case lists:nth(IP + 1, Bytecode) of
        {loop_back, _TargetIP} when Depth =:= 0 ->
            IP + 1;  %% Exit to IP after loop_back
        {loop_back, _TargetIP} ->
            find_loop_back_exit(Bytecode, IP + 1, Depth - 1);
        {loop_check, _Policy} ->
            find_loop_back_exit(Bytecode, IP + 1, Depth + 1);
        _Opcode ->
            find_loop_back_exit(Bytecode, IP + 1, Depth)
    end.
```

This scans forward to find matching `loop_back` opcode (handling nested loops via depth counter) and returns IP after it.

### Phase 3: Acceptance Test

Add to `test/wf_acceptance_tests.erl`:

```erlang
loop_count_test_() ->
    {"Loop executes exactly N times with count policy",
     fun() ->
         %% Build workflow: loop({count, 3}, task(counter))
         Term = wf_term:loop(
             {count, 3},
             wf_term:task(counter, #{
                 function => fun(Ctx) ->
                     N = maps:get(n, Ctx, 0),
                     {ok, Ctx#{n => N + 1}}
                 end
             })
         ),

         %% Compile to bytecode
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),

         %% Execute workflow
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),

         %% Verify result is {done, _} not {yield, _}
         ?assertMatch({done, _}, {done, ExecState1}),

         %% Verify task ran exactly 3 times
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(3, maps:get(n, FinalCtx))
     end}.
```

Test verifies:
1. Loop runs exactly N=3 times (n counter in context = 3)
2. Workflow completes (not yielded)
3. Real task functions are dispatched from metadata

## Open Questions

1. **Calculate loop exit IP**: Should we implement IP arithmetic heuristic (fragile but works for item 054) or add `loop_exit` opcode (proper fix requiring compiler change)? Recommendation: Use heuristic for item 054, file item 052 for proper fix.

2. **Loop counter cleanup**: Should we remove counter entries from `loop_counters` map when loop exits to avoid memory leaks in long-running workflows? Recommendation: Yes, add cleanup in `{false, _}` branch of evaluate_loop_condition.

3. **Context key collision**: If using Option B (context-based counters), what if user's workflow already has `loop_counter` key in context? Recommendation: Use unique key prefix like `__wf_loop_counter_` or avoid context storage entirely (use Option A).

4. **Test coverage**: Do we need tests for nested loops to verify counter isolation? Recommendation: Yes, add nested_loop_counters_dont_interfere_test/0 to wf_acceptance_tests.erl.

5. **wf_case_runner compatibility**: Does wf_case_runner.erl:121 need type specification update since `wf_vm:wf_bc()` is now a union type? Recommendation: Update type spec to `wf_vm:wf_bc() | [wf_vm:opcode()]` or leave as-is (backward compatible).
