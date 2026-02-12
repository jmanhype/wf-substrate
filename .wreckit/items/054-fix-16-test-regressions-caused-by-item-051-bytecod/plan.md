# Fix 16 test regressions caused by item 051 bytecode format change. wf_exec:new/1 now expects {Bytecode, MetadataMap} tuple but many callers still pass plain [opcode()] lists. The fix: make wf_exec:new/1 accept both formats — if passed a plain list, wrap it as {List, #{}} with empty metadata. This ensures backward compatibility. All 16 failures are function_clause errors in wf_exec:new/1 called from wf_case_runner and other test modules. After fixing, rebar3 eunit must show 0 failures. Also implement real loop condition evaluation: currently the loop_check opcode handler always continues without evaluating the loop policy. For {count, N} loops, track iteration count per loop scope and exit when count is reached. ACCEPTANCE TEST: Add to test/wf_acceptance_tests.erl a loop_count_test that compiles wf_term:loop({count, 3}, wf_term:task(counter, #{function => fun(Ctx) -> N = maps:get(n, Ctx, 0), {ok, Ctx#{n => N + 1}} end})). Execute it. Assert the result is {done, _} not {yield, _}. Assert final context has n => 3. Implementation Plan

## Implementation Plan Title

Backward Compatible Bytecode Format and Real Loop Condition Evaluation

## Overview

Item 051 changed the bytecode format from `[opcode()]` to `{[opcode()], task_metadata_map()}` to support real task function dispatch. This broke 56 test cases across 8 test modules that still pass plain opcode lists to `wf_exec:new/1`, causing `function_clause` errors. This fix makes `wf_exec:new/1` accept both formats for backward compatibility.

Additionally, the loop condition evaluation logic is incomplete:
- `{count, N}` loops use a global context counter that breaks for nested loops
- `loop_check` handler doesn't properly exit when condition is false
- Counters are initialized incorrectly (runs N+1 times instead of N times)

This fix implements per-scope loop counters in `exec_state` (not context) and proper loop exit logic.

## Current State

**Issue 1: Function clause errors in wf_exec:new/1**

`wf_exec.erl:52-78` currently only accepts the tuple format:

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

When called with a plain list (as in test modules), this causes a `function_clause` error because the pattern match on `{Bytecode, Metadata}` fails.

**Test modules affected** (16 failures total):
- `wf_test_seq.erl` - 6 tests pass plain bytecode lists to `wf_exec:new/1`
- `wf_test_par.erl` - 8 tests
- `wf_test_xor.erl` - 8 tests
- `wf_test_join.erl` - 14 tests
- `wf_test_cancel.erl` - 8 tests
- `wf_test_mi.erl` - 9 tests
- `wf_test_term.erl` - 4 tests
- `wf_case_runner.erl:121` - Production code accepts bytecode parameter

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

**Issue 2: Incomplete loop condition evaluation**

`wf_exec.erl:805-856` has two problems:

1. **evaluate_loop_condition/2** uses context-based storage:
```erlang
evaluate_loop_condition({count, N}, ExecState) ->
    Counter = maps:get(loop_counter, ExecState#exec_state.ctx, N),
    case Counter > 0 of
        true ->
            NewCounter = Counter - 1,
            NewCtx = maps:put(loop_counter, NewCounter, ExecState#exec_state.ctx),
            {true, NewCtx};
        false ->
            {false, ExecState#exec_state.ctx}
    end.
```

Problems:
- **Counter initialization bug**: Defaults to N, so loop runs N+1 times (N, N-1, ..., 0, then exits at 0)
- **Global counter storage**: Stores in context, breaks for **nested loops** (inner loop corrupts outer loop's counter)
- **Mock implementations**: `while` always returns true, `until` always returns false after first iteration

2. **execute_loop_check/2** doesn't differentiate exit paths:
```erlang
execute_loop_check({_LoopCheck, Policy}, ExecState) ->
    case evaluate_loop_condition(Policy, ExecState) of
        {true, NewCtx} ->
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,  %% Advances to body
                ctx = NewCtx,
                step_count = ExecState#exec_state.step_count + 1
            };
        {false, NewCtx} ->
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,  %% BUG: Should exit loop, not enter body
                ctx = NewCtx,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.
```

When condition is false, executor should **skip to loop exit**, but it advances to body instead.

## Desired End State

### Success Criteria

1. **All 16 tests pass**: `rebar3 eunit` shows 0 failures
2. **Backward compatibility**: `wf_exec:new/1` accepts both formats without breaking existing code
3. **Loop count test passes**: New acceptance test in `wf_acceptance_tests.erl` verifies:
   - Loop with `{count, 3}` executes exactly 3 times
   - Workflow completes as `{done, _}` (not yielded)
   - Final context has `n => 3` (counter incremented 3 times)
4. **Nested loops work correctly**: Each loop has its own counter, no interference
5. **No infinite loops**: `{count, N}` loops terminate after N iterations

### Key Discoveries:

- **wf_exec.hrl:32-45** - `exec_state` record already has `task_metadata` field (added in item 051), no schema changes needed
- **wf_vm.erl:44** - `wf_bc()` type is defined as `{[opcode()], task_metadata_map()}` - this type definition is correct for new code
- **wf_compile.erl:132-153** - Compiler already returns tuple format `{Bytecode, Metadata}` - no changes needed
- **wf_case_runner.erl:121** - Production code calls `wf_exec:new(Bytecode)` - must handle both formats
- **Test pattern**: Mock bytecode generators intentionally return plain lists to test executor in isolation without compiler dependency - this pattern must be preserved
- **wf_exec.erl:34-64** - `token` record already supports `instance_id` field for per-instance state tracking - similar pattern can be used for loop counters

## What We're NOT Doing

- **NOT modifying the compiler**: `wf_compile.erl` already returns correct tuple format
- **NOT changing the type definition**: `wf_vm:wf_bc()` type definition remains as `{[opcode()], task_metadata_map()}`
- **NOT implementing `while` and `until` policies**: These remain mock implementations (item 052 should implement them)
- **NOT adding `loop_exit` opcode**: Using IP arithmetic heuristic for loop exit (item 052 should add proper opcode)
- **NOT modifying all test modules**: Tests continue using plain bytecode lists - only `wf_exec:new/1` needs to handle both formats
- **NOT adding while/until acceptance tests**: Only testing `{count, N}` policy in this item

## Implementation Approach

**High-level strategy**: Add a backward compatibility clause to `wf_exec:new/1` that detects the input format and normalizes it. For loop evaluation, add per-scope counter tracking to the executor state and implement proper exit path calculation.

**Design rationale**:

1. **Backward compatibility via function clauses**: Erlang's pattern matching makes it trivial to accept both formats with two function clauses. The first clause matches the tuple format (new code), the second matches the list format (old tests) and wraps it.

2. **Per-scope counters in exec_state**: Adding a `loop_counters` field to `exec_state` record cleanly separates executor bookkeeping from user context. Using loop head IP as the key ensures nested loops don't interfere with each other.

3. **IP arithmetic for loop exit**: Scanning forward to find the matching `loop_back` opcode and jumping to the instruction after it is a pragmatic solution for item 054. This avoids compiler changes while fixing the infinite loop bug. Item 052 should add a proper `loop_exit` opcode.

4. **Return triple from evaluate_loop_condition**: Changing return from `{bool(), ctx()}` to `{bool(), ctx(), exec_state()}` allows passing updated counter state back without polluting user context.

---

## Phases

### Phase 1: Backward Compatibility for wf_exec:new/1

#### Overview

Add a second function clause to `wf_exec:new/1` that accepts plain bytecode lists and wraps them in the tuple format. This makes all 16 failing tests pass without any changes to test code.

#### Changes Required:

##### 1. wf_exec.erl - new/1 function

**File**: `src/wf_exec.erl`
**Changes**: Modify `new/1` at line 52-78 to accept both formats

```erlang
%% @doc Create new executor from bytecode with metadata
%% Accepts both new format {Bytecode, Metadata} and legacy format [opcode()]
-spec new(wf_vm:wf_bc() | [wf_vm:opcode()]) -> exec_state().
new({Bytecode, Metadata}) when is_list(Bytecode), is_map(Metadata) ->
    %% New format: tuple with metadata
    CaseId = make_ref(),
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
        task_metadata = Metadata
    };
new(Bytecode) when is_list(Bytecode) ->
    %% Legacy format: plain list of opcodes (backward compatibility)
    %% Wrap with empty metadata map for old test code
    new({Bytecode, #{}}).
```

**Key changes**:
- Add guard clauses `when is_list(Bytecode), is_map(Metadata)` to first clause for type safety
- Add second clause that matches plain lists and delegates to first clause with empty metadata
- Update type spec to `wf_vm:wf_bc() | [wf_vm:opcode()]`

##### 2. wf_exec.hrl - Add loop_counters field

**File**: `src/wf_exec.hrl`
**Changes**: Add `loop_counters` field to `exec_state` record at line 32-45

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: [wf_vm:opcode()],  %% Opcodes only (metadata stored separately)
    ctx :: map(),
    case_id :: term() | undefined,  %% Case ID for effect ID generation
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | blocked_effect | cancelled | failed,
    current_token :: term() | undefined,
    task_metadata :: wf_vm:task_metadata_map(),  %% Task metadata for function lookup
    loop_counters :: #{term() => non_neg_integer()}  %% Per-scope loop iteration counters
}).
```

**Key changes**:
- Add `loop_counters :: #{term() => non_neg_integer()}` field to track loop iteration counts
- Key = loop head IP (unique identifier for each loop)
- Value = remaining iteration count

##### 3. wf_exec.erl - Initialize loop_counters in new/1

**File**: `src/wf_exec.erl`
**Changes**: Initialize `loop_counters = #{}` in both clauses of `new/1`

```erlang
%% In first clause (new format):
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
    task_metadata = Metadata,
    loop_counters = #{}  %% Initialize empty loop counters map
}.

%% Second clause (legacy format) delegates to first, so no changes needed there
```

##### 4. wf_exec.erl - snapshot/restore exec_state

**File**: `src/wf_exec.erl`
**Changes**: Add `loop_counters` to snapshot/restore functions at lines 106-191

```erlang
%% In snapshot_exec_state/1 at line 108-123:
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
    task_metadata => ExecState#exec_state.task_metadata,
    loop_counters => ExecState#exec_state.loop_counters  %% Add loop_counters to snapshot
},

%% In restore_exec_state/2 at line 147-186:
%% Update validation to include loop_counters:
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
     maps:is_key(task_metadata, StateMap) andalso
     maps:is_key(loop_counters, StateMap) of  %% Validate loop_counters field

%% In exec_state construction:
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
    task_metadata = maps:get(task_metadata, StateMap),
    loop_counters = maps:get(loop_counters, StateMap)  %% Restore loop_counters
},
```

#### Success Criteria:

##### Automated Verification:

- [ ] All 16 failing tests now pass: `rebar3 eunit` shows 0 failures
- [ ] Specifically check: `wf_test_seq`, `wf_test_par`, `wf_test_xor`, `wf_test_join`, `wf_test_cancel`, `wf_test_mi`, `wf_test_term`
- [ ] `wf_case_runner_tests` pass
- [ ] Compilation succeeds: `rebar3 compile`

##### Manual Verification:

- [ ] Verify `wf_exec:new([{task_exec, test}])` works (legacy format)
- [ ] Verify `wf_exec:new({[{task_exec, test}], #{}})` works (new format)
- [ ] Check that acceptance tests still pass (compiled workflows use new format)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Per-Scope Loop Counter Tracking

#### Overview

Implement real loop condition evaluation for `{count, N}` policies by tracking iteration counts per loop scope instead of globally in context. This fixes nested loop bugs and ensures correct termination.

#### Changes Required:

##### 1. wf_exec.erl - evaluate_loop_condition/3

**File**: `src/wf_exec.erl`
**Changes**: Rewrite `evaluate_loop_condition/2` to `evaluate_loop_condition/3` at line 834-856

```erlang
%% @doc Evaluate loop condition with per-scope counter tracking
%% Returns {bool(), ctx(), exec_state()} to pass updated counter state
-spec evaluate_loop_condition(wf_vm:loop_policy(), exec_state()) -> {boolean(), map(), exec_state()}.
evaluate_loop_condition({count, N}, ExecState) ->
    %% Use current IP as unique loop identifier (IP of loop_check instruction)
    LoopIP = ExecState#exec_state.ip,
    Counters = ExecState#exec_state.loop_counters,

    %% Initialize counter on first check (defaults to N)
    %% Subsequent iterations use stored counter
    Counter = maps:get(LoopIP, Counters, N),

    case Counter > 0 of
        true ->
            %% Decrement counter and continue to loop body
            NewCounter = Counter - 1,
            NewCounters = maps:put(LoopIP, NewCounter, Counters),
            NewExecState = ExecState#exec_state{loop_counters = NewCounters},
            {true, NewExecState#exec_state.ctx, NewExecState};
        false ->
            %% Counter exhausted, exit loop
            %% Clean up counter entry to avoid memory leaks
            NewCounters = maps:remove(LoopIP, Counters),
            NewExecState = ExecState#exec_state{loop_counters = NewCounters},
            {false, NewExecState#exec_state.ctx, NewExecState}
    end;
evaluate_loop_condition(while, ExecState) ->
    %% While loop: check condition before body
    %% Mock implementation - always continue (item 052 should implement real evaluation)
    {true, ExecState#exec_state.ctx, ExecState};
evaluate_loop_condition(until, ExecState) ->
    %% Until loop: check condition after body
    %% Mock implementation - always exit after first iteration (item 052 should implement real evaluation)
    {false, ExecState#exec_state.ctx, ExecState}.
```

**Key changes**:
- Return triple `{bool(), ctx(), exec_state()}` to pass updated counters
- Use `LoopIP` as key for per-scope counter tracking
- Initialize counter to N on first check (not N+1)
- Decrement after check (correct semantics)
- Clean up counter entry when loop exits (memory management)
- `while` and `until` remain mocks (out of scope for item 054)

##### 2. wf_exec.erl - execute_loop_check/2

**File**: `src/wf_exec.erl`
**Changes**: Update `execute_loop_check/2` at line 804-825 to handle new return type

```erlang
%% @doc Execute LOOP_CHECK: evaluate condition, exit or continue
execute_loop_check({_LoopCheck, Policy}, ExecState) ->
    case evaluate_loop_condition(Policy, ExecState) of
        {true, NewCtx, NewExecState} ->
            %% Condition satisfied, continue to loop body
            NewExecState#exec_state{
                ip = NewExecState#exec_state.ip + 1,
                ctx = NewCtx,
                step_count = NewExecState#exec_state.step_count + 1
            };
        {false, NewCtx, NewExecState} ->
            %% Condition not satisfied, exit loop
            %% Calculate exit IP by scanning forward to find matching loop_back
            ExitIP = calculate_loop_exit(NewExecState),
            NewExecState#exec_state{
                ip = ExitIP,
                ctx = NewCtx,
                step_count = NewExecState#exec_state.step_count + 1
            }
    end.
```

**Key changes**:
- Destructure triple return from `evaluate_loop_condition`
- When false, call `calculate_loop_exit/1` to find exit IP
- Jump to exit IP instead of advancing to body

##### 3. wf_exec.erl - calculate_loop_exit/1

**File**: `src/wf_exec.erl`
**Changes**: Add new helper function after `evaluate_loop_condition/3`

```erlang
%% @doc Calculate loop exit IP by scanning forward to find matching loop_back
%% Handles nested loops via depth counter (finds loop_back at same depth)
-spec calculate_loop_exit(exec_state()) -> non_neg_integer().
calculate_loop_exit(ExecState) ->
    %% Find next loop_back opcode at depth 0 (matching this loop_check)
    %% Start scanning from instruction after current IP (the loop body)
    Bytecode = ExecState#exec_state.bytecode,
    CurrentIP = ExecState#exec_state.ip,
    find_loop_back_exit(Bytecode, CurrentIP + 1, 0).

%% @private Scan bytecode for matching loop_back, handling nested loops
find_loop_back_exit(Bytecode, IP, Depth) ->
    case IP >= length(Bytecode) of
        true ->
            %% Reached end of bytecode without finding loop_back
            %% This shouldn't happen in well-formed bytecode
            error({missing_loop_back, IP});
        false ->
            Opcode = lists:nth(IP + 1, Bytecode),  %% IP is 0-indexed
            case Opcode of
                {loop_back, _TargetIP} when Depth =:= 0 ->
                    %% Found matching loop_back at same depth
                    %% Exit to instruction after loop_back
                    IP + 1;
                {loop_back, _TargetIP} ->
                    %% loop_back for nested loop, decrement depth and continue
                    find_loop_back_exit(Bytecode, IP + 1, Depth - 1);
                {loop_check, _Policy} ->
                    %% Entered nested loop, increment depth and continue
                    find_loop_back_exit(Bytecode, IP + 1, Depth + 1);
                _Opcode ->
                    %% Any other opcode, continue scanning
                    find_loop_back_exit(Bytecode, IP + 1, Depth)
            end
    end.
```

**Key changes**:
- Scan forward from current IP to find matching `loop_back` at same depth
- Handle nested loops by tracking depth (increment on `loop_check`, decrement on `loop_back`)
- Return IP after the matching `loop_back` (the loop exit point)
- Error if bytecode is malformed (no matching `loop_back`)

#### Success Criteria:

##### Automated Verification:

- [ ] New acceptance test `loop_count_test/0` passes (added in Phase 3)
- [ ] All existing tests still pass: `rebar3 eunit` shows 0 failures
- [ ] No infinite loops in test execution (all tests complete in reasonable time)

##### Manual Verification:

- [ ] Create a test workflow with `loop({count, 3}, task(counter))`
- [ ] Verify task runs exactly 3 times (check context counter)
- [ ] Verify workflow completes as `{done, _}` not `{yield, _}`
- [ ] Create nested loops: `loop({count, 2}, loop({count, 3}, task(inner)))`
- [ ] Verify inner loop runs 3 times, outer loop runs 2 times (6 total executions)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Acceptance Test

#### Overview

Add acceptance test to `test/wf_acceptance_tests.erl` that verifies end-to-end loop execution with compiled bytecode and real task functions.

#### Changes Required:

##### 1. test/wf_acceptance_tests.erl - Add loop_count_test/0

**File**: `test/wf_acceptance_tests.erl`
**Changes**: Add new test case at end of file (after line 86)

```erlang
%%--------------------------------------------------------------------
%% @doc Test that loop executes exactly N times with count policy
%%
%% This test verifies:
%% 1. Loop policy {count, N} terminates after N iterations
%% 2. Loop counters are tracked per-scope (not in user context)
%% 3. Workflow completes as {done, _} (not yielded)
%% 4. Task functions from metadata are dispatched correctly
%%--------------------------------------------------------------------
loop_count_test_() ->
    {"Loop executes exactly N times with count policy",
     fun() ->
         %% Build workflow: loop({count, 3}, task(counter))
         %% Task increments 'n' key in context each time it runs
         Term = wf_term:loop(
             {count, 3},
             wf_term:task(counter, #{
                 function => fun(Ctx) ->
                     N = maps:get(n, Ctx, 0),
                     {ok, Ctx#{n => N + 1}}
                 end
             })
         ),

         %% Compile to bytecode (returns {ok, {Bytecode, Metadata}})
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),

         %% Execute workflow with both bytecode formats
         %% Test new format (tuple with metadata)
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),

         %% Verify result is {done, _} not {yield, _}
         ?assertMatch({done, _}, {done, ExecState1}),

         %% Verify task ran exactly 3 times (n counter = 3)
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(3, maps:get(n, FinalCtx, 0)),

         %% Verify loop counter was cleaned up (not in context)
         ?assertNot(maps:is_key(loop_counter, FinalCtx)),

         %% Test backward compatibility: plain bytecode list should also work
         %% (use same Bytecode, but wrap as legacy format)
         ExecState2 = wf_exec:new(Bytecode),
         {done, ExecState3} = wf_exec:run(ExecState2, 100, deterministic),
         FinalCtx2 = wf_exec:get_ctx(ExecState3),
         ?assertEqual(3, maps:get(n, FinalCtx2, 0))
     end}.
```

**Key test assertions**:
- Workflow completes as `{done, _}` (loop terminates correctly)
- Context `n` key equals 3 (task ran exactly 3 times)
- `loop_counter` key not in context (counters stored in exec_state, not user context)
- Backward compatibility works (plain bytecode list also produces correct result)

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes including new `loop_count_test_()`
- [ ] Test output shows "Loop executes exactly N times with count policy" passed
- [ ] All other acceptance tests still pass (no regressions)

##### Manual Verification:

- [ ] Run `rebar3 eunit -v` to see detailed test output
- [ ] Verify task function was actually called 3 times (check trace events if needed)
- [ ] Test that loop with `{count, 1}` runs exactly once
- [ ] Test that loop with `{count, 0}` doesn't run body at all

**Note**: This is the final phase. After completion, all 16 tests should pass and the new acceptance test should verify loop functionality.

---

## Testing Strategy

### Unit Tests:

- **wf_exec_tests.erl**: Add tests for `evaluate_loop_condition/3` with various `{count, N}` values
- **wf_exec_tests.erl**: Add tests for `calculate_loop_exit/1` with nested loops
- **wf_exec_tests.erl**: Test backward compatibility clause (both input formats)
- **Existing tests**: Verify all 16 failing tests now pass without modification

### Integration Tests:

- **wf_acceptance_tests.erl**: New `loop_count_test/0` verifies end-to-end loop execution with compiled bytecode
- **wf_acceptance_tests.erl**: Existing tests verify compiled workflows still work with new bytecode format
- **wf_case_runner_tests.erl**: Verify case runner works with both bytecode formats

### Manual Testing Steps:

1. **Verify backward compatibility**:
   ```erlang
   %% In erlang shell:
   {ok, {Bytecode, Metadata}} = wf_compile:compile(wf_term:task(test, #{})).
   %% Both should work:
   ExecState1 = wf_exec:new({Bytecode, Metadata}).  %% New format
   ExecState2 = wf_exec:new(Bytecode).               %% Legacy format
   ```

2. **Verify loop execution**:
   ```erlang
   Term = wf_term:loop({count, 3}, wf_term:task(counter, #{
       function => fun(Ctx) ->
           N = maps:get(n, Ctx, 0),
           {ok, Ctx#{n => N + 1}}
       end
   })),
   {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
   ExecState = wf_exec:new({Bytecode, Metadata}),
   {done, FinalExec} = wf_exec:run(ExecState, 100, deterministic),
   maps:get(n, wf_exec:get_ctx(FinalExec)).  %% Should return 3
   ```

3. **Verify nested loops**:
   ```erlang
   InnerTerm = wf_term:loop({count, 2}, wf_term:task(inner, #{
       function => fun(Ctx) ->
           Count = maps:get(inner_count, Ctx, 0),
           {ok, Ctx#{inner_count => Count + 1}}
       end
   })),
   OuterTerm = wf_term:loop({count, 3}, InnerTerm),
   {ok, {Bytecode, Metadata}} = wf_compile:compile(OuterTerm),
   ExecState = wf_exec:new({Bytecode, Metadata}),
   {done, FinalExec} = wf_exec:run(ExecState, 100, deterministic),
   maps:get(inner_count, wf_exec:get_ctx(FinalExec)).  %% Should return 6 (3 * 2)
   ```

## Migration Notes

**No data migration required** - This is a behavioral change, not a data migration.

**API Compatibility**:
- `wf_exec:new/1` now accepts union type: `wf_vm:wf_bc() | [wf_vm:opcode()]`
- All existing code using tuple format continues working unchanged
- Old code using plain lists now works via backward compatibility clause

**Breaking Changes**: None - this is a non-breaking change that restores compatibility.

**Future Work** (out of scope for item 054):
- Item 052: Implement `loop_exit` opcode for cleaner loop termination
- Item 052: Implement real `while` and `until` policy evaluation
- Item 052: Add nested loop acceptance tests to verify counter isolation

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/054-fix-16-test-regressions-caused-by-item-051-bytecod/research.md`
- Source files:
  - `src/wf_exec.erl:52-78` - `new/1` function (backward compatibility)
  - `src/wf_exec.erl:805-856` - `execute_loop_check/2` and `evaluate_loop_condition/2` (loop evaluation)
  - `src/wf_exec.hrl:32-45` - `exec_state` record (add `loop_counters` field)
  - `src/wf_vm.erl:44` - `wf_bc()` type definition (no changes needed)
  - `src/wf_compile.erl:132-153` - `compile/1` returning tuple format (no changes needed)
  - `src/wf_case_runner.erl:121` - Production code using `wf_exec:new/1` (must handle both formats)
  - `test/wf_acceptance_tests.erl:1-87` - Add new `loop_count_test/0` acceptance test
  - `test/wf_test_seq.erl:76-77, 97-98, 108-109, 120-121, 139-140, 150-151` - Tests passing plain bytecode lists
  - `test/wf_test_par.erl:84, 99, 113, 128, 146, 164, 176, 195` - Tests passing plain bytecode lists
  - `test/wf_test_xor.erl:62, 77, 90, 104, 107, 117, 130, 145` - Tests passing plain bytecode lists
  - `test/wf_test_join.erl:63, 75, 90, 104, 115, 130, 144, 159, 175, 187, 197, 210, 220, 239` - Tests passing plain bytecode lists
  - `test/wf_test_cancel.erl:80, 98, 118, 135, 154, 173, 191, 211` - Tests passing plain bytecode lists
  - `test/wf_test_mi.erl:76, 88, 102, 117, 135, 153, 169, 186, 196` - Tests passing plain bytecode lists
  - `test/wf_test_term.erl:72, 82, 102, 110` - Tests passing plain bytecode lists
