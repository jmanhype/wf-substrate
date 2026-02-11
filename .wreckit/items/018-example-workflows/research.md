# Research: Implement example workflow modules

**Date**: 2025-01-11
**Item**: 018-example-workflows

## Research Question

Implement example modules demonstrating real workflow patterns:

1. wf_example_basic.erl — Vending machine workflow: seq(task(insert_coin), xor([seq(task(select_drink), task(dispense)), task(refund)])). Demonstrates basic sequence, exclusive choice with signal. Includes run/0 that executes with mock effects and prints trace.

2. wf_example_discriminator.erl — Discriminator pattern: par of 3 approval tasks, first-complete join cancels remaining. Demonstrates parallel split, discriminator join, automatic cancellation. Simulates varying completion times via effect delays.

3. wf_example_cancel_region.erl — Cancel region workflow: main flow with a cancel region containing a long-running subprocess. External signal triggers region cancellation mid-execution. Demonstrates structured cancellation preserving the main flow.

4. wf_example_multiple_instances.erl — Multiple instance workflow: dynamic instance count (from ctx), each instance processes an item, wait_n join (3 of 5 must succeed). Demonstrates MI spawning, partial completion, cancellation of excess instances.

Each example module exports:
- term/0 -> wf_term() — the workflow definition.
- run/0 -> {ok, Trace} — execute with mock effects, return trace.
- expected_trace_structure/0 -> [atom()] — expected sequence of opcode categories for validation.

Include wf_test_examples.erl with EUnit tests asserting that each example's actual trace matches its expected_trace_structure.

## Summary

This task requires creating four example workflow modules that demonstrate real-world workflow patterns using the wf_substrate bytecode executor. Each example must be self-contained with three exports (term/0, run/0, expected_trace_structure/0) and use the existing bytecode executor (wf_exec.erl) and tracing infrastructure (wf_trace.erl). The examples should be educational, showing how to compose workflow patterns from bytecodes, with a test module (wf_test_examples.erl) validating trace structure matches expectations.

The existing codebase has comprehensive test modules (wf_test_seq, wf_test_par, wf_test_xor, wf_test_join, wf_test_cancel, wf_test_mi) that demonstrate how to construct bytecode and use the executor. However, these are unit tests, not executable examples with documentation. The new examples should be more narrative, showing realistic use cases with comments explaining each pattern.

Key challenge: MI_SPAWN opcode is referenced in tests (wf_test_mi.erl:20) but has no handler in wf_exec:execute_opcode/2. Item 009 (multiple-instance-support) is not complete. The wf_example_multiple_instances.erl example should either use PAR_FORK as a workaround or note this limitation.

Cancellation is stubbed (wf_exec.erl:512-529), so wf_example_cancel_region.erl will demonstrate the bytecode structure but won't actually cancel at runtime. This should be documented.

The examples should follow the pattern from test modules: define bytecode as list of opcodes, create executor with wf_exec:new/1, run with wf_exec:run/3, and collect trace events. Mock effects can be simulated via TASK_EXEC opcodes that update context (current behavior in wf_exec.erl:231-239).

## Current State Analysis

### Existing Implementation

**Executor Core:**
- src/wf_exec.erl: Bytecode executor with step/2, run/3, new/1 functions (lines 1-598)
- Opcodes handled: SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, TASK_EXEC, DONE (lines 187-208)
- Missing: MI_SPAWN opcode handler (referenced in wf_test_mi.erl but not implemented)

**Bytecode Format:**
- src/wf_vm.erl: Defines wf_bc() as [opcode()], opcode() as tagged tuples (lines 9-21)
- Join policies: all, {first_n, N}, {n_of_m, N, M}, first_complete, sync_merge (lines 24-29)
- Loop policies: {count, N}, while, until (lines 32-35)
- MI policies: {fixed, N}, {dynamic, Min, Max} (lines 38-40)

**Test Modules as Reference:**
- test/wf_test_seq.erl: Mock bytecode generators for sequences (lines 17-51), EUnit tests (lines 75-159)
- test/wf_test_par.erl: Parallel patterns with PAR_FORK + JOIN_WAIT (lines 17-76)
- test/wf_test_xor.erl: Exclusive choice with XOR_CHOOSE (lines 17-54)
- test/wf_test_join.erl: All join policies with PAR_FORK + JOIN_WAIT (lines 10-56)
- test/wf_test_cancel.erl: Cancellation scopes with CANCEL_SCOPE (lines 10-71)
- test/wf_test_mi.erl: Multiple instance patterns (lines 18-68) - includes non-existent header (line 11)

**Tracing Infrastructure:**
- src/wf_trace.erl: Trace event collection with levels (none/min/full), sinks (ets/process/callback/replay) (lines 1-335)
- test/wf_test_trace_helpers.erl: run_with_trace/3 executes workflow and returns {DoneState, Events} (lines 63-98)
- Trace event record: #trace_event{} with step_seq, opcode, state_before, state_after, timestamp, scope, branch_id, metadata (wf_trace.erl:10-19)

**Current Patterns:**
- docs/PATTERNS.md: Maps 16 implemented patterns to opcodes and test modules (lines 21-42)
- Kernel primitives directly supported (SEQ_ENTER, PAR_FORK, etc.)
- Derived patterns combine kernel primitives (discriminator = PAR_FORK + JOIN_WAIT(first_complete))

**Limitations to Document:**
1. **Cancellation stubbed**: wf_exec.erl:512-529 returns false/noop (awaiting item 008)
2. **MI not implemented**: MI_SPAWN opcode has no handler (awaiting item 009)
3. **Scheduler missing**: wf_sched module doesn't exist, scheduler policy ignored (wf_test_trace_helpers.erl:69)
4. **Effect system missing**: No way to execute external effects (awaiting item 010)

### Integration Points

**Example modules should:**
- Use wf_exec:new/1 to create executor from bytecode (wf_exec.erl:80-101)
- Use wf_exec:run/3 to execute workflow (wf_exec.erl:149-166)
- Use wf_trace:new/1 to create trace state (wf_trace.erl:80-94)
- Use erlang:put(wf_trace_state, TraceState) to enable tracing (wf_test_trace_helpers.erl:84)
- Use wf_trace:get_events/1 to retrieve trace events (wf_trace.erl:129-142)
- Follow naming convention: wf_example_*.erl for examples, wf_test_examples.erl for tests

**Testing infrastructure:**
- test/wf_test_examples.erl should use EUnit (include_lib("eunit/include/eunit.hrl"))
- Each test should call example's run/0 and validate trace structure
- Use wf_test_trace_helpers:compare_traces/2 or manual structure validation

## Key Files

### Core Executor
- `src/wf_exec.erl:80-101` - wf_exec:new/1 creates executor from bytecode
- `src/wf_exec.erl:149-166` - wf_exec:run/3 executes workflow with quanta limit
- `src/wf_exec.erl:187-208` - execute_opcode/2 dispatch table showing all opcodes
- `src/wf_exec.erl:231-239` - execute_task_exec/2 shows mock task behavior (updates ctx)

### Bytecode Definitions
- `src/wf_vm.erl:9-21` - opcode() type definition showing all opcode formats
- `src/wf_vm.erl:24-29` - join_policy() types (all, first_complete, etc.)
- `src/wf_vm.erl:38-40` - mi_policy() types (fixed, dynamic)

### Test References (patterns to follow)
- `test/wf_test_seq.erl:17-24` - Simple sequence bytecode example
- `test/wf_test_par.erl:17-25` - Parallel fork + join example
- `test/wf_test_xor.erl:17-23` - XOR choice example
- `test/wf_test_join.erl:34-43` - Discriminator (first_complete) example
- `test/wf_test_cancel.erl:10-15` - Cancel scope example

### Tracing
- `src/wf_trace.erl:80-94` - wf_trace:new/1 creates trace state
- `src/wf_trace.erl:129-142` - wf_trace:get_events/1 retrieves events
- `test/wf_test_trace_helpers.erl:63-98` - run_with_trace/3 shows full tracing workflow

### Pattern Documentation
- `docs/PATTERNS.md:21-42` - Pattern table mapping patterns to opcodes
- `docs/PATTERNS.md:88-99` - Discriminator derived pattern example
- `docs/PATTERNS.md:101-112` - N-out-of-M join example

### Validation
- `src/wf_validate.erl:635-652` - wf_validate:check_soundness/1 validates bytecode
- Can be used in examples to show workflow validation before execution

## Technical Considerations

### Dependencies

**Internal modules to use:**
- wf_exec: Executor (new/1, run/3, is_done/1)
- wf_vm: Opcode types (wf_bc(), opcode(), join_policy())
- wf_trace: Tracing (new/1, get_events/1, set_level/1)
- wf_validate: Optional validation (validate_bytecode/1)

**External dependencies:**
- eunit: For test module (wf_test_examples.erl)
- No external deps (pure Erlang/OTP)

**Limitations to work around:**
1. **MI_SPAWN not implemented**: Use PAR_FORK to simulate multiple instances or document as placeholder
2. **Cancellation stubbed**: Document that cancel region example shows structure but won't actually cancel
3. **No scheduler**: Ignore scheduler policy (pass undefined to wf_exec:run/3)
4. **No effects**: Simulate effects via TASK_EXEC opcodes that update context

### Patterns to Follow

**From test modules:**
1. **Bytecode structure**: List of opcodes as tagged tuples: `{'OPCODE_NAME', Arg}`
2. **Module layout**: Export bytecode generators, include EUnit for tests
3. **Helper functions**: Use wf_test_helpers.erl patterns (exec_until_done/1, count_tokens_with_status/2)
4. **Commentary**: Add comments explaining what each opcode does (see wf_test_seq.erl:17-24)

**From PROMPT.md:**
- Examples should be narrative and educational (not just unit tests)
- Each example should demonstrate a specific workflow pattern
- Use mock effects to simulate real behavior
- Print trace to show execution flow

**From item 017 (pattern-coverage-docs):**
- Distinguish kernel primitives (direct opcodes) from derived patterns
- Map to van der Aalst pattern names (Sequence, Discriminator, etc.)
- Reference specific file:line numbers for implementations

### Module Structure

**Each example module (wf_example_*.erl):**
```erlang
-module(wf_example_<name>).
-export([term/0, run/0, expected_trace_structure/0]).
-include_lib("kernel/include/logger.hrl").

%% @doc Returns the workflow bytecode term
term() ->
    [
        {'OPCODE', arg},
        ...
    ].

%% @doc Executes the workflow and returns trace
run() ->
    Bytecode = term(),
    {ok, TraceState} = wf_trace:new(full),
    erlang:put(wf_trace_state, TraceState),
    ExecState = wf_exec:new(Bytecode),
    {done, DoneState} = wf_exec:run(ExecState, 1000, undefined),
    Events = wf_trace:get_events(TraceState),
    {ok, Events}.

%% @doc Expected sequence of opcode categories
expected_trace_structure() ->
    [seq_enter, task_exec, xor_choose, task_exec, done, ...].
```

**Test module (wf_test_examples.erl):**
```erlang
-module(wf_test_examples).
-include_lib("eunit/include/eunit.hrl").

example_basic_trace_test() ->
    {ok, Events} = wf_example_basic:run(),
    Expected = wf_example_basic:expected_trace_structure(),
    Actual = [extract_category(E) || E <- Events],
    ?assertEqual(Expected, Actual).

extract_category(#trace_event{opcode = {OpName, _Arg}}) ->
    OpName;
extract_category(#trace_event{opcode = OpName}) when is_atom(OpName) ->
    OpName.
```

## Risks and Mitigations

| Risk     | Impact            | Mitigation       |
| -------- | ----------------- | ---------------- |
| MI_SPAWN opcode not implemented | High - wf_example_multiple_instances won't run | Use PAR_FORK to simulate MI, or document as placeholder awaiting item 009 |
| Cancellation is stubbed | Medium - wf_example_cancel_region won't actually cancel | Document limitation, show bytecode structure only |
| No real effect system | Low - Can't demonstrate external effects | Simulate effects via TASK_EXEC opcodes that update context |
| Trace format may change | Medium - expected_trace_structure/0 may break | Use opcode category (atom) not full opcode tuple for comparison |
| Examples might be too simple | Low - Won't demonstrate real complexity | Add nested patterns, use realistic scenarios (vending machine, approval workflow) |
| Test infrastructure gaps | Medium - wf_test_trace_helpers assumes features | Keep tests simple: compare opcode categories, not full trace events |

## Recommended Approach

### Phase 1: Create Basic Example (wf_example_basic.erl)

**Pattern: Sequence + XOR Choice (Vending Machine)**

1. Implement term/0 returning bytecode:
   ```erlang
   term() ->
       [
           {'SEQ_ENTER', 0},
           {'TASK_EXEC', insert_coin},
           {'XOR_CHOOSE', [3, 6]},  % Select drink OR refund
           {'SEQ_ENTER', 0},
           {'TASK_EXEC', select_drink},
           {'SEQ_NEXT', 5},
           {'TASK_EXEC', dispense},
           {'DONE'},
           {'TASK_EXEC', refund},
           {'DONE'}
       ].
   ```

2. Implement run/0 that:
   - Creates trace state with wf_trace:new(full)
   - Stores in process dictionary
   - Runs workflow with wf_exec:run/3
   - Retrieves events with wf_trace:get_events/1
   - Prints trace with io:format
   - Returns {ok, Events}

3. Implement expected_trace_structure/0:
   ```erlang
   expected_trace_structure() ->
       [seq_enter, task_exec, xor_choose, seq_enter, task_exec,
        seq_next, task_exec, done].
   ```

4. Add comments explaining:
   - SEQ_ENTER starts sequential scope
   - TASK_EXEC executes task (mock: updates context)
   - XOR_CHOOSE selects one branch (first: drink, second: refund)
   - SEQ_NEXT jumps to next sequence element
   - DONE completes token

### Phase 2: Create Discriminator Example (wf_example_discriminator.erl)

**Pattern: Parallel Split + First Complete Join**

1. Implement term/0:
   ```erlang
   term() ->
       [
           {'PAR_FORK', [1, 3, 5]},  % 3 approval tasks
           {'TASK_EXEC', approve_manager},
           {'DONE'},
           {'TASK_EXEC', approve_director},
           {'DONE'},
           {'TASK_EXEC', approve_vp},
           {'DONE'},
           {'JOIN_WAIT', first_complete},  % First approval cancels rest
           {'TASK_EXEC', notify_result},
           {'DONE'}
       ].
   ```

2. Implement run/0 with trace printing (same as basic)

3. Implement expected_trace_structure/0:
   - PAR_FORK creates 3 tokens
   - First branch completes (3 opcodes)
   - JOIN_WAIT on first_complete
   - Remaining 2 tokens cancelled (automatic)
   - Final task executes

4. Add comments explaining:
   - PAR_FORK spawns parallel branches
   - JOIN_WAIT with first_complete acts as discriminator
   - Excess instances cancelled automatically (when item 008 implemented)
   - Simulate varying completion times via task ordering in bytecode

### Phase 3: Create Cancel Region Example (wf_example_cancel_region.erl)

**Pattern: Cancel Scope with Long-Running Subprocess**

1. Implement term/0:
   ```erlang
   term() ->
       [
           {'CANCEL_SCOPE', {enter, main_flow}},
           {'TASK_EXEC', start_process},
           {'CANCEL_SCOPE', {enter, cancel_region}},
           {'SEQ_ENTER', 0},
           {'TASK_EXEC', long_task},
           {'TASK_EXEC', another_long_task},
           {'SEQ_NEXT', 7},
           {'DONE'},
           {'CANCEL_SCOPE', {exit, cancel_region}},
           {'TASK_EXEC', continue_main_flow},
           {'CANCEL_SCOPE', {exit, main_flow}}
       ].
   ```

2. Implement run/0:
   - Execute partially (stop mid-region)
   - Call wf_exec:cancel/1 (currently stubbed)
   - Show that cancellation doesn't work yet (document limitation)
   - Print trace showing CANCEL_SCOPE opcodes

3. Implement expected_trace_structure/0:
   - Include cancel_scope_enter, cancel_scope_exit opcodes
   - Note: actual cancellation won't occur (stubbed)

4. Add comments explaining:
   - CANCEL_SCOPE marks region boundaries
   - External cancel signal would trigger region cancellation
   - Currently stubbed (wf_exec.erl:512-529)
   - Main flow should continue after region cancelled

### Phase 4: Create Multiple Instances Example (wf_example_multiple_instances.erl)

**Pattern: Dynamic MI with Wait-N Join**

1. Implement term/0:
   ```erlang
   term() ->
       [
           %% Simulate MI with PAR_FORK (workaround for missing MI_SPAWN)
           {'PAR_FORK', [1, 3, 5, 7, 9]},  % 5 instances
           {'TASK_EXEC', process_item_1},
           {'DONE'},
           {'TASK_EXEC', process_item_2},
           {'DONE'},
           {'TASK_EXEC', process_item_3},
           {'DONE'},
           {'TASK_EXEC', process_item_4},
           {'DONE'},
           {'TASK_EXEC', process_item_5},
           {'DONE'},
           {'JOIN_WAIT', {wait_n, 3}},  % Wait for 3 of 5
           {'TASK_EXEC', aggregate_results},
           {'DONE'}
       ].
   ```

2. Implement run/0:
   - Show that 5 parallel tasks execute
   - Only 3 complete before join satisfied
   - Remaining 2 cancelled (when join policy works)
   - Print trace showing token counts

3. Implement expected_trace_structure/0:
   - PAR_FORK, 5 parallel branches
   - First 3 branches complete
   - JOIN_WAIT with wait_n satisfied
   - Remaining branches cancelled
   - Final aggregation task

4. Add comments explaining:
   - MI_SPAWN opcode not implemented (awaiting item 009)
   - Using PAR_FORK as workaround
   - JOIN_WAIT with {wait_n, 3} waits for 3 of 5
   - Excess instances cancelled automatically
   - Dynamic count from ctx not possible without MI_SPAWN

### Phase 5: Create Test Module (wf_test_examples.erl)

1. Import example modules and EUnit

2. For each example, write test:
   ```erlang
   example_basic_trace_structure_test() ->
       {ok, Events} = wf_example_basic:run(),
       Expected = wf_example_basic:expected_trace_structure(),
       Actual = [extract_category(E) || E <- Events],
       ?assertEqual(Expected, Actual).
   ```

3. Helper function extract_category/1:
   ```erlang
   extract_category(#trace_event{opcode = {OpName, _Arg}}) ->
       list_to_atom(string:lowercase(atom_to_list(OpName)));
   extract_category(#trace_event{opcode = OpName}) when is_atom(OpName) ->
       list_to_atom(string:lowercase(atom_to_list(OpName))).
   ```

4. Add tests for:
   - Trace structure matches expected
   - Workflow completes successfully
   - All tokens in final state are complete or cancelled as expected

### Phase 6: Documentation and Integration

1. Add examples to README or examples.md:
   - Explain each pattern
   - Show how to run examples
   - Link to pattern documentation (docs/PATTERNS.md)

2. Update docs/PATTERNS.md:
   - Add reference to example modules
   - Link examples to patterns they demonstrate

3. Update docs/TESTING.md:
   - Add section on running examples
   - Explain trace structure validation

## Open Questions

1. **Should examples use PAR_FORK to simulate MI, or document MI_SPAWN placeholder?**
   - Option A: Use PAR_FORK workaround (example runs but doesn't demonstrate MI)
   - Option B: Use MI_SPAWN opcode (example shows correct pattern but won't run)
   - Recommendation: Use PAR_FORK with comments explaining limitation

2. **How should examples handle cancellation being stubbed?**
   - Option A: Skip cancel region example entirely
   - Option B: Implement with warning that it won't actually cancel
   - Recommendation: Implement with clear documentation of limitation

3. **Should run/0 print trace or just return it?**
   - Option A: Print trace with io:format (more demo-friendly)
   - Option B: Return trace only (more programmatic)
   - Recommendation: Print AND return trace

4. **Should examples validate bytecode before execution?**
   - Option A: Always call wf_validate:validate_bytecode/1
   - Option B: Skip validation (simpler examples)
   - Recommendation: Call validator in run/0 and print warnings

5. **How to handle XOR branch selection in expected_trace_structure?**
   - XOR nondeterministically selects branch (but scheduler is deterministic)
   - Should expected structure include both branches or just selected one?
   - Recommendation: Match only selected branch (deterministic with current stubbed scheduler)

6. **Should examples use trace level=min or full?**
   - Option A: min (shows only structural events: PAR_FORK, JOIN_WAIT, DONE)
   - Option B: full (shows all opcodes including SEQ_ENTER, TASK_EXEC)
   - Recommendation: Use full to show complete execution flow
