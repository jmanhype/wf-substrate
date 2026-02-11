# Research: Implement determinism and replay test suites

**Date**: 2025-01-18
**Item**: 015-determinism-and-replay-tests

## Research Question

Implement test suites validating determinism and replay correctness:

Determinism tests:
- Run the same workflow case twice with the deterministic scheduler policy and identical inputs.
- Assert that both executions produce identical trace event sequences (same step_seq, same opcodes, same state transitions).
- Test with: simple sequence, parallel split+join, nested par in seq, XOR with deterministic signal, MI with fixed count.
- Verify that deterministic policy produces stable ordering regardless of Erlang scheduler timing.

Replay tests:
- Run a workflow case with the nondeterministic scheduler policy (random seed).
- Record the scheduler choice log and any effect results.
- Create a replay scheduler from the recorded log.
- Re-run the same case with the replay scheduler.
- Assert that the replay produces an identical trace to the original run.
- Test replay divergence detection: modify the workflow term slightly, attempt replay with old log, verify the replay scheduler detects divergence and fails with a clear error.

Receipt stability tests:
- For pure steps (no real effects, mock effect handler), verify that receipts are identical across replay runs.
- Verify receipt hashes match for the same effect spec.
- Verify that replayed effects with idempotency keys return cached results.

All tests use EUnit. Include helper functions for comparing traces and formatting diff output on failure.

## Summary

This item requires implementing comprehensive test suites for three critical areas: determinism validation, replay correctness, and receipt stability. The codebase already has solid foundations for all three areas:

1. **Determinism testing**: The `wf_sched_deterministic` module (wf_sched_deterministic.erl:18-75) implements stable sorting of enabled actions using `lists:sort/2` with a deterministic comparison function. Tests need to verify that running workflows with this scheduler produces identical traces across multiple executions.

2. **Replay testing**: The scheduler infrastructure provides complete replay support through `wf_sched_nondeterministic` (wf_sched_nondeterministic.erl:18-91) which logs choices, and `wf_sched_replay` (wf_sched_replay.erl:18-117) which replays them with divergence detection. Tests need to verify full-cycle: nondeterministic run → log extraction → replay → trace comparison.

3. **Receipt stability**: The trace system (wf_trace.erl:10-27) defines `#replay_entry{}` records that can store effect results. However, effect handling and receipt generation is not yet fully implemented in the executor. This will require mocking effects for testing purposes.

All tests should follow the existing EUnit patterns in the codebase (wf_test_seq.erl, wf_test_par.erl, wf_test_xor.erl, wf_test_mi.erl) which use mock bytecode generators and helper functions from wf_test_helpers.erl.

## Current State Analysis

### Existing Implementation

**Scheduler Infrastructure:**
- **wf_sched.erl** (lines 1-174): Defines the scheduler behavior interface and unified API
  - `sched_policy()` type: `deterministic | nondeterministic | {replay, choice_log()}` (wf_sched.erl:25-28)
  - `choose/2` and `choose_with_entry/2` functions for action selection (wf_sched.erl:84-114)
  - `get_log/1` extracts choice log from nondeterministic scheduler (wf_sched.erl:117-124)
  - `from_log/1` creates replay scheduler from recorded log (wf_sched.erl:127-129)

- **wf_sched_deterministic.erl** (lines 1-75): Implements deterministic scheduler
  - Stateless: returns same state unchanged (wf_sched_deterministic.erl:22)
  - `stable_sort/1` uses `lists:sort/2` with `compare_actions/2` (wf_sched_deterministic.erl:50-51)
  - Sorts by type first (token < xor_branch), then by ID (wf_sched_deterministic.erl:54-65)
  - Handles refs by converting to binary for deterministic ordering (wf_sched_deterministic.erl:69-71)

- **wf_sched_nondeterministic.erl** (lines 1-91): Implements nondeterministic scheduler
  - Optional seed parameter for reproducibility (wf_sched_nondeterministic.erl:22-30)
  - Uses `rand:uniform_s/2` for explicit state handling (wf_sched_nondimitistic.erl:48)
  - Logs every choice as `{StepSeq, EnabledSet, Chosen}` (wf_sched_nondeterministic.erl:52)
  - Maintains step sequence counter in state (wf_sched_nondeterministic.erl:36-37)

- **wf_sched_replay.erl** (lines 1-117): Implements replay scheduler
  - Validates log format on creation (wf_sched_replay.erl:22, 91-105)
  - Tracks position in log for sequential replay (wf_sched_replay.erl:28)
  - Validates enabled sets match exactly using `validate_enabled_set/2` (wf_sched_replay.erl:50-56)
  - Returns `{error, {divergence, Expected, Actual}}` on mismatch (wf_sched_replay.erl:56)

**Tracing Infrastructure:**
- **wf_trace.erl** (lines 1-335): Comprehensive tracing system
  - `#trace_event{}` record with step_seq, opcode, state_before/after, timestamp (wf_trace.erl:10-19)
  - `#replay_entry{}` for scheduler choices and effect results (wf_trace.erl:21-26)
  - Three trace levels: none, min, full (wf_trace.erl:38)
  - ETS table sink for event storage (wf_trace.erl:81-94)
  - `to_replay_log/1` extracts scheduler choices and effect results (wf_trace.erl:183-206)
  - `from_replay_log/1` creates trace state from replay log (wf_trace.erl:209-225)

**Executor Integration:**
- **wf_exec.erl** (lines 1-783): Main execution engine
  - `snapshot_exec_state/1` serializes state to binary (wf_exec.erl:761-764)
  - `restore_exec_state/2` deserializes with bytecode validation (wf_exec.erl:767-781)
  - `step/2` emits trace events with scheduler choice metadata (wf_exec.erl:113-137)
  - Each trace event includes opcode, state snapshots, scheduler decision (wf_exec.erl:124-130)

**Existing Test Patterns:**
- **wf_sched_tests.erl** (lines 1-362): Comprehensive scheduler unit tests
  - Tests deterministic stable ordering with refs (wf_sched_tests.erl:26-38)
  - Tests nondeterministic seed reproducibility (wf_sched_tests.erl:124-137)
  - Tests replay divergence detection (wf_sched_tests.erl:176-185)
  - Tests full cycle: nondeterministic → log → replay (wf_sched_tests.erl:277-300)

- **Pattern test modules** (wf_test_seq.erl, wf_test_par.erl, wf_test_xor.erl, wf_test_mi.erl):
  - Mock bytecode generators for each pattern (e.g., wf_test_seq.erl:10-62)
  - Tests use `wf_exec:new/1`, `wf_exec:run/3`, `wf_exec:is_done/1` (wf_test_seq.erl:70-86)
  - Helper functions in wf_test_helpers.erl: `exec_until_done/1`, `exec_steps/2` (wf_test_helpers.erl:15-28)

**Trace Testing:**
- **wf_trace_tests.erl** (lines 1-335): Basic trace infrastructure tests
  - Tests trace levels (none, min, full) (wf_trace_tests.erl:12-62)
  - Tests different sinks (callback, ETS, process) (wf_trace_tests.erl:68-118)
  - Tests state snapshot/restore (wf_trace_tests.erl:124-154)
  - Tests replay log extraction (wf_trace_tests.erl:208-259)
  - Integration tests verify events are emitted during execution (wf_trace_tests.erl:265-322)

### Gaps and Missing Pieces

1. **No integration tests for determinism**: While scheduler tests verify stable ordering, there are no end-to-end tests that run full workflows with deterministic scheduler and compare complete traces.

2. **No integration tests for replay**: Scheduler tests verify replay mechanics, but there are no tests that run a workflow with nondeterministic scheduler, extract the log, replay it, and compare traces.

3. **No trace comparison helpers**: No utility functions for comparing two trace event lists and formatting diffs on failure.

4. **Receipt testing deferred**: Effect handling and receipts are not yet implemented (see item 010-effect-boundary-and-receipts). Item 015 should focus on determinism and replay tests with mock effects.

## Key Files

- `src/wf_sched.erl:1-174` - Scheduler behavior and unified API
- `src/wf_sched_deterministic.erl:1-75` - Deterministic policy implementation
- `src/wf_sched_nondeterministic.erl:1-91` - Nondeterministic policy with logging
- `src/wf_sched_replay.erl:1-117` - Replay policy with divergence detection
- `src/wf_trace.erl:1-335` - Tracing system with replay support
- `src/wf_exec.erl:113-137, 761-781` - Trace emission in executor
- `test/wf_sched_tests.erl:1-362` - Scheduler unit tests (reference for patterns)
- `test/wf_trace_tests.erl:1-335` - Trace infrastructure tests
- `test/wf_test_seq.erl:1-179` - Sequence pattern tests (bytecode generator pattern)
- `test/wf_test_par.erl:1-218` - Parallel pattern tests
- `test/wf_test_xor.erl:1-148` - XOR choice pattern tests
- `test/wf_test_mi.erl:1-208` - Multiple instance pattern tests
- `test/wf_test_helpers.erl:1-60` - Test utility functions

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- `wf_sched`: Scheduler policies (deterministic, nondeterministic, replay)
- `wf_exec`: Workflow executor with trace emission
- `wf_trace`: Trace collection, filtering, replay log extraction
- `wf_test_seq`, `wf_test_par`, `wf_test_xor`, `wf_test_mi`: Mock bytecode generators

**External dependencies:**
- `eunit`: Built-in Erlang testing framework (already in rebar.config)
- No additional dependencies required

### Patterns to Follow

**Test Module Structure:**
```erlang
-module(wf_test_determinism).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").
-include("../src/wf_trace.hrl").
```

**Mock Bytecode Generators (from wf_test_seq.erl:10-62):**
```erlang
%% Simple sequence for baseline determinism test
mock_bytecode_simple_seq() ->
    [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ].
```

**Test Execution Pattern (from wf_test_seq.erl:68-86):**
```erlang
determinism_simple_seq_test() ->
    Bytecode = mock_bytecode_simple_seq(),
    {ok, TraceState1} = wf_trace:new(full),
    ExecState1 = wf_exec:new(Bytecode),
    {done, DoneState1} = wf_exec:run(ExecState1, 100, deterministic),
    Events1 = wf_trace:get_events(TraceState1),

    %% Run again
    {ok, TraceState2} = wf_trace:new(full),
    ExecState2 = wf_exec:new(Bytecode),
    {done, DoneState2} = wf_exec:run(ExecState2, 100, deterministic),
    Events2 = wf_trace:get_events(TraceState2),

    ?assertEqual(Events1, Events2).
```

**Helper Functions Pattern (from wf_test_helpers.erl:15-28):**
```erlang
%% @doc Execute until done (max 1000 steps)
-spec exec_until_done(wf_exec:exec_state()) -> wf_exec:exec_state().
exec_until_done(ExecState) ->
    case wf_exec:run(ExecState, 1000, undefined) of
        {done, DoneState} -> DoneState;
        {yield, YieldState} -> exec_until_done(YieldState)
    end.
```

**EUnit Setup/Teardown (from TESTING.md:93-104):**
```erlang
setup() ->
    {ok, TraceState} = wf_trace:new(full),
    TraceState.

cleanup(TraceState) ->
    %% Clean up ETS tables if needed
    ok.

my_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(assert_something())
    ]}.
```

### Trace Comparison Helpers Needed

New helper functions for comparing traces:

```erlang
%% @doc Compare two trace event lists, return ok | {error, Diff}
compare_traces(Events1, Events2) ->
    case length(Events1) =:= length(Events2) of
        false ->
            {error, {length_mismatch, length(Events1), length(Events2)}};
        true ->
            compare_events(Events1, Events2, 1, [])
    end.

%% @doc Compare individual events, collect differences
compare_events([], [], _Index, Acc) ->
    case Acc of
        [] -> ok;
        _ -> {error, lists:reverse(Acc)}
    end;
compare_events([E1 | Rest1], [E2 | Rest2], Index, Acc) ->
    Diff = compare_event(E1, E2),
    compare_events(Rest1, Rest2, Index + 1, Acc ++ Diff).

%% @doc Compare single event fields
compare_event(E1, E2) ->
    Checks = [
        fun() -> E1#trace_event.step_seq =:= E2#trace_event.step_seq end,
        fun() -> E1#trace_event.opcode =:= E2#trace_event.opcode end,
        fun() -> E1#trace_event.state_before =:= E2#trace_event.state_before end,
        fun() -> E1#trace_event.state_after =:= E2#trace_event.state_after end
    ],
    [Index || {Index, Check} <- enumerate(Checks), Check() =:= false].
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ------- | --------- |
| **Trace comparison complexity** | High | Implement incremental comparison with clear diff output showing step number and field differences. Use EUnit's `?assertEqual` for readable failure messages. |
| **Timing-dependent failures** | Medium | Use deterministic scheduler for determinism tests. For nondeterministic tests, use fixed random seed ({A, B, C} tuple). |
| **ETS table cleanup** | Medium | Each test creates new trace state with dedicated ETS table. Use setup/teardown to ensure cleanup. Follow pattern in wf_trace_tests.erl:88-96. |
| **Ref comparison instability** | Low | Deterministic scheduler converts refs to binary for sorting (wf_sched_deterministic.erl:69-71). Traces will contain refs which are comparable via =:=. |
| **Replay log exhaustion** | Low | Test both exact replay (log length matches) and premature exhaustion (divergence error). Existing tests show pattern (wf_sched_tests.erl:187-199). |
| **Missing effect system** | Low | Scope receipt tests to "deferred" status. Focus on scheduler choice replay. Mock effects as simple function calls in test bytecode. |
| **Token ID non-determinism** | Low | Even with deterministic scheduler, token IDs are refs. Compare traces by structure (opcodes, state transitions) not by specific ref values. |

## Recommended Approach

### Phase 1: Helper Functions (wf_test_trace_helpers.erl)

Create new helper module for trace comparison:

1. **compare_traces/2**: Compare two trace event lists
   - Check length equality
   - Compare step_seq, opcode, state_before, state_after
   - Return ok | {error, [{StepNum, FieldDiff}]}

2. **format_trace_diff/1**: Format diff for readable output
   - Generate human-readable diff showing mismatched steps
   - Show expected vs actual values

3. **run_with_trace/3**: Execute workflow with trace collection
   - Wrapper: `run_with_trace(Bytecode, SchedPolicy, Options)`
   - Returns `{DoneState, TraceEvents}` for easy comparison

4. **extract_scheduler_choices/1**: Extract choice log from trace
   - Filter events for scheduler_choice metadata
   - Return list compatible with `wf_sched:from_log/1`

### Phase 2: Determinism Tests (wf_test_determinism.erl)

Create comprehensive determinism test suite:

1. **Simple sequence determinism**:
   - Use `wf_test_seq:mock_bytecode_seq_2_tasks()`
   - Run twice with deterministic scheduler
   - Compare traces with `compare_traces/2`

2. **Parallel split+join determinism**:
   - Use `wf_test_par:mock_bytecode_par_3_branches()`
   - Verify deterministic token ordering produces same trace
   - Test with multiple runs (10 iterations) to verify stability

3. **Nested par in seq determinism**:
   - Create bytecode: SEQ_ENTER, PAR_FORK, tasks, JOIN_WAIT, SEQ_NEXT, task
   - Verify nested patterns are deterministic

4. **XOR with deterministic signal**:
   - Use `wf_test_xor:mock_bytecode_xor_3_branches()`
   - Verify same branch always selected with deterministic scheduler
   - Test across 20 runs

5. **MI with fixed count determinism**:
   - Use `wf_test_mi:mock_bytecode_mi_fixed_5()`
   - Verify instance ordering is stable across runs

6. **Erlang scheduler independence**:
   - Spawn multiple concurrent executions
   - Verify all produce identical traces
   - Test with different scheduler loads

### Phase 3: Replay Tests (wf_test_replay.erl)

Create comprehensive replay test suite:

1. **Full cycle replay**:
   - Run workflow with nondeterministic scheduler (seed={1,2,3})
   - Extract choice log with `wf_sched:get_log/1`
   - Create replay scheduler with `wf_sched:from_log/1`
   - Re-run workflow with replay scheduler
   - Compare traces - should be identical

2. **Replay with complex workflows**:
   - Test with parallel split+join (wf_test_par:mock_bytecode_par_3_branches())
   - Test with XOR choice (wf_test_xor:mock_bytecode_xor_3_branches())
   - Test with MI (wf_test_mi:mock_bytecode_mi_fixed_5())

3. **Divergence detection - modified bytecode**:
   - Run workflow with nondeterministic scheduler
   - Extract log
   - Modify bytecode (add task, change branch count)
   - Attempt replay - should fail with {divergence, Expected, Actual}
   - Verify error message is clear

4. **Divergence detection - different enabled sets**:
   - Create two workflows with same structure but different token IDs
   - Run first, extract log
   - Attempt replay on second - should detect divergence

5. **Replay log exhaustion**:
   - Run workflow, extract log
   - Attempt replay with additional steps
   - Should fail with {replay_complete, Position}

6. **Replay idempotency**:
   - Replay same log multiple times
   - All replays should produce identical traces

### Phase 4: Deferred - Receipt Tests (wf_test_receipts.erl)

Create placeholder for future receipt testing:

1. **Mock effect framework**:
   - Simple effect handler that returns {ok, Result}
   - Receipt format: {EffectSpec, IdempotencyKey, Result}
   - No persistence needed for tests

2. **Pure step receipt stability**:
   - Execute workflow with mock effects (no real I/O)
   - Verify receipts are identical across replay runs

3. **Receipt hash matching**:
   - Hash receipt: crypto:hash(sha256, term_to_binary(Receipt))
   - Verify same effect spec produces same hash

4. **Idempotency key caching**:
   - Execute effect with idempotency key
   - Re-execute with same key
   - Verify cached result returned

**Note**: Receipt tests should be marked as "deferred pending effect system implementation" in test comments. The focus for item 015 is determinism and replay.

## Open Questions

1. **Should receipt tests be implemented now or deferred?**
   - Recommendation: Defer to item 010 (effect boundary) completion. Implement placeholder tests with `?_test(skip)` to document intent.

2. **How to handle token ID (ref) comparison in traces?**
   - Recommendation: Compare trace structure (step_seq, opcodes, state transitions) not specific ref values. Refs are deterministic within a single run but different across runs.

3. **Should we test concurrent executions for determinism?**
   - Recommendation: Yes, add test that spawns 10 processes running same workflow with deterministic scheduler, all should produce identical traces (modulo ref values).

4. **What constitutes "identical traces" for replay testing?**
   - Recommendation: Compare step_seq, opcode, state_before, state_after. Exclude timestamp (always different) and metadata (may contain process-specific info).

5. **How to verify "Erlang scheduler independence"?**
   - Recommendation: Spawn concurrent workflows that create scheduler contention. Use `spawn/1` to run multiple executions in parallel, verify deterministic scheduler produces same order.

6. **Should trace helpers be in separate module or added to wf_test_helpers?**
   - Recommendation: Create new `wf_test_trace_helpers.erl` module to keep trace-specific utilities isolated and testable.
