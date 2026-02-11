# Research: Fix test generator crash in test/wf_exec_tests.erl: join_wait_test_/0 fails with badmatch error. The wf_exec:step/2 function returns a tuple that does not match the expected {ExecState, Trace} pattern — likely the PAR_FORK or JOIN_WAIT opcodes return additional data. Check the actual return type of wf_exec:step/2 for parallel opcodes and fix the test to match. The 5 cancelled tests in rebar3 eunit are from this generator crash. Target: 0 failures, 0 cancelled.

**Date**: 2025-01-11
**Item**: 035-fix-test-generator-crash-in-testwfexectestserl-joi

## Research Question

The `join_wait_test_/0` test generator in `test/wf_exec_tests.erl` crashes with a badmatch error. The issue description suggests that `wf_exec:step/2` returns a tuple that doesn't match the expected `{ExecState, Trace}` pattern, possibly related to PAR_FORK or JOIN_WAIT opcodes returning additional data. The goal is to identify the root cause and fix it to achieve 0 failures and 0 cancelled tests in rebar3 eunit.

## Summary

After comprehensive analysis of the `wf_exec` module and test suite, I've identified that the issue is NOT with `wf_exec:step/2` returning an unexpected tuple type - the function correctly returns `{exec_state(), map()}` in all code paths. The actual problem is a fundamental architecture issue in the multi-token executor where `ExecState.ip` (the global instruction pointer) becomes desynchronized from `Token.ip` (the per-token instruction pointer) during parallel execution.

The root cause manifests in two ways:

1. **IP Desynchronization**: When `execute_done` switches to the next token (line 804, 808, 849-850), it updates `current_token` but does NOT update `ExecState.ip` to match the new token's IP. This causes `fetch_opcode` to fetch the wrong instruction.

2. **Missing blocked_join Handling**: When `execute_join_wait` sets status to `blocked_join` (line 600), the next call to `step/2` calls `step_normal/2` instead of handling the blocked state, because `step/2` only checks for `blocked_effect` status (line 154).

The test crashes because the executor attempts to execute instructions at incorrect IPs, leading to state mismatches and eventually a badmatch error when trying to access empty join counters or execute invalid opcodes.

## Current State Analysis

### Existing Implementation

**wf_exec:step/2 Return Type** (src/wf_exec.erl:151-158)
- The function signature is: `-spec step(exec_state(), term()) -> {exec_state(), map()}.`
- All code paths return a 2-tuple: `{exec_state(), map()}`
- `step_normal/2` (line 217-221) wraps opcode execution results in a tuple with a trace event
- `step_check_effect/2` (line 161-214) always returns `{ExecState, TraceEvent}`

**Multi-Token Executor Architecture** (src/wf_exec.erl:32-44)
- `exec_state` record contains both `ip` (global IP) and `tokens` (map of token_id to token records)
- Each `token` record has its own `ip` field (wf_exec.hrl:7-15)
- `current_token` field tracks which token is currently being executed
- `fetch_opcode/2` (line 263-269) uses `ExecState#exec_state.ip`, NOT the current token's IP

**PAR_FORK Execution** (src/wf_exec.erl:404-468)
- Spawns N tokens at different IPs (e.g., IP 1 and IP 3)
- Creates branch_info and join_counter records
- Sets `ExecState.ip = ExecState#exec_state.ip + 1` (line 456), which points to the first branch's IP
- Sets `current_token = NextToken` (line 467)

**Token Completion** (src/wf_exec.erl:771-855)
- `execute_done/1` handles token completion
- When a parallel branch completes, it increments the join counter (line 784)
- Selects the next active token (lines 804, 849)
- **CRITICAL BUG**: Does NOT update `ExecState.ip` when switching tokens (lines 805-810, 850-853)

**JOIN_WAIT Execution** (src/wf_exec.erl:565-603)
- Checks if join counter is satisfied (line 570)
- If satisfied: removes join counter, creates continuation token, advances IP (lines 571-596)
- If not satisfied: sets status to `blocked_join` (line 600)
- **CRITICAL BUG**: `step/2` doesn't handle `blocked_join` status (only checks `blocked_effect`)

### Current Patterns and Conventions

**Single-Token Executor** (works correctly)
- `ExecState.ip` tracks the current instruction pointer
- Sequential execution advances IP through the bytecode
- Test examples: `single_task_test_`, `sequence_test_`, `done_test_`

**Multi-Token Executor** (broken)
- Multiple tokens exist with different IPs
- `ExecState.ip` should sync with `current_token.ip` but doesn't
- Test examples: `par_fork_test_`, `join_wait_test_`, `mi_*_test_`

**Integration Points**
- `wf_sched:select_action/2` (line 241) - scheduler selects which action to take
- `wf_cancel:propagate/2` (line 764) - cancellation propagation
- `wf_mi:spawn_instances/4` (line 475) - multiple instance spawning
- `wf_effect:yield/4` (line 342) - effect yielding

## Key Files

- `src/wf_exec.erl:151-158` - `step/2` function entry point, only handles `blocked_effect` status
- `src/wf_exec.erl:216-221` - `step_normal/2` wraps opcode execution in tuple
- `src/wf_exec.erl:263-269` - `fetch_opcode/2` uses `ExecState.ip` not token.ip
- `src/wf_exec.erl:391-397` - `select_next_token/1` returns first active token or undefined
- `src/wf_exec.erl:404-468` - `execute_par_fork/2` spawns parallel tokens
- `src/wf_exec.erl:565-603` - `execute_join_wait/2` handles join completion or blocking
- `src/wf_exec.erl:771-855` - `execute_done/1` handles token completion, doesn't sync IP
- `src/wf_exec.erl:606-611` - `find_active_join/1` assumes join_counters is non-empty, will badmatch if empty
- `src/wf_exec.hrl:7-44` - Record definitions for exec_state, token, branch_info, join_counter
- `test/wf_exec_tests.erl:109-125` - `join_wait_test_/0` - the failing test
- `test/wf_exec_tests.erl:12-20` - `mock_bytecode_par/0` - bytecode for parallel execution test

## Technical Considerations

### Dependencies

**External Dependencies**
- None identified for the core issue

**Internal Modules**
- `wf_sched` - scheduler for action selection (line 241)
- `wf_cancel` - cancellation propagation (line 764)
- `wf_mi` - multiple instance management (line 475, 815-817)
- `wf_effect` - effect yielding and results (line 166, 342)

### Patterns to Follow

**Existing IP Management Pattern** (in single-token executor)
- Each opcode handler updates `ExecState.ip` appropriately
- Example: `execute_task_exec/2` (line 337): `ip = ExecState#exec_state.ip + 1`
- Example: `execute_seq_next/2` (line 318): `ip = TargetIP`

**Token Switching Pattern** (broken, needs fixing)
- When switching tokens, must update `ExecState.ip` to match new token's IP
- Locations needing fix: `execute_par_fork/2` (line 467), `execute_mi_spawn/2` (line 554), `execute_done/1` (lines 808, 851)

**Blocked State Handling Pattern** (partially implemented)
- `step_check_effect/2` (line 161-214) shows how to handle blocked states
- Pattern: Check status in `step/2`, dispatch to specialized handler
- Missing: Handler for `blocked_join` status

## Risks and Mitigations

| Risk | Impact | Mitigation |
| -------- | ----------------- | ---------------- |
| IP desynchronization causes wrong instruction execution | High | Add IP sync when switching tokens; update `step_normal/2` to sync IP before fetching opcode |
| `blocked_join` status unhandled causes executor to continue when blocked | High | Add `blocked_join` case to `step/2`; create `step_check_join/2` handler |
| Empty join_counters causes badmatch in `find_active_join/1` | Medium | Add guard clause to handle empty join_counters; return error or special status |
| `current_token` becomes undefined when no active tokens exist | Medium | Add check in `step_normal/2` to handle undefined current_token; return done status |
| Fixing IP sync might break single-token executor | Low | Ensure IP sync only applies in multi-token mode; check if tokens map has multiple entries |

## Recommended Approach

Based on research findings, the fix requires two main changes:

### 1. Fix IP Synchronization

**Option A: Sync in step_normal/2**
- Modify `step_normal/2` to sync `ExecState.ip` with `current_token.ip` before calling `fetch_opcode`
- Add helper function: `sync_ip_with_token/1` that updates ExecState.ip from current_token.ip
- Pros: Centralized fix, handles all token switching cases
- Cons: Adds overhead to every step

**Option B: Sync at token switching points**
- Update `execute_done/1`, `execute_par_fork/2`, `execute_mi_spawn/2` to sync IP when updating `current_token`
- Add: `ip = maps:get(NextToken, Tokens)#token.ip` to ExecState updates
- Pros: Minimal overhead, explicit at token switch points
- Cons: Multiple locations to update, easier to miss one

**Recommended**: Option A with Option B as fallback - sync in `step_normal/2` for safety, and also add explicit sync in token switch locations for clarity.

### 2. Handle blocked_join Status

**Add to step/2 function** (src/wf_exec.erl:152-158):
```erlang
step(ExecState, SchedDecision) ->
    case ExecState#exec_state.status of
        blocked_effect ->
            step_check_effect(ExecState, SchedDecision);
        blocked_join ->
            step_check_join(ExecState, SchedDecision);
        _ ->
            step_normal(ExecState, SchedDecision)
    end.
```

**Create step_check_join/2 function**:
- Check if any join counters are satisfied
- If satisfied: create continuation token and transition to running
- If not satisfied: remain blocked and yield
- Similar pattern to `step_check_effect/2`

### 3. Defensive Error Handling

**Add guards to find_active_join/1** (src/wf_exec.erl:606-611):
```erlang
find_active_join(#exec_state{join_counters = JoinCounters}) ->
    case maps:keys(JoinCounters) of
        [] -> error({no_active_join, join_counters_empty});
        [JoinId | _] -> JoinId
    end.
```

**Handle undefined current_token** in `step_normal/2`:
```erlang
step_normal(ExecState, SchedDecision) ->
    case ExecState#exec_state.current_token of
        undefined ->
            %% No active tokens, check if done
            case maps:values(ExecState#exec_state.tokens) of
                [] -> {ExecState#exec_state{status = done}, #{type => done, reason => no_tokens}};
                _ -> {ExecState, #{type => blocked, reason => no_active_token}}
            end;
        _TokenId ->
            %% Sync IP with current token
            SyncedExecState = sync_ip_with_current_token(ExecState),
            Opcode = fetch_opcode(SyncedExecState),
            NewExecState = execute_opcode(Opcode, SyncedExecState, SchedDecision),
            TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
            {NewExecState, TraceEvent}
    end.
```

## Open Questions

1. **Architecture Decision**: Should `ExecState.ip` track the global instruction pointer OR the current token's instruction pointer? The current code mixes both approaches inconsistently.

2. **Token IP Usage**: The `token.ip` field is set but never read by `fetch_opcode`. Is this field intended for future use, or should `fetch_opcode` be using it instead of `ExecState.ip`?

3. **blocked_join Semantics**: When the executor is in `blocked_join` status, what should cause it to unblock? Should it check join counters on every step, or should tokens notify the executor when they complete?

4. **Test Coverage**: The `join_wait_test_/0` test expects 6 steps to execute successfully. What is the expected state after each step? We need to trace through the expected execution to verify the fix.

5. **Scheduler Integration**: How should the scheduler (`wf_sched:select_action/2`) interact with the multi-token executor? Should it select which token to execute, or should the executor handle token selection internally?

6. **Performance Impact**: If we add IP synchronization to every step (Option A), what's the performance impact? Should we optimize by only syncing when tokens switch?

7. **Backward Compatibility**: Will fixing the IP synchronization break existing tests that rely on the current (broken) behavior? We need to audit all tests that use parallel execution.
