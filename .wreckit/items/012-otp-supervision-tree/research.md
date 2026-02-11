# Research: Implement OTP supervision tree and public API

**Date**: 2025-01-10
**Item**: 012-otp-supervision-tree

## Research Question

Implement the OTP supervision tree and public API for wf_substrate.

Supervision hierarchy:
- wf_substrate_sup: top-level supervisor (one_for_one), starts wf_case_sup and optional singletons.
- wf_case_sup: dynamic supervisor (simple_one_for_one) for per-case runner processes.
- wf_effect_sup (optional): dynamic supervisor for effect handler processes if effects are executed in-process.
- wf_trace_sink: optional gen_server that collects trace events from all cases (for centralized logging/monitoring).

Per-case runner as gen_statem with states: {initializing, running, waiting_effect, waiting_signal, cancelled, done}. The runner holds the exec_state, steps in configurable bursts (N reductions then yield to scheduler), handles incoming signals (external events that feed into XOR choices or task completions), effect responses, and cancel requests. Uses state_timeout for overall case timeout.

Public API module wf_substrate.erl exports:
- new_case/3 :: (CaseId, wf_term(), Ctx) -> {ok, Pid} — compile term, create runner.
- signal/2 :: (CaseId, Signal) -> ok — send external signal to running case.
- cancel/1 :: (CaseId) -> ok — cancel entire case.
- cancel_region/2 :: (CaseId, ScopeId) -> ok — cancel a region within a case.
- await/2 :: (CaseId, Timeout) -> {ok, Result} | {error, Reason} — block until case completes.
- status/1 :: (CaseId) -> Status — get current case status.
- trace/3 :: (CaseId, Level, Sink) -> ok — configure tracing for a case.
- validate/2 :: (wf_term(), Options) -> ok | {error, Issues} — static validation without execution.

Include wf_substrate_app.erl (application behaviour) that starts wf_substrate_sup.

## Summary

The OTP supervision tree and public API implementation will provide the production-grade process architecture for wf_substrate. This item creates the bridge between the core execution engine (wf_exec, wf_sched, wf_state, wf_trace) and external consumers, wrapping the low-level execution model into an OTP-compliant, supervised, fault-tolerant system.

The implementation consists of three main components: (1) A supervision tree with four levels - top-level supervisor (wf_substrate_sup), dynamic case supervisor (wf_case_sup) managing per-case gen_statem runners, optional effect supervisor (wf_effect_sup) for effect handlers, and optional trace sink (wf_trace_sink) for centralized logging. (2) Per-case runner processes implemented as gen_statem behaviors that manage execution lifecycle through states (initializing, running, waiting_effect, waiting_signal, cancelled, done), handle external signals, effect responses, and cancellation requests. (3) Public API module (wf_substrate.erl) providing a clean interface for workflow case management, signal delivery, cancellation, status queries, and trace configuration.

Key architectural decisions: Use gen_statem instead of gen_server for case runners to leverage explicit state modeling and state timeouts; implement simple_one_for_one dynamic supervision for case runners to allow unlimited concurrent cases; separate process-per-case isolation for fault containment; integrate existing wf_exec bytecode executor, wf_sched scheduler policies, wf_state ETS-backed persistence, and wf_trace structured tracing. The public API delegates to appropriate supervisors and processes while providing a synchronous interface for operations like await/2 and status/1. The validate/2 function will be a stub pending item 013 implementation.

## Current State Analysis

### Existing Implementation

**Project Scaffold Complete**: Items 001-011 have established the core infrastructure:

1. **OTP Application Structure** (Item 001):
   - `/Users/speed/wf-substrate/src/wf_substrate_app.erl:6-39` - Application behavior implementing start/2 and stop/1, already calls `wf_substrate_sup:start_link()`
   - `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file with registered=[wf_substrate_sup], depends on kernel, stdlib, sasl
   - `/Users/speed/wf-substrate/rebar.config:1-29` - Build configuration requiring OTP 26+, warnings_as_errors enabled

2. **Top-Level Supervisor Stub** (Item 001):
   - `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:6-51` - Supervisor with one_for_one strategy, intensity=10, period=60
   - Currently has empty ChildSpecs (line 46), needs to be populated with wf_case_sup and optional singletons

3. **Public API Stub** (Item 001):
   - `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder module with commented API functions
   - No implementations yet, just documentation comments listing planned API

4. **Core Execution Engine** (Item 005):
   - `/Users/speed/wf-substrate/src/wf_exec.erl:1-783` - Bytecode executor with new/1, step/2, run/3, snapshot/1, restore/2
   - Executes wf_vm:wf_bc() bytecode, manages exec_state with tokens, branches, joins
   - Integrates with wf_trace:emit/2 (line 133) and wf_state for persistence

5. **Scheduler Policies** (Item 007):
   - `/Users/speed/wf-substrate/src/wf_sched.erl:1-174` - Scheduler behavior and factory module
   - Implementations: wf_sched_deterministic.erl, wf_sched_nondeterministic.erl, wf_sched_replay.erl
   - Supports choose/2 for action selection, choice_log recording for replay

6. **State Store with Atomic Commit** (Item 006):
   - `/Users/speed/wf-substrate/src/wf_state.erl:1-579` - gen_server managing ETS table (wf_state_store)
   - Buffer-apply-commit protocol with validation, receipt generation
   - Per-case state: ctx, tokens, scopes, metadata, status

7. **Tracing and Replay** (Item 011):
   - `/Users/speed/wf-substrate/src/wf_trace.erl:1-335` - Structured trace events with levels (none, min, full)
   - Sink types: callback, ETS table, process, replay log
   - to_replay_log/1 extracts scheduler choices and effect results for deterministic replay

8. **Multiple Instance Support** (Item 009):
   - `/Users/speed/wf-substrate/src/wf_mi.erl:1-239` - MI pattern semantics
   - spawn_instances/4, collect_result/3, cancel_remaining/2
   - Integrates with wf_exec MI_SPAWN opcode handler

9. **Record Structures** (src/*.hrl):
   - `/Users/speed/wf-substrate/src/wf_exec.hrl:1-44` - exec_state, token, branch_info, join_counter records
   - `/Users/speed/wf-substrate/src/wf_mi.hrl:1-24` - mi_instance, mi_config records

### Current Patterns and Conventions

**OTP Behavior Usage**:
- gen_server: wf_state (line 27), wf_trace_sink (to be implemented)
- gen_statem: wf_case_runner (to be implemented in this item)
- supervisor: wf_substrate_sup, wf_case_sup, wf_effect_sup (to be implemented)

**Type Specifications**: All modules use -spec for exported functions (wf_exec:38-97, wf_sched:47-54)

**Testing Patterns**:
- EUnit for unit tests (test/*_tests.erl)
- Setup/teardown with gen_server start_link (wf_state_tests:8-19)
- Include header files in tests (wf_exec_tests:3)

**Integration Points**:
- wf_exec integrates wf_trace:emit/2 (wf_exec:133)
- wf_exec integrates wf_mi for MI_SPAWN (wf_exec:335-417)
- wf_state provides ETS-backed persistence (wf_state:173-181)
- wf_sched policies support deterministic/nondeterministic/replay (wf_sched:72-81)

### Missing Components

**Not Yet Implemented** (referenced in item 012 overview but not existing):
- wf_case_sup.erl - Dynamic supervisor for case runners
- wf_case_runner.erl - Per-case gen_statem process
- wf_effect_sup.erl - Optional supervisor for effect handlers
- wf_trace_sink.erl - Optional gen_server for centralized trace collection
- wf_substrate.erl API functions (new_case/3, signal/2, cancel/1, etc.)
- wf_term.erl and wf_compile.erl (items 002, 004) - needed for new_case/3 compilation

## Key Files

### Files to Modify

- `src/wf_substrate_sup.erl:42-47` - Update init/1 to add wf_case_sup and optional singleton children
- `src/wf_substrate.erl:21-23` - Uncomment and implement API exports

### Files to Create

1. **src/wf_case_sup.erl** - Dynamic supervisor (simple_one_for_one) for case runner processes
   - Exports: start_link/0, start_case/3 (CaseId, Bytecode, Options)
   - Child spec: #{id => CaseId, start => {wf_case_runner, start_link, [CaseId, Bytecode, Options]}, restart => temporary}
   - Uses supervisor module (not behavior) for dynamic supervision

2. **src/wf_case_runner.erl** - Per-case gen_statem managing execution lifecycle
   - States: initializing, running, waiting_effect, waiting_signal, cancelled, done
   - Callbacks: callback_mode, init, initializing, running, waiting_effect, waiting_signal, cancelled, done, handle_event, terminate, code_change
   - State data: case_id, exec_state, sched_state, options (step_quanta, timeout, trace_level), caller_pid (for await/2)
   - Integrates: wf_exec:run/3 for execution bursts, wf_sched:choose/2 for scheduling, wf_state:commit/1 for persistence, wf_trace for tracing

3. **src/wf_effect_sup.erl** (optional) - Dynamic supervisor for effect handler processes
   - Only if effects are executed in-process (vs out-of-process)
   - Similar structure to wf_case_sup: simple_one_for_one, temporary restart
   - Child processes: gen_servers or gen_statems implementing effect behavior

4. **src/wf_trace_sink.erl** (optional) - gen_server collecting trace events from all cases
   - Exports: start_link/0, add_case/2, remove_case/1, get_events/2
   - State: #{case_id => trace_state()}
   - API: wf_trace_sink:collect_event/2 (CaseId, TraceEvent)
   - Sinks can be configured: ETS table, process, callback function

5. **src/wf_substrate.erl** - Public API implementation
   - new_case/3: Compiles wf_term() via wf_compile:compile/1, calls wf_case_sup:start_case/3
   - signal/2: Sends {signal, Signal} message to case runner process
   - cancel/1: Sends cancel request to case runner
   - cancel_region/2: Sends {cancel_region, ScopeId} message
   - await/2: Monitors case process, waits for {'DOWN', ..., ..., case_completed, Result}
   - status/1: Calls sys:get_status/2 on case runner and extracts status
   - trace/3: Sends {set_trace, Level, Sink} message to case runner
   - validate/2: Stub returning ok (full implementation in item 013)

### Supporting Files to Reference

- `src/wf_exec.erl:153-169` - run/3 for execution quanta
- `src/wf_state.erl:209-244` - new/1 for initial state creation
- `src/wf_trace.erl:78-94` - new/1 for trace state creation
- `src/wf_sched.erl:72-81` - new/2 for scheduler creation

### Test Files to Create

- `test/wf_substrate_api_tests.erl` - EUnit tests for public API
- `test/wf_case_runner_tests.erl` - Tests for gen_statem behavior
- `test/wf_supervision_tree_tests.erl` - Tests for supervisor start/stop, restart strategies

### Files That Will Be Created by Other Items

- `src/wf_term.erl` (item 002) - Pattern term algebra
- `src/wf_compile.erl` (item 004) - Compiler from wf_term() to wf_bc()
- `src/wf_validate.erl` (item 013) - Static validation backend

## Technical Considerations

### Dependencies

**Internal Module Dependencies** (all implemented or in-progress):
- wf_exec (item 005, done) - Execution engine
- wf_sched (item 007, done) - Scheduler policies
- wf_state (item 006, done) - State store with ETS
- wf_trace (item 011, done) - Tracing and replay
- wf_mi (item 009, done) - Multiple instance semantics
- wf_vm (item 005, done) - Bytecode definitions
- wf_term (item 002, idea) - Pattern term AST
- wf_compile (item 004, idea) - Compiler
- wf_validate (item 013, idea) - Validation

**External Dependencies**: None (pure Erlang/OTP, no NIFs or ports)

**OTP Behaviors Required**:
- supervisor (standard OTP)
- gen_statem (OTP 19.0+, available in OTP 26+)
- gen_server (standard OTP)

### Patterns to Follow

**Supervision Tree Pattern** (from PROMPT.md:183-186):
```
wf_substrate_sup (one_for_one)
├── wf_case_sup (simple_one_for_one)
│   └── wf_case_runner (temporary, per case)
├── wf_effect_sup (simple_one_for_one, optional)
│   └── wf_effect_handler (temporary)
└── wf_trace_sink (one_for_one, optional)
```

**Case Runner Lifecycle** (gen_statem state machine):
1. **initializing**: Compile bytecode, create wf_exec state, create wf_sched state, create wf_trace state, transition to running
2. **running**: Execute wf_exec:run/3 quanta, handle signals, check for termination
   - If done: transition to done, notify caller
   - If blocked on effect: transition to waiting_effect
   - If blocked on signal: transition to waiting_signal
   - If cancelled: transition to cancelled
3. **waiting_effect**: Await effect response from external system, then transition to running
4. **waiting_signal**: Await signal from wf_substrate:signal/2, then transition to running
5. **cancelled**: Cleanup, notify caller, terminate
6. **done**: Cleanup, return result to caller, terminate

**Process Registration**: Use gproc or local registry for case PIDs by CaseId (avoid process registry conflicts)

**Error Handling**: Use gen_statem timeouts for overall case timeout; let supervisor handle crashes (one_for_one restart)

**Trace Event Propagation**: Case runner sends trace events to wf_trace_sink if configured (optional feature)

### Integration Points

**wf_exec Integration**:
- Call wf_exec:new/1 to create initial exec_state from bytecode
- Call wf_exec:run/3 (Quanta, SchedPolicy) to execute burst of reductions
- Check wf_exec:is_done/1 and wf_exec:is_blocked/1 to determine state transitions
- Use wf_exec:snapshot_exec_state/1 for crash recovery

**wf_sched Integration**:
- Call wf_sched:new/2 with policy from options
- Call wf_sched:choose/2 to select next action when multiple tokens are active
- Use wf_sched:get_log/1 to extract choice log for replay

**wf_state Integration**:
- Call wf_state:new/1 to create initial state with Ctx
- Call wf_state:commit/1 after each quantum to persist state mutations
- Call wf_state:restore_from_ets/1 on process restart (if using permanent/ transient restarts)

**wf_trace Integration**:
- Call wf_trace:new/1 to create trace state based on trace_level option
- Set trace sink via wf_trace:set_sink/2 (callback, ETS, process)
- Trace events emitted via wf_trace:emit/2 called by wf_exec:step/2

**wf_compile Integration** (future item 004):
- Call wf_compile:compile/1 to convert wf_term() to wf_bc() bytecode
- For now, use mock bytecode directly in tests

### gen_statem-Specific Patterns

**State Callback Mode**:
```erlang
callback_mode() -> state_functions.
```
Each state (initializing, running, etc.) is a separate callback function.

**State Timeout for Overall Case Timeout**:
```erlang
running(timeout, StateData) ->
    {next_state, cancelled, StateData, [{reply, From, {error, timeout}}]};
```

**Event Handling**:
- `{cast, {signal, Signal}}` - External signal
- `{cast, cancel}` - Cancel case
- `{cast, {cancel_region, ScopeId}}` - Cancel region
- `{cast, {set_trace, Level, Sink}}` - Update trace configuration
- `{call, From, status}` - Query status
- `timeout` - Overall case timeout
- `info` - Effect responses and other messages

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **gen_statem Complexity** | High | gen_statem has steeper learning curve than gen_server; use state_functions mode for clarity; write comprehensive tests for each state transition; document state machine in module @doc |
| **Process Naming Conflicts** | Medium | Avoid using global process registry for case PIDs; use gproc or local name registry with {via, gproc, {n, l, {wf_case, CaseId}}}; handle name conflicts gracefully in new_case/3 |
| **Supervisor Restarts Not Appropriate** | Medium | Case runners should be temporary (not restarted) because execution state is hard to replay; use temporary restart intensity; supervisor restarts only for crashes, not normal termination |
| **Memory Leaks from ETS Tables** | High | wf_state ETS table grows unbounded; implement cleanup in wf_case_runner terminate/2; add periodic ETS table cleanup in wf_state gen_server; document max_cases limit |
| **Trace Event Storm** | Medium | Full tracing at high step counts can overwhelm system; implement rate limiting or sampling in wf_trace_sink; document trace_level=full overhead; use min level for production |
| **Deadlock in await/2** | High | Caller blocks waiting for case completion; if case crashes without notification, caller hangs forever; use monitor/2 and {'DOWN', ...} messages; implement timeout in await/2; cleanup monitor on all exit paths |
| **wf_compile/wf_term Not Implemented** | High | new_case/3 depends on wf_compile:compile/1 which doesn't exist yet; Mitigation: Implement mock compiler for now, call directly with bytecode in tests, add TODO for item 004 integration |
| **State Replication Overhead** | Medium | Copying exec_state on every message adds overhead; consider persistent_term for shared bytecode; optimize exec_state copy in hot loop; benchmark with wf_bench (item 016) |
| **Scheduler Choice Log Size** | Low | Nondeterministic scheduler logs can grow large; implement log rotation or sampling; compress choice logs for long-running cases |

## Recommended Approach

**Phase 1: Supervision Tree Skeleton** (2-3 hours)

1. Update `wf_substrate_sup.erl` init/1 to add child specs:
   ```erlang
   ChildSpecs = [
       #{id => wf_case_sup,
         start => {wf_case_sup, start_link, []},
         restart => permanent,
         shutdown => 5000,
         type => supervisor,
         modules => [wf_case_sup]},
       %% Optional singletons added later:
       %% #{id => wf_effect_sup, ...},
       %% #{id => wf_trace_sink, ...}
   ],
   ```

2. Create `wf_case_sup.erl`:
   - Use supervisor module (not behavior)
   - simple_one_for_one strategy
   - Export start_link/0 and start_case/3
   - Child spec uses temporary restart (cases not auto-restarted on completion)
   - Register via gproc: {n, l, {wf_case, CaseId}}

**Phase 2: Case Runner gen_statem** (6-8 hours)

3. Create `wf_case_runner.erl` with state functions:
   - init/1: Parse options, create initial exec_state, sched_state, trace_state
   - initializing/3: Set up execution context, transition to running
   - running/3: Execute wf_exec:run/3 quanta, handle done/blocked states
   - waiting_effect/3: Await effect response, timeout, or cancel
   - waiting_signal/3: Await signal via cast, timeout, or cancel
   - cancelled/3: Cleanup, notify caller, stop
   - done/3: Return result, notify caller, stop
   - handle_event/4: Catch-all for unexpected events
   - terminate/3: Cleanup resources, remove from wf_trace_sink

4. Implement options parsing:
   - step_quanta: N (default 100)
   - timeout: milliseconds (default 5000)
   - trace_level: none | min | full (default none)
   - scheduler_policy: deterministic | nondeterministic | {replay, Log}
   - effect_handler: module() (optional)

5. Add comprehensive EUnit tests for each state transition

**Phase 3: Public API** (4-5 hours)

6. Implement `wf_substrate.erl` API functions:
   - new_case/3: Call wf_compile (mock for now), then wf_case_sup:start_case/3
   - signal/2: Send cast to case PID via gproc lookup
   - cancel/1: Send cancel cast
   - cancel_region/2: Send {cancel_region, ScopeId} cast
   - await/2: Monitor case process, await DOWN or case_completed message
   - status/1: Call sys:get_status/2, parse status term
   - trace/3: Send {set_trace, Level, Sink} cast
   - validate/2: Stub returning ok (item 013 will implement)

7. Add error handling: Return {error, Reason} tuples for failure cases

8. Write EUnit tests for all API functions

**Phase 4: Optional Components** (2-3 hours)

9. Create `wf_effect_sup.erl` (if in-process effects):
   - Similar to wf_case_sup: simple_one_for_one
   - Child spec for effect handler gen_servers

10. Create `wf_trace_sink.erl`:
    - gen_server collecting trace events from cases
    - API: add_case/2, remove_case/1, get_events/2, collect_event/2
    - Store events in ETS table per case

**Phase 5: Integration and Testing** (3-4 hours)

11. Write integration tests:
    - Start application, create multiple concurrent cases
    - Test signal delivery during execution
    - Test cancellation at various states
    - Test await/2 timeout behavior
    - Test trace collection via wf_trace_sink

12. Update app.src to include new modules

13. Verify rebar3 compile and rebar3 eunit pass

14. Document supervision tree in ARCHITECTURE.md (item 003)

**Rationale**: This phased approach builds the supervision tree bottom-up from infrastructure to API, ensuring each component is tested before integration. The temporary restart strategy and gen_statem state machine provide robust fault containment. Using gproc for process registration avoids global naming conflicts while allowing efficient PID lookup by CaseId.

## Open Questions

1. **Compiler Integration Timing**: wf_compile (item 004) and wf_term (item 002) are not yet implemented. Should new_case/3:
   - Accept wf_bc() bytecode directly for now (bypassing wf_compile)?
   - Implement a stub compiler that returns mock bytecode?
   - Return {error, not_implemented} until item 004 is complete?
   - *Recommendation*: Implement mock compiler for unit tests, accept bytecode directly in API for now, add TODO for item 004 integration

2. **Effect Execution Model**: Are effects executed:
   - In-process by wf_case_runner (requires wf_effect_sup)?
   - Out-of-process by external effect handlers (no wf_effect_sup needed)?
   - *Recommendation*: Start with out-of-process model (simpler), add wf_effect_sup in future if needed for sandboxing

3. **Trace Sink Scope**: Should wf_trace_sink be:
   - Per-application singleton (all cases send to one sink)?
   - Per-case configurable (each case has its own sink)?
   - Optional (disabled by default)?
   - *Recommendation*: Optional per-application singleton, enabled via application environment variable

4. **Case ID Format**: Should CaseId be:
   - User-provided atom/string (risk of collisions)?
   - Auto-generated reference() (unpredictable but unique)?
   - UUID via library (violates "no external deps")?
   - *Recommendation*: User-provided for convenience, auto-generated reference() if undefined in new_case/3

5. **await/2 Blocking**: Should await/2 block the calling process indefinitely or use a timeout?
   - Specification says Timeout parameter, but default should be?
   - Should infinity be allowed?
   - *Recommendation*: Default 5000ms, allow infinity, document potential deadlock risk

6. **State Persistence Strategy**: Should case state be persisted:
   - On every quantum (expensive but safe)?
   - On yield only (cheaper but vulnerable to crash)?
   - Configurable via options?
   - *Recommendation*: Configurable via persist_quantum option (default: true)

7. **Supervision Restart Strategy**: Should case runners be:
   - temporary (not restarted - execution state lost)?
   - transient (restarted on abnormal termination only)?
   - intrinsic (always restarted - may replay from log)?
   - *Recommendation*: temporary restart (cases are one-shot executions), rely on caller to retry on failure

8. **gen_statem vs gen_server**: Specification requires gen_statem, but could gen_server suffice?
   - gen_statem better models explicit states
   - gen_server simpler for hot loop with state field
   - *Recommendation*: Follow specification, use gen_statem for clarity and state_timeout support
