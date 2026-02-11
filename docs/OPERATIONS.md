# Operations Guide

## Overview

This guide covers operational concerns for deploying and running wf_substrate in production. It includes supervision tree structure, failure modes and recovery, telemetry and monitoring, and configuration options.

**Note:** wf_substrate is currently in early development. Many operational features (scheduler policies, effect system, supervision tree) are planned but not yet implemented. This document distinguishes between current state and planned architecture.

## Supervision Tree

### Current State (Skeleton Only)

From src/wf_substrate_sup.erl:42-47:

```erlang
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [],  %% EMPTY - no children yet
    {ok, {SupFlags, ChildSpecs}}.
```

**Status:** Top-level supervisor exists but has no child specifications (line 46). The supervisor uses `one_for_one` restart strategy.

### Planned Architecture

From PROMPT.md:182-185:

```erlang
%% Top-level supervisor (wf_substrate_sup)
%% - Strategy: one_for_one
%% - Children:
%%   - wf_case_sup: simple_one_for_one (dynamic case runners)
%%   - wf_effect_sup: one_for_one (effect executors)
%%   - wf_trace_sink_sup: one_for_one (trace sinks)
```

**Dependency:** Awaiting item 012 (otp-supervision-tree) for full implementation.

### Restart Strategies

**one_for_one (Top-level):**
- If a child process crashes, only that child is restarted
- Used for: wf_substrate_sup (wf_substrate_sup.erl:43)
- Max restart intensity: 10 times per 60 seconds (wf_substrate_sup.erl:44-45)

**simple_one_for_one (Case supervisor - planned):**
- Dynamic children: One supervisor instance per workflow case
- Each case runner is a child process
- Used for: wf_case_sup (not yet implemented)

**one_for_one (Effect supervisor - planned):**
- Static children: Effect executor processes
- Used for: wf_effect_sup (not yet implemented)

## Failure Modes and Recovery

### Case Runner Crash

**Current State:**
- Case runners are not separate processes (executor is single-process)
- wf_exec:run/3 runs entirely in calling process (wf_exec.erl:152-166)
- No supervision or restart mechanism

**Planned Behavior:**
- Each case runner runs as gen_server under wf_case_sup
- If case runner crashes, supervisor restarts it
- **State Loss:** No persistence yet - case restarts from initial state
- **Future:** Add state persistence (checkpointing) to resume cases

**Recovery Steps (Current):**
1. Detect crash: Process terminates abnormally
2. Investigate: Check logs for crash reason
3. Restart: Create new executor with same bytecode
4. Manual intervention: No automatic recovery

**Recovery Steps (Planned):**
1. Supervisor detects crash
2. wf_case_sup restarts case runner automatically
3. Case runner reloads state from checkpoint (item 006)
4. Resume execution from last committed state

### Effect Timeout

**Status:** Not implemented (pending item 010)

**Planned Behavior (PROMPT.md:145-155):**
- Task can request external effect: `{effect, Spec, Continuation}`
- wf_effect manager executes Spec with timeout
- On timeout:
  - Effect is cancelled
  - Case receives error receipt
  - Workflow can handle error or propagate

**Recovery Steps (Planned):**
1. Detect timeout: Effect exceeds configured duration
2. Cancel effect: Signal effect executor to stop
3. Generate error receipt: `{error, timeout, EffectSpec}`
4. Handle error: Workflow decides whether to retry, abort, or continue
5. Retry policy: Configurable max retries, backoff strategy

### State Corruption Detection

**Current Mechanisms:**

**Validation Checks (wf_validate.erl):**
- `check_dead_transitions/1`: No transitions from unreachable places (wf_validate.erl:532-561)
- `check_option_to_complete/1`: Option to complete exists (wf_validate.erl:563-589)
- `check_proper_completion/1`: Proper completion semantics (wf_validate.erl:591-607)
- `check_deadlock/1`: No deadlocks (wf_validate.erl:609-633)
- `check_soundness/1`: All 5 checks combined (wf_validate.erl:635-652)

**Runtime Detection:**
- Executor maintains invariants:
  - All tokens have valid IPs (wf_exec.erl:29-34)
  - Join counters have completed ≤ required (wf_exec.erl:45-51)
  - Scope stack is consistent (wf_exec.erl:61)

**Recovery Steps:**
1. Run validation: `wf_validate:validate_bytecode(Bytecode)`
2. Check report: Look for `#issue{severity = error}` in `#report{issues = [...]}` (wf_validate.erl:11-20)
3. Fix bytecode: Address validation errors
4. Retry execution: Create new executor with fixed bytecode

**No Automatic Recovery:** State corruption requires manual bytecode correction.

### Cancellation Failure

**Current State:** Stubbed implementation (wf_exec.erl:512-529)

```erlang
is_scope_cancelled(_ExecState, _ScopeId) ->
    %% TODO: Implement scope cancellation checking (item 008)
    false.

propagate_cancellation(ExecState, _ScopeId) ->
    %% TODO: Implement cancellation propagation (item 008)
    ExecState.
```

**Planned Behavior:**
- Cancellation scope entered via `CANCEL_SCOPE({enter, ScopeId})` (wf_exec.erl:437-441)
- Scope active while in scope_stack (wf_exec.erl:61)
- On cancel: propagate_cancellation/2 marks all tokens in scope as cancelled
- Cancelled tokens are removed from active set (wf_exec.erl:29-34)

**Recovery Steps (Planned):**
1. Detect cancellation failure: Tokens remain active after cancel signal
2. Force cancel: Re-send cancel signal to scope
3. Clean up: Remove stuck tokens manually
4. Log incident: Record cancellation failure for debugging

**Dependency:** Awaiting item 008 (cancellation-semantics)

## Telemetry and Tracing

### Trace Levels

From src/wf_trace.erl:38:

```erlang
-type trace_level() :: none | min | full.
```

**none (Zero Overhead):**
- No tracing
- No performance impact
- Used in production when tracing not needed

**min (Structural Events Only):**
- Traces only high-level events (wf_trace.erl:232-237)
- Opcodes traced: PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE
- Low overhead (~5-10% performance impact)
- Use for debugging workflow structure

**full (All Events + State Snapshots):**
- Traces every opcode execution (wf_trace.erl:246-252)
- Includes state snapshots (wf_trace.erl:258-261)
- High overhead (~50-100% performance impact)
- Use for debugging and failure analysis

### Trace Sinks

From src/wf_trace.erl:40-44:

```erlang
-type trace_sink() ::
    {callback, fun((#trace_event{}) -> ok)} |
    {ets, ets:tid()} |
    {process, pid()} |
    {replay, replay_log()}.
```

**callback (User Function):**
- Calls user-provided function for each event
- Use case: Custom logging, metrics collection
- Example:
  ```erlang
  Sink = {callback, fun(Event) ->
      logger:info("Step ~p: ~p", [Event#trace_event.step_seq, Event#trace_event.opcode])
  end},
  ```

**ets (In-Memory Table):**
- Default sink (wf_trace.erl:82-86)
- Named table: `wf_trace_events`
- Type: `bag` (allows duplicate events per step_seq)
- Concurrency: `{read_concurrency, true}`
- Use case: In-memory trace collection for testing
- Retrieval: `wf_trace:get_events/1` (wf_trace.erl:116-139)

**process (Message Passing):**
- Sends trace events to pid
- Use case: External trace consumer (e.g., logging service)
- Example:
  ```erlang
  Sink = {process, TraceConsumerPid},
  ```

**replay (Deterministic Replay):**
- Stores replay log for later execution
- Use case: Reproducing bugs, testing
- Creation: `wf_trace:to_replay_log/1` (wf_trace.erl:183-206)
- Loading: `wf_trace:from_replay_log/1` (wf_trace.erl:209-224)
- **Limitation:** Replay execution not implemented (scheduler required)

### Configuring Tracing

**Set Trace Level:**
```erlang
%% In process dictionary (per-execution)
wf_trace:set_level(full).

%% When creating trace state
{ok, TraceState} = wf_trace:new(full).
```

**Set Trace Sink:**
```erlang
%% Change sink after creation
wf_trace:set_sink({process, Pid}).
```

**Enable Tracing for Execution:**
```erlang
%% Create trace state
{ok, TraceState} = wf_trace:new(full),

%% Store in process dictionary (wf_exec:step/2 checks for this)
erlang:put(wf_trace_state, TraceState),

%% Execute workflow (will emit events)
ExecState = wf_exec:new(Bytecode),
{done, DoneState} = wf_exec:run(ExecState, 1000, undefined),

%% Retrieve events
Events = wf_trace:get_events(TraceState).
```

**Filtering Events:**
```erlang
%% Filter by opcode
ParEvents = wf_trace:filter(Events, fun(E) ->
    element(1, E#trace_event.opcode) =:= 'PAR_FORK'
end),

%% Filter by step range
RecentEvents = wf_trace:filter(Events, fun(E) ->
    E#trace_event.step_seq >= 100
end).
```

### Log Format

**Trace Event Record (wf_trace.hrl:4-8):**
```erlang
-record(trace_event, {
    step_seq :: non_neg_integer(),      %% Step number
    opcode :: wf_vm:opcode(),           %% Opcode executed
    state_before :: binary() | undefined,%% State snapshot before
    state_after :: binary() | undefined,  %% State snapshot after
    timestamp :: erlang:monotonic_time(),%% Monotonic timestamp
    scope :: [term()],                   %% Cancellation scope stack
    branch_id :: term() | undefined,     %% Parallel branch ID
    metadata :: map()                    %% Additional info
}).
```

**Example Event:**
```erlang
#trace_event{
    step_seq = 42,
    opcode = {'PAR_FORK', [1, 3, 5]},
    state_before = <<"<<exec_state binary>>">>,
    state_after = <<"<<exec_state binary>>">>,
    timestamp = 1699876543210,
    scope = [root, outer_scope],
    branch_id = undefined,
    metadata = #{branches => 3}
}
```

### Telemetry Integration

**Current State:** No built-in telemetry (OpenTelemetry, Prometheus, etc.)

**Recommended Approach:**
1. Use `callback` sink to emit metrics
2. Example with Prometheus:
   ```erlang
   Sink = {callback, fun(Event) ->
       prometheus_histogram:observe(
           wf_workflow_duration_seconds,
           [element(1, Event#trace_event.opcode)],
           erlang:monotonic_time() - Event#trace_event.timestamp
       )
   end},
   ```

3. Example with OpenTelemetry:
   ```erlang
   Sink = {callback, fun(Event) ->
       opentelemetry:record_span(Event#trace_event.metadata#{span_name => wf_step})
   end},
   ```

## Monitoring Running Systems

### Inspecting Active Cases

**Current State:** No API available (case runners not implemented)

**Planned API:**
```erlang
%% List all active cases
wf_substrate:list_cases() -> [{case_id(), pid(), wf_vm:wf_bc()}].

%% Get case status
wf_substrate:case_status(CaseId) -> #{
    status => running | done | blocked | cancelled,
    step_count => non_neg_integer(),
    active_tokens => [term()],
    join_counters => [term()]
}.
```

**Workaround (Current):**
```erlang
%% If running executor in separate process
%% (manual process spawning required)
ProcessInfo = erlang:process_info(CaseRunnerPid),
%% Look for messages, heap size, reductions
```

### Checking for Stuck Workflows

**Symptoms:**
- Executor status = `running` but no progress
- Tokens stuck with status = `active` but no enabled transitions
- Long-running loops (while/until without exit condition)

**Detection Methods:**

1. **Manual Inspection:**
   ```erlang
   %% Check executor status
   wf_exec:is_blocked(ExecState),  %% Returns true if no progress possible

   %% Check active tokens
   Tokens = maps:values(ExecState#exec_state.tokens),
   StuckTokens = [T || T <- Tokens, T#token.status =:= active],
   ```

2. **Validation Check:**
   ```erlang
   %% Run deadlock detection
   {ok, Report} = wf_validate:check_deadlock(Bytecode),
   %% Check for deadlock issues in Report#report.issues
   ```

3. **Automatic Monitoring (Planned):**
   - Use `callback` trace sink to detect repeated opcodes
   - Alert if same opcode executed > N times in loop
   - Detect long-running tasks (timeout config)

**Recovery:**
1. Identify stuck case: Use inspection methods above
2. Cancel case: Send cancel signal (if cancellation implemented)
3. Kill process: Last resort (loss of state)
4. Restart: Create new executor with same bytecode

### Retrieving Trace Logs

**From ETS Table:**
```erlang
%% Get all events from trace state
Events = wf_trace:get_events(TraceState).

%% Access ETS table directly (if table not deleted)
Table = ets:whereis(wf_trace_events),
Events = ets:tab2list(Table).
```

**From Process Sink:**
```erlang
%% If using {process, Pid} sink
%% Events are sent as messages: {trace_event, Event}
receive
    {trace_event, Event} ->
        handle_event(Event)
end.
```

**Converting to Replay Log:**
```erlang
%% Extract replay log from trace events
ReplayLog = wf_trace:to_replay_log(Events),

%% Replay log can be saved to file for later analysis
ok = file:write_file("trace.log", io_lib:format("~p.~n", [ReplayLog])).
```

### Health Checks

**Executor Health:**
```erlang
check_executor_health(ExecState) ->
    #{
        status => ExecState#exec_state.status,
        steps => ExecState#exec_state.step_count,
        tokens => maps:size(ExecState#exec_state.tokens),
        is_done => wf_exec:is_done(ExecState),
        is_blocked => wf_exec:is_blocked(ExecState),
        scope_depth => wf_exec:get_scope_stack_depth(ExecState)
    }.
```

**Supervisor Health:**
```erlang
check_supervisor_health() ->
    Children = supervisor:which_children(wf_substrate_sup),
    #{
        child_count => length(Children),
        children => [{Id, Pid, Type, Modules} || {Id, Pid, Type, Modules} <- Children]
    }.
```

**System Health:**
```erlang
check_system_health() ->
    #{
        memory => erlang:memory(total),
        process_count => erlang:system_info(process_count),
        scheduler_utilization => erlang:statistics(scheduler_wall_time)
    }.
```

## Configuration Options

### Executor Configuration

**step_quanta (from wf_exec:run/3):**
- Type: `non_neg_integer()`
- Description: Maximum number of steps to execute per call
- Default: 1000
- Usage: `wf_exec:run(ExecState, StepQuanta, SchedulerPolicy)`
- Effect: Larger values = longer bursts, less cooperative scheduling

**Example:**
```erlang
%% Run up to 100 steps (fine-grained preemption)
wf_exec:run(ExecState, 100, undefined),

%% Run up to 10000 steps (coarse-grained)
wf_exec:run(ExecState, 10000, undefined).
```

### Trace Configuration

**trace_level:**
- Type: `none | min | full`
- Default: `none`
- Set: `wf_trace:set_level/1`
- Overhead: none (0%), min (5-10%), full (50-100%)

**trace_sink:**
- Type: `callback | ets | process | replay`
- Default: `ets` (named table)
- Set: `wf_trace:set_sink/1`
- Choose based on use case:
  - `ets`: Testing, in-memory collection
  - `process`: External log consumer
  - `callback`: Custom processing, metrics
  - `replay`: Deterministic replay

### Scheduler Policy (Planned)

**From PROMPT.md:140-143:**
- Type: `deterministic | nondeterministic | replay(ChoiceLog)`
- Status: Not implemented (wf_sched module doesn't exist)
- Usage: `wf_exec:run(ExecState, StepQuanta, SchedulerPolicy)`

**Policies:**
1. **deterministic:**
   - Stable ordering of enabled transitions
   - First active token selected (wf_exec.erl:246-252)
   - First branch selected in XOR (wf_exec.erl:398)
   - Use case: Reproducible execution, testing

2. **nondeterministic:**
   - Random selection of enabled transitions
   - Use case: Exploring all execution paths, stress testing

3. **replay(ChoiceLog):**
   - Replay recorded choices from previous execution
   - Use case: Reproducing bugs, debugging

**Current Status:** Scheduler policy parameter is ignored (wf_test_trace_helpers.erl:69)

### Effect Handler Configuration (Planned)

**From PROMPT.md:284-289:**
- Type: `module()`
- Description: Callback module for effect execution
- Status: Not implemented (pending item 010)

**Planned Configuration:**
```erlang
%% In application environment (sys.config)
{wf_substrate, [
    {effect_handler, my_effect_handler},
    {effect_timeout, 5000}  %% milliseconds
]}.

%% Effect handler callback:
-callback execute_effect(EffectSpec, Context) ->
    {ok, Result} | {error, Reason} | {timeout, EffectSpec}.
```

### Effect Budgets (Planned)

**From PROMPT.md:296:**
- Description: Limit number of effects per workflow
- Status: Not implemented
- Use case: Prevent runaway effect execution

**Planned Configuration:**
```erlang
{wf_substrate, [
    {max_effects, 100},  %% Maximum effects per workflow
    {effect_timeout, 5000}  %% Per-effect timeout
]}.
```

### Timeout Configuration

**Operation Timeouts:**
- `gen_server:call/2` timeout: 5000ms (default)
- `wf_exec:run/3` burst timeout: None (cooperative)
- Effect timeout: Planned (item 010)

**Recommendations:**
- Set gen_server timeouts based on workflow complexity
- Monitor step counts to detect infinite loops
- Use validation to check for deadlocks before deployment

## Deployment

### Building for Production

```bash
# Compile with optimizations
rebar3 compile

# Build release
rebar3 release

# Tarball release
rebar3 tar
```

**Output:** `_build/default/rel/wf_substrate/`

### Running as Service

**Systemd Unit File:**
```ini
[Unit]
Description=wf_substrate workflow engine
After=network.target

[Service]
Type=simple
User=wf_substrate
WorkingDirectory=/opt/wf_substrate
ExecStart=/opt/wf_substrate/bin/wf_substrate foreground
ExecStop=/opt/wf_substrate/bin/wf_substrate stop
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

**Enable and start:**
```bash
sudo systemctl enable wf_substrate
sudo systemctl start wf_substrate
sudo systemctl status wf_substrate
```

### Environment Variables

```bash
# Erlang VM options
export ERL_OPTS="+K true +A 128 +SDio 100"

# Distribution
export ERLANG_COOKIE="wf_substrate_cookie"
export NODE_NAME="wf_substrate@127.0.0.1"

# Logging
export LOG_LEVEL="info"

# Start application
erl -sname $NODE_NAME -setcookie $ERLANG_COOKIE $ERL_OPTS -pa ebin -s wf_substrate
```

### Upgrading

**Code Reload (Hot Upgrade):**
```erlang
%% Check current version
application:which_applications().

%% Load new version
l(wf_exec).
l(wf_vm).

%% Verify
wf_exec:new([{'DONE'}]).
```

**Release Upgrade:**
```bash
# Build new release
rebar3 release

# Upgrade script
rebar3 relups

# Install upgrade
wf_substrate upgrade <release_package>.tar.gz
```

## Troubleshooting

### Common Issues

**1. Workflow Stuck in Running State**
- Symptoms: `wf_exec:is_done/1` returns false, no progress
- Causes: Deadlock, infinite loop, waiting for join
- Debug: Check tokens, join counters, validation
- Fix: Add loop exit condition, fix join policy, use timeout

**2. High Memory Usage**
- Symptoms: Process memory grows continuously
- Causes: Large workflows, many active cases, trace events not cleaned up
- Debug: `erlang:process_info(self(), memory)`
- Fix: Clear ETS trace tables, limit concurrent cases, use trace level=none

**3. Slow Execution**
- Symptoms: Steps/sec lower than expected
- Causes: Trace level=full, large bytecode, complex join policies
- Debug: Check trace level, use wf_bench:run_all() for metrics
- Fix: Reduce trace level, optimize bytecode, use first_complete joins

**4. Crash on Startup**
- Symptoms: Application fails to start
- Causes: Missing dependencies, invalid config
- Debug: Check logs for `{error, Reason}`
- Fix: Install dependencies, fix sys.config

### Debugging Checklist

1. **Check Executor Status:**
   ```erlang
   wf_exec:is_done(ExecState),
   wf_exec:is_blocked(ExecState),
   wf_exec:get_step_count(ExecState).
   ```

2. **Inspect Tokens:**
   ```erlang
   Tokens = maps:values(ExecState#exec_state.tokens),
   [io:format("Token ~p: IP=~p, Status=~p~n", [T#token.token_id, T#token.ip, T#token.status]) || T <- Tokens].
   ```

3. **Validate Bytecode:**
   ```erlang
   {ok, Report} = wf_validate:validate_bytecode(Bytecode),
   io:format("~p~n", [Report]).
   ```

4. **Enable Tracing:**
   ```erlang
   wf_trace:set_level(full),
   erlang:put(wf_trace_state, TraceState),
   %% Run workflow
   Events = wf_trace:get_events(TraceState),
   io:format("~p~n", [Events]).
   ```

5. **Check Process Info:**
   ```erlang
   erlang:process_info(self(), memory),
   erlang:process_info(self(), message_queue_len),
   erlang:process_info(self(), dictionary).
   ```

## Further Reading

- **Pattern Coverage:** docs/PATTERNS.md - All implemented workflow patterns
- **Testing Strategy:** docs/TESTING.md - How to test workflows
- **Source of Truth:** PROMPT.md - Architectural requirements
- **Validation:** src/wf_validate.erl - Correctness checks
- **Tracing:** src/wf_trace.erl - Telemetry infrastructure
