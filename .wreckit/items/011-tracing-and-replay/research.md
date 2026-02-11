# Research: Implement structured tracing and replay system

**Date**: 2025-01-10
**Item**: 011-tracing-and-replay

## Research Question

Implement wf_trace.erl: structured trace events for every reduction step in the executor. Each trace event contains: step_seq (monotonic sequence number), opcode (the instruction executed), state_before (snapshot or diff of exec_state before), state_after (snapshot or diff after), timestamp (monotonic microseconds), scope (current cancel scope stack), branch_id (if in a parallel branch), additional metadata per opcode type.

Trace levels:
- none: no tracing overhead, trace callbacks are no-ops.
- min: emit only structural events (fork, join, cancel, effect_yield, done) — low overhead for production.
- full: emit every single reduction step — for debugging and replay validation.

Replay log format: a list of {step_seq, opcode, scheduler_choice, effect_result} tuples sufficient to deterministically reproduce an execution. The replay log is a subset of the full trace — it captures only the nondeterministic inputs (scheduler choices and effect results), not the derived state.

Integration: wf_exec calls wf_trace:emit/2 after each step. Trace sink is configurable (function callback, ETS table, or process). Trace events are structured terms, not formatted strings.

Export: new/1 (trace level), emit/2, set_sink/2, get_events/1, to_replay_log/1, from_replay_log/1, filter/2 (filter events by opcode/scope/branch).

## Summary

This task involves implementing **wf_trace.erl**, a comprehensive tracing and replay system that provides observability for workflow execution. The tracing system captures structured trace events at multiple levels of granularity (none, min, full), enabling production monitoring, debugging, and deterministic replay of workflow executions. The replay log format captures only nondeterministic inputs (scheduler choices and effect results), making it a compact representation for reproducing executions.

The tracing system is designed with **minimal performance overhead** when disabled (level=none) and **configurable granularity** for different use cases. The "min" level captures only structural events (fork, join, cancel, effect_yield, done) for production monitoring, while the "full" level captures every reduction step for deep debugging and replay validation. The **replay log** is a subset of the full trace that captures only the nondeterministic choices made during execution, enabling deterministic reproduction without storing the entire state trace.

**Key Architectural Requirements**:
- **Zero overhead when disabled**: Trace calls at level=none are no-ops
- **Structured events**: Trace events are Erlang terms, not formatted strings
- **Multiple sinks**: Support function callbacks, ETS tables, and process sinks
- **Deterministic replay**: Replay log captures only nondeterministic inputs
- **Filtering**: Query events by opcode, scope, branch_id
- **Integration**: wf_exec calls wf_trace:emit/2 after each step

**Integration Points**:
- **wf_exec** (item 005): Executor calls wf_trace:emit/2 after each reduction step
- **wf_sched** (item 007): Scheduler choices logged to replay log
- **wf_effect** (item 010): Effect results logged to replay log
- **wf_state** (item 006): State snapshots/diffs for trace events

## Current State Analysis

### Existing Implementation

**Current State**: No `wf_trace.erl` module exists yet. However, the executor has basic trace event generation:

**1. Executor Trace Event Stub** (`/Users/speed/wf-substrate/src/wf_exec.erl:101-107`):

```erlang
%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), map()}.
step(ExecState, _SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),
    TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, TraceEvent}.
```

**Analysis**:
- Executor already returns a TraceEvent map from step/2
- TraceEvent currently contains only opcode and step_count
- **Missing**: wf_trace module to process and store events
- **Missing**: Trace level support (none, min, full)
- **Missing**: Configurable trace sinks
- **Missing**: State snapshots/diffs (state_before, state_after)
- **Missing**: Metadata fields (timestamp, scope, branch_id)
- **Missing**: Replay log generation
- **Missing**: Event filtering

**2. Trace Event Discarded** (`/Users/speed/wf-substrate/src/wf_exec.erl:125`):

```erlang
run_loop(ExecState0, Quanta, Count) ->
    %% Execute one step (no scheduler integration yet)
    {ExecState1, _TraceEvent} = step(ExecState0, undefined),
    run_loop(ExecState1, Quanta, Count + 1).
```

**Analysis**:
- TraceEvent is currently discarded (underscore prefix)
- **Integration point**: Call wf_trace:emit/2 with TraceEvent
- **Missing**: wf_trace module to receive events

**3. Scheduler Choice Logging** (`/Users/speed/wf-substrate/src/wf_sched_nondeterministic.erl:39-61`):

```erlang
%% @doc Choose action randomly and log the choice
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, #{log := Log, step_seq := StepSeq, rand_state := RandState0} = State)
    when is_list(EnabledActions), length(EnabledActions) > 0 ->

    %% Random selection: pick index 1..N with explicit state
    N = length(EnabledActions),
    {Index, RandState1} = rand:uniform_s(N, RandState0),
    Chosen = lists:nth(Index, EnabledActions),

    %% Log choice for replay
    Entry = {StepSeq, EnabledActions, Chosen},
    NewLog = [Entry | Log],

    %% Update state
    NewState = State#{
        log => NewLog,
        step_seq => StepSeq + 1,
        rand_state => RandState1
    },

    {Chosen, NewState}.
```

**Analysis**:
- Scheduler already logs choices to a choice log
- Choice log format: `{StepSeq, EnabledActions, Chosen}`
- **Integration point**: Extract scheduler choices for replay log
- **Note**: StepSeq in scheduler is independent of executor step_count
- **Gap**: Need to unify step_seq tracking between executor and scheduler

**4. Replay Scheduler** (`/Users/speed/wf-substrate/src/wf_sched_replay.erl:17-57`):

```erlang
%% @doc Create replay scheduler from choice log
-spec new(wf_sched:choice_log(), proplists:proplist()) -> {ok, wf_sched:sched_state()}.
new(ChoiceLog, _Options) when is_list(ChoiceLog) ->
    %% Validate log format
    ok = validate_log(ChoiceLog),

    %% Initialize state with log and position
    {ok, #{
        policy => replay,
        log => ChoiceLog,
        position => 1
    }}.

%% @doc Replay next choice from log, validate enabled set matches
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, #{log := ChoiceLog, position := Position} = State)
    when is_list(EnabledActions) ->

    %% Check if log exhausted
    case Position > length(ChoiceLog) of
        true ->
            error({replay_complete, Position});
        false ->
            %% Fetch recorded choice
            {_StepSeq, RecordedEnabled, Chosen} = lists:nth(Position, ChoiceLog),

            %% Validate enabled set matches recording
            case validate_enabled_set(EnabledActions, RecordedEnabled) of
                ok ->
                    %% Match! Return recorded choice
                    NewState = State#{position => Position + 1},
                    {Chosen, NewState};
                {error, {divergence, Expected, Actual}} ->
                    error({divergence, Expected, Actual})
            end
    end.
```

**Analysis**:
- Replay scheduler already implemented (item 007)
- Validates enabled set matches recorded choice
- Detects divergence if enabled set differs
- **Integration point**: wf_trace generates replay log for replay scheduler
- **Gap**: Replay log format differs from specification (needs effect_result)

**5. State Snapshot Support** (`/Users/speed/wf-substrate/src/wf_state.erl:376-415`):

```erlang
%% @doc Snapshot state to binary (for debugging/replay)
%% Note: Non-serializable fields (buffered_mutations, ets_table) are excluded.
-spec snapshot(state()) -> binary().
snapshot(State) ->
    %% Filter out non-serializable fields
    CleanState = State#state{
        buffered_mutations = [],  %% Don't snapshot buffered mutations
        ets_table = undefined      %% Don't snapshot ETS reference
    },
    term_to_binary(CleanState).

%% @doc Restore state from snapshot binary
-spec restore(binary(), case_id()) -> {ok, state()} | {error, term()}.
restore(Binary, CaseId) ->
    try binary_to_term(Binary) of
        State ->
            %% Verify case_id matches
            case State#state.case_id of
                CaseId ->
                    %% Reset ETS reference and buffered mutations
                    RestoredState = State#state{
                        buffered_mutations = [],
                        ets_table = whereis(wf_state_store)
                    },
                    {ok, RestoredState};
                _OtherId ->
                    {error, {case_id_mismatch, CaseId}}
            end
    catch
        _:_ ->
            {error, invalid_snapshot}
    end.
```

**Analysis**:
- State store already supports snapshot/restore
- Snapshots exclude non-serializable fields
- **Integration point**: Use snapshot for state_before/state_after in trace events
- **Gap**: Need diff format for efficiency (not just full snapshots)
- **Note**: wf_state.state is separate from wf_exec.exec_state (different concerns)

### Specification Context

**From item.json:6** (item 011 specification):

```
Trace event contains:
- step_seq (monotonic sequence number)
- opcode (the instruction executed)
- state_before (snapshot or diff of exec_state before)
- state_after (snapshot or diff after)
- timestamp (monotonic microseconds)
- scope (current cancel scope stack)
- branch_id (if in a parallel branch)
- additional metadata per opcode type

Trace levels:
- none: no tracing overhead, trace callbacks are no-ops.
- min: emit only structural events (fork, join, cancel, effect_yield, done) — low overhead for production.
- full: emit every single reduction step — for debugging and replay validation.

Replay log format:
- a list of {step_seq, opcode, scheduler_choice, effect_result} tuples
- sufficient to deterministically reproduce an execution
- subset of full trace (captures only nondeterministic inputs)

Export:
- new/1 (trace level)
- emit/2
- set_sink/2
- get_events/1
- to_replay_log/1
- from_replay_log/1
- filter/2 (filter events by opcode/scope/branch)
```

**From PROMPT.md:39-41, 61, 332**:

```
0.5 Observability and Replay Are First-Class
- Every reduction step produces structured trace events.
- A run can be replayed deterministically (when configured).

Deliverable modules:
- wf_trace.erl: structured tracing & replay log

11. Tracing + structured events (wf_trace)
```

**From item 005 PRD** (`/Users/speed/wf-substrate/.wreckit/items/005-executor-hot-loop/prd.json:181-194`):

```
Integration with tracing (wf_trace):
- step/2 calls wf_trace:emit_before/2 before opcode execution
- step/2 calls wf_trace:emit_after/3 after opcode execution
- wf_trace stub module created with emit_before/2, emit_after/3, emit_if/2 functions
- wf_trace supports trace levels: none, min, full
- wf_trace:set_level/1 sets trace level
- wf_trace:get_level/0 gets current trace level
- Trace event contains: type (before/after), opcode, IP, step_count, timestamp
- Trace level none: no trace events emitted
- Trace level min: basic events emitted
- Trace level full: detailed events emitted
```

**Analysis**:
- Original spec (item 005) called for emit_before/2 and emit_after/3
- Current spec (item 011) calls for emit/2 (single call after step)
- **Decision**: Follow item 011 spec (emit/2 after step) for simplicity
- State snapshots needed: state_before, state_after
- Metadata fields: timestamp, scope, branch_id

### Workflow Pattern Context

This implements the **Observability Pattern** from distributed systems:
- **Structured logging**: Events as structured terms, not strings
- **Causal tracing**: Each event tagged with step_seq, scope, branch_id
- **Deterministic replay**: Capture nondeterministic choices for reproduction
- **Granular levels**: Production (min) vs debugging (full)
- **Multiple sinks**: Callbacks, ETS, processes for flexibility

## Key Files

### Existing Files

- `/Users/speed/wf-substrate/src/wf_exec.erl:101-107` - **Executor step function**
  - Lines 101-107: `step/2` returns TraceEvent map
  - **Integration point**: Call wf_trace:emit/2 after step execution
  - **Current fields**: opcode, step_count
  - **Missing**: timestamp, scope, branch_id, state snapshots

- `/Users/speed/wf-substrate/src/wf_exec.erl:109-126` - **Executor run loop**
  - Lines 109-126: `run/3` and `run_loop/3`
  - Line 125: TraceEvent discarded (_TraceEvent)
  - **Integration point**: Call wf_trace:emit(ExecState, TraceEvent)

- `/Users/speed/wf-substrate/src/wf_exec.hrl:31-42` - **exec_state record**
  - Lines 31-42: `#exec_state{}` record definition
  - **Fields available for trace**: ip, bytecode, ctx, tokens, branch_map, join_counters, scope_stack, step_count, status, current_token
  - **Note**: No case_id field (needs to be added for trace correlation)

- `/Users/speed/wf-substrate/src/wf_sched.erl:28-34` - **Choice log type**
  - Lines 28-34: `choice_log()` and `choice_entry()` types
  - **Format**: `{StepSeq, EnabledSet, Chosen}`
  - **Integration point**: Extract scheduler choices for replay log

- `/Users/speed/wf-substrate/src/wf_sched_nondeterministic.erl:39-61` - **Scheduler logging**
  - Lines 39-61: `choose/2` logs choices to state log
  - **Integration point**: wf_trace accesses scheduler log for replay log generation

- `/Users/speed/wf-substrate/src/wf_sched_replay.erl:17-57` - **Replay scheduler**
  - Lines 17-57: Replay from choice log
  - **Validation**: Checks enabled set matches recording
  - **Integration point**: wf_trace generates replay log for this scheduler

- `/Users/speed/wf-substrate/src/wf_state.erl:376-415` - **State snapshot/restore**
  - Lines 376-415: `snapshot/1` and `restore/2`
  - **Integration point**: Use for state_before/state_after snapshots
  - **Gap**: Need exec_state snapshots (currently only wf_state.state)

- `/Users/speed/wf-substrate/src/wf_vm.erl:13-21` - **Opcode types**
  - Lines 13-21: `opcode()` type definition
  - **All opcodes**: SEQ_ENTER, SEQ_NEXT, MI_SPAWN, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, TASK_EXEC, DONE
  - **Integration point**: Trace events include opcode type

### Specification Files

- `/Users/speed/wf-substrate/.wreckit/items/011-tracing-and-replay/item.json:6` - **Item specification**
  - Trace event fields: step_seq, opcode, state_before, state_after, timestamp, scope, branch_id, metadata
  - Trace levels: none, min, full
  - Replay log format: `{step_seq, opcode, scheduler_choice, effect_result}`
  - Export functions: new/1, emit/2, set_sink/2, get_events/1, to_replay_log/1, from_replay_log/1, filter/2

- `/Users/speed/wf-substrate/PROMPT.md:39-41, 61, 332` - **Project specification**
  - Lines 39-41: Observability and replay are first-class
  - Line 61: wf_trace.erl module
  - Line 332: Tracing + structured events

### Files to Create

- `src/wf_trace.erl` - **Tracing and replay module** (primary deliverable)
  - `-record(trace_event, {})` - Trace event record
  - `-record(replay_log, {})` - Replay log record
  - `-type trace_level()` - none | min | full
  - `-type trace_sink()` - function | ets | process
  - `-type step_seq()` - Monotonic sequence number
  - `new/1` - Create trace state with level
  - `emit/2` - Emit trace event (checks level)
  - `set_sink/2` - Set trace sink (callback, ETS, process)
  - `get_events/1` - Get all events from sink
  - `to_replay_log/1` - Extract replay log from trace events
  - `from_replay_log/1` - Create replay log from external source
  - `filter/2` - Filter events by opcode/scope/branch
  - `set_level/1` - Set trace level dynamically
  - `get_level/0` - Get current trace level

- `src/wf_trace_state.erl` - **Trace state management** (optional, can be internal to wf_trace)
  - ETS table management for trace storage
  - Process sink message handling
  - Callback sink invocation

- `test/wf_trace_tests.erl` - **Trace system tests**
  - Test trace level none (no events)
  - Test trace level min (structural events only)
  - Test trace level full (all events)
  - Test trace sinks (callback, ETS, process)
  - Test event filtering (by opcode, scope, branch)
  - Test replay log generation
  - Test replay log validation
  - Test state snapshots/diffs

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Modules Needed**:
- `ets` - For trace event storage (ETS table sink)
- `erlang:monotonic_time/1` - For monotonic microsecond timestamps
- `erlang:term_to_binary/1` - For state snapshots
- `erlang:binary_to_term/1` - For state restoration
- `self/0` and process messaging - For process sink

**Internal Module Dependencies**:
- **wf_exec (item 005)**: Executor calls wf_trace:emit/2 after each step
  - wf_exec provides: exec_state, opcode, step_count
  - wf_exec provides: scope_stack, branch_id (from branch_map)
  - **Extension needed**: Add case_id to exec_state for trace correlation

- **wf_sched (item 007)**: Scheduler choices logged for replay
  - wf_sched provides: choice_log (StepSeq, EnabledSet, Chosen)
  - **Integration**: wf_trace extracts scheduler choices from sched state
  - **Gap**: Need unified step_seq tracking (executor step_count vs scheduler step_seq)

- **wf_effect (item 010)**: Effect results logged for replay
  - wf_effect provides: effect_id, result, duration_us
  - **Integration**: wf_trace logs effect_yield and effect_complete events
  - **Gap**: wf_effect not implemented yet (item 010 is idea state)

- **wf_state (item 006)**: State snapshots for trace events
  - wf_state provides: snapshot/1 for wf_state.state
  - **Gap**: Need snapshot function for wf_exec.exec_state (different record)

**No Circular Dependencies**: wf_trace depends on wf_exec (for exec_state structure), wf_sched (for choice log), but reads data only. Safe to implement.

### Trace Level Design

**From item.json:6**:

```
Trace levels:
- none: no tracing overhead, trace callbacks are no-ops.
- min: emit only structural events (fork, join, cancel, effect_yield, done) — low overhead for production.
- full: emit every single reduction step — for debugging and replay validation.
```

**Implementation**:

```erlang
-type trace_level() :: none | min | full.

%% Structural opcodes (emitted at level=min)
-define(STRUCTURAL_OPCODES, [
    {'PAR_FORK', _},
    {'JOIN_WAIT', _},
    {'CANCEL_SCOPE', _},
    {'EFFECT_YIELD', _},  %% Future (item 010)
    {'DONE'}
]).

%% Emit event based on trace level
emit(ExecState, TraceEvent) ->
    Level = get_level(),
    Opcode = maps:get(opcode, TraceEvent),

    case Level of
        none ->
            %% No-op, zero overhead
            ok;
        min ->
            %% Emit only structural events
            case is_structural_opcode(Opcode) of
                true -> emit_event(ExecState, TraceEvent);
                false -> ok
            end;
        full ->
            %% Emit all events
            emit_event(ExecState, TraceEvent)
    end.

is_structural_opcode({OpcodeName, _Arg}) ->
    lists:keymember(OpcodeName, 1, ?STRUCTURAL_OPCODES).
```

**Performance Consideration**: At level=none, emit/2 should compile to a no-op or minimal check (no function calls, no allocations). Use macro or compile-time flag if possible.

### Trace Event Structure

**From item.json:6**:

```
Trace event contains:
- step_seq (monotonic sequence number)
- opcode (the instruction executed)
- state_before (snapshot or diff of exec_state before)
- state_after (snapshot or diff after)
- timestamp (monotonic microseconds)
- scope (current cancel scope stack)
- branch_id (if in a parallel branch)
- additional metadata per opcode type
```

**Implementation**:

```erlang
-record(trace_event, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode(),
    state_before :: exec_state_snapshot() | diff() | undefined,
    state_after :: exec_state_snapshot() | diff() | undefined,
    timestamp :: erlang:monotonic_time(),
    scope :: [term()],  %% scope_stack
    branch_id :: term() | undefined,
    metadata :: map()  %% Additional per-opcode metadata
}).

-type exec_state_snapshot() :: binary().
-type diff() :: {removed, map(), added, map()}.  %% Simple diff format

%% Additional metadata per opcode type
metadata_for_opcode({'PAR_FORK', TargetIPs}, ExecState) ->
    #{num_branches => length(TargetIPs),
      target_ips => TargetIPs};

metadata_for_opcode({'MI_SPAWN', {MIPolicy, MIJoinPolicy, BodyIP}}, ExecState) ->
    #{mi_policy => MIPolicy,
      mi_join_policy => MIJoinPolicy,
      num_instances => maps:size(ExecState#exec_state.tokens)};

metadata_for_opcode({'JOIN_WAIT', Policy}, _ExecState) ->
    #{join_policy => Policy,
      join_satisfied => false};  %% Updated after execution

metadata_for_opcode({'CANCEL_SCOPE', {enter, ScopeId}}, _ExecState) ->
    #{cancel_op => enter,
      scope_id => ScopeId};

metadata_for_opcode(_Opcode, _ExecState) ->
    #{}.
```

**State Snapshot/Diff Trade-off**:
- **Snapshot**: Full exec_state serialized to binary (term_to_binary/1)
  - Pros: Simple, accurate, easy to restore
  - Cons: Large memory footprint, slow for large states
- **Diff**: Incremental changes (fields changed)
  - Pros: Compact, fast for large states
  - Cons: Complex to compute, harder to restore

**Recommendation**: Implement both, use snapshot for small states (level=min), diff for large states (level=full). Or use snapshot for v1, add diff for v2.

### Trace Sink Design

**From item.json:6**:

> "Trace sink is configurable (function callback, ETS table, or process)."

**Implementation**:

```erlang
-type trace_sink() ::
    {callback, fun((trace_event()) -> ok)} |
    {ets, ets:tid()} |
    {process, pid()}.

-record(trace_state, {
    level :: trace_level(),
    sink :: trace_sink(),
    case_id :: term()  %% For correlation
}).

%% Function callback sink
emit_to_sink(#trace_event{} = Event, {callback, Fun}) ->
    Fun(Event),
    ok;

%% ETS table sink
emit_to_sink(#trace_event{} = Event, {ets, Table}) ->
    ets:insert(Table, Event),
    ok;

%% Process sink (send message)
emit_to_sink(#trace_event{} = Event, {process, Pid}) ->
    Pid ! {trace_event, Event},
    ok.
```

**Sink Selection**:
- **Callback**: Simplest, good for testing and custom handlers
- **ETS**: Persistent storage, good for querying and filtering
- **Process**: Asynchronous, good for external logging services

**Default Sink**: Use ETS table (named table `wf_trace_events`) for default behavior.

### Replay Log Format

**From item.json:6**:

```
Replay log format: a list of {step_seq, opcode, scheduler_choice, effect_result} tuples
sufficient to deterministically reproduce an execution.
The replay log is a subset of the full trace — it captures only the
nondeterministic inputs (scheduler choices and effect results), not the derived state.
```

**Implementation**:

```erlang
-record(replay_entry, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode() | undefined,
    scheduler_choice :: {step_seq(), [wf_sched:enabled_action()], wf_sched:enabled_action()} | undefined,
    effect_result :: {effect_id(), term()} | undefined
}).

-type replay_log() :: [replay_entry()].

%% Extract replay log from trace events
to_replay_log(TraceEvents) ->
    %% Filter events with scheduler choices or effect results
    lists:filtermap(fun(#trace_event{step_seq = StepSeq, opcode = Opcode, metadata = Metadata}) ->
        case maps:get(scheduler_choice, Metadata, undefined) of
            undefined ->
                case maps:get(effect_result, Metadata, undefined) of
                    undefined -> false;
                    EffectResult ->
                        {true, #replay_entry{
                            step_seq = StepSeq,
                            opcode = Opcode,
                            scheduler_choice = undefined,
                            effect_result = EffectResult
                        }}
                end;
            SchedChoice ->
                {true, #replay_entry{
                    step_seq = StepSeq,
                    opcode = Opcode,
                    scheduler_choice = SchedChoice,
                    effect_result = maps:get(effect_result, Metadata, undefined)
                }}
        end
    end, TraceEvents).
```

**Replay Log Usage**:
1. **Generate replay log**: After execution, call wf_trace:to_replay_log(Events)
2. **Replay execution**: Create replay scheduler with wf_sched_replay:new(ReplayLog)
3. **Validate replay**: Compare new trace events with original trace

**Gap**: Scheduler choice format in replay log differs from wf_sched:choice_entry()
- **wf_sched format**: `{StepSeq, EnabledSet, Chosen}`
- **Replay log format**: Embedded in trace metadata
- **Solution**: Extract scheduler log from wf_sched state and merge with trace events

### Step Sequence Tracking

**Challenge**: Executor step_count vs scheduler step_seq

- **Executor step_count**: Increments on every reduction step
- **Scheduler step_seq**: Increments on every scheduler choice (may differ)

**Solution**: Use executor step_count as primary step_seq, log scheduler choices with executor step_seq.

```erlang
%% In wf_exec:step/2
step(ExecState, SchedDecision) ->
    StepSeq = ExecState#exec_state.step_count + 1,
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),

    %% Trace event includes scheduler decision (if any)
    TraceEvent = #{
        step_seq => StepSeq,
        opcode => Opcode,
        scheduler_choice => SchedDecision  %% undefined if no choice made
    },

    {NewExecState, TraceEvent}.
```

**Issue**: wf_sched:choose/2 returns chosen action, not the choice entry. Need to capture choice entry separately.

**Solution**: Modify wf_sched to return choice entry along with chosen action.

```erlang
%% Extended scheduler API
-spec choose_with_log([wf_sched:enabled_action()], sched_state()) ->
    {wf_sched:enabled_action(), choice_entry(), sched_state()}.

choose_with_log(EnabledActions, SchedState) ->
    {Chosen, NewSchedState} = wf_sched:choose(EnabledActions, SchedState),
    ChoiceEntry = extract_choice_entry(NewSchedState),
    {Chosen, ChoiceEntry, NewSchedState}.
```

**Alternative**: Store choice log in scheduler state, extract after execution.

```erlang
%% In wf_trace:to_replay_log/1
to_replay_log(TraceEvents, SchedState) ->
    ChoiceLog = wf_sched:get_log(SchedState),
    merge_choice_log(TraceEvents, ChoiceLog).
```

### Event Filtering

**From item.json:6**:

> "filter/2 (filter events by opcode/scope/branch)"

**Implementation**:

```erlang
%% Filter by opcode
-spec filter([trace_event()], {opcode, wf_vm:opcode()}) -> [trace_event()].
filter(Events, {opcode, Opcode}) ->
    [E || E <- Events, E#trace_event.opcode =:= Opcode];

%% Filter by scope (exact match)
filter(Events, {scope, ScopeId}) ->
    [E || E <- Events, lists:member(ScopeId, E#trace_event.scope)];

%% Filter by branch_id
filter(Events, {branch, BranchId}) ->
    [E || E <- E <- Events, E#trace_event.branch_id =:= BranchId];

%% Filter by predicate function
filter(Events, {predicate, Fun}) when is_function(Fun, 1) ->
    [E || E <- Events, Fun(E)].
```

**Use Cases**:
- Debug parallel branch: `filter(Events, {branch, BranchId})`
- Debug cancel scope: `filter(Events, {scope, ScopeId})`
- Debug specific opcode: `filter(Events, {opcode, {'PAR_FORK', _}})`

### State Snapshot Implementation

**Challenge**: wf_state:snapshot/1 exists for wf_state.state, but trace needs wf_exec.exec_state snapshots.

**Solution**: Implement exec_state snapshot in wf_exec (similar to wf_state).

```erlang
%% In wf_exec.erl
-spec snapshot_exec_state(exec_state()) -> binary().
snapshot_exec_state(ExecState) ->
    %% All fields are serializable (no pids, ports, refs)
    term_to_binary(ExecState).

-spec restore_exec_state(binary(), wf_vm:wf_bc()) -> {ok, exec_state()} | {error, term()}.
restore_exec_state(Binary, Bytecode) ->
    try binary_to_term(Binary) of
        ExecState ->
            %% Verify bytecode matches (optional)
            case ExecState#exec_state.bytecode of
                Bytecode ->
                    {ok, ExecState};
                _OtherBytecode ->
                    {error, {bytecode_mismatch, Bytecode}}
            end
    catch
        _:_ ->
            {error, invalid_snapshot}
    end.
```

**Gap**: exec_state contains references (make_ref()), which are serializable but not portable across runs. This is acceptable for replay (run-specific identifiers).

### Performance Considerations

**Trace Level = None**:
- emit/2 should be a no-op (zero overhead)
- Use macro or compile-time flag: `?TRACE_EMIT(Level, Event)`
- Or use simple if check: `if Level =:= none -> ok; true -> ... end`

**Trace Level = Min**:
- Emit ~10-20 events per workflow (structural events only)
- Low overhead (O(1) per structural event)

**Trace Level = Full**:
- Emit N events per workflow (N = step_count)
- High overhead (O(N) for large workflows)
- State snapshots expensive (large binary serialization)
- **Optimization**: Use diffs instead of full snapshots

**Memory Usage**:
- Full trace with snapshots: O(N * StateSize)
- Replay log: O(N) (much smaller, no state snapshots)
- **Recommendation**: Store full trace in ETS with size limit, rotate old events

**Sink Performance**:
- Callback sink: O(1) per event (function call)
- ETS sink: O(1) per event (ETS insert)
- Process sink: O(1) per event (message send)
- **Bottleneck**: Sink function may block (use async process for heavy processing)

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Performance overhead at level=full** | High | Full tracing with state snapshots may slow execution 10-100x. Mitigation: Use diffs instead of snapshots. Disable in production unless debugging. |
| **Memory exhaustion (large traces)** | High | Long-running workflows generate massive trace events. Mitigation: Configure ETS table size limit. Rotate old events. Use replay log instead of full trace. |
| **Step sequence divergence** | High | Executor step_count and scheduler step_seq may diverge, causing replay log mismatch. Mitigation: Use single step_seq counter (executor step_count). Log scheduler choices with executor step_seq. |
| **State snapshot serialization failure** | Medium | exec_state contains non-serializable terms (pid, port). Mitigation: Filter non-serializable fields before snapshot. Validate with try-catch. |
| **Trace event loss on crash** | Medium | If ETS table owner crashes, trace events lost. Mitigation: Own ETS table in separate gen_server (wf_trace_store). Persist to disk (future). |
| **Replay log validation failure** | Medium | Replay execution diverges from original (enabled set mismatch). Mitigation: Clear error messages. Record divergence point. Partial replay support. |
| **Effect result not available** | Medium | Replay log requires effect results, but wf_effect not implemented (item 010). Mitigation: Stub effect_result field for v1. Integrate when item 010 complete. |
| **Trace sink blocking** | Medium | Callback or process sink may block, slowing executor. Mitigation: Use async process with message queue. Timeout on sink calls. |
| **Missing case_id in exec_state** | Medium | Trace events need case_id for correlation. Mitigation: Add case_id field to exec_state record. Generate in wf_exec:new/1. |
| **Scheduler choice extraction** | Medium | wf_sched choice log format differs from replay log spec. Mitigation: Extend wf_sched API to return choice entry. Or extract from sched state after execution. |
| **Diff computation complexity** | Medium | Computing diffs between exec_state records is complex. Mitigation: Use full snapshots for v1. Implement diff for v2 if performance issue. |
| **Trace event ordering** | Low | Concurrent events may be out-of-order in process sink. Mitigation: Include step_seq in event. Sort by step_seq on retrieval. |
| **ETS table growth** | Medium | Trace events accumulate in ETS table. Mitigation: Implement log rotation. Delete old events after retention period. |
| **Replay log format compatibility** | Medium | Replay log format differs from wf_sched:choice_entry(). Mitigation: Document replay log format clearly. Provide conversion functions. |
| **Trace level switch at runtime** | Low | Switching trace level during execution may cause inconsistent trace. Mitigation: Document restriction (set level before execution). Or handle gracefully. |
| **Metadata per opcode complexity** | Low | Each opcode type may require different metadata. Mitigation: Default to empty map. Implement metadata extraction function per opcode. |
| **Branch_id tracking missing** | Medium | exec_state.branch_map tracks branches, but current token may not be in a branch. Mitigation: Extract branch_id from branch_map using current_token. |
| **Scope stack snapshot** | Low | Scope stack is a list (serializable). Mitigation: Include as-is in trace event. No special handling needed. |
| **Test coverage for all opcodes** | Medium | Need to test tracing for all opcode types (11 opcodes). Mitigation: Parameterized tests. Property-based tests. |
| **Replay determinism** | High | Replay must produce identical trace. Mitigation: Use deterministic scheduler. Test replay vs original trace. Validate state equality at each step. |

## Recommended Approach

**High-Level Strategy**: Implement wf_trace module with trace level support (none, min, full), configurable sinks (callback, ETS, process), state snapshot/diff for exec_state, and replay log extraction. Integrate with wf_exec to emit events after each step. Extend wf_sched API to capture scheduler choices for replay log. Filter events by opcode/scope/branch_id. Validate replay determinism by comparing replayed trace with original.

### Phase 1: Types and Records (wf_trace)

**1. Define types and records** (`src/wf_trace.erl`):

```erlang
%% Trace event record
-record(trace_event, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode(),
    state_before :: binary() | undefined,
    state_after :: binary() | undefined,
    timestamp :: erlang:monotonic_time(),
    scope :: [term()],
    branch_id :: term() | undefined,
    metadata :: map()
}).

%% Replay entry record
-record(replay_entry, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode() | undefined,
    scheduler_choice :: wf_sched:choice_entry() | undefined,
    effect_result :: {effect_id(), term()} | undefined
}).

%% Trace state
-record(trace_state, {
    level :: trace_level(),
    sink :: trace_sink(),
    case_id :: term()
}).

%% Types
-type trace_level() :: none | min | full.
-type trace_sink() :: {callback, fun()} | {ets, ets:tid()} | {process, pid()}.
-type step_seq() :: non_neg_integer().

%% Type exports
-export_type([
    trace_level/0,
    trace_sink/0,
    trace_event/0,
    replay_entry/0,
    replay_log/0
]).
```

**2. Test type compilation**

### Phase 2: Trace State Management

**1. Implement `new/1`**:

```erlang
-spec new(trace_level()) -> {ok, trace_state()}.
new(Level) ->
    %% Create default ETS table sink
    Table = ets:new(wf_trace_events, [
        named_table,
        bag,  %% Allow duplicate events (multiple events per step_seq)
        public,
        {read_concurrency, true}
    ]),

    State = #trace_state{
        level = Level,
        sink = {ets, Table},
        case_id = undefined
    },
    {ok, State}.
```

**2. Implement `set_level/1`**:

```erlang
-spec set_level(trace_state(), trace_level()) -> trace_state().
set_level(State, Level) ->
    State#trace_state{level = Level}.
```

**3. Implement `get_level/0`**:

```erlang
-spec get_level(trace_state()) -> trace_level().
get_level(State) ->
    State#trace_state.level.
```

**4. Implement `set_sink/2`**:

```erlang
-spec set_sink(trace_state(), trace_sink()) -> trace_state().
set_sink(State, Sink) ->
    State#trace_state{sink = Sink}.
```

**5. Test trace state management**

### Phase 3: Event Emission

**1. Implement `emit/2`**:

```erlang
-spec emit(trace_state(), exec_state(), map()) -> ok.
emit(#trace_state{level = none}, _ExecState, _TraceEvent) ->
    %% No-op, zero overhead
    ok;

emit(#trace_state{level = Level} = State, ExecState, TraceEvent) ->
    Opcode = maps:get(opcode, TraceEvent),

    case Level of
        min ->
            %% Emit only structural events
            case is_structural_opcode(Opcode) of
                true -> emit_event(State, ExecState, TraceEvent);
                false -> ok
            end;
        full ->
            %% Emit all events
            emit_event(State, ExecState, TraceEvent)
    end.

is_structural_opcode({OpcodeName, _Arg}) ->
    lists:member(OpcodeName, ['PAR_FORK', 'JOIN_WAIT', 'CANCEL_SCOPE', 'DONE']).
```

**2. Implement `emit_event/3`**:

```erlang
emit_event(#trace_state{sink = Sink}, ExecState, TraceEvent) ->
    %% Build full trace event
    StepSeq = ExecState#exec_state.step_count + 1,
    Timestamp = erlang:monotonic_time(microsecond),
    Scope = ExecState#exec_state.scope_stack,

    %% Extract branch_id (if current token is in a branch)
    BranchId = extract_branch_id(ExecState),

    %% Capture state snapshots (if level=full)
    StateBefore = case State#trace_state.level of
        full -> snapshot_exec_state(ExecState);
        min -> undefined
    end,

    %% State after will be captured by caller (post-execution)
    %% For now, use undefined
    StateAfter = undefined,

    %% Build metadata
    Metadata = maps:get(metadata, TraceEvent, #{}),

    Event = #trace_event{
        step_seq = StepSeq,
        opcode = maps:get(opcode, TraceEvent),
        state_before = StateBefore,
        state_after = StateAfter,
        timestamp = Timestamp,
        scope = Scope,
        branch_id = BranchId,
        metadata = Metadata
    },

    %% Send to sink
    emit_to_sink(Event, Sink).

extract_branch_id(#exec_state{current_token = TokenId, branch_map = BranchMap}) ->
    case wf_exec:find_branch_for_token(TokenId, BranchMap) of
        {ok, BranchId} -> BranchId;
        error -> undefined
    end.
```

**3. Implement `emit_to_sink/2`**:

```erlang
emit_to_sink(Event, {callback, Fun}) ->
    Fun(Event),
    ok;

emit_to_sink(Event, {ets, Table}) ->
    ets:insert(Table, Event),
    ok;

emit_to_sink(Event, {process, Pid}) ->
    Pid ! {trace_event, Event},
    ok.
```

**4. Test event emission**

### Phase 4: State Snapshots

**1. Add snapshot functions to wf_exec** (`src/wf_exec.erl`):

```erlang
%% @doc Snapshot exec_state to binary
-spec snapshot_exec_state(exec_state()) -> binary().
snapshot_exec_state(ExecState) ->
    term_to_binary(ExecState).

%% @doc Restore exec_state from snapshot binary
-spec restore_exec_state(binary(), wf_vm:wf_bc()) -> {ok, exec_state()} | {error, term()}.
restore_exec_state(Binary, Bytecode) ->
    try binary_to_term(Binary) of
        ExecState ->
            case ExecState#exec_state.bytecode of
                Bytecode ->
                    {ok, ExecState};
                _Other ->
                    {error, {bytecode_mismatch, Bytecode}}
            end
    catch
        _:_ ->
            {error, invalid_snapshot}
    end.
```

**2. Export snapshot functions from wf_exec**

**3. Test snapshot/restore**

### Phase 5: Event Querying

**1. Implement `get_events/1`**:

```erlang
-spec get_events(trace_state()) -> [trace_event()].
get_events(#trace_state{sink = {ets, Table}}) ->
    ets:tab2list(Table);

get_events(#trace_state{sink = {process, _Pid}}) ->
    %% Process sink doesn't store events
    [];

get_events(#trace_state{sink = {callback, _Fun}}) ->
    %% Callback sink doesn't store events
    [].
```

**2. Implement `filter/2`**:

```erlang
-spec filter([trace_event()], {opcode, wf_vm:opcode()}) -> [trace_event()];
            ([trace_event()], {scope, term()}) -> [trace_event()];
            ([trace_event()], {branch, term()}) -> [trace_event()];
            ([trace_event()], {predicate, fun()}) -> [trace_event()].

filter(Events, {opcode, Opcode}) ->
    [E || E <- Events, E#trace_event.opcode =:= Opcode];

filter(Events, {scope, ScopeId}) ->
    [E || E <- Events, lists:member(ScopeId, E#trace_event.scope)];

filter(Events, {branch, BranchId}) ->
    [E || E <- Events, E#trace_event.branch_id =:= BranchId];

filter(Events, {predicate, Fun}) when is_function(Fun, 1) ->
    [E || E <- Events, Fun(E)].
```

**3. Test event querying**

### Phase 6: Replay Log

**1. Implement `to_replay_log/1`**:

```erlang
-spec to_replay_log([trace_event()]) -> replay_log().
to_replay_log(TraceEvents) ->
    lists:filtermap(fun(#trace_event{step_seq = StepSeq, opcode = Opcode, metadata = Metadata}) ->
        case maps:get(scheduler_choice, Metadata, undefined) of
            undefined ->
                case maps:get(effect_result, Metadata, undefined) of
                    undefined -> false;
                    EffectResult ->
                        {true, #replay_entry{
                            step_seq = StepSeq,
                            opcode = Opcode,
                            scheduler_choice = undefined,
                            effect_result = EffectResult
                        }}
                end;
            SchedChoice ->
                {true, #replay_entry{
                    step_seq = StepSeq,
                    opcode = Opcode,
                    scheduler_choice = SchedChoice,
                    effect_result = maps:get(effect_result, Metadata, undefined)
                }}
        end
    end, TraceEvents).
```

**2. Implement `from_replay_log/1`**:

```erlang
-spec from_replay_log(replay_log()) -> {ok, trace_state()}.
from_replay_log(ReplayLog) ->
    %% Create trace state from replay log
    State = #trace_state{
        level = full,
        sink = {replay, ReplayLog},
        case_id = undefined
    },
    {ok, State}.
```

**3. Test replay log generation**

### Phase 7: Executor Integration

**1. Extend wf_exec:step/2 to call wf_trace**:

```erlang
step(ExecState, SchedDecision) ->
    %% Capture state before
    StateBefore = wf_exec:snapshot_exec_state(ExecState),

    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),

    %% Capture state after
    StateAfter = wf_exec:snapshot_exec_state(NewExecState),

    %% Build trace event
    TraceEvent = #{
        opcode => Opcode,
        state_before => StateBefore,
        state_after => StateAfter,
        scheduler_choice => SchedDecision,
        metadata => extract_metadata(Opcode, NewExecState)
    },

    %% Emit trace event
    wf_trace:emit(wf_trace_state, NewExecState, TraceEvent),

    {NewExecState, TraceEvent}.
```

**2. Add wf_trace_state to exec_state** (optional, or use process dictionary)

**3. Test executor integration**

### Phase 8: Scheduler Integration

**1. Extend wf_sched to return choice entry**:

```erlang
-spec choose_with_entry([wf_sched:enabled_action()], sched_state()) ->
    {wf_sched:enabled_action(), choice_entry(), sched_state()}.
choose_with_entry(EnabledActions, SchedState) ->
    {Chosen, NewSchedState} = wf_sched:choose(EnabledActions, SchedState),
    Entry = extract_choice_entry(NewSchedState),
    {Chosen, Entry, NewSchedState}.
```

**2. Capture scheduler choice in trace event**:

```erlang
%% In wf_exec:step/2
{Chosen, ChoiceEntry, NewSchedState} = wf_sched:choose_with_entry(EnabledActions, SchedState),

TraceEvent = #{
    opcode => Opcode,
    scheduler_choice => ChoiceEntry,
    metadata => #{}
},
```

**3. Test scheduler integration**

### Phase 9: Comprehensive Testing

**1. Unit tests**:
- Test trace level none (no events)
- Test trace level min (structural events only)
- Test trace level full (all events)
- Test trace sinks (callback, ETS, process)
- Test event filtering (opcode, scope, branch)
- Test replay log generation
- Test state snapshots
- Test event ordering

**2. Integration tests**:
- Test wf_exec emits trace events
- Test trace events contain correct fields
- Test replay log captures scheduler choices
- Test replay replay produces identical trace

**3. Property-based tests**:
```erlang
%% Property: Replay log is smaller than full trace
prop_replay_log_smaller() ->
    ?FORALL(Trace, trace_gen(),
        begin
            ReplayLog = wf_trace:to_replay_log(Trace),
            length(ReplayLog) =< length(Trace)
        end).

%% Property: Trace events ordered by step_seq
prop_trace_events_ordered() ->
    ?FORALL(Trace, trace_gen(),
        begin
            StepSeqs = [E#trace_event.step_seq || E <- Trace],
            StepSeqs =:= lists:sort(StepSeqs)
        end).
```

## Open Questions

1. **Trace State Storage**: Should trace state be stored in exec_state, process dictionary, or gen_server?
   - **Recommendation**: Process dictionary for v1 (simple). Gen_server for v2 (persistence, query API).

2. **State Snapshot Format**: Should we use snapshots or diffs for state_before/state_after?
   - **Recommendation**: Snapshots for v1 (simple). Diffs for v2 (performance).

3. **Effect Result Field**: wf_effect not implemented yet (item 010 is idea state). How to handle effect_result in replay log?
   - **Recommendation**: Stub effect_result field for v1. Integrate when item 010 complete.

4. **Scheduler Choice Capture**: Should we extend wf_sched API to return choice entry, or extract from sched state?
   - **Recommendation**: Extend API with `choose_with_entry/2` (cleaner than inspecting state).

5. **Trace Event Ordering**: Should we enforce ordering in trace sink, or rely on step_seq?
   - **Recommendation**: Rely on step_seq for ordering. Sort on retrieval if needed.

6. **ETS Table Ownership**: Should trace ETS table be owned by wf_trace gen_server or wf_exec process?
   - **Recommendation**: Own in wf_trace gen_server (persistent across executors).

7. **Trace Retention Policy**: How long to keep trace events in ETS table?
   - **Recommendation**: For v1, keep until deleted. For v2, implement retention policy (time-based or size-based).

8. **Multiple Executors**: Should each executor have separate trace state, or shared global trace state?
   - **Recommendation**: Each executor has separate trace state (case_id correlation). Global trace store aggregates all.

9. **Trace Event Compression**: Should we compress trace events (e.g., gzip) for storage?
   - **Recommendation**: No compression for v1. Consider for v2 if disk persistence implemented.

10. **Replay Log Validation**: Should we validate replay log before use (check format, verify step_seq ordering)?
    - **Recommendation**: Yes, validate in `from_replay_log/1`. Return error if invalid.

11. **Trace Level Switching**: Should we allow switching trace level during execution?
    - **Recommendation**: No, set level before execution. Document restriction.

12. **Metadata Extraction**: How to extract metadata per opcode type without complex pattern matching?
    - **Recommendation**: Implement `metadata_for_opcode/2` function with pattern match per opcode.

13. **Branch ID Extraction**: How to extract branch_id for current token efficiently?
    - **Recommendation**: Use existing `find_branch_for_token/2` in wf_exec (line 682-688).

14. **Scope Stack Depth**: Should we limit scope stack depth for trace events?
    - **Recommendation**: No limit for v1. Warn if depth > 1000 (possible infinite loop).

15. **Trace Sink Error Handling**: What to do if trace sink fails (e.g., ETS insert fails, process not alive)?
    - **Recommendation**: Log error, continue execution. Don't crash executor.

16. **Replay Determinism Tests**: How to validate replay produces identical execution?
    - **Recommendation**: Compare trace events step-by-step. Use property-based tests.

17. **Effect Yield Tracing**: How to trace effect_yield events (item 010 not implemented)?
    - **Recommendation**: Stub effect_yield as structural event. Implement when item 010 complete.

18. **Trace Event ID**: Should each trace event have unique ID (independent of step_seq)?
    - **Recommendation**: No, step_seq is sufficient identifier.

19. **Timestamp Precision**: Should we use microseconds or nanoseconds for timestamp?
    - **Recommendation**: Microseconds (erlang:monotonic_time(microsecond)). Nanoseconds not needed.

20. **Case ID Addition**: Should we add case_id to exec_state record for trace correlation?
    - **Recommendation**: Yes, add case_id field to #exec_state{}. Generate in wf_exec:new/1 using make_ref().
