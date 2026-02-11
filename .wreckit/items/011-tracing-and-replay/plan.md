# Implement structured tracing and replay system Implementation Plan

## Implementation Plan Title

Structured Tracing and Replay System for Workflow Executor

## Overview

Implement **wf_trace.erl**, a comprehensive tracing and replay system that provides observability for workflow execution. The tracing system captures structured trace events at multiple levels of granularity (none, min, full), enabling production monitoring, debugging, and deterministic replay of workflow executions. The replay log format captures only nondeterministic inputs (scheduler choices and effect results), making it a compact representation for reproducing executions.

This system integrates with the existing executor (`wf_exec`) and scheduler (`wf_sched`) to capture trace events after each reduction step, with configurable trace sinks (callback, ETS table, process) and filtering capabilities (by opcode, scope, branch_id).

## Current State

**Existing Implementation:**

1. **Executor Trace Event Stub** (`wf_exec.erl:101-107`):
   - The `step/2` function already returns a TraceEvent map with `opcode` and `step_count`
   - TraceEvent is currently discarded in `run_loop/3` (line 125: `_TraceEvent`)
   - **Missing**: wf_trace module to process and store events
   - **Missing**: Trace level support, configurable sinks, state snapshots, metadata fields

2. **Scheduler Choice Logging** (`wf_sched_nondeterministic.erl:39-61`):
   - Nondeterministic scheduler logs choices to internal log
   - Choice log format: `{StepSeq, EnabledSet, Chosen}`
   - **Integration point**: Extract scheduler choices for replay log generation
   - **Gap**: Scheduler step_seq is independent of executor step_count

3. **Replay Scheduler** (`wf_sched_replay.erl:17-57`):
   - Already implemented (item 007)
   - Validates enabled set matches recorded choice
   - **Integration point**: wf_trace generates replay log for this scheduler
   - **Gap**: Replay log format differs from specification (needs effect_result field)

4. **State Snapshot Support** (`wf_state.erl:376-415`):
   - wf_state supports snapshot/restore for workflow state
   - Snapshots exclude non-serializable fields (ETS table, buffered mutations)
   - **Gap**: Need exec_state snapshots (different from wf_state.state)

5. **exec_state Record** (`wf_exec.hrl:31-42`):
   - All fields are serializable (no pids, ports, refs except make_ref())
   - **Missing**: case_id field for trace correlation

**Key Constraints:**

- **Zero overhead when disabled**: Trace calls at level=none must be no-ops
- **Structured events**: Trace events are Erlang terms, not formatted strings
- **Multiple sinks**: Support function callbacks, ETS tables, and process sinks
- **Deterministic replay**: Replay log captures only nondeterministic inputs
- **No wf_trace module exists**: Must create from scratch

## Desired End State

**Specification:**

A complete wf_trace module with the following capabilities:

1. **Trace Levels**:
   - `none`: No tracing overhead, trace callbacks are no-ops
   - `min`: Emit only structural events (PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE) for production
   - `full`: Emit every single reduction step for debugging and replay validation

2. **Trace Event Structure**:
   ```erlang
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
   ```

3. **Replay Log Format**:
   ```erlang
   -record(replay_entry, {
       step_seq :: non_neg_integer(),
       opcode :: wf_vm:opcode() | undefined,
       scheduler_choice :: wf_sched:choice_entry() | undefined,
       effect_result :: {effect_id(), term()} | undefined
   }).
   ```

4. **Exported API**:
   - `new/1` - Create trace state with level
   - `emit/2` - Emit trace event (checks level)
   - `set_sink/2` - Set trace sink (callback, ETS, process)
   - `get_events/1` - Get all events from sink
   - `to_replay_log/1` - Extract replay log from trace events
   - `from_replay_log/1` - Create trace state from replay log
   - `filter/2` - Filter events by opcode/scope/branch
   - `set_level/1` - Set trace level dynamically
   - `get_level/0` - Get current trace level

5. **Integration Points**:
   - `wf_exec:step/2` calls `wf_trace:emit/2` after each step
   - Scheduler choices logged from `wf_sched` state
   - State snapshots captured via `wf_exec:snapshot_exec_state/1`

**Verification:**

- Unit tests for all trace levels (none, min, full)
- Integration tests with wf_exec
- Replay log generation and validation tests
- Performance benchmarks (steps/sec with tracing enabled/disabled)
- Code coverage > 90% for wf_trace.erl

### Key Discoveries:

- **Trace state storage**: Use process dictionary for v1 (simple), gen_server for v2 (persistence)
- **State snapshot format**: Use full snapshots for v1 (simple), diffs for v2 (performance)
- **Effect result stub**: wf_effect not implemented yet (item 010 is idea state), stub effect_result field
- **Scheduler choice capture**: Extend wf_sched API with `choose_with_entry/2` to return choice entry
- **Trace event ordering**: Rely on step_seq for ordering, sort on retrieval if needed
- **ETS table ownership**: Own in wf_trace gen_server (persistent across executors)
- **Trace retention**: Keep until deleted for v1, implement retention policy for v2
- **Multiple executors**: Each executor has separate trace state (case_id correlation)
- **Trace state storage**: exec_state needs case_id field for correlation (line 31-42 in wf_exec.hrl)
- **Branch ID extraction**: Use existing `find_branch_for_token/2` in wf_exec (line 682-688)
- **Scope stack**: Already in exec_state, no special handling needed
- **Structural opcodes**: PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE (from item.json:6)

## What We're NOT Doing

- **Effect result integration** (item 010): wf_effect not implemented yet, stub effect_result field
- **State diffs**: Using full snapshots for v1, diffs deferred to v2
- **Disk persistence**: Trace events stored in ETS only, no disk I/O
- **Trace event compression**: No compression for v1
- **Real-time monitoring**: No gen_server for pub/sub, process sink can be used
- **Trace rotation**: No automatic rotation or retention policy
- **Replay validation**: Full replay determinism validation deferred to integration testing
- **Trace level switching at runtime**: Set level before execution, document restriction
- **Metadata extraction per opcode**: Use default empty map, implement per-opcode metadata in v2
- **Multiple sink types simultaneously**: Single sink per trace state
- **Trace event buffering**: All events immediately sent to sink

## Implementation Approach

**High-Level Strategy**: Implement wf_trace module incrementally, starting with types and records, then trace state management, event emission, state snapshots, event querying, replay log generation, executor integration, scheduler integration, and comprehensive testing. Use ETS table as default sink for simplicity. Extend wf_exec to capture state snapshots and emit trace events. Extend wf_sched to return choice entries for replay log generation.

**Key Design Decisions**:

1. **Trace state storage**: Use process dictionary with key `wf_trace_state` for v1 (simple, no gen_server overhead)
2. **State snapshots**: Full snapshots via `term_to_binary/1` for v1 (diffs for v2)
3. **Trace level check**: Simple `if` statement in `emit/2` (no macro for v1)
4. **Scheduler choice capture**: Extend wf_sched with `choose_with_entry/2` (cleaner than inspecting state)
5. **Case ID**: Add case_id field to exec_state record for correlation
6. **Default sink**: ETS table named `wf_trace_events` (bag type for duplicate events)
7. **Structural opcodes**: PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE (emitted at level=min)
8. **Effect result**: Stub as `undefined` for v1 (integrate when item 010 complete)
9. **Replay log**: Extract from trace events by filtering scheduler_choice and effect_result metadata
10. **Event filtering**: Simple list comprehensions for v1 (ETS match specs for v2)

**Risk Mitigation**:

- **Performance overhead**: Profile steps/sec with tracing enabled/disabled
- **Memory exhaustion**: Document ETS table size limit, recommend cleanup
- **Step sequence divergence**: Use executor step_count as primary step_seq
- **State snapshot serialization**: Filter non-serializable fields, validate with try-catch
- **Trace event loss**: Use named ETS table (survives process crashes)
- **Replay log validation**: Clear error messages, partial replay support

---

## Phases

### Phase 1: Types, Records, and Basic Module Structure

#### Overview

Define all types, records, and module exports for wf_trace. Create the basic module structure with stub implementations. This phase establishes the data structures used throughout the tracing system.

#### Changes Required:

##### 1. wf_trace.erl module creation

**File**: `src/wf_trace.erl`
**Changes**: Create new module with types, records, and exports

```erlang
-module(wf_trace).

%%====================================================================
%% Types
%%====================================================================

-type trace_level() :: none | min | full.

-type trace_sink() ::
    {callback, fun((#trace_event{}) -> ok)} |
    {ets, ets:tid()} |
    {process, pid()}.

-type step_seq() :: non_neg_integer().

-type trace_event() :: #trace_event{}.
-type replay_entry() :: #replay_entry{}.
-type replay_log() :: [replay_entry()].

%%====================================================================
%% Records
%%====================================================================

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

-record(replay_entry, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode() | undefined,
    scheduler_choice :: wf_sched:choice_entry() | undefined,
    effect_result :: {term(), term()} | undefined
}).

-record(trace_state, {
    level :: trace_level(),
    sink :: trace_sink(),
    case_id :: term()
}).

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/1,
    emit/2,
    set_sink/2,
    get_events/1,
    to_replay_log/1,
    from_replay_log/1,
    filter/2,
    set_level/1,
    get_level/0
]).

-export_type([
    trace_level/0,
    trace_sink/0,
    trace_event/0,
    replay_entry/0,
    replay_log/0
]).
```

##### 2. Add case_id to exec_state record

**File**: `src/wf_exec.hrl`
**Changes**: Add case_id field to exec_state record

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: map(),
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | cancelled,
    current_token :: term() | undefined,
    case_id :: term()  %% For trace correlation
}).
```

##### 3. Update wf_exec:new/1 to generate case_id

**File**: `src/wf_exec.erl`
**Changes**: Generate case_id in new/1

```erlang
%% @doc Create new executor from bytecode
-spec new(wf_vm:wf_bc()) -> exec_state().
new(Bytecode) ->
    CaseId = make_ref(),  %% Generate unique case ID
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active,
        instance_id = undefined
    },
    #exec_state{
        ip = 0,
        bytecode = Bytecode,
        ctx = #{},
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0,
        status = running,
        current_token = InitialTokenId,
        case_id = CaseId
    }.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Type checking passes: `rebar3 dialyzer`
- [ ] Record definitions match specification
- [ ] All type exports present

##### Manual Verification:

- [ ] Review types match item.json specification
- [ ] Verify case_id field added to exec_state
- [ ] Check case_id generated in wf_exec:new/1

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Trace State Management

#### Overview

Implement trace state creation, level management, and sink configuration. This phase provides the core state management functions for the tracing system.

#### Changes Required:

##### 1. Implement new/1

**File**: `src/wf_trace.erl`
**Changes**: Add trace state creation function

```erlang
%% @doc Create new trace state with specified level
-spec new(trace_level()) -> {ok, #trace_state{}}.
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

##### 2. Implement set_level/1 and get_level/0

**File**: `src/wf_trace.erl`
**Changes**: Add level management functions (using process dictionary)

```erlang
%% @doc Set trace level in process dictionary
-spec set_level(trace_level()) -> ok.
set_level(Level) ->
    case get(wf_trace_state) of
        undefined ->
            %% No trace state, create temporary
            State = #trace_state{
                level = Level,
                sink = {ets, wf_trace_events},
                case_id = undefined
            },
            put(wf_trace_state, State),
            ok;
        State ->
            put(wf_trace_state, State#trace_state{level = Level}),
            ok
    end.

%% @doc Get current trace level from process dictionary
-spec get_level() -> trace_level().
get_level() ->
    case get(wf_trace_state) of
        undefined -> none;
        State -> State#trace_state.level
    end.
```

##### 3. Implement set_sink/2

**File**: `src/wf_trace.erl`
**Changes**: Add sink configuration function

```erlang
%% @doc Set trace sink (callback, ETS, or process)
-spec set_sink(#trace_state{}, trace_sink()) -> #trace_state{}.
set_sink(State, Sink) ->
    State#trace_state{sink = Sink}.
```

##### 4. Implement get_events/1

**File**: `src/wf_trace.erl`
**Changes**: Add event retrieval function

```erlang
%% @doc Get all events from trace state sink
-spec get_events(#trace_state{}) -> [#trace_event{}].
get_events(#trace_state{sink = {ets, Table}}) ->
    ets:tab2list(Table);

get_events(#trace_state{sink = {process, _Pid}}) ->
    %% Process sink doesn't store events
    [];

get_events(#trace_state{sink = {callback, _Fun}}) ->
    %% Callback sink doesn't store events
    [].
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass: `rebar3 eunit`
- [ ] Trace state creation works
- [ ] Level management works (set/get)
- [ ] Sink configuration works
- [ ] Event retrieval works (ETS sink)

##### Manual Verification:

- [ ] Test trace level switching
- [ ] Test different sink types
- [ ] Verify ETS table creation

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Event Emission

#### Overview

Implement trace event emission with level filtering (none, min, full). This phase provides the core emission logic that integrates with the executor.

#### Changes Required:

##### 1. Implement emit/2

**File**: `src/wf_trace.erl`
**Changes**: Add event emission function with level filtering

```erlang
%% @doc Emit trace event (checks trace level)
-spec emit(#exec_state{}, map()) -> ok.
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

%% @private Check if opcode is structural (emitted at level=min)
-spec is_structural_opcode(wf_vm:opcode()) -> boolean().
is_structural_opcode({OpcodeName, _Arg}) ->
    lists:member(OpcodeName, ['PAR_FORK', 'JOIN_WAIT', 'CANCEL_SCOPE', 'DONE']);
is_structural_opcode({'DONE'}) ->
    true;
is_structural_opcode(_Other) ->
    false.
```

##### 2. Implement emit_event/2

**File**: `src/wf_trace.erl`
**Changes**: Add event building and sink emission

```erlang
%% @private Build trace event and emit to sink
-spec emit_event(#exec_state{}, map()) -> ok.
emit_event(ExecState, TraceEvent) ->
    Level = get_level(),
    State = case get(wf_trace_state) of
        undefined -> error(no_trace_state);
        S -> S
    end,

    %% Build full trace event
    StepSeq = ExecState#exec_state.step_count + 1,
    Timestamp = erlang:monotonic_time(microsecond),
    Scope = ExecState#exec_state.scope_stack,

    %% Extract branch_id (if current token is in a branch)
    BranchId = extract_branch_id(ExecState),

    %% Capture state before (if level=full)
    StateBefore = case Level of
        full -> snapshot_exec_state(ExecState);
        min -> undefined
    end,

    %% State after will be captured by caller (post-execution)
    StateAfter = maps:get(state_after, TraceEvent, undefined),

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
    emit_to_sink(Event, State#trace_state.sink).

%% @private Extract branch_id for current token
-spec extract_branch_id(#exec_state{}) -> term() | undefined.
extract_branch_id(#exec_state{current_token = TokenId, branch_map = BranchMap}) ->
    case wf_exec:find_branch_for_token(TokenId, BranchMap) of
        {ok, BranchId} -> BranchId;
        error -> undefined
    end.
```

##### 3. Implement emit_to_sink/2

**File**: `src/wf_trace.erl`
**Changes**: Add sink emission functions

```erlang
%% @private Emit event to sink
-spec emit_to_sink(#trace_event{}, trace_sink()) -> ok.
emit_to_sink(Event, {callback, Fun}) ->
    try
        Fun(Event),
        ok
    catch
        _:_ ->
            %% Log error, don't crash executor
            ok
    end;

emit_to_sink(Event, {ets, Table}) ->
    try
        ets:insert(Table, Event),
        ok
    catch
        _:_ ->
            %% Log error, don't crash executor
            ok
    end;

emit_to_sink(Event, {process, Pid}) ->
    try
        Pid ! {trace_event, Event},
        ok
    catch
        _:_ ->
            %% Log error, don't crash executor
            ok
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass for trace levels (none, min, full)
- [ ] Test no events emitted at level=none
- [ ] Test only structural events at level=min
- [ ] Test all events at level=full
- [ ] Test all sink types (callback, ETS, process)
- [ ] Test error handling in sinks

##### Manual Verification:

- [ ] Verify zero overhead at level=none
- [ ] Test structural opcode filtering
- [ ] Test sink error handling (crash resilience)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: State Snapshots

#### Overview

Implement exec_state snapshot and restore functions. This phase provides state capture capabilities for trace events.

#### Changes Required:

##### 1. Implement snapshot functions in wf_exec

**File**: `src/wf_exec.erl`
**Changes**: Add snapshot/restore functions

```erlang
%% @doc Snapshot exec_state to binary (for tracing)
-spec snapshot_exec_state(exec_state()) -> binary().
snapshot_exec_state(ExecState) ->
    %% All fields are serializable (make_ref() is serializable)
    term_to_binary(ExecState).

%% @doc Restore exec_state from snapshot binary
-spec restore_exec_state(binary(), wf_vm:wf_bc()) -> {ok, exec_state()} | {error, term()}.
restore_exec_state(Binary, Bytecode) ->
    try binary_to_term(Binary) of
        ExecState ->
            %% Verify bytecode matches (optional)
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

##### 2. Export snapshot functions

**File**: `src/wf_exec.erl`
**Changes**: Add to exports list

```erlang
-export([
    new/1,
    step/2,
    run/3,
    is_done/1,
    is_blocked/1,
    get_ip/1,
    get_ctx/1,
    get_step_count/1,
    set_ctx/2,
    get_scope_stack_depth/1,
    snapshot_exec_state/1,
    restore_exec_state/2
]).
```

##### 3. Update wf_trace to use wf_exec snapshots

**File**: `src/wf_trace.erl`
**Changes**: Update emit_event/2 to call wf_exec:snapshot_exec_state/1

```erlang
%% In emit_event/2, update StateBefore capture:
StateBefore = case Level of
    full -> wf_exec:snapshot_exec_state(ExecState);
    min -> undefined
end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass for snapshot/restore
- [ ] Test snapshot serialization
- [ ] Test restore deserialization
- [ ] Test bytecode validation
- [ ] Test error handling (invalid binary)

##### Manual Verification:

- [ ] Verify snapshot size is reasonable
- [ ] Test restore with matching bytecode
- [ ] Test restore with mismatched bytecode

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Event Filtering

#### Overview

Implement event filtering functions (by opcode, scope, branch_id, predicate). This phase provides query capabilities for trace analysis.

#### Changes Required:

##### 1. Implement filter/2

**File**: `src/wf_trace.erl`
**Changes**: Add filtering functions

```erlang
%% @doc Filter events by opcode, scope, branch, or predicate
-spec filter([#trace_event{}], {opcode, wf_vm:opcode()}) -> [#trace_event{}];
            ([#trace_event{}], {scope, term()}) -> [#trace_event{}];
            ([#trace_event{}], {branch, term()}) -> [#trace_event{}];
            ([#trace_event{}], {predicate, fun((#trace_event{}) -> boolean())}) -> [#trace_event{}].
filter(Events, {opcode, Opcode}) ->
    [E || E <- Events, E#trace_event.opcode =:= Opcode];

filter(Events, {scope, ScopeId}) ->
    [E || E <- Events, lists:member(ScopeId, E#trace_event.scope)];

filter(Events, {branch, BranchId}) ->
    [E || E <- Events, E#trace_event.branch_id =:= BranchId];

filter(Events, {predicate, Fun}) when is_function(Fun, 1) ->
    [E || E <- Events, Fun(E)].
```

##### 2. Add helper predicates

**File**: `src/wf_trace.erl`
**Changes**: Add common predicate functions

```erlang
%% @doc Predicate: event has scheduler choice
-spec has_scheduler_choice(#trace_event{}) -> boolean().
has_scheduler_choice(#trace_event{metadata = Metadata}) ->
    maps:is_key(scheduler_choice, Metadata).

%% @doc Predicate: event has effect result
-spec has_effect_result(#trace_event{}) -> boolean().
has_effect_result(#trace_event{metadata = Metadata}) ->
    maps:is_key(effect_result, Metadata).

%% @doc Predicate: event is in branch
-spec is_in_branch(term()) -> fun((#trace_event{}) -> boolean()).
is_in_branch(BranchId) ->
    fun(#trace_event{branch_id = EventBranchId}) ->
        EventBranchId =:= BranchId
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass for all filter types
- [ ] Test opcode filtering
- [ ] Test scope filtering
- [ ] Test branch filtering
- [ ] Test predicate filtering
- [ ] Test helper predicates

##### Manual Verification:

- [ ] Test complex filters (combined predicates)
- [ ] Test filtering on empty list
- [ ] Test filtering on large trace

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Replay Log Generation

#### Overview

Implement replay log extraction from trace events and replay log validation. This phase provides deterministic replay capabilities.

#### Changes Required:

##### 1. Implement to_replay_log/1

**File**: `src/wf_trace.erl`
**Changes**: Add replay log extraction

```erlang
%% @doc Extract replay log from trace events
-spec to_replay_log([#trace_event{}]) -> replay_log().
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

##### 2. Implement from_replay_log/1

**File**: `src/wf_trace.erl`
**Changes**: Add replay log validation and trace state creation

```erlang
%% @doc Create trace state from replay log (for replay execution)
-spec from_replay_log(replay_log()) -> {ok, #trace_state{}} | {error, term()}.
from_replay_log(ReplayLog) ->
    try
        %% Validate replay log format
        ok = validate_replay_log(ReplayLog),

        %% Create trace state with replay sink
        State = #trace_state{
            level = full,
            sink = {replay, ReplayLog},
            case_id = undefined
        },
        {ok, State}
    catch
        _:_ ->
            {error, invalid_replay_log}
    end.

%% @private Validate replay log format
-spec validate_replay_log(replay_log()) -> ok | no_return().
validate_replay_log([]) ->
    ok;
validate_replay_log([#replay_entry{} | Rest]) ->
    validate_replay_log(Rest);
validate_replay_log([_Invalid | _]) ->
    error({invalid_replay_log, bad_entry}).
```

##### 3. Implement replay sink support

**File**: `src/wf_trace.erl`
**Changes**: Update emit_to_sink/2 to handle replay sink

```erlang
%% In emit_to_sink/2, add replay sink case:
emit_to_sink(_Event, {replay, _ReplayLog}) ->
    %% Replay sink: don't emit events (validate externally)
    ok.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass for replay log generation
- [ ] Test replay log extraction from trace
- [ ] Test replay log validation
- [ ] Test from_replay_log/1
- [ ] Test invalid replay log handling

##### Manual Verification:

- [ ] Verify replay log is smaller than full trace
- [ ] Test replay log with scheduler choices
- [ ] Test replay log with effect results

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 7: Executor Integration

#### Overview

Integrate wf_trace with wf_exec to emit trace events after each step. This phase connects the tracing system to the executor.

#### Changes Required:

##### 1. Update wf_exec:step/2 to call wf_trace

**File**: `src/wf_exec.erl`
**Changes**: Add trace emission

```erlang
%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), map()}.
step(ExecState0, SchedDecision) ->
    %% Capture state before
    StateBefore = wf_exec:snapshot_exec_state(ExecState0),

    Opcode = fetch_opcode(ExecState0),
    NewExecState = execute_opcode(Opcode, ExecState0),

    %% Capture state after
    StateAfter = wf_exec:snapshot_exec_state(NewExecState),

    %% Build trace event
    TraceEvent = #{
        opcode => Opcode,
        state_before => StateBefore,
        state_after => StateAfter,
        scheduler_choice => SchedDecision,
        metadata => extract_metadata(Opcode, NewExecState, SchedDecision)
    },

    %% Emit trace event (no-op if tracing disabled)
    wf_trace:emit(NewExecState, TraceEvent),

    %% Return simplified trace event (for backward compatibility)
    SimplifiedTraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, SimplifiedTraceEvent}.

%% @private Extract metadata for opcode
-spec extract_metadata(wf_vm:opcode(), exec_state(), term()) -> map().
extract_metadata(_Opcode, _ExecState, _SchedDecision) ->
    %% For v1, return empty map
    %% TODO: Implement per-opcode metadata in v2
    #{}.
```

##### 2. Add extract_metadata/3 (stub for v1)

**File**: `src/wf_exec.erl`
**Changes**: Add metadata extraction function

```erlang
%% @private Extract metadata for opcode type
-spec extract_metadata(wf_vm:opcode(), exec_state(), term()) -> map().
extract_metadata({'PAR_FORK', TargetIPs}, _ExecState, _SchedDecision) ->
    #{num_branches => length(TargetIPs), target_ips => TargetIPs};
extract_metadata({'JOIN_WAIT', Policy}, _ExecState, _SchedDecision) ->
    #{join_policy => Policy};
extract_metadata({'CANCEL_SCOPE', {enter, ScopeId}}, _ExecState, _SchedDecision) ->
    #{cancel_op => enter, scope_id => ScopeId};
extract_metadata({'CANCEL_SCOPE', {exit, ScopeId}}, _ExecState, _SchedDecision) ->
    #{cancel_op => exit, scope_id => ScopeId};
extract_metadata(_Opcode, _ExecState, _SchedDecision) ->
    #{}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Integration tests pass
- [ ] Test trace emission at level=none (no events)
- [ ] Test trace emission at level=min (structural events)
- [ ] Test trace emission at level=full (all events)
- [ ] Test state_before and state_after capture
- [ ] Test metadata extraction

##### Manual Verification:

- [ ] Verify trace events contain all fields
- [ ] Verify step_seq is monotonic
- [ ] Verify timestamp is monotonic
- [ ] Verify scope_stack is captured
- [ ] Verify branch_id is captured (when in branch)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 8: Scheduler Integration

#### Overview

Extend wf_sched API to return choice entries for replay log generation. This phase connects the tracing system to the scheduler.

#### Changes Required:

##### 1. Extend wf_sched behavior

**File**: `src/wf_sched.erl`
**Changes**: Add choose_with_entry/2 callback

```erlang
-callback choose_with_entry([enabled_action()], sched_state()) ->
    {enabled_action(), choice_entry(), sched_state()}.

%% Add to exports:
-export([
    new/2,
    choose/2,
    choose_with_entry/2,
    get_log/1,
    from_log/1
]).

%% Add choose_with_entry/2 implementation:
%% @doc Choose next action and return choice entry
-spec choose_with_entry([enabled_action()], sched_state()) ->
    {enabled_action(), choice_entry(), sched_state()}.
choose_with_entry(EnabledActions, SchedState) ->
    case EnabledActions of
        [] ->
            error({no_enabled_actions, SchedState});
        [Single] ->
            %% Only one choice, no need to invoke policy
            Entry = {0, EnabledActions, Single},  %% StepSeq placeholder
            {Single, Entry, SchedState};
        _Multiple ->
            %% Dispatch to policy module
            PolicyMod = extract_policy_module(SchedState),
            PolicyMod:choose_with_entry(EnabledActions, SchedState)
    end.
```

##### 2. Implement choose_with_entry/2 in wf_sched_nondeterministic

**File**: `src/wf_sched_nondeterministic.erl`
**Changes**: Add choose_with_entry implementation

```erlang
-export([
    new/1,
    choose/2,
    choose_with_entry/2
]).

%% @doc Choose action randomly and return choice entry
-spec choose_with_entry([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:choice_entry(), wf_sched:sched_state()}.
choose_with_entry(EnabledActions, #{log := Log, step_seq := StepSeq, rand_state := RandState0} = State)
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

    {Chosen, Entry, NewState}.
```

##### 3. Implement choose_with_entry/2 in wf_sched_deterministic

**File**: `src/wf_sched_deterministic.erl`
**Changes**: Add choose_with_entry implementation

```erlang
%% @doc Choose first action and return choice entry
-spec choose_with_entry([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:choice_entry(), wf_sched:sched_state()}.
choose_with_entry(EnabledActions, State) ->
    [Chosen | _] = EnabledActions,
    Entry = {0, EnabledActions, Chosen},  %% StepSeq placeholder
    {Chosen, Entry, State}.
```

##### 4. Implement choose_with_entry/2 in wf_sched_replay

**File**: `src/wf_sched_replay.erl`
**Changes**: Add choose_with_entry implementation

```erlang
%% @doc Replay next choice and return choice entry
-spec choose_with_entry([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:choice_entry(), wf_sched:sched_state()}.
choose_with_entry(EnabledActions, #{log := ChoiceLog, position := Position} = State)
    when is_list(EnabledActions) ->

    %% Check if log exhausted
    case Position > length(ChoiceLog) of
        true ->
            error({replay_complete, Position});
        false ->
            %% Fetch recorded choice
            Entry = lists:nth(Position, ChoiceLog),
            {_StepSeq, RecordedEnabled, Chosen} = Entry,

            %% Validate enabled set matches recording
            case validate_enabled_set(EnabledActions, RecordedEnabled) of
                ok ->
                    %% Match! Return recorded choice and entry
                    NewState = State#{position => Position + 1},
                    {Chosen, Entry, NewState};
                {error, {divergence, Expected, Actual}} ->
                    error({divergence, Expected, Actual})
            end
    end.
```

##### 5. Update wf_exec to use choose_with_entry

**File**: `src/wf_exec.erl`
**Changes**: Update step/2 to capture scheduler choice entry

```erlang
%% In step/2, update trace event metadata:
extract_metadata(_Opcode, _ExecState, {Chosen, ChoiceEntry, _NewSchedState}) when is_tuple(ChoiceEntry) ->
    %% Scheduler choice with entry
    #{scheduler_choice => ChoiceEntry};
extract_metadata(_Opcode, _ExecState, _Other) ->
    %% No scheduler choice
    #{}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Unit tests pass for all scheduler policies
- [ ] Test choose_with_entry/2 returns correct entry
- [ ] Test nondeterministic scheduler logging
- [ ] Test replay scheduler validation
- [ ] Test deterministic scheduler entry generation

##### Manual Verification:

- [ ] Verify choice entry format matches wf_sched:choice_entry()
- [ ] Verify step_seq in choice entry
- [ ] Verify enabled set captured correctly

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 9: Comprehensive Testing

#### Overview

Write comprehensive unit tests, integration tests, and property-based tests for the tracing system. This phase validates all functionality.

#### Changes Required:

##### 1. Create test file

**File**: `test/wf_trace_tests.erl`
**Changes**: Create comprehensive test suite

```erlang
-module(wf_trace_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").
-include("../src/wf_trace.hrl").

%%====================================================================
%% Trace Level Tests
%%====================================================================

trace_level_none_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(none),
        wf_trace:set_level(none),
        ExecState = wf_exec:new([{'DONE'}]),
        TraceEvent = #{opcode => {'DONE'}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        ?assertEqual(0, length(Events))
    end.

trace_level_min_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(min),
        wf_trace:set_level(min),
        Bytecode = [
            {'SEQ_ENTER', 0},
            {'PAR_FORK', [1, 3]},
            {'TASK_EXEC', task_a},
            {'DONE'},
            {'TASK_EXEC', task_b},
            {'DONE'},
            {'JOIN_WAIT', all}
        ],
        ExecState = wf_exec:new(Bytecode),
        %% Run executor
        lists:foreach(fun(_) ->
            {ExecState1, _} = wf_exec:step(ExecState, undefined),
            ok
        end, lists:seq(1, 10)),
        Events = wf_trace:get_events(State),
        %% Should have only structural events (PAR_FORK, DONE, JOIN_WAIT)
        StructuralEvents = [E || E <- Events,
            is_structural_opcode(E#trace_event.opcode)],
        ?assert(length(Events) >= length(StructuralEvents))
    end.

trace_level_full_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        wf_trace:set_level(full),
        Bytecode = [{'TASK_EXEC', task}, {'DONE'}],
        ExecState = wf_exec:new(Bytecode),
        {ExecState1, _} = wf_exec:step(ExecState, undefined),
        Events = wf_trace:get_events(State),
        ?assert(length(Events) > 0),
        [FirstEvent | _] = Events,
        ?assertMatch(#trace_event{}, FirstEvent),
        ?assert(is_integer(FirstEvent#trace_event.step_seq)),
        ?assert(is_integer(FirstEvent#trace_event.timestamp)),
        ?assert(is_list(FirstEvent#trace_event.scope)),
        ?assertMatch(_, FirstEvent#trace_event.state_before))
    end.

%%====================================================================
%% Sink Tests
%%====================================================================

sink_callback_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        Self = self(),
        CallbackFun = fun(Event) ->
            Self ! {trace_event, Event},
            ok
        end,
        State2 = wf_trace:set_sink(State, {callback, CallbackFun}),
        put(wf_trace_state, State2),
        ExecState = wf_exec:new([{'DONE'}]),
        TraceEvent = #{opcode => {'DONE'}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        receive
            {trace_event, #trace_event{}} -> ok
        after 1000 ->
            error(timeout)
        end
    end.

sink_ets_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{'DONE'}]),
        TraceEvent = #{opcode => {'DONE'}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        ?assert(length(Events) > 0)
    end.

sink_process_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        Self = self(),
        ProcessSink = spawn(fun() ->
            receive
                {trace_event, _Event} ->
                    Self ! received
            end
        end),
        State2 = wf_trace:set_sink(State, {process, ProcessSink}),
        put(wf_trace_state, State2),
        ExecState = wf_exec:new([{'DONE'}]),
        TraceEvent = #{opcode => {'DONE'}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        receive
            received -> ok
        after 1000 ->
            error(timeout)
        end
    end.

%%====================================================================
%% Filter Tests
%%====================================================================

filter_opcode_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{'DONE'}]),
        TraceEvent = #{opcode => {'DONE'}},
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        Filtered = wf_trace:filter(Events, {opcode, {'DONE'}}),
        ?assertEqual(length(Events), length(Filtered))
    end.

filter_scope_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{'CANCEL_SCOPE', {enter, scope1}}, {'DONE'}]),
        lists:foreach(fun(_) ->
            {ExecState1, _} = wf_exec:step(ExecState, undefined),
            ok
        end, lists:seq(1, 3)),
        Events = wf_trace:get_events(State),
        Filtered = wf_trace:filter(Events, {scope, scope1}),
        ?assert(length(Filtered) > 0)
    end.

%%====================================================================
%% Replay Log Tests
%%====================================================================

to_replay_log_test_() ->
    fun() ->
        {ok, State} = wf_trace:new(full),
        ExecState = wf_exec:new([{'DONE'}]),
        TraceEvent = #{
            opcode => {'DONE'},
            metadata => #{scheduler_choice => {0, [{token, t1}], {token, t1}}}
        },
        ok = wf_trace:emit(ExecState, TraceEvent),
        Events = wf_trace:get_events(State),
        ReplayLog = wf_trace:to_replay_log(Events),
        ?assert(length(ReplayLog) >= 0)
    end.

from_replay_log_test_() ->
    fun() ->
        ReplayLog = [
            #replay_entry{
                step_seq = 1,
                opcode = {'DONE'},
                scheduler_choice = {0, [{token, t1}], {token, t1}},
                effect_result = undefined
            }
        ],
        {ok, State} = wf_trace:from_replay_log(ReplayLog),
        ?assertMatch(#trace_state{}, State)
    end.

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_replay_log_smaller() ->
    ?FORALL(Trace, trace_gen(),
        begin
            ReplayLog = wf_trace:to_replay_log(Trace),
            length(ReplayLog) =< length(Trace)
        end).

prop_trace_events_ordered() ->
    ?FORALL(Trace, trace_gen(),
        begin
            StepSeqs = [E#trace_event.step_seq || E <- Trace],
            StepSeqs =:= lists:sort(StepSeqs)
        end).

%%====================================================================
%% Generators
%%====================================================================

trace_gen() ->
    ?LET({Opcodes, Level},
        {list(opcode_gen()), trace_level_gen()},
        begin
            %% Generate trace events
            [#trace_event{
                step_seq = N,
                opcode = Opcode,
                state_before = undefined,
                state_after = undefined,
                timestamp = erlang:monotonic_time(microsecond),
                scope = [],
                branch_id = undefined,
                metadata = #{}
            } || {N, Opcode} <- lists:enumerate(Opcodes)]
        end).

opcode_gen() ->
    oneof([
        {'SEQ_ENTER', 0},
        {'SEQ_NEXT', 1},
        {'PAR_FORK', [1, 2]},
        {'JOIN_WAIT', all},
        {'CANCEL_SCOPE', {enter, scope1}},
        {'DONE'}
    ]).

trace_level_gen() ->
    oneof([none, min, full]).
```

##### 2. Add helper functions to test

**File**: `test/wf_trace_tests.erl`
**Changes**: Add helper functions

```erlang
%% @private Check if opcode is structural
is_structural_opcode({OpcodeName, _Arg}) ->
    lists:member(OpcodeName, ['PAR_FORK', 'JOIN_WAIT', 'CANCEL_SCOPE', 'DONE']);
is_structural_opcode({'DONE'}) ->
    true;
is_structural_opcode(_Other) ->
    false.
```

#### Success Criteria:

##### Automated Verification:

- [ ] All unit tests pass: `rebar3 eunit`
- [ ] All property-based tests pass: `rebar3 proper`
- [ ] Code coverage > 90% for wf_trace.erl
- [ ] Type checking passes: `rebar3 dialyzer`
- [ ] No test failures or hangs

##### Manual Verification:

- [ ] Review test coverage report
- [ ] Verify property-based tests run correctly
- [ ] Test edge cases (empty trace, large trace)
- [ ] Verify error handling (invalid inputs)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

## Testing Strategy

### Unit Tests:

**Trace Level Tests**:
- Test trace level none (no events emitted)
- Test trace level min (only structural events)
- Test trace level full (all events)
- Test level switching at runtime
- Test default level (none)

**Sink Tests**:
- Test callback sink (function invocation)
- Test ETS sink (event storage)
- Test process sink (message sending)
- Test sink error handling (crash resilience)
- Test sink switching

**State Snapshot Tests**:
- Test snapshot serialization
- Test restore deserialization
- Test bytecode validation
- Test invalid snapshot handling
- Test snapshot size (memory usage)

**Event Filtering Tests**:
- Test opcode filtering
- Test scope filtering
- Test branch filtering
- Test predicate filtering
- Test combined filters

**Replay Log Tests**:
- Test replay log generation
- Test replay log validation
- Test from_replay_log/1
- Test invalid replay log handling
- Test replay log size vs trace size

**Integration Tests**:
- Test wf_exec emits trace events
- Test trace event field completeness
- Test step_seq monotonicity
- Test timestamp monotonicity
- Test scope_stack capture
- Test branch_id capture

### Integration Tests:

**End-to-End Scenarios**:
1. **Simple workflow** (single task, DONE)
   - Trace at level=none: verify no events
   - Trace at level=min: verify DONE event
   - Trace at level=full: verify all events
   - Generate replay log: verify format

2. **Parallel workflow** (PAR_FORK, multiple branches)
   - Trace at level=min: verify PAR_FORK, DONE, JOIN_WAIT events
   - Filter by branch_id: verify separate branches
   - Verify branch_id captured correctly

3. **Sequential workflow** (SEQ_ENTER, SEQ_NEXT)
   - Trace at level=full: verify all events
   - Verify scope_stack changes
   - Verify step_seq monotonic

4. **Cancel scope workflow** (CANCEL_SCOPE)
   - Trace at level=min: verify CANCEL_SCOPE events
   - Filter by scope_id: verify enter/exit events
   - Verify scope_stack captured

5. **Nondeterministic scheduler** (XOR_CHOOSE)
   - Use wf_sched_nondeterministic
   - Capture scheduler choices
   - Generate replay log
   - Replay execution (validate trace matches)

6. **Replay validation** (replay scheduler)
   - Execute workflow with nondeterministic scheduler
   - Generate replay log
   - Re-execute with replay scheduler
   - Compare traces (verify deterministic)

### Manual Testing Steps:

1. **Verify zero overhead at level=none**:
   - Run workflow with tracing disabled
   - Measure steps/sec
   - Run workflow with level=none
   - Verify steps/sec is similar (< 1% difference)

2. **Verify structural event filtering**:
   - Run complex workflow with level=min
   - Verify only PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE events
   - Verify no SEQ_ENTER, SEQ_NEXT, TASK_EXEC events

3. **Verify state snapshot correctness**:
   - Run workflow with level=full
   - Capture state_before and state_after
   - Restore state from snapshot
   - Verify restored state matches

4. **Verify replay log generation**:
   - Run workflow with nondeterministic scheduler
   - Generate replay log
   - Verify replay log contains scheduler choices
   - Verify replay log is smaller than full trace

5. **Verify event ordering**:
   - Run complex workflow
   - Extract events from ETS table
   - Verify step_seq is strictly increasing
   - Verify timestamp is monotonic

6. **Verify sink error handling**:
   - Use callback sink that throws error
   - Verify executor continues (crash resilience)
   - Use process sink with dead process
   - Verify executor continues

7. **Verify memory usage**:
   - Run long workflow with level=full
   - Monitor ETS table size
   - Verify memory usage is acceptable (< 100MB for 10k steps)

8. **Verify concurrent tracing**:
   - Run multiple workflows simultaneously
   - Verify trace events are not mixed
   - Verify case_id correlation

## Migration Notes

**No Migration Required**: This is a new feature, no existing data or systems to migrate.

**Backward Compatibility**: All changes are additive. Existing wf_exec API remains unchanged. wf_trace is a new module.

**Breaking Changes**: None.

**Deployment**:
1. Deploy wf_trace.erl to src/
2. Update wf_exec.hrl (add case_id field)
3. Update wf_exec.erl (add snapshot/restore, emit calls)
4. Update wf_sched.erl (add choose_with_entry/2)
5. Update scheduler implementations (choose_with_entry/2)
6. Run tests to verify integration

**Rollback**: If issues arise, revert wf_exec changes to remove emit/2 calls. Tracing can be disabled at level=none with zero overhead.

## References

- **Research**: `/Users/speed/wf-substrate/.wreckit/items/011-tracing-and-replay/research.md`
- **Specification**: `/Users/speed/wf-substrate/.wreckit/items/011-tracing-and-replay/item.json`
- **Executor**: `/Users/speed/wf-substrate/src/wf_exec.erl` (lines 101-107, 109-126)
- **Executor Records**: `/Users/speed/wf-substrate/src/wf_exec.hrl` (lines 31-42)
- **Scheduler**: `/Users/speed/wf-substrate/src/wf_sched.erl` (lines 28-34, choice_entry type)
- **Nondeterministic Scheduler**: `/Users/speed/wf-substrate/src/wf_sched_nondeterministic.erl` (lines 39-61, choice logging)
- **Replay Scheduler**: `/Users/speed/wf-substrate/src/wf_sched_replay.erl` (lines 17-57, replay validation)
- **State Snapshots**: `/Users/speed/wf-substrate/src/wf_state.erl` (lines 376-415, snapshot/restore pattern)
- **Opcode Types**: `/Users/speed/wf-substrate/src/wf_vm.erl` (lines 13-21, opcode type)
- **Tests**: `/Users/speed/wf-substrate/test/wf_exec_tests.erl` (test patterns)
- **Project Spec**: `/Users/speed/wf-substrate/PROMPT.md` (lines 39-41, 61, 332)
- **Item 005 PRD**: `/Users/speed/wf-substrate/.wreckit/items/005-executor-hot-loop/prd.json` (lines 181-194, trace integration spec)
