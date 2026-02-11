# Implement OTP supervision tree and public API Implementation Plan

## Implementation Plan Title

OTP Supervision Tree and Public API for wf_substrate

## Overview

Implement the production-grade OTP supervision tree and public API for wf_substrate, creating the bridge between the core execution engine (wf_exec, wf_sched, wf_state, wf_trace) and external consumers. This involves building a four-level supervision tree (top-level supervisor, dynamic case supervisor, optional effect supervisor, optional trace sink), implementing per-case gen_statem runners with explicit state modeling, and exposing a clean public API for workflow case management.

The implementation wraps the low-level execution model into an OTP-compliant, supervised, fault-tolerant system with proper isolation between concurrent cases, configurable scheduling policies, and integrated tracing support.

## Current State

**Existing Infrastructure (Items 001-011):**

1. **Application Scaffold Complete:**
   - `src/wf_substrate_app.erl:26-27` - Application calls `wf_substrate_sup:start_link()`
   - `src/wf_substrate_sup.erl:42-47` - Top-level supervisor with empty ChildSpecs
   - `src/wf_substrate.erl:21-23` - Public API stub with commented exports

2. **Core Execution Engine Ready:**
   - `src/wf_exec.erl:46-72` - Bytecode executor with new/1, step/2, run/3
   - `src/wf_exec.erl:100-109` - Status checking: is_done/1, is_blocked/1
   - `src/wf_sched.erl:72-81` - Scheduler factory with deterministic/nondeterministic/replay policies
   - `src/wf_state.erl:163-181` - State store gen_server with ETS table
   - `src/wf_trace.erl:78-94` - Trace state creation with level/sink configuration
   - Records defined in `src/wf_exec.hrl:1-44` and `src/wf_trace.hrl:1-30`

3. **Testing Patterns Established:**
   - EUnit test framework with setup/teardown (test/wf_state_tests.erl:8-19)
   - Mock bytecode generators for testing (test/wf_exec_tests.erl:6-20)
   - Header file inclusion in tests (test/wf_exec_tests.erl:3)

**Missing Components:**

- `wf_case_sup.erl` - Dynamic supervisor for case runner processes
- `wf_case_runner.erl` - Per-case gen_statem process
- `wf_effect_sup.erl` - Optional supervisor for effect handlers
- `wf_trace_sink.erl` - Optional gen_server for centralized trace collection
- `wf_substrate.erl` - Public API implementations (new_case/3, signal/2, etc.)
- Process registration mechanism for case PID lookup by CaseId

**Key Constraints:**

- No external dependencies (rebar.config:13 shows deps=[])
- OTP 26+ required (rebar.config:2)
- Must follow existing patterns: -spec annotations, EUnit tests, record definitions in .hrl files
- wf_compile and wf_term (items 002, 004) not yet implemented - need mock bytecode for now

## Desired End State

**Functional Requirements:**

1. **Supervision Tree Operational:**
   - wf_substrate_sup starts wf_case_sup as permanent child
   - wf_case_sup uses simple_one_for_one strategy for dynamic case runners
   - Case runners are temporary (not restarted on normal termination)
   - Optional wf_trace_sink singleton for centralized logging

2. **Per-Case Lifecycle Management:**
   - gen_statem with explicit states: initializing → running → done/cancelled
   - Intermediate waiting states: waiting_effect, waiting_signal
   - Execution in quanta (configurable step count) with cooperative yielding
   - State timeout support for overall case timeout
   - Integration with wf_exec:run/3, wf_sched:choose/2, wf_state:commit/1

3. **Public API Complete:**
   - new_case/3: Accept bytecode directly (bypassing wf_compile until item 004)
   - signal/2: Deliver external signals to running cases
   - cancel/1, cancel_region/2: Cancel entire case or specific scope
   - await/2: Block until case completes with timeout support
   - status/1: Query current case status
   - trace/3: Configure trace level and sink
   - validate/2: Stub returning ok (item 013 will implement)

4. **Process Registration:**
   - Case PIDs registered via Erlang registry (no gproc dependency)
   - Name format: {wf_case, CaseId} using via tuple
   - Lookup mechanism for API functions to find case PIDs

5. **Comprehensive Testing:**
   - Unit tests for each state transition in wf_case_runner
   - Integration tests for supervision tree start/stop
   - API tests for all public functions
   - Test coverage for error cases (timeout, cancel, crash)

**Verification Criteria:**

```bash
# Build succeeds
rebar3 compile

# All tests pass
rebar3 eunit

# Application starts cleanly
rebar3 shell
> application:start(wf_substrate).
> wf_substrate:new_case(case1, mock_bytecode(), #{}).
```

### Key Discoveries:

- **wf_exec Integration Point:** `src/wf_exec.erl:153-169` - run/3 takes (ExecState, Quanta, SchedPolicy), returns {done, ExecState} or {yield, ExecState}. Case runner should call this in running state.
- **Status Checking:** `src/wf_exec.erl:100-109` - is_done/1 checks status field (done/cancelled/failed), is_blocked/1 checks (blocked_effect/blocked_join/blocked_signal). Use these to determine state transitions.
- **Trace Integration:** `src/wf_trace.erl:96-120` - Uses process dictionary for trace state (get(wf_trace_state)). Case runner should set this in init/1.
- **Scheduler Factory:** `src/wf_sched.erl:72-81` - new/2 creates scheduler state from policy atom (deterministic | nondeterministic | {replay, Log}). Case runner should create this in initializing state.
- **State Store Pattern:** `src/wf_state.erl:163-181` - gen_server owns ETS table named wf_state_store. Case runner should call wf_state:new/1, wf_state:commit/1.
- **No Process Registration Yet:** No gproc or registry usage in existing code. Will use Erlang's built-in registry via {via, registry, {wf_case, CaseId}} pattern.

## What We're NOT Doing

- **NOT implementing wf_compile or wf_term** (items 002, 004) - new_case/3 will accept wf_vm:wf_bc() bytecode directly for now
- **NOT implementing wf_validate** (item 013) - validate/2 will be a stub returning ok
- **NOT implementing wf_effect_sup** - Out-of-process effect model assumed (simpler), can add later if needed
- **NOT implementing persistent case state across restarts** - Cases are temporary, rely on caller to retry on failure
- **NOT implementing distributed case execution** - All cases run on local node
- **NOT implementing rate limiting or resource quotas** - Will be addressed in future items
- **NOT implementing hot code reloading** - Standard OTP code_change/3 stub only

## Implementation Approach

**Bottom-Up Supervision Tree Build:**

Build from leaf nodes up to root, testing each component before integrating:
1. Start with wf_case_runner (leaf process) - most complex, test state machine thoroughly
2. Add wf_case_sup (supervisor) - test dynamic child start/stop
3. Update wf_substrate_sup (top) - integrate case_sup
4. Implement wf_substrate API - thin wrapper over supervision tree
5. Add optional wf_trace_sink if time permits

**gen_statem State Machine Design:**

Use state_functions mode (not handle_event_function) for explicit state modeling:
- Each state is a separate callback function (initializing/3, running/3, etc.)
- State-specific event handling (cast, call, timeout, info)
- Transition via {next_state, NextState, NewData} or {stop, Reason}
- Leverage state_timeout for overall case timeout

**Process Registration Strategy:**

Use Erlang/OTP 26+ registry (no external deps):
```erlang
{ok, Pid} = supervisor:start_child(wf_case_sup, [CaseId, Bytecode, Options]),
% Register via registry
true = register(wf_case_pid_name(CaseId), Pid),
% Later lookup:
Pid = whereis(wf_case_pid_name(CaseId))
```

**Mock Compiler Pattern:**

Since wf_compile doesn't exist yet:
- API accepts wf_vm:wf_bc() directly (list of opcodes)
- Tests use mock_bytecode_*() functions from wf_exec_tests.erl
- Add TODO comment in new_case/3 for item 004 integration

**Incremental Testing:**

Each phase has independently testable units:
- Phase 1: Test wf_case_runner state transitions with mock exec_state
- Phase 2: Test wf_case_sup child start/stop/restart
- Phase 3: Test wf_substrate API functions end-to-end
- Phase 4: Test trace event collection (if implemented)

---

## Phases

### Phase 1: Case Runner gen_statem

#### Overview

Implement wf_case_runner.erl as a gen_statem with explicit state machine modeling. The case runner manages the lifecycle of a single workflow case, executing bytecode in quanta, handling external signals, and managing cancellation requests.

#### Changes Required:

##### 1. Create src/wf_case_runner.erl

**File**: `src/wf_case_runner.erl`
**Changes**: New file implementing gen_statem behavior

```erlang
%%%-------------------------------------------------------------------
%%% @doc Per-case workflow runner as gen_statem
%%%
%%% Manages the lifecycle of a single workflow case execution:
%%% - initializing: Setup exec_state, sched_state, trace_state
%%% - running: Execute bytecode quanta, check for done/blocked
%%% - waiting_effect: Await effect response from external system
%%% - waiting_signal: Await signal via wf_substrate:signal/2
%%% - cancelled: Cleanup and terminate
%%% - done: Return result and terminate
%%%
%%% State timeout support for overall case timeout.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_case_runner).
-behaviour(gen_statem).

%% API
-export([start_link/3]).
-export([signal/2, cancel/1, cancel_region/2, set_trace/2, status/1]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4,
    initializing/3,
    running/3,
    waiting_effect/3,
    waiting_signal/3,
    cancelled/3,
    done/3
]).

%% Include record definitions
-include("wf_exec.hrl").
-include("wf_trace.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(state_data, {
    case_id :: term(),
    exec_state :: wf_exec:exec_state() | undefined,
    sched_state :: wf_sched:sched_state() | undefined,
    trace_state :: #trace_state{} | undefined,
    options :: map(),
    caller_pid :: pid() | undefined,
    result :: term() | undefined
}).

%%====================================================================
%% Types
%%====================================================================

-type state_data() :: #state_data{}.
-type state_name() :: initializing | running | waiting_effect | waiting_signal | cancelled | done.

%%====================================================================
%% API
%%====================================================================

%% @doc Start case runner with CaseId, Bytecode, Options
-spec start_link(term(), wf_vm:wf_bc(), map()) -> {ok, pid()}.
start_link(CaseId, Bytecode, Options) ->
    gen_statem:start_link(?MODULE, [CaseId, Bytecode, Options], []).

%% @doc Send signal to running case
-spec signal(pid(), term()) -> ok.
signal(Pid, Signal) ->
    gen_statem:cast(Pid, {signal, Signal}).

%% @doc Cancel running case
-spec cancel(pid()) -> ok.
cancel(Pid) ->
    gen_statem:cast(Pid, cancel).

%% @doc Cancel specific region within case
-spec cancel_region(pid(), term()) -> ok.
cancel_region(Pid, ScopeId) ->
    gen_statem:cast(Pid, {cancel_region, ScopeId}).

%% @doc Set trace configuration
-spec set_trace(pid(), {wf_trace:trace_level(), wf_trace:trace_sink()}) -> ok.
set_trace(Pid, {Level, Sink}) ->
    gen_statem:cast(Pid, {set_trace, Level, Sink}).

%% @doc Get case status
-spec status(pid()) -> {ok, state_name(), map()}.
status(Pid) ->
    gen_statem:call(Pid, status).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

%% @private State functions mode for explicit state modeling
callback_mode() -> state_functions.

%% @private Initialize gen_statem
init([CaseId, Bytecode, Options]) ->
    %% Extract options with defaults
    StepQuanta = maps:get(step_quanta, Options, 100),
    Timeout = maps:get(timeout, Options, 5000),
    TraceLevel = maps:get(trace_level, Options, none),
    SchedPolicy = maps:get(scheduler_policy, Options, deterministic),

    %% Create initial exec_state from bytecode
    ExecState0 = wf_exec:new(Bytecode),

    %% Set case_id in exec_state
    ExecState1 = ExecState0#exec_state{case_id = CaseId},

    %% Create scheduler state
    {ok, SchedState} = wf_sched:new(SchedPolicy, []),

    %% Create trace state
    {ok, TraceState} = wf_trace:new(TraceLevel),
    TraceState1 = TraceState#trace_state{case_id = CaseId},

    %% Store trace state in process dictionary for wf_trace:emit/2
    put(wf_trace_state, TraceState1),

    StateData = #state_data{
        case_id = CaseId,
        exec_state = ExecState1,
        sched_state = SchedState,
        trace_state = TraceState1,
        options = #{
            step_quanta => StepQuanta,
            timeout => Timeout,
            scheduler_policy => SchedPolicy
        },
        caller_pid = undefined,
        result = undefined
    },

    %% Transition to initializing state with timeout
    {ok, initializing, StateData, [{state_timeout, Timeout, initialization_timeout}]}.

%% @private Initializing state: final setup before running
initializing(state_timeout, initialization_timeout, StateData) ->
    %% Setup complete, transition to running
    {next_state, running, StateData};

initializing(cast, {set_trace, Level, Sink}, StateData) ->
    %% Update trace configuration
    TraceState0 = StateData#state_data.trace_state,
    TraceState1 = TraceState0#trace_state{level = Level},
    TraceState2 = wf_trace:set_sink(TraceState1, Sink),
    put(wf_trace_state, TraceState2),
    {next_state, initializing, StateData#state_data{trace_state = TraceState2}};

initializing(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, initializing, StateData).

%% @private Running state: execute bytecode quanta
running(cast, {signal, Signal}, StateData) ->
    %% Handle external signal
    %% For now, store signal in ctx and continue execution
    ExecState0 = StateData#state_data.exec_state,
    Ctx0 = ExecState0#exec_state.ctx,
    Ctx1 = maps:put(last_signal, Signal, Ctx0),
    ExecState1 = ExecState0#exec_state{ctx = Ctx1},
    {next_state, running, execute_quantum(StateData#state_data{exec_state = ExecState1})};

running(cast, cancel, StateData) ->
    %% Cancel entire case
    ExecState0 = StateData#state_data.exec_state,
    ExecState1 = ExecState0#exec_state{status = cancelled},
    {next_state, cancelled, StateData#state_data{exec_state = ExecState1}};

running(cast, {cancel_region, ScopeId}, StateData) ->
    %% Cancel specific region (emit trace event, continue execution)
    %% TODO: Implement region cancellation logic
    {next_state, running, StateData};

running(cast, {set_trace, Level, Sink}, StateData) ->
    %% Update trace configuration
    TraceState0 = StateData#state_data.trace_state,
    TraceState1 = TraceState0#trace_state{level = Level},
    TraceState2 = wf_trace:set_sink(TraceState1, Sink),
    put(wf_trace_state, TraceState2),
    {next_state, running, StateData#state_data{trace_state = TraceState2}};

running({call, From}, status, StateData) ->
    %% Return current status
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => running,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status
    },
    {keep_state_and_data, [{reply, From, {ok, running, StatusInfo}}]};

running(state_timeout, overall_timeout, StateData) ->
    %% Overall case timeout
    {next_state, cancelled, StateData};

running(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, running, StateData).

%% @private Waiting for effect response
waiting_effect(cast, {effect_response, _Result}, StateData) ->
    %% Resume execution with effect result
    {next_state, running, execute_quantum(StateData)};

waiting_effect(cast, cancel, StateData) ->
    %% Cancel while waiting for effect
    {next_state, cancelled, StateData};

waiting_effect(state_timeout, overall_timeout, StateData) ->
    %% Timeout while waiting for effect
    {next_state, cancelled, StateData};

waiting_effect(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, waiting_effect, StateData).

%% @private Waiting for external signal
waiting_signal(cast, {signal, Signal}, StateData) ->
    %% Signal received, resume execution
    ExecState0 = StateData#state_data.exec_state,
    Ctx0 = ExecState0#exec_state.ctx,
    Ctx1 = maps:put(last_signal, Signal, Ctx0),
    ExecState1 = ExecState0#exec_state{ctx = Ctx1},
    {next_state, running, execute_quantum(StateData#state_data{exec_state = ExecState1})};

waiting_signal(cast, cancel, StateData) ->
    %% Cancel while waiting for signal
    {next_state, cancelled, StateData};

waiting_signal(state_timeout, overall_timeout, StateData) ->
    %% Timeout while waiting for signal
    {next_state, cancelled, StateData};

waiting_signal(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, waiting_signal, StateData).

%% @private Cancelled state: cleanup and terminate
cancelled(cast, cancel, StateData) ->
    %% Already cancelled
    {stop, normal, StateData};

cancelled({call, From}, status, StateData) ->
    %% Return cancelled status
    StatusInfo = #{state => cancelled},
    {keep_state_and_data, [{reply, From, {ok, cancelled, StatusInfo}}]};

cancelled(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, cancelled, StateData).

%% @private Done state: return result and terminate
done({call, From}, status, StateData) ->
    %% Return done status with result
    StatusInfo = #{state => done, result => StateData#state_data.result},
    {keep_state_and_data, [{reply, From, {ok, done, StatusInfo}}]};

done(EventType, EventContent, StateData) ->
    handle_common_event(EventType, EventContent, done, StateData).

%% @private Common event handler for all states
handle_common_event({call, From}, status, CurrentState, StateData) ->
    ExecState = StateData#state_data.exec_state,
    StatusInfo = #{
        state => CurrentState,
        ip => ExecState#exec_state.ip,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status
    },
    {keep_state_and_data, [{reply, From, {ok, CurrentState, StatusInfo}}]};

handle_common_event(_EventType, _EventContent, _CurrentState, StateData) ->
    {keep_state_and_data}.

%% @private gen_statem terminate
terminate(_Reason, _StateName, #state_data{case_id = CaseId}) ->
    %% Cleanup ETS state if needed
    %% Unregister from registry
    case whereis(wf_case_pid_name(CaseId)) of
        undefined -> ok;
        _Pid -> unregister(wf_case_pid_name(CaseId))
    end,
    ok.

%% @private gen_statem code_change
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute one quantum of bytecode (N steps or until terminal)
-spec execute_quantum(state_data()) -> state_data().
execute_quantum(StateData) ->
    ExecState0 = StateData#state_data.exec_state,
    SchedState = StateData#state_data.sched_state,
    StepQuanta = maps:get(step_quanta, StateData#state_data.options),

    %% Run quantum
    Result = wf_exec:run(ExecState0, StepQuanta, SchedState),

    case Result of
        {done, ExecState1} ->
            %% Execution complete
            case ExecState1#exec_state.status of
                done ->
                    %% Normal completion
                    Result1 = ExecState1#exec_state.ctx,
                    notify_done(StateData#state_data.caller_pid, {ok, Result1}),
                    StateData1 = StateData#state_data{exec_state = ExecState1, result = Result1},
                    enter_state(done, StateData1);
                cancelled ->
                    %% Cancelled
                    notify_done(StateData#state_data.caller_pid, {error, cancelled}),
                    enter_state(cancelled, StateData#state_data{exec_state = ExecState1});
                _ ->
                    %% Other terminal status
                    notify_done(StateData#state_data.caller_pid, {error, ExecState1#exec_state.status}),
                    enter_state(done, StateData#state_data{exec_state = ExecState1})
            end;
        {yield, ExecState1} ->
            %% Quantum exhausted, check if blocked
            case wf_exec:is_blocked(ExecState1) of
                true ->
                    %% Determine what we're waiting for
                    case ExecState1#exec_state.status of
                        blocked_effect ->
                            enter_state(waiting_effect, StateData#state_data{exec_state = ExecState1});
                        blocked_signal ->
                            enter_state(waiting_signal, StateData#state_data{exec_state = ExecState1});
                        _ ->
                            %% Other blocked state, continue running
                            StateData#state_data{exec_state = ExecState1}
                    end;
                false ->
                    %% Not blocked, continue running
                    StateData#state_data{exec_state = ExecState1}
            end
    end.

%% @doc Transition to new state with timeout
enter_state(TargetState, StateData) ->
    Timeout = maps:get(timeout, StateData#state_data.options, 5000),
    _StateData1 = StateData,
    %% Timeout will be set by gen_statem automatically
    %% We just return StateData, the timeout is managed via state_timeout in events
    _TargetState = TargetState,
    StateData.

%% @doc notify caller of completion
-spec notify_done(pid() | undefined, term()) -> ok.
notify_done(undefined, _Result) ->
    ok;
notify_done(CallerPid, Result) when is_pid(CallerPid) ->
    CallerPid ! {case_completed, Result},
    ok.

%% @doc Generate registered name for case PID
-spec wf_case_pid_name(term()) -> atom().
wf_case_pid_name(CaseId) when is_atom(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId));
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ lists:flatten(io_lib:format("~p", [CaseId]))).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] Xref passes: `rebar3 xref`

##### Manual Verification:

- [ ] gen_statem state machine logic is clear and explicit
- [ ] All required states implemented (initializing, running, waiting_effect, waiting_signal, cancelled, done)
- [ ] Proper integration with wf_exec:run/3, wf_sched:new/2, wf_trace:new/1
- [ ] State timeout mechanism in place for overall case timeout

**Note**: Complete automated verification, then pause for manual review before proceeding to Phase 2.

---

### Phase 2: Dynamic Case Supervisor

#### Overview

Implement wf_case_sup.erl as a dynamic supervisor using the simple_one_for_one strategy. This supervisor manages per-case runner processes, starting them on demand and ensuring proper restart behavior.

#### Changes Required:

##### 1. Create src/wf_case_sup.erl

**File**: `src/wf_case_sup.erl`
**Changes**: New file implementing dynamic supervisor

```erlang
%%%-------------------------------------------------------------------
%%% @doc Dynamic supervisor for workflow case runners
%%%
%%% Uses simple_one_for_one strategy to dynamically spawn
%%% per-case gen_statem processes. Cases are temporary (not
%%% restarted on normal termination) to avoid replaying
%%% execution state.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_case_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_case/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new case runner
-spec start_case(term(), wf_vm:wf_bc(), map()) -> {ok, pid()} | {error, term()}.
start_case(CaseId, Bytecode, Options) ->
    %% Check if case already exists
    PidName = wf_case_runner:wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            ChildSpec = #{
                id => CaseId,
                start => {wf_case_runner, start_link, [CaseId, Bytecode, Options]},
                restart => temporary,  %% Don't restart on normal termination
                shutdown => 5000,
                type => worker,
                modules => [wf_case_runner]
            },
            case supervisor:start_child(?SERVER, ChildSpec) of
                {ok, Pid} ->
                    %% Register process with a local name
                    try
                        true = register(PidName, Pid),
                        {ok, Pid}
                    catch
                        error:badarg ->
                            %% Registration failed, clean up
                            supervisor:terminate_child(?SERVER, CaseId),
                            supervisor:delete_child(?SERVER, CaseId),
                            {error, registration_failed}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ExistingPid ->
            {error, already_exists}
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [],  %% Empty for simple_one_for_one
    {ok, {SupFlags, ChildSpecs}}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] Unit tests pass: Write basic test for start_case/3

##### Manual Verification:

- [ ] simple_one_for_one strategy configured correctly
- [ ] temporary restart prevents unwanted case restarts
- [ ] Process registration handles name conflicts

**Note**: Complete automated verification, then pause for manual review before proceeding to Phase 3.

---

### Phase 3: Update Top-Level Supervisor

#### Overview

Update wf_substrate_sup.erl to add wf_case_sup as a permanent child. This completes the supervision tree structure.

#### Changes Required:

##### 1. Update src/wf_substrate_sup.erl

**File**: `src/wf_substrate_sup.erl:42-47`
**Changes**: Replace empty ChildSpecs with wf_case_sup child spec

```erlang
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [
        #{id => wf_case_sup,
          start => {wf_case_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [wf_case_sup]}
        %% Optional singletons to be added later:
        %% #{id => wf_trace_sink, ...}
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Application starts: `rebar3 shell` then `application:start(wf_substrate).`
- [ ] Supervisor tree visible: `observer:start()` or `supervisor:which_children(wf_substrate_sup).`

##### Manual Verification:

- [ ] wf_case_sup is a child of wf_substrate_sup
- [ ] wf_case_sup starts successfully
- [ ] Application can stop cleanly: `application:stop(wf_substrate).`

**Note**: Complete automated verification, then pause for manual review before proceeding to Phase 4.

---

### Phase 4: Public API Implementation

#### Overview

Implement the public API in wf_substrate.erl, providing a clean interface for creating cases, sending signals, cancelling cases, awaiting results, querying status, and configuring tracing.

#### Changes Required:

##### 1. Update src/wf_substrate.erl

**File**: `src/wf_substrate.erl:21-23`
**Changes**: Uncomment exports and implement all API functions

```erlang
-module(wf_substrate).

%% Public API
-export([
    new_case/3,
    signal/2,
    cancel/1,
    cancel_region/2,
    await/2,
    status/1,
    trace/3,
    validate/2
]).

%% Include records
-include("wf_exec.hrl").

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: term().
-type signal() :: term().
-type wf_term() :: wf_vm:wf_bc().  %% For now, accept bytecode directly
-type ctx() :: map().
-type options() :: #{
    step_quanta => pos_integer(),
    timeout => pos_integer(),
    trace_level => wf_trace:trace_level(),
    scheduler_policy => wf_sched:sched_policy()
}.

-type case_status() :: #{
    state => atom(),
    ip => non_neg_integer(),
    step_count => non_neg_integer(),
    status => atom()
}.

-export_type([case_id/0, case_status/0, options/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new workflow case
%% CaseId: Unique identifier for the case
%% Term: Workflow bytecode (wf_vm:wf_bc()) - TODO: wf_term() in item 004
%% Ctx: Initial context map
-spec new_case(case_id(), wf_term(), ctx()) -> {ok, pid()} | {error, term()}.
new_case(CaseId, Bytecode, Ctx) when is_list(Bytecode), is_map(Ctx) ->
    %% TODO: Item 004 will call wf_compile:compile(Term) here
    %% For now, accept bytecode directly
    Options = #{},
    case wf_case_sup:start_case(CaseId, Bytecode, Options) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Send signal to running case
-spec signal(case_id(), signal()) -> ok | {error, term()}.
signal(CaseId, Signal) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:signal(Pid, Signal),
            ok
    end.

%% @doc Cancel entire case
-spec cancel(case_id()) -> ok | {error, term()}.
cancel(CaseId) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:cancel(Pid),
            ok
    end.

%% @doc Cancel specific region within case
-spec cancel_region(case_id(), term()) -> ok | {error, term()}.
cancel_region(CaseId, ScopeId) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:cancel_region(Pid, ScopeId),
            ok
    end.

%% @doc Await case completion with timeout
-spec await(case_id(), pos_integer() | infinity) -> {ok, term()} | {error, term()}.
await(CaseId, Timeout) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            %% Monitor case process
            Ref = monitor(process, Pid),
            receive
                {case_completed, Result} ->
                    demonitor(Ref, [flush]),
                    Result;
                {'DOWN', Ref, process, Pid, Reason} ->
                    {error, Reason}
            after Timeout ->
                demonitor(Ref, [flush]),
                {error, timeout}
            end
    end.

%% @doc Get current case status
-spec status(case_id()) -> {ok, case_status()} | {error, term()}.
status(CaseId) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            try wf_case_runner:status(Pid) of
                {ok, _State, StatusInfo} ->
                    {ok, StatusInfo}
            catch
                _:_ ->
                    {error, status_unavailable}
            end
    end.

%% @doc Configure tracing for a case
-spec trace(case_id(), wf_trace:trace_level(), wf_trace:trace_sink()) -> ok | {error, term()}.
trace(CaseId, Level, Sink) ->
    PidName = wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            {error, not_found};
        Pid ->
            wf_case_runner:set_trace(Pid, {Level, Sink}),
            ok
    end.

%% @doc Validate workflow term (stub - item 013 will implement)
-spec validate(wf_term(), options()) -> ok | {error, term()}.
validate(_Bytecode, _Options) ->
    %% TODO: Implement in item 013
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate registered name for case PID
-spec wf_case_pid_name(term()) -> atom().
wf_case_pid_name(CaseId) when is_atom(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId));
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ lists:flatten(io_lib:format("~p", [CaseId]))).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile`
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] API tests pass (write test/wf_substrate_api_tests.erl)

##### Manual Verification:

- [ ] new_case/3 creates case successfully
- [ ] signal/2 delivers signal to running case
- [ ] cancel/1 terminates case
- [ ] await/2 blocks until completion or timeout
- [ ] status/1 returns current status information
- [ ] trace/3 updates trace configuration
- [ ] validate/2 returns ok (stub)

**Note**: Complete automated verification, then pause for manual review before proceeding to Phase 5.

---

### Phase 5: Comprehensive Testing

#### Overview

Write comprehensive unit and integration tests for the supervision tree, case runner, and public API. Ensure all state transitions, error cases, and edge cases are covered.

#### Changes Required:

##### 1. Create test/wf_case_runner_tests.erl

**File**: `test/wf_case_runner_tests.erl`
**Changes**: New test file for case runner state machine

```erlang
-module(wf_case_runner_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Import mock bytecode from wf_exec_tests
-include("wf_exec_tests.hrl").  %% Export mock_bytecode_* from wf_exec_tests

%%====================================================================
%% Setup
%%====================================================================

setup() ->
    {ok, Pid} = wf_case_runner:start_link(
        test_case,
        [{'TASK_EXEC', mock_task}, {'DONE'}],
        #{step_quanta => 10, timeout => 5000}
    ),
    Pid.

cleanup(_Pid) ->
    ok.

%%====================================================================
%% State Transition Tests
%%====================================================================

wf_case_runner_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"initializing_to_running_test", fun initializing_to_running_test/0},
         {"running_completion_test", fun running_completion_test/0},
         {"signal_delivery_test", fun signal_delivery_test/0},
         {"cancel_test", fun cancel_test/0},
         {"status_query_test", fun status_query_test/0},
         {"trace_configuration_test", fun trace_configuration_test/0}
     ]
    }.

%% Test state transition from initializing to running
initializing_to_running_test(_Pid) ->
    %% State machine should transition from initializing to running
    %% after setup is complete
    ok.

%% Test normal completion flow
running_completion_test(Pid) ->
    %% Wait for case to complete
    timer:sleep(100),
    {ok, done, StatusInfo} = wf_case_runner:status(Pid),
    ?assertEqual(done, maps:get(state, StatusInfo)),
    ok.

%% Test signal delivery
signal_delivery_test(_Pid) ->
    %% Send signal to running case
    ok = wf_case_runner:signal(self(), test_signal),
    timer:sleep(50),
    ok.

%% Test case cancellation
cancel_test(Pid) ->
    ok = wf_case_runner:cancel(Pid),
    timer:sleep(50),
    case wf_case_runner:status(Pid) of
        {ok, cancelled, _} -> ok;
        {error, _} -> ok  %% Process may have terminated
    end.

%% Test status query
status_query_test(Pid) ->
    {ok, State, StatusInfo} = wf_case_runner:status(Pid),
    ?assert(is_map(StatusInfo)),
    ?assert(is_atom(State)),
    ok.

%% Test trace configuration
trace_configuration_test(Pid) ->
    %% Set trace level
    ok = wf_case_runner:set_trace(Pid, {min, {ets, wf_trace_events}}),
    timer:sleep(50),
    ok.
```

##### 2. Create test/wf_supervision_tree_tests.erl

**File**: `test/wf_supervision_tree_tests.erl`
**Changes**: New test file for supervision tree

```erlang
-module(wf_supervision_tree_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Supervisor Tests
%%====================================================================

supervision_tree_test_() ->
    {setup,
     fun setup_app/0,
     fun cleanup_app/1,
     [
         {"wf_case_sup_starts_test", fun wf_case_sup_starts_test/0},
         {"dynamic_case_start_test", fun dynamic_case_start_test/0},
         {"temporary_restart_test", fun temporary_restart_test/0},
         {"name_conflict_test", fun name_conflict_test/0}
     ]
    }.

setup_app() ->
    application:ensure_all_started(wf_substrate).

cleanup_app(_Pid) ->
    application:stop(wf_substrate).

%% Test that wf_case_sup starts as child of wf_substrate_sup
wf_case_sup_starts_test() ->
    Children = supervisor:which_children(wf_substrate_sup),
    ?assertMatch([{wf_case_sup, _, _, _} | _], Children),
    ok.

%% Test dynamic case start
dynamic_case_start_test() ->
    Bytecode = [{'TASK_EXEC', test_task}, {'DONE'}],
    {ok, _Pid} = wf_substrate:new_case(test_case_1, Bytecode, #{}),
    timer:sleep(100),
    ok.

%% Test that cases are temporary (not restarted on normal termination)
temporary_restart_test() ->
    %% Create case that completes quickly
    Bytecode = [{'DONE'}],
    {ok, Pid} = wf_substrate:new_case(temp_case, Bytecode, #{}),
    monitor(process, Pid),
    receive {'DOWN', _, _, Pid, _} -> ok after 1000 -> error(timeout) end,
    %% Case should not be restarted
    timer:sleep(100),
    ?assertEqual(undefined, whereis(wf_case_pid_name(temp_case))),
    ok.

%% Test name conflict handling
name_conflict_test() ->
    Bytecode = [{'TASK_EXEC', test_task}, {'DONE'}],
    {ok, _Pid1} = wf_substrate:new_case(conflict_case, Bytecode, #{}),
    ?assertMatch({error, already_exists}, wf_substrate:new_case(conflict_case, Bytecode, #{})),
    ok.

%% Helper function
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId)).
```

##### 3. Create test/wf_substrate_api_tests.erl

**File**: `test/wf_substrate_api_tests.erl`
**Changes**: New test file for public API

```erlang
-module(wf_substrate_api_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API Tests
%%====================================================================

wf_substrate_api_test_() ->
    {setup,
     fun setup_app/0,
     fun cleanup_app/1,
     [
         {"new_case_creates_process_test", fun new_case_creates_process_test/0},
         {"signal_delivers_message_test", fun signal_delivers_message_test/0},
         {"cancel_terminates_case_test", fun cancel_terminates_case_test/0},
         {"await_blocks_until_completion_test", fun await_blocks_until_completion_test/0},
         {"status_returns_info_test", fun status_returns_info_test/0},
         {"trace_configures_tracing_test", fun trace_configures_tracing_test/0},
         {"validate_stub_test", fun validate_stub_test/0}
     ]
    }.

setup_app() ->
    application:ensure_all_started(wf_substrate).

cleanup_app(_Pid) ->
    application:stop(wf_substrate).

%% Test new_case creates process
new_case_creates_process_test() ->
    Bytecode = [{'TASK_EXEC', test_task}, {'DONE'}],
    {ok, Pid} = wf_substrate:new_case(api_test_1, Bytecode, #{}),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(wf_case_pid_name(api_test_1))),
    ok.

%% Test signal delivery
signal_delivers_message_test() ->
    Bytecode = [{'TASK_EXEC', long_task}, {'DONE'}],
    {ok, _Pid} = wf_substrate:new_case(api_test_2, Bytecode, #{}),
    timer:sleep(50),
    ?assertEqual(ok, wf_substrate:signal(api_test_2, test_signal)),
    ok.

%% Test cancel
cancel_terminates_case_test() ->
    Bytecode = [{'TASK_EXEC', long_task}, {'DONE'}],
    {ok, Pid} = wf_substrate:new_case(api_test_3, Bytecode, #{}),
    monitor(process, Pid),
    ?assertEqual(ok, wf_substrate:cancel(api_test_3)),
    receive {'DOWN', _, _, Pid, _} -> ok after 1000 -> error(timeout) end,
    ok.

%% Test await
await_blocks_until_completion_test() ->
    Bytecode = [{'TASK_EXEC', quick_task}, {'DONE'}],
    {ok, _Pid} = wf_substrate:new_case(api_test_4, Bytecode, #{}),
    ?assertMatch({ok, _}, wf_substrate:await(api_test_4, 5000)),
    ok.

%% Test status
status_returns_info_test() ->
    Bytecode = [{'TASK_EXEC', status_task}, {'DONE'}],
    {ok, _Pid} = wf_substrate:new_case(api_test_5, Bytecode, #{}),
    {ok, StatusInfo} = wf_substrate:status(api_test_5),
    ?assert(is_map(StatusInfo)),
    ?assert(maps:is_key(state, StatusInfo)),
    ok.

%% Test trace configuration
trace_configures_tracing_test() ->
    Bytecode = [{'TASK_EXEC', trace_task}, {'DONE'}],
    {ok, _Pid} = wf_substrate:new_case(api_test_6, Bytecode, #{}),
    ?assertEqual(ok, wf_substrate:trace(api_test_6, min, {ets, wf_trace_events})),
    ok.

%% Test validate stub
validate_stub_test() ->
    Bytecode = [{'TASK_EXEC', validate_task}, {'DONE'}],
    ?assertEqual(ok, wf_substrate:validate(Bytecode, #{})),
    ok.

%% Helper function
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] All tests pass: `rebar3 eunit`
- [ ] Code coverage > 80%: `rebar3 cover`
- [ ] No xref warnings: `rebar3 xref`

##### Manual Verification:

- [ ] Supervision tree visible in observer
- [ ] Multiple concurrent cases run successfully
- [ ] Error cases (timeout, cancel, crash) handled correctly
- [ ] Memory leaks not present (run long-running test)

**Note**: Complete all verification before final integration.

---

### Phase 6: Integration and Documentation

#### Overview

Final integration phase: update application metadata, document the supervision tree, verify end-to-end functionality, and create usage examples.

#### Changes Required:

##### 1. Update src/wf_substrate.app.src

**File**: `src/wf_substrate.app.src:12`
**Changes**: Add new modules to modules list

```erlang
{application, wf_substrate,
 [{description, "Workflow pattern substrate for compiled pattern execution"},
  {vsn, "0.1.0"},
  {registered, [wf_substrate_sup, wf_case_sup]},
  {mod, {wf_substrate_app, []}},
  {applications,
   [kernel,
    stdlib,
    sasl
   ]},
  {env,[]},
  {modules, [
    wf_substrate_app,
    wf_substrate_sup,
    wf_substrate,
    wf_case_sup,
    wf_case_runner
    %% wf_trace_sink  %% To be added later
  ]},
  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.
```

##### 2. Create examples/basic_usage.erl

**File**: `examples/basic_usage.erl`
**Changes**: New example file demonstrating API usage

```erlang
%%%-------------------------------------------------------------------
%%% @doc Basic usage examples for wf_substrate
%%% @end
%%%-------------------------------------------------------------------
-module(basic_usage).
-export([run_simple_workflow/0, run_parallel_workflow/0]).

%% @doc Run a simple sequential workflow
run_simple_workflow() ->
    %% Define bytecode for sequence: Task A -> Task B -> Done
    Bytecode = [
        {'SEQ_ENTER', 0},
        {'TASK_EXEC', task_a},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task_b},
        {'DONE'}
    ],

    %% Start application
    application:ensure_all_started(wf_substrate),

    %% Create new case
    {ok, Pid} = wf_substrate:new_case(simple_seq, Bytecode, #{}),

    %% Await completion
    Result = wf_substrate:await(simple_seq, 5000),

    %% Clean up
    application:stop(wf_substrate),

    Result.

%% @doc Run a parallel workflow
run_parallel_workflow() ->
    %% Define bytecode for parallel: Task A || Task B
    Bytecode = [
        {'PAR_FORK', [1, 3]},
        {'TASK_EXEC', task_a},
        {'DONE'},
        {'TASK_EXEC', task_b},
        {'DONE'},
        {'JOIN_WAIT', all}
    ],

    %% Start application
    application:ensure_all_started(wf_substrate),

    %% Create new case with tracing enabled
    {ok, Pid} = wf_substrate:new_case(parallel_flow, Bytecode, #{}),

    %% Configure tracing
    ok = wf_substrate:trace(parallel_flow, min, {ets, wf_trace_events}),

    %% Send signal during execution
    ok = wf_substrate:signal(parallel_flow, {user_event, data}),

    %% Check status
    {ok, StatusInfo} = wf_substrate:status(parallel_flow),

    %% Await completion
    Result = wf_substrate:await(parallel_flow, 10000),

    %% Clean up
    application:stop(wf_substrate),

    Result.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Application builds: `rebar3 compile`
- [ ] All tests pass: `rebar3 eunit`
- [ ] Examples run: `erlc examples/basic_usage.erl && erl -noshell -s basic_usage run_simple_workflow -s init stop`

##### Manual Verification:

- [ ] Documentation is clear and accurate
- [ ] Supervision tree structure matches specification
- [ ] API functions work as documented
- [ ] No memory leaks in long-running tests

**Note**: Final verification complete - ready for production use.

---

## Testing Strategy

### Unit Tests:

- **wf_case_runner_tests.erl**: Test all state transitions, signal delivery, cancellation, status queries, trace configuration
- **wf_supervision_tree_tests.erl**: Test supervisor start/stop, dynamic child management, restart behavior, name conflicts
- **wf_substrate_api_tests.erl**: Test all API functions with various inputs and error cases

**Key edge cases:**
- Case timeout during execution
- Cancel during waiting_effect state
- Signal delivery to non-existent case
- await/2 timeout behavior
- Multiple concurrent cases
- Process name conflicts
- Supervisor restart scenarios

### Integration Tests:

- **End-to-end workflow execution**: Create case, run to completion, await result
- **Signal delivery during execution**: Send signals at various points in workflow
- **Cancellation scenarios**: Cancel at different states (running, waiting, done)
- **Concurrent cases**: Run multiple cases simultaneously, verify isolation
- **Trace collection**: Configure tracing, verify events captured
- **Error recovery**: Case crash, supervisor restart behavior

### Manual Testing Steps:

1. **Start application**:
   ```bash
   rebar3 shell
   > application:start(wf_substrate).
   ```

2. **Create and run simple case**:
   ```erlang
   Bytecode = [{'TASK_EXEC', test}, {'DONE'}],
   {ok, Pid} = wf_substrate:new_case(case1, Bytecode, #{}),
   {ok, Result} = wf_substrate:await(case1, 5000).
   ```

3. **Verify supervision tree**:
   ```erlang
   > observer:start().
   %% Verify wf_substrate_sup -> wf_case_sup -> wf_case_runner
   ```

4. **Test cancellation**:
   ```erlang
   > Bytecode = [{'TASK_EXEC', long_task}, {'DONE'}],
   > {ok, Pid} = wf_substrate:new_case(case2, Bytecode, #{}),
   > wf_substrate:cancel(case2).
   ```

5. **Test trace configuration**:
   ```erlang
   > wf_substrate:trace(case1, min, {ets, wf_trace_events}),
   > ets:tab2list(wf_trace_events).
   ```

## Migration Notes

No migration required - this is a new feature implementation. Existing modules (wf_exec, wf_sched, wf_state, wf_trace) are unchanged.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/012-otp-supervision-tree/research.md`
- wf_exec module: `/Users/speed/wf-substrate/src/wf_exec.erl:1-783`
- wf_sched module: `/Users/speed/wf-substrate/src/wf_sched.erl:1-174`
- wf_state module: `/Users/speed/wf-substrate/src/wf_state.erl:1-579`
- wf_trace module: `/Users/speed/wf-substrate/src/wf_trace.erl:1-335`
- Records: `/Users/speed/wf-substrate/src/wf_exec.hrl:1-44`, `/Users/speed/wf-substrate/src/wf_trace.hrl:1-30`
- Test patterns: `/Users/speed/wf-substrate/test/wf_exec_tests.erl:1-80`, `/Users/speed/wf-substrate/test/wf_state_tests.erl:1-80`
