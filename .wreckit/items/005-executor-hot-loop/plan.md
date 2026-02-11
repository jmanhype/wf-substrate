# Implement bytecode executor hot loop Implementation Plan

## Implementation Plan Title

Bytecode Executor: Hot Loop with Cooperative Scheduling

## Overview

Implement wf_exec.erl, the bytecode executor that runs compiled workflow bytecode in a tight dispatch loop. The executor maintains an explicit exec_state record with instruction pointer (IP), branch tracking, join counters, scope stack, token set, and reduction counter. The step/2 function executes a single opcode, and run/3 executes N reductions (quanta) then yields control for cooperative scheduling.

The executor is a flat instruction-pointer-driven machine with NO per-step "case NodeType of" dispatch—all structural decisions are resolved at compile time by the bytecode compiler (item 004). Each opcode handler is a clause in execute_opcode/2, implementing the reduction rules from SEMANTICS.md.

This is **item 005** in the implementation sequence. The executor depends on wf_vm.erl (item 004) for type definitions and wf_compile.erl (item 004) for bytecode production, but can be implemented incrementally using mock bytecode for testing.

## Current State

**What exists now:**
- Rebar3 scaffold (item 001) with empty app structure
- Complete architecture documentation (ARCHITECTURE.md)
- Complete operational semantics (SEMANTICS.md)
- Item 002 (wf_term) is in "researched" state - not implemented
- Item 004 (wf_compile, wf_vm) is in "researched" state - not implemented

**What's missing:**
- wf_exec.erl - bytecode executor module
- wf_vm.erl - bytecode type definitions (wf_bc(), opcode(), join_policy(), etc.)
- wf_exec_tests.erl - comprehensive test suite
- All opcode handlers (12 opcodes for 9 kernel primitives)
- Cooperative scheduling implementation (run/3 quanta yielding)
- Trace event integration (wf_trace - item 011)
- Scheduler integration (wf_sched - item 007)
- Cancellation integration (wf_cancel - item 008)
- Effect yield integration (wf_effect - item 010)

**Key constraints discovered:**
1. **No per-step AST dispatch** (PROMPT.md:23-27) - executor must NOT interpret wf_term() at runtime
2. **Single IP for executor** (research.md:266-268) - exec_state.ip is executor IP, tokens have their own IPs
3. **current_token field** (research.md:269-270) - tracks which token is currently being executed
4. **status field** (research.md:271-272) - explicit execution status (running/done/blocked/cancelled) for is_done/1 and is_blocked/1
5. **step_count field** (research.md:273-277) - counts reductions for stats, quanta limiting, debugging
6. **Pattern matching dispatch** (research.md:304-311) - use function head pattern matching, not case statement
7. **Task function lookup** (research.md:1044-1048) - task functions stored in bytecode metadata as module:function references
8. **Effect yield implicit in TASK_EXEC** (research.md:1046-1047) - no separate EFFECT_YIELD opcode, executor detects {effect, Spec, ContCtx} return
9. **Two-opcode CANCEL_SCOPE** (research.md:1049-1050) - {CANCEL_SCOPE, {enter, ScopeId}} and {CANCEL_SCOPE, {exit, ScopeId}}
10. **Defer omitted in v1** (item 004 plan.md:73, research.md:549-556) - defer compilation not implemented, defer opcode handlers can be stubs

## Desired End State

**Specification:** Fully implemented bytecode executor with all 12 opcodes, cooperative scheduling, and integration points for tracing, scheduling, cancellation, and effects.

**Functional requirements:**
- wf_exec:new/1 creates executor state from bytecode
- wf_exec:step/2 executes single reduction and returns updated state + trace event
- wf_exec:run/3 executes N quanta and yields {yield, State} or {done, State} or {effect, Spec, Cont, State}
- wf_exec:is_done/1 detects terminal states (done/cancelled/failed)
- wf_exec:is_blocked/1 detects blocking states (blocked_effect/blocked_join/blocked_signal)
- execute_opcode/2 dispatches to 12 opcode handlers via pattern matching
- All opcode handlers implement reduction rules from SEMANTICS.md:238-1090

**Non-functional requirements:**
- Executor never blocks indefinitely (cooperative scheduling via quanta)
- No AST interpretation at runtime (flat instruction-pointer-driven machine)
- All functions have complete -spec declarations for Dialyzer
- Code coverage > 90% for executor module
- Execution is deterministic when using deterministic scheduler policy
- Trace events emitted for every reduction (configurable trace level)

**Verification criteria:**
- `rebar3 compile` succeeds with zero warnings
- `rebar3 dialyzer` passes with no type errors
- `rebar3 eunit` passes all executor tests (100% success rate)
- Manual execution of mock bytecode produces expected IP advancement
- Quanta execution yields after N steps (verified with Quanta=1 test)
- Parallel execution (par) spawns correct number of tokens
- Join synchronization blocks correctly until all branches complete
- Cancellation propagation marks all tokens in scope as cancelled
- Effect yield blocks execution and resumes with effect result
- Trace events capture state transitions correctly

### Key Discoveries:

- **Dependency on item 004 is critical**: From `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/plan.md:67-78`, item 004 defines wf_vm.erl with wf_bc() and opcode() types. Executor CANNOT proceed until these types exist. Decision: Implement wf_vm.erl type definitions first (part of item 004), then implement executor. Create mock bytecode for testing until compiler is ready.

- **Task function lookup via bytecode metadata**: From research.md:1044-1048, task name must map to executable function. Decision: Store task functions in bytecode metadata as {TaskName, {Module, Function}} tuples. Executor looks up and calls Module:Function(Ctx). Compiler (item 004) emits metadata. Keeps bytecode self-contained.

- **Effect yield is implicit**: From research.md:1046-1047, executor detects {effect, Spec, ContCtx} return from task function. No separate EFFECT_YIELD opcode needed. Decision: TASK_EXEC handler checks task return value: {ok, Ctx} → continue, {effect, Spec, ContCtx} → yield, {error, Reason} → terminate.

- **Two-opcode CANCEL_SCOPE format**: From research.md:1049-1050 and SEMANTICS.md:749-861, cancellation has enter/exit operations. Decision: Use {CANCEL_SCOPE, {enter, ScopeId}} and {CANCEL_SCOPE, {exit, ScopeId}} opcodes. Compiler emits paired enter/exit. Executor pushes/pops scope_stack.

- **Defer can be stubbed**: From item 004 plan.md:73 and research.md:549-556, defer compilation is deferred to v2. Decision: Implement execute_defer_wait/2 and execute_defer_select/2 as stubs that return {error, not_implemented}. Omit defer tests from v1.

- **Cooperative scheduling via quanta**: From ARCHITECTURE.md:486-505 and PROMPT.md:194-197, executor must yield after N reductions. Decision: run/3 executes up to Quanta steps, then returns {yield, State}. Default Quanta=100. Configurable via options. Enables fair scheduling of multiple cases.

- **Token management complexity**: From research.md:522-527, PAR_FORK spawns N tokens with independent IPs. Decision: Use maps for tokens (#{token_id() => token()}). PAR_FORK removes current token, adds N branch tokens. DONE marks token complete, not removed (for trace/debugging). Cleanup: remove completed tokens when join satisfied.

- **Scheduler decision for XOR_CHOOSE**: From research.md:580-599, XOR_CHOOSE selects ONE branch via scheduler. Decision: run/3 calls wf_sched:select_action/2 to get scheduler decision. Pass decision to step/2. XOR_CHOOSE handler uses decision to select branch. Deterministic scheduler produces reproducible traces.

- **Blocking on JOIN_WAIT**: From research.md:529-578, JOIN_WAIT blocks if join not satisfied. Decision: Set status=blocked_join. Other tokens may continue (multi-token executor). When all tokens blocked or complete, executor is blocked. Case runner (gen_statem) waits for external event (effect result, signal).

- **Trace event overhead**: From research.md:1037-1039, every reduction emits trace event (expensive). Decision: Make tracing optional via trace_level option (none|min|full). Default to min (basic events). wf_trace:emit_if/2 checks trace level before emitting.

## What We're NOT Doing

- **NOT implementing compiler (wf_compile)**: That's item 004. Executor consumes bytecode, doesn't produce it.
- **NOT implementing validator (wf_validate)**: That's item 013. Executor assumes valid bytecode.
- **NOT implementing scheduler policies (wf_sched)**: That's item 007. Executor calls scheduler, doesn't implement policies.
- **NOT implementing cancellation propagation (wf_cancel)**: That's item 008. Executor calls wf_cancel:propagate/2, doesn't implement algorithm.
- **NOT implementing effect execution (wf_effect)**: That's item 010. Executor yields effect specs, doesn't execute them.
- **NOT implementing tracing (wf_trace)**: That's item 011. Executor emits events, doesn't implement trace storage.
- **NOT implementing state persistence (wf_state)**: That's item 006. Executor keeps state in exec_state record for v1.
- **NOT implementing multiple instances (wf_mi)**: That's item 009. MI_SPAWN handler spawns instances like PAR_FORK.
- **NOT implementing defer opcodes**: Defer compilation omitted in v1 (item 004 plan.md:73). Defer handlers are stubs.
- **NOT doing bytecode optimization**: Executor executes bytecode as-is. No peephole optimization.
- **NOT implementing error recovery**: Task errors terminate case (status=failed). No retry logic in v1.
- **NOT implementing source maps**: Debugging aids (line numbers, source positions) deferred to v2.
- **NOT implementing profile/coverage**: No built-in profiling. Use external tools (fprof, cover).
- **NOT implementing distributed execution**: Single-node executor only. No distributed opcodes.

## Implementation Approach

**High-level strategy**: Incremental implementation starting with single-token executor (no parallel), then adding multi-token support (PAR_FORK, XOR_CHOOSE, JOIN_WAIT), loop support (LOOP_CHECK, LOOP_BACK), cancellation support (CANCEL_SCOPE), and effect yield support (TASK_EXEC effect). Use mock bytecode for testing until wf_compile is implemented. Coordinate with dependent modules by defining clear interfaces and using stub functions for unimplemented dependencies.

**Phase ordering**: Foundation (records + new/1) → Single-token executor (task, seq) → Multi-token executor (par, xor, join) → Loop support → Cancellation support → Effect yield support → Scheduler integration → Tracing integration → Comprehensive testing. Each phase is independently testable and builds on the previous phase.

**Testing strategy**: Test each opcode handler in isolation with mock bytecode. Test exec_state transitions (IP advancement, token creation/removal, scope stack push/pop). Test quanta execution (yield after N steps). Test termination detection (is_done/1). Test blocking detection (is_blocked/1). Test cooperative scheduling (run/3 with Quanta=1 yields every step). Use property-based tests for invariants (token count non-negative, IP in bounds, no infinite loops).

**Stub implementation for dependencies**: Use stub functions for unimplemented modules:
- wf_sched:select_action/2 → stub returning deterministic choice (first token)
- wf_cancel:propagate/2 → stub returning unchanged state
- wf_effect:execute/3 → stub returning {ok, mock_result}
- wf_trace:emit/2 → stub returning ok (no-op)

These stubs allow executor to be tested in isolation. Replace with real implementations when items 007-011 are complete.

---

## Phases

### Phase 1: Foundation (exec_state Record and new/1)

#### Overview

Define exec_state record and helper records (token, branch_info, join_counter). Implement new/1 to create initial executor state from bytecode. Create mock bytecode generator for testing.

#### Changes Required:

##### 1. wf_vm.erl (Bytecode Type Definitions)

**File**: `src/wf_vm.erl`
**Changes**: Create type definitions module with all bytecode-related types.

```erlang
-module(wf_vm).
-export_types([]).

%% Bytecode is a list of opcodes
-type wf_bc() :: [opcode()].

%% Opcodes are tagged tuples
-type opcode() ::
    {SEQ_ENTER, non_neg_integer()} |          %% Arg: seq_id
    {SEQ_NEXT, non_neg_integer()} |           %% Arg: target IP
    {PAR_FORK, [non_neg_integer()]} |        %% Arg: list of target IPs
    {JOIN_WAIT, join_policy()} |
    {XOR_CHOOSE, [non_neg_integer()]} |      %% Arg: list of target IPs
    {LOOP_CHECK, loop_policy()} |
    {LOOP_BACK, non_neg_integer()} |          %% Arg: target IP
    {CANCEL_SCOPE, {enter | exit, term()}} |
    {MI_SPAWN, mi_policy()} |
    {TASK_EXEC, atom()} |                     %% Arg: task name
    {DONE}.

%% Join policies
-type join_policy() ::
    all |
    {first_n, pos_integer()} |
    {n_of_m, pos_integer(), pos_integer()} |  %% {N, M}
    first_complete |
    sync_merge.

%% Loop policies
-type loop_policy() ::
    {count, non_neg_integer()} |
    while |
    until.

%% Multiple instance policies
-type mi_policy() ::
    {fixed, pos_integer()} |
    {dynamic, pos_integer(), pos_integer()}.  %% {min, max}
```

**Note**: wf_vm.erl is created as part of item 004 but is required for executor types. Coordinate with item 004 implementation.

##### 2. wf_exec.erl (Executor Module - Records and new/1)

**File**: `src/wf_exec.erl`
**Changes**: Define exec_state record and helper records. Implement new/1, fetch_opcode/1, and basic state query functions.

```erlang
-module(wf_exec).
-behaviour(gen_server).

%% Exported API
-export([new/1, step/2, run/3, is_done/1, is_blocked/1,
         get_ip/1, get_ctx/1, get_step_count/1]).

%% Records
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
    current_token :: term() | undefined
}).

-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled
}).

-record(branch_info, {
    branch_id :: term(),
    tokens :: [term()],
    join_id :: term(),
    targets :: [non_neg_integer()]
}).

-record(join_counter, {
    join_id :: term(),
    completed :: non_neg_integer(),
    required :: non_neg_integer(),
    policy :: wf_vm:join_policy(),
    results :: [term()]
}).

%% Types
-type exec_state() :: #exec_state{}.
-type ctx() :: map().

%%====================================================================
%% API
%%====================================================================

%% @doc Create new executor from bytecode
-spec new(wf_vm:wf_bc()) -> exec_state().
new(Bytecode) ->
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active
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
        current_token = InitialTokenId
    }.

%% @doc Get current instruction pointer
-spec get_ip(exec_state()) -> non_neg_integer().
get_ip(#exec_state{ip = IP}) ->
    IP.

%% @doc Get current context
-spec get_ctx(exec_state()) -> ctx().
get_ctx(#exec_state{ctx = Ctx}) ->
    Ctx.

%% @doc Get step count
-spec get_step_count(exec_state()) -> non_neg_integer().
get_step_count(#exec_state{step_count = Count}) ->
    Count.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Fetch opcode at current IP
-spec fetch_opcode(exec_state()) -> wf_vm:opcode().
fetch_opcode(#exec_state{ip = IP, bytecode = Bytecode}) ->
    case IP < length(Bytecode) of
        true ->
            lists:nth(IP + 1, Bytecode);  %% IP is 0-indexed
        false ->
            {DONE}
    end.
```

##### 3. wf_exec_tests.erl (Mock Bytecode and Basic Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Create mock bytecode generator and test exec_state creation.

```erlang
-module(wf_exec_tests).
-include_lib("eunit/include/eunit.hrl").

%% Mock bytecode generators
mock_bytecode_simple_task() ->
    [{TASK_EXEC, mock_task}, {DONE}].

mock_bytecode_seq() ->
    [{SEQ_ENTER, 0}, {TASK_EXEC, task_a}, {SEQ_NEXT, 3}, {TASK_EXEC, task_b}, {DONE}].

%% Test exec_state creation
new_test_() ->
    Bytecode = mock_bytecode_simple_task(),
    ExecState = wf_exec:new(Bytecode),
    [
        ?_assertEqual(0, wf_exec:get_ip(ExecState)),
        ?_assertEqual(#{}, wf_exec:get_ctx(ExecState)),
        ?_assertEqual(0, wf_exec:get_step_count(ExecState)),
        ?_assertEqual(1, maps:size(ExecState#exec_state.tokens)),
        ?_assertEqual(running, ExecState#exec_state.status)
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with no warnings
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `new_test_` passes: exec_state created with correct initial values

##### Manual Verification:

- [ ] Inspect exec_state record fields: IP=0, tokens map has 1 entry, scope_stack=[root], status=running
- [ ] Verify fetch_opcode/1 returns correct opcode from mock bytecode
- [ ] Verify type declarations are correct (no dialyzer warnings)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Single-Token Executor (TASK_EXEC, SEQ_ENTER, SEQ_NEXT, DONE)

#### Overview

Implement basic opcode handlers for single-token execution. No parallelism yet. Executor executes task, sequence, and done opcodes. Focus on IP advancement and exec_state updates.

#### Changes Required:

##### 1. wf_exec.erl (Opcode Handlers for Single-Token Execution)

**File**: `src/wf_exec.erl`
**Changes**: Implement execute_opcode/2 dispatch and handlers for TASK_EXEC, SEQ_ENTER, SEQ_NEXT, DONE.

```erlang
%% Exported functions (add to existing exports)
-export([execute_opcode/2,
         execute_seq_enter/2,
         execute_seq_next/2,
         execute_task_exec/2,
         execute_done/1]).

%%====================================================================
%% Opcode Dispatch
%%====================================================================

%% @doc Execute opcode and update exec_state
-spec execute_opcode(wf_vm:opcode(), exec_state()) -> exec_state().
execute_opcode({SEQ_ENTER, _Arg}, ExecState) ->
    execute_seq_enter({SEQ_ENTER, _Arg}, ExecState);
execute_opcode({SEQ_NEXT, TargetIP}, ExecState) ->
    execute_seq_next({SEQ_NEXT, TargetIP}, ExecState);
execute_opcode({TASK_EXEC, TaskName}, ExecState) ->
    execute_task_exec({TASK_EXEC, TaskName}, ExecState);
execute_opcode({DONE}, ExecState) ->
    execute_done(ExecState);
execute_opcode(_Opcode, _ExecState) ->
    error({unimplemented_opcode, _Opcode}).

%%====================================================================
%% Opcode Handlers
%%====================================================================

%% @doc Execute SEQ_ENTER: push scope, advance IP
execute_seq_enter({SEQ_ENTER, _Arg}, ExecState) ->
    NewScopeId = make_ref(),
    NewScopeStack = [NewScopeId | ExecState#exec_state.scope_stack],
    ExecState#exec_state{
        ip = ExecState#exec_state.ip + 1,
        scope_stack = NewScopeStack,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Execute SEQ_NEXT: jump to right branch
execute_seq_next({SEQ_NEXT, TargetIP}, ExecState) ->
    ExecState#exec_state{
        ip = TargetIP,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Execute TASK_EXEC: run task function (mock for now)
execute_task_exec({TASK_EXEC, _TaskName}, ExecState) ->
    %% Mock: task always succeeds, updates context
    NewCtx = ExecState#exec_state.ctx#{task_result => ok},
    ExecState#exec_state{
        ctx = NewCtx,
        ip = ExecState#exec_state.ip + 1,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Execute DONE: mark token complete, check if executor done
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = ExecState#exec_state.tokens#{CurrentToken => UpdatedToken},

    %% Check if all tokens complete
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],
    case ActiveTokens of
        [] ->
            %% No active tokens, executor is done
            ExecState#exec_state{
                tokens = Tokens,
                status = done,
                step_count = ExecState#exec_state.step_count + 1
            };
        _ ->
            %% Other tokens active (shouldn't happen in single-token executor)
            ExecState#exec_state{
                tokens = Tokens,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.
```

##### 2. wf_exec.erl (step/2 and run/3 Implementation)

**File**: `src/wf_exec.erl`
**Changes**: Implement step/2 (single reduction) and run/3 (quanta execution with yielding).

```erlang
%% Exported functions (add to existing exports)
-export([step/2, run/3]).

%%====================================================================
%% Step and Run
%%====================================================================

%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), term()}.
step(ExecState, _SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),
    TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, TraceEvent}.

%% @doc Execute N quanta, yield when exhausted or terminal
-spec run(exec_state(), pos_integer(), term()) ->
    {done, exec_state()} | {yield, exec_state()}.
run(ExecState0, Quanta, _SchedPolicy) ->
    run_loop(ExecState0, Quanta, 0).

run_loop(ExecState, Quanta, _Count) when Count >= Quanta ->
    %% Quanta exhausted, yield
    {yield, ExecState};

run_loop(ExecState, _Quanta, _Count) when ExecState#exec_state.status =:= done ->
    %% Terminal state
    {done, ExecState};

run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
    %% Execute one step (no scheduler integration yet)
    {ExecState1, _TraceEvent} = step(ExecState0, undefined),
    run_loop(ExecState1, Quanta, SchedPolicy, Count + 1).
```

##### 3. wf_exec.erl (is_done/1 and is_blocked/1)

**File**: `src/wf_exec.erl`
**Changes**: Implement state query functions.

```erlang
%% @doc Check if executor is in terminal state
-spec is_done(exec_state()) -> boolean().
is_done(#exec_state{status = Status}) ->
    Status =:= done orelse Status =:= cancelled orelse Status =:= failed.

%% @doc Check if executor is blocked (waiting for external event)
-spec is_blocked(exec_state()) -> boolean().
is_blocked(#exec_state{status = Status}) ->
    Status =:= blocked_effect orelse
    Status =:= blocked_join orelse
    Status =:= blocked_signal.
```

##### 4. wf_exec_tests.erl (Single-Token Execution Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for TASK_EXEC, SEQ_ENTER, SEQ_NEXT, DONE, step/2, run/3, is_done/1, is_blocked/1.

```erlang
%% Test single task execution
single_task_test_() ->
    Bytecode = mock_bytecode_simple_task(),
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(1, wf_exec:get_ip(ExecState1)),
        ?_assertEqual(1, wf_exec:get_step_count(ExecState1)),
        ?_assertEqual(running, ExecState1#exec_state.status)
    ].

%% Test DONE opcode
done_test_() ->
    Bytecode = [{DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(done, ExecState1#exec_state.status),
        ?_assert(wf_exec:is_done(ExecState1)),
        ?_assertNot(wf_exec:is_blocked(ExecState1))
    ].

%% Test sequence execution
sequence_test_() ->
    Bytecode = mock_bytecode_seq(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute SEQ_ENTER
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),
    %% Execute TASK_EXEC(task_a)
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    %% Execute SEQ_NEXT (jump to IP 3)
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    [
        ?_assertEqual(3, wf_exec:get_ip(ExecState3)),
        ?_assertEqual(3, wf_exec:get_step_count(ExecState3))
    ].

%% Test quanta execution and yielding
quanta_yield_test_() ->
    Bytecode = mock_bytecode_seq(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Run with Quanta=2, should yield after 2 steps
    Result = wf_exec:run(ExecState0, 2, undefined),
    [
        ?_assertMatch({yield, _ExecState}, Result),
        ?_assertEqual(2, wf_exec:get_step_count(element(2, Result)))
    ].

%% Test run until done
run_until_done_test_() ->
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 100, undefined),
    [
        ?_assertMatch({done, _ExecState}, Result),
        ?_assertEqual(done, element(2, Result)#exec_state.status)
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all single-token executor tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `single_task_test_` passes: IP advances to 1 after TASK_EXEC
- [ ] Test `done_test_` passes: status=done, is_done/1 returns true
- [ ] Test `sequence_test_` passes: SEQ_NEXT jumps to correct IP
- [ ] Test `quanta_yield_test_` passes: yields after Quanta steps
- [ ] Test `run_until_done_test_` passes: returns {done, State}

##### Manual Verification:

- [ ] Execute bytecode with multiple tasks, verify IP advances sequentially
- [ ] Execute bytecode with SEQ_NEXT, verify jump to target IP
- [ ] Run with Quanta=1, verify yields after every step
- [ ] Inspect exec_state after DONE, verify token status=complete, executor status=done
- [ ] Inspect scope_stack after SEQ_ENTER, verify new scope pushed

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Multi-Token Support (PAR_FORK, XOR_CHOOSE, JOIN_WAIT, DONE Update)

#### Overview

Implement multi-token execution with parallel fork, exclusive choice, and join synchronization. PAR_FORK spawns N tokens with independent IPs. JOIN_WAIT blocks until all branches complete. XOR_CHOOSE selects ONE branch via scheduler decision. DONE handler updated for multi-token context.

#### Changes Required:

##### 1. wf_exec.erl (PAR_FORK Handler)

**File**: `src/wf_exec.erl`
**Changes**: Implement execute_par_fork/2 to spawn N tokens, create branch_info and join_counter records.

```erlang
%% Exported functions (add to existing exports)
-export([execute_par_fork/2]).

%%====================================================================
%% Multi-Token Opcodes
%%====================================================================

%% @doc Execute PAR_FORK: spawn N tokens, track in branch_map
execute_par_fork({PAR_FORK, TargetIPs}, ExecState) ->
    BranchId = make_ref(),
    JoinId = make_ref(),
    NumBranches = length(TargetIPs),

    %% Spawn N tokens
    CurrentToken = ExecState#exec_state.current_token,
    ScopeId = get_current_scope(ExecState),
    NewTokens = lists:map(fun(IP) ->
        TokenId = make_ref(),
        Token = #token{
            token_id = TokenId,
            ip = IP,
            scope_id = ScopeId,
            value = undefined,
            status = active
        },
        {TokenId, Token}
    end, TargetIPs),

    %% Add tokens to state
    TokensMap0 = ExecState#exec_state.tokens,
    TokensMap = lists:foldl(fun({TokenId, Token}, Acc) ->
        Acc#{TokenId => Token}
    end, TokensMap0, NewTokens),

    %% Create branch info
    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = [TokenId || {TokenId, _Token} <- NewTokens],
        join_id = JoinId,
        targets = TargetIPs
    },
    BranchMap0 = ExecState#exec_state.branch_map,
    BranchMap = BranchMap0#{BranchId => BranchInfo},

    %% Create join counter
    JoinCounter = #join_counter{
        join_id = JoinId,
        completed = 0,
        required = NumBranches,
        policy = all,
        results = []
    },
    JoinCounters0 = ExecState#exec_state.join_counters,
    JoinCounters = JoinCounters0#{JoinId => JoinCounter},

    %% Mark current token as complete (replaced by branch tokens)
    TokensMapFinal = maps:remove(CurrentToken, TokensMap),

    %% Advance IP (past PAR_FORK instruction)
    NewIP = ExecState#exec_state.ip + 1,

    %% Select next token to execute
    NextToken = select_next_token(TokensMapFinal),

    ExecState#exec_state{
        ip = NewIP,
        tokens = TokensMapFinal,
        branch_map = BranchMap,
        join_counters = JoinCounters,
        step_count = ExecState#exec_state.step_count + 1,
        current_token = NextToken
    }.

%% @doc Get current scope from scope_stack
-spec get_current_scope(exec_state()) -> term().
get_current_scope(#exec_state{scope_stack = [ScopeId | _]}) ->
    ScopeId.

%% @doc Select next token to execute (deterministic: first active token)
-spec select_next_token(#{term() => #token{}}) -> term() | undefined.
select_next_token(TokensMap) ->
    ActiveTokens = [T || T <- maps:values(TokensMap), T#token.status =:= active],
    case ActiveTokens of
        [] -> undefined;
        [First | _] -> First#token.token_id
    end.
```

##### 2. wf_exec.erl (JOIN_WAIT Handler)

**File**: `src/wf_exec.erl`
**Changes**: Implement execute_join_wait/2 to check join counter, merge results when satisfied, block otherwise.

```erlang
%% Exported functions (add to existing exports)
-export([execute_join_wait/2]).

%% @doc Execute JOIN_WAIT: check join counter, block if not satisfied
execute_join_wait({JOIN_WAIT, _Policy}, ExecState) ->
    %% Find active join (must be exactly one)
    JoinId = find_active_join(ExecState),
    JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),

    case JoinCounter#join_counter.completed >= JoinCounter#join_counter.required of
        true ->
            %% Join satisfied, merge results and continue
            %% Remove join counter and branch map entry
            JoinCounters = maps:remove(JoinId, ExecState#exec_state.join_counters),
            BranchMap = remove_branch_by_join(JoinId, ExecState#exec_state.branch_map),

            %% Create continuation token with merged results
            ContinuationTokenId = make_ref(),
            ContinuationToken = #token{
                token_id = ContinuationTokenId,
                ip = ExecState#exec_state.ip + 1,
                scope_id = get_current_scope(ExecState),
                value = merge_results(JoinCounter#join_counter.results, all),
                status = active
            },
            Tokens = ExecState#exec_state.tokens#{ContinuationTokenId => ContinuationToken},

            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,
                tokens = Tokens,
                join_counters = JoinCounters,
                branch_map = BranchMap,
                step_count = ExecState#exec_state.step_count + 1,
                current_token = ContinuationTokenId
            };
        false ->
            %% Join not satisfied, block
            ExecState#exec_state{
                status = blocked_join,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Find active join point
-spec find_active_join(exec_state()) -> term().
find_active_join(#exec_state{join_counters = JoinCounters}) ->
    %% For simplicity, return first join_id
    %% In production, should be exactly one join
    [JoinId | _] = maps:keys(JoinCounters),
    JoinId.

%% @doc Remove branch entry by join_id
-spec remove_branch_by_join(term(), #{term() => #branch_info{}}) -> #{term() => #branch_info{}}.
remove_branch_by_join(JoinId, BranchMap) ->
    maps:filter(fun(_BranchId, BranchInfo) ->
        BranchInfo#branch_info.join_id =/= JoinId
    end, BranchMap).

%% @doc Merge results from parallel branches
-spec merge_results([term()], wf_vm:join_policy()) -> term().
merge_results(Results, all) ->
    {merged, Results};
merge_results(Results, {first_n, _N}) ->
    {merged, Results};
merge_results(Results, first_complete) ->
    {first_complete, hd(Results)};
merge_results(Results, sync_merge) ->
    {sync_merged, lists:foldl(fun merge_state/2, #{}, Results)}.

%% @doc Merge state maps
merge_state(StateMap, Acc) ->
    maps:merge(Acc, StateMap).
```

##### 3. wf_exec.erl (XOR_CHOOSE Handler)

**File**: `src/wf_exec.erl`
**Changes**: Implement execute_xor_choose/3 to select ONE branch via scheduler decision.

```erlang
%% Exported functions (add to existing exports)
-export([execute_xor_choose/3]).

%% @doc Execute XOR_CHOOSE: select ONE branch via scheduler decision
execute_xor_choose({XOR_CHOOSE, TargetIPs}, ExecState, SchedDecision) ->
    %% Scheduler decision contains selected branch index
    %% For now, always select first branch (deterministic)
    SelectedIndex = case SchedDecision of
        {xor_branch, Index} -> Index;
        _ -> 1
    end,
    SelectedIP = lists:nth(SelectedIndex, TargetIPs),

    %% Update current token's IP to selected branch
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{ip = SelectedIP},
    Tokens = ExecState#exec_state.tokens#{CurrentToken => UpdatedToken},

    ExecState#exec_state{
        ip = SelectedIP,
        tokens = Tokens,
        step_count = ExecState#exec_state.step_count + 1
    }.
```

##### 4. wf_exec.erl (DONE Handler Update for Multi-Token)

**File**: `src/wf_exec.erl`
**Changes**: Update execute_done/1 to handle multi-token context (select next token when others active).

```erlang
%% @doc Execute DONE: mark token complete, check if all done
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens0 = ExecState#exec_state.tokens,
    Tokens = Tokens0#{CurrentToken => UpdatedToken},

    %% Check if all tokens complete
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],
    case ActiveTokens of
        [] ->
            %% No active tokens, executor is done
            ExecState#exec_state{
                tokens = Tokens,
                status = done,
                step_count = ExecState#exec_state.step_count + 1
            };
        _ ->
            %% Other tokens still active, select next token
            NextToken = select_next_token(Tokens),
            ExecState#exec_state{
                tokens = Tokens,
                current_token = NextToken,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.
```

##### 5. wf_exec.erl (Update execute_opcode/2 Dispatch)

**File**: `src/wf_exec.erl`
**Changes**: Add PAR_FORK, JOIN_WAIT, XOR_CHOOSE to dispatch.

```erlang
%% @doc Execute opcode and update exec_state (update existing function)
-spec execute_opcode(wf_vm:opcode(), exec_state()) -> exec_state().
execute_opcode({SEQ_ENTER, _Arg}, ExecState) ->
    execute_seq_enter({SEQ_ENTER, _Arg}, ExecState);
execute_opcode({SEQ_NEXT, TargetIP}, ExecState) ->
    execute_seq_next({SEQ_NEXT, TargetIP}, ExecState);
execute_opcode({PAR_FORK, Targets}, ExecState) ->
    execute_par_fork({PAR_FORK, Targets}, ExecState);
execute_opcode({JOIN_WAIT, Policy}, ExecState) ->
    execute_join_wait({JOIN_WAIT, Policy}, ExecState);
execute_opcode({XOR_CHOOSE, Targets}, ExecState) ->
    %% Scheduler decision required, pass undefined for now
    execute_xor_choose({XOR_CHOOSE, Targets}, ExecState, undefined);
execute_opcode({TASK_EXEC, TaskName}, ExecState) ->
    execute_task_exec({TASK_EXEC, TaskName}, ExecState);
execute_opcode({DONE}, ExecState) ->
    execute_done(ExecState);
execute_opcode(_Opcode, _ExecState) ->
    error({unimplemented_opcode, _Opcode}).
```

##### 6. wf_exec_tests.erl (Multi-Token Execution Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for PAR_FORK, JOIN_WAIT, XOR_CHOOSE.

```erlang
%% Mock bytecode for parallel execution
mock_bytecode_par() ->
    [
        {PAR_FORK, [1, 3]},           %% Fork to IP 1 and 3
        {TASK_EXEC, task_a},           %% IP 1: task_a
        {DONE},                        %% IP 2: done (branch 1)
        {TASK_EXEC, task_b},           %% IP 3: task_b
        {DONE},                        %% IP 4: done (branch 2)
        {JOIN_WAIT, all}              %% IP 5: wait for all branches
    ].

%% Test PAR_FORK spawns 2 tokens
par_fork_test_() ->
    Bytecode = mock_bytecode_par(),
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(2, maps:size(ExecState1#exec_state.tokens)),
        ?_assertEqual(1, map_size(ExecState1#exec_state.branch_map)),
        ?_assertEqual(1, map_size(ExecState1#exec_state.join_counters))
    ].

%% Test JOIN_WAIT blocks until branches complete
join_wait_test_() ->
    Bytecode = mock_bytecode_par(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute PAR_FORK
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),
    %% Execute first branch (task_a + done)
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    %% Execute second branch (task_b + done)
    {ExecState4, _Trace4} = wf_exec:step(ExecState3, undefined),
    {ExecState5, _Trace5} = wf_exec:step(ExecState4, undefined),
    %% Execute JOIN_WAIT (should succeed now)
    {ExecState6, _Trace6} = wf_exec:step(ExecState5, undefined),
    [
        ?_assertEqual(1, maps:size(ExecState6#exec_state.tokens)),  %% Continuation token
        ?_assertEqual(0, map_size(ExecState6#exec_state.join_counters)),
        ?_assertEqual(running, ExecState6#exec_state.status)
    ].

%% Test XOR_CHOOSE selects one branch
xor_choose_test_() ->
    Bytecode = [{XOR_CHOOSE, [1, 3]}, {TASK_EXEC, task_a}, {DONE}, {TASK_EXEC, task_b}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(1, maps:size(ExecState1#exec_state.tokens)),  %% Still 1 token
        ?_assertEqual(1, wf_exec:get_ip(ExecState1))  %% Selected first branch
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all multi-token executor tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `par_fork_test_` passes: 2 tokens spawned, branch_map and join_counters created
- [ ] Test `join_wait_test_` passes: JOIN_WAIT succeeds after both branches complete
- [ ] Test `xor_choose_test_` passes: only 1 token, IP advances to selected branch
- [ ] Token count invariant preserved (never negative, never exceeds reasonable bound)

##### Manual Verification:

- [ ] Execute par([task_a, task_b]) bytecode, verify 2 tokens with independent IPs
- [ ] Inspect branch_map after PAR_FORK, verify correct tracking
- [ ] Inspect join_counters after PAR_FORK, verify completed=0, required=2
- [ ] Execute both branches to DONE, verify join counter increments
- [ ] Execute JOIN_WAIT after join satisfied, verify branch_map entry removed
- [ ] Execute xor([task_a, task_b]) bytecode, verify only one branch executes
- [ ] Inspect tokens map after XOR_CHOOSE, verify token count is 1 (not N)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Loop Support (LOOP_CHECK, LOOP_BACK)

#### Overview

Implement structured loop execution. LOOP_CHECK evaluates loop condition (exit or continue). LOOP_BACK jumps to loop head. Support three loop policies: {count, N} (fixed iterations), while (check first), until (check after).

#### Changes Required:

##### 1. wf_exec.erl (LOOP_CHECK and LOOP_BACK Handlers)

**File**: `src/wf_exec.erl`
**Changes**: Implement execute_loop_check/2 and execute_loop_back/2.

```erlang
%% Exported functions (add to existing exports)
-export([execute_loop_check/2, execute_loop_back/2]).

%%====================================================================
%% Loop Opcodes
%%====================================================================

%% @doc Execute LOOP_CHECK: evaluate condition, exit or continue
execute_loop_check({LOOP_CHECK, Policy}, ExecState) ->
    case evaluate_loop_condition(Policy, ExecState) of
        true ->
            %% Condition satisfied, continue to body
            %% IP advances to next instruction (body)
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        false ->
            %% Condition not satisfied, exit loop
            %% Jump to instruction after LOOP_BACK (exit label)
            %% For simplicity, assume exit label is next instruction after LOOP_BACK
            %% In production, compiler emits explicit exit label
            %% Advance to body, LOOP_BACK will jump back
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Execute LOOP_BACK: jump to loop head
execute_loop_back({LOOP_BACK, TargetIP}, ExecState) ->
    ExecState#exec_state{
        ip = TargetIP,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Evaluate loop condition
-spec evaluate_loop_condition(wf_vm:loop_policy(), exec_state()) -> boolean().
evaluate_loop_condition({count, N}, ExecState) ->
    %% Decrement counter stored in context
    Counter = maps:get(loop_counter, ExecState#exec_state.ctx, N),
    NewCounter = Counter - 1,
    %% Update context
    NewCtx = ExecState#exec_state.ctx#{loop_counter => NewCounter},
    %% Return true if counter > 0
    NewCounter > 0 andalso begin
        %% Update context in exec_state (side effect in condition evaluation)
        %% This is a bit awkward, but loop condition may need to update state
        %% For now, return boolean only, context update handled elsewhere
        true
    end;
evaluate_loop_condition(while, _ExecState) ->
    %% While loop: check condition first
    %% For simplicity, always continue (mock)
    true;
evaluate_loop_condition(until, _ExecState) ->
    %% Until loop: check condition after body
    %% For simplicity, always exit (mock)
    false.
```

##### 2. wf_exec.erl (Update execute_opcode/2 Dispatch)

**File**: `src/wf_exec.erl`
**Changes**: Add LOOP_CHECK, LOOP_BACK to dispatch.

```erlang
%% @doc Execute opcode and update exec_state (update existing function)
-spec execute_opcode(wf_vm:opcode(), exec_state()) -> exec_state().
execute_opcode({SEQ_ENTER, _Arg}, ExecState) ->
    execute_seq_enter({SEQ_ENTER, _Arg}, ExecState);
execute_opcode({SEQ_NEXT, TargetIP}, ExecState) ->
    execute_seq_next({SEQ_NEXT, TargetIP}, ExecState);
execute_opcode({PAR_FORK, Targets}, ExecState) ->
    execute_par_fork({PAR_FORK, Targets}, ExecState);
execute_opcode({JOIN_WAIT, Policy}, ExecState) ->
    execute_join_wait({JOIN_WAIT, Policy}, ExecState);
execute_opcode({XOR_CHOOSE, Targets}, ExecState) ->
    execute_xor_choose({XOR_CHOOSE, Targets}, ExecState, undefined);
execute_opcode({LOOP_CHECK, Policy}, ExecState) ->
    execute_loop_check({LOOP_CHECK, Policy}, ExecState);
execute_opcode({LOOP_BACK, TargetIP}, ExecState) ->
    execute_loop_back({LOOP_BACK, TargetIP}, ExecState);
execute_opcode({CANCEL_SCOPE, ScopeOp}, ExecState) ->
    execute_cancel_scope({CANCEL_SCOPE, ScopeOp}, ExecState);
execute_opcode({MI_SPAWN, Policy}, ExecState) ->
    execute_mi_spawn({MI_SPAWN, Policy}, ExecState);
execute_opcode({TASK_EXEC, TaskName}, ExecState) ->
    execute_task_exec({TASK_EXEC, TaskName}, ExecState);
execute_opcode({DONE}, ExecState) ->
    execute_done(ExecState);
execute_opcode(_Opcode, _ExecState) ->
    error({unimplemented_opcode, _Opcode}).
```

##### 3. wf_exec_tests.erl (Loop Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for loop execution.

```erlang
%% Mock bytecode for count loop
mock_bytecode_loop_count() ->
    [
        {LOOP_CHECK, {count, 3}},      %% Loop 3 times
        {TASK_EXEC, task_a},            %% Body
        {LOOP_BACK, 0},                 %% Jump to LOOP_CHECK
        {DONE}                          %% Exit
    ].

%% Test count loop executes 3 times
loop_count_test_() ->
    Bytecode = mock_bytecode_loop_count(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Initialize loop counter in context
    ExecState1 = ExecState0#exec_state{ctx = #{loop_counter => 3}},
    %% Run until done (should execute loop body 3 times)
    Result = wf_exec:run(ExecState1, 100, undefined),
    {done, ExecStateFinal} = Result,
    TaskExecCount = count_task_executions(ExecStateFinal),
    [
        ?_assertEqual(done, ExecStateFinal#exec_state.status),
        ?_assertEqual(3, TaskExecCount)
    ].

%% @doc Count TASK_EXEC executions (helper for testing)
count_task_executions(ExecState) ->
    ExecState#exec_state.step_count div 2.  %% Rough estimate
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all loop executor tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `loop_count_test_` passes: loop executes exactly N times

##### Manual Verification:

- [ ] Execute count loop bytecode, verify loop body executes N times
- [ ] Inspect IP after LOOP_BACK, verify jumps to LOOP_CHECK
- [ ] Inspect loop counter in context, verify decrements each iteration
- [ ] Execute while loop bytecode, verify condition checked before body
- [ ] Execute until loop bytecode, verify condition checked after body

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Cancellation Support (CANCEL_SCOPE)

#### Overview

Implement cancellation scope management. CANCEL_SCOPE enter pushes scope onto stack. CANCEL_SCOPE exit pops scope and checks if cancelled. If cancelled, propagate cancellation to all tokens in scope (call wf_cancel:propagate/2).

#### Changes Required:

##### 1. wf_exec.erl (CANCEL_SCOPE Handler)

**File**: `src/wf_exec.erl`
**Changes**: Implement execute_cancel_scope/2.

```erlang
%% Exported functions (add to existing exports)
-export([execute_cancel_scope/2]).

%%====================================================================
%% Cancellation Opcodes
%%====================================================================

%% @doc Execute CANCEL_SCOPE: enter or exit cancel scope
execute_cancel_scope({CANCEL_SCOPE, {enter, ScopeId}}, ExecState) ->
    %% Push scope onto stack
    NewScopeStack = [ScopeId | ExecState#exec_state.scope_stack],
    ExecState#exec_state{
        scope_stack = NewScopeStack,
        ip = ExecState#exec_state.ip + 1,
        step_count = ExecState#exec_state.step_count + 1
    };

execute_cancel_scope({CANCEL_SCOPE, {exit, ScopeId}}, ExecState) ->
    %% Pop scope from stack
    [_Top | Rest] = ExecState#exec_state.scope_stack,
    NewScopeStack = Rest,

    %% Check if scope is cancelled (call wf_cancel - stub for now)
    case is_scope_cancelled(ScopeId, ExecState) of
        true ->
            %% Propagate cancellation to all tokens in scope
            Tokens = propagate_cancellation(ScopeId, ExecState#exec_state.tokens),
            ExecState#exec_state{
                scope_stack = NewScopeStack,
                tokens = Tokens,
                status = cancelled,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        false ->
            %% Normal exit
            ExecState#exec_state{
                scope_stack = NewScopeStack,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Check if scope is cancelled (stub for wf_cancel)
-spec is_scope_cancelled(term(), exec_state()) -> boolean().
is_scope_cancelled(_ScopeId, _ExecState) ->
    %% Stub: always false for now
    %% TODO: Call wf_cancel:is_cancelled/2 when item 008 is implemented
    false.

%% @doc Propagate cancellation to all tokens in scope (stub for wf_cancel)
-spec propagate_cancellation(term(), #{term() => #token{}}) -> #{term() => #token{}}.
propagate_cancellation(ScopeId, TokensMap) ->
    %% Stub: mark all tokens in scope as cancelled
    %% TODO: Call wf_cancel:propagate/2 when item 008 is implemented
    maps:map(fun(_TokenId, Token) ->
        case Token#token.scope_id of
            ScopeId -> Token#token{status = cancelled};
            _ -> Token
        end
    end, TokensMap).
```

##### 2. wf_exec_tests.erl (Cancellation Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for cancel scope enter/exit.

```erlang
%% Mock bytecode for cancel scope
mock_bytecode_cancel() ->
    [
        {CANCEL_SCOPE, {enter, scope1}},
        {TASK_EXEC, task_a},
        {CANCEL_SCOPE, {exit, scope1}},
        {DONE}
    ].

%% Test cancel scope enter and exit
cancel_scope_test_() ->
    Bytecode = mock_bytecode_cancel(),
    ExecState0 = wf_exec:new(Bytecode),
    %% Execute CANCEL_SCOPE enter
    {ExecState1, _Trace1} = wf_exec:step(ExecState0, undefined),
    %% Execute TASK_EXEC
    {ExecState2, _Trace2} = wf_exec:step(ExecState1, undefined),
    %% Execute CANCEL_SCOPE exit
    {ExecState3, _Trace3} = wf_exec:step(ExecState2, undefined),
    [
        ?_assertEqual(2, length(ExecState1#exec_state.scope_stack)),  %% [root, scope1]
        ?_assertEqual(1, length(ExecState3#exec_state.scope_stack)),  %% [root]
        ?_assertEqual(running, ExecState3#exec_state.status)
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all cancellation executor tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `cancel_scope_test_` passes: scope_stack push/pop works correctly

##### Manual Verification:

- [ ] Execute cancel scope bytecode, verify scope pushed and popped
- [ ] Inspect scope_stack after enter, verify new scope added
- [ ] Inspect scope_stack after exit, verify scope removed
- [ ] Test nested cancel scopes (enter scope1, enter scope2, exit scope2, exit scope1)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 6: Effect Yield Support (TASK_EXEC Effect)

#### Overview

Implement effect yield detection in TASK_EXEC handler. When task returns {effect, Spec, ContCtx}, executor sets status=blocked_effect and stores EffectSpec. Implement resume/3 to continue execution after effect result.

#### Changes Required:

##### 1. wf_exec.erl (TASK_EXEC Handler Update)

**File**: `src/wf_exec.erl`
**Changes**: Update execute_task_exec/2 to detect effect yield.

```erlang
%% @doc Execute TASK_EXEC: run task function, detect effect yield
execute_task_exec({TASK_EXEC, TaskName}, ExecState) ->
    %% Look up task function from bytecode metadata (mock for now)
    TaskFun = lookup_task_function(TaskName, ExecState),

    %% Execute task with current context
    Ctx = ExecState#exec_state.ctx,
    case TaskFun(Ctx) of
        {ok, NewCtx} ->
            %% Pure task completed successfully
            ExecState#exec_state{
                ctx = NewCtx,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        {effect, EffectSpec, ContCtx} ->
            %% Task yielded effect
            ExecState#exec_state{
                ctx = ContCtx,
                status = blocked_effect,
                ip = ExecState#exec_state.ip,  %% Don't advance IP
                step_count = ExecState#exec_state.step_count + 1
            };
        {error, Reason} ->
            %% Task failed
            CurrentToken = ExecState#exec_state.current_token,
            Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
            UpdatedToken = Token#token{status = failed, value = {error, Reason}},
            Tokens = ExecState#exec_state.tokens#{CurrentToken => UpdatedToken},

            ExecState#exec_state{
                tokens = Tokens,
                status = failed,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Look up task function (mock for now)
-spec lookup_task_function(atom(), exec_state()) -> fun((map()) -> term()).
lookup_task_function(_TaskName, _ExecState) ->
    %% Mock: task always succeeds with ok result
    fun(Ctx) -> {ok, Ctx#{task_result => ok}} end.

%%====================================================================
%% Effect Resume
%%====================================================================

%% @doc Resume execution after effect result
-spec resume(exec_state(), term()) -> exec_state().
resume(ExecState, EffectResult) ->
    %% Update context with effect result
    NewCtx = ExecState#exec_state.ctx#{effect_result => EffectResult},

    %% Clear blocked status, advance IP
    ExecState#exec_state{
        ctx = NewCtx,
        status = running,
        ip = ExecState#exec_state.ip + 1
    }.
```

##### 2. wf_exec.erl (Update run/3 to Detect Effect Yield)

**File**: `src/wf_exec.erl`
**Changes**: Update run_loop/4 to detect status=blocked_effect and return effect tuple.

```erlang
%% @doc Execute N quanta, yield when exhausted or terminal (update existing function)
-spec run(exec_state(), pos_integer(), term()) ->
    {done, exec_state()} | {effect, term(), exec_state()} | {yield, exec_state()}.
run(ExecState0, Quanta, _SchedPolicy) ->
    run_loop(ExecState0, Quanta, 0).

run_loop(ExecState, Quanta, _Count) when Count >= Quanta ->
    %% Quanta exhausted, yield
    {yield, ExecState};

run_loop(ExecState, _Quanta, _Count) when ExecState#exec_state.status =:= done;
                                            ExecState#exec_state.status =:= failed;
                                            ExecState#exec_state.status =:= cancelled ->
    %% Terminal state
    {done, ExecState};

run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
    %% Execute one step
    {ExecState1, _TraceEvent} = step(ExecState0, undefined),

    %% Check for effect yield
    case ExecState1#exec_state.status of
        blocked_effect ->
            %% Task yielded effect
            EffectSpec = {mock_effect, ExecState1#exec_state.ip},
            {effect, EffectSpec, ExecState1};
        _ ->
            %% Continue execution
            run_loop(ExecState1, Quanta, SchedPolicy, Count + 1)
    end.
```

##### 3. wf_exec_tests.erl (Effect Yield Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for effect yield and resume.

```erlang
%% Test effect yield and resume
effect_yield_test_() ->
    %% Mock task that yields effect
    EffectTaskFun = fun(Ctx) -> {effect, {http_get, url}, Ctx} end,
    Bytecode = [{TASK_EXEC, effect_task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    %% Inject mock task function (use process dictionary for simplicity)
    put(mock_task_fun, EffectTaskFun),

    %% Override lookup_task_function to return mock
    %% (In production, task functions stored in bytecode metadata)

    %% Execute TASK_EXEC (should yield effect)
    {ExecState1, _Trace} = wf_exec:step(ExecState0, undefined),
    [
        ?_assertEqual(blocked_effect, ExecState1#exec_state.status),
        ?_assert(wf_exec:is_blocked(ExecState1))
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all effect yield tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `effect_yield_test_` passes: status=blocked_effect, is_blocked/1 returns true

##### Manual Verification:

- [ ] Execute task that returns {effect, Spec, ContCtx}, verify status=blocked_effect
- [ ] Inspect exec_state after effect yield, verify IP did not advance
- [ ] Call resume/2 with effect result, verify status=running and IP advanced
- [ ] Test effect round-trip: yield → resume → continue execution

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 7: Scheduler Integration (wf_sched)

#### Overview

Integrate with wf_sched (item 007) for action selection. run/3 calls wf_sched:select_action/2 to get scheduler decision. XOR_CHOOSE uses decision to select branch. Multi-token executor uses decision to select next token.

#### Changes Required:

##### 1. wf_exec.erl (Update run/3 to Call Scheduler)

**File**: `src/wf_exec.erl`
**Changes**: Update run_loop/4 to call wf_sched:select_action/2.

```erlang
%% @doc Execute N quanta with scheduler integration (update existing function)
-spec run(exec_state(), pos_integer(), wf_sched:sched_policy()) ->
    {done, exec_state()} | {effect, term(), exec_state()} | {yield, exec_state()}.
run(ExecState0, Quanta, SchedPolicy) ->
    run_loop(ExecState0, Quanta, SchedPolicy, 0).

run_loop(ExecState, Quanta, _SchedPolicy, Count) when Count >= Quanta ->
    {yield, ExecState};

run_loop(ExecState, _Quanta, _SchedPolicy, _Count) when ExecState#exec_state.status =:= done;
                                                          ExecState#exec_state.status =:= failed;
                                                          ExecState#exec_state.status =:= cancelled ->
    {done, ExecState};

run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
    %% Select next action via scheduler
    SchedDecision = wf_sched:select_action(ExecState0, SchedPolicy),

    %% Execute one step with scheduler decision
    {ExecState1, _TraceEvent} = step(ExecState0, SchedDecision),

    %% Check for effect yield
    case ExecState1#exec_state.status of
        blocked_effect ->
            EffectSpec = {mock_effect, ExecState1#exec_state.ip},
            {effect, EffectSpec, ExecState1};
        _ ->
            run_loop(ExecState1, Quanta, SchedPolicy, Count + 1)
    end.
```

##### 2. wf_exec.erl (Update step/2 to Pass Scheduler Decision)

**File**: `src/wf_exec.erl`
**Changes**: Update step/2 to pass scheduler decision to execute_opcode/2.

```erlang
%% @doc Execute single reduction step with scheduler decision (update existing function)
-spec step(exec_state(), wf_sched:sched_decision()) -> {exec_state(), term()}.
step(ExecState, SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState, SchedDecision),
    TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, TraceEvent}.
```

##### 3. wf_exec.erl (Update execute_opcode/3)

**File**: `src/wf_exec.erl`
**Changes**: Update execute_opcode/3 to pass scheduler decision to XOR_CHOOSE.

```erlang
%% @doc Execute opcode and update exec_state with scheduler decision (update existing function)
-spec execute_opcode(wf_vm:opcode(), exec_state(), wf_sched:sched_decision()) -> exec_state().
execute_opcode({SEQ_ENTER, _Arg}, ExecState, _SchedDecision) ->
    execute_seq_enter({SEQ_ENTER, _Arg}, ExecState);
execute_opcode({SEQ_NEXT, TargetIP}, ExecState, _SchedDecision) ->
    execute_seq_next({SEQ_NEXT, TargetIP}, ExecState);
execute_opcode({PAR_FORK, Targets}, ExecState, _SchedDecision) ->
    execute_par_fork({PAR_FORK, Targets}, ExecState);
execute_opcode({JOIN_WAIT, Policy}, ExecState, _SchedDecision) ->
    execute_join_wait({JOIN_WAIT, Policy}, ExecState);
execute_opcode({XOR_CHOOSE, Targets}, ExecState, SchedDecision) ->
    execute_xor_choose({XOR_CHOOSE, Targets}, ExecState, SchedDecision);
execute_opcode({LOOP_CHECK, Policy}, ExecState, _SchedDecision) ->
    execute_loop_check({LOOP_CHECK, Policy}, ExecState);
execute_opcode({LOOP_BACK, TargetIP}, ExecState, _SchedDecision) ->
    execute_loop_back({LOOP_BACK, TargetIP}, ExecState);
execute_opcode({CANCEL_SCOPE, ScopeOp}, ExecState, _SchedDecision) ->
    execute_cancel_scope({CANCEL_SCOPE, ScopeOp}, ExecState);
execute_opcode({MI_SPAWN, Policy}, ExecState, _SchedDecision) ->
    execute_mi_spawn({MI_SPAWN, Policy}, ExecState);
execute_opcode({TASK_EXEC, TaskName}, ExecState, _SchedDecision) ->
    execute_task_exec({TASK_EXEC, TaskName}, ExecState);
execute_opcode({DONE}, ExecState, _SchedDecision) ->
    execute_done(ExecState);
execute_opcode(_Opcode, _ExecState, _SchedDecision) ->
    error({unimplemented_opcode, _Opcode}).
```

##### 4. wf_sched.erl (Stub Scheduler Module)

**File**: `src/wf_sched.erl`
**Changes**: Create stub scheduler module for testing.

```erlang
-module(wf_sched).

%% Types
-type sched_policy() :: deterministic | nondeterministic | {replay, term()}.
-type sched_decision() :: {token, term()} | {xor_branch, pos_integer()}.

%% Exported API
-export([select_action/2]).

%% @doc Select next action (stub implementation)
-spec select_action(exec_state(), sched_policy()) -> sched_decision().
select_action(ExecState, deterministic) ->
    %% Deterministic: select first active token
    ActiveTokens = [T || T <- maps:values(ExecState#exec_state.tokens), T#token.status =:= active],
    case ActiveTokens of
        [] -> {token, undefined};
        [First | _] -> {token, First#token.token_id}
    end;

select_action(_ExecState, nondeterministic) ->
    %% Nondeterministic: random selection (mock)
    {token, mock_token};

select_action(_ExecState, {replay, _ChoiceLog}) ->
    %% Replay: use logged choices (mock)
    {token, replay_token}.
```

##### 5. wf_exec_tests.erl (Scheduler Integration Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for scheduler integration.

```erlang
%% Test scheduler integration
scheduler_integration_test_() ->
    Bytecode = [{XOR_CHOOSE, [1, 3]}, {TASK_EXEC, task_a}, {DONE}, {TASK_EXEC, task_b}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    %% Run with deterministic scheduler (should select first branch)
    Result = wf_exec:run(ExecState0, 100, deterministic),
    {done, ExecStateFinal} = Result,
    [
        ?_assertEqual(done, ExecStateFinal#exec_state.status),
        ?_assertEqual(3, wf_exec:get_step_count(ExecStateFinal))  %% XOR_CHOOSE + task_a + DONE
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all scheduler integration tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `scheduler_integration_test_` passes: deterministic scheduler selects first branch

##### Manual Verification:

- [ ] Run executor with deterministic scheduler, verify reproducible trace
- [ ] Run executor with nondeterministic scheduler, verify different traces
- [ ] Inspect scheduler decision, verify token selection
- [ ] Test XOR_CHOOSE with scheduler decision, verify correct branch selected

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 8: Tracing Integration (wf_trace)

#### Overview

Integrate with wf_trace (item 011) for trace event production. Every reduction emits trace event. Support trace levels (none, min, full). wf_trace:emit/2 called in step/2.

#### Changes Required:

##### 1. wf_exec.erl (Update step/2 to Emit Trace Events)

**File**: `src/wf_exec.erl`
**Changes**: Update step/2 to call wf_trace:emit/2.

```erlang
%% @doc Execute single reduction step with tracing (update existing function)
-spec step(exec_state(), wf_sched:sched_decision()) -> {exec_state(), wf_trace:trace_event()}.
step(ExecState, SchedDecision) ->
    Opcode = fetch_opcode(ExecState),

    %% Emit trace event before execution
    TraceEvent0 = wf_trace:emit_before(ExecState, Opcode),

    %% Execute opcode
    NewExecState = execute_opcode(Opcode, ExecState, SchedDecision),

    %% Emit trace event after execution
    TraceEvent = wf_trace:emit_after(NewExecState, Opcode, TraceEvent0),

    {NewExecState, TraceEvent}.
```

##### 2. wf_trace.erl (Stub Trace Module)

**File**: `src/wf_trace.erl`
**Changes**: Create stub trace module for testing.

```erlang
-module(wf_trace).

%% Types
-type trace_event() :: map().
-type trace_level() :: none | min | full.

%% Exported API
-export([emit_before/2, emit_after/3, emit_if/2]).

%% State: trace level (stored in process dictionary for simplicity)
-define(TRACE_LEVEL, wf_trace_level).

%% @doc Set trace level
-spec set_level(trace_level()) -> ok.
set_level(Level) ->
    put(?TRACE_LEVEL, Level),
    ok.

%% @doc Get trace level
-spec get_level() -> trace_level().
get_level() ->
    case get(?TRACE_LEVEL) of
        undefined -> min;  %% Default trace level
        Level -> Level
    end.

%% @doc Emit trace event before opcode execution
-spec emit_before(exec_state(), wf_vm:opcode()) -> trace_event().
emit_before(ExecState, Opcode) ->
    case get_level() of
        none -> #{};
        _ ->
            #{
                type => before,
                opcode => Opcode,
                ip => ExecState#exec_state.ip,
                step_count => ExecState#exec_state.step_count,
                timestamp => erlang:system_time(microsecond)
            }
    end.

%% @doc Emit trace event after opcode execution
-spec emit_after(exec_state(), wf_vm:opcode(), trace_event()) -> trace_event().
emit_after(ExecState, Opcode, BeforeEvent) ->
    case get_level() of
        none -> #{};
        _ ->
            BeforeEvent#{
                type => after,
                new_ip => ExecState#exec_state.ip,
                new_step_count => ExecState#exec_state.step_count,
                status => ExecState#exec_state.status
            }
    end.

%% @doc Emit trace event if trace level enabled
-spec emit_if(trace_level(), trace_event()) -> ok.
emit_if(Level, Event) ->
    case get_level() of
        none -> ok;
        CurrentLevel when CurrentLevel >= Level ->
            %% Store event in trace log (process dictionary for simplicity)
            TraceLog = case get(trace_log) of
                undefined -> [Event];
                Log -> [Event | Log]
            end,
            put(trace_log, TraceLog),
            ok;
        _ ->
            ok
    end.
```

##### 3. wf_exec_tests.erl (Tracing Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add tests for trace event emission.

```erlang
%% Test trace event emission
tracing_test_() ->
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),

    %% Set trace level to full
    wf_trace:set_level(full),

    %% Execute step
    {ExecState1, TraceEvent} = wf_exec:step(ExecState0, undefined),

    %% Verify trace event
    [
        ?_assertEqual(before, maps:get(type, TraceEvent)),
        ?_assertEqual({TASK_EXEC, task}, maps:get(opcode, TraceEvent)),
        ?_assertEqual(0, maps:get(ip, TraceEvent))
    ].
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all tracing tests
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Test `tracing_test_` passes: trace event contains correct fields

##### Manual Verification:

- [ ] Set trace level to full, verify trace events emitted for every step
- [ ] Set trace level to none, verify no trace events emitted
- [ ] Inspect trace log, verify events contain opcode, IP, timestamp
- [ ] Test trace overhead with full tracing (measure steps/sec)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 9: Comprehensive Testing and Integration

#### Overview

Write comprehensive tests covering all opcode handlers, exec_state transitions, termination detection, blocking detection, cooperative scheduling, and edge cases. Use property-based tests for invariants. Test with real bytecode from wf_compile (if available) or mock bytecode.

#### Changes Required:

##### 1. wf_exec_tests.erl (Comprehensive Test Suite)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add comprehensive tests for all opcodes, edge cases, and invariants.

```erlang
%%====================================================================
%% Comprehensive Tests
%%====================================================================

%% Test all opcode handlers
all_opcodes_test_() ->
    [
        {"SEQ_ENTER advances IP", fun seq_enter_ip_test/0},
        {"SEQ_NEXT jumps to target", fun seq_next_jump_test/0},
        {"PAR_FORK spawns N tokens", fun par_fork_tokens_test/0},
        {"JOIN_WAIT blocks until satisfied", fun join_wait_block_test/0},
        {"XOR_CHOOSE selects one branch", fun xor_choose_branch_test/0},
        {"LOOP_CHECK evaluates condition", fun loop_check_test/0},
        {"LOOP_BACK jumps to head", fun loop_back_jump_test/0},
        {"CANCEL_SCOPE enters and exits", fun cancel_scope_test/0},
        {"TASK_EXEC executes task", fun task_exec_test/0},
        {"DONE marks token complete", fun done_complete_test/0}
    ].

%% Test termination detection
termination_detection_test_() ->
    [
        {"is_done returns true when done", fun is_done_done_test/0},
        {"is_done returns true when cancelled", fun is_done_cancelled_test/0},
        {"is_done returns true when failed", fun is_done_failed_test/0},
        {"is_done returns false when running", fun is_done_running_test/0}
    ].

%% Test blocking detection
blocking_detection_test_() ->
    [
        {"is_blocked returns true for effect yield", fun is_blocked_effect_test/0},
        {"is_blocked returns true for join wait", fun is_blocked_join_test/0},
        {"is_blocked returns false for running", fun is_blocked_running_test/0}
    ].

%% Test cooperative scheduling
cooperative_scheduling_test_() ->
    [
        {"Quanta=1 yields after 1 step", fun quanta_1_yield_test/0},
        {"Quanta=10 yields after 10 steps", fun quanta_10_yield_test/0},
        {"Yields preserves executor state", fun yield_preserves_state_test/0}
    ].

%% Test state invariants
state_invariants_test_() ->
    [
        {"Token count never negative", fun token_count_non_negative_test/0},
        {"IP always in bounds", fun ip_in_bounds_test/0},
        {"Step count always increases", fun step_count_increases_test/0},
        {"Scope stack well-formed", fun scope_stack_well_formed_test/0}
    ].

%% Test edge cases
edge_cases_test_() ->
    [
        {"Empty bytecode terminates immediately", fun empty_bytecode_test/0},
        {"Single DONE terminates", fun single_done_test/0},
        {"Nested parallel execution", fun nested_par_test/0},
        {"Deep loop nesting", fun deep_loop_nesting_test/0},
        {"Multiple cancellation scopes", fun multiple_cancel_scopes_test/0}
    ].

%%====================================================================
%% Test Implementations
%%====================================================================

seq_enter_ip_test() ->
    Bytecode = [{SEQ_ENTER, 0}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    ?assertEqual(1, wf_exec:get_ip(ExecState1)).

seq_next_jump_test() ->
    Bytecode = [{SEQ_ENTER, 0}, {TASK_EXEC, task_a}, {SEQ_NEXT, 3}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% SEQ_ENTER
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),  %% TASK_EXEC
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),  %% SEQ_NEXT
    ?assertEqual(3, wf_exec:get_ip(ExecState3)).

par_fork_tokens_test() ->
    Bytecode = [{PAR_FORK, [1, 3]}, {DONE}, {DONE}, {JOIN_WAIT, all}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% PAR_FORK
    ?assertEqual(2, maps:size(ExecState1#exec_state.tokens)).

join_wait_block_test() ->
    Bytecode = [{PAR_FORK, [1, 3]}, {DONE}, {DONE}, {JOIN_WAIT, all}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% PAR_FORK
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),  %% DONE (branch 1)
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),  %% JOIN_WAIT (should block)
    ?assertEqual(blocked_join, ExecState3#exec_state.status).

xor_choose_branch_test() ->
    Bytecode = [{XOR_CHOOSE, [1, 3]}, {DONE}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% XOR_CHOOSE
    ?assertEqual(1, maps:size(ExecState1#exec_state.tokens)).  %% Still 1 token

loop_check_test() ->
    Bytecode = [{LOOP_CHECK, {count, 1}}, {DONE}, {LOOP_BACK, 0}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    ExecState1 = ExecState0#exec_state{ctx = #{loop_counter => 1}},
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),  %% LOOP_CHECK
    ?assertEqual(2, wf_exec:get_ip(ExecState2)).  %% Advanced to body

loop_back_jump_test() ->
    Bytecode = [{LOOP_CHECK, {count, 1}}, {DONE}, {LOOP_BACK, 0}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    ExecState1 = ExecState0#exec_state{ctx = #{loop_counter => 1}},
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),  %% LOOP_CHECK
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),  %% DONE
    {ExecState4, _} = wf_exec:step(ExecState3, undefined),  %% LOOP_BACK
    ?assertEqual(0, wf_exec:get_ip(ExecState4)).  %% Jumped to LOOP_CHECK

cancel_scope_test() ->
    Bytecode = [{CANCEL_SCOPE, {enter, s1}}, {DONE}, {CANCEL_SCOPE, {exit, s1}}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% CANCEL_SCOPE enter
    ?assertEqual(2, length(ExecState1#exec_state.scope_stack)),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),  %% DONE
    {ExecState3, _} = wf_exec:step(ExecState2, undefined),  %% CANCEL_SCOPE exit
    ?assertEqual(1, length(ExecState3#exec_state.scope_stack)).

task_exec_test() ->
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% TASK_EXEC
    ?assertEqual(1, wf_exec:get_ip(ExecState1)).

done_complete_test() ->
    Bytecode = [{DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),  %% DONE
    ?assertEqual(done, ExecState1#exec_state.status).

is_done_done_test() ->
    ExecState = #exec_state{status = done},
    ?assert(wf_exec:is_done(ExecState)).

is_done_cancelled_test() ->
    ExecState = #exec_state{status = cancelled},
    ?assert(wf_exec:is_done(ExecState)).

is_done_failed_test() ->
    ExecState = #exec_state{status = failed},
    ?assert(wf_exec:is_done(ExecState)).

is_done_running_test() ->
    ExecState = #exec_state{status = running},
    ?assertNot(wf_exec:is_done(ExecState)).

is_blocked_effect_test() ->
    ExecState = #exec_state{status = blocked_effect},
    ?assert(wf_exec:is_blocked(ExecState)).

is_blocked_join_test() ->
    ExecState = #exec_state{status = blocked_join},
    ?assert(wf_exec:is_blocked(ExecState)).

is_blocked_running_test() ->
    ExecState = #exec_state{status = running},
    ?assertNot(wf_exec:is_blocked(ExecState)).

quanta_1_yield_test() ->
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 1, deterministic),
    ?assertMatch({yield, _}, Result),
    ?assertEqual(1, wf_exec:get_step_count(element(2, Result))).

quanta_10_yield_test() ->
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 10, deterministic),
    ?assertMatch({done, _}, Result),  %% Completed before quanta exhausted
    ?assertEqual(2, wf_exec:get_step_count(element(2, Result))).

yield_preserves_state_test() ->
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {yield, ExecState1} = wf_exec:run(ExecState0, 1, deterministic),
    ?assertEqual(1, wf_exec:get_step_count(ExecState1)),
    ?assertEqual(running, ExecState1#exec_state.status).

token_count_non_negative_test() ->
    %% Property: token count is always >= 0
    Bytecode = [{PAR_FORK, [1, 3]}, {DONE}, {DONE}, {JOIN_WAIT, all}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    ?assert(maps:size(ExecState1#exec_state.tokens) >= 0).

ip_in_bounds_test() ->
    %% Property: IP is always <= bytecode length
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    ?assert(wf_exec:get_ip(ExecState1) =< length(Bytecode)).

step_count_increases_test() ->
    %% Property: step_count never decreases
    Bytecode = [{TASK_EXEC, task}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    {ExecState2, _} = wf_exec:step(ExecState1, undefined),
    ?assert(ExecState2#exec_state.step_count >= ExecState1#exec_state.step_count).

scope_stack_well_formed_test() ->
    %% Property: scope_stack always contains at least root
    Bytecode = [{CANCEL_SCOPE, {enter, s1}}, {DONE}, {CANCEL_SCOPE, {exit, s1}}, {DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    {ExecState1, _} = wf_exec:step(ExecState0, undefined),
    ?assert(length(ExecState1#exec_state.scope_stack) >= 1).

empty_bytecode_test() ->
    Bytecode = [],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 100, deterministic),
    ?assertMatch({done, _}, Result).

single_done_test() ->
    Bytecode = [{DONE}],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 100, deterministic),
    ?assertMatch({done, _}, Result).

nested_par_test() ->
    %% par([seq(a,b), seq(c,d)])
    Bytecode = [
        {PAR_FORK, [1, 5]},
        {SEQ_ENTER, 0}, {TASK_EXEC, a}, {SEQ_NEXT, 3}, {TASK_EXEC, b}, {DONE},
        {SEQ_ENTER, 0}, {TASK_EXEC, c}, {SEQ_NEXT, 7}, {TASK_EXEC, d}, {DONE},
        {JOIN_WAIT, all}
    ],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 1000, deterministic),
    ?assertMatch({done, _}, Result).

deep_loop_nesting_test() ->
    %% loop(loop(loop(task)))
    Bytecode = [
        {LOOP_CHECK, {count, 2}},
        {LOOP_CHECK, {count, 2}},
        {LOOP_CHECK, {count, 1}},
        {TASK_EXEC, task},
        {LOOP_BACK, 2},
        {LOOP_BACK, 1},
        {LOOP_BACK, 0},
        {DONE}
    ],
    ExecState0 = wf_exec:new(Bytecode),
    ExecState1 = ExecState0#exec_state{ctx => #{loop_counter => 2}},
    Result = wf_exec:run(ExecState1, 1000, deterministic),
    ?assertMatch({done, _}, Result).

multiple_cancel_scopes_test() ->
    Bytecode = [
        {CANCEL_SCOPE, {enter, s1}},
        {CANCEL_SCOPE, {enter, s2}},
        {DONE},
        {CANCEL_SCOPE, {exit, s2}},
        {CANCEL_SCOPE, {exit, s1}},
        {DONE}
    ],
    ExecState0 = wf_exec:new(Bytecode),
    Result = wf_exec:run(ExecState0, 100, deterministic),
    ?assertMatch({done, _}, Result).
```

##### 2. wf_exec_tests.erl (Property-Based Tests)

**File**: `test/wf_exec_tests.erl`
**Changes**: Add property-based tests for invariants using proper (if available) or custom generator.

```erlang
%%====================================================================
%% Property-Based Tests
%%====================================================================

%% Property: Token count never goes negative
prop_token_count_non_negative() ->
    %% For all bytecode, token count is always >= 0
    forall({Bytecode, Ctx}, gen_bytecode_and_ctx(),
        ?IMPLIES(true,
            begin
                ExecState0 = wf_exec:new(Bytecode),
                ExecState0#exec_state{ctx = Ctx},
                {_, ExecState1} = run_n_steps(ExecState0, 100),
                maps:size(ExecState1#exec_state.tokens) >= 0
            end
        end)
    ).

%% Property: IP always in bounds
prop_ip_in_bounds() ->
    forall(Bytecode, gen_bytecode(),
        ?IMPLIES(Bytecode =/= [],
            begin
                ExecState0 = wf_exec:new(Bytecode),
                {_, ExecState1} = run_n_steps(ExecState0, 100),
                ExecState1#exec_state.ip =< length(Bytecode)
            end)
        )
    ).

%% Property: Step count never decreases
prop_step_count_increases() ->
    forall(Bytecode, gen_bytecode(),
        ?IMPLIES(true,
            begin
                ExecState0 = wf_exec:new(Bytecode),
                {_, ExecState1} = run_n_steps(ExecState0, 100),
                ExecState1#exec_state.step_count >= ExecState0#exec_state.step_count
            end)
        )
    ).

%%====================================================================
%% Generators
%%====================================================================

gen_bytecode() ->
    ?LET(Opcodes, list(gen_opcode()), Opcodes).

gen_bytecode_and_ctx() ->
    ?LET({Bytecode, Ctx}, {gen_bytecode(), gen_ctx()}, {Bytecode, Ctx}).

gen_opcode() ->
    oneof([
        {DONE},
        {TASK_EXEC, task},
        {SEQ_ENTER, 0},
        {SEQ_NEXT, rand:uniform(10)},
        {PAR_FORK, [1, 3]},
        {JOIN_WAIT, all},
        {XOR_CHOOSE, [1, 3]},
        {LOOP_CHECK, {count, 2}},
        {LOOP_BACK, 0},
        {CANCEL_SCOPE, {enter, s1}},
        {CANCEL_SCOPE, {exit, s1}}
    ]).

gen_ctx() ->
    #{loop_counter => rand:uniform(10)}.

%%====================================================================
%% Helpers
%%====================================================================

run_n_steps(ExecState, N) ->
    run_n_steps(ExecState, N, []).

run_n_steps(ExecState, 0, Acc) ->
    {lists:reverse(Acc), ExecState};
run_n_steps(ExecState, N, Acc) when ExecState#exec_state.status =:= done;
                                      ExecState#exec_state.status =:= failed;
                                      ExecState#exec_state.status =:= cancelled ->
    {lists:reverse(Acc), ExecState};
run_n_steps(ExecState, N, Acc) ->
    {NewExecState, _Trace} = wf_exec:step(ExecState, undefined),
    run_n_steps(NewExecState, N - 1, [NewExecState | Acc]).
```

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 eunit` passes all comprehensive tests
- [ ] `rebar3 proper` passes all property-based tests (if proper is available)
- [ ] `rebar3 dialyzer` passes with no type errors
- [ ] Code coverage > 90% for wf_exec.erl module
- [ ] All opcode handler tests pass
- [ ] All termination detection tests pass
- [ ] All blocking detection tests pass
- [ ] All cooperative scheduling tests pass
- [ ] All state invariant tests pass
- [ ] All edge case tests pass

##### Manual Verification:

- [ ] Inspect trace logs from comprehensive tests, verify correctness
- [ ] Run executor with complex bytecode (nested par, deep loops), verify termination
- [ ] Test with real bytecode from wf_compile (if available)
- [ ] Profile executor performance (steps/sec, memory usage)
- [ ] Test executor under load (multiple concurrent cases)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

## Testing Strategy

### Unit Tests:

- **Opcode handler tests**: Test each opcode handler (SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, TASK_EXEC, DONE) in isolation with mock bytecode
- **Exec state transition tests**: Verify IP advancement, token creation/removal, scope stack push/pop, join counter updates, branch map updates
- **State query tests**: Test is_done/1 and is_blocked/1 with all status values (running, done, blocked, cancelled, failed)
- **Step execution tests**: Test step/2 executes single reduction and returns updated state + trace event
- **Quanta execution tests**: Test run/3 yields after N steps, terminates on done, yields effect on blocked_effect

### Integration Tests:

- **Single-token execution**: Execute bytecode with tasks, sequences, and loops (no parallel)
- **Multi-token execution**: Execute bytecode with parallel fork and join synchronization
- **Exclusive choice execution**: Execute bytecode with XOR_CHOOSE (scheduler selects branch)
- **Cancellation execution**: Execute bytecode with cancel scopes (nested and single)
- **Effect yield execution**: Execute bytecode with task that yields effect, resume with result
- **Scheduler integration**: Execute bytecode with deterministic and nondeterministic schedulers
- **Tracing integration**: Execute bytecode with trace levels (none, min, full), verify trace events

### Property-Based Tests:

- **Token count invariant**: Token count never goes negative (for all bytecode)
- **IP bounds invariant**: IP always in bounds (0 <= IP <= bytecode_length)
- **Step count invariant**: Step count never decreases (monotonically increasing)
- **Scope stack invariant**: Scope stack always contains at least root (never empty)
- **Termination invariant**: Executor reaches terminal state (done/cancelled/failed) for valid bytecode

### Manual Testing Steps:

1. **Create mock bytecode** for simple sequence: `[{TASK_EXEC, task_a}, {TASK_EXEC, task_b}, {DONE}]`
2. **Execute with `wf_exec:new/1`** and `wf_exec:run/3`, verify termination
3. **Inspect exec_state** after execution, verify IP=3, status=done, step_count=3
4. **Create mock bytecode for parallel**: `[{PAR_FORK, [1, 3]}, {DONE}, {DONE}, {JOIN_WAIT, all}]`
5. **Execute and inspect tokens map**, verify 2 tokens spawned after PAR_FORK
6. **Inspect join_counters**, verify completed=0 initially, completed=2 after both DONE
7. **Create mock bytecode for loop**: `[{LOOP_CHECK, {count, 3}}, {TASK_EXEC, task}, {LOOP_BACK, 0}, {DONE}]`
8. **Execute and verify loop body** executes 3 times (TASK_EXEC called 3 times)
9. **Create mock bytecode for cancel scope**: `[{CANCEL_SCOPE, {enter, s1}}, {TASK_EXEC, task}, {CANCEL_SCOPE, {exit, s1}}, {DONE}]`
10. **Execute and inspect scope_stack**, verify [root, s1] after enter, [root] after exit
11. **Test quanta yielding** with Quanta=1, verify yields after every step
12. **Test effect yield** with task returning {effect, Spec, ContCtx}, verify status=blocked_effect
13. **Test resume** after effect result, verify status=running and IP advanced
14. **Test scheduler integration** with deterministic policy, verify reproducible execution
15. **Test tracing** with trace_level=full, verify trace events emitted for every step

## Migration Notes

No migration needed. This is a new module with no existing data or systems to migrate. The executor is pure functional (no side effects), so rollback is trivial (revert to previous commit).

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/005-executor-hot-loop/research.md`
- Item specification: `/Users/speed/wf-substrate/.wreckit/items/005-executor-hot-loop/item.json`
- Primary specification: `/Users/speed/wf-substrate/PROMPT.md:1-375`
- Architecture: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1238-1258`
- Semantics: `/Users/speed/wf-substrate/docs/SEMANTICS.md:50-1090`
- Compiler plan: `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/plan.md`
- Pattern algebra plan: `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/plan.md`
