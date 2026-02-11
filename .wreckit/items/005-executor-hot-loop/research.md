# Research: Implement bytecode executor hot loop

**Date**: 2025-01-10
**Item**: 005-executor-hot-loop

## Research Question

Implement wf_exec.erl: the bytecode reducer/executor. Core is a tight loop over compiled wf_bc() bytecode with an explicit exec_state record containing: instruction pointer (IP), branch tracking (active branches map), join counters (per join point), scope stack (for cancel regions), token set, and reduction counter.

The step/2 function executes a single instruction and returns the updated exec_state. The run/3 function steps in configurable quanta (N reductions per tick) then yields {yield, State} or {done, State} or {effect, EffectSpec, Continuation, State}. This enables cooperative scheduling — the executor never blocks indefinitely.

No 'case NodeType of' dispatch — all branching is resolved by the bytecode. The executor is a flat instruction-pointer-driven machine. Each opcode handler is a clause in a single dispatch function. Handle PAR_FORK by spawning branch entries in the branch map, JOIN_WAIT by checking/decrementing counters, XOR_CHOOSE by jumping to selected branch, CANCEL_SCOPE by pushing/popping the scope stack.

Export: new/1 (from wf_bc()), step/2, run/3, is_done/1, is_blocked/1.

## Summary

This task involves implementing the bytecode executor (`wf_exec.erl`) that executes compiled workflow bytecode in a tight dispatch loop. The executor is the runtime heart of the workflow substrate, consuming bytecode produced by `wf_compile` (item 004) and managing execution state explicitly rather than interpreting the AST.

The executor must maintain an `exec_state` record with all execution context: instruction pointer (IP), bytecode, user context, active tokens, branch map (for parallel execution), join counters (for synchronization), scope stack (for cancellation), and reduction counter. The `step/2` function executes one opcode and returns updated state. The `run/3` function executes N reductions (quanta) and yields control, enabling cooperative scheduling without blocking.

**Critical Design Constraint**: No per-step "case NodeType of" dispatch. All structural decisions are resolved at compile time (item 004). The executor is a flat instruction-pointer-driven machine where each opcode handler is a clause in `execute_opcode/2`. This satisfies PROMPT.md:23-27 requirement to avoid AST interpretation at runtime.

**Key Architectural Context** (from ARCHITECTURE.md and SEMANTICS.md):
- This is **item 005** in the implementation sequence
- Consumes bytecode from `wf_compile` (item 004)
- Depends on `wf_vm` types (wf_bc(), opcode())
- Coordinates with `wf_sched` (scheduler policies), `wf_cancel` (cancellation), `wf_mi` (multiple instances), `wf_effect` (effect boundary), `wf_trace` (observability)
- Executor produces trace events for replay (item 011)
- Execution semantics defined in SEMANTICS.md as small-step reduction rules

## Current State Analysis

### Existing Implementation

**Current State**: No implementation exists yet. The project has:
- Rebar3 scaffold (item 001) with empty app structure
- Complete architecture documentation (ARCHITECTURE.md)
- Complete operational semantics (SEMANTICS.md)
- Item 002 (wf_term) is in "researched" state - not implemented
- Item 004 (wf_compile) is in "researched" state - not implemented

**Blocking Dependencies**:
1. **wf_term.erl** (item 002) - Must exist for wf_term() type (though executor doesn't directly use it)
2. **wf_compile.erl** (item 004) - Must produce bytecode for executor to consume
3. **wf_vm.erl** (item 004) - Must define wf_bc() and opcode() types

**Current Files** (from item 001):
- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file
- `/Users/speed/wf-substrate/src/wf_substrate_app.erl:1-43` - Application callback
- `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:1-51` - Empty supervisor
- `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder API

**Planned But Unimplemented** (dependencies for this item):
- `wf_compile.erl` - Bytecode compiler (item 004)
- `wf_vm.erl` - Bytecode type definitions (item 004)
- `wf_sched.erl` - Scheduler policies (item 007)
- `wf_cancel.erl` - Cancellation semantics (item 008)
- `wf_state.erl` - State store (item 006)
- `wf_effect.erl` - Effect boundary (item 010)
- `wf_trace.erl` - Tracing (item 011)

**Implementation Status**: This is **item 005**, but it depends on items 002 and 004. However, the executor can be implemented incrementally using mock bytecode for testing. The core dispatch loop and exec_state management are independent of other modules.

### Specification Context

The authoritative specification is in `/Users/speed/wf-substrate/PROMPT.md`:

**Lines 162-166**: Bytecode VM Strategy (chosen approach)
```
Strategy S1: Bytecode VM
- Compile wf_term() to wf_bc() list of opcodes
- Opcodes are pattern primitives (SEQ_ENTER, PAR_FORK, JOIN_WAIT, etc.)
- wf_exec runs a tight loop executing opcodes and mutating exec_state
- No per-step AST dispatch
```

**Lines 23-27**: The Critical Constraint
> "Runtime must not repeatedly interpret node types in a slow dispatch loop. Runtime must operate on a compiled executable form (bytecode + direct reducer OR compiled continuation network). Avoid per-step 'case NodeType of ...' in the hot loop."

**Lines 50-55**: Executor Module Specification
> "wf_exec.erl: reducer/executor (hot loop)"

**From ARCHITECTURE.md:1238-1258**: Executor Module Responsibilities
- Export: `new/1` (create executor from bytecode), `step/2` (execute single reduction), `run/3` (execute N quanta), `is_done/1`, `is_blocked/1`
- Dependencies: wf_vm, wf_state, wf_sched, wf_cancel, wf_mi, wf_effect, wf_trace
- Coordinates: state persistence, action selection, cancellation, instance management, effect handling, tracing

**From SEMANTICS.md:50-159**: Execution State Definition
The execution state σ is defined as:
```
σ = (ctx, tokens, scopes, ip, branch_map, join_counters, step_count)
```

Components:
- **ctx**: User-provided context map (opaque to engine)
- **tokens**: Set of active tokens (logical threads of execution)
- **scopes**: Map of scope_id → scope_metadata (cancel scopes with nesting)
- **ip**: Instruction pointer (index into bytecode)
- **branch_map**: Map of branch_id → branch_info (active parallel branches)
- **join_counters**: Map of join_id → join_counter (join synchronization state)
- **step_count**: Number of reductions executed

**From SEMANTICS.md:238-1090**: Opcode Semantics
Each opcode has defined reduction rules that executor must implement:
- Lines 238-306: TASK_EXEC (pure task, effect-yielding task, error handling)
- Lines 307-375: SEQ_ENTER, SEQ_NEXT (sequential composition)
- Lines 376-448: PAR_FORK, PAR_COMPLETE, PAR_JOIN (parallel fork)
- Lines 449-480: XOR_CHOOSE (exclusive choice)
- Lines 481-591: JOIN_WAIT with policies (all, first_n, n_of_m, first_complete, sync_merge)
- Lines 592-695: LOOP_CHECK, LOOP_BACK (structured loop)
- Lines 697-748: DEFER_WAIT, DEFER_SELECT (deferred evaluation)
- Lines 749-861: CANCEL_SCOPE (enter, propagate, exit)
- Lines 862-968: MI_SPAWN, MI_COLLECT, MI_JOIN (multiple instances)

## Key Files

### Specification Files

- `/Users/speed/wf-substrate/PROMPT.md:1-375` - **Primary specification**. Key sections:
  - Lines 23-27: No per-step AST dispatch requirement
  - Lines 50-55: wf_exec.erl module specification
  - Lines 162-166: Bytecode VM strategy
  - Lines 97-136: Pattern algebra definition (for context)

- `/Users/speed/wf-substrate/.wreckit/items/005-executor-hot-loop/item.json:1-14` - **This item's specification**. Defines:
  - Module: `wf_exec.erl`
  - Core: tight loop over compiled wf_bc() bytecode
  - exec_state record: IP, branch tracking, join counters, scope stack, token set, reduction counter
  - Functions: new/1, step/2, run/3, is_done/1, is_blocked/1
  - Dispatch: No "case NodeType of", flat instruction-pointer-driven machine
  - Opcodes: PAR_FORK (spawn branches), JOIN_WAIT (check/decrement counters), XOR_CHOOSE (jump), CANCEL_SCOPE (push/pop stack)

### Documentation Files

- `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1238-1258` - **Executor module specification**. Defines:
  - Lines 1238-1258: Module exports and dependencies
  - Lines 1238-1258: Key functions (new, step, run, is_done, is_blocked)
  - Lines 210-226: exec_state record structure
  - Lines 486-505: Execution flow (wf_exec:run in quanta)

- `/Users/speed/wf-substrate/docs/SEMANTICS.md:50-1090` - **Operational semantics**. Defines:
  - Lines 50-159: Execution state definition (σ components)
  - Lines 162-236: Configuration and reduction rule notation
  - Lines 238-1090: Reduction rules for all 12 opcodes
  - Lines 1088-1348: Examples showing execution traces with IP advancement

### Dependency Files (Not Yet Implemented)

- **Item 004**: `src/wf_compile.erl` - Bytecode compiler (must exist before executor can be tested)
  - Research: `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/research.md:1-1075`
  - Plan: `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/plan.md:1-1541`
  - Produces: wf_bc() bytecode consumed by executor

- **Item 004**: `src/wf_vm.erl` - Bytecode type definitions
  - Defines: wf_bc(), opcode(), join_policy(), loop_policy(), mi_policy()
  - Executor imports these types

- **Item 006**: `src/wf_state.erl` - State store
  - Atomic commit protocol for state mutations
  - Executor may use for persistence (optional for v1)

- **Item 007**: `src/wf_sched.erl` - Scheduler policies
  - select_action/2 for choosing next enabled action
  - Policies: deterministic, nondeterministic, replay

- **Item 008**: `src/wf_cancel.erl` - Cancellation semantics
  - propagate/2 for cancel propagation
  - O(scope size) cancellation algorithm

- **Item 010**: `src/wf_effect.erl` - Effect boundary
  - execute/3 for effect execution
  - cancel/2 for effect cancellation

- **Item 011**: `src/wf_trace.erl` - Tracing
  - emit/2 for trace event production
  - Every reduction should emit trace event

### Files to Create

- `src/wf_exec.erl` - **Bytecode executor** (primary deliverable)
  - `-record(exec_state, {...})` - Execution state record
  - `new/1` - Create executor from bytecode
  - `step/2` - Execute single reduction
  - `run/3` - Execute N quanta
  - `is_done/1` - Check if execution complete
  - `is_blocked/1` - Check if waiting for external event
  - `execute_opcode/2` - Dispatch function (one clause per opcode)
  - Opcode handlers: `execute_seq_enter/2`, `execute_seq_next/2`, `execute_par_fork/2`, etc.

- `test/wf_exec_tests.erl` - **Executor tests**
  - Test each opcode handler
  - Test exec_state transitions
  - Test quanta execution (run/3)
  - Test termination detection (is_done/1)
  - Test blocking detection (is_blocked/1)
  - Test cooperative scheduling (yield after N reductions)

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Applications Needed**:
- `stdlib` - For lists, maps, sets, ordsets, queue (token management)
- `kernel` - For basic types and processes

**Internal Module Dependencies**:
- **wf_vm.erl** (item 004): Executor MUST import `wf_bc()`, `opcode()`, and all policy types
- **wf_compile.erl** (item 004): Executor consumes bytecode produced by compiler (runtime dependency only)
- **wf_sched.erl** (item 007): Executor calls `wf_sched:select_action/2` for action selection (if multiple tokens enabled)
- **wf_cancel.erl** (item 008): Executor calls `wf_cancel:propagate/2` for cancellation (can defer to item 008)
- **wf_effect.erl** (item 010): Executor yields effect specs to effect manager (can defer to item 010)
- **wf_trace.erl** (item 011): Executor emits trace events (can be stub in v1)
- **wf_state.erl** (item 006): Optional for state persistence (v1 can keep state in exec_state record)

**No Circular Dependencies**: Executor is downstream of compiler. No cycles.

### exec_state Record Design

**From ARCHITECTURE.md:210-226** and **SEMANTICS.md:50-80**:

```erlang
-record(exec_state, {
    ip :: non_neg_integer(),              %% Instruction pointer (index into bytecode)
    bytecode :: wf_vm:wf_bc(),            %% Compiled bytecode (immutable reference)
    ctx :: ctx(),                         %% User context (map)
    tokens :: #{token_id() => token()},   %% Active tokens (logical threads)
    branch_map :: #{branch_id() => branch_info()}, %% Parallel branches tracking
    join_counters :: #{join_id() => join_counter()}, %% Join synchronization state
    scope_stack :: [scope_id()],          %% Cancellation scope stack (top is current)
    step_count :: non_neg_integer(),      %% Reduction counter (for stats/limits)
    status :: running | done | blocked | cancelled, %% Execution status
    current_token :: token_id() | undefined %% Token currently being executed
}).

%% Token: Logical thread of execution
-record(token, {
    token_id :: token_id(),
    ip :: non_neg_integer(),              %% Current IP for this token
    scope_id :: scope_id(),               %% Active cancel scope
    value :: term(),                      %% Accumulated result
    status :: active | complete | cancelled
}).

%% Branch info: Parallel branch tracking
-record(branch_info, {
    branch_id :: branch_id(),
    tokens :: [token_id()],               %% Active tokens in branch
    join_id :: join_id(),                 %% Associated join point
    targets :: [non_neg_integer()]        %% IP targets for branches
}).

%% Join counter: Join synchronization state
-record(join_counter, {
    join_id :: join_id(),
    completed :: non_neg_integer(),       %% Number of completed branches
    required :: non_neg_integer(),        %% Required for join policy
    policy :: wf_vm:join_policy(),        %% Join policy
    results :: [term()]                   %% Accumulated results
}).
```

**Key Design Decisions**:

1. **IP is per-exec_state, not per-token**: Single IP for executor, tokens have their own IPs. This is correct because executor is single-threaded (cooperative scheduling). Multiple tokens are managed in tokens map, but executor executes one token at a time.

2. **current_token field**: Tracks which token is currently being executed. Executor selects token, sets current_token, executes until token yields or completes, then selects next token.

3. **status field**: High-level execution status. Alternative is to infer from tokens/state (e.g., tokens empty → done). Explicit status is clearer and enables is_done/1 and is_blocked/1 without complex inferences.

4. **step_count field**: Counts reductions executed. Used for:
   - Statistics (steps/sec metrics)
   - Quanta limiting (yield after N steps)
   - Debugging (trace event sequencing)

### Opcode Dispatch Strategy

**From item.json:6** and **ARCHITECTURE.md:1238-1258**:

**Requirement**: "No 'case NodeType of' dispatch — all branching is resolved by the bytecode. The executor is a flat instruction-pointer-driven machine. Each opcode handler is a clause in a single dispatch function."

**Implementation**:
```erlang
%% Dispatch function (one clause per opcode)
execute_opcode(Opcode, ExecState) ->
    case Opcode of
        {SEQ_ENTER, _Arg} -> execute_seq_enter(Opcode, ExecState);
        {SEQ_NEXT, TargetIP} -> execute_seq_next(Opcode, ExecState);
        {PAR_FORK, Targets} -> execute_par_fork(Opcode, ExecState);
        {JOIN_WAIT, Policy} -> execute_join_wait(Opcode, ExecState);
        {XOR_CHOOSE, Targets} -> execute_xor_choose(Opcode, ExecState);
        {LOOP_BACK, TargetIP} -> execute_loop_back(Opcode, ExecState);
        {LOOP_CHECK, Policy} -> execute_loop_check(Opcode, ExecState);
        {CANCEL_SCOPE, ScopeOp} -> execute_cancel_scope(Opcode, ExecState);
        {MI_SPAWN, Policy} -> execute_mi_spawn(Opcode, ExecState);
        {TASK_EXEC, TaskName} -> execute_task_exec(Opcode, ExecState);
        {DONE} -> execute_done(ExecState)
    end.
```

**Alternative**: Use pattern matching in function heads (more Erlang-ish):
```erlang
execute_opcode({SEQ_ENTER, _Arg}, ExecState) -> execute_seq_enter(Opcode, ExecState);
execute_opcode({SEQ_NEXT, TargetIP}, ExecState) -> execute_seq_next(Opcode, ExecState);
...
```

**Recommendation**: Pattern matching in function heads is more idiomatic Erlang and leverages compiler optimizations. Case statement is acceptable if single function is preferred.

**No AST Dispatch**: Executor never sees wf_term() nodes. Only opcodes. All structural decisions (which branch to take, when to join) are resolved at compile time.

### step/2 Implementation

**From SEMANTICS.md:163-236** (reduction rule notation):

**Purpose**: Execute single reduction step. Fetch opcode at IP, dispatch to handler, update exec_state.

**Signature**:
```erlang
-spec step(exec_state(), wf_sched:sched_decision()) ->
    {exec_state(), wf_trace:trace_event()}.
```

**Algorithm**:
```erlang
step(ExecState, SchedDecision) ->
    %% Fetch current opcode
    Opcode = fetch_opcode(ExecState),

    %% Emit trace event before execution
    TraceEvent0 = wf_trace:emit_before(ExecState, Opcode),

    %% Execute opcode
    NewExecState = execute_opcode(Opcode, ExecState),

    %% Emit trace event after execution
    TraceEvent = wf_trace:emit_after(NewExecState, Opcode, TraceEvent0),

    %% Check termination
    FinalState = check_termination(NewExecState),

    {FinalState, TraceEvent}.
```

**Key Points**:
- Executes ONE opcode only (not quanta)
- Returns updated exec_state and trace event
- Does NOT advance IP automatically (opcode handler advances IP)
- Scheduler decision used for XOR_CHOOSE (which branch to select)

### run/3 Implementation

**From ARCHITECTURE.md:486-505** and **item.json:6**:

**Purpose**: Execute N reductions (quanta) then yield. Enables cooperative scheduling without blocking.

**Signature**:
```erlang
-spec run(exec_state(), pos_integer(), wf_sched:sched_policy()) ->
    {done, exec_state()} |
    {effect, wf_effect:effect_spec(), wf_effect:continuation(), exec_state()} |
    {yield, exec_state()}.
```

**Algorithm**:
```erlang
run(ExecState0, Quanta, SchedPolicy) ->
    run_loop(ExecState0, Quanta, SchedPolicy, 0).

run_loop(ExecState, Quanta, _SchedPolicy, Count) when Count >= Quanta ->
    %% Quanta exhausted, yield
    {yield, ExecState};

run_loop(ExecState, _Quanta, _SchedPolicy, _Count) when ExecState#exec_state.status =:= done ->
    %% Terminal state
    {done, ExecState};

run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
    %% Select next action (token + IP)
    SchedDecision = wf_sched:select_action(ExecState0, SchedPolicy),

    %% Execute one step
    {ExecState1, _TraceEvent} = step(ExecState0, SchedDecision),

    %% Check for effect yield
    case ExecState1#exec_state.status of
        blocked_effect ->
            %% Task yielded effect
            {effect, EffectSpec, Continuation, ExecState1};
        _ ->
            %% Continue execution
            run_loop(ExecState1, Quanta, SchedPolicy, Count + 1)
    end.
```

**Key Points**:
- Executes up to Quanta reductions
- Yields {yield, State} after Quanta exhausted (cooperative scheduling)
- Returns {done, State} if terminal state reached
- Returns {effect, ...} if task yields effect (waits for external operation)
- Never blocks indefinitely (enables fair scheduling)

**Quanta Selection**:
- Default: 100 reductions per tick
- Configurable per case (tradeoff: responsiveness vs throughput)
- High quanta → better throughput, worse responsiveness
- Low quanta → better responsiveness, more overhead

### Opcode Handler Semantics

**From SEMANTICS.md:238-1090**, each opcode handler implements reduction rules:

#### SEQ_ENTER (SEMANTICS.md:311-327)

**Semantics**: Push sequence scope onto scope stack, advance to next instruction.

**Implementation**:
```erlang
execute_seq_enter({SEQ_ENTER, _Arg}, ExecState) ->
    %% Push new scope onto stack
    ScopeStack = ExecState#exec_state.scope_stack,
    NewScopeId = make_scope_id(),
    NewScopeStack = [NewScopeId | ScopeStack],

    %% Advance IP
    NewIP = ExecState#exec_state.ip + 1,

    ExecState#exec_state{
        ip = NewIP,
        scope_stack = NewScopeStack,
        step_count = ExecState#exec_state.step_count + 1
    }.
```

#### SEQ_NEXT (SEMANTICS.md:329-375)

**Semantics**: Left branch complete, advance IP to right branch target.

**Implementation**:
```erlang
execute_seq_next({SEQ_NEXT, TargetIP}, ExecState) ->
    %% Jump to right branch
    ExecState#exec_state{
        ip = TargetIP,
        step_count = ExecState#exec_state.step_count + 1
    }.
```

#### PAR_FORK (SEMANTICS.md:376-448)

**Semantics**: Spawn N tokens, one per branch. Track in branch_map. Set join counter.

**Implementation**:
```erlang
execute_par_fork({PAR_FORK, TargetIPs}, ExecState) ->
    %% Create branch ID and join ID
    BranchId = make_branch_id(),
    JoinId = make_join_id(),
    NumBranches = length(TargetIPs),

    %% Spawn N tokens
    CurrentToken = ExecState#exec_state.current_token,
    ScopeId = get_current_scope(ExecState),
    NewTokens = lists:map(fun(IP) ->
        TokenId = make_token_id(),
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

    ExecState#exec_state{
        ip = NewIP,
        tokens = TokensMapFinal,
        branch_map = BranchMap,
        join_counters = JoinCounters,
        step_count = ExecState#exec_state.step_count + 1,
        current_token = select_next_token(TokensMapFinal)
    }.
```

**Key Points**:
- Original token is removed (replaced by N branch tokens)
- Each branch token has independent IP
- All tokens share same scope_id
- Join counter initialized to 0 completed, N required
- Branch info tracks which tokens belong to which branch

#### JOIN_WAIT (SEMANTICS.md:481-591)

**Semantics**: Check join counter. If satisfied, merge results and continue. Otherwise, block.

**Implementation**:
```erlang
execute_join_wait({JOIN_WAIT, Policy}, ExecState) ->
    %% Find active join (must be exactly one)
    JoinId = find_active_join(ExecState),
    JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),

    case JoinCounter#join_counter.completed >= JoinCounter#join_counter.required of
        true ->
            %% Join satisfied, merge results and continue
            %% Remove join counter and branch map entry
            JoinCounters = maps:remove(JoinId, ExecState#exec_state.join_counters),
            BranchMap = remove_branch(JoinId, ExecState#exec_state.branch_map),

            %% Create continuation token with merged results
            CurrentToken = ExecState#exec_state.current_token,
            ContinuationToken = (maps:get(CurrentToken, ExecState#exec_state.tokens))#token{
                value = merge_results(JoinCounter#join_counter.results, Policy),
                status = active
            },
            Tokens = ExecState#exec_state.tokens#{CurrentToken => ContinuationToken},

            %% Advance IP
            NewIP = ExecState#exec_state.ip + 1,

            ExecState#exec_state{
                ip = NewIP,
                tokens = Tokens,
                join_counters = JoinCounters,
                branch_map = BranchMap,
                step_count = ExecState#exec_state.step_count + 1
            };
        false ->
            %% Join not satisfied, block
            ExecState#exec_state{
                status = blocked_join,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.
```

**Key Points**:
- Blocking is OK (other tokens may continue)
- When join satisfied, merge branch results per policy
- Different policies: all (wait for everyone), first_n (wait for N), first_complete (first wins), sync_merge (merge state)
- Join counter and branch map entry removed on completion

#### XOR_CHOOSE (SEMANTICS.md:449-480)

**Semantics**: Scheduler selects ONE branch. Only selected branch token is created. Other branches never spawned.

**Implementation**:
```erlang
execute_xor_choose({XOR_CHOOSE, TargetIPs}, ExecState, SchedDecision) ->
    %% Scheduler selects which branch
    SelectedIndex = wf_sched:select_xor_branch(TargetIPs, SchedDecision),
    SelectedIP = lists:nth(SelectedIndex, TargetIPs),

    %% Update current token's IP to selected branch
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{ip = SelectedIP},
    Tokens = ExecState#exec_state.tokens#{CurrentToken => UpdatedToken},

    %% Advance IP to selected branch
    ExecState#exec_state{
        ip = SelectedIP,
        tokens = Tokens,
        step_count = ExecState#exec_state.step_count + 1
    }.
```

**Key Points**:
- Only ONE token exists (current token)
- No join needed (only one branch runs)
- No branch tracking (unlike PAR_FORK)
- Scheduler decision determines which branch

#### LOOP_CHECK and LOOP_BACK (SEMANTICS.md:592-695)

**Semantics**: LOOP_CHECK evaluates condition (exit or continue). LOOP_BACK jumps to loop head.

**Implementation**:
```erlang
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
            %% Find exit label (after LOOP_BACK)
            %% Skip to instruction after LOOP_BACK
            %% This requires bytecode inspection or label tracking
            %% Simplified: advance to next instruction (body), LOOP_BACK will jump back
            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

execute_loop_back({LOOP_BACK, TargetIP}, ExecState) ->
    %% Jump back to loop head (LOOP_CHECK)
    ExecState#exec_state{
        ip = TargetIP,
        step_count = ExecState#exec_state.step_count + 1
    }.
```

**Key Point**: Loop condition evaluation depends on policy:
- `{count, N}`: Decrement counter, exit if 0
- `while`: Check condition first, exit if false
- `until`: Execute body, check condition after, exit if true

#### CANCEL_SCOPE (SEMANTICS.md:749-861)

**Semantics**: ENTER pushes scope onto stack. EXIT pops scope. If cancellation active, mark all tokens in scope as cancelled.

**Implementation**:
```erlang
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

    %% Check if scope is cancelled
    case wf_cancel:is_cancelled(ScopeId, ExecState) of
        true ->
            %% Propagate cancellation to all tokens in scope
            Tokens = wf_cancel:propagate_to_scope(ScopeId, ExecState#exec_state.tokens),
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
```

**Key Points**:
- Enter/exit must be paired (compiler ensures this)
- Cancellation propagation is O(scope size)
- Nested scopes: if parent cancelled, all children cancelled

#### TASK_EXEC (SEMANTICS.md:238-306)

**Semantics**: Execute task function. If pure, update context and continue. If effect-yielding, yield to effect manager. If error, mark token as failed.

**Implementation**:
```erlang
execute_task_exec({TASK_EXEC, TaskName}, ExecState) ->
    %% Look up task function from registry (or metadata)
    %% For now, assume task functions are registered in a process dictionary or ETS table
    TaskFun = lookup_task_function(TaskName),

    %% Execute task with current context
    Ctx = ExecState#exec_state.ctx,
    case TaskFun(Ctx) of
        {ok, NewCtx} ->
            %% Pure task completed successfully
            %% Update context and advance IP
            ExecState#exec_state{
                ctx = NewCtx,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        {effect, EffectSpec, ContCtx} ->
            %% Task yielded effect
            %% Store continuation context for resumption
            ExecState#exec_state{
                ctx = ContCtx,
                status = blocked_effect,
                effect_spec = EffectSpec,
                step_count = ExecState#exec_state.step_count + 1
            };
        {error, Reason} ->
            %% Task failed
            %% Mark current token as failed
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
```

**Key Points**:
- Task function lookup: How to map task name → function?
  - Option 1: Task functions stored in bytecode metadata
  - Option 2: Task registry (ETS table)
  - Option 3: Task metadata passed in exec_state
- Effect yield: Set status = blocked_effect, store EffectSpec
- Error handling: Mark token as failed, executor may terminate or invoke error handler

#### DONE (SEMANTICS.md:1088-1090)

**Semantics**: Terminate execution path for current token. Remove token from active set. If no tokens remain, executor is done.

**Implementation**:
```erlang
execute_done(ExecState) ->
    %% Mark current token as complete
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

**Key Points**:
- Token marked complete (not removed from map, for trace/debugging)
- If all tokens complete/inactive, executor status = done
- Otherwise, select next token and continue

### is_done/1 and is_blocked/1

**Purpose**: Query executor state for termination and blocking.

**Implementation**:
```erlang
is_done(ExecState) ->
    ExecState#exec_state.status =:= done orelse
    ExecState#exec_state.status =:= cancelled orelse
    ExecState#exec_state.status =:= failed.

is_blocked(ExecState) ->
    ExecState#exec_state.status =:= blocked_effect orelse
    ExecState#exec_state.status =:= blocked_join orelse
    ExecState#exec_state.status =:= blocked_signal.
```

**Key Points**:
- is_done/1: Terminal states (done, cancelled, failed)
- is_blocked/1: Waiting for external event (effect, join, signal)
- Both used by case runner (gen_statem) to determine next action

### Cooperative Scheduling

**From ARCHITECTURE.md:486-505** and **PROMPT.md:194-197**:

**Purpose**: Executor yields control after N reductions to avoid monopolizing scheduler. Enables fair scheduling of multiple cases.

**Mechanism**:
1. `run/3` executes up to Quanta reductions
2. After Quanta steps, returns `{yield, State}`
3. Case runner (gen_statem) receives yield, reschedules with `state_timeout`
4. Executor resumes with `run/3` again

**Benefits**:
- No single case runs forever (prevents starvation)
- Responsive to signals (cancel, effect_result)
- Fair scheduling across multiple cases

**Quanta Tuning**:
- Default: 100 reductions/tick
- High quanta (1000+): Better throughput, worse latency
- Low quanta (10-50): Better latency, more overhead
- Can be configured per case (from options)

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Dependency on item 004 (wf_compile)** | High | Executor cannot be tested without bytecode. Mitigation: Implement mock bytecode generator for unit tests. Implement wf_compile first (item 004). |
| **Token management complexity** | High | Multiple active tokens with independent IPs are hard to reason about. Mitigation: Start with single-token executor (no parallel), add multi-token support incrementally. Use property-based tests to verify invariants (e.g., "token count never negative"). |
| **Join counter bugs** | High | Join synchronization is subtle (counter increments, policy checks, result collection). Mitigation: Comprehensive unit tests for all join policies. Use model checking (item 013) to verify join semantics. |
| **Cancellation propagation bugs** | High | O(scope size) cancellation must correctly mark all tokens. Mitigation: Use wf_cancel:propagate/2 (item 008) instead of implementing inline. Test deeply nested scopes. |
| **Infinite loops in bytecode** | Medium | Executor could loop forever (e.g., while(true) with no exit). Mitigation: Add step limit to run/3 (max reductions). Timeout in case runner (gen_statem). |
| **Effect yield and resume bugs** | High | Task yields effect, executor must correctly resume with result. Mitigation: Store continuation context in exec_state. Test effect round-trip (yield → resume → continue). |
| **Scheduler integration bugs** | Medium | wf_sched:select_action/2 must select valid action. Mitigation: Validate scheduler output (token must exist, IP must be valid). Test with deterministic scheduler first. |
| **State corruption** | High | exec_state mutations must be atomic (no partial updates). Mitigation: Use pure functional updates (record updates). No in-place mutation. Every step returns new exec_state. |
| **Memory leaks** | Medium | Tokens, branch map, join counters accumulate. Mitigation: Remove completed tokens from map. Remove branch/join entries when join satisfied. Add GC checks in tests. |
| **Trace event overhead** | Medium | Every reduction emits trace event (expensive). Mitigation: Make tracing optional (trace_level = none|min|full). Use wf_trace:emit_if/2. |
| **Task function lookup** | Medium | How to map task name → function? Mitigation: Store task functions in bytecode metadata (item 004). Or use task registry (ETS table). Document clearly. |
| **Opcode handler bugs** | High | 12 opcodes, each with subtle semantics. Mitigation: Unit test each handler in isolation. Use SEMANTICS.md reduction rules as oracle. Compare actual output to expected output. |
| **Blocking detection bugs** | Medium | is_blocked/1 must correctly detect blocking states. Mitigation: Unit test all blocking scenarios (effect yield, join wait, signal wait). Assert is_blocked/1 returns true when expected. |
| **Cooperative scheduling bugs** | Low | Executor might not yield (busy loop). Mitigation: Assert run/3 yields after Quanta steps. Add watchdog timeout in case runner. |
| **Label resolution bugs** | Medium | Bytecode has unresolved labels (compiler bug). Mitigation: validate_bytecode/1 in wf_compile (item 004). Executor can assert all operands are integers (not refs). |

## Recommended Approach

**High-Level Strategy**: Incremental implementation starting with single-token executor (no parallel), then adding multi-token support, join synchronization, and cancellation. Use mock bytecode for testing until wf_compile is implemented. Coordinate with dependent modules (wf_sched, wf_cancel, wf_effect) by defining clear interfaces and stubs.

### Phase 1: Foundation (exec_state record and new/1)

**1. Define exec_state record**:
```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: ctx(),
    tokens :: #{token_id() => token()},
    branch_map :: #{branch_id() => branch_info()},
    join_counters :: #{join_id() => join_counter()},
    scope_stack :: [scope_id()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | cancelled,
    current_token :: token_id() | undefined
}).
```

**2. Define helper records** (token, branch_info, join_counter)

**3. Implement new/1**:
```erlang
-spec new(wf_vm:wf_bc()) -> exec_state().
new(Bytecode) ->
    InitialToken = make_initial_token(),
    #exec_state{
        ip = 0,
        bytecode = Bytecode,
        ctx = #{},
        tokens = #{InitialToken#token.token_id => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [root],
        step_count = 0,
        status = running,
        current_token = InitialToken#token.token_id
    }.
```

**4. Create mock bytecode generator**:
```erlang
mock_bytecode_simple_task() ->
    [{TASK_EXEC, mock_task}, {DONE}].

mock_bytecode_seq() ->
    [{SEQ_ENTER, 0}, {TASK_EXEC, task_a}, {SEQ_NEXT, 3}, {TASK_EXEC, task_b}, {DONE}].
```

**5. Test exec_state creation and IP advancement**

### Phase 2: Single-Token Executor (No Parallel)

**1. Implement fetch_opcode/1**:
```erlang
fetch_opcode(ExecState) ->
    IP = ExecState#exec_state.ip,
    Bytecode = ExecState#exec_state.bytecode,
    case IP < length(Bytecode) of
        true -> lists:nth(IP + 1, Bytecode);  %% IP is 0-indexed
        false -> {DONE}
    end.
```

**2. Implement execute_opcode/2 dispatch**:
```erlang
execute_opcode(Opcode, ExecState) ->
    case Opcode of
        {SEQ_ENTER, _} -> execute_seq_enter(Opcode, ExecState);
        {SEQ_NEXT, TargetIP} -> execute_seq_next(Opcode, ExecState);
        {TASK_EXEC, TaskName} -> execute_task_exec(Opcode, ExecState);
        {DONE} -> execute_done(ExecState)
        %% Other opcodes not yet implemented
    end.
```

**3. Implement simple opcodes**:
- SEQ_ENTER: Push scope, advance IP
- SEQ_NEXT: Jump to target IP
- TASK_EXEC: Execute mock task, advance IP
- DONE: Mark token complete, set status = done

**4. Implement step/2** (single reduction):
```erlang
step(ExecState, _SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),
    TraceEvent = #trace_event{opcode = Opcode, state_before = ExecState, state_after = NewExecState},
    {NewExecState, TraceEvent}.
```

**5. Implement run/3** (quanta execution):
```erlang
run(ExecState0, Quanta, _SchedPolicy) ->
    run_loop(ExecState0, Quanta, 0).

run_loop(ExecState, Quanta, Count) when Count >= Quanta ->
    {yield, ExecState};
run_loop(ExecState, _Quanta, _Count) when ExecState#exec_state.status =:= done ->
    {done, ExecState};
run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
    {ExecState1, _TraceEvent} = step(ExecState0, undefined),
    run_loop(ExecState1, Quanta, SchedPolicy, Count + 1).
```

**6. Implement is_done/1 and is_blocked/1**:
```erlang
is_done(ExecState) -> ExecState#exec_state.status =:= done.
is_blocked(ExecState) -> false.  %% No blocking in single-token executor
```

**7. Test with mock bytecode**:
- Test single task execution (TASK_EXEC → DONE → done)
- Test sequence execution (TASK_EXEC → SEQ_NEXT → TASK_EXEC → DONE → done)
- Test IP advancement through bytecode
- Test quanta yielding (run with Quanta=1 yields after 1 step)

### Phase 3: Multi-Token Support (PAR_FORK, XOR_CHOOSE, JOIN_WAIT)

**1. Implement token management**:
- make_token_id/0: Generate unique token ID
- select_next_token/1: Choose next token from tokens map
- update_token/3: Update token in tokens map

**2. Implement PAR_FORK handler**:
- Spawn N tokens with independent IPs
- Create branch_info record
- Create join_counter record
- Remove current token, add branch tokens
- Test: par([task_a, task_b]) spawns 2 tokens

**3. Implement JOIN_WAIT handler**:
- Check join counter (completed >= required)
- If satisfied: merge results, remove branch/join entries, continue
- If not satisfied: set status = blocked_join
- Test: par([task_a, task_b]) → JOIN_WAIT blocks until both complete

**4. Implement XOR_CHOOSE handler**:
- Use scheduler decision to select branch
- Update current token's IP to selected branch
- No join needed
- Test: xor([task_a, task_b]) selects one branch

**5. Implement DONE handler for multi-token**:
- Mark current token complete
- Check if any active tokens remain
- If yes: select next token, continue
- If no: set status = done
- Test: par([task_a, task_b]) completes only after both DONE

**6. Test parallel execution**:
- Test par with 2 tasks (both execute, then join)
- Test xor with 2 alternatives (only one executes)
- Test nested par (par([seq(a,b), seq(c,d)]))
- Test join policies (all, first_n, first_complete)

### Phase 4: Loop Support (LOOP_CHECK, LOOP_BACK)

**1. Implement LOOP_CHECK handler**:
- Evaluate loop condition (policy-dependent)
- If continue: advance to body (next instruction)
- If exit: skip to exit label (instruction after LOOP_BACK)
- Test: loop({count, 3}, task_a) executes 3 times

**2. Implement LOOP_BACK handler**:
- Jump to target IP (loop head)
- Test: LOOP_BACK jumps to LOOP_CHECK

**3. Implement loop condition evaluation**:
- `{count, N}`: Decrement counter in exec_state, exit if 0
- `while`: Evaluate condition function, exit if false
- `until`: Execute body, check condition after, exit if true
- Test: All three loop policies work correctly

**4. Test loops**:
- Test count loop (fixed iterations)
- Test while loop (condition check first)
- Test until loop (condition check after)
- Test nested loops (loop within loop)

### Phase 5: Cancellation Support (CANCEL_SCOPE)

**1. Implement CANCEL_SCOPE handler**:
- Enter: Push scope onto stack
- Exit: Pop scope from stack, check if cancelled
- Test: cancel(scope, task_a) wraps task in scope

**2. Integrate with wf_cancel** (item 008):
- Call wf_cancel:propagate/2 when scope cancelled
- Mark all tokens in scope as cancelled
- Test: Cancel signal during par marks both branches as cancelled

**3. Test cancellation**:
- Test cancel scope (enter and exit)
- Test nested cancel scopes
- Test cancel propagation (parent cancels children)
- Test cancel during parallel execution

### Phase 6: Effect Yield Support (TASK_EXEC effect)

**1. Implement effect yield in TASK_EXEC**:
- Detect {effect, EffectSpec, ContCtx} return
- Set status = blocked_effect
- Store EffectSpec in exec_state
- Test: Task returns {effect, ...}, executor blocks

**2. Implement effect resumption**:
- Provide resume/3 function to continue after effect result
- Update context with effect result
- Advance IP and continue
- Test: Effect result resumes executor

**3. Integrate with wf_effect** (item 010):
- Yield EffectSpec to effect manager
- Effect manager executes and returns result
- Executor resumes with result
- Test: Effect round-trip (yield → execute → resume)

### Phase 7: Scheduler Integration (wf_sched)

**1. Integrate with wf_sched** (item 007):
- Call wf_sched:select_action/2 in run/3
- Pass scheduler decision to step/2
- Use decision for XOR_CHOOSE (branch selection)
- Test: Deterministic scheduler produces same trace every time

**2. Implement token selection**:
- When multiple tokens active, use scheduler to choose
- select_next_token/1 uses scheduler policy
- Test: Scheduler selects token with lowest IP (deterministic)

### Phase 8: Tracing Integration (wf_trace)

**1. Emit trace events**:
- Every step emits trace_event
- Include: opcode, state_before, state_after, timestamp
- Test: Trace events capture execution history

**2. Integrate with wf_trace** (item 011):
- Call wf_trace:emit/2 for each reduction
- Support trace levels (none, min, full)
- Test: Trace log enables replay

### Phase 9: Comprehensive Testing

**1. Unit tests**:
- Test each opcode handler in isolation
- Test exec_state transitions
- Test is_done/1 and is_blocked/1
- Test quanta execution and yielding

**2. Integration tests**:
- Test complete workflows (seq, par, xor, loop, cancel)
- Test nested patterns
- Test all join policies
- Test all loop policies

**3. Property-based tests**:
- Generate random bytecode
- Assert executor never crashes
- Assert invariants preserved (token count non-negative, IP in bounds)
- Assert termination (no infinite loops)

**4. Performance tests**:
- Measure reductions/sec
- Measure memory usage
- Profile hot paths
- Optimize if needed

## Open Questions

1. **Task Function Lookup**: How does executor map task name → task function?
   - **Option 1**: Task functions stored in bytecode metadata (item 004 responsibility)
   - **Option 2**: Task registry (ETS table) populated before execution
   - **Option 3**: Task metadata passed in exec_state initialization
   - **Recommendation**: Option 1 (bytecode metadata). Compiler emits task functions as module:function references in bytecode. Executor looks up and calls them. This keeps bytecode self-contained.

2. **Token ID Generation**: Should token IDs be refs, integers, or UUIDs?
   - **Recommendation**: Use `erlang:make_ref()` for uniqueness (same as labels in item 004). No collision risk, no counter management.

3. **Branch ID Generation**: Should branch IDs be auto-generated or encoded in bytecode?
   - **Recommendation**: Auto-generate in executor using make_ref(). Bytecode doesn't know about runtime branch IDs.

4. **Join ID Generation**: Should join IDs be auto-generated or specified in bytecode?
   - **Recommendation**: Auto-generate in executor. PAR_FORK creates branch and join, associates them in maps.

5. **Loop Exit Label Detection**: How does LOOP_CHECK know where to exit (IP of instruction after LOOP_BACK)?
   - **Option 1**: Bytecode includes exit label (compiler emits label)
   - **Option 2**: Executor scans forward to find LOOP_BACK, exits after it
   - **Recommendation**: Option 1. Compiler emits explicit exit label. LOOP_CHECK jumps to exit label when condition false.

6. **Effect Yield vs Separate Opcode**: Should effect yield be separate EFFECT_YIELD opcode or implicit in TASK_EXEC?
   - **Recommendation**: Implicit in TASK_EXEC (as shown in SEMANTICS.md:265-287). Task returns {effect, Spec, ContCtx}. Executor detects this and yields. No separate opcode needed.

7. **Scope Stack Initialization**: What is initial scope stack value?
   - **Recommendation**: `[root]` (single root scope). All execution happens in root scope by default. CANCEL_SCOPE pushes nested scopes.

8. **Quanta Default Value**: What is default quanta for run/3?
   - **Recommendation**: 100 reductions/tick. Configurable via options. Tradeoff: throughput vs responsiveness.

9. **Trace Event Overhead**: Should tracing be mandatory or optional?
   - **Recommendation**: Optional with trace_level parameter (none | min | full). Default to min (basic events). Full tracing for debugging/replay.

10. **Error Handling in Tasks**: How should executor handle task errors?
    - **Option 1**: Mark token as failed, continue execution
    - **Option 2**: Propagate error to parent scope, terminate case
    - **Recommendation**: Option 2 (terminate case). Task errors are fatal (workflow pattern semantics). Mark status = failed, terminate.

11. **Blocking on JOIN_WAIT**: If JOIN_WAIT is not satisfied, does executor block entirely or switch to another token?
    - **Recommendation**: Switch to another token. If multiple tokens active, continue executing others. Join check happens when all tokens complete or blocked.

12. **Cooperative Scheduling Fairness**: How to ensure fair scheduling across multiple cases?
    - **Recommendation**: Quanta limiting is first step. Add priority levels later. Use gen_statem state_timeout with delay=0 for fair scheduling.

13. **State Persistence**: Should executor persist state to wf_state (item 006) or keep in-memory?
    - **Recommendation**: In-memory for v1. Add persistence later (item 006). State persistence is orthogonal to execution.

14. **Cancellation Propagation Implementation**: Should executor implement cancellation inline or delegate to wf_cancel (item 008)?
    - **Recommendation**: Delegate to wf_cancel. Executor calls wf_cancel:propagate/2, which marks tokens as cancelled. Keeps separation of concerns.

15. **Deterministic Replay**: How does executor support replay mode (item 011)?
    - **Recommendation**: Use wf_sched:replay policy with recorded choices. Executor records scheduler decisions in trace log. Replay feeds decisions back to scheduler.

16. **Bytecode Validation**: Should executor validate bytecode before execution?
    - **Recommendation**: No. Compiler (item 004) validates bytecode. Executor assumes valid bytecode. Can add assertions for debugging (e.g., IP in bounds).

17. **Multiple Instance Support (mi)**: Should executor handle MI_SPAWN or delegate to wf_mi (item 009)?
    - **Recommendation**: Executor handles MI_SPAWN (spawns instances like PAR_FORK). wf_mi provides helper functions for instance management.

18. **Defer Pattern Support**: How to handle defer (deferred choice)?
    - **Recommendation**: Defer requires external signal selection (DEFER_SELECT). Executor blocks on DEFER_WAIT until signal arrives. Defer is kernel primitive, must be supported.

19. **Parallel Token Scheduling**: When multiple tokens active, which one executes first?
    - **Recommendation**: Use wf_sched:select_action/2. Scheduler chooses based on policy (deterministic, nondeterministic, replay).

20. **Memory Management**: When to remove completed tokens from tokens map?
    - **Recommendation**: Remove immediately when DONE executed (or join satisfied). Keep only active tokens. Completed tokens are marked for trace/debugging but removed from active set.
