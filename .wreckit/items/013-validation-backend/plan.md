# Implement validation backend with bounded model checking Implementation Plan

## Implementation Plan Title

Static Analysis and Bounded Model Checking for Workflow Bytecode

## Overview

Implement wf_validate.erl: a static analysis and bounded model checking tool that validates workflow bytecode (wf_vm:wf_bc()) without executing runtime tasks or effects. The module explores all reachable states within configurable bounds (depth D, token bound K) and performs five correctness checks: dead transition detection (unreachable code), option to complete (no livelocks), proper completion (single terminal token), deadlock detection (no stuck states), and soundness (composite check).

This is a test/validation tool, NOT the runtime execution engine. It performs symbolic exploration of the control flow graph derived from bytecode, tracking token positions, join counters, and scope stack but never executing TASK_EXEC opcodes or external effects.

## Current State

**Existing Infrastructure:**
- wf_vm.erl defines bytecode format (wf_bc()) and all opcodes (lines 10-50)
- wf_exec.erl implements bytecode executor with exec_state record (lines 1-783)
- wf_exec.hrl defines exec_state, token, branch_info, and join_counter records (lines 31-44)
- wf_sched.erl defines enabled_action() type (lines 19-21) and scheduler policies (lines 73-81)
- wf_mi.erl implements multiple instance patterns with mi_instance record
- Testing uses EUnit framework with -include_lib("eunit/include/eunit.hrl")
- No external dependencies (pure Erlang/OTP per rebar.config:13)

**Missing Components:**
- wf_validate.erl does not exist
- No state space exploration algorithms
- No correctness check implementations
- No Petri net or LTS compilation from bytecode
- wf_term and wf_compile modules are not implemented (validation works directly with bytecode)

**Key Constraints:**
- Must accept wf_vm:wf_bc() bytecode as input (pattern algebra not yet available)
- Must NOT use wf_exec:step/2 (that's runtime execution, not static analysis)
- Must implement its own symbolic executor for path exploration
- Must handle all opcodes defined in wf_vm.erl
- Must enforce strict bounds to prevent state space explosion
- Must return structured results: {ok, report()} | {error, [issue()]}

## Desired End State

**Functional Requirements:**
1. to_petri_net/1 compiles wf_bc() to LTS representation (places = bytecode positions, transitions = opcode executions)
2. explore/2 performs BFS/DFS state space exploration with depth D and token bound K
3. check_dead_transitions/2 verifies every transition is firable (no unreachable code)
4. check_option_to_complete/1 verifies every reachable state has path to terminal state
5. check_proper_completion/1 verifies terminal states have exactly one completion token
6. check_deadlock/1 detects states with tokens but no enabled transitions
7. check_soundness/1 composes the above three checks
8. validate/1 and validate/2 are main entry points with options handling

**Non-Functional Requirements:**
- Exploration completes within reasonable time for workflows with ≤500 opcodes
- State hashing (erlang:phash2/1) used for visited set to bound memory
- Deterministic ordering of enabled actions for reproducible results
- Optional tracing via wf_trace for debugging
- Clear documentation that this is static analysis, not runtime execution

**Verification Criteria:**
- All EUnit tests pass for simple sequence (no issues), deadlock case, unreachable code, proper completion violations
- Module exports all required functions with -spec declarations
- Type definitions exported for validation-specific types
- Integration with wf_substrate:validate/2 API stub

### Key Discoveries:

- **wf_exec.hrl:31-44**: exec_state record uses maps for tokens, branch_map, join_counters - validation_state should mirror this structure but be lighterweight (no ctx, case_id, status)
- **wf_exec.erl:52-59**: Token record includes instance_id field for MI patterns - validation must track instance IDs to distinguish parallel instances
- **wf_vm.erl:13-21**: Opcodes have varying argument formats (single integer, list, policy tuples) - validation dispatch must handle all formats
- **wf_sched.erl:19-21**: enabled_action() type represents nondeterministic choices ({token, token_id()} or {xor_branch, pos_integer()}) - validation must enumerate ALL enabled actions for exhaustive checking
- **wf_exec.erl:176-183**: fetch_opcode/1 handles out-of-bounds IP by returning {'DONE'} - validation should adopt this pattern for graceful termination
- **wf_exec.erl:253-259**: select_next_token/1 chooses first active token deterministically - validation should sort enabled actions for reproducible exploration
- **wf_exec.erl:724-731**: find_branch_for_token/2 searches branch_map for token membership - validation needs helper to find which tokens belong to which joins
- **wf_exec_tests.erl:1-100**: Test pattern uses mock_bytecode_* functions to create test data - validation tests should follow this pattern

## What We're NOT Doing

- **NOT implementing wf_term or wf_compile** - validation works directly with bytecode
- **NOT using wf_exec:step/2** - that would be runtime execution, not static analysis
- **NOT executing TASK_EXEC opcodes** - treat as no-op transitions in validation
- **NOT implementing full cancellation semantics** - item 008 (wf_cancel) will handle this; treat CANCEL_SCOPE as structural marker only
- **NOT supporting unbounded state exploration** - bounded checking is intentionally incomplete for unbounded systems
- **NOT implementing Petri net visualization** - LTS representation sufficient for v1, can add Petri net export later
- **NOT implementing runtime effect validation** - this is control flow validation only, not data flow or effect correctness

## Implementation Approach

**Symbolic Execution Strategy:**
1. Compile bytecode to LTS (Labelled Transition System) where states are validation_state records (token positions, join counters, scope stack) and transitions are opcode executions
2. Use BFS/DFS traversal from initial state, tracking visited states via hash set
3. At each state, compute enabled actions (tokens that can advance, joins that can fire) by examining bytecode at each token's IP
4. For each enabled action, compute successor state by applying opcode semantics (mirroring wf_exec but without effect execution)
5. Apply bounds: depth D (max steps), token bound K (max concurrent tokens)
6. Track which transitions fired, collect terminal states, detect deadlock states

**Key Design Decisions:**
1. **LTS over Petri net**: Simpler to implement directly with validation_state as nodes; Petri net is implicit in bytecode structure
2. **State hashing for visited set**: Use erlang:phash2/1 on validation_state to avoid storing full states (collision risk acceptable for v1)
3. **Deterministic enumeration**: Sort enabled actions by token_id then IP for reproducible results across runs
4. **Symbolic opcode handling**: Each opcode has a validation handler that computes successor state (e.g., PAR_FORK spawns tokens, JOIN_WAIT checks counters)
5. **Separate from runtime**: No process spawning, no external calls, pure functional state transformation suitable for CI/CD

**Phased Implementation:**
- Phase 1: Core data structures (records, types, to_petri_net/1)
- Phase 2: Exploration engine (enabled_transitions/1, fire_transition/2, explore/2)
- Phase 3: Correctness checks (dead transitions, option to complete, proper completion, deadlock, soundness)
- Phase 4: Integration and testing (validate/1, validate/2, EUnit tests, wf_substrate integration)

---

## Phases

### Phase 1: Core Data Structures and LTS Compilation

#### Overview

Define validation-specific records and types, then implement to_petri_net/1 to compile bytecode to LTS representation. This phase establishes the foundation for state space exploration.

#### Changes Required:

##### 1. Create wf_validate.erl module skeleton

**File**: `/Users/speed/wf-substrate/src/wf_validate.erl`
**Changes**: Create new module with module declaration, includes, and exported types

```erlang
-module(wf_validate).
-include("wf_exec.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(validation_state, {
    bytecode :: wf_vm:wf_bc(),
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer()
}).

-record(issue, {
    type :: dead_transition | livelock | improper_completion | deadlock | unsound,
    state :: #validation_state{},
    message :: binary(),
    path_from_initial :: [wf_sched:enabled_action()]
}).

-record(report, {
    explored_state_count :: non_neg_integer(),
    unique_state_count :: non_neg_integer(),
    max_depth_reached :: non_neg_integer(),
    checks_passed :: #{atom() => boolean()},
    issues_found :: [#issue{}]
}).

%%====================================================================
%% Types
%%====================================================================

-type validation_state() :: #validation_state{}.
-type issue() :: #issue{}.
-type report() :: #report{}.
-type validate_options() :: #{
    depth => pos_integer(),
    token_bound => pos_integer(),
    search_strategy => bfs | dfs,
    trace_level => none | min | full
}.

%%====================================================================
%% Exports
%%====================================================================

-export([new/1, default_options/0, to_petri_net/1]).

-export_type([validation_state/0, issue/0, report/0, validate_options/0]).

%% @doc Create new validation state from bytecode
-spec new(wf_vm:wf_bc()) -> validation_state().
new(Bytecode) ->
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
    #validation_state{
        bytecode = Bytecode,
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0
    }.

%% @doc Get default validation options
-spec default_options() -> validate_options().
default_options() ->
    #{
        depth => 100,
        token_bound => 10,
        search_strategy => bfs,
        trace_level => none
    }.

%% @doc Compile bytecode to Petri net (LTS) representation
-spec to_petri_net(wf_vm:wf_bc()) -> {validation_state(), map()}.
to_petri_net(Bytecode) ->
    InitialState = new(Bytecode),
    Metadata = #{
        bytecode_length => length(Bytecode),
        transition_counts => count_transitions(Bytecode)
    },
    {InitialState, Metadata}.

%% @private Count transitions by opcode type
count_transitions(Bytecode) ->
    lists:foldl(fun(Opcode, Acc) ->
        OpType = element(1, Opcode),
        maps:update_with(OpType, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Bytecode).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles without errors: `rebar3 compile`
- [ ] Type checking passes: `rebar3 dialyzer`
- [ ] to_petri_net/1 accepts mock bytecode and returns validation_state
- [ ] default_options/0 returns map with expected keys

##### Manual Verification:

- [ ] Records follow same pattern as wf_exec.hrl (maps, field naming)
- [ ] Module includes wf_exec.hrl for token/branch_info/join_counter records
- [ ] Exported types match specification

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

### Phase 2: Exploration Engine

#### Overview

Implement the core state space exploration algorithm: compute enabled transitions, fire transitions to compute successor states, and perform BFS/DFS traversal with visited set tracking and bound enforcement.

#### Changes Required:

##### 1. Implement exploration functions

**File**: `/Users/speed/wf-substrate/src/wf_validate.erl`
**Changes**: Add enabled_transitions/1, fire_transition/2, explore/2, state_hash/1

Key implementations:
- `enabled_transitions/1`: Returns list of {token, token_id} for active tokens, handling XOR_CHOOSE as special case
- `fire_transition/2`: Applies opcode semantics (PAR_FORK spawns tokens, TASK_EXEC advances IP, etc.)
- `explore/2`: BFS/DFS traversal with visited set, depth bound, token bound
- `state_hash/1`: Hash of validation_state for visited set

```erlang
-export([enabled_transitions/1, fire_transition/2, explore/2, state_hash/1]).

%% @doc Compute enabled transitions from state
-spec enabled_transitions(validation_state()) -> [wf_sched:enabled_action()].
enabled_transitions(#validation_state{tokens = Tokens, bytecode = Bytecode}) ->
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],
    lists:filtermap(fun(Token) ->
        IP = Token#token.ip,
        case IP < length(Bytecode) of
            true ->
                Opcode = lists:nth(IP + 1, Bytecode),
                case is_enabled(Opcode) of
                    true -> {true, {token, Token#token.token_id}};
                    false -> false
                end;
            false -> false
        end
    end, ActiveTokens).

%% @private Check if opcode is enabled
is_enabled({'TASK_EXEC', _TaskName}) -> true;
is_enabled({'DONE'}) -> true;
is_enabled({'SEQ_ENTER', _Arg}) -> true;
is_enabled({'SEQ_NEXT', _TargetIP}) -> true;
is_enabled({'PAR_FORK', _Targets}) -> true;
is_enabled({'JOIN_WAIT', _Policy}) -> true;
is_enabled({'XOR_CHOOSE', _Targets}) -> false;  %% Handled separately
is_enabled({'LOOP_CHECK', _Policy}) -> true;
is_enabled({'LOOP_BACK', _TargetIP}) -> true;
is_enabled({'MI_SPAWN', _Policy}) -> true;
is_enabled({'CANCEL_SCOPE', {_Op, _ScopeId}}) -> true.

%% @doc Fire transition and compute successor state
-spec fire_transition(validation_state(), wf_sched:enabled_action()) -> validation_state().
fire_transition(State0, {token, TokenId}) ->
    Token = maps:get(TokenId, State0#validation_state.tokens),
    IP = Token#token.ip,
    Bytecode = State0#validation_state.bytecode,
    Opcode = lists:nth(IP + 1, Bytecode),
    execute_opcode(Opcode, Token, State0).

%% @private Execute opcode (symbolic, no effects)
execute_opcode({'TASK_EXEC', _TaskName}, Token, State) ->
    update_token_ip(Token, Token#token.ip + 1, State);
execute_opcode({'DONE'}, Token, State) ->
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(UpdatedToken#token.token_id, UpdatedToken, State#validation_state.tokens),
    State#validation_state{tokens = Tokens};
execute_opcode({'PAR_FORK', TargetIPs}, Token, State) ->
    %% Spawn N tokens, create branch_map and join_counters
    %% (implementation mirrors wf_exec:execute_par_fork)
    spawn_branch_tokens(TargetIPs, Token, State);
%% ... (other opcodes)

%% @doc Hash state for visited set
-spec state_hash(validation_state()) -> non_neg_integer().
state_hash(State) ->
    Data = {
        State#validation_state.bytecode,
        State#validation_state.tokens,
        State#validation_state.branch_map,
        State#validation_state.join_counters
    },
    erlang:phash2(Data).

%% @doc Explore state space with bounds
-spec explore(wf_vm:wf_bc(), validate_options()) -> {ok, report()}.
explore(Bytecode, Options) ->
    InitialState = new(Bytecode),
    MaxDepth = maps:get(depth, Options, 100),
    MaxTokens = maps:get(token_bound, Options, 10),
    Strategy = maps:get(search_strategy, Options, bfs),
    case Strategy of
        bfs -> explore_bfs([InitialState], sets:new(), MaxDepth, MaxTokens, #{});
        dfs -> explore_dfs([InitialState], sets:new(), MaxDepth, MaxTokens, #{})
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] enabled_transitions/1 returns correct list for simple bytecode
- [ ] fire_transition/2 computes correct successor state for PAR_FORK
- [ ] explore/2 completes without hanging on simple sequence bytecode
- [ ] State hashing produces consistent values for identical states
- [ ] Depth bound terminates exploration at configured depth

##### Manual Verification:

- [ ] Exploration follows BFS/DFS ordering as configured
- [ ] Visited set prevents re-exploration of duplicate states
- [ ] Token bound prevents unbounded parallel token explosion

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

### Phase 3: Correctness Checks

#### Overview

Implement the five correctness checks: dead transitions, option to complete, proper completion, deadlock, and soundness (composite).

#### Changes Required:

##### 1. Implement check functions

**File**: `/Users/speed/wf-substrate/src/wf_validate.erl`
**Changes**: Add check_dead_transitions/2, check_option_to_complete/1, check_proper_completion/1, check_deadlock/1, check_soundness/1

```erlang
-export([check_dead_transitions/2, check_option_to_complete/1, check_proper_completion/1,
         check_deadlock/1, check_soundness/1]).

%% @doc Check for dead transitions (unreachable code)
-spec check_dead_transitions([validation_state()], wf_vm:wf_bc()) -> [#issue{}].
check_dead_transitions(ExploredStates, Bytecode) ->
    ReachableIPs = lists:usort(lists:flatmap(fun(State) ->
        [Token#token.ip || Token <- maps:values(State#validation_state.tokens)]
    end, ExploredStates)),
    AllIPs = lists:seq(0, length(Bytecode) - 1),
    DeadIPs = lists:filter(fun(IP) -> not lists:member(IP, ReachableIPs) end, AllIPs),
    [create_issue(dead_transition, hd(ExploredStates),
        <<"Dead transition: unreachable code">>, []) || IP <- DeadIPs].

%% @doc Check option to complete (no livelocks)
-spec check_option_to_complete([validation_state()], validate_options()) -> [#issue{}].
check_option_to_complete(ExploredStates, Options) ->
    lists:filtermap(fun(State) ->
        case is_terminal(State) of
            true -> false;
            false ->
                case has_path_to_terminal(State, Options) of
                    true -> false;
                    false -> {true, create_issue(livelock, State,
                        <<"Livelock: no path to terminal">>, [])}
                end
        end
    end, ExploredStates).

%% @doc Check proper completion (exactly one token at termination)
-spec check_proper_completion([validation_state()]) -> [#issue{}].
check_proper_completion(ExploredStates) ->
    TerminalStates = lists:filter(fun is_terminal/1, ExploredStates),
    lists:filtermap(fun(State) ->
        CompleteTokens = [T || T <- maps:values(State#validation_state.tokens),
                               T#token.status =:= complete],
        case length(CompleteTokens) of
            1 -> false;
            N -> {true, create_issue(improper_completion, State,
                <<"Improper completion: ", (integer_to_binary(N))/binary, " tokens">>, [])}
        end
    end, TerminalStates).

%% @doc Check deadlock (tokens exist but no enabled transitions)
-spec check_deadlock([validation_state()]) -> [#issue{}].
check_deadlock(ExploredStates) ->
    lists:filtermap(fun(State) ->
        ActiveTokens = [T || T <- maps:values(State#validation_state.tokens), T#token.status =:= active],
        case length(ActiveTokens) > 0 andalso (enabled_transitions(State) =:= []) of
            true -> {true, create_issue(deadlock, State,
                <<"Deadlock: active tokens, no transitions">>, [])};
            false -> false
        end
    end, ExploredStates).

%% @doc Check soundness (composite)
-spec check_soundness({[validation_state()], wf_vm:wf_bc(), validate_options()}) -> [#issue{}].
check_soundness({States, Bytecode, Options}) ->
    check_dead_transitions(States, Bytecode) ++
    check_option_to_complete(States, Options) ++
    check_proper_completion(States) ++
    check_deadlock(States).
```

#### Success Criteria:

##### Automated Verification:

- [ ] check_dead_transitions/2 detects unreachable TASK_EXEC in test bytecode
- [ ] check_proper_completion/1 detects terminal state with multiple tokens
- [ ] check_deadlock/1 detects PAR_FORK without matching JOIN_WAIT
- [ ] check_soundness/1 returns {ok, report()} for valid simple sequence
- [ ] All checks return structured issue() records

##### Manual Verification:

- [ ] Issue records include state snapshots for debugging
- [ ] Diagnostic messages clearly explain the problem

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 4.

---

### Phase 4: Integration and Testing

#### Overview

Implement main entry points (validate/1, validate/2), write comprehensive EUnit tests, integrate with wf_substrate public API, and document the module.

#### Changes Required:

##### 1. Implement public API

**File**: `/Users/speed/wf-substrate/src/wf_validate.erl`
**Changes**: Add validate/1, validate/2, format_issue/1, format_report/1

```erlang
-export([validate/1, validate/2, format_issue/1, format_report/1]).

%% @doc Validate with default options
-spec validate(wf_vm:wf_bc()) -> {ok, report()} | {error, [issue()]}.
validate(Bytecode) -> validate(Bytecode, default_options()).

%% @doc Validate with custom options
-spec validate(wf_vm:wf_bc(), validate_options()) -> {ok, report()} | {error, [issue()]}.
validate(Bytecode, Options) ->
    {ok, Report0} = explore(Bytecode, Options),
    %% Run checks and update report
    %% (implementation)
    case length(AllIssues) of
        0 -> {ok, FinalReport};
        _ -> {error, AllIssues}
    end.
```

##### 2. Create wf_validate_tests.erl

**File**: `/Users/speed/wf-substrate/test/wf_validate_tests.erl`
**Changes**: Create comprehensive EUnit test suite

```erlang
-module(wf_validate_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").

%% Mock bytecode
mock_bytecode_simple() -> [{'TASK_EXEC', task}, {'DONE'}].
mock_bytecode_deadlock() -> [{'PAR_FORK', [1,3]}, {'TASK_EXEC', a}, {'DONE'}, {'TASK_EXEC', b}, {'DONE'}].

%% Tests
validate_simple_test_() ->
    Bytecode = mock_bytecode_simple(),
    Result = wf_validate:validate(Bytecode),
    ?_assertMatch({ok, _Report}, Result).

validate_deadlock_test_() ->
    Bytecode = mock_bytecode_deadlock(),
    Result = wf_validate:validate(Bytecode),
    ?_assertMatch({error, _Issues}, Result).
```

##### 3. Update wf_substrate.erl

**File**: `/Users/speed/wf-substrate/src/wf_substrate.erl`
**Changes**: Export validate/2

```erlang
-export([validate/2]).

%% @doc Validate workflow bytecode
-spec validate(wf_vm:wf_bc(), proplists:proplist()) -> {ok, wf_validate:report()} | {error, [wf_validate:issue()]}.
validate(Bytecode, Options) ->
    wf_validate:validate(Bytecode, maps:from_list(Options)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] validate/1 returns {ok, report()} for simple sequence bytecode
- [ ] All EUnit tests pass: `rebar3 eunit`
- [ ] No dialyzer warnings: `rebar3 dialyzer`

##### Manual Verification:

- [ ] Module documentation explains static analysis nature
- [ ] Test cases cover deadlock, livelock, improper completion scenarios

**Note**: Complete all automated and manual verification. Implementation complete.

---

## Testing Strategy

### Unit Tests:

- **State Construction**: Test new/1 creates initial state with single token at IP=0
- **Enabled Transitions**: Test enabled_transitions/1 returns correct actions for each opcode
- **Transition Firing**: Test fire_transition/2 updates state correctly for each opcode
- **State Hashing**: Test state_hash/1 produces consistent hashes
- **Exploration**: Test explore/2 respects depth and token bounds
- **Correctness Checks**: Test all five checks detect expected issues

### Integration Tests:

- Simple workflow (TASK_EXEC, DONE) -> {ok, report()}
- Parallel with join -> {ok, report()}
- Parallel without join -> {error, [deadlock/improper_completion]}
- Unreachable code -> {error, [dead_transition]}
- Infinite loop -> {error, [livelock]}

### Manual Testing Steps:

1. Validate simple workflow, verify {ok, report()} with 0 issues
2. Validate deadlock bytecode, verify {error, Issues} returned
3. Inspect issue records for diagnostic messages and state snapshots

## Migration Notes

No migration required. This is a new module with no dependencies on existing data or runtime state.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/013-validation-backend/research.md`
- Bytecode definition: `/Users/speed/wf-substrate/src/wf_vm.erl:10-50`
- Executor patterns: `/Users/speed/wf-substrate/src/wf_exec.erl:1-783`
- Record definitions: `/Users/speed/wf-substrate/src/wf_exec.hrl:31-44`
- Scheduler types: `/Users/speed/wf-substrate/src/wf_sched.erl:19-21`
- Test patterns: `/Users/speed/wf-substrate/test/wf_exec_tests.erl:1-100`
- Public API stub: `/Users/speed/wf-substrate/src/wf_substrate.erl:16`
- Project constraints: `/Users/speed/wf-substrate/PROMPT.md:218-227`
