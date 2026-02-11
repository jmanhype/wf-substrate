# Research: Implement pluggable scheduler policies

**Date**: 2025-01-10
**Item**: 007-scheduler-policies

## Research Question

Implement wf_sched.erl: pluggable scheduler policies for the executor. The scheduler decides which enabled action to execute next when multiple are available (e.g., multiple parallel branches ready, multiple XOR choices available).

Three policies:
1. Deterministic: stable ordering of enabled actions (sorted by label/ID). Given the same set of enabled actions, always picks the same one. Useful for testing and reproducibility.
2. Nondeterministic: random selection among enabled actions with recorded choices. Every choice is logged as {step_seq, enabled_set, chosen} for later replay. Uses rand module with configurable seed.
3. Replay: feed a recorded choice log, reproduce the exact execution trace. On each decision point, consumes the next recorded choice. Fails loudly if the enabled set doesn't match the recording (divergence detection).

The scheduler is a behaviour with callback choose/2 :: (EnabledActions, SchedState) -> {Chosen, NewSchedState}. Each policy is a module implementing this behaviour.

Export: new/2 (policy, options), choose/2, get_log/1, from_log/1 (create replay scheduler from recorded log).

## Summary

This task involves implementing a pluggable scheduler policy system (`wf_sched.erl`) that determines which enabled action to execute next when multiple actions are available (e.g., multiple parallel branches ready, multiple XOR choices). The scheduler is critical for workflow determinism, testing, and replay capabilities.

The implementation requires three policies: **deterministic** (stable ordering for reproducibility), **nondeterministic** (random selection with choice logging for replay), and **replay** (reproduces exact execution traces from recorded choices). The scheduler is implemented as a behaviour with a `choose/2` callback, where each policy module implements this behaviour.

**Key Architectural Context**:
- This is **item 007** in the implementation sequence
- Depends on `wf_exec` (executor) which consumes scheduler decisions (item 005)
- Used by `wf_exec:run/3` to select next action during execution
- Integrates with `wf_trace` for choice logging (item 011)
- No external dependencies (pure Erlang/OTP, uses `rand` module)

**Current State**: A stub implementation exists at `/Users/speed/wf-substrate/src/wf_sched.erl:1-32` with only a mock `select_action/2` function that returns placeholder decisions regardless of policy. The stub is sufficient for initial executor testing but lacks the required behaviour pattern, choice logging, and replay capabilities.

## Current State Analysis

### Existing Implementation

**File**: `/Users/speed/wf-substrate/src/wf_sched.erl:1-32`

The current implementation is a minimal stub:

```erlang
-module(wf_sched).

-type sched_policy() :: deterministic | nondeterministic | {replay, term()}.
-type sched_decision() :: {token, term()} | {xor_branch, pos_integer()}.
-type exec_state() :: term().  %% Opaque for scheduler

-export([select_action/2]).

-spec select_action(exec_state(), sched_policy()) -> sched_decision().
select_action(_ExecState, Policy) ->
    %% Stub: always return token decision regardless of policy
    case Policy of
        undefined -> {token, mock_token};
        deterministic -> {token, mock_token};
        nondeterministic -> {token, mock_token};
        {replay, _} -> {token, replay_token}
    end.
```

**Problems with Current Implementation**:

1. **No behaviour pattern**: Policies are not separate modules implementing a behaviour callback
2. **No state management**: Each invocation is stateless, cannot track choice history or replay position
3. **No choice logging**: Nondeterministic policy doesn't record choices for replay
4. **No replay validation**: Replay policy doesn't validate enabled sets match recording
5. **Mock decisions only**: Returns placeholder values, doesn't inspect exec_state
6. **Missing exports**: Lacks `new/2`, `get_log/1`, `from_log/1` as specified in item.json

**Integration Points**:

- **Used by executor** (`/Users/speed/wf-substrate/src/wf_exec.erl:167`):
  ```erlang
  SchedDecision = wf_sched:select_action(ExecState0, SchedPolicy),
  {ExecState1, _TraceEvent} = step(ExecState0, SchedDecision),
  ```
  Executor calls scheduler in `run_loop/4` to select next action before each step.

- **From ARCHITECTURE.md:1280-1300**: Scheduler module specification defines:
  - `select_action/2`: Choose next enabled action
  - Policies: `deterministic`, `nondeterministic`, `replay`
  - Deterministic: Stable ordering (e.g., by token_id, then IP)
  - Nondeterministic: Random selection with logging
  - Replay: Use logged choices

- **From SEMANTICS.md:1050-1068**: Determinism rule defines:
  - Under deterministic scheduler, same term + same ctx → same trace
  - Scheduler produces unique reduction sequence
  - Replay is possible by recording scheduler choices

### Current Patterns and Conventions

**From existing codebase**:

1. **Record-based state**: `wf_exec` uses `#exec_state{}` records (lines 55-66 of wf_exec.erl)
2. **Opaque types**: Scheduler treats `exec_state()` as opaque (line 9 of wf_sched.erl)
3. **Type specifications**: All modules use `-type` and `-spec` for dialyzer
4. **Erlang/OTP conventions**: Uses `rand` module for randomness, no external dependencies
5. **Behaviour pattern**: OTP behaviours (gen_server, gen_statem) used elsewhere

**Integration with wf_exec**:

From `/Users/speed/wf-substrate/src/wf_exec.erl:290-297`:
```erlang
execute_xor_choose({_XorChoose, TargetIPs}, ExecState, _SchedDecision) ->
    %% Scheduler decision contains selected branch index
    %% For now, always select first branch (deterministic)
    SelectedIndex = 1,
    SelectedIP = lists:nth(SelectedIndex, TargetIPs),
    ...
```

Currently, executor ignores `SchedDecision` and hardcodes `SelectedIndex = 1`. The scheduler must return `{xor_branch, Index}` to enable proper branch selection.

**Decision Types Required**:

1. **Token selection**: When multiple tokens active (e.g., parallel branches), choose which token to execute
2. **XOR branch selection**: When executing XOR_CHOOSE opcode, choose which branch index

### Blocking Dependencies

**No blocking dependencies**: Scheduler can be implemented independently. However:

1. **wf_exec (item 005)**: Executor currently uses stub scheduler. Must update executor to use scheduler decisions properly.
2. **wf_trace (item 011)**: Nondeterministic policy should log choices via tracing system. Can use simple term logging for v1.

## Key Files

### Existing Files

- `/Users/speed/wf-substrate/src/wf_sched.erl:1-32` - **Current stub implementation**
  - Lines 7-9: Type definitions (`sched_policy()`, `sched_decision()`, `exec_state()`)
  - Lines 21-31: Mock `select_action/2` function (always returns placeholder)
  - **Needs complete rewrite** to implement behaviour pattern and three policies

- `/Users/speed/wf-substrate/src/wf_exec.erl:55-66` - **Executor state record**
  - Lines 55-66: `#exec_state{}` record definition with tokens, branch_map, join_counters
  - Lines 167: Scheduler call in `run_loop/4`
  - Lines 212: XOR_CHOOSE handler (currently ignores scheduler decision)
  - **Integration point**: Executor must use scheduler decisions for token and branch selection

- `/Users/speed/wf-substrate/src/wf_vm.erl:1-50` - **Bytecode type definitions**
  - Lines 9-21: Opcode type definitions (used for context)
  - No scheduler-specific types

- `/Users/speed/wf-substrate/test/wf_exec_tests.erl:294-302` - **Scheduler integration test**
  - Lines 294-302: Test for scheduler integration with XOR_CHOOSE
  - **Uses**: `deterministic` policy (always selects first branch)

### Specification Files

- `/Users/speed/wf-substrate/.wreckit/items/007-scheduler-policies/item.json:1-14` - **Item specification**
  - Lines 6-7: Three policies (deterministic, nondeterministic, replay)
  - Lines 8-10: Choice logging format: `{step_seq, enabled_set, chosen}`
  - Lines 11-12: Behaviour callback: `choose/2 :: (EnabledActions, SchedState) -> {Chosen, NewSchedState}`
  - Lines 13: Export requirements: `new/2`, `choose/2`, `get_log/1`, `from_log/1`

- `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1280-1300` - **Module specification**
  - Lines 1280-1300: Scheduler module responsibilities and key functions
  - Defines: `select_action(EnabledActions, Policy) -> {ok, action()}`
  - Policies: `deterministic` (stable ordering), `nondeterministic` (random + logging), `replay` (logged choices)

- `/Users/speed/wf-substrate/docs/SEMANTICS.md:1050-1068` - **Determinism rule**
  - Lines 1050-1068: Formal definition of determinism property
  - Under deterministic scheduler, same term + same ctx → same trace
  - Replay enabled by recording scheduler choices

### Files to Create

- `src/wf_sched.erl` - **Rewrite existing file** with behaviour pattern and three policy modules
  - `-callback choose/2` for behaviour
  - `wf_sched_deterministic.erl` - Deterministic policy implementation
  - `wf_sched_nondeterministic.erl` - Nondeterministic policy with choice logging
  - `wf_sched_replay.erl` - Replay policy with divergence detection
  - API functions: `new/2`, `choose/2`, `get_log/1`, `from_log/1`

- `test/wf_sched_tests.erl` - **Scheduler policy tests**
  - Test deterministic policy (stable ordering)
  - Test nondeterministic policy (randomness + logging)
  - Test replay policy (reproduces trace, detects divergence)
  - Test integration with executor

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only)

**Standard OTP Modules**:
- `rand` - For random number generation in nondeterministic policy
  - `rand:uniform(N)` - Select random integer 1..N
  - `rand:seed(exro928, {A, B, C})` - Configurable seed for reproducibility
- `lists` - For sorting enabled actions in deterministic policy
  - `lists:sort/1`, `lists:keysort/2` - Stable ordering

**Internal Module Dependencies**:
- **wf_exec (item 005)**: Consumer of scheduler decisions
  - Executor calls `wf_sched:select_action/2` before each step
  - Must update executor to use scheduler decisions (currently ignored)
- **wf_trace (item 011)**: Choice logging for nondeterministic policy
  - Can defer to simple term logging for v1
  - Choice log: `[{step_seq, enabled_set, chosen}, ...]`

### Behaviour Pattern Design

**From item.json:11-12**:

> "The scheduler is a behaviour with callback choose/2 :: (EnabledActions, SchedState) -> {Chosen, NewSchedState}. Each policy is a module implementing this behaviour."

**Behaviour Definition**:

```erlang
%% wf_sched.erl

-callback choose(EnabledActions, SchedState) -> {Chosen, NewSchedState}
    when EnabledActions :: [enabled_action()],
         SchedState :: term(),
         Chosen :: enabled_action(),
         NewSchedState :: term().

-type enabled_action() ::
    {token, token_id()} |
    {xor_branch, pos_integer()}.

-type sched_state() :: term().

-type choice_log() :: [{step_seq(), enabled_set(), chosen()}].

-type step_seq() :: non_neg_integer().

-type enabled_set() :: [enabled_action()].

-type chosen() :: enabled_action().
```

**Policy Module Structure**:

Each policy module implements the behaviour:

```erlang
%% wf_sched_deterministic.erl
-module(wf_sched_deterministic).
-behaviour(wf_sched).

-export([choose/2]).

%% For deterministic policy, state is unused (always returns same choice for same set)
choose(EnabledActions, _State) ->
    %% Sort by action type, then by ID/index for stable ordering
    Sorted = sort_actions(EnabledActions),
    {hd(Sorted), undefined}.

%% Private helper
sort_actions(Actions) ->
    %% Sort tokens by token_id, xor branches by index
    lists:keysort(1, Actions).
```

### State Management

**From item.json:13**:

> "Export: new/2 (policy, options), choose/2, get_log/1, from_log/1 (create replay scheduler from recorded log)."

**API Design**:

```erlang
%% wf_sched.erl (API module)

%% Create new scheduler instance
-spec new(Policy :: sched_policy(), Options :: proplists:proplist()) ->
    {ok, sched_state()}.
new(deterministic, Options) ->
    %% Deterministic: no state needed
    {ok, undefined};
new(nondeterministic, Options) ->
    %% Nondeterministic: initialize rand seed, empty choice log
    Seed = proplists:get_value(seed, Options, exro928),
    rand:seed(exro928, Seed),
    {ok, #{log => [], step_seq => 0}};
new({replay, ChoiceLog}, Options) ->
    %% Replay: load choice log
    {ok, #{log => ChoiceLog, position => 1}}.

%% Select next action (delegates to policy module)
-spec choose(EnabledActions, sched_state()) -> {chosen(), sched_state()}.
choose(EnabledActions, State) ->
    %% Dispatch to policy implementation
    PolicyModule = policy_module_from_state(State),
    PolicyModule:choose(EnabledActions, State).

%% Get choice log (for nondeterministic policy)
-spec get_log(sched_state()) -> choice_log().
get_log(#{log := Log}) ->
    lists:reverse(Log).  %% Return in chronological order

%% Create replay scheduler from recorded log
-spec from_log(choice_log()) -> sched_state().
from_log(ChoiceLog) ->
    #{log => ChoiceLog, position => 1}.
```

### Deterministic Policy Implementation

**From item.json:3** and **ARCHITECTURE.md:1298**:

> "Deterministic: stable ordering of enabled actions (sorted by label/ID). Given the same set of enabled actions, always picks the same one. Useful for testing and reproducibility."

**Algorithm**:

1. Receive enabled actions list (e.g., `[{token, token_id1}, {token, token_id2}, {xor_branch, 1}]`)
2. Sort actions by:
   - Primary: Action type (atom ordering: `token < xor_branch`)
   - Secondary: ID/index (numeric or term ordering)
3. Return first action in sorted list
4. State is unused (deterministic choices are stateless)

**Implementation**:

```erlang
%% wf_sched_deterministic.erl

choose(EnabledActions, _State) ->
    %% Stable sort: first by action type, then by ID/index
    Sorted = lists:sort(
        fun({A, IdA}, {B, IdB}) when A < B -> true;
           ({A, IdA}, {A, IdB}) -> IdA < IdB;
           (_, _) -> false
        end,
        EnabledActions
    ),
    {hd(Sorted), undefined}.
```

**Key Points**:
- Same enabled set → same choice (deterministic)
- No state mutations
- Enables reproducible traces for testing

### Nondeterministic Policy Implementation

**From item.json:4** and **ARCHITECTURE.md:1299**:

> "Nondeterministic: random selection among enabled actions with recorded choices. Every choice is logged as {step_seq, enabled_set, chosen} for later replay. Uses rand module with configurable seed."

**Algorithm**:

1. Receive enabled actions list and state (containing log and step_seq)
2. Use `rand:uniform(N)` to select random action (N = length of enabled actions)
3. Log choice: `{step_seq, enabled_actions, chosen}`
4. Return chosen action and updated state (log appended, step_seq incremented)
5. Support configurable seed via `new/2` options

**Implementation**:

```erlang
%% wf_sched_nondeterministic.erl

choose(EnabledActions, #{log := Log, step_seq := StepSeq} = State) ->
    %% Random selection
    N = length(EnabledActions),
    Index = rand:uniform(N),
    Chosen = lists:nth(Index, EnabledActions),

    %% Log choice for replay
    Choice = {StepSeq, EnabledActions, Chosen},
    NewLog = [Choice | Log],

    %% Update state
    NewState = State#{log => NewLog, step_seq => StepSeq + 1},

    {Chosen, NewState}.
```

**Key Points**:
- Uses `rand` module (std Erlang/OTP)
- Logs every choice for replay
- Step sequence enables ordering during replay
- Seed configured via `wf_sched:new(nondeterministic, [{seed, {A,B,C}}])`

### Replay Policy Implementation

**From item.json:5** and **ARCHITECTURE.md:1300**:

> "Replay: feed a recorded choice log, reproduce the exact execution trace. On each decision point, consumes the next recorded choice. Fails loudly if the enabled set doesn't match the recording (divergence detection)."

**Algorithm**:

1. Receive enabled actions list and state (containing log and position)
2. Fetch next choice from log at current position
3. **Validate**: Check if `enabled_actions` matches recorded `enabled_set`
   - If mismatch: throw `{divergence, ExpectedSet, ActualSet}`
4. Return recorded `chosen` action and updated state (position incremented)
5. If log exhausted: throw `{replay_complete, Position}`

**Implementation**:

```erlang
%% wf_sched_replay.erl

choose(EnabledActions, #{log := ChoiceLog, position := Position} = State) ->
    case Position > length(ChoiceLog) of
        true ->
            error({replay_complete, Position});
        false ->
            %% Fetch recorded choice
            {_StepSeq, RecordedEnabled, Chosen} = lists:nth(Position, ChoiceLog),

            %% Validate enabled set matches recording
            case EnabledActions =:= RecordedEnabled of
                false ->
                    error({divergence, RecordedEnabled, EnabledActions});
                true ->
                    %% Match! Return recorded choice
                    NewState = State#{position := Position + 1},
                    {Chosen, NewState}
            end
    end.
```

**Key Points**:
- **Divergence detection**: Validates enabled sets match exactly
- Fails loudly if workflow behavior differs from recording
- Enables replay debugging (find where execution diverged)
- Step sequence not used for ordering (position in log suffices)

### Integration with Executor

**Current Problem** (from `/Users/speed/wf-substrate/src/wf_exec.erl:290-297`):

Executor ignores scheduler decision:

```erlang
execute_xor_choose({_XorChoose, TargetIPs}, ExecState, _SchedDecision) ->
    %% Scheduler decision contains selected branch index
    %% For now, always select first branch (deterministic)
    SelectedIndex = 1,  %% BUG: ignores SchedDecision
    SelectedIP = lists:nth(SelectedIndex, TargetIPs),
    ...
```

**Required Changes**:

1. **Update executor to use scheduler decisions**:
   ```erlang
   execute_xor_choose({_XorChoose, TargetIPs}, ExecState, SchedDecision) ->
       %% Extract branch index from scheduler decision
       {xor_branch, SelectedIndex} = SchedDecision,
       SelectedIP = lists:nth(SelectedIndex, TargetIPs),
       ...
   ```

2. **Add token selection in executor**:
   ```erlang
   run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
       %% Get enabled actions (all active tokens)
       EnabledActions = get_enabled_actions(ExecState0),

       %% Select next action via scheduler
       SchedState = ExecState0#exec_state.sched_state,
       {SchedDecision, NewSchedState} = wf_sched:choose(EnabledActions, SchedState),

       %% Execute selected action
       case SchedDecision of
           {token, TokenId} -> execute_token(TokenId, ExecState0);
           {xor_branch, Index} -> execute_xor_branch(Index, ExecState0)
       end,
       ...
   ```

3. **Add sched_state field to exec_state**:
   ```erlang
   -record(exec_state, {
       ...
       sched_state :: wf_sched:sched_state()  %% Add this field
   }).
   ```

**Note**: This integration is **out of scope** for item 007 but must be done in item 005 (executor) or a follow-up task.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Behaviour pattern complexity** | Medium | Erlang behaviours are well-understood. Follow OTP gen_server pattern. Document clearly with examples. |
| **State management bugs** | High | Each policy manages state differently. Use opaque types, document state structure. Property-based tests for state transitions. |
| **Deterministic policy not truly deterministic** | High | Sorting must be stable. Use `lists:sort/1` with consistent comparison function. Test with same enabled set multiple times. |
| **Nondeterministic policy seed issues** | Medium | `rand:seed/2` syntax varies by OTP version. Use `exro928` algorithm (default in OTP 26+). Document seed format. |
| **Replay policy false positives** | High | Divergence detection might fail due to minor differences (e.g., token ID ordering). Mitigation: Sort enabled sets before comparison, or use set equivalence (`ordsets`) instead of list equality. |
| **Choice log memory growth** | Medium | Long-running workflows accumulate large logs. Mitigation: Limit log size (circular buffer), or stream to disk (item 011). |
| **Executor integration bugs** | High | Executor currently ignores scheduler decisions. Mitigation: Update executor in separate task (item 005 follow-up). Add integration tests. |
| **Performance overhead** | Medium | Sorting and logging add overhead per step. Mitigation: Profile hot paths. Optimize deterministic sort (use `ordsets` for O(1) min). |
| **Replay log corruption** | High | Invalid replay logs cause crashes. Mitigation: Validate log format in `from_log/1`. Add try/catch in replay policy. |
| **Concurrency issues** | Low | Scheduler is single-threaded (called by executor). No shared state. Mitigation: Ensure policy state is immutable (functional updates). |
| **Backward compatibility** | Medium | Stub `select_action/2` currently used by executor. Mitigation: Keep old API as deprecated wrapper. Add migration guide. |

## Recommended Approach

**High-Level Strategy**: Implement behaviour pattern with three policy modules. Start with deterministic policy (simplest), then nondeterministic (adds state and logging), then replay (adds validation). Use opaque types and strict state management. Update executor integration in follow-up task.

### Phase 1: Behaviour Definition and API Skeleton

**1. Define behaviour callback** (`wf_sched.erl`):

```erlang
-callback choose(EnabledActions, SchedState) -> {Chosen, NewSchedState}
    when EnabledActions :: [enabled_action()],
         SchedState :: term(),
         Chosen :: enabled_action(),
         NewSchedState :: term().
```

**2. Define types**:

```erlang
-type enabled_action() :: {token, token_id()} | {xor_branch, pos_integer()}.
-type sched_policy() :: deterministic | nondeterministic | {replay, choice_log()}.
-type sched_state() :: term().
-type choice_log() :: [{step_seq(), enabled_set(), chosen()}].
```

**3. Implement API functions** (stubs initially):

```erlang
-spec new(sched_policy(), proplists:proplist()) -> {ok, sched_state()}.
new(Policy, Options) ->
    %% Dispatch to policy initialization
    ok.

-spec choose([enabled_action()], sched_state()) -> {enabled_action(), sched_state()}.
choose(EnabledActions, State) ->
    %% Dispatch to policy module
    ok.

-spec get_log(sched_state()) -> choice_log().
get_log(State) ->
    ok.

-spec from_log(choice_log()) -> sched_state().
from_log(Log) ->
    ok.
```

**4. Test behaviour compilation**:

```erlang
%% Test that behaviour compiles
wf_sched_behaviour_test() ->
    ?assert(true).  %% Compilation test
```

### Phase 2: Deterministic Policy

**1. Create `wf_sched_deterministic.erl`**:

```erlang
-module(wf_sched_deterministic).
-behaviour(wf_sched).

-export([choose/2]).

choose(EnabledActions, _State) ->
    %% Sort for stable ordering
    Sorted = stable_sort(EnabledActions),
    {hd(Sorted), undefined}.

stable_sort(Actions) ->
    %% Sort by action type, then ID/index
    lists:sort(fun compare_actions/2, Actions).

compare_actions({A, IdA}, {B, IdB}) when A < B -> true;
compare_actions({A, IdA}, {A, IdB}) -> IdA < IdB;
compare_actions(_, _) -> false.
```

**2. Test deterministic policy**:

```erlang
deterministic_test_() ->
    %% Test: same enabled set → same choice
    Enabled = [{token, ref1}, {xor_branch, 2}, {token, ref2}],
    {Choice1, _} = wf_sched_deterministic:choose(Enabled, undefined),
    {Choice2, _} = wf_sched_deterministic:choose(Enabled, undefined),
    ?assertEqual(Choice1, Choice2).

deterministic_ordering_test_() ->
    %% Test: choices are sorted correctly
    Enabled = [{token, ref2}, {token, ref1}, {xor_branch, 1}],
    {Choice, _} = wf_sched_deterministic:choose(Enabled, undefined),
    ?assertEqual({token, ref1}, Choice).  %% token < xor_branch, ref1 < ref2
```

**3. Update API `new/2` for deterministic**:

```erlang
new(deterministic, _Options) ->
    {ok, undefined}.
```

### Phase 3: Nondeterministic Policy

**1. Create `wf_sched_nondeterministic.erl`**:

```erlang
-module(wf_sched_nondeterministic).
-behaviour(wf_sched).

-export([choose/2]).

choose(EnabledActions, #{log := Log, step_seq := StepSeq} = State) ->
    %% Random selection
    N = length(EnabledActions),
    Index = rand:uniform(N),
    Chosen = lists:nth(Index, EnabledActions),

    %% Log choice
    Choice = {StepSeq, EnabledActions, Chosen},
    NewLog = [Choice | Log],
    NewState = State#{log => NewLog, step_seq => StepSeq + 1},

    {Chosen, NewState}.
```

**2. Test nondeterministic policy**:

```erlang
nondeterministic_test_() ->
    %% Test: different choices possible
    Enabled = [{token, ref1}, {token, ref2}],
    State0 = #{log => [], step_seq => 0},
    {Choice1, State1} = wf_sched_nondeterministic:choose(Enabled, State0),
    {Choice2, State2} = wf_sched_nondeterministic:choose(Enabled, State1),
    %% Choices may differ (probabilistic test)
    ok.

nondeterministic_logging_test_() ->
    %% Test: choices are logged
    Enabled = [{token, ref1}, {xor_branch, 1}],
    State0 = #{log => [], step_seq => 0},
    {_Choice, State1} = wf_sched_nondeterministic:choose(Enabled, State0),
    Log = maps:get(log, State1),
    ?assertEqual(1, length(Log)),
    {0, LoggedEnabled, LoggedChoice} = hd(Log),
    ?assertEqual(Enabled, LoggedEnabled).
```

**3. Update API `new/2` for nondeterministic**:

```erlang
new(nondeterministic, Options) ->
    Seed = proplists:get_value(seed, Options, {0, 0, 0}),
    rand:seed(exro928, Seed),
    {ok, #{log => [], step_seq => 0}}.
```

**4. Implement `get_log/1`**:

```erlang
get_log(#{log := Log}) ->
    lists:reverse(Log).  %% Chronological order
```

### Phase 4: Replay Policy

**1. Create `wf_sched_replay.erl`**:

```erlang
-module(wf_sched_replay).
-behaviour(wf_sched).

-export([choose/2]).

choose(EnabledActions, #{log := ChoiceLog, position := Position} = State) ->
    case Position > length(ChoiceLog) of
        true ->
            error({replay_complete, Position});
        false ->
            {_StepSeq, RecordedEnabled, Chosen} = lists:nth(Position, ChoiceLog),

            %% Validate enabled set matches
            case EnabledActions =:= RecordedEnabled of
                false ->
                    error({divergence, RecordedEnabled, EnabledActions});
                true ->
                    NewState = State#{position := Position + 1},
                    {Chosen, NewState}
            end
    end.
```

**2. Test replay policy**:

```erlang
replay_test_() ->
    %% Test: reproduces recorded choices
    Enabled1 = [{token, ref1}, {token, ref2}],
    Enabled2 = [{xor_branch, 1}],
    ChoiceLog = [{0, Enabled1, {token, ref1}}, {1, Enabled2, {xor_branch, 1}}],
    State0 = #{log => ChoiceLog, position => 1},

    %% First choice
    {Choice1, State1} = wf_sched_replay:choose(Enabled1, State0),
    ?assertEqual({token, ref1}, Choice1),
    ?assertEqual(2, maps:get(position, State1)),

    %% Second choice
    {Choice2, State2} = wf_sched_replay:choose(Enabled2, State1),
    ?assertEqual({xor_branch, 1}, Choice2),
    ?assertEqual(3, maps:get(position, State2)).

replay_divergence_test_() ->
    %% Test: detects enabled set mismatch
    Enabled = [{token, ref1}],
    ChoiceLog = [{0, [{token, ref2}], {token, ref2}}],  %% Different set
    State0 = #{log => ChoiceLog, position => 1},
    ?assertError({divergence, _, _}, wf_sched_replay:choose(Enabled, State0)).

replay_complete_test_() ->
    %% Test: fails when log exhausted
    Enabled = [{token, ref1}],
    ChoiceLog = [{0, Enabled, {token, ref1}}],
    State0 = #{log => ChoiceLog, position => 2},  %% Past end
    ?assertError({replay_complete, 2}, wf_sched_replay:choose(Enabled, State0)).
```

**3. Update API `from_log/1`**:

```erlang
from_log(ChoiceLog) ->
    #{log => ChoiceLog, position => 1}.
```

**4. Update API `new/2` for replay**:

```erlang
new({replay, ChoiceLog}, _Options) ->
    {ok, from_log(ChoiceLog)}.
```

### Phase 5: Integration and Testing

**1. Update executor** (separate task, item 005 follow-up):

- Add `sched_state` field to `#exec_state{}`
- Update `run_loop/4` to call `wf_sched:choose/2`
- Update `execute_xor_choose/3` to use scheduler decision
- Add `get_enabled_actions/1` helper

**2. Integration tests**:

```erlang
%% Test: deterministic scheduler produces same trace
deterministic_reproducibility_test_() ->
    Bytecode = mock_bytecode_xor(),
    ExecState0 = wf_exec:new(Bytecode),
    {ok, SchedState} = wf_sched:new(deterministic, []),

    %% Run twice with same policy
    {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),
    {done, ExecState2} = wf_exec:run(ExecState0, 100, deterministic),

    %% Same trace
    ?assertEqual(wf_exec:get_step_count(ExecState1), wf_exec:get_step_count(ExecState2)).

%% Test: nondeterministic + replay produces same trace
nondeterministic_replay_test_() ->
    Bytecode = mock_bytecode_par(),
    ExecState0 = wf_exec:new(Bytecode),
    {ok, SchedState0} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),

    %% Run with nondeterministic
    {done, ExecState1, SchedState1} = wf_exec:run_with_sched(ExecState0, 100, SchedState0),
    ChoiceLog = wf_sched:get_log(SchedState1),

    %% Replay with recorded log
    {ok, ReplayState} = wf_sched:new({replay, ChoiceLog}, []),
    {done, ExecState2} = wf_exec:run_with_sched(ExecState0, 100, ReplayState),

    %% Same trace
    ?assertEqual(wf_exec:get_ctx(ExecState1), wf_exec:get_ctx(ExecState2)).
```

**3. Property-based tests** (using PropEr or custom):

```erlang
%% Property: deterministic scheduler always returns same choice for same set
prop_deterministic_stable() ->
    ?FORALL(EnabledSet, list(enabled_action_gen()),
        begin
            {Choice1, _} = wf_sched_deterministic:choose(EnabledSet, undefined),
            {Choice2, _} = wf_sched_deterministic:choose(EnabledSet, undefined),
            Choice1 =:= Choice2
        end).

%% Property: replay reproduces nondeterministic trace
prop_replay_matches_nondeterministic() ->
    ?FORALL({Seed, Bytecode}, {nat(), bytecode_gen()},
        begin
            {ok, SchedState0} = wf_sched:new(nondeterministic, [{seed, Seed}]),
            %% Run with nondeterministic
            {done, _, SchedState1} = wf_exec:run_with_sched(Bytecode, 1000, SchedState0),
            ChoiceLog = wf_sched:get_log(SchedState1),

            %% Replay
            {ok, ReplayState} = wf_sched:new({replay, ChoiceLog}, []),
            {done, _, _} = wf_exec:run_with_sched(Bytecode, 1000, ReplayState),

            %% Should succeed without divergence
            true
        end).
```

**4. Performance tests**:

```erlang
%% Measure: deterministic policy overhead
bench_deterministic() ->
    Enabled = [{token, make_ref()} || _ <- lists:seq(1, 100)],
    {Time, _} = timer:tc(fun() ->
        [wf_sched_deterministic:choose(Enabled, undefined) || _ <- lists:seq(1, 10000)]
    end),
    io:format("Deterministic: ~p microsec/choice~n", [Time/10000]).

%% Measure: nondeterministic policy overhead (with logging)
bench_nondeterministic() ->
    Enabled = [{token, make_ref()} || _ <- lists:seq(1, 100)],
    State0 = #{log => [], step_seq => 0},
    {Time, _} = timer:tc(fun() ->
        {_, State} = wf_sched_deterministic:choose(Enabled, State0),
        %% Iterate 10000 times (accumulating log)
        ok
    end),
    io:format("Nondeterministic: ~p microsec/choice~n", [Time/10000]).
```

## Open Questions

1. **Deterministic Sort Stability**: Should deterministic policy use `lists:sort/1` (stable in OTP 26+) or `lists:keysort/2` for guaranteed stability?
   - **Recommendation**: Use `lists:sort/1` with custom comparison function. OTP 26+ guarantees stable sort. Document OTP version requirement.

2. **Token ID Comparison**: How to compare `token_id()` values (refs are opaque)?
   - **Option 1**: Convert refs to binaries using `term_to_binary/1` for comparison
   - **Option 2**: Use ref creation time (refs are partially ordered by creation time)
   - **Recommendation**: Option 1 (binary conversion) for deterministic ordering. Refs have internal ordering but it's not guaranteed across Erlang versions.

3. **Enabled Set Representation**: Should enabled actions be a list or set?
   - **List**: Preserves duplicates, allows ordering
   - **Set**: No duplicates, faster membership test
   - **Recommendation**: List (as specified in item.json). Preserves order, simpler for small N (< 100 actions typically).

4. **Choice Log Format**: Should choice log use `{step_seq, enabled_set, chosen}` or `{step_seq, enabled_count, chosen_index}`?
   - **Full set**: Enables divergence detection (validates exact enabled set)
   - **Count + index**: More compact, no divergence detection
   - **Recommendation**: Full set (as specified). Divergence detection is critical for replay debugging.

5. **Replay Divergence Tolerance**: Should replay policy tolerate minor differences (e.g., token ID ordering) or fail strictly?
   - **Strict**: Fail on any mismatch (current design)
   - **Lenient**: Use set equivalence (ignore order/duplicates)
   - **Recommendation**: Strict (current design). Divergence indicates workflow behavior changed, which should be detected. Add lenient mode as option if needed.

6. **State Opacity**: Should `sched_state()` be fully opaque or allow introspection for debugging?
   - **Opaque**: Better abstraction, policy state hidden
   - **Transparent**: Allows debugging (e.g., inspect log, position)
   - **Recommendation**: Opaque with accessor functions (`get_log/1`, `get_position/1`). Balances abstraction and debuggability.

7. **Seed Format**: What is the format of `rand:seed/2` algorithm parameter?
   - **OTP 26+**: `exro928` is default and recommended
   - **Older OTP**: `exsss` or `exsp`
   - **Recommendation**: Use `exro928` (OTP 26+). Document minimum OTP version requirement. Add fallback for older versions if needed.

8. **Log Memory Limits**: Should choice log have maximum size to prevent memory exhaustion?
   - **Unlimited**: Simple, but risky for long-running workflows
   - **Circular buffer**: Fixed size, overwrites old choices
   - **Streaming**: Write to disk (item 011)
   - **Recommendation**: Unlimited for v1 (simplest). Add size limits in production via options.

9. **Executor Integration Timing**: When to update executor to use scheduler decisions?
   - **Now**: Update executor in same item (007)
   - **Later**: Follow-up task for executor (005)
   - **Recommendation**: Later (item 005 follow-up). Item 007 scope is scheduler implementation only. Executor integration is separate concern.

10. **Backward Compatibility**: Should stub `select_action/2` be maintained for compatibility?
    - **Keep**: Old tests continue working
    - **Remove**: Clean break, force migration
    - **Recommendation**: Deprecate with warning logger. Remove in next major version.

11. **Error Handling**: Should policies throw errors or return `{error, Reason}` tuples?
    - **Throw**: Erlang convention for "let it crash"
    - **Return**: Explicit error handling
    - **Recommendation**: Throw for fatal errors (divergence, replay_complete). Return errors for recoverable issues (if any).

12. **Performance Profiling**: What is acceptable overhead per scheduler choice?
    - **Target**: < 10 microseconds per choice
    - **Measurement**: Use `timer:tc/1` to profile
    - **Optimization**: If > 10 μs, optimize sorting (use `ordsets` for deterministic, avoid logging in hot path)

13. **Testing Randomness**: How to test nondeterministic policy without flaky tests?
    - **Fixed seed**: Always use known seed in tests
    - **Property-based**: Use statistical tests (distribution uniformity)
    - **Recommendation**: Fixed seed for unit tests. Property-based tests for distribution validation.

14. **Replay Log Validation**: Should `from_log/1` validate log format?
    - **Strict**: Validate all entries have correct structure
    - **Lenient**: Trust caller, validate lazily
    - **Recommendation**: Strict validation in `from_log/1`. Fails fast if log is corrupted.

15. **Multicore Considerations**: Is scheduler thread-safe for concurrent execution?
    - **Single-threaded**: Called by executor (single process)
    - **Multi-threaded**: Multiple executors might share scheduler
    - **Recommendation**: Single-threaded (current design). State is immutable per instance. No locks needed.

16. **Scheduler State Persistence**: Should scheduler state be persisted (e.g., to ETS) for crash recovery?
    - **In-memory**: Lost on crash
    - **Persisted**: Survives crash, enables recovery
    - **Recommendation**: In-memory for v1. Add persistence in item 011 (tracing) as part of crash recovery.

17. **Choice Log Serialization**: Should choice log be serializable (e.g., for network transmission)?
    - **Opaque term**: Simple, Erlang-specific
    - **External format**: JSON, MessagePack
    - **Recommendation**: Opaque term for v1. Add serialization in item 011 if needed for distributed tracing.

18. **Action Type Extensibility**: Should `enabled_action()` type be extensible for future action types?
    - **Closed**: Only `{token, _}` and `{xor_branch, _}`
    - **Open**: Allow custom action types
    - **Recommendation**: Closed (current design). Only two action types needed for kernel primitives. Add new types if new patterns require them.
