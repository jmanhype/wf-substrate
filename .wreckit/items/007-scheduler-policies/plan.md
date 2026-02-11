# Implement pluggable scheduler policies Implementation Plan

## Implementation Plan Title

Pluggable Scheduler Policies for Workflow Executor

## Overview

Implement a behaviour-based scheduler system for the workflow executor that determines which enabled action to execute next when multiple choices are available. The scheduler supports three policies: deterministic (stable ordering for reproducibility), nondeterministic (random selection with choice logging), and replay (reproduces exact traces from recorded choices).

This is a foundational feature for testing, debugging, and replay capabilities in the workflow substrate.

## Current State

**Existing Implementation** (`/Users/speed/wf-substrate/src/wf_sched.erl:1-32`):
- Minimal stub with `select_action/2` that returns mock decisions
- No behaviour pattern or policy modules
- No state management or choice logging
- Executor ignores scheduler decisions (hardcoded choices)

**Executor Integration Points**:
- `/Users/speed/wf-substrate/src/wf_exec.erl:167` - Calls `wf_sched:select_action/2` but ignores result
- `/Users/speed/wf-substrate/src/wf_exec.erl:292` - `select_next_token/1` hardcodes first token selection
- `/Users/speed/wf-substrate/src/wf_exec.erl:443` - XOR_CHOOSE ignores scheduler decision (hardcoded index 1)

**Test Coverage**:
- `/Users/speed/wf-substrate/test/wf_exec_tests.erl:294-302` - Basic test using `deterministic` policy

### Key Discoveries:

- **Scheduler called but ignored**: Executor invokes `wf_sched:select_action/2` but uses hardcoded choices instead
- **Two decision types needed**: Token selection (multiple active tokens) and XOR branch selection
- **Token IDs are refs**: Created via `make_ref()`, need deterministic ordering via `term_to_binary/1`
- **No enabled actions extraction**: Executor doesn't currently extract enabled actions to pass to scheduler
- **Choice log format specified**: `{step_seq, enabled_set, chosen}` for replay capability

## Desired End State

A complete scheduler behaviour system with three policy implementations:

1. **Behaviour Module** (`wf_sched.erl`):
   - Defines `-callback choose/2` for policy modules
   - Exports: `new/2`, `choose/2`, `get_log/1`, `from_log/1`
   - Manages scheduler state opaquely

2. **Policy Modules**:
   - `wf_sched_deterministic.erl` - Stable sorting, stateless
   - `wf_sched_nondeterministic.erl` - Random selection with choice logging
   - `wf_sched_replay.erl` - Replay from log with divergence detection

3. **Test Coverage**:
   - Unit tests for each policy
   - Property-based tests for determinism and replay
   - Integration tests with executor

### Verification Criteria:

- [ ] All three policies implement `choose/2` callback correctly
- [ ] Deterministic policy: same enabled set → same choice (100% reproducible)
- [ ] Nondeterministic policy: logs all choices in correct format
- [ ] Replay policy: reproduces exact trace, detects divergence
- [ ] All existing tests pass
- [ ] Dialyzer type checking passes
- [ ] New tests achieve >90% code coverage

## What We're NOT Doing

**Out of Scope**:
- ❌ Updating executor to use scheduler decisions (separate task)
- ❌ Integration with wf_trace module (item 011)
- ❌ Log persistence or disk storage
- ❌ Distributed scheduler across nodes
- ❌ Performance optimization beyond basic requirements
- ❌ Configurable sort order for deterministic policy
- ❌ Circular buffer or log size limits
- ❌ Serialization format for choice logs (using Erlang terms)

**Deferred**:
- ⏳ Executor integration (update `select_next_token/1`, `execute_xor_choose/3`)
- ⏳ Scheduler state field in `#exec_state{}`
- ⏳ Integration tests with real executor workflows

## Implementation Approach

**Strategy**: Implement behaviour pattern with three policy modules incrementally. Start with deterministic (simplest, stateless), then nondeterministic (adds state and logging), then replay (adds validation). Keep executor integration separate to limit scope.

**Rationale**:
- Behaviour pattern allows independent testing of policies
- Stateless deterministic policy is easiest to verify
- Choice log format shared between nondeterministic and replay
- Executor integration deferred to avoid circular dependencies

---

## Phases

### Phase 1: Behaviour Definition and Core API

#### Overview

Define the scheduler behaviour callback, type specifications, and API skeleton. This establishes the contract that all policy modules must implement.

#### Changes Required:

##### 1. wf_sched.erl - Behaviour Module

**File**: `/Users/speed/wf-substrate/src/wf_sched.erl`
**Changes**: Complete rewrite to define behaviour and API

```erlang
-module(wf_sched).

%%====================================================================
%% Behaviour Definition
%%====================================================================

-callback choose(EnabledActions :: [enabled_action()],
                 SchedState :: sched_state()) ->
    {Chosen :: enabled_action(), NewSchedState :: sched_state()}.

%%====================================================================
%% Types
%%====================================================================

-type enabled_action() ::
    {token, token_id()} |
    {xor_branch, pos_integer()}.

-type token_id() :: term().

-type sched_policy() ::
    deterministic |
    nondeterministic |
    {replay, choice_log()}.

-type sched_state() :: term().

-type choice_log() :: [choice_entry()].

-type choice_entry() :: {
    StepSeq :: non_neg_integer(),
    EnabledSet :: [enabled_action()],
    Chosen :: enabled_action()
}.

%%====================================================================
%% Exports
%%====================================================================

-export_type([
    enabled_action/0,
    sched_policy/0,
    sched_state/0,
    choice_log/0,
    choice_entry/0
]).

-export([
    new/2,
    choose/2,
    get_log/1,
    from_log/1
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new scheduler state for given policy
-spec new(sched_policy(), proplists:proplist()) -> {ok, sched_state()}.
new(Policy, Options) ->
    case Policy of
        deterministic ->
            wf_sched_deterministic:new(Options);
        nondeterministic ->
            wf_sched_nondeterministic:new(Options);
        {replay, ChoiceLog} ->
            wf_sched_replay:new(ChoiceLog, Options)
    end.

%% @doc Choose next action from enabled set
-spec choose([enabled_action()], sched_state()) ->
    {enabled_action(), sched_state()}.
choose(EnabledActions, SchedState) when is_list(EnabledActions) ->
    case EnabledActions of
        [] ->
            error({no_enabled_actions, SchedState});
        [Single] ->
            %% Only one choice, no need to invoke policy
            {Single, SchedState};
        _Multiple ->
            %% Dispatch to policy module
            PolicyMod = extract_policy_module(SchedState),
            PolicyMod:choose(EnabledActions, SchedState)
    end.

%% @doc Extract choice log from scheduler state (nondeterministic only)
-spec get_log(sched_state()) -> choice_log().
get_log(SchedState) ->
    case SchedState of
        #{log := Log} ->
            lists:reverse(Log);
        _ ->
            []
    end.

%% @doc Create replay scheduler from recorded choice log
-spec from_log(choice_log()) -> sched_state().
from_log(ChoiceLog) when is_list(ChoiceLog) ->
    #{log => ChoiceLog, position => 1, policy => replay}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Extract policy module from state
-spec extract_policy_module(sched_state()) -> module().
extract_policy_module(#{policy := deterministic}) ->
    wf_sched_deterministic;
extract_policy_module(#{policy := nondeterministic}) ->
    wf_sched_nondeterministic;
extract_policy_module(#{policy := replay}) ->
    wf_sched_replay;
extract_policy_module(undefined) ->
    wf_sched_deterministic;
extract_policy_module(_State) ->
    %% Default to deterministic for backward compatibility
    wf_sched_deterministic.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc src/wf_sched.erl`
- [ ] Dialyzer passes: `dialyzer src/wf_sched.erl`
- [ ] Type specs are valid
- [ ] Behaviour callback is recognized by compiler

##### Manual Verification:

- [ ] Behaviour definition matches specification in item.json
- [ ] Type definitions cover all required concepts
- [ ] API matches specification: `new/2`, `choose/2`, `get_log/1`, `from_log/1`

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Deterministic Policy Implementation

#### Overview

Implement the deterministic scheduler policy that provides stable, reproducible ordering of enabled actions. This policy is stateless and always returns the same choice for the same enabled set.

#### Changes Required:

##### 1. wf_sched_deterministic.erl - Deterministic Policy Module

**File**: `/Users/speed/wf-substrate/src/wf_sched_deterministic.erl`
**Changes**: New file

```erlang
-module(wf_sched_deterministic).
-behaviour(wf_sched).

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/1,
    choose/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create deterministic scheduler state (stateless)
-spec new(proplists:proplist()) -> {ok, wf_sched:sched_state()}.
new(_Options) ->
    %% Deterministic policy is stateless
    {ok, #{policy => deterministic}}.

%% @doc Choose action with stable ordering
%% Sorts enabled actions: first by type (token < xor_branch), then by ID
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, State) when is_list(EnabledActions), length(EnabledActions) > 0 ->
    %% Stable sort: deterministic ordering
    Sorted = stable_sort(EnabledActions),
    {hd(Sorted), State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Stable sort of enabled actions
%% Ordering: token < xor_branch, then by ID/index
-spec stable_sort([wf_sched:enabled_action()]) -> [wf_sched:enabled_action()].
stable_sort(Actions) ->
    lists:sort(fun compare_actions/2, Actions).

%% @private Compare two enabled actions for ordering
-spec compare_actions(wf_sched:enabled_action(), wf_sched:enabled_action()) -> boolean().
compare_actions({TypeA, IdA}, {TypeB, IdB}) ->
    if
        TypeA < TypeB ->
            true;
        TypeA > TypeB ->
            false;
        TypeA =:= TypeB ->
            %% Same type, compare IDs
            %% Convert refs to binaries for deterministic ordering
            compare_ids(IdA, IdB)
    end.

%% @private Compare two IDs (may be refs, integers, or other terms)
-spec compare_ids(term(), term()) -> boolean().
compare_ids(IdA, IdB) when is_reference(IdA), is_reference(IdB) ->
    %% Refs need conversion for deterministic ordering
    term_to_binary(IdA) < term_to_binary(IdB);
compare_ids(IdA, IdB) ->
    %% Other types use standard Erlang ordering
    IdA < IdB.
```

##### 2. wf_sched_tests.erl - Deterministic Policy Tests

**File**: `/Users/speed/wf-substrate/test/wf_sched_tests.erl`
**Changes**: New file

```erlang
-module(wf_sched_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Deterministic Policy Tests
%%====================================================================

deterministic_new_test() ->
    {ok, State} = wf_sched_deterministic:new([]),
    ?assertEqual(#{policy => deterministic}, State).

deterministic_single_choice_test() ->
    {ok, State} = wf_sched_deterministic:new([]),
    Enabled = [{token, ref1}],
    {Choice, NewState} = wf_sched_deterministic:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice),
    ?assertEqual(State, NewState).

deterministic_stable_ordering_test() ->
    {ok, State} = wf_sched_deterministic:new([]),
    %% Test ordering: token < xor_branch
    Enabled = [{xor_branch, 1}, {token, ref1}],
    {Choice, _} = wf_sched_deterministic:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice).

deterministic_token_id_ordering_test() ->
    {ok, State} = wf_sched_deterministic:new([]),
    %% Test token ID ordering (refs converted to binaries)
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Enabled = [{token, Ref2}, {token, Ref1}],
    {Choice, _} = wf_sched_deterministic:choose(Enabled, State),
    %% Should pick the "smaller" ref
    Expected = case term_to_binary(Ref1) < term_to_binary(Ref2) of
        true -> {token, Ref1};
        false -> {token, Ref2}
    end,
    ?assertEqual(Expected, Choice).

deterministic_reproducibility_test() ->
    {ok, State} = wf_sched_deterministic:new([]),
    %% Same enabled set should always produce same choice
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Enabled = [{token, Ref1}, {token, Ref2}, {xor_branch, 1}],

    {Choice1, _} = wf_sched_deterministic:choose(Enabled, State),
    {Choice2, _} = wf_sched_deterministic:choose(Enabled, State),
    {Choice3, _} = wf_sched_deterministic:choose(Enabled, State),

    ?assertEqual(Choice1, Choice2),
    ?assertEqual(Choice2, Choice3).

deterministic_xor_branch_ordering_test() ->
    {ok, State} = wf_sched_deterministic:new([]),
    %% Test XOR branch index ordering
    Enabled = [{xor_branch, 3}, {xor_branch, 1}, {xor_branch, 2}],
    {Choice, _} = wf_sched_deterministic:choose(Enabled, State),
    ?assertEqual({xor_branch, 1}, Choice).

%%====================================================================
%% API Integration Tests
%%====================================================================

api_new_deterministic_test() ->
    {ok, State} = wf_sched:new(deterministic, []),
    ?assertEqual(#{policy => deterministic}, State).

api_choose_single_test() ->
    {ok, State} = wf_sched:new(deterministic, []),
    Enabled = [{token, ref1}],
    {Choice, _} = wf_sched:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice).

api_choose_multiple_test() ->
    {ok, State} = wf_sched:new(deterministic, []),
    Enabled = [{xor_branch, 2}, {token, ref1}],
    {Choice, _} = wf_sched:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice).

api_empty_enabled_error_test() ->
    {ok, State} = wf_sched:new(deterministic, []),
    ?assertError({no_enabled_actions, _}, wf_sched:choose([], State)).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc src/wf_sched_deterministic.erl`
- [ ] All tests pass: `erl -noshell -eval "eunit:test([wf_sched_tests, wf_sched_deterministic], [verbose])" -s erlang halt`
- [ ] Dialyzer passes: `dialyzer src/wf_sched*.erl`
- [ ] Reproducibility test passes (same choice for same set)

##### Manual Verification:

- [ ] Stable ordering verified: token < xor_branch
- [ ] Token ID ordering deterministic (refs via binary conversion)
- [ ] XOR branch ordering deterministic (numeric order)
- [ ] No state mutations (stateless policy)

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Nondeterministic Policy Implementation

#### Overview

Implement the nondeterministic scheduler policy that randomly selects among enabled actions while logging all choices for replay capability. Uses the `rand` module with configurable seed.

#### Changes Required:

##### 1. wf_sched_nondeterministic.erl - Nondeterministic Policy Module

**File**: `/Users/speed/wf-substrate/src/wf_sched_nondeterministic.erl`
**Changes**: New file

```erlang
-module(wf_sched_nondeterministic).
-behaviour(wf_sched).

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/1,
    choose/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create nondeterministic scheduler state with optional seed
-spec new(proplists:proplist()) -> {ok, wf_sched:sched_state()}.
new(Options) ->
    %% Initialize random seed
    Seed = proplists:get_value(seed, Options),
    case Seed of
        undefined ->
            %% Use default seed (time-based)
            rand:seed_default(exro928);
        {A, B, C} when is_integer(A), is_integer(B), is_integer(C) ->
            %% Use provided seed
            rand:seed(exro928, {A, B, C})
    end,

    %% Initialize state with empty log
    {ok, #{
        policy => nondeterministic,
        log => [],
        step_seq => 0
    }}.

%% @doc Choose action randomly and log the choice
-spec choose([wf_sched:enabled_action()], wf_sched:sched_state()) ->
    {wf_sched:enabled_action(), wf_sched:sched_state()}.
choose(EnabledActions, #{log := Log, step_seq := StepSeq} = State)
    when is_list(EnabledActions), length(EnabledActions) > 0 ->

    %% Random selection: pick index 1..N
    N = length(EnabledActions),
    Index = rand:uniform(N),
    Chosen = lists:nth(Index, EnabledActions),

    %% Log choice for replay
    Entry = {StepSeq, EnabledActions, Chosen},
    NewLog = [Entry | Log],

    %% Update state
    NewState = State#{
        log => NewLog,
        step_seq => StepSeq + 1
    },

    {Chosen, NewState}.

%%====================================================================
%% Internal Functions
%%====================================================================
```

##### 2. wf_sched_tests.erl - Add Nondeterministic Tests

**File**: `/Users/speed/wf-substrate/test/wf_sched_tests.erl`
**Changes**: Append tests for nondeterministic policy

```erlang
%%====================================================================
%% Nondeterministic Policy Tests
%%====================================================================

nondeterministic_new_test() ->
    {ok, State} = wf_sched_nondeterministic:new([{seed, {1, 2, 3}}]),
    ?assertEqual(#{policy => nondeterministic, log => [], step_seq => 0}, State).

nondeterministic_single_choice_test() ->
    {ok, State} = wf_sched_nondeterministic:new([{seed, {1, 2, 3}}]),
    Enabled = [{token, ref1}],
    {Choice, NewState} = wf_sched_nondeterministic:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice),
    %% Check log
    ?assertEqual(1, length(maps:get(log, NewState))),
    ?assertEqual(1, maps:get(step_seq, NewState)).

nondeterministic_randomness_test() ->
    %% Test that different seeds produce different choices (probabilistic)
    {ok, State1} = wf_sched_nondeterministic:new([{seed, {1, 2, 3}}]),
    {ok, State2} = wf_sched_nondeterministic:new([{seed, {4, 5, 6}}]),

    Enabled = [{token, ref1}, {token, ref2}, {token, ref3}],

    {Choice1, _} = wf_sched_nondeterministic:choose(Enabled, State1),
    {Choice2, _} = wf_sched_nondeterministic:choose(Enabled, State2),

    %% Different seeds *should* produce different choices (high probability)
    %% We don't assert this strictly since it's probabilistic
    %% But we verify they are both valid choices
    ?assert(lists:member(Choice1, Enabled)),
    ?assert(lists:member(Choice2, Enabled)).

nondeterministic_logging_format_test() ->
    {ok, State} = wf_sched_nondeterministic:new([{seed, {1, 2, 3}}]),
    Enabled = [{token, ref1}, {xor_branch, 1}],

    {Choice, NewState} = wf_sched_nondeterministic:choose(Enabled, State),
    Log = maps:get(log, NewState),

    %% Verify log format: {step_seq, enabled_set, chosen}
    ?assertEqual(1, length(Log)),
    [{StepSeq, LoggedEnabled, LoggedChoice}] = Log,
    ?assertEqual(0, StepSeq),
    ?assertEqual(Enabled, LoggedEnabled),
    ?assertEqual(Choice, LoggedChoice).

nondeterministic_multiple_choices_test() ->
    {ok, State0} = wf_sched_nondeterministic:new([{seed, {1, 2, 3}}]),
    Enabled = [{token, ref1}, {token, ref2}],

    {_, State1} = wf_sched_nondeterministic:choose(Enabled, State0),
    {_, State2} = wf_sched_nondeterministic:choose(Enabled, State1),
    {_, State3} = wf_sched_nondeterministic:choose(Enabled, State2),

    %% Verify log accumulates
    Log = maps:get(log, State3),
    ?assertEqual(3, length(Log)),
    ?assertEqual(3, maps:get(step_seq, State3)).

nondeterministic_seed_reproducibility_test() ->
    %% Same seed should produce same sequence
    {ok, State1a} = wf_sched_nondeterministic:new([{seed, {42, 42, 42}}]),
    {ok, State1b} = wf_sched_nondeterministic:new([{seed, {42, 42, 42}}]),

    Enabled = [{token, ref1}, {token, ref2}, {token, ref3}],

    {C1a, S1a} = wf_sched_nondeterministic:choose(Enabled, State1a),
    {C1b, S1b} = wf_sched_nondeterministic:choose(Enabled, State1b),
    ?assertEqual(C1a, C1b),

    {C2a, _} = wf_sched_nondeterministic:choose(Enabled, S1a),
    {C2b, _} = wf_sched_nondeterministic:choose(Enabled, S1b),
    ?assertEqual(C2a, C2b).

%%====================================================================
%% API Integration Tests - Nondeterministic
%%====================================================================

api_new_nondeterministic_test() ->
    {ok, State} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),
    ?assertEqual(nondeterministic, maps:get(policy, State)),
    ?assertEqual([], maps:get(log, State)),
    ?assertEqual(0, maps:get(step_seq, State)).

api_get_log_test() ->
    {ok, State0} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),
    Enabled = [{token, ref1}, {token, ref2}],

    {_, State1} = wf_sched:choose(Enabled, State0),
    {_, State2} = wf_sched:choose(Enabled, State1),

    Log = wf_sched:get_log(State2),
    ?assertEqual(2, length(Log)),
    %% Log should be in chronological order (reversed from internal storage)
    [{0, _, _}, {1, _, _}] = Log.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc src/wf_sched_nondeterministic.erl`
- [ ] All tests pass: `erl -noshell -eval "eunit:test([wf_sched_tests, wf_sched_nondeterministic], [verbose])" -s erlang halt`
- [ ] Dialyzer passes: `dialyzer src/wf_sched*.erl`
- [ ] Choice logging format correct: `{step_seq, enabled_set, chosen}`
- [ ] Seed reproducibility test passes

##### Manual Verification:

- [ ] Random selection verified (different choices possible)
- [ ] Log accumulates correctly across multiple choices
- [ ] Step sequence increments properly
- [ ] API `get_log/1` returns chronological order

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Replay Policy Implementation

#### Overview

Implement the replay scheduler policy that reproduces exact execution traces from recorded choice logs. Validates enabled sets match recording and fails loudly on divergence.

#### Changes Required:

##### 1. wf_sched_replay.erl - Replay Policy Module

**File**: `/Users/speed/wf-substrate/src/wf_sched_replay.erl`
**Changes**: New file

```erlang
-module(wf_sched_replay).
-behaviour(wf_sched).

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/2
]).

%%====================================================================
%% API Functions
%%====================================================================

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

%%====================================================================
%% Behaviour Callback
%%====================================================================

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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate choice log format
-spec validate_log(wf_sched:choice_log()) -> ok | {error, term()}.
validate_log([]) ->
    ok;
validate_log([{StepSeq, EnabledSet, Chosen} | Rest])
    when is_integer(StepSeq), is_list(EnabledSet) ->
    %% Verify chosen action is in enabled set
    case lists:member(Chosen, EnabledSet) of
        true ->
            validate_log(Rest);
        false ->
            error({invalid_log, {chosen_not_in_enabled, Chosen, EnabledSet}})
    end;
validate_log([InvalidEntry | _]) ->
    error({invalid_log, {bad_entry, InvalidEntry}}).

%% @private Validate that actual enabled set matches recorded set
-spec validate_enabled_set([wf_sched:enabled_action()], [wf_sched:enabled_action()]) ->
    ok | {error, {divergence, term(), term()}}.
validate_enabled_set(Actual, Recorded) ->
    %% Use exact equality for strict divergence detection
    case Actual =:= Recorded of
        true ->
            ok;
        false ->
            {error, {divergence, Recorded, Actual}}
    end.
```

##### 2. wf_sched_tests.erl - Add Replay Tests

**File**: `/Users/speed/wf-substrate/test/wf_sched_tests.erl`
**Changes**: Append tests for replay policy

```erlang
%%====================================================================
%% Replay Policy Tests
%%====================================================================

replay_new_test() ->
    ChoiceLog = [{0, [{token, ref1}], {token, ref1}}],
    {ok, State} = wf_sched_replay:new(ChoiceLog, []),
    ?assertEqual(replay, maps:get(policy, State)),
    ?assertEqual(ChoiceLog, maps:get(log, State)),
    ?assertEqual(1, maps:get(position, State)).

replay_single_choice_test() ->
    Enabled = [{token, ref1}],
    ChoiceLog = [{0, Enabled, {token, ref1}}],
    {ok, State} = wf_sched_replay:new(ChoiceLog, []),

    {Choice, NewState} = wf_sched_replay:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice),
    ?assertEqual(2, maps:get(position, NewState)).

replay_multiple_choices_test() ->
    Enabled1 = [{token, ref1}, {token, ref2}],
    Enabled2 = [{xor_branch, 1}, {xor_branch, 2}],
    ChoiceLog = [
        {0, Enabled1, {token, ref1}},
        {1, Enabled2, {xor_branch, 2}}
    ],
    {ok, State0} = wf_sched_replay:new(ChoiceLog, []),

    {Choice1, State1} = wf_sched_replay:choose(Enabled1, State0),
    ?assertEqual({token, ref1}, Choice1),
    ?assertEqual(2, maps:get(position, State1)),

    {Choice2, State2} = wf_sched_replay:choose(Enabled2, State1),
    ?assertEqual({xor_branch, 2}, Choice2),
    ?assertEqual(3, maps:get(position, State2)).

replay_divergence_detection_test() ->
    RecordedEnabled = [{token, ref1}],
    ActualEnabled = [{token, ref2}],  %% Different!
    ChoiceLog = [{0, RecordedEnabled, {token, ref1}}],
    {ok, State} = wf_sched_replay:new(ChoiceLog, []),

    ?assertError(
        {divergence, RecordedEnabled, ActualEnabled},
        wf_sched_replay:choose(ActualEnabled, State)
    ).

replay_complete_test() ->
    Enabled = [{token, ref1}],
    ChoiceLog = [{0, Enabled, {token, ref1}}],
    {ok, State0} = wf_sched_replay:new(ChoiceLog, []),

    %% First choice succeeds
    {_, State1} = wf_sched_replay:choose(Enabled, State0),

    %% Second choice fails (log exhausted)
    ?assertError(
        {replay_complete, 2},
        wf_sched_replay:choose(Enabled, State1)
    ).

replay_log_validation_test() ->
    %% Valid log
    ValidLog = [{0, [{token, ref1}], {token, ref1}}],
    ?assertMatch({ok, _}, wf_sched_replay:new(ValidLog, [])),

    %% Invalid log: chosen not in enabled set
    InvalidLog = [{0, [{token, ref1}], {token, ref2}}],
    ?assertError({invalid_log, _}, wf_sched_replay:new(InvalidLog, [])),

    %% Invalid log: bad entry format
    BadLog = [{0, ref1}],  %% Missing enabled set
    ?assertError({invalid_log, _}, wf_sched_replay:new(BadLog, [])).

%%====================================================================
%% API Integration Tests - Replay
%%====================================================================

api_new_replay_test() ->
    ChoiceLog = [{0, [{token, ref1}], {token, ref1}}],
    {ok, State} = wf_sched:new({replay, ChoiceLog}, []),
    ?assertEqual(replay, maps:get(policy, State)).

api_from_log_test() ->
    ChoiceLog = [{0, [{token, ref1}], {token, ref1}}],
    State = wf_sched:from_log(ChoiceLog),
    ?assertEqual(replay, maps:get(policy, State)),
    ?assertEqual(ChoiceLog, maps:get(log, State)),
    ?assertEqual(1, maps:get(position, State)).

api_replay_full_cycle_test() ->
    %% Full cycle: nondeterministic -> log -> replay
    {ok, SchedState0} = wf_sched:new(nondeterministic, [{seed, {100, 200, 300}}]),
    Enabled = [{token, ref1}, {token, ref2}, {xor_branch, 1}],

    %% Make 3 choices with nondeterministic
    {C1, S1} = wf_sched:choose(Enabled, SchedState0),
    {C2, S2} = wf_sched:choose(Enabled, S1),
    {C3, S3} = wf_sched:choose(Enabled, S2),

    %% Extract log
    ChoiceLog = wf_sched:get_log(S3),
    ?assertEqual(3, length(ChoiceLog)),

    %% Replay with same log
    {ok, ReplayState0} = wf_sched:new({replay, ChoiceLog}, []),
    {R1, ReplayState1} = wf_sched:choose(Enabled, ReplayState0),
    {R2, ReplayState2} = wf_sched:choose(Enabled, ReplayState1),
    {R3, ReplayState3} = wf_sched:choose(Enabled, ReplayState2),

    %% Verify replay matches original choices
    ?assertEqual(C1, R1),
    ?assertEqual(C2, R2),
    ?assertEqual(C3, R3).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `erlc src/wf_sched_replay.erl`
- [ ] All tests pass: `erl -noshell -eval "eunit:test([wf_sched_tests, wf_sched_replay], [verbose])" -s erlang halt`
- [ ] Dialyzer passes: `dialyzer src/wf_sched*.erl`
- [ ] Divergence detection works
- [ ] Replay complete detection works
- [ ] Full cycle test (nondeterministic -> log -> replay) passes

##### Manual Verification:

- [ ] Replay reproduces exact choices
- [ ] Divergence detected when enabled sets differ
- [ ] Error when log exhausted
- [ ] Log validation catches malformed logs

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Final Integration and Documentation

#### Overview

Complete the implementation with backward compatibility, comprehensive test suite, and documentation. Ensure all existing tests continue to pass.

#### Changes Required:

##### 1. wf_sched.erl - Add Backward Compatibility

**File**: `/Users/speed/wf-substrate/src/wf_sched.erl`
**Changes**: Add deprecated `select_action/2` for backward compatibility

```erlang
%%====================================================================
%% Deprecated API (for backward compatibility)
%%====================================================================

%% @deprecated Use new/2 and choose/2 instead
%% @doc Legacy API for backward compatibility with existing code
-spec select_action(exec_state(), sched_policy()) -> sched_decision().
select_action(_ExecState, Policy) ->
    %% Issue deprecation warning
    error_logger:warning_msg("wf_sched:select_action/2 is deprecated, use new/2 and choose/2~n"),

    %% Create scheduler and return mock decision
    {ok, State} = new(normalize_policy(Policy), []),
    %% Return mock decision for backward compatibility
    case Policy of
        deterministic -> {token, mock_token};
        nondeterministic -> {token, mock_token};
        {replay, _} -> {token, replay_token};
        undefined -> {token, mock_token}
    end.

%% @private Normalize legacy policy atoms to new format
-spec normalize_policy(term()) -> sched_policy().
normalize_policy(undefined) -> deterministic;
normalize_policy(Policy) -> Policy.
```

##### 2. rebar.config - Enable Dialyzer

**File**: `/Users/speed/wf-substrate/rebar.config`
**Changes**: Add dialyzer configuration (if not already present)

```erlang
{dialyzer, [
    {warnings, [
        unmatched_returns,
        error_handling,
        race_conditions,
        underspecs
    ]}
]}.
```

##### 3. README.md - Update Documentation

**File**: `/Users/speed/wf-substrate/README.md`
**Changes**: Add scheduler documentation section

```markdown
## Scheduler Policies (wf_sched)

The scheduler determines which action to execute next when multiple are available.

### Policies

- **Deterministic**: Stable ordering (sorted by type, then ID). Always produces same choice for same enabled set.
- **Nondeterministic**: Random selection with choice logging. Logs format: `{step_seq, enabled_set, chosen}`.
- **Replay**: Reproduces exact trace from recorded choice log. Detects divergence.

### Usage

```erlang
%% Deterministic policy
{ok, SchedState} = wf_sched:new(deterministic, []),
{Choice, NewState} = wf_sched:choose([{token, ref1}, {token, ref2}], SchedState).

%% Nondeterministic policy with seed
{ok, SchedState} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),
{Choice, NewState} = wf_sched:choose(Enabled, SchedState),
Log = wf_sched:get_log(NewState).

%% Replay from log
ReplayState = wf_sched:from_log(Log),
{Choice, _} = wf_sched:choose(Enabled, ReplayState).
```
```

##### 4. wf_sched_tests.erl - Add Comprehensive Tests

**File**: `/Users/speed/wf-substrate/test/wf_sched_tests.erl`
**Changes**: Add edge case and property tests

```erlang
%%====================================================================
%% Edge Case Tests
%%====================================================================

empty_enabled_actions_test() ->
    {ok, State} = wf_sched:new(deterministic, []),
    ?assertError({no_enabled_actions, _}, wf_sched:choose([], State)).

single_action_no_policy_invocation_test() ->
    %% Single action should not invoke policy (optimization)
    {ok, State} = wf_sched:new(deterministic, []),
    Enabled = [{token, ref1}],
    {Choice, NewState} = wf_sched:choose(Enabled, State),
    ?assertEqual({token, ref1}, Choice),
    %% State should be unchanged (no policy invocation)
    ?assertEqual(State, NewState).

large_enabled_set_test() ->
    %% Test with many actions
    {ok, State} = wf_sched:new(deterministic, []),
    ManyTokens = [{token, make_ref()} || _ <- lists:seq(1, 100)],
    ManyBranches = [{xor_branch, N} || N <- lists:seq(1, 50)],
    Enabled = ManyTokens ++ ManyBranches,

    {Choice, _} = wf_sched:choose(Enabled, State),
    %% Should pick first token (sorted order)
    ?assertMatch({token, _}, Choice).

replay_empty_log_test() ->
    {ok, State} = wf_sched_replay:new([], []),
    %% Empty log should complete immediately
    ?assertError({replay_complete, 1}, wf_sched_replay:choose([{token, ref1}], State)).

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_deterministic_reproducibility_test() ->
    %% Property: deterministic always returns same choice for same set
    Enabled = [{token, make_ref()}, {token, make_ref()}, {xor_branch, 1}],
    {ok, State} = wf_sched:new(deterministic, []),

    Choices = [element(1, wf_sched:choose(Enabled, State)) || _ <- lists:seq(1, 100)],
    %% All choices should be identical
    ?assertEqual(lists:usort(Choices), [hd(Choices)]).

prop_nondeterministic_logging_complete_test() ->
    %% Property: all choices are logged in nondeterministic policy
    {ok, State0} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),
    Enabled = [{token, ref1}, {token, ref2}],

    {_, State1} = wf_sched:choose(Enabled, State0),
    {_, State2} = wf_sched:choose(Enabled, State1),
    {_, State3} = wf_sched:choose(Enabled, State2),

    Log = wf_sched:get_log(State3),
    %% All 3 choices should be logged
    ?assertEqual(3, length(Log)),
    %% Each entry should have valid format
    [?assertMatch({N, _, _} when is_integer(N), Entry) || Entry <- Log].
```

##### 5. Run Full Test Suite

**Command**: Run all tests to ensure no regressions

```bash
# Compile all modules
rebar3 compile

# Run dialyzer
rebar3 dialyzer

# Run all tests
rebar3 eunit

# Run specific scheduler tests
rebar3 eunit --module=wf_sched_tests
```

#### Success Criteria:

##### Automated Verification:

- [ ] All modules compile: `rebar3 compile`
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] All tests pass: `rebar3 eunit`
- [ ] Existing wf_exec_tests pass: `rebar3 eunit --module=wf_exec_tests`
- [ ] Code coverage >90% for scheduler modules

##### Manual Verification:

- [ ] Deprecation warning issued for legacy `select_action/2`
- [ ] Documentation clear and accurate
- [ ] All three policies work correctly
- [ ] No regressions in existing executor tests

**Note**: Complete all automated verification, then pause for manual confirmation before completing the implementation.

---

## Testing Strategy

### Unit Tests:

**Deterministic Policy**:
- Stable ordering (same choice for same enabled set)
- Token vs XOR branch ordering
- Token ID ordering (refs converted to binaries)
- Reproducibility across multiple invocations

**Nondeterministic Policy**:
- Randomness (different choices possible)
- Choice logging format `{step_seq, enabled_set, chosen}`
- Log accumulation across multiple choices
- Seed reproducibility (same seed → same sequence)

**Replay Policy**:
- Exact choice reproduction
- Divergence detection (enabled set mismatch)
- Replay complete detection (log exhausted)
- Log validation (malformed entries)

### Integration Tests:

**API Integration**:
- `new/2` for all three policies
- `choose/2` dispatches to correct policy
- `get_log/1` extracts log from nondeterministic state
- `from_log/1` creates replay state

**Full Cycle**:
- Nondeterministic → log → replay produces same choices
- Multiple interleaved choices

**Edge Cases**:
- Empty enabled set → error
- Single action → no policy invocation
- Large enabled sets (100+ actions)
- Empty replay log

### Manual Testing Steps:

1. **Deterministic Policy**:
   ```erlang
   {ok, S} = wf_sched:new(deterministic, []),
   Enabled = [{token, make_ref()}, {xor_branch, 1}],
   {C1, _} = wf_sched:choose(Enabled, S),
   {C2, _} = wf_sched:choose(Enabled, S),
   {C3, _} = wf_sched:choose(Enabled, S),
   %% Verify C1 = C2 = C3
   ```

2. **Nondeterministic Policy**:
   ```erlang
   {ok, S0} = wf_sched:new(nondeterministic, [{seed, {1,2,3}}]),
   Enabled = [{token, ref1}, {token, ref2}],
   {_, S1} = wf_sched:choose(Enabled, S0),
   {_, S2} = wf_sched:choose(Enabled, S1),
   Log = wf_sched:get_log(S2),
   %% Verify Log has 2 entries in chronological order
   ```

3. **Replay Policy**:
   ```erlang
   Log = [{0, [{token, ref1}], {token, ref1}}],
   {ok, S} = wf_sched:new({replay, Log}, []),
   {C, _} = wf_sched:choose([{token, ref1}], S),
   %% Verify C = {token, ref1}
   ```

4. **Full Cycle**:
   ```erlang
   %% Run workflow with nondeterministic, get log, replay
   {ok, S0} = wf_sched:new(nondeterministic, [{seed, {100,200,300}}]),
   Enabled = [{token, ref1}, {token, ref2}],
   {C1, S1} = wf_sched:choose(Enabled, S0),
   {C2, S2} = wf_sched:choose(Enabled, S1),
   Log = wf_sched:get_log(S2),

   {ok, R0} = wf_sched:new({replay, Log}, []),
   {R1, _} = wf_sched:choose(Enabled, R0),
   {R2, _} = wf_sched:choose(Enabled, R0),
   %% Verify C1 = R1, C2 = R2
   ```

## Migration Notes

**Backward Compatibility**:
- Legacy `select_action/2` maintained with deprecation warning
- Returns mock decisions for existing code
- No changes required to existing tests

**Future Integration** (Out of Scope):
- Executor will need `get_enabled_actions/1` helper to extract enabled actions
- Executor will need `sched_state` field in `#exec_state{}`
- `execute_xor_choose/3` will need to use scheduler decision
- `select_next_token/1` will need to use scheduler decision

**API Migration**:
```erlang
%% Old (deprecated):
Decision = wf_sched:select_action(ExecState, deterministic).

%% New:
{ok, SchedState} = wf_sched:new(deterministic, []),
Enabled = get_enabled_actions(ExecState),  %% Not yet implemented
{Decision, NewSchedState} = wf_sched:choose(Enabled, SchedState).
```

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/007-scheduler-policies/research.md`
- Item Spec: `/Users/speed/wf-substrate/.wreckit/items/007-scheduler-policies/item.json`
- Architecture: `/Users/speed/wf-substrate/docs/ARCHITECTURE.md:1280-1300`
- Semantics: `/Users/speed/wf-substrate/docs/SEMANTICS.md:1050-1068`
- Executor: `/Users/speed/wf-substrate/src/wf_exec.erl:167,292,443`
- Current Stub: `/Users/speed/wf-substrate/src/wf_sched.erl:1-32`
