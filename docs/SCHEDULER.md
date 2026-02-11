# Scheduler Policies (wf_sched)

The scheduler determines which action to execute next when multiple are available (e.g., multiple parallel branches ready, multiple XOR choices available).

## Policies

### Deterministic
Stable ordering (sorted by type, then ID). Always produces same choice for same enabled set. Useful for testing and reproducibility.

**Features:**
- Stateless policy
- Ordering: `token < xor_branch`
- Token ID ordering via `term_to_binary/1` conversion for refs
- XOR branch ordering by numeric index

### Nondeterministic
Random selection with choice logging. Logs format: `{step_seq, enabled_set, chosen}`.

**Features:**
- Configurable seed for reproducibility
- Logs all choices for replay
- Uses `exsss` random algorithm with explicit state

### Replay
Reproduces exact trace from recorded choice log. Detects divergence when enabled sets differ.

**Features:**
- Validates enabled sets match recording
- Fails on divergence: `{divergence, ExpectedSet, ActualSet}`
- Fails on log exhaustion: `{replay_complete, Position}`

## Usage

### Deterministic Policy

```erlang
%% Create scheduler
{ok, SchedState} = wf_sched:new(deterministic, []),

%% Choose from enabled actions
Enabled = [{token, ref1}, {token, ref2}, {xor_branch, 1}],
{Choice, NewState} = wf_sched:choose(Enabled, SchedState).

%% Choice will always be the same for same enabled set
%% Result: {token, ref1} (tokens sorted first, then ref1 < ref2)
```

### Nondeterministic Policy

```erlang
%% Create scheduler with seed
{ok, SchedState} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),

%% Make choices
Enabled = [{token, ref1}, {token, ref2}],
{Choice1, State1} = wf_sched:choose(Enabled, SchedState),
{Choice2, State2} = wf_sched:choose(Enabled, State1),

%% Extract choice log
Log = wf_sched:get_log(State2).
%% Log: [{0, Enabled, Choice1}, {1, Enabled, Choice2}]
```

### Replay Policy

```erlang
%% Create from recorded log
ChoiceLog = [
    {0, [{token, ref1}, {token, ref2}], {token, ref1}},
    {1, [{xor_branch, 1}, {xor_branch, 2}], {xor_branch, 2}}
],
{ok, ReplayState} = wf_sched:new({replay, ChoiceLog}, []),

%% Replay choices
{R1, State1} = wf_sched:choose([{token, ref1}, {token, ref2}], ReplayState),
%% R1 = {token, ref1} (matches recorded choice)

{R2, _State2} = wf_sched:choose([{xor_branch, 1}, {xor_branch, 2}], State1).
%% R2 = {xor_branch, 2} (matches recorded choice)
```

### Full Cycle: Record and Replay

```erlang
%% Run workflow with nondeterministic policy
{ok, SchedState0} = wf_sched:new(nondeterministic, [{seed, {100, 200, 300}}]),
Enabled = [{token, ref1}, {token, ref2}],

{C1, S1} = wf_sched:choose(Enabled, SchedState0),
{C2, S2} = wf_sched:choose(Enabled, S1),
{C3, S3} = wf_sched:choose(Enabled, S2),

%% Extract choice log
ChoiceLog = wf_sched:get_log(S3),

%% Replay with same log
{ok, ReplayState0} = wf_sched:new({replay, ChoiceLog}, []),
{R1, _} = wf_sched:choose(Enabled, ReplayState0),
{R2, _} = wf_sched:choose(Enabled, ReplayState0),
{R3, _} = wf_sched:choose(Enabled, ReplayState0).

%% Verify: C1 = R1, C2 = R2, C3 = R3
```

## API Reference

### `new/2`
Create new scheduler state for given policy.

```erlang
-spec new(sched_policy(), proplists:proplist()) -> {ok, sched_state()}.

%% Deterministic (stateless)
{ok, State} = wf_sched:new(deterministic, []).

%% Nondeterministic (with optional seed)
{ok, State} = wf_sched:new(nondeterministic, [{seed, {A, B, C}}]).

%% Replay (from choice log)
{ok, State} = wf_sched:new({replay, ChoiceLog}, []).
```

### `choose/2`
Choose next action from enabled set.

```erlang
-spec choose([enabled_action()], sched_state()) ->
    {enabled_action(), sched_state()}.

Enabled = [{token, ref1}, {token, ref2}],
{Choice, NewState} = wf_sched:choose(Enabled, State).
```

### `get_log/1`
Extract choice log from scheduler state (nondeterministic only).

```erlang
-spec get_log(sched_state()) -> choice_log().

Log = wf_sched:get_log(State).
%% Returns: [{StepSeq, EnabledSet, Chosen}, ...]
```

### `from_log/1`
Create replay scheduler from recorded choice log.

```erlang
-spec from_log(choice_log()) -> sched_state().

State = wf_sched:from_log(ChoiceLog).
```

## Types

```erlang
-type enabled_action() ::
    {token, token_id()} |
    {xor_branch, pos_integer()}.

-type sched_policy() ::
    deterministic |
    nondeterministic |
    {replay, choice_log()}.

-type choice_log() :: [choice_entry()].

-type choice_entry() :: {
    StepSeq :: non_neg_integer(),
    EnabledSet :: [enabled_action()],
    Chosen :: enabled_action()
}.
```

## Migration from Legacy API

The old `select_action/2` API is deprecated but maintained for backward compatibility.

**Old (deprecated):**
```erlang
Decision = wf_sched:select_action(ExecState, deterministic).
```

**New:**
```erlang
{ok, SchedState} = wf_sched:new(deterministic, []),
Enabled = get_enabled_actions(ExecState),  %% Extract enabled actions
{Decision, NewSchedState} = wf_sched:choose(Enabled, SchedState).
```

## Testing

Run scheduler tests:

```bash
erl -noshell -pa ebin -eval "eunit:test([wf_sched_tests], [verbose])" -s erlang halt
```

## Implementation Notes

- **Deterministic policy** uses `lists:sort/2` with custom comparison function
- **Nondeterministic policy** uses `rand:uniform_s/2` with explicit state for reproducibility
- **Replay policy** validates enabled sets match exactly (strict divergence detection)
- Behaviour callback: `choose/2 :: (EnabledActions, SchedState) -> {Chosen, NewSchedState}`
