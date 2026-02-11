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
%% Nondeterministic Policy Tests
%%====================================================================

nondeterministic_new_test() ->
    {ok, State} = wf_sched_nondeterministic:new([{seed, {1, 2, 3}}]),
    ?assertEqual(nondeterministic, maps:get(policy, State)),
    ?assertEqual([], maps:get(log, State)),
    ?assertEqual(0, maps:get(step_seq, State)),
    ?assert(maps:is_key(rand_state, State)).

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
%% API Integration Tests - Deterministic
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

%%====================================================================
%% API Integration Tests - Nondeterministic
%%====================================================================

api_new_nondeterministic_test() ->
    {ok, State} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),
    ?assertEqual(nondeterministic, maps:get(policy, State)),
    ?assertEqual([], maps:get(log, State)),
    ?assertEqual(0, maps:get(step_seq, State)),
    ?assert(maps:is_key(rand_state, State)).

api_get_log_test() ->
    {ok, State0} = wf_sched:new(nondeterministic, [{seed, {1, 2, 3}}]),
    Enabled = [{token, ref1}, {token, ref2}],

    {_, State1} = wf_sched:choose(Enabled, State0),
    {_, State2} = wf_sched:choose(Enabled, State1),

    Log = wf_sched:get_log(State2),
    ?assertEqual(2, length(Log)),
    %% Log should be in chronological order (reversed from internal storage)
    [{0, _, _}, {1, _, _}] = Log.

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
    {R3, _ReplayState3} = wf_sched:choose(Enabled, ReplayState2),

    %% Verify replay matches original choices
    ?assertEqual(C1, R1),
    ?assertEqual(C2, R2),
    ?assertEqual(C3, R3).

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
