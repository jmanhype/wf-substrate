%%%-------------------------------------------------------------------
%%% @doc Unit tests for wf_validate
%%%-------------------------------------------------------------------
-module(wf_validate_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([nowarn_unused_function]).

%% Include record definitions
-include("wf_exec.hrl").
-include("wf_validate.hrl").

%%====================================================================
%% Test Data
%%====================================================================

%% Simple valid workflow: TASK_EXEC -> DONE
mock_bytecode_simple() ->
    [
        {'TASK_EXEC', task},
        {'DONE'}
    ].

%% Workflow with deadlock: PAR_FORK without JOIN_WAIT
mock_bytecode_deadlock() ->
    [
        {'PAR_FORK', [2, 4]},
        {'TASK_EXEC', a},
        {'DONE'},
        {'TASK_EXEC', b},
        {'DONE'}
    ].

%% Workflow with unreachable code
mock_bytecode_unreachable() ->
    [
        {'TASK_EXEC', task1},
        {'DONE'},
        {'TASK_EXEC', unreachable_task},  %% After DONE, unreachable
        {'DONE'}
    ].

%% Sequential workflow
mock_bytecode_seq() ->
    [
        {'TASK_EXEC', task1},
        {'SEQ_NEXT', 3},
        {'TASK_EXEC', task2},
        {'DONE'}
    ].

%%====================================================================
%% Tests: Phase 1 - Core Data Structures
%%====================================================================

new_creates_initial_state_test() ->
    Bytecode = mock_bytecode_simple(),
    State = wf_validate:new(Bytecode),

    ?assertEqual(Bytecode, State#validation_state.bytecode),
    ?assertEqual(1, map_size(State#validation_state.tokens)),
    ?assertEqual(0, State#validation_state.step_count),
    ?assertEqual([root], State#validation_state.scope_stack).

default_options_returns_defaults_test() ->
    Options = wf_validate:default_options(),

    ?assertEqual(100, maps:get(depth, Options)),
    ?assertEqual(10, maps:get(token_bound, Options)),
    ?assertEqual(bfs, maps:get(search_strategy, Options)),
    ?assertEqual(none, maps:get(trace_level, Options)).

to_petri_net_returns_state_and_metadata_test() ->
    Bytecode = mock_bytecode_simple(),
    {State, Metadata} = wf_validate:to_petri_net(Bytecode),

    ?assert(is_record(State, validation_state)),
    ?assertEqual(2, maps:get(bytecode_length, Metadata)),
    ?assertMatch(#{'TASK_EXEC' := 1, 'DONE' := 1}, maps:get(transition_counts, Metadata)).

%%====================================================================
%% Tests: Phase 2 - Exploration Engine
%%====================================================================

enabled_transitions_simple_test() ->
    Bytecode = mock_bytecode_simple(),
    State = wf_validate:new(Bytecode),
    Enabled = wf_validate:enabled_transitions(State),

    ?assertEqual(1, length(Enabled)),
    {token, _TokenId} = hd(Enabled).

enabled_transitions_no_active_tokens_test() ->
    %% Create state with completed tokens
    Bytecode = mock_bytecode_simple(),
    State0 = wf_validate:new(Bytecode),
    TokenId = hd(maps:keys(State0#validation_state.tokens)),
    CompleteToken = (maps:get(TokenId, State0#validation_state.tokens))#token{status = complete},
    State1 = State0#validation_state{tokens = #{TokenId => CompleteToken}},

    Enabled = wf_validate:enabled_transitions(State1),
    ?assertEqual(0, length(Enabled)).

fire_transition_task_exec_test() ->
    Bytecode = mock_bytecode_simple(),
    State0 = wf_validate:new(Bytecode),
    TokenId = hd(maps:keys(State0#validation_state.tokens)),

    State1 = wf_validate:fire_transition(State0, {token, TokenId}),

    %% Token should have advanced IP
    UpdatedToken = maps:get(TokenId, State1#validation_state.tokens),
    ?assertEqual(1, UpdatedToken#token.ip),
    ?assertEqual(1, State1#validation_state.step_count).

fire_transition_done_test() ->
    Bytecode = [{'DONE'}],
    State0 = wf_validate:new(Bytecode),
    TokenId = hd(maps:keys(State0#validation_state.tokens)),

    State1 = wf_validate:fire_transition(State0, {token, TokenId}),

    %% Token should be complete
    UpdatedToken = maps:get(TokenId, State1#validation_state.tokens),
    ?assertEqual(complete, UpdatedToken#token.status).

state_hash_consistent_test() ->
    Bytecode = mock_bytecode_simple(),
    State = wf_validate:new(Bytecode),
    Hash1 = wf_validate:state_hash(State),
    Hash2 = wf_validate:state_hash(State),

    ?assertEqual(Hash1, Hash2).

explore_simple_workflow_test() ->
    Bytecode = mock_bytecode_simple(),
    Options = wf_validate:default_options(),
    Result = wf_validate:explore(Bytecode, Options),

    ?assertMatch({ok, _Report}, Result),
    {ok, Report} = Result,
    ?assert(Report#report.explored_state_count > 0),
    ?assert(Report#report.unique_state_count > 0).

%%====================================================================
%% Tests: Phase 3 - Correctness Checks
%%====================================================================

check_dead_transitions_none_test_() ->
    Bytecode = mock_bytecode_simple(),
    States = collect_states_simple(Bytecode, 10),
    [?_assertEqual([], wf_validate:check_dead_transitions(States, Bytecode))].

check_dead_transitions_unreachable_test_() ->
    Bytecode = mock_bytecode_unreachable(),
    States = collect_states_simple(Bytecode, 10),
    Issues = wf_validate:check_dead_transitions(States, Bytecode),
    [?_assert(length(Issues) > 0)].

check_proper_completion_valid_test_() ->
    Bytecode = mock_bytecode_simple(),
    States = collect_states_simple(Bytecode, 10),
    Issues = wf_validate:check_proper_completion(States),
    [?_assertEqual([], Issues)].

check_deadlock_par_fork_test_() ->
    Bytecode = mock_bytecode_deadlock(),
    States = collect_states_simple(Bytecode, 10),
    Issues = wf_validate:check_proper_completion(States),
    [?_assert(length(Issues) > 0)].

%%====================================================================
%% Tests: Phase 4 - Public API
%%====================================================================

validate_simple_workflow_test_() ->
    Bytecode = mock_bytecode_simple(),
    [?_assertMatch({ok, _}, wf_validate:validate(Bytecode))].

validate_deadlock_workflow_test_() ->
    Bytecode = mock_bytecode_deadlock(),
    [?_assertMatch({error, _}, wf_validate:validate(Bytecode))].

validate_with_custom_options_test_() ->
    Bytecode = mock_bytecode_simple(),
    Options = #{depth => 5, token_bound => 2},
    [?_assertMatch({ok, _}, wf_validate:validate(Bytecode, Options))].

%%====================================================================
%% Helper Functions
%%====================================================================

%% Simple state collector for testing
collect_states_simple(Bytecode, MaxSteps) ->
    InitialState = wf_validate:new(Bytecode),
    collect_states_simple([InitialState], MaxSteps, [], sets:new([{version, 2}])).

collect_states_simple([], _MaxSteps, Acc, _Visited) ->
    lists:usort(Acc);
collect_states_simple([State | Rest], MaxSteps, Acc, Visited) ->
    Hash = wf_validate:state_hash(State),
    case sets:is_element(Hash, Visited) of
        true -> collect_states_simple(Rest, MaxSteps, Acc, Visited);
        false ->
            NewVisited = sets:add_element(Hash, Visited),
            if State#validation_state.step_count >= MaxSteps ->
                collect_states_simple(Rest, MaxSteps, [State | Acc], NewVisited);
            true ->
                Enabled = wf_validate:enabled_transitions(State),
                Successors = [wf_validate:fire_transition(State, Action) || Action <- Enabled],
                collect_states_simple(Rest ++ Successors, MaxSteps, [State | Acc], NewVisited)
            end
    end.
