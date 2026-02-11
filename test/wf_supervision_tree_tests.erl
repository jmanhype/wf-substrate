-module(wf_supervision_tree_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_exec_tests.hrl").

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
    {ok, _Apps} = application:ensure_all_started(wf_substrate),
    ok.

cleanup_app(_ok) ->
    application:stop(wf_substrate).

%% Test that wf_case_sup starts as child of wf_substrate_sup
wf_case_sup_starts_test() ->
    Children = supervisor:which_children(wf_substrate_sup),
    ?assertMatch([{wf_case_sup, _, _, _} | _], Children),
    ok.

%% Test dynamic case start
dynamic_case_start_test() ->
    {ok, _Pid} = wf_substrate:new_case(test_case_1, ?MOCK_BYTECODE_SIMPLE_TASK, #{}),
    timer:sleep(100),
    ok.

%% Test that cases are temporary (not restarted on normal termination)
temporary_restart_test() ->
    %% Create case that completes quickly
    Bytecode = [{done}],
    {ok, Pid} = wf_substrate:new_case(temp_case, Bytecode, #{}),
    monitor(process, Pid),
    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 1000 ->
        error(timeout)
    end,
    %% Case should not be restarted
    timer:sleep(100),
    ?assertEqual(undefined, whereis(wf_case_pid_name(temp_case))),
    ok.

%% Test name conflict handling
name_conflict_test() ->
    {ok, _Pid1} = wf_substrate:new_case(conflict_case, ?MOCK_BYTECODE_SIMPLE_TASK, #{}),
    timer:sleep(50),
    ?assertMatch({error, already_exists},
                 wf_substrate:new_case(conflict_case, ?MOCK_BYTECODE_SIMPLE_TASK, #{})),
    ok.

%% Helper function
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId)).
