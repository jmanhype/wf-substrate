-module(wf_substrate_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_exec_tests.hrl").

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
         {"validate_stub_test", fun validate_stub_test/0},
         {"not_found_errors_test", fun not_found_errors_test/0}
     ]
    }.

setup_app() ->
    {ok, _Apps} = application:ensure_all_started(wf_substrate),
    ok.

cleanup_app(_ok) ->
    application:stop(wf_substrate).

%% Test new_case creates process
new_case_creates_process_test() ->
    CaseId = api_test_1,
    {ok, Pid} = wf_substrate:new_case(CaseId, ?MOCK_BYTECODE_SIMPLE_TASK, #{}),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(wf_case_pid_name(CaseId))),
    timer:sleep(100),
    ok.

%% Test signal delivery
signal_delivers_message_test() ->
    CaseId = api_test_2,
    {ok, _Pid} = wf_substrate:new_case(CaseId, ?MOCK_BYTECODE_SEQ, #{}),
    timer:sleep(50),
    ?assertEqual(ok, wf_substrate:signal(CaseId, test_signal)),
    timer:sleep(50),
    ok.

%% Test cancel
cancel_terminates_case_test() ->
    CaseId = api_test_3,
    Bytecode = [{task_exec, long_task}, {done}],
    {ok, Pid} = wf_substrate:new_case(CaseId, Bytecode, #{}),
    monitor(process, Pid),
    ?assertEqual(ok, wf_substrate:cancel(CaseId)),
    receive
        {'DOWN', _, _, Pid, _} -> ok
    after 1000 ->
        error(timeout)
    end,
    ok.

%% Test await
await_blocks_until_completion_test() ->
    CaseId = api_test_4,
    {ok, _Pid} = wf_substrate:new_case(CaseId, ?MOCK_BYTECODE_SIMPLE_TASK, #{}),
    Result = wf_substrate:await(CaseId, 5000),
    ?assertMatch({ok, _}, Result),
    ok.

%% Test status
status_returns_info_test() ->
    CaseId = api_test_5,
    {ok, _Pid} = wf_substrate:new_case(CaseId, ?MOCK_BYTECODE_SEQ, #{}),
    {ok, StatusInfo} = wf_substrate:status(CaseId),
    ?assert(is_map(StatusInfo)),
    ?assert(maps:is_key(state, StatusInfo)),
    timer:sleep(100),
    ok.

%% Test trace configuration
trace_configures_tracing_test() ->
    CaseId = api_test_6,
    {ok, _Pid} = wf_substrate:new_case(CaseId, ?MOCK_BYTECODE_SEQ, #{}),
    ?assertEqual(ok, wf_substrate:trace(CaseId, min, {ets, wf_trace_events})),
    timer:sleep(50),
    ok.

%% Test validate stub
validate_stub_test() ->
    ?assertEqual(ok, wf_substrate:validate(?MOCK_BYTECODE_SIMPLE_TASK, #{})),
    ok.

%% Test not_found errors for non-existent cases
not_found_errors_test() ->
    CaseId = nonexistent_case,
    ?assertEqual({error, not_found}, wf_substrate:signal(CaseId, test_signal)),
    ?assertEqual({error, not_found}, wf_substrate:cancel(CaseId)),
    ?assertEqual({error, not_found}, wf_substrate:cancel_region(CaseId, scope1)),
    ?assertEqual({error, not_found}, wf_substrate:status(CaseId)),
    ?assertEqual({error, not_found}, wf_substrate:trace(CaseId, min, {ets, test})),
    ok.

%% Helper function
wf_case_pid_name(CaseId) ->
    list_to_atom("wf_case_" ++ atom_to_list(CaseId)).
