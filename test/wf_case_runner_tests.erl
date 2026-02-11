-module(wf_case_runner_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_exec_tests.hrl").

%%====================================================================
%% Setup
%%====================================================================

setup() ->
    {ok, Pid} = wf_case_runner:start_link(
        test_case_runner,
        ?MOCK_BYTECODE_SIMPLE_TASK,
        #{step_quanta => 10, timeout => 5000}
    ),
    Pid.

cleanup(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> gen_statem:stop(Pid);
        false -> ok
    end;
cleanup(_) ->
    ok.

%%====================================================================
%% State Transition Tests
%%====================================================================

wf_case_runner_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
             {"running_to_done_completion_test", ?_test(running_to_done_completion_test(Pid))},
             {"signal_delivery_test", ?_test(signal_delivery_test(Pid))},
             {"cancel_test", ?_test(cancel_test(Pid))},
             {"status_query_test", ?_test(status_query_test(Pid))},
             {"trace_configuration_test", ?_test(trace_configuration_test(Pid))}
         ]
     end
    }.

%% Test normal completion flow from running to done
running_to_done_completion_test(Pid) ->
    %% Wait for case to complete
    timer:sleep(100),
    case wf_case_runner:status(Pid) of
        {ok, done, StatusInfo} ->
            ?assertEqual(done, maps:get(state, StatusInfo));
        {error, _} ->
            %% Process may have terminated
            ok
    end,
    ok.

%% Test signal delivery
signal_delivery_test(Pid) ->
    %% Send signal to running case
    ok = wf_case_runner:signal(Pid, test_signal),
    timer:sleep(50),
    ok.

%% Test case cancellation
cancel_test(Pid) ->
    %% Cancel the case
    ok = wf_case_runner:cancel(Pid),
    timer:sleep(50),
    case erlang:is_process_alive(Pid) of
        true ->
            case wf_case_runner:status(Pid) of
                {ok, cancelled, _} -> ok;
                {error, _} -> ok
            end;
        false ->
            ok
    end,
    ok.

%% Test status query
status_query_test(Pid) ->
    {ok, State, StatusInfo} = wf_case_runner:status(Pid),
    ?assert(is_map(StatusInfo)),
    ?assert(is_atom(State)),
    ?assert(maps:is_key(ip, StatusInfo)),
    ?assert(maps:is_key(step_count, StatusInfo)),
    ok.

%% Test trace configuration
trace_configuration_test(Pid) ->
    %% Set trace level
    ok = wf_case_runner:set_trace(Pid, {min, {ets, wf_trace_events}}),
    timer:sleep(50),
    ok.
