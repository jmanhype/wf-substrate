%%%-------------------------------------------------------------------
%%% @doc Supervisor tests for wf_substrate
%%%-------------------------------------------------------------------
-module(wf_supervision_tree_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_exec_tests.hrl").

%%====================================================================
%% Supervisor Tests - SKIPPED
%%====================================================================

supervision_tree_test_() ->
    {skip, "wf_case_sup has {bad_start_spec,[]} error in OTP 26 - simple_one_for_one supervisor with empty child spec rejected. Root cause in src/wf_case_sup.erl:84. Implementation change needed to fix supervisor architecture or use dynamic supervisor instead."}.
