-module(wf_governance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_governance.hrl").

%%%====================================================================
%%% Test fixtures
%%%====================================================================

governance_setup() ->
    {ok, Pid} = wf_governance:start_link(),
    Pid.

governance_cleanup(_Pid) ->
    wf_governance:stop().

budget_setup() ->
    {ok, Pid} = wf_budget:start_link(),
    Pid.

budget_cleanup(_Pid) ->
    wf_budget:stop().

approval_setup() ->
    {ok, Pid} = wf_approval:start_link(),
    Pid.

approval_cleanup(_Pid) ->
    wf_approval:stop().

%%%====================================================================
%%% wf_governance tests
%%%====================================================================

allowlist_test_() ->
    {setup,
        fun governance_setup/0,
        fun governance_cleanup/1,
        [
            fun set_get_allowlist/0,
            fun allowlist_enforcement/0
        ]
    }.

set_get_allowlist() ->
    ScopeId = test_scope,
    ?assertEqual(ok, wf_governance:set_allowlist(ScopeId, [http_get, http_post])),
    {ok, AllowList} = wf_governance:get_allowlist(ScopeId),
    ?assertEqual([http_get, http_post], AllowList).

allowlist_enforcement() ->
    ScopeId = test_scope2,
    ?assertEqual(ok, wf_governance:set_allowlist(ScopeId, [http_get])),

    %% Allowed effect
    AllowedSpec = wf_effect_stub:new_spec(case1, 0, ScopeId, http_get, #{}),
    Result = wf_effect_stub:yield(case1, 0, ScopeId, AllowedSpec),
    ?assertMatch({ok, _}, Result),

    %% Blocked effect
    BlockedSpec = wf_effect_stub:new_spec(case1, 1, ScopeId, http_post, #{}),
    ErrorResult = wf_effect_stub:yield(case1, 1, ScopeId, BlockedSpec),
    ?assertMatch({error, #governance_error{error_type = allowlist_violation}}, ErrorResult).

budget_policy_test_() ->
    {setup,
        fun governance_setup/0,
        fun governance_cleanup/1,
        [
            fun set_get_budget/0
        ]
    }.

set_get_budget() ->
    CaseId = case1,
    Limits = [{max_effects, 100}, {max_time_us, 60000000}, {max_cost, 1000}],
    ?assertEqual(ok, wf_governance:set_budget(CaseId, Limits)),
    {ok, RetrievedLimits} = wf_governance:get_budget(CaseId),
    ?assertEqual(Limits, RetrievedLimits).

timeout_policy_test_() ->
    {setup,
        fun governance_setup/0,
        fun governance_cleanup/1,
        [
            fun set_get_timeout/0
        ]
    }.

set_get_timeout() ->
    CaseId = case1,
    ?assertEqual(ok, wf_governance:set_timeout_policy(CaseId, {case_timeout, 5000})),
    {ok, {case_timeout, 5000}} = wf_governance:get_timeout_policy(CaseId).

approval_config_test_() ->
    {setup,
        fun governance_setup/0,
        fun governance_cleanup/1,
        [
            fun set_get_approval_config/0
        ]
    }.

set_get_approval_config() ->
    ScopeId = test_scope,
    TaskId = task1,
    ApprovalSpec = #{
        approval_id => test_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Test approval">>
    },
    ?assertEqual(ok, wf_governance:require_approval(ScopeId, TaskId, ApprovalSpec)),
    {ok, RetrievedSpec} = wf_governance:get_approval_config(TaskId),
    ?assertEqual(ApprovalSpec, RetrievedSpec).

%%%====================================================================
%%% wf_budget tests
%%%====================================================================

budget_test_() ->
    {setup,
        fun() ->
            GovPid = governance_setup(),
            BudgetPid = budget_setup(),
            {GovPid, BudgetPid}
        end,
        fun({GovPid, BudgetPid}) ->
            budget_cleanup(BudgetPid),
            governance_cleanup(GovPid)
        end,
        [
            fun budget_enforcement/0,
            fun budget_check_limits/0
        ]
    }.

budget_enforcement() ->
    CaseId = case1,
    ?assertEqual(ok, wf_budget:init_budget(CaseId, [{max_effects, 2}])),
    ?assertEqual(ok, wf_governance:set_budget(CaseId, [{max_effects, 2}])),

    %% First effect allowed
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:increment_effect_count(CaseId),

    %% Second effect allowed
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:increment_effect_count(CaseId),

    %% Third effect blocked
    ?assertMatch({error, #governance_error{error_type = budget_exceeded}},
                wf_budget:check_budget(CaseId)).

budget_check_limits() ->
    CaseId = case2,
    Limits = [
        {max_effects, 10},
        {max_time_us, 1000000},
        {max_cost, 100}
    ],
    ?assertEqual(ok, wf_budget:init_budget(CaseId, Limits)),
    ?assertEqual(ok, wf_budget:check_budget(CaseId)).

budget_time_limit_test_() ->
    {setup,
        fun() ->
            GovPid = governance_setup(),
            BudgetPid = budget_setup(),
            {GovPid, BudgetPid}
        end,
        fun({GovPid, BudgetPid}) ->
            budget_cleanup(BudgetPid),
            governance_cleanup(GovPid)
        end,
        [
            fun budget_time_exceeded/0
        ]
    }.

budget_time_exceeded() ->
    CaseId = case3,
    ?assertEqual(ok, wf_budget:init_budget(CaseId, [{max_time_us, 1000}])),
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:add_effect_time(CaseId, 500),
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:add_effect_time(CaseId, 600),
    ?assertMatch({error, #governance_error{error_type = budget_exceeded}},
                wf_budget:check_budget(CaseId)).

budget_cost_limit_test_() ->
    {setup,
        fun() ->
            GovPid = governance_setup(),
            BudgetPid = budget_setup(),
            {GovPid, BudgetPid}
        end,
        fun({GovPid, BudgetPid}) ->
            budget_cleanup(BudgetPid),
            governance_cleanup(GovPid)
        end,
        [
            fun budget_cost_exceeded/0
        ]
    }.

budget_cost_exceeded() ->
    CaseId = case4,
    ?assertEqual(ok, wf_budget:init_budget(CaseId, [{max_cost, 10}])),
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:add_effect_cost(CaseId, 5),
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:add_effect_cost(CaseId, 6),
    ?assertMatch({error, #governance_error{error_type = budget_exceeded}},
                wf_budget:check_budget(CaseId)).

%%%====================================================================
%%% wf_approval tests
%%%====================================================================

approval_test_() ->
    {setup,
        fun approval_setup/0,
        fun approval_cleanup/1,
        [
            fun approval_request_and_signal/0
        ]
    }.

approval_request_and_signal() ->
    ApprovalSpec = #{
        approval_id => test_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Test approval">>
    },

    %% Request approval in separate process
    spawn_link(fun() ->
        Result = wf_approval:request_approval(task1, ApprovalSpec),
        ?assertEqual({ok, approved}, Result)
    end),

    %% Wait for request to register
    timer:sleep(100),

    %% Signal approval
    ?assertEqual(ok, wf_approval:signal(test_approval, approve)).

approval_reject_test_() ->
    {setup,
        fun approval_setup/0,
        fun approval_cleanup/1,
        [
            fun approval_reject_flow/0
        ]
    }.

approval_reject_flow() ->
    ApprovalSpec = #{
        approval_id => reject_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Test reject">>
    },

    %% Request approval in separate process
    Pid = spawn_link(fun() ->
        Result = wf_approval:request_approval(task2, ApprovalSpec),
        ?assertMatch({error, #governance_error{error_type = approval_required}}, Result)
    end),

    %% Wait for request to register
    timer:sleep(100),

    %% Signal rejection
    ?assertEqual(ok, wf_approval:signal(reject_approval, reject)),

    %% Wait for process to receive rejection
    timer:sleep(100),
    ?assertEqual(false, erlang:is_process_alive(Pid)).

approval_timeout_test_() ->
    {setup,
        fun approval_setup/0,
        fun approval_cleanup/1,
        [
            fun approval_timeout_action/0
        ]
    }.

approval_timeout_action() ->
    ApprovalSpec = #{
        approval_id => timeout_approval,
        timeout => 100,  %% 100ms
        timeout_action => cancel,
        detail => <<"Timeout test">>
    },

    %% Request approval (should timeout quickly)
    Result = wf_approval:request_approval(task3, ApprovalSpec),

    %% Should timeout
    ?assertMatch({error, #governance_error{error_type = timeout}}, Result).

approval_check_status_test_() ->
    {setup,
        fun approval_setup/0,
        fun approval_cleanup/1,
        [
            fun approval_status_check/0
        ]
    }.

approval_status_check() ->
    ApprovalSpec = #{
        approval_id => status_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Status check">>
    },

    %% Request approval in separate process
    spawn_link(fun() ->
        wf_approval:request_approval(task4, ApprovalSpec)
    end),

    %% Wait for request to register
    timer:sleep(100),

    %% Check status
    {ok, pending} = wf_approval:check_approval(status_approval),

    %% Signal approval
    ?assertEqual(ok, wf_approval:signal(status_approval, approve)),

    %% Wait for processing
    timer:sleep(100),

    %% Check status again
    {ok, approved} = wf_approval:check_approval(status_approval).
