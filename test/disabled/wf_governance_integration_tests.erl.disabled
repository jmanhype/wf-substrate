-module(wf_governance_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_governance.hrl").

%%%====================================================================
%%% Test fixtures
%%%====================================================================

setup() ->
    {ok, GovPid} = wf_governance:start_link(),
    {ok, BudgetPid} = wf_budget:start_link(),
    {ok, ApprovalPid} = wf_approval:start_link(),
    {GovPid, BudgetPid, ApprovalPid}.

cleanup({GovPid, BudgetPid, ApprovalPid}) ->
    wf_approval:stop(),
    wf_budget:stop(),
    wf_governance:stop().

%%%====================================================================
%%% Integration tests
%%%====================================================================

allowlist_budget_combined_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun allowlist_and_budget_enforcement/0
        ]
    }.

allowlist_and_budget_enforcement() ->
    %% Setup: Scope with allowlist, Case with budget
    ScopeId = integration_scope,
    CaseId = integration_case,

    %% Set allowlist
    ok = wf_governance:set_allowlist(ScopeId, [http_get, file_read]),

    %% Set budget
    ok = wf_budget:init_budget(CaseId, [{max_effects, 2}]),

    %% Execute allowed effect (should succeed)
    Spec1 = wf_effect_stub:new_spec(CaseId, 0, ScopeId, http_get, #{}),
    {ok, _} = wf_effect_stub:yield(CaseId, 0, ScopeId, Spec1),
    wf_budget:increment_effect_count(CaseId),

    %% Execute another allowed effect (should succeed)
    Spec2 = wf_effect_stub:new_spec(CaseId, 1, ScopeId, file_read, #{}),
    {ok, _} = wf_effect_stub:yield(CaseId, 1, ScopeId, Spec2),
    wf_budget:increment_effect_count(CaseId),

    %% Try disallowed effect (should be blocked by allowlist)
    Spec3 = wf_effect_stub:new_spec(CaseId, 2, ScopeId, http_post, #{}),
    {error, #governance_error{error_type = allowlist_violation}} =
        wf_effect_stub:yield(CaseId, 2, ScopeId, Spec3),

    %% Even if allowed, budget should be exhausted
    ?assertMatch({error, #governance_error{error_type = budget_exceeded}},
                wf_budget:check_budget(CaseId)).

timeout_policy_enforcement_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun timeout_policy_storage/0
        ]
    }.

timeout_policy_storage() ->
    %% Test that timeout policies can be set and retrieved
    CaseId = timeout_case,
    TaskId = timeout_task,
    ScopeId = timeout_scope,

    %% Set different timeout policies
    ok = wf_governance:set_timeout_policy(CaseId, {case_timeout, 300000}),
    ok = wf_governance:set_timeout_policy(TaskId, {task_timeout, 30000}),
    ok = wf_governance:set_timeout_policy(ScopeId, {effect_timeout, 5000}),

    %% Verify retrieval
    {ok, {case_timeout, 300000}} = wf_governance:get_timeout_policy(CaseId),
    {ok, {task_timeout, 30000}} = wf_governance:get_timeout_policy(TaskId),
    {ok, {effect_timeout, 5000}} = wf_governance:get_timeout_policy(ScopeId).

approval_gate_flow_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun approval_full_flow/0
        ]
    }.

approval_full_flow() ->
    %% Setup approval configuration
    ScopeId = approval_scope,
    TaskId = approval_task,

    ApprovalSpec = #{
        approval_id => flow_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Integration test approval">>
    },

    ok = wf_governance:require_approval(ScopeId, TaskId, ApprovalSpec),

    %% Request approval in separate process
    Parent = self(),
    Pid = spawn_link(fun() ->
        Result = wf_approval:request_approval(TaskId, ApprovalSpec),
        Parent ! {approval_result, Result}
    end),

    %% Wait for request to register
    timer:sleep(100),

    %% Check that approval is pending
    {ok, pending} = wf_approval:check_approval(flow_approval),

    %% Signal approval
    ok = wf_approval:signal(flow_approval, approve),

    %% Wait for result
    receive
        {approval_result, {ok, approved}} ->
            ?assert(true)
    after 2000 ->
        ?assert(false, approval_timeout)
    end,

    %% Verify approval status
    {ok, approved} = wf_approval:check_approval(flow_approval).

governance_error_structure_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun error_record_fields/0,
            fun error_types_enforced/0
        ]
    }.

error_record_fields() ->
    %% Verify that governance errors have all required fields
    ScopeId = error_scope,
    CaseId = error_case,

    %% Set allowlist
    ok = wf_governance:set_allowlist(ScopeId, [http_get]),

    %% Trigger allowlist violation
    Spec = wf_effect_stub:new_spec(CaseId, 0, ScopeId, http_post, #{}),
    {error, Error} = wf_effect_stub:yield(CaseId, 0, ScopeId, Spec),

    %% Verify error structure
    ?assertMatch(#governance_error{
        error_type := allowlist_violation,
        scope := ScopeId,
        task_id := undefined,
        detail := <<>>,
        timestamp := {_MegaSecs, _Secs, _MicroSecs}
    }, Error).

error_types_enforced() ->
    %% Test that all error types are produced correctly

    %% 1. Allowlist violation
    Scope1 = error_type1,
    ok = wf_governance:set_allowlist(Scope1, [http_get]),
    Spec1 = wf_effect_stub:new_spec(case1, 0, Scope1, http_post, #{}),
    {error, #governance_error{error_type = allowlist_violation}} =
        wf_effect_stub:yield(case1, 0, Scope1, Spec1),

    %% 2. Budget exceeded
    Case2 = error_type2,
    ok = wf_budget:init_budget(Case2, [{max_effects, 1}]),
    ok = wf_budget:increment_effect_count(Case2),
    {error, #governance_error{error_type = budget_exceeded}} =
        wf_budget:check_budget(Case2),

    %% 3. Timeout (via approval timeout)
    ApprovalSpec = #{
        approval_id => timeout_error,
        timeout => 100,
        timeout_action => cancel,
        detail => <<"Test timeout">>
    },
    {error, #governance_error{error_type = timeout}} =
        wf_approval:request_approval(task1, ApprovalSpec).

multiple_independent_cases_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
        fun independent_case_policies/0
    ]
    }.

independent_case_policies() ->
    %% Verify that different cases have independent policies

    %% Setup two cases
    Case1 = independent_case1,
    Case2 = independent_case2,
    Scope = shared_scope,

    %% Set different budgets
    ok = wf_budget:init_budget(Case1, [{max_effects, 1}]),
    ok = wf_budget:init_budget(Case2, [{max_effects, 5}]),

    %% Set allowlist (shared)
    ok = wf_governance:set_allowlist(Scope, [http_get]),

    %% Exhaust Case1 budget
    Spec = wf_effect_stub:new_spec(Case1, 0, Scope, http_get, #{}),
    {ok, _} = wf_effect_stub:yield(Case1, 0, Scope, Spec),
    wf_budget:increment_effect_count(Case1),

    %% Case1 should be exhausted
    {error, #governance_error{error_type = budget_exceeded}} =
        wf_budget:check_budget(Case1),

    %% Case2 should still have budget available
    ok = wf_budget:check_budget(Case2).

governance_policy_update_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
        fun policy_update_changes_enforcement/0
    ]
    }.

policy_update_changes_enforcement() ->
    %% Verify that policy updates change enforcement behavior
    Scope = update_scope,
    Case = update_case,

    %% Initial policy: only http_get allowed
    ok = wf_governance:set_allowlist(Scope, [http_get]),

    %% http_post should be blocked
    Spec1 = wf_effect_stub:new_spec(Case, 0, Scope, http_post, #{}),
    {error, #governance_error{error_type = allowlist_violation}} =
        wf_effect_stub:yield(Case, 0, Scope, Spec1),

    %% Update policy to allow http_post
    ok = wf_governance:set_allowlist(Scope, [http_get, http_post]),

    %% http_post should now be allowed
    Spec2 = wf_effect_stub:new_spec(Case, 1, Scope, http_post, #{}),
    {ok, _} = wf_effect_stub:yield(Case, 1, Scope, Spec2).
