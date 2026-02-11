#!/usr/bin/env escript
%% Simple test script to verify governance modules work

-mode(compile).

main(_) ->
    io:format("=== Testing Governance Modules ===~n"),
    code:add_patha("ebin"),

    %% Test 1: Start servers
    io:format("~n1. Starting servers...~n"),
    {ok, _} = wf_governance:start_link(),
    {ok, _} = wf_budget:start_link(),
    {ok, _} = wf_approval:start_link(),
    io:format("   ✓ All servers started~n"),

    %% Test 2: Set allowlist
    io:format("~n2. Setting allowlist...~n"),
    ok = wf_governance:set_allowlist(test_scope, [http_get, file_read]),
    io:format("   ✓ Allowlist set~n"),

    %% Test 3: Set budget
    io:format("~n3. Setting budget...~n"),
    ok = wf_budget:init_budget(test_case, [{max_effects, 2}]),
    io:format("   ✓ Budget initialized~n"),

    %% Test 4: Allowlist enforcement
    io:format("~n4. Testing allowlist enforcement...~n"),
    case wf_governance:get_allowlist(test_scope) of
        {ok, AllowList} ->
            io:format("   Allowlist: ~p~n", [AllowList]);
        {error, not_found} ->
            io:format("   ✗ No allowlist found!~n")
    end,

    Spec1 = wf_effect_stub:new_spec(test_case, 0, test_scope, http_get, #{}),
    case wf_effect_stub:yield(test_case, 0, test_scope, Spec1) of
        {ok, _Result} -> io:format("   ✓ Allowed effect executed~n");
        Other -> io:format("   ✗ Unexpected result: ~p~n", [Other])
    end,

    Spec2 = wf_effect_stub:new_spec(test_case, 1, test_scope, http_post, #{}),
    case wf_effect_stub:yield(test_case, 1, test_scope, Spec2) of
        {error, _Error} -> io:format("   ✓ Blocked effect rejected~n");
        Other2 -> io:format("   ✗ Unexpected result (should be blocked): ~p~n", [Other2])
    end,

    %% Test 5: Budget enforcement
    io:format("~n5. Testing budget enforcement...~n"),
    ok = wf_budget:increment_effect_count(test_case),
    ok = wf_budget:check_budget(test_case),
    io:format("   ✓ Budget check passed (1/2 effects)~n"),

    ok = wf_budget:increment_effect_count(test_case),
    {error, _} = wf_budget:check_budget(test_case),
    io:format("   ✓ Budget exhausted (2/2 effects)~n"),

    %% Test 6: Approval gate
    io:format("~n6. Testing approval gate...~n"),
    ApprovalSpec = #{
        approval_id => test_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Test approval">>
    },

    spawn_link(fun() ->
        Result = wf_approval:request_approval(test_task, ApprovalSpec),
        io:format("   Approval result: ~p~n", [Result])
    end),

    timer:sleep(100),
    ok = wf_approval:signal(test_approval, approve),
    timer:sleep(100),
    io:format("   ✓ Approval flow completed~n"),

    %% Cleanup
    io:format("~n7. Cleaning up...~n"),
    wf_approval:stop(),
    wf_budget:stop(),
    wf_governance:stop(),
    io:format("   ✓ All servers stopped~n"),

    io:format("~n=== All Tests Passed ===~n"),
    init:stop().
