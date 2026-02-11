-module(wf_effect_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_effect.hrl").
-include("../src/wf_receipt.hrl").

%%====================================================================
%% Effect Spec Creation Tests
%%====================================================================

new_spec_test_() ->
    CaseId = make_ref(),
    StepSeq = 0,
    ScopeId = root,
    EffectType = mock_effect,
    Payload = #{data => test},

    EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, EffectType, Payload),

    [
        ?_assertEqual({CaseId, StepSeq, ScopeId}, EffectSpec#effect_spec.effect_id),
        ?_assertEqual(EffectType, EffectSpec#effect_spec.effect_type),
        ?_assertEqual(Payload, EffectSpec#effect_spec.payload),
        ?_assertEqual(undefined, EffectSpec#effect_spec.idempotency_key),
        ?_assertEqual(undefined, EffectSpec#effect_spec.timeout)
    ].

set_idempotency_key_test_() ->
    EffectSpec = wf_effect:new_spec(make_ref(), 0, root, mock_effect, #{}),
    Key = make_ref(),
    UpdatedSpec = wf_effect:set_idempotency_key(EffectSpec, Key),

    ?_assertEqual(Key, UpdatedSpec#effect_spec.idempotency_key).

set_timeout_test_() ->
    EffectSpec = wf_effect:new_spec(make_ref(), 0, root, mock_effect, #{}),
    Timeout = 5000,
    UpdatedSpec = wf_effect:set_timeout(EffectSpec, Timeout),

    ?_assertEqual(Timeout, UpdatedSpec#effect_spec.timeout).

%%====================================================================
%% Effect Yielding Tests
%%====================================================================

yield_new_effect_test_() ->
    {setup,
     fun() ->
         %% Start gen_servers
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         %% Stop gen_servers
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),

         {ok, EffectOrReceipt} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         [
             ?_assert(is_record(EffectOrReceipt, effect)),
             ?_assertEqual(pending, EffectOrReceipt#effect.status)
         ]
     end}.

yield_idempotent_effect_cached_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         Key = make_ref(),
         EffectSpec1 = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),
         EffectSpec1WithKey = wf_effect:set_idempotency_key(EffectSpec1, Key),

         %% Submit first effect
         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec1WithKey),

         %% Wait for completion
         timer:sleep(50),

         %% Submit second effect with same key
         EffectSpec2 = wf_effect:new_spec(CaseId, 1, root, mock_effect, #{}),
         EffectSpec2WithKey = wf_effect:set_idempotency_key(EffectSpec2, Key),
         {ok, ReceiptOrEffect} = wf_effect:yield(CaseId, 1, root, EffectSpec2WithKey),

         [
             ?_assert(is_record(ReceiptOrEffect, receipt)),
             ?_assertEqual({ok, mock_result}, ReceiptOrEffect#receipt.result)
         ]
     end}.

%%====================================================================
%% Effect Cancellation Tests
%%====================================================================

cancel_effect_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_delay, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         %% Cancel immediately
         Result = wf_effect:cancel_effect(Effect#effect.effect_id, test_cancel),

         timer:sleep(150),  %% Wait for effect to complete/cancel

         ?_assertEqual(ok, Result)
     end}.

is_cancelled_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_delay, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),
         wf_effect:cancel_effect(Effect#effect.effect_id, test_cancel),

         timer:sleep(150),

         ?_assert(wf_effect:is_cancelled(Effect#effect.effect_id))
     end}.

%%====================================================================
%% Effect Execution Tests
%%====================================================================

get_result_pending_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_delay, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         %% Check immediately (should be pending or complete)
         Result = wf_effect:get_result(Effect#effect.effect_id),

         [
             ?_assert(Result =:= pending orelse element(1, Result) =:= ok)
         ]
     end}.

get_result_complete_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         %% Wait for completion
         timer:sleep(50),

         Result = wf_effect:get_result(Effect#effect.effect_id),

         ?_assertEqual({ok, mock_result}, Result)
     end}.

%%====================================================================
%% Mock Handler Tests
%%====================================================================

mock_effect_handler_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),
         timer:sleep(50),
         Result = wf_effect:get_result(Effect#effect.effect_id),

         ?_assertEqual({ok, mock_result}, Result)
     end}.

mock_error_handler_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_error, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),
         timer:sleep(50),
         Result = wf_effect:get_result(Effect#effect.effect_id),

         ?_assertEqual({error, mock_error}, Result)
     end}.

mock_crash_handler_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_effect:start_link(),
         {ok, Pid2} = wf_receipt:start_link(),
         {Pid, Pid2}
     end,
     fun({Pid, Pid2}) ->
         gen_server:stop(Pid2),
         gen_server:stop(Pid)
     end,
     fun({_Pid, _Pid2}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_crash, #{}),

         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),
         timer:sleep(50),
         Result = wf_effect:get_result(Effect#effect.effect_id),

         ?_assertMatch({error, {handler_crashed, _, _, _}}, Result)
     end}.
