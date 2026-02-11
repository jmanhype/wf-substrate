-module(wf_exec_effect_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf_exec.hrl").
-include("../src/wf_effect.hrl").
-include("../src/wf_receipt.hrl").

%%====================================================================
%% Integration Tests: Effect Yielding and Blocking
%%====================================================================

%% Test that effect system components work together
effect_yield_resume_cycle_test_() ->
    {setup,
     fun() ->
         %% Start gen_servers
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         %% Test complete effect lifecycle
         CaseId = make_ref(),
         StepSeq = 0,
         ScopeId = root,

         %% Create effect spec
         EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, mock_effect, #{}),

         %% Yield effect
         {ok, Effect} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpec),

         [
             ?_assert(is_record(Effect, effect)),
             ?_assertEqual(pending, Effect#effect.status),
             ?_assertEqual({CaseId, StepSeq, ScopeId}, Effect#effect.effect_id)
         ]
     end}.

%% Test effect completion and result retrieval
effect_completion_test_() ->
    {setup,
     fun() ->
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),

         %% Yield effect
         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         %% Wait for completion
         timer:sleep(50),

         %% Check result
         Result = wf_effect:get_result(Effect#effect.effect_id),

         %% Verify receipt created
         {ok, Receipt} = wf_receipt:lookup(CaseId, Effect#effect.effect_id),

         [
             ?_assertEqual({ok, mock_result}, Result),
             ?_assert(is_record(Receipt, receipt)),
             ?_assertEqual({ok, mock_result}, Receipt#receipt.result)
         ]
     end}.

%% Test idempotency with cached results
idempotency_caching_test_() ->
    {setup,
     fun() ->
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         CaseId = make_ref(),
         Key = make_ref(),

         %% First effect
         EffectSpec1 = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),
         EffectSpec1WithKey = wf_effect:set_idempotency_key(EffectSpec1, Key),
         {ok, Effect1} = wf_effect:yield(CaseId, 0, root, EffectSpec1WithKey),

         %% Wait for completion
         timer:sleep(50),

         %% Second effect with same key
         EffectSpec2 = wf_effect:new_spec(CaseId, 1, root, mock_effect, #{}),
         EffectSpec2WithKey = wf_effect:set_idempotency_key(EffectSpec2, Key),
         {ok, ReceiptOrEffect} = wf_effect:yield(CaseId, 1, root, EffectSpec2WithKey),

         [
             ?_assert(is_record(ReceiptOrEffect, receipt)),
             ?_assertEqual({ok, mock_result}, ReceiptOrEffect#receipt.result),
             ?_assertEqual(Key, ReceiptOrEffect#receipt.idempotency_key)
         ]
     end}.

%% Test effect cancellation
effect_cancellation_test_() ->
    {setup,
     fun() ->
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         CaseId = make_ref(),
         EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_delay, #{}),

         %% Yield effect
         {ok, Effect} = wf_effect:yield(CaseId, 0, root, EffectSpec),

         %% Cancel effect
         ok = wf_effect:cancel_effect(Effect#effect.effect_id, test_cancel),

         %% Wait for cancellation to process
         timer:sleep(150),

         %% Check cancellation status
         IsCancelled = wf_effect:is_cancelled(Effect#effect.effect_id),
         Result = wf_effect:get_result(Effect#effect.effect_id),

         [
             ?_assert(IsCancelled),
             ?_assertMatch({cancelled, test_cancel}, Result)
         ]
     end}.

%% Test receipt verification
receipt_verification_test_() ->
    {setup,
     fun() ->
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         EffectId = {make_ref(), 0, root},
         EffectSpec = #effect_spec{
             effect_id = EffectId,
             effect_type = mock_effect,
             payload = #{data => test},
             idempotency_key = undefined,
             timeout = undefined
         },

         %% Create receipt
         Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),
         wf_receipt:store(Receipt, EffectSpec),

         %% Lookup receipt
         {ok, Lookup} = wf_receipt:lookup(element(1, EffectId), EffectId),

         %% Verify receipt
         IsValid = wf_receipt:verify(Lookup, EffectSpec),

         [
             ?_assert(IsValid),
             ?_assertEqual({ok, test_result}, Lookup#receipt.result)
         ]
     end}.

%% Test multiple receipts for same case
all_receipts_test_() ->
    {setup,
     fun() ->
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         CaseId = make_ref(),

         %% Create multiple effects
         EffectSpec1 = #effect_spec{
             effect_id = {CaseId, 0, root},
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },
         EffectSpec2 = #effect_spec{
             effect_id = {CaseId, 1, root},
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },

         Receipt1 = wf_receipt:new({CaseId, 0, root}, {ok, result1}, 1000),
         wf_receipt:store(Receipt1, EffectSpec1),

         Receipt2 = wf_receipt:new({CaseId, 1, root}, {ok, result2}, 1000),
         wf_receipt:store(Receipt2, EffectSpec2),

         %% Get all receipts
         AllReceipts = wf_receipt:all(CaseId),

         [
             ?_assertEqual(2, length(AllReceipts))
         ]
     end}.
