-module(wf_receipt_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_receipt.hrl").
-include("wf_effect.hrl").

%%====================================================================
%% Receipt Creation Tests
%%====================================================================

new_receipt_test_() ->
    EffectId = {make_ref(), 0, root},
    Result = {ok, test_result},
    DurationUs = 1000,

    Receipt = wf_receipt:new(EffectId, Result, DurationUs),

    [
        ?_assert(is_reference(Receipt#receipt.receipt_id)),
        ?_assertEqual(element(1, EffectId), Receipt#receipt.case_id),
        ?_assertEqual(EffectId, Receipt#receipt.effect_id),
        ?_assertEqual(Result, Receipt#receipt.result),
        ?_assertEqual(DurationUs, Receipt#receipt.duration_us)
    ].

%%====================================================================
%% Receipt Storage and Lookup Tests
%%====================================================================

store_and_lookup_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         EffectId = {make_ref(), 0, root},
         EffectSpec = #effect_spec{
             effect_id = EffectId,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },
         Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

         wf_receipt:store(Receipt, EffectSpec),

         {ok, Lookup} = wf_receipt:lookup(element(1, EffectId), EffectId),

         [
             ?_assertEqual(Receipt#receipt.receipt_id, Lookup#receipt.receipt_id),
             ?_assertEqual({ok, test_result}, Lookup#receipt.result)
         ]
     end}.

lookup_by_key_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectId = {CaseId, 0, root},
         Key = make_ref(),
         EffectSpec = #effect_spec{
             effect_id = EffectId,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = Key,
             timeout = undefined
         },
         Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

         wf_receipt:store(Receipt, EffectSpec),

         {ok, Lookup} = wf_receipt:lookup_by_key(CaseId, Key),

         [
             ?_assertEqual({ok, test_result}, Lookup#receipt.result),
             ?_assertEqual(Key, Lookup#receipt.idempotency_key)
         ]
     end}.

%%====================================================================
%% Receipt Verification Tests
%%====================================================================

verify_receipt_test_() ->
    EffectId = {make_ref(), 0, root},
    EffectSpec = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test},
        idempotency_key = undefined,
        timeout = undefined
    },
    Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

    %% Manually set hash (normally done by store/2)
    Hash = wf_receipt:hash_effect_spec(EffectSpec),
    UpdatedReceipt = Receipt#receipt{effect_spec_hash = Hash},

    ?_assert(wf_receipt:verify(UpdatedReceipt, EffectSpec)).

verify_tampered_receipt_fails_test_() ->
    EffectId = {make_ref(), 0, root},
    EffectSpec = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test},
        idempotency_key = undefined,
        timeout = undefined
    },
    Receipt = wf_receipt:new(EffectId, {ok, test_result}, 1000),

    %% Tamper with receipt
    TamperedReceipt = Receipt#receipt{effect_spec_hash = <<>>},

    ?_assertNot(wf_receipt:verify(TamperedReceipt, EffectSpec)).

hash_effect_spec_consistent_test_() ->
    EffectId = {make_ref(), 0, root},
    EffectSpec = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test},
        idempotency_key = undefined,
        timeout = undefined
    },

    Hash1 = wf_receipt:hash_effect_spec(EffectSpec),
    Hash2 = wf_receipt:hash_effect_spec(EffectSpec),

    ?_assertEqual(Hash1, Hash2).

hash_effect_spec_different_payload_test_() ->
    EffectId = {make_ref(), 0, root},
    EffectSpec1 = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test1},
        idempotency_key = undefined,
        timeout = undefined
    },
    EffectSpec2 = #effect_spec{
        effect_id = EffectId,
        effect_type = mock_effect,
        payload = #{data => test2},
        idempotency_key = undefined,
        timeout = undefined
    },

    Hash1 = wf_receipt:hash_effect_spec(EffectSpec1),
    Hash2 = wf_receipt:hash_effect_spec(EffectSpec2),

    ?_assertNot(Hash1 =:= Hash2).

%%====================================================================
%% Receipt Query Tests
%%====================================================================

all_receipts_for_case_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         CaseId = make_ref(),
         EffectId1 = {CaseId, 0, root},
         EffectId2 = {CaseId, 1, root},
         EffectSpec = #effect_spec{
             effect_id = EffectId1,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },

         Receipt1 = wf_receipt:new(EffectId1, {ok, result1}, 1000),
         wf_receipt:store(Receipt1, EffectSpec),

         Receipt2 = wf_receipt:new(EffectId2, {ok, result2}, 1000),
         EffectSpec2 = EffectSpec#effect_spec{effect_id = EffectId2},
         wf_receipt:store(Receipt2, EffectSpec2),

         AllReceipts = wf_receipt:all(CaseId),

         ?_assertEqual(2, length(AllReceipts))
     end}.

%%====================================================================
%% Receipt Append-Only Tests
%%====================================================================

receipt_append_only_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = wf_receipt:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         EffectId = {make_ref(), 0, root},
         EffectSpec = #effect_spec{
             effect_id = EffectId,
             effect_type = mock_effect,
             payload = #{},
             idempotency_key = undefined,
             timeout = undefined
         },
         Receipt1 = wf_receipt:new(EffectId, {ok, result1}, 1000),
         wf_receipt:store(Receipt1, EffectSpec),

         Receipt2 = wf_receipt:new(EffectId, {ok, result2}, 1000),
         wf_receipt:store(Receipt2, EffectSpec),

         %% Lookup should return the second receipt (append-only, not replace)
         {ok, Lookup} = wf_receipt:lookup(element(1, EffectId), EffectId),

         ?_assertEqual(Receipt2#receipt.receipt_id, Lookup#receipt.receipt_id)
     end}.
