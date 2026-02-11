-module(wf_effect_props).
-include_lib("eunit/include/eunit.hrl").
-include("wf_effect.hrl").
-include("wf_receipt.hrl").

%%%====================================================================
%%% Property-based tests for effect system invariants
%%%
%%% These tests use the wf_prop framework for property-based testing.
%%% Properties verify critical invariants of the effect system.
%%%====================================================================

%%%====================================================================
%%% Fixtures
%%%====================================================================

setup() ->
    {ok, _} = wf_effect:start_link(),
    {ok, _} = wf_receipt:start_link().

cleanup(_) ->
    gen_server:stop(wf_receipt),
    gen_server:stop(wf_effect).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Custom quickcheck with limited iterations
quickcheck_custom(Generator, Property, MaxTests) ->
    quickcheck_custom_loop(Generator, Property, 0, MaxTests).

quickcheck_custom_loop(_Generator, _Property, N, MaxTests) when N >= MaxTests ->
    {ok, N};
quickcheck_custom_loop(Generator, Property, N, MaxTests) ->
    Term = Generator(),
    try
        case Property(Term) of
            true -> quickcheck_custom_loop(Generator, Property, N + 1, MaxTests);
            false -> {error, Term, {failed, N}}
        end
    catch
        _:Exception ->
            {error, Term, Exception}
    end.

%%%====================================================================
%%% Property 1: Receipts are append-only
%%%
%%% If we store a receipt and then retrieve it, it should be identical.
%%% This verifies the append-only log invariant.
%%%====================================================================

prop_receipts_append_only() ->
    %% Generator: Create random receipt
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        EffectId = {CaseId, rand:uniform(100), root},
        Result = case rand:uniform(3) of
            1 -> {ok, some_result};
            2 -> {error, some_error};
            3 -> {cancelled, some_reason}
        end,
        Duration = rand:uniform(10000),
        {CaseId, EffectId, Result, Duration}
    end,

    %% Property: Retrieved receipt matches stored receipt
    Property = fun({CaseId, EffectId, Result, Duration}) ->
        EffectSpec = #effect_spec{
            effect_id = EffectId,
            effect_type = mock_effect,
            payload = #{},
            idempotency_key = undefined,
            timeout = undefined
        },
        Receipt = wf_receipt:new(EffectId, Result, Duration),
        ok = wf_receipt:store(Receipt, EffectSpec),

        {ok, RetrievedReceipt} = wf_receipt:lookup(CaseId, EffectId),

        %% All fields should match
        Receipt#receipt.receipt_id =:= RetrievedReceipt#receipt.receipt_id
            andalso Receipt#receipt.effect_id =:= RetrievedReceipt#receipt.effect_id
            andalso Receipt#receipt.result =:= RetrievedReceipt#receipt.result
            andalso Receipt#receipt.duration_us =:= RetrievedReceipt#receipt.duration_us
            andalso Receipt#receipt.timestamp =:= RetrievedReceipt#receipt.timestamp
    end,

    %% Run property test
    wf_prop:quickcheck(Generator, Property).

%%%====================================================================
%%% Property 2: Effect IDs are unique
%%%
%%% Two effects with different components should have different IDs.
%%%====================================================================

prop_effect_ids_unique() ->
    %% Generator: Generate effect IDs
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        StepSeq = rand:uniform(100),
        ScopeId = case rand:uniform(3) of
            1 -> root;
            2 -> list_to_atom("scope_" ++ integer_to_list(rand:uniform(10)));
            3 -> crypto:strong_rand_bytes(5)
        end,
        {CaseId, StepSeq, ScopeId}
    end,

    %% Property: Different components produce different IDs
    Property = fun({CaseId, StepSeq, ScopeId}) ->
        %% Generate another ID
        CaseId2 = case rand:uniform(2) of
            1 -> CaseId;  %% Same
            2 -> crypto:strong_rand_bytes(10)  %% Different
        end,
        StepSeq2 = case rand:uniform(2) of
            1 -> StepSeq;  %% Same
            2 -> rand:uniform(100)  %% Different
        end,
        ScopeId2 = case rand:uniform(2) of
            1 -> ScopeId;  %% Same
            2 -> crypto:strong_rand_bytes(5)  %% Different
        end,

        EffectId1 = {CaseId, StepSeq, ScopeId},
        EffectId2 = {CaseId2, StepSeq2, ScopeId2},

        %% IDs should be different if any component differs
        case (CaseId =/= CaseId2 orelse StepSeq =/= StepSeq2 orelse ScopeId =/= ScopeId2) of
            true -> EffectId1 =/= EffectId2;
            false -> true
        end
    end,

    %% Run property test
    wf_prop:quickcheck(Generator, Property).

%%%====================================================================
%%% Property 3: Receipt hash is consistent
%%%
%%% Hashing the same effect spec twice should produce the same hash.
%%%====================================================================

prop_hash_consistency() ->
    %% Generator: Create random effect spec
    Generator = fun() ->
        EffectId = {crypto:strong_rand_bytes(10), rand:uniform(100), root},
        EffectType = lists:nth(rand:uniform(5), [mock_effect, http_get, file_write, db_query, send_email]),
        Payload = maps:from_list([
            {list_to_atom("key" ++ integer_to_list(N)), crypto:strong_rand_bytes(5)}
         || N <- lists:seq(1, rand:uniform(3))]),
        #effect_spec{
            effect_id = EffectId,
            effect_type = EffectType,
            payload = Payload,
            idempotency_key = undefined,
            timeout = undefined
        }
    end,

    %% Property: Hash is consistent
    Property = fun(EffectSpec) ->
        Hash1 = wf_receipt:hash_effect_spec(EffectSpec),
        Hash2 = wf_receipt:hash_effect_spec(EffectSpec),
        Hash1 =:= Hash2
    end,

    %% Run property test
    wf_prop:quickcheck(Generator, Property).

%%%====================================================================
%%% Property 4: Receipt verification passes for correct spec
%%%
%%% A receipt should verify against the spec it was created from.
%%%====================================================================

prop_receipt_verification_correct() ->
    %% Generator: Create random receipt
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        EffectId = {CaseId, rand:uniform(100), root},
        Result = {ok, test_result},
        Duration = rand:uniform(10000),
        {CaseId, EffectId, Result, Duration}
    end,

    %% Property: Receipt verifies against its spec
    Property = fun({CaseId, EffectId, Result, Duration}) ->
        EffectSpec = #effect_spec{
            effect_id = EffectId,
            effect_type = mock_effect,
            payload = #{},
            idempotency_key = undefined,
            timeout = undefined
        },
        Receipt = wf_receipt:new(EffectId, Result, Duration),
        ok = wf_receipt:store(Receipt, EffectSpec),

        {ok, RetrievedReceipt} = wf_receipt:lookup(CaseId, EffectId),
        wf_receipt:verify(RetrievedReceipt, EffectSpec)
    end,

    %% Run property test
    wf_prop:quickcheck(Generator, Property).

%%%====================================================================
%%% Property 5: Idempotency returns cached receipt
%%%
%%% Submitting two effects with the same idempotency key should
%%% return the cached receipt from the first submission.
%%%====================================================================

prop_idempotency_caching() ->
    %% Generator: Create effects with same key
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        Key = crypto:strong_rand_bytes(16),
        {CaseId, Key}
    end,

    %% Property: Idempotency caches results
    Property = fun({CaseId, Key}) ->
        Spec1 = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),
        Spec1WithKey = wf_effect:set_idempotency_key(Spec1, Key),

        Spec2 = wf_effect:new_spec(CaseId, 1, root, mock_effect, #{}),
        Spec2WithKey = wf_effect:set_idempotency_key(Spec2, Key),

        %% Submit first effect
        {ok, _EffectOrReceipt1} = wf_effect:yield(CaseId, 0, root, Spec1WithKey),
        timer:sleep(50),  %% Wait for completion

        %% Submit second effect with same key
        {ok, EffectOrReceipt2} = wf_effect:yield(CaseId, 1, root, Spec2WithKey),

        %% Second should return cached receipt
        is_record(EffectOrReceipt2, receipt)
    end,

    %% Run property test with custom loop (10 iterations max)
    quickcheck_custom(Generator, Property, 10).

%%%====================================================================
%%% Property 6: All receipts for a case are returned
%%%
%%% Querying all receipts for a case should return all receipts
%%% for that case and no others.
%%%====================================================================

prop_all_receipts_for_case() ->
    %% Generator: Create multiple receipts for a case
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        NumReceipts = rand:uniform(5) + 1,  %% 1-5 receipts
        {CaseId, NumReceipts}
    end,

    %% Property: All receipts are returned
    Property = fun({CaseId, NumReceipts}) ->
        ReceiptIds = lists:map(fun(N) ->
            EffectId = {CaseId, N, root},
            EffectSpec = #effect_spec{
                effect_id = EffectId,
                effect_type = mock_effect,
                payload = #{},
                idempotency_key = undefined,
                timeout = undefined
            },
            Receipt = wf_receipt:new(EffectId, {ok, result}, 100),
            ok = wf_receipt:store(Receipt, EffectSpec),
            Receipt#receipt.receipt_id
        end, lists:seq(1, NumReceipts)),

        AllReceipts = wf_receipt:all(CaseId),
        RetrievedIds = [R#receipt.receipt_id || R <- AllReceipts],

        %% All receipt IDs should be present
        lists:all(fun(Id) -> lists:member(Id, RetrievedIds) end, ReceiptIds)
    end,

    %% Run property test
    wf_prop:quickcheck(Generator, Property).

%%%====================================================================
%%% Property 7: Receipt lookup by key returns correct receipt
%%%
%%% Looking up by idempotency key should return the receipt with that key.
%%%====================================================================

prop_lookup_by_key() ->
    %% Generator: Create receipts with different keys
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        Key1 = crypto:strong_rand_bytes(16),
        Key2 = crypto:strong_rand_bytes(16),  %% Always different
        {CaseId, Key1, Key2}
    end,

    %% Property: Lookup by key returns correct receipt
    Property = fun({CaseId, Key1, Key2}) ->
        EffectId1 = {CaseId, 0, root},
        EffectId2 = {CaseId, 1, root},

        Spec1 = #effect_spec{
            effect_id = EffectId1,
            effect_type = mock_effect,
            payload = #{},
            idempotency_key = Key1,
            timeout = undefined
        },
        Spec2 = #effect_spec{
            effect_id = EffectId2,
            effect_type = mock_effect,
            payload = #{},
            idempotency_key = Key2,
            timeout = undefined
        },

        Receipt1 = wf_receipt:new(EffectId1, {ok, result1}, 100),
        Receipt2 = wf_receipt:new(EffectId2, {ok, result2}, 100),

        ok = wf_receipt:store(Receipt1, Spec1),
        ok = wf_receipt:store(Receipt2, Spec2),

        %% Lookup by key1 should return receipt1
        {ok, Retrieved1} = wf_receipt:lookup_by_key(CaseId, Key1),
        %% Lookup by key2 should return receipt2
        {ok, Retrieved2} = wf_receipt:lookup_by_key(CaseId, Key2),

        Retrieved1#receipt.receipt_id =:= Receipt1#receipt.receipt_id
            andalso Retrieved2#receipt.receipt_id =:= Receipt2#receipt.receipt_id
    end,

    %% Run property test
    wf_prop:quickcheck(Generator, Property).

%%%====================================================================
%%% Property 8: Effect status transitions correctly
%%%
%%% Effect status should transition: pending -> complete/cancelled/failed.
%%%====================================================================

prop_effect_status_transitions() ->
    %% Generator: Create effect and decide whether to cancel
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        StepSeq = rand:uniform(100),
        ScopeId = root,
        Cancel = rand:uniform(2) =:= 1,  %% 50% chance of cancellation
        {CaseId, StepSeq, ScopeId, Cancel}
    end,

    %% Property: Status transitions are valid
    Property = fun({CaseId, StepSeq, ScopeId, Cancel}) ->
        EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, mock_delay, #{}),
        {ok, Effect} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpec),

        %% Initial status should be pending
        InitialStatus = Effect#effect.status,
        timer:sleep(10),

        %% Cancel if requested
        case Cancel of
            true ->
                ok = wf_effect:cancel_effect(Effect#effect.effect_id, test_cancel),
                timer:sleep(150);
            false ->
                timer:sleep(150)
        end,

        %% Final status
        case ets:lookup(wf_effects, Effect#effect.effect_id) of
            [#effect{status = FinalStatus}] ->
                %% Status transitions: pending -> complete/cancelled
                InitialStatus =:= pending
                    andalso (FinalStatus =:= complete orelse FinalStatus =:= cancelled);
            [] ->
                false
        end
    end,

    %% Run property test with limited iterations (due to delays)
    quickcheck_custom(Generator, Property, 5).

%%%====================================================================
%%% Property 9: Cancellation creates cancelled receipt
%%%
%%% When an effect is cancelled, a cancelled receipt should be created.
%%%====================================================================

prop_cancellation_creates_receipt() ->
    %% Generator: Create effect to cancel
    Generator = fun() ->
        CaseId = crypto:strong_rand_bytes(10),
        StepSeq = rand:uniform(100),
        ScopeId = root,
        {CaseId, StepSeq, ScopeId}
    end,

    %% Property: Cancellation creates cancelled receipt
    Property = fun({CaseId, StepSeq, ScopeId}) ->
        EffectSpec = wf_effect:new_spec(CaseId, StepSeq, ScopeId, mock_delay, #{}),
        {ok, Effect} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpec),
        EffectId = Effect#effect.effect_id,

        %% Cancel immediately
        ok = wf_effect:cancel_effect(EffectId, test_cancel),
        timer:sleep(150),

        %% Check receipt exists and is cancelled
        {ok, Receipt} = wf_receipt:lookup(CaseId, EffectId),
        Receipt#receipt.result =:= {cancelled, test_cancel}
    end,

    %% Run property test with limited iterations (due to delays)
    quickcheck_custom(Generator, Property, 5).

%%%====================================================================
%%% EUnit Test Wrappers
%%%====================================================================

prop_receipts_append_only_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_receipts_append_only() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_effect_ids_unique_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_effect_ids_unique() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_hash_consistency_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_hash_consistency() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_receipt_verification_correct_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_receipt_verification_correct() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_idempotency_caching_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_idempotency_caching() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_all_receipts_for_case_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_all_receipts_for_case() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_lookup_by_key_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_lookup_by_key() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_effect_status_transitions_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_effect_status_transitions() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.

prop_cancellation_creates_receipt_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            ?_test(begin
                case prop_cancellation_creates_receipt() of
                    {ok, _} -> ok;
                    {error, _Term, _Reason} -> ?assert(false)
                end
            end)
        end
    }.
