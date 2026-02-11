%%%-------------------------------------------------------------------
%%% @doc wf_receipt record definitions
%%% This header file exports receipt record for use in tests and
%%% other modules (wf_effect, wf_exec, etc.)
%%%-------------------------------------------------------------------

%% Receipt record: Immutable receipt for completed effect
-record(receipt, {
    receipt_id,
    case_id,
    effect_id,
    effect_spec_hash,
    idempotency_key,
    timestamp,
    result,
    duration_us
}).
