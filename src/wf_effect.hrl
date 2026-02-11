%%%-------------------------------------------------------------------
%%% @doc wf_effect record definitions
%%% This header file exports effect_spec and effect records for use
%%% in tests and other modules (wf_exec, wf_receipt, etc.)
%%%-------------------------------------------------------------------

%% Effect specification (submitted by task)
-record(effect_spec, {
    effect_id,
    effect_type,
    payload,
    idempotency_key,
    timeout
}).

%% Active effect (running/completed/cancelled)
-record(effect, {
    effect_id,
    spec,
    status,
    submitted_at,
    completed_at,
    result,
    reason
}).
