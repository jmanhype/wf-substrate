%%%-------------------------------------------------------------------
%%% wf_cancel.hrl
%%% Record definitions for wf_cancel module
%%%-------------------------------------------------------------------

%% Cancel event records
-record(cancel_activity, {
    scope_id :: term(),
    cancelled_tokens :: [term()],
    cancelled_effects :: [term()],
    timestamp :: erlang:timestamp()
}).

-record(cancel_case, {
    scope_id :: term(),
    cancelled_tokens :: [term()],
    cancelled_effects :: [term()],
    timestamp :: erlang:timestamp()
}).

-record(cancel_region, {
    scope_id :: term(),
    cancelled_tokens :: [term()],
    cancelled_effects :: [term()],
    timestamp :: erlang:timestamp()
}).
