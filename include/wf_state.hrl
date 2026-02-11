%%%-------------------------------------------------------------------
%%% wf_state.hrl
%%% Record definitions for wf_state module
%%% These must match the definitions in wf_state.erl
%%%-------------------------------------------------------------------

%% Token record: Logical thread of execution
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled
}).

%% Scope record: Cancel scope
-record(scope, {
    scope_id :: term(),
    parent_scope :: term() | undefined,
    status :: active | cancelled,
    tokens :: [term()],
    entered_at :: erlang:timestamp()
}).

%% Metadata record: Execution metadata
-record(metadata, {
    step_count :: non_neg_integer(),
    start_time :: erlang:timestamp(),
    last_commit_time :: erlang:timestamp() | undefined
}).

%% Mutation record: Single state mutation
-record(mutation, {
    id :: reference(),
    type :: term(),
    timestamp :: erlang:timestamp()
}).

%% Receipt record: Stub for item 010 integration
-record(receipt, {
    receipt_id :: term(),
    case_id :: term(),
    mutations :: [#mutation{}],
    timestamp :: erlang:timestamp(),
    state_before_hash :: binary(),
    state_after_hash :: binary()
}).

%% State record: Per-case execution state
-record(state, {
    case_id :: term(),
    ctx :: map(),
    tokens :: #{term() => #token{}},
    scopes :: #{term() => #scope{}},
    metadata :: #metadata{},
    status :: running | cancelled | done | failed,
    buffered_mutations :: [#mutation{}],
    ets_table :: ets:tid() | undefined
}).
