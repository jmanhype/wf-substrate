%%%-------------------------------------------------------------------
%%% @doc Governance error record for structured violation reporting
%%% @end
%%%-------------------------------------------------------------------

%% Governance error record
-record(governance_error, {
    error_type :: allowlist_violation | budget_exceeded | timeout | approval_required,
    scope :: term(),
    task_id :: term() | undefined,
    detail :: binary(),
    timestamp :: erlang:timestamp()
}).

%% Governance policy types
-type scope_id() :: term().
-type case_id() :: term().
-type task_id() :: term().
-type effect_type() :: atom().

%% Budget types
-type budget_limit() :: {max_effects, non_neg_integer()} |
                      {max_time_us, non_neg_integer()} |
                      {max_cost, non_neg_integer()}.

%% Timeout types
-type timeout_policy() :: {case_timeout, non_neg_integer()} |
                        {task_timeout, non_neg_integer()} |
                        {effect_timeout, non_neg_integer()}.

%% Approval types
-type approval_id() :: term().
-type approval_action() :: cancel | default | escalate.
-type approval_spec() :: #{
    approval_id => approval_id(),
    timeout => non_neg_integer() | undefined,
    timeout_action => approval_action(),
    detail => binary()
}.
