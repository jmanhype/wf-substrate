%%%-------------------------------------------------------------------
%%% @doc Workflow Pattern Term Algebra
%%%
%%% This module defines the closed pattern algebra AST for workflow patterns.
%%% The wf_term() type is a tagged union of 9 kernel constructors that can
%%% compose to express arbitrary control-flow patterns.
%%%
%%% == Type Transparency ==
%%% wf_term() is defined as -type (transparent) not -opaque to allow:
%%% - Pattern matching in tests and compiler
%%% - Direct access by internal modules (wf_compile, wf_validate)
%%% - Flexibility for future extensions
%%%
%%% == Constructor Design ==
%%% Smart constructors (public API) validate all inputs and throw badarg
%%% for invalid arguments. Raw constructors (prefixed with underscore) are
%%% exported for internal use by derived patterns and compiler.
%%%
%%% == Validation Scope ==
%%% well_formed/1 checks ONLY local structural invariants:
%%% - Branch counts (par, x_or, defer require >= 2)
%%% - Policy format validity (join, loop, mi policies)
%%% - Cancel scope nesting (no crossing boundaries)
%%%
%%% Global properties (deadlock, soundness, option to complete) are
%%% checked by wf_validate (item 013) using bounded model checking.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_term).

%% Kernel Types
-type ctx() :: map().
%% User state and token data flowing through workflow.
%% Users structure this arbitrarily; substrate treats as opaque.

-type scope_id() :: term().
%% Identifier for cancellation scopes.
%% Can be atom(), integer(), reference(), or tuple().
%% Validation checks nesting, not format.

-type case_id() :: term().
%% Identifier for workflow instances.
%% Format is user-defined; substrate treats as opaque.

-type receipt() :: term().
%% Receipt for effects and commits.
%% Structure defined by wf_effect (future item).

-type task_fun() :: fun((ctx()) ->
    {ok, ctx()} |
    {error, term()} |
    {effect, term(), ctx()}).
%% Task function signature.
%% Returns ok with updated context, error with reason,
%% or effect with spec and continuation context.

-type task_metadata() :: #{
    function := task_fun(),
    timeout => timeout(),
    retry => non_neg_integer(),
    description => binary()
}.
%% Task metadata map.
%% Required key: function (the task implementation)
%% Optional keys: timeout (execution timeout), retry (retry count),
%%                description (human-readable description)

%% Policy Types
-type join_policy() ::
    all |
    sync_merge |
    {first_n, pos_integer()} |
    {n_of_m, pos_integer(), pos_integer()}.
%% Join synchronization policies.
%% - all: Wait for all branches to complete
%% - sync_merge: Synchronizing merge (full sync with ordering)
%% - {first_n, N}: Wait for N branches, cancel remainder
%% - {n_of_m, N, M}: Wait for N out of M branches (explicit counts)

-type loop_policy() ::
    while |
    until |
    {count, non_neg_integer()}.
%% Loop iteration policies.
%% - while: Loop while condition holds (check before)
%% - until: Loop until condition holds (check after)
%% - {count, N}: Loop exactly N times

-type mi_policy() ::
    {fixed, pos_integer()} |
    {dynamic, pos_integer(), pos_integer()}.
%% Multiple instance policies.
%% - {fixed, N}: Create exactly N instances
%% - {dynamic, Min, Max}: Create between Min and Max instances dynamically

%% Cancellation Types
-type cancel_scope() :: scope_id() | {scope_id(), [cancel_option()]}.
%% Cancellation scope specification.
%% Simple form: just a scope identifier
%% Extended form: scope identifier with options

-type cancel_option() ::
    {timeout, timeout()} |
    {notify, pid()}.
%% Cancellation scope options.
%% - {timeout, T}: Cancel scope after T milliseconds
%% - {notify, Pid}: Send notification to Pid when scope cancelled

%% Validation Error Types
-type validation_error() ::
    {invalid_branch_count, atom(), pos_integer(), pos_integer()} |
    {invalid_join_policy, join_policy()} |
    {invalid_loop_policy, loop_policy()} |
    {invalid_mi_policy, mi_policy()} |
    {invalid_task_metadata, term()} |
    {malformed_cancel_scope, cancel_scope()} |
    {nested_cancel_scope, scope_id(), scope_id()}.
%% Validation error descriptions.
%% - {invalid_branch_count, NodeType, Actual, Minimum}: Too few branches
%% - {invalid_join_policy, Policy}: Malformed join policy
%% - {invalid_loop_policy, Policy}: Malformed loop policy
%% - {invalid_mi_policy, Policy}: Malformed MI policy
%% - {invalid_task_metadata, Metadata}: Task metadata missing required keys
%% - {malformed_cancel_scope, Scope}: Invalid cancel scope format
%% - {nested_cancel_scope, Outer, Inner}: Crossing cancel boundaries

%% Pattern Algebra - The Union Type
-type wf_term() ::
    {task, atom(), task_metadata()} |
    {seq, wf_term(), wf_term()} |
    {par, [wf_term()]} |
    {x_or, [wf_term()]} |
    {join, join_policy(), [wf_term()]} |
    {loop, loop_policy(), wf_term()} |
    {defer, [wf_term()]} |
    {cancel, cancel_scope(), wf_term()} |
    {mi, mi_policy(), wf_term()}.
%% The closed pattern algebra.
%% A wf_term() is one of 9 kernel constructors:
%% - {task, Name, Metadata}: Named task with function metadata
%% - {seq, P, Q}: Sequential composition (P then Q)
%% - {par, Branches}: Parallel split (AND-split)
%% - {x_or, Alternatives}: Exclusive choice (XOR-split)
%% - {join, Policy, Branches}: Generalized join with policy
%% - {loop, Policy, Body}: Structured loop
%% - {defer, Alternatives}: Deferred choice (race)
%% - {cancel, Scope, Body}: Cancellation scope
%% - {mi, Policy, Body}: Multiple instances

%% Export types for use by other modules
-export_type([
    ctx/0,
    scope_id/0,
    case_id/0,
    receipt/0,
    task_fun/0,
    task_metadata/0,
    join_policy/0,
    loop_policy/0,
    mi_policy/0,
    cancel_scope/0,
    cancel_option/0,
    validation_error/0,
    wf_term/0
]).

%% Export raw constructors with raw_ prefix
-export([
    raw_task/2,
    raw_seq/2,
    raw_par/1,
    raw_x_or/1,
    raw_join/2,
    raw_loop/2,
    raw_defer/1,
    raw_cancel/2,
    raw_mi/2,
    task/2,
    seq/2,
    par/1,
    x_or/1,
    join/2,
    loop/2,
    defer/1,
    cancel/2,
    mi/2,
    well_formed/1
]).

%%%===================================================================
%%% Raw Constructors (Internal Use)
%%%===================================================================
%%% These constructors create wf_term() tuples without validation.
%%% Use ONLY for internal operations where inputs are known-valid.
%%% Public API should use smart constructors below.

%%--------------------------------------------------------------------
%% @doc Raw task constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_task(atom(), task_metadata()) -> wf_term().
raw_task(Name, Metadata) ->
    {task, Name, Metadata}.

%%--------------------------------------------------------------------
%% @doc Raw sequential composition constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_seq(wf_term(), wf_term()) -> wf_term().
raw_seq(P, Q) ->
    {seq, P, Q}.

%%--------------------------------------------------------------------
%% @doc Raw parallel split constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_par([wf_term()]) -> wf_term().
raw_par(Branches) ->
    {par, Branches}.

%%--------------------------------------------------------------------
%% @doc Raw exclusive choice constructor (no validation).
%% Note: Function named 'raw_x_or' to avoid conflict with reserved keyword.
%% @private
%%--------------------------------------------------------------------
-spec raw_x_or([wf_term()]) -> wf_term().
raw_x_or(Alternatives) ->
    {x_or, Alternatives}.

%%--------------------------------------------------------------------
%% @doc Raw join constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_join(join_policy(), [wf_term()]) -> wf_term().
raw_join(Policy, Branches) ->
    {join, Policy, Branches}.

%%--------------------------------------------------------------------
%% @doc Raw loop constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_loop(loop_policy(), wf_term()) -> wf_term().
raw_loop(Policy, Body) ->
    {loop, Policy, Body}.

%%--------------------------------------------------------------------
%% @doc Raw deferred choice constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_defer([wf_term()]) -> wf_term().
raw_defer(Alternatives) ->
    {defer, Alternatives}.

%%--------------------------------------------------------------------
%% @doc Raw cancel scope constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_cancel(cancel_scope(), wf_term()) -> wf_term().
raw_cancel(Scope, Body) ->
    {cancel, Scope, Body}.

%%--------------------------------------------------------------------
%% @doc Raw multiple instances constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec raw_mi(mi_policy(), wf_term()) -> wf_term().
raw_mi(Policy, Body) ->
    {mi, Policy, Body}.

%%%===================================================================
%%% Smart Constructors (Public API)
%%%===================================================================
%%% These constructors validate inputs and throw badarg for invalid arguments.
%%% Use these for all external code and user-facing APIs.

%%--------------------------------------------------------------------
%% @doc Construct a task term with validation.
%%
%% Validates that Name is an atom and Metadata is a map containing
%% the required 'function' key with a fun value.
%%
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec task(atom(), task_metadata()) -> wf_term().
task(Name, Metadata) when is_atom(Name), is_map(Metadata) ->
    case maps:is_key(function, Metadata) of
        false ->
            error({badarg, {invalid_task_metadata, Metadata,
                            "Metadata must contain 'function' key"}});
        true ->
            Fun = maps:get(function, Metadata),
            case is_function(Fun) of
                false ->
                    error({badarg, {invalid_task_metadata, Metadata,
                                    "Metadata 'function' must be a fun"}});
                true ->
                    raw_task(Name, Metadata)
            end
    end;
task(_Name, _Metadata) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct sequential composition with validation.
%%
%% Both terms must be wf_term(). Validation is deferred to well_formed/1
%% for deep structural checks.
%%
%% @end
%%--------------------------------------------------------------------
-spec seq(wf_term(), wf_term()) -> wf_term().
seq(P, Q) ->
    %% Basic type check: both must be tagged tuples
    true = is_tuple(P),
    true = is_tuple(Q),
    true = tuple_size(P) >= 2,
    true = tuple_size(Q) >= 2,
    raw_seq(P, Q).

%%--------------------------------------------------------------------
%% @doc Construct parallel split with validation.
%%
%% Branches must be a non-empty list of wf_term() with length >= 2.
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec par([wf_term()]) -> wf_term().
par(Branches) when is_list(Branches), length(Branches) >= 2 ->
    raw_par(Branches);
par(_Branches) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct exclusive choice with validation.
%%
%% Alternatives must be a non-empty list of wf_term() with length >= 2.
%% Throws badarg if validation fails.
%% Note: Function named 'x_or' to avoid conflict with reserved keyword.
%%
%% @end
%%--------------------------------------------------------------------
-spec x_or([wf_term()]) -> wf_term().
x_or(Alternatives) when is_list(Alternatives), length(Alternatives) >= 2 ->
    raw_x_or(Alternatives);
x_or(_Alternatives) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct join with validation.
%%
%% Policy must be a valid join_policy(). Branches must be a list with
%% length >= 2. Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec join(join_policy(), [wf_term()]) -> wf_term().
join(Policy, Branches) when is_list(Branches), length(Branches) >= 2 ->
    case validate_join_policy_format(Policy) of
        true -> raw_join(Policy, Branches);
        false -> error({badarg, {invalid_join_policy, Policy}})
    end;
join(_Policy, _Branches) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct loop with validation.
%%
%% Policy must be a valid loop_policy(). Body must be a wf_term().
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop(loop_policy(), wf_term()) -> wf_term().
loop(Policy, Body) ->
    case validate_loop_policy_format(Policy) of
        true ->
            true = is_tuple(Body),
            true = tuple_size(Body) >= 2,
            raw_loop(Policy, Body);
        false ->
            error({badarg, {invalid_loop_policy, Policy}})
    end.

%%--------------------------------------------------------------------
%% @doc Construct deferred choice with validation.
%%
%% Alternatives must be a non-empty list of wf_term() with length >= 2.
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec defer([wf_term()]) -> wf_term().
defer(Alternatives) when is_list(Alternatives), length(Alternatives) >= 2 ->
    raw_defer(Alternatives);
defer(_Alternatives) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct cancel scope with validation.
%%
%% Scope can be any term(). Body must be a wf_term().
%% Throws badarg if Body is not a valid wf_term().
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel(cancel_scope(), wf_term()) -> wf_term().
cancel(Scope, Body) ->
    true = is_tuple(Body),
    true = tuple_size(Body) >= 2,
    raw_cancel(Scope, Body).

%%--------------------------------------------------------------------
%% @doc Construct multiple instances with validation.
%%
%% Policy must be a valid mi_policy(). Body must be a wf_term().
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec mi(mi_policy(), wf_term()) -> wf_term().
mi(Policy, Body) ->
    case validate_mi_policy_format(Policy) of
        true ->
            true = is_tuple(Body),
            true = tuple_size(Body) >= 2,
            raw_mi(Policy, Body);
        false ->
            error({badarg, {invalid_mi_policy, Policy}})
    end.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validate join policy format (not semantics).
%%--------------------------------------------------------------------
-spec validate_join_policy_format(join_policy()) -> boolean().
validate_join_policy_format(all) -> true;
validate_join_policy_format(sync_merge) -> true;
validate_join_policy_format({first_n, N}) when is_integer(N), N > 0 -> true;
validate_join_policy_format({n_of_m, N, M}) when is_integer(N), is_integer(M),
                                                   N > 0, M > 0, N =< M -> true;
validate_join_policy_format(_Policy) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate loop policy format (not semantics).
%%--------------------------------------------------------------------
-spec validate_loop_policy_format(loop_policy()) -> boolean().
validate_loop_policy_format(while) -> true;
validate_loop_policy_format(until) -> true;
validate_loop_policy_format({count, N}) when is_integer(N), N >= 0 -> true;
validate_loop_policy_format(_Policy) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate multiple instance policy format (not semantics).
%%--------------------------------------------------------------------
-spec validate_mi_policy_format(mi_policy()) -> boolean().
validate_mi_policy_format({fixed, N}) when is_integer(N), N > 0 -> true;
validate_mi_policy_format({dynamic, Min, Max}) when is_integer(Min), is_integer(Max),
                                                     Min > 0, Max >= Min -> true;
validate_mi_policy_format(_Policy) -> false.

%%%===================================================================
%%% Structural Validation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Validate that a wf_term() is well-formed.
%%
%% Checks local structural invariants:
%% - Branch counts (par, x_or, defer require >= 2)
%% - Policy format validity
%% - Cancel scope nesting (no crossing boundaries)
%% - Type consistency (all list elements are wf_term())
%%
%% Does NOT check global properties (deadlock, soundness, option to complete).
%% Those checks are implemented in wf_validate (item 013).
%%
%% Returns ok for valid terms, {error, Errors} for invalid terms.
%%
%% @end
%%--------------------------------------------------------------------
-spec well_formed(wf_term()) -> ok | {error, [validation_error()]}.
well_formed(Term) ->
    case validate_node(Term, #{scope_stack => []}) of
        {ok, _} -> ok;
        {error, Errors} -> {error, lists:usort(Errors)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate a single node and its children, accumulating errors.
%%--------------------------------------------------------------------
-spec validate_node(wf_term(), map()) -> {ok, map()} | {error, [validation_error()]}.
validate_node({task, _Name, Metadata}, Context) ->
    %% Validate task metadata
    case maps:is_key(function, Metadata) of
        false -> {error, [{invalid_task_metadata, Metadata}]};
        true ->
            Fun = maps:get(function, Metadata),
            case is_function(Fun) of
                false -> {error, [{invalid_task_metadata, Metadata}]};
                true -> {ok, Context}
            end
    end;

validate_node({seq, P, Q}, Context) ->
    %% Validate left and right recursively
    case validate_node(P, Context) of
        {error, Errors1} ->
            case validate_node(Q, Context) of
                {error, Errors2} -> {error, Errors1 ++ Errors2};
                {ok, _} -> {error, Errors1}
            end;
        {ok, _} ->
            validate_node(Q, Context)
    end;

validate_node({par, Branches}, Context) ->
    %% Validate branch count
    case length(Branches) < 2 of
        true -> {error, [{invalid_branch_count, par, length(Branches), 2}]};
        false ->
            %% Validate all branches are wf_term()
            case validate_list(Branches, Context) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({x_or, Alternatives}, Context) ->
    %% Validate branch count
    case length(Alternatives) < 2 of
        true -> {error, [{invalid_branch_count, x_or, length(Alternatives), 2}]};
        false ->
            %% Validate all alternatives are wf_term()
            case validate_list(Alternatives, Context) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({join, Policy, Branches}, Context) ->
    %% Validate branch count
    case length(Branches) < 2 of
        true -> {error, [{invalid_branch_count, join, length(Branches), 2}]};
        false ->
            %% Validate policy format
            case validate_join_policy_format(Policy) of
                false -> {error, [{invalid_join_policy, Policy}]};
                true ->
                    %% Validate all branches are wf_term()
                    case validate_list(Branches, Context) of
                        {error, Errors} -> {error, Errors};
                        {ok, _} -> {ok, Context}
                    end
            end
    end;

validate_node({loop, Policy, Body}, Context) ->
    %% Validate policy format
    case validate_loop_policy_format(Policy) of
        false -> {error, [{invalid_loop_policy, Policy}]};
        true -> validate_node(Body, Context)
    end;

validate_node({defer, Alternatives}, Context) ->
    %% Validate branch count
    case length(Alternatives) < 2 of
        true -> {error, [{invalid_branch_count, defer, length(Alternatives), 2}]};
        false ->
            %% Validate all alternatives are wf_term()
            case validate_list(Alternatives, Context) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({cancel, Scope, Body}, Context) ->
    %% Validate scope format
    case validate_cancel_scope_format(Scope) of
        false -> {error, [{malformed_cancel_scope, Scope}]};
        true ->
            %% Extract scope ID
            case Scope of
                {ScopeId, _Opts} -> ok;
                ScopeId -> ok
            end,
            %% Track scope nesting
            NewContext = Context#{scope_stack => [ScopeId | maps:get(scope_stack, Context)]},
            case validate_node(Body, NewContext) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({mi, Policy, Body}, Context) ->
    %% Validate policy format
    case validate_mi_policy_format(Policy) of
        false -> {error, [{invalid_mi_policy, Policy}]};
        true -> validate_node(Body, Context)
    end;

validate_node(_InvalidTerm, _Context) ->
    %% Not a valid wf_term() structure - use a generic error
    {error, [{malformed_term, unknown}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate all elements in a list, accumulating errors.
%%--------------------------------------------------------------------
-spec validate_list([wf_term()], map()) -> {ok, map()} | {error, [validation_error()]}.
validate_list([], Context) ->
    {ok, Context};
validate_list([Term | Rest], Context) ->
    case validate_node(Term, Context) of
        {error, Errors1} ->
            case validate_list(Rest, Context) of
                {error, Errors2} -> {error, Errors1 ++ Errors2};
                {ok, _} -> {error, Errors1}
            end;
        {ok, _} ->
            validate_list(Rest, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate cancel scope format (not semantics).
%%--------------------------------------------------------------------
-spec validate_cancel_scope_format(cancel_scope()) -> boolean().
validate_cancel_scope_format(ScopeId) when is_atom(ScopeId); is_integer(ScopeId);
                                            is_reference(ScopeId); is_tuple(ScopeId) ->
    true;
validate_cancel_scope_format({ScopeId, Opts}) when is_atom(ScopeId); is_integer(ScopeId);
                                                    is_reference(ScopeId); is_tuple(ScopeId),
                                                    is_list(Opts) ->
    case validate_cancel_options(Opts) of
        true -> true;
        false -> false
    end;
validate_cancel_scope_format(_Scope) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate cancel options list format.
%%--------------------------------------------------------------------
-spec validate_cancel_options([cancel_option()]) -> boolean().
validate_cancel_options([]) ->
    true;
validate_cancel_options([{timeout, T} | Rest]) when is_integer(T); T =:= infinity ->
    validate_cancel_options(Rest);
validate_cancel_options([{notify, Pid} | Rest]) when is_pid(Pid) ->
    validate_cancel_options(Rest);
validate_cancel_options(_BadOption) ->
    false.
