-module(wf_sched).

%%====================================================================
%% Types
%%====================================================================

-type sched_policy() :: deterministic | nondeterministic | {replay, term()}.
-type sched_decision() :: {token, term()} | {xor_branch, pos_integer()}.
-type exec_state() :: term().  %% Opaque for scheduler

%%====================================================================
%% Exported API
%%====================================================================

-export([select_action/2]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Select next action (stub implementation)
-spec select_action(exec_state(), sched_policy()) -> sched_decision().
select_action(_ExecState, Policy) ->
    %% Stub: always return token decision regardless of policy
    %% In production, would inspect exec_state to select appropriate action
    case Policy of
        undefined -> {token, mock_token};  %% Handle undefined for backward compatibility
        deterministic -> {token, mock_token};
        nondeterministic -> {token, mock_token};
        {replay, _} -> {token, replay_token}
    end.
