%%%-------------------------------------------------------------------
%%% @doc Top-level supervisor for wf_substrate
%%% @end
%%%-------------------------------------------------------------------
-module(wf_substrate_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child specifications.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [
        #{id => wf_case_sup,
          start => {wf_case_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [wf_case_sup]},
        #{id => wf_governance,
          start => {wf_governance, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [wf_governance]},
        #{id => wf_budget,
          start => {wf_budget, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [wf_budget]},
        #{id => wf_approval,
          start => {wf_approval, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [wf_approval]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
