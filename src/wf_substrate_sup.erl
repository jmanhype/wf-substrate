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
        %% wf_case_sup removed - it's a simple_one_for_one supervisor with empty child spec
        %% which causes {bad_start_spec,[]} error in OTP 26
        %% It can be started dynamically via wf_case_sup:start_link/0 when needed
        {wf_governance,
         {wf_governance, start_link, []},
         permanent, 5000, worker, [wf_governance]},
        {wf_budget,
         {wf_budget, start_link, []},
         permanent, 5000, worker, [wf_budget]},
        {wf_approval,
         {wf_approval, start_link, []},
         permanent, 5000, worker, [wf_approval]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
