%%%-------------------------------------------------------------------
%%% @doc Dynamic supervisor for workflow case runners
%%%
%%% Uses simple_one_for_one strategy to dynamically spawn
%%% per-case gen_statem processes. Cases are temporary (not
%%% restarted on normal termination) to avoid replaying
%%% execution state.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_case_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_case/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Type definitions
%%%====================================================================

-type case_id() :: term().
-type bytecode() :: wf_vm:wf_bc().
-type options() :: map().

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new case runner
-spec start_case(case_id(), bytecode(), options()) -> {ok, pid()} | {error, term()}.
start_case(CaseId, Bytecode, Options) ->
    %% Check if case already exists
    PidName = wf_case_runner:wf_case_pid_name(CaseId),
    case whereis(PidName) of
        undefined ->
            ChildSpec = #{
                id => CaseId,
                start => {wf_case_runner, start_link, [CaseId, Bytecode, Options]},
                restart => temporary,  %% Don't restart on normal termination
                shutdown => 5000,
                type => worker,
                modules => [wf_case_runner]
            },
            case supervisor:start_child(?SERVER, ChildSpec) of
                {ok, Pid} ->
                    %% Register process with a local name
                    try
                        true = register(PidName, Pid),
                        {ok, Pid}
                    catch
                        error:badarg ->
                            %% Registration failed, clean up
                            supervisor:terminate_child(?SERVER, CaseId),
                            supervisor:delete_child(?SERVER, CaseId),
                            {error, registration_failed}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ExistingPid ->
            {error, already_exists}
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [],  %% Empty for simple_one_for_one
    {ok, {SupFlags, ChildSpecs}}.
