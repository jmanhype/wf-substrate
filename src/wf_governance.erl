-module(wf_governance).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    set_allowlist/2,
    get_allowlist/1,
    set_budget/2,
    get_budget/1,
    set_timeout_policy/2,
    get_timeout_policy/1,
    require_approval/3,
    get_approval_config/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("wf_governance.hrl").

-define(SERVER, ?MODULE).
-define(POLICIES_TABLE, wf_governance_policies).

-record(state, {
    policies_table :: ets:tid()
}).

-record(policy, {
    scope_id :: scope_id(),
    allowlist :: [effect_type()] | undefined,
    budget :: [budget_limit()] | undefined,
    timeout :: timeout_policy() | undefined,
    approval :: map() | undefined
}).

%% We'll store policies as {ScopeId, PolicyRecord} tuples in ETS
%% so ScopeId is the key

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Start governance policy server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop governance policy server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Set effect allowlist for scope
-spec set_allowlist(scope_id(), [effect_type()]) -> ok.
set_allowlist(ScopeId, EffectTypes) ->
    gen_server:call(?SERVER, {set_allowlist, ScopeId, EffectTypes}).

%% @doc Get allowlist for scope
-spec get_allowlist(scope_id()) -> {ok, [effect_type()]} | {error, not_found}.
get_allowlist(ScopeId) ->
    gen_server:call(?SERVER, {get_allowlist, ScopeId}).

%% @doc Set budget limits for case
-spec set_budget(case_id(), [budget_limit()]) -> ok.
set_budget(CaseId, Limits) ->
    gen_server:call(?SERVER, {set_budget, CaseId, Limits}).

%% @doc Get budget for case
-spec get_budget(case_id()) -> {ok, [budget_limit()]} | {error, not_found}.
get_budget(CaseId) ->
    gen_server:call(?SERVER, {get_budget, CaseId}).

%% @doc Set timeout policy for case or scope
-spec set_timeout_policy(case_id() | scope_id(), timeout_policy()) -> ok.
set_timeout_policy(Id, Policy) ->
    gen_server:call(?SERVER, {set_timeout_policy, Id, Policy}).

%% @doc Get timeout policy
-spec get_timeout_policy(case_id() | scope_id()) ->
    {ok, timeout_policy()} | {error, not_found}.
get_timeout_policy(Id) ->
    gen_server:call(?SERVER, {get_timeout_policy, Id}).

%% @doc Require approval for task in scope
-spec require_approval(scope_id(), task_id(), approval_spec()) -> ok.
require_approval(ScopeId, TaskId, ApprovalSpec) ->
    gen_server:call(?SERVER, {require_approval, ScopeId, TaskId, ApprovalSpec}).

%% @doc Get approval configuration for task
-spec get_approval_config(task_id()) -> {ok, approval_spec()} | {error, not_found}.
get_approval_config(TaskId) ->
    gen_server:call(?SERVER, {get_approval_config, TaskId}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    %% Create ETS table for policies
    Table = ets:new(?POLICIES_TABLE, [
        named_table,
        set,
        public,
        {read_concurrency, true}
    ]),
    {ok, #state{policies_table = Table}}.

handle_call({set_allowlist, ScopeId, EffectTypes}, _From, State) ->
    Policy = case ets:lookup(?POLICIES_TABLE, ScopeId) of
        [{ScopeId, #policy{} = P}] -> P;
        [] -> #policy{scope_id = ScopeId}
    end,
    UpdatedPolicy = Policy#policy{allowlist = EffectTypes},
    ets:insert(?POLICIES_TABLE, {ScopeId, UpdatedPolicy}),
    {reply, ok, State};

handle_call({get_allowlist, ScopeId}, _From, State) ->
    case ets:lookup(?POLICIES_TABLE, ScopeId) of
        [{ScopeId, #policy{allowlist = AllowList}}] when AllowList =/= undefined ->
            {reply, {ok, AllowList}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_budget, CaseId, Limits}, _From, State) ->
    Policy = case ets:lookup(?POLICIES_TABLE, CaseId) of
        [{CaseId, #policy{} = P}] -> P;
        [] -> #policy{scope_id = CaseId}
    end,
    UpdatedPolicy = Policy#policy{budget = Limits},
    ets:insert(?POLICIES_TABLE, {CaseId, UpdatedPolicy}),
    {reply, ok, State};

handle_call({get_budget, CaseId}, _From, State) ->
    case ets:lookup(?POLICIES_TABLE, CaseId) of
        [{CaseId, #policy{budget = Budget}}] when Budget =/= undefined ->
            {reply, {ok, Budget}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_timeout_policy, Id, Policy}, _From, State) ->
    PolicyRec = case ets:lookup(?POLICIES_TABLE, Id) of
        [{Id, #policy{} = P}] -> P;
        [] -> #policy{scope_id = Id}
    end,
    UpdatedPolicy = PolicyRec#policy{timeout = Policy},
    ets:insert(?POLICIES_TABLE, {Id, UpdatedPolicy}),

    %% Schedule timeout timer
    case Policy of
        {case_timeout, Ms} ->
            erlang:send_after(Ms, self(), {case_timeout, Id});
        {task_timeout, Ms} ->
            erlang:send_after(Ms, self(), {task_timeout, Id});
        {effect_timeout, Ms} ->
            erlang:send_after(Ms, self(), {effect_timeout, Id})
    end,

    {reply, ok, State};

handle_call({get_timeout_policy, Id}, _From, State) ->
    case ets:lookup(?POLICIES_TABLE, Id) of
        [{Id, #policy{timeout = Timeout}}] when Timeout =/= undefined ->
            {reply, {ok, Timeout}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({require_approval, ScopeId, TaskId, ApprovalSpec}, _From, State) ->
    Policy = case ets:lookup(?POLICIES_TABLE, ScopeId) of
        [{ScopeId, #policy{} = P}] -> P;
        [] -> #policy{scope_id = ScopeId}
    end,
    %% Store approval config indexed by task_id
    ApprovalMap = Policy#policy.approval,
    UpdatedMap = case ApprovalMap of
        undefined -> #{TaskId => ApprovalSpec};
        Map -> Map#{TaskId => ApprovalSpec}
    end,
    UpdatedPolicy = Policy#policy{approval = UpdatedMap},
    ets:insert(?POLICIES_TABLE, {ScopeId, UpdatedPolicy}),
    {reply, ok, State};

handle_call({get_approval_config, TaskId}, _From, State) ->
    %% Scan all policies to find task approval config
    case ets:foldl(fun({_ScopeId, #policy{approval = undefined}}, Acc) -> Acc;
                       ({_ScopeId, #policy{approval = ApprovalMap}}, Acc) ->
                           case ApprovalMap of
                               #{TaskId := Spec} -> {found, Spec};
                               _ -> Acc
                           end;
                       (_, Acc) -> Acc
                    end, not_found, ?POLICIES_TABLE) of
        {found, Spec} -> {reply, {ok, Spec}, State};
        not_found -> {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({case_timeout, CaseId}, State) ->
    %% Produce timeout event
    Error = #governance_error{
        error_type = timeout,
        scope = CaseId,
        task_id = undefined,
        detail = <<"Case timeout exceeded">>,
        timestamp = erlang:timestamp()
    },
    %% TODO: Trigger cancellation via wf_cancel when item 008 completes
    io:format("Case timeout error: ~p~n", [Error]),
    {noreply, State};

handle_info({task_timeout, TaskId}, State) ->
    Error = #governance_error{
        error_type = timeout,
        scope = TaskId,
        task_id = TaskId,
        detail = <<"Task timeout exceeded">>,
        timestamp = erlang:timestamp()
    },
    io:format("Task timeout error: ~p~n", [Error]),
    {noreply, State};

handle_info({effect_timeout, EffectId}, State) ->
    Error = #governance_error{
        error_type = timeout,
        scope = EffectId,
        task_id = undefined,
        detail = <<"Effect timeout exceeded">>,
        timestamp = erlang:timestamp()
    },
    io:format("Effect timeout error: ~p~n", [Error]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{policies_table = Table}) ->
    ets:delete(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
