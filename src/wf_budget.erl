-module(wf_budget).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    init_budget/2,
    check_budget/1,
    increment_effect_count/1,
    add_effect_time/2,
    add_effect_cost/2,
    get_budget_state/1
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
-define(BUDGETS_TABLE, wf_budgets).

-record(state, {
    budgets_table :: ets:tid()
}).

-record(budget_state, {
    case_id :: case_id(),
    effect_count :: non_neg_integer(),
    effect_time_us :: non_neg_integer(),
    total_cost :: non_neg_integer(),
    limits :: [budget_limit()]
}).

%%%====================================================================
%%% API
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Initialize budget tracking for case
-spec init_budget(case_id(), [budget_limit()]) -> ok.
init_budget(CaseId, Limits) ->
    gen_server:call(?SERVER, {init_budget, CaseId, Limits}).

%% @doc Check if budget allows another effect
-spec check_budget(case_id()) -> ok | {error, #governance_error{}}.
check_budget(CaseId) ->
    gen_server:call(?SERVER, {check_budget, CaseId}).

%% @doc Increment effect count after effect execution
-spec increment_effect_count(case_id()) -> ok.
increment_effect_count(CaseId) ->
    gen_server:cast(?SERVER, {increment_effect_count, CaseId}).

%% @doc Add effect execution time to budget
-spec add_effect_time(case_id(), non_neg_integer()) -> ok.
add_effect_time(CaseId, TimeUs) ->
    gen_server:cast(?SERVER, {add_effect_time, CaseId, TimeUs}).

%% @doc Add effect cost to budget
-spec add_effect_cost(case_id(), integer()) -> ok.
add_effect_cost(CaseId, Cost) ->
    gen_server:cast(?SERVER, {add_effect_cost, CaseId, Cost}).

%% @doc Get current budget state
-spec get_budget_state(case_id()) -> {ok, #budget_state{}} | {error, not_found}.
get_budget_state(CaseId) ->
    gen_server:call(?SERVER, {get_budget_state, CaseId}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    Table = ets:new(?BUDGETS_TABLE, [
        named_table,
        set,
        public,
        {read_concurrency, true}
    ]),
    {ok, #state{budgets_table = Table}}.

handle_call({init_budget, CaseId, Limits}, _From, State) ->
    BudgetState = #budget_state{
        case_id = CaseId,
        effect_count = 0,
        effect_time_us = 0,
        total_cost = 0,
        limits = Limits
    },
    ets:insert(?BUDGETS_TABLE, {CaseId, BudgetState}),
    {reply, ok, State};

handle_call({check_budget, CaseId}, _From, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [{CaseId, #budget_state{limits = Limits, effect_count = Count,
                     effect_time_us = TimeUs, total_cost = Cost}}] ->
            case check_limits(Limits, Count, TimeUs, Cost) of
                ok ->
                    {reply, ok, State};
                {error, LimitType, CurrentValue, Limit} ->
                    Error = #governance_error{
                        error_type = budget_exceeded,
                        scope = CaseId,
                        task_id = undefined,
                        detail = list_to_binary(
                            io_lib:format("~p limit exceeded: ~p/~p",
                                [LimitType, CurrentValue, Limit])),
                        timestamp = erlang:timestamp()
                    },
                    {reply, {error, Error}, State}
            end;
        [] ->
            %% No budget configured, allow
            {reply, ok, State}
    end;

handle_call({get_budget_state, CaseId}, _From, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [{CaseId, BudgetState}] ->
            {reply, {ok, BudgetState}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({increment_effect_count, CaseId}, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [{CaseId, #budget_state{effect_count = Count} = Budget}] ->
            UpdatedBudget = Budget#budget_state{effect_count = Count + 1},
            ets:insert(?BUDGETS_TABLE, {CaseId, UpdatedBudget});
        [] -> ok
    end,
    {noreply, State};

handle_cast({add_effect_time, CaseId, TimeUs}, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [{CaseId, #budget_state{effect_time_us = CurrentTime} = Budget}] ->
            UpdatedBudget = Budget#budget_state{effect_time_us = CurrentTime + TimeUs},
            ets:insert(?BUDGETS_TABLE, {CaseId, UpdatedBudget});
        [] -> ok
    end,
    {noreply, State};

handle_cast({add_effect_cost, CaseId, Cost}, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [{CaseId, #budget_state{total_cost = CurrentCost} = Budget}] ->
            UpdatedBudget = Budget#budget_state{total_cost = CurrentCost + Cost},
            ets:insert(?BUDGETS_TABLE, {CaseId, UpdatedBudget});
        [] -> ok
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{budgets_table = Table}) ->
    ets:delete(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

-spec check_limits([budget_limit()], non_neg_integer(), non_neg_integer(), integer()) ->
    ok | {error, term(), term(), term()}.
check_limits([], _Count, _TimeUs, _Cost) ->
    ok;
check_limits([{max_effects, Max} | _Rest], Count, _TimeUs, _Cost) when Count >= Max ->
    {error, max_effects, Count, Max};
check_limits([{max_time_us, Max} | _Rest], _Count, TimeUs, _Cost) when TimeUs >= Max ->
    {error, max_time_us, TimeUs, Max};
check_limits([{max_cost, Max} | _Rest], _Count, _TimeUs, Cost) when Cost >= Max ->
    {error, max_cost, Cost, Max};
check_limits([_Limit | Rest], Count, TimeUs, Cost) ->
    check_limits(Rest, Count, TimeUs, Cost).
