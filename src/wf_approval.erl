-module(wf_approval).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    request_approval/2,
    signal/2,
    check_approval/1,
    cancel_approval/1
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
-define(APPROVALS_TABLE, wf_approvals).

-record(state, {
    approvals_table :: ets:tid(),
    waiting_map :: map()  %% approval_id -> {CallerPid, MonitorRef}
}).

-record(approval_state, {
    approval_id :: approval_id(),
    task_id :: task_id(),
    status :: pending | approved | rejected | timeout,
    detail :: binary(),
    requested_at :: erlang:timestamp(),
    timeout :: non_neg_integer() | undefined,
    timeout_action :: approval_action(),
    timer_ref :: reference() | undefined
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

%% @doc Request approval for task (blocks until approval)
-spec request_approval(task_id(), approval_spec()) ->
    {ok, approved} | {error, #governance_error{}}.
request_approval(TaskId, ApprovalSpec) ->
    ApprovalId = maps:get(approval_id, ApprovalSpec, make_ref()),
    case gen_server:call(?SERVER, {request_approval, TaskId, ApprovalSpec, self()}, infinity) of
        waiting ->
            %% Block until approval received
            receive
                {approval_granted, ApprovalId} ->
                    {ok, approved};
                {approval_rejected, ApprovalId, Reason} ->
                    Error = #governance_error{
                        error_type = approval_required,
                        scope = TaskId,
                        task_id = TaskId,
                        detail = list_to_binary(io_lib:format("Approval rejected: ~p", [Reason])),
                        timestamp = erlang:timestamp()
                    },
                    {error, Error};
                {approval_timeout, ApprovalId} ->
                    Error = #governance_error{
                        error_type = timeout,
                        scope = TaskId,
                        task_id = TaskId,
                        detail = <<"Approval timeout">>,
                        timestamp = erlang:timestamp()
                    },
                    {error, Error}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Signal approval (approve or reject)
-spec signal(approval_id(), approve | reject) -> ok | {error, term()}.
signal(ApprovalId, Decision) ->
    gen_server:call(?SERVER, {signal, ApprovalId, Decision}).

%% @doc Check approval status
-spec check_approval(approval_id()) ->
    {ok, pending | approved | rejected | timeout} | {error, not_found}.
check_approval(ApprovalId) ->
    gen_server:call(?SERVER, {check_approval, ApprovalId}).

%% @doc Cancel pending approval
-spec cancel_approval(approval_id()) -> ok.
cancel_approval(ApprovalId) ->
    gen_server:cast(?SERVER, {cancel_approval, ApprovalId}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    Table = ets:new(?APPROVALS_TABLE, [
        named_table,
        set,
        public,
        {read_concurrency, true}
    ]),
    {ok, #state{approvals_table = Table, waiting_map = #{}}}.

handle_call({request_approval, TaskId, ApprovalSpec, CallerPid}, _From, State) ->
    ApprovalId = maps:get(approval_id, ApprovalSpec, make_ref()),
    Timeout = maps:get(timeout, ApprovalSpec, undefined),
    TimeoutAction = maps:get(timeout_action, ApprovalSpec, cancel),
    Detail = maps:get(detail, ApprovalSpec, <<>>),

    %% Monitor caller process
    MonitorRef = erlang:monitor(process, CallerPid),

    ApprovalState = #approval_state{
        approval_id = ApprovalId,
        task_id = TaskId,
        status = pending,
        detail = Detail,
        requested_at = erlang:timestamp(),
        timeout = Timeout,
        timeout_action = TimeoutAction,
        timer_ref = case Timeout of
            undefined -> undefined;
            Ms -> erlang:send_after(Ms, self(), {approval_timeout, ApprovalId})
        end
    },
    ets:insert(?APPROVALS_TABLE, {ApprovalId, ApprovalState}),

    %% Store waiting process mapping
    UpdatedMap = maps:put(ApprovalId, {CallerPid, MonitorRef}, State#state.waiting_map),

    {reply, waiting, State#state{waiting_map = UpdatedMap}};

handle_call({signal, ApprovalId, Decision}, _From, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [{ApprovalId, #approval_state{status = pending, timer_ref = TimerRef} = Approval}] ->
            %% Cancel timeout timer
            case TimerRef of
                undefined -> ok;
                _ -> erlang:cancel_timer(TimerRef)
            end,

            %% Get waiting process
            case maps:get(ApprovalId, State#state.waiting_map, undefined) of
                {CallerPid, _MonitorRef} when Decision =:= approve ->
                    UpdatedApproval = Approval#approval_state{status = approved},
                    ets:insert(?APPROVALS_TABLE, {ApprovalId, UpdatedApproval}),
                    %% Notify waiting process
                    CallerPid ! {approval_granted, ApprovalId},
                    {reply, ok, State};
                {CallerPid, _MonitorRef} when Decision =:= reject ->
                    UpdatedApproval = Approval#approval_state{status = rejected},
                    ets:insert(?APPROVALS_TABLE, {ApprovalId, UpdatedApproval}),
                    CallerPid ! {approval_rejected, ApprovalId, rejected_by_signaler},
                    {reply, ok, State};
                _ ->
                    {reply, {error, no_waiting_process}, State}
            end;
        [{ApprovalId, #approval_state{status = Status}}] when Status =/= pending ->
            {reply, {error, {already_processed, Status}}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({check_approval, ApprovalId}, _From, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [{ApprovalId, #approval_state{status = Status}}] ->
            {reply, {ok, Status}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({cancel_approval, ApprovalId}, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [{ApprovalId, #approval_state{status = pending, timer_ref = TimerRef}}] ->
            case TimerRef of
                undefined -> ok;
                _ -> erlang:cancel_timer(TimerRef)
            end,
            ets:delete(?APPROVALS_TABLE, ApprovalId),
            %% Remove from waiting map
            UpdatedMap = maps:remove(ApprovalId, State#state.waiting_map),
            {noreply, State#state{waiting_map = UpdatedMap}};
        _ ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({approval_timeout, ApprovalId}, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [{ApprovalId, #approval_state{status = pending, timeout_action = Action} = Approval}] ->
            UpdatedApproval = Approval#approval_state{status = timeout},
            ets:insert(?APPROVALS_TABLE, {ApprovalId, UpdatedApproval}),

            %% Get waiting process
            case maps:get(ApprovalId, State#state.waiting_map, undefined) of
                {CallerPid, _MonitorRef} ->
                    %% Execute timeout action
                    case Action of
                        cancel ->
                            %% Notify waiting process with timeout error
                            CallerPid ! {approval_timeout, ApprovalId};
                        default ->
                            %% Auto-approve with default value
                            CallerPid ! {approval_granted, ApprovalId};
                        escalate ->
                            %% Notify escalation handler (log for now)
                            io:format("Approval timeout escalated: ~p~n", [ApprovalId]),
                            CallerPid ! {approval_timeout, ApprovalId}
                    end;
                undefined ->
                    ok
            end,
            {noreply, State};
        _ ->
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    %% Handle monitored process death - clean up approvals
    %% Find and remove approvals for this process
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{approvals_table = Table}) ->
    ets:delete(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
