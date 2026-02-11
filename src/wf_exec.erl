-module(wf_exec).

%% Bytecode executor for workflow patterns
%% Hot loop with cooperative scheduling

%%====================================================================
%% API
%%====================================================================

-export([
    new/1,
    step/2,
    run/3,
    is_done/1,
    is_blocked/1,
    get_ip/1,
    get_ctx/1,
    get_step_count/1
]).

%%====================================================================
%% Records
%%====================================================================

%% Token: Logical thread of execution
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled
}).

%% Branch info: Parallel branch tracking
-record(branch_info, {
    branch_id :: term(),
    tokens :: [term()],
    join_id :: term(),
    targets :: [non_neg_integer()]
}).

%% Join counter: Join synchronization state
-record(join_counter, {
    join_id :: term(),
    completed :: non_neg_integer(),
    required :: non_neg_integer(),
    policy :: wf_vm:join_policy(),
    results :: [term()]
}).

%% Execution state record
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: map(),
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | cancelled,
    current_token :: term() | undefined
}).

%%====================================================================
%% Types
%%====================================================================

-type exec_state() :: #exec_state{}.
-type ctx() :: map().

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new executor from bytecode
-spec new(wf_vm:wf_bc()) -> exec_state().
new(Bytecode) ->
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active
    },
    #exec_state{
        ip = 0,
        bytecode = Bytecode,
        ctx = #{},
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0,
        status = running,
        current_token = InitialTokenId
    }.

%% @doc Get current instruction pointer
-spec get_ip(exec_state()) -> non_neg_integer().
get_ip(#exec_state{ip = IP}) ->
    IP.

%% @doc Get current context
-spec get_ctx(exec_state()) -> ctx().
get_ctx(#exec_state{ctx = Ctx}) ->
    Ctx.

%% @doc Get step count
-spec get_step_count(exec_state()) -> non_neg_integer().
get_step_count(#exec_state{step_count = Count}) ->
    Count.

%% @doc Check if executor is in terminal state
-spec is_done(exec_state()) -> boolean().
is_done(#exec_state{status = Status}) ->
    Status =:= done orelse Status =:= cancelled orelse Status =:= failed.

%% @doc Check if executor is blocked (waiting for external event)
-spec is_blocked(exec_state()) -> boolean().
is_blocked(#exec_state{status = Status}) ->
    Status =:= blocked_effect orelse
    Status =:= blocked_join orelse
    Status =:= blocked_signal.

%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), map()}.
step(ExecState, _SchedDecision) ->
    Opcode = fetch_opcode(ExecState),
    NewExecState = execute_opcode(Opcode, ExecState),
    TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
    {NewExecState, TraceEvent}.

%% @doc Execute N quanta, yield when exhausted or terminal
-spec run(exec_state(), pos_integer(), term()) ->
    {done, exec_state()} | {yield, exec_state()}.
run(ExecState0, Quanta, _SchedPolicy) ->
    run_loop(ExecState0, Quanta, 0).

run_loop(ExecState, Quanta, Count) when Count >= Quanta ->
    %% Quanta exhausted, yield
    {yield, ExecState};

run_loop(ExecState, _Quanta, _Count) when ExecState#exec_state.status =:= done ->
    %% Terminal state
    {done, ExecState};

run_loop(ExecState0, Quanta, Count) ->
    %% Execute one step (no scheduler integration yet)
    {ExecState1, _TraceEvent} = step(ExecState0, undefined),
    run_loop(ExecState1, Quanta, Count + 1).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Fetch opcode at current IP
-spec fetch_opcode(exec_state()) -> wf_vm:opcode().
fetch_opcode(#exec_state{ip = IP, bytecode = Bytecode}) ->
    case IP < length(Bytecode) of
        true ->
            lists:nth(IP + 1, Bytecode);  %% IP is 0-indexed
        false ->
            {'DONE'}
    end.

%%====================================================================
%% Opcode Dispatch
%%====================================================================

%% @doc Execute opcode and update exec_state
-spec execute_opcode(wf_vm:opcode(), exec_state()) -> exec_state().
execute_opcode({SeqEnter, _Arg}, ExecState) when SeqEnter =:= 'SEQ_ENTER'; SeqEnter =:= seq_enter ->
    execute_seq_enter({SeqEnter, _Arg}, ExecState);
execute_opcode({SeqNext, TargetIP}, ExecState) when SeqNext =:= 'SEQ_NEXT'; SeqNext =:= seq_next ->
    execute_seq_next({SeqNext, TargetIP}, ExecState);
execute_opcode({TaskExec, _TaskName}, ExecState) when TaskExec =:= 'TASK_EXEC'; TaskExec =:= task_exec ->
    execute_task_exec({TaskExec, _TaskName}, ExecState);
execute_opcode({Done}, ExecState) when Done =:= 'DONE'; Done =:= done ->
    execute_done(ExecState);
execute_opcode(_Opcode, _ExecState) ->
    error({unimplemented_opcode, _Opcode}).

%%====================================================================
%% Opcode Handlers
%%====================================================================

%% @doc Execute SEQ_ENTER: push scope, advance IP
execute_seq_enter({_SeqEnter, _Arg}, ExecState) ->
    NewScopeId = make_ref(),
    NewScopeStack = [NewScopeId | ExecState#exec_state.scope_stack],
    ExecState#exec_state{
        ip = ExecState#exec_state.ip + 1,
        scope_stack = NewScopeStack,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Execute SEQ_NEXT: jump to right branch
execute_seq_next({_SeqNext, TargetIP}, ExecState) ->
    ExecState#exec_state{
        ip = TargetIP,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Execute TASK_EXEC: run task function (mock for now)
execute_task_exec({_TaskExec, _TaskName}, ExecState) ->
    %% Mock: task always succeeds, updates context
    NewCtx = maps:put(task_result, ok, ExecState#exec_state.ctx),
    ExecState#exec_state{
        ctx = NewCtx,
        ip = ExecState#exec_state.ip + 1,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Execute DONE: mark token complete, check if executor done
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Check if all tokens complete
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],
    case ActiveTokens of
        [] ->
            %% No active tokens, executor is done
            ExecState#exec_state{
                tokens = Tokens,
                status = done,
                step_count = ExecState#exec_state.step_count + 1
            };
        _ ->
            %% Other tokens active (shouldn't happen in single-token executor)
            ExecState#exec_state{
                tokens = Tokens,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

