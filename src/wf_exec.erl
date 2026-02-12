-module(wf_exec).

%% Bytecode executor for workflow patterns
%% Hot loop with cooperative scheduling
%%
%% Cancellation Integration:
%% - is_scope_cancelled/2 checks if scope is cancelled (stub for now)
%% - propagate_cancellation/2 delegates to wf_cancel:propagate/2
%% - TODO: Integrate with wf_state:state() when executor refactored
%%

%% Include record definitions
-include("wf_mi.hrl").
-include("wf_exec.hrl").
-include("wf_effect.hrl").
-include("wf_receipt.hrl").

%%====================================================================
%% API
%%====================================================================

-export([
    new/1,
    step/2,
    run/3,
    resume/2,
    is_done/1,
    is_blocked/1,
    get_ip/1,
    get_ctx/1,
    get_step_count/1,
    set_ctx/2,
    get_scope_stack_depth/1,
    snapshot_exec_state/1,
    restore_exec_state/2,
    find_branch_for_token/2
]).

%%====================================================================
%% Types
%%====================================================================

-type exec_state() :: #exec_state{}.
-type ctx() :: map().

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new executor from bytecode with metadata
%% Accepts both new format {Bytecode, Metadata} and legacy format [opcode()]
-spec new(wf_vm:wf_bc() | [wf_vm:opcode()]) -> exec_state().
new({Bytecode, Metadata}) when is_list(Bytecode), is_map(Metadata) ->
    %% New format: tuple with metadata
    CaseId = make_ref(),  %% Generate case_id for effect IDs
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active,
        instance_id = undefined,
        current_effect = undefined
    },
    #exec_state{
        ip = 0,
        bytecode = Bytecode,
        ctx = #{},
        case_id = CaseId,
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0,
        status = running,
        current_token = InitialTokenId,
        task_metadata = Metadata,  %% Store task metadata for lookup
        loop_counters = #{}  %% Initialize empty loop counters map
    };
new(Bytecode) when is_list(Bytecode) ->
    %% Legacy format: plain list of opcodes (backward compatibility)
    %% Wrap with empty metadata map for old test code
    new({Bytecode, #{}}).

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

%% @doc Set context (for testing)
-spec set_ctx(exec_state(), map()) -> exec_state().
set_ctx(ExecState, Ctx) ->
    ExecState#exec_state{ctx = Ctx}.

%% @doc Get scope stack depth (for testing)
-spec get_scope_stack_depth(exec_state()) -> non_neg_integer().
get_scope_stack_depth(#exec_state{scope_stack = ScopeStack}) ->
    length(ScopeStack).

%% @doc Snapshot execution state for tracing
%% Returns a binary serialization of the exec_state for storage in trace events
-spec snapshot_exec_state(exec_state()) -> binary().
snapshot_exec_state(ExecState) ->
    StateMap = #{
        ip => ExecState#exec_state.ip,
        bytecode => ExecState#exec_state.bytecode,
        ctx => ExecState#exec_state.ctx,
        case_id => ExecState#exec_state.case_id,
        tokens => ExecState#exec_state.tokens,
        branch_map => ExecState#exec_state.branch_map,
        join_counters => ExecState#exec_state.join_counters,
        scope_stack => ExecState#exec_state.scope_stack,
        step_count => ExecState#exec_state.step_count,
        status => ExecState#exec_state.status,
        current_token => ExecState#exec_state.current_token,
        task_metadata => ExecState#exec_state.task_metadata,
        loop_counters => ExecState#exec_state.loop_counters
    },
    term_to_binary(StateMap).

%% @doc Find branch_id for a token in branch_map
%% Searches through all branches to find which one contains the token
-spec find_branch_for_token(term(), #{term() => #branch_info{}}) -> {ok, term()} | error.
find_branch_for_token(TokenId, BranchMap) ->
    SearchFun = fun(_BranchId, BranchInfo) ->
        lists:member(TokenId, BranchInfo#branch_info.tokens)
    end,
    case maps:filter(SearchFun, BranchMap) of
        Map when map_size(Map) > 0 ->
            [{BranchId, _BranchInfo}] = maps:to_list(Map),
            {ok, BranchId};
        _ ->
            error
    end.

%% @doc Restore execution state from snapshot binary
%% Validates that bytecode in snapshot matches provided bytecode
%% Returns {ok, ExecState} on success, {error, Reason} on failure
-spec restore_exec_state(binary(), wf_vm:wf_bc()) -> {ok, exec_state()} | {error, term()}.
restore_exec_state(Binary, Bytecode) ->
    try
        StateMap = binary_to_term(Binary),

        %% Validate required fields
        case maps:is_key(ip, StateMap) andalso
             maps:is_key(bytecode, StateMap) andalso
             maps:is_key(ctx, StateMap) andalso
             maps:is_key(case_id, StateMap) andalso
             maps:is_key(tokens, StateMap) andalso
             maps:is_key(branch_map, StateMap) andalso
             maps:is_key(join_counters, StateMap) andalso
             maps:is_key(scope_stack, StateMap) andalso
             maps:is_key(step_count, StateMap) andalso
             maps:is_key(status, StateMap) andalso
             maps:is_key(current_token, StateMap) andalso
             maps:is_key(task_metadata, StateMap) andalso
             maps:is_key(loop_counters, StateMap) of
            false ->
                {error, invalid_snapshot};
            true ->
                %% Validate bytecode matches
                StoredBytecode = maps:get(bytecode, StateMap),
                case StoredBytecode =:= Bytecode of
                    false ->
                        {error, {bytecode_mismatch, {expected, Bytecode, got, StoredBytecode}}};
                    true ->
                        %% Bytecode matches, restore exec_state
                        ExecState = #exec_state{
                            ip = maps:get(ip, StateMap),
                            bytecode = maps:get(bytecode, StateMap),
                            ctx = maps:get(ctx, StateMap),
                            case_id = maps:get(case_id, StateMap),
                            tokens = maps:get(tokens, StateMap),
                            branch_map = maps:get(branch_map, StateMap),
                            join_counters = maps:get(join_counters, StateMap),
                            scope_stack = maps:get(scope_stack, StateMap),
                            step_count = maps:get(step_count, StateMap),
                            status = maps:get(status, StateMap),
                            current_token = maps:get(current_token, StateMap),
                            task_metadata = maps:get(task_metadata, StateMap),
                            loop_counters = maps:get(loop_counters, StateMap)
                        },
                        {ok, ExecState}
                end
        end
    catch
        error:_ ->
            {error, invalid_snapshot}
    end.

%% @doc Check if executor is in terminal state
-spec is_done(exec_state()) -> boolean().
is_done(#exec_state{status = Status}) ->
    Status =:= done orelse Status =:= cancelled orelse Status =:= failed.

%% @doc Check if executor is blocked (waiting for external event)
-spec is_blocked(exec_state()) -> boolean().
is_blocked(#exec_state{status = Status}) ->
    Status =:= blocked_effect orelse
    Status =:= blocked_join orelse
    Status =:= blocked_signal orelse
    Status =:= blocked_approval.

%% @doc Execute single reduction step
-spec step(exec_state(), term()) -> {exec_state(), map()}.
step(ExecState, SchedDecision) ->
    case ExecState#exec_state.status of
        blocked_effect ->
            step_check_effect(ExecState, SchedDecision);
        blocked_join ->
            step_check_join(ExecState, SchedDecision);
        _ ->
            step_normal(ExecState, SchedDecision)
    end.

%% @private Check effect completion (blocked_effect path)
step_check_effect(ExecState, _SchedDecision) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    EffectId = Token#token.current_effect,

    case wf_effect:get_result(EffectId) of
        {ok, Result} ->
            %% Effect completed, resume task
            NewCtx = maps:put(effect_result, Result, ExecState#exec_state.ctx),
            UpdatedToken = Token#token{
                status = active,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_complete,
                effect_id => EffectId,
                result => Result
            },
            {ExecState#exec_state{tokens = Tokens, status = running, ctx = NewCtx}, TraceEvent};
        {cancelled, Reason} ->
            %% Effect cancelled, propagate cancellation
            UpdatedToken = Token#token{
                status = cancelled,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_cancelled,
                effect_id => EffectId,
                reason => Reason
            },
            {ExecState#exec_state{tokens = Tokens, status = cancelled}, TraceEvent};
        {error, Reason} ->
            %% Effect failed
            UpdatedToken = Token#token{
                status = failed,
                current_effect = undefined
            },
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            TraceEvent = #{
                type => effect_failed,
                effect_id => EffectId,
                reason => Reason
            },
            {ExecState#exec_state{tokens = Tokens, status = failed}, TraceEvent};
        pending ->
            %% Effect still pending, yield
            TraceEvent = #{type => blocked_effect, effect_id => EffectId},
            {ExecState, TraceEvent}
    end.

%% @private Check join completion (blocked_join path)
step_check_join(ExecState, _SchedDecision) ->
    case maps:keys(ExecState#exec_state.join_counters) of
        [] ->
            %% No join counters (shouldn't happen), unblock
            {ExecState#exec_state{status = running}, #{type => join_unblocked, reason => no_joins}};
        _ ->
            %% Check if any join is satisfied
            JoinId = find_active_join(ExecState),
            JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),

            case JoinCounter#join_counter.completed >= JoinCounter#join_counter.required of
                true ->
                    %% Join satisfied, create continuation token
                    JoinCounters = maps:remove(JoinId, ExecState#exec_state.join_counters),
                    BranchMap = remove_branch_by_join(JoinId, ExecState#exec_state.branch_map),

                    ContinuationTokenId = make_ref(),
                    ContinuationToken = #token{
                        token_id = ContinuationTokenId,
                        ip = ExecState#exec_state.ip + 1,
                        scope_id = get_current_scope(ExecState),
                        value = merge_results(JoinCounter#join_counter.results, JoinCounter#join_counter.policy),
                        status = active,
                        instance_id = undefined
                    },
                    Tokens = maps:put(ContinuationTokenId, ContinuationToken, ExecState#exec_state.tokens),

                    TraceEvent = #{
                        type => join_complete,
                        join_id => JoinId,
                        results => JoinCounter#join_counter.results
                    },
                    {ExecState#exec_state{
                        ip = ExecState#exec_state.ip + 1,
                        tokens = Tokens,
                        join_counters = JoinCounters,
                        branch_map = BranchMap,
                        status = running,
                        current_token = ContinuationTokenId
                    }, TraceEvent};
                false ->
                    %% Join not satisfied, remain blocked
                    TraceEvent = #{
                        type => blocked_join,
                        join_id => JoinId,
                        completed => JoinCounter#join_counter.completed,
                        required => JoinCounter#join_counter.required
                    },
                    {ExecState, TraceEvent}
            end
    end.

%% @private Normal step (not blocked)
step_normal(ExecState, SchedDecision) when not is_tuple(SchedDecision) ->
    %% Legacy callers pass undefined or atom — resolve to active token
    ResolvedDecision = case SchedDecision of
        undefined ->
            %% Find an active token to execute (backward compat for step/2 callers)
            ActiveTokens = [T || T <- maps:values(ExecState#exec_state.tokens),
                            T#token.status =:= active],
            case ActiveTokens of
                [T | _] -> {token, T#token.token_id};
                [] -> {token, undefined}
            end;
        Other ->
            {token, Other}
    end,
    step_normal(ExecState, ResolvedDecision);
step_normal(ExecState, {token, SelectedTokenId}) ->
    case SelectedTokenId of
        undefined ->
            %% No active tokens available
            case maps:values(ExecState#exec_state.tokens) of
                [] ->
                    %% All tokens complete, mark as done
                    FinalExecState = ExecState#exec_state{status = done},
                    TraceEvent = #{type => done, reason => no_tokens},
                    {FinalExecState, TraceEvent};
                _ ->
                    %% Tokens exist but none are active (blocked state)
                    TraceEvent = #{type => blocked, reason => no_active_token},
                    {ExecState, TraceEvent}
            end;
        _TokenId ->
            %% Update current_token from scheduler decision
            ExecState1 = ExecState#exec_state{current_token = SelectedTokenId},

            %% In multi-token mode, sync IP with current token before fetching opcode
            %% In single-token mode, trust ExecState.ip (token.ip is stale)
            SyncedExecState = case maps:size(ExecState1#exec_state.tokens) > 1 of
                true -> sync_ip_with_current_token(ExecState1);
                false -> ExecState1
            end,
            Opcode = fetch_opcode(SyncedExecState),
            NewExecState0 = execute_opcode(Opcode, SyncedExecState, {token, SelectedTokenId}),
            %% Write back updated IP to current token (multi-token mode)
            %% Opcodes advance ExecState.ip but don't update the token's ip field.
            %% Without this, sync_ip_with_current_token reads stale token IP next step.
            NewExecState = case maps:size(NewExecState0#exec_state.tokens) > 1 of
                true ->
                    CurTok = NewExecState0#exec_state.current_token,
                    case maps:find(CurTok, NewExecState0#exec_state.tokens) of
                        {ok, Tok} ->
                            UpdatedTok = Tok#token{ip = NewExecState0#exec_state.ip},
                            NewToks = maps:put(CurTok, UpdatedTok, NewExecState0#exec_state.tokens),
                            NewExecState0#exec_state{tokens = NewToks};
                        error ->
                            NewExecState0
                    end;
                false ->
                    NewExecState0
            end,
            TraceEvent = #{opcode => Opcode, step_count => NewExecState#exec_state.step_count},
            {NewExecState, TraceEvent}
    end.

%% @private Sync ExecState.ip with current token's IP
%% This is necessary in multi-token execution where each token has its own IP.
-spec sync_ip_with_current_token(exec_state()) -> exec_state().
sync_ip_with_current_token(#exec_state{current_token = undefined} = ExecState) ->
    %% No current token, keep IP as-is (will be handled elsewhere)
    ExecState;
sync_ip_with_current_token(#exec_state{current_token = CurrentToken, tokens = Tokens} = ExecState) ->
    Token = maps:get(CurrentToken, Tokens),
    ExecState#exec_state{ip = Token#token.ip}.

%% @doc Execute N quanta, yield when exhausted or terminal
-spec run(exec_state(), pos_integer(), term()) ->
    {done, exec_state()} | {effect, term(), exec_state()} | {yield, exec_state()}.
run(ExecState0, Quanta, SchedPolicy) ->
    run_loop(ExecState0, Quanta, SchedPolicy, 0).

run_loop(ExecState, Quanta, _SchedPolicy, Count) when Count >= Quanta ->
    %% Quanta exhausted, yield
    {yield, ExecState};

run_loop(ExecState, _Quanta, _SchedPolicy, _Count) when ExecState#exec_state.status =:= done;
                                                           ExecState#exec_state.status =:= failed;
                                                           ExecState#exec_state.status =:= cancelled ->
    %% Terminal state
    {done, ExecState};

run_loop(ExecState0, Quanta, SchedPolicy, Count) ->
    %% Select next action via scheduler
    SchedDecision = wf_sched:select_action(ExecState0, SchedPolicy),

    %% Execute one step with scheduler decision
    {ExecState1, _TraceEvent} = step(ExecState0, SchedDecision),

    %% Check for effect yield
    case ExecState1#exec_state.status of
        blocked_effect ->
            %% Task yielded effect
            EffectSpec = {mock_effect, ExecState1#exec_state.ip},
            {effect, EffectSpec, ExecState1};
        _ ->
            %% Continue execution
            run_loop(ExecState1, Quanta, SchedPolicy, Count + 1)
    end.

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
-spec execute_opcode(wf_vm:opcode(), exec_state(), term()) -> exec_state().
execute_opcode({SeqEnter, _Arg}, ExecState, _SchedDecision) when SeqEnter =:= 'SEQ_ENTER'; SeqEnter =:= seq_enter ->
    execute_seq_enter({SeqEnter, _Arg}, ExecState);
execute_opcode({SeqNext, TargetIP}, ExecState, _SchedDecision) when SeqNext =:= 'SEQ_NEXT'; SeqNext =:= seq_next ->
    execute_seq_next({SeqNext, TargetIP}, ExecState);
execute_opcode({MiSpawn, Policy}, ExecState, _SchedDecision) when MiSpawn =:= 'MI_SPAWN'; MiSpawn =:= mi_spawn ->
    execute_mi_spawn({MiSpawn, Policy}, ExecState);
execute_opcode({ParFork, Targets}, ExecState, _SchedDecision) when ParFork =:= 'PAR_FORK'; ParFork =:= par_fork ->
    execute_par_fork({ParFork, Targets}, ExecState);
execute_opcode({JoinWait, _Policy}, ExecState, _SchedDecision) when JoinWait =:= 'JOIN_WAIT'; JoinWait =:= join_wait ->
    execute_join_wait({JoinWait, _Policy}, ExecState);
execute_opcode({XorChoose, Targets}, ExecState, SchedDecision) when XorChoose =:= 'XOR_CHOOSE'; XorChoose =:= xor_choose ->
    execute_xor_choose({XorChoose, Targets}, ExecState, SchedDecision);
execute_opcode({LoopCheck, Policy}, ExecState, _SchedDecision) when LoopCheck =:= 'LOOP_CHECK'; LoopCheck =:= loop_check ->
    execute_loop_check({LoopCheck, Policy}, ExecState);
execute_opcode({LoopBack, TargetIP}, ExecState, _SchedDecision) when LoopBack =:= 'LOOP_BACK'; LoopBack =:= loop_back ->
    execute_loop_back({LoopBack, TargetIP}, ExecState);
execute_opcode({CancelScope, ScopeOp}, ExecState, _SchedDecision) when CancelScope =:= 'CANCEL_SCOPE'; CancelScope =:= cancel_scope ->
    execute_cancel_scope({CancelScope, ScopeOp}, ExecState);
execute_opcode({TaskExec, _TaskName}, ExecState, _SchedDecision) when TaskExec =:= 'TASK_EXEC'; TaskExec =:= task_exec ->
    execute_task_exec({TaskExec, _TaskName}, ExecState);
execute_opcode({Done}, ExecState, _SchedDecision) when Done =:= 'DONE'; Done =:= done ->
    execute_done(ExecState);
execute_opcode(_Opcode, _ExecState, _SchedDecision) ->
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

%% @doc Execute TASK_EXEC: run task function, detect effect yield
execute_task_exec({_TaskExec, _TaskName}, ExecState) ->
    %% Mock: look up task function and execute it
    TaskFun = lookup_task_function(_TaskName, ExecState),
    Ctx = ExecState#exec_state.ctx,
    CaseId = ExecState#exec_state.case_id,
    StepSeq = ExecState#exec_state.step_count,
    ScopeId = get_current_scope(ExecState),

    case TaskFun(Ctx) of
        {ok, NewCtx} ->
            %% Pure task completed successfully
            ExecState#exec_state{
                ctx = NewCtx,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        {effect, EffectSpec, ContCtx} ->
            %% Task yielded effect
            {ok, EffectOrReceipt} = wf_effect:yield(CaseId, StepSeq, ScopeId, EffectSpec),

            case EffectOrReceipt of
                #receipt{result = Result} ->
                    %% Cached result from idempotency, resume immediately
                    NewCtx2 = maps:put(effect_result, Result, ContCtx),
                    ExecState#exec_state{
                        ctx = NewCtx2,
                        ip = ExecState#exec_state.ip + 1,
                        step_count = ExecState#exec_state.step_count + 1
                    };
                #effect{effect_id = EffectId} ->
                    %% New effect, block executor
                    CurrentToken = ExecState#exec_state.current_token,
                    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
                    UpdatedToken = Token#token{
                        status = blocked_effect,
                        current_effect = EffectId
                    },
                    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

                    ExecState#exec_state{
                        ctx = ContCtx,
                        tokens = Tokens,
                        status = blocked_effect,
                        ip = ExecState#exec_state.ip + 1,
                        step_count = ExecState#exec_state.step_count + 1
                    }
            end;
        {error, Reason} ->
            %% Task failed
            CurrentToken = ExecState#exec_state.current_token,
            Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
            UpdatedToken = Token#token{status = failed, value = {error, Reason}},
            Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

            ExecState#exec_state{
                tokens = Tokens,
                status = failed,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Get current scope from scope_stack
-spec get_current_scope(exec_state()) -> term().
get_current_scope(#exec_state{scope_stack = [ScopeId | _]}) ->
    ScopeId.

%% @doc Select next token to execute (deterministic: first active token)
-spec select_next_token(#{term() => #token{}}) -> term() | undefined.
select_next_token(TokensMap) ->
    ActiveTokens = [T || T <- maps:values(TokensMap), T#token.status =:= active],
    case ActiveTokens of
        [] -> undefined;
        [First | _] -> First#token.token_id
    end.

%%====================================================================
%% Multi-Token Opcodes
%%====================================================================

%% @doc Execute PAR_FORK: spawn N tokens, track in branch_map
execute_par_fork({_ParFork, TargetIPs}, ExecState) ->
    BranchId = make_ref(),
    JoinId = make_ref(),
    NumBranches = length(TargetIPs),

    %% Spawn N tokens
    CurrentToken = ExecState#exec_state.current_token,
    ScopeId = get_current_scope(ExecState),
    NewTokens = lists:map(fun(IP) ->
        TokenId = make_ref(),
        Token = #token{
            token_id = TokenId,
            ip = IP,
            scope_id = ScopeId,
            value = undefined,
            status = active,
            instance_id = undefined
        },
        {TokenId, Token}
    end, TargetIPs),

    %% Add tokens to state
    TokensMap0 = ExecState#exec_state.tokens,
    TokensMap = lists:foldl(fun({TokenId, Token}, Acc) ->
        maps:put(TokenId, Token, Acc)
    end, TokensMap0, NewTokens),

    %% Create branch info
    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = [TokenId || {TokenId, _Token} <- NewTokens],
        join_id = JoinId,
        targets = TargetIPs
    },
    BranchMap0 = ExecState#exec_state.branch_map,
    BranchMap = maps:put(BranchId, BranchInfo, BranchMap0),

    %% Create join counter
    JoinCounter = #join_counter{
        join_id = JoinId,
        completed = 0,
        required = NumBranches,
        policy = all,
        results = []
    },
    JoinCounters0 = ExecState#exec_state.join_counters,
    JoinCounters = maps:put(JoinId, JoinCounter, JoinCounters0),

    %% Mark current token as complete (replaced by branch tokens)
    TokensMapFinal = maps:remove(CurrentToken, TokensMap),

    %% Advance IP (past PAR_FORK instruction)
    NewIP = ExecState#exec_state.ip + 1,

    %% Select next token to execute
    NextToken = select_next_token(TokensMapFinal),

    ExecState#exec_state{
        ip = NewIP,
        tokens = TokensMapFinal,
        branch_map = BranchMap,
        join_counters = JoinCounters,
        step_count = ExecState#exec_state.step_count + 1,
        current_token = NextToken
    }.

%% @doc Execute MI_SPAWN: spawn N instances of body at same IP
%% Unlike PAR_FORK (spawns to different IPs), MI_SPAWN spawns N instances
%% to the SAME body IP, with per-instance state via instance_id
execute_mi_spawn({MiSpawn, {MIPolicy, MIJoinPolicy, BodyIP}}, ExecState)
        when MiSpawn =:= 'MI_SPAWN'; MiSpawn =:= mi_spawn ->
    %% Spawn instances via wf_mi
    {ok, Instances, _MIConfig} = wf_mi:spawn_instances(
        MIPolicy, MIJoinPolicy, BodyIP, get_current_scope(ExecState)
    ),

    %% Create tokens for each instance
    ScopeId = get_current_scope(ExecState),
    CurrentToken = ExecState#exec_state.current_token,
    NewTokens = lists:map(fun(Instance) ->
        Token = #token{
            token_id = Instance#mi_instance.token_id,
            ip = BodyIP,  %% All instances execute SAME body
            scope_id = ScopeId,
            value = #{instance_id => Instance#mi_instance.instance_id,
                      instance_num => Instance#mi_instance.instance_num},
            status = active,
            instance_id = Instance#mi_instance.instance_id
        },
        {Instance#mi_instance.token_id, Token}
    end, Instances),

    %% Add tokens to state
    TokensMap0 = ExecState#exec_state.tokens,
    TokensMap = lists:foldl(fun({TokenId, Token}, Acc) ->
        maps:put(TokenId, Token, Acc)
    end, TokensMap0, NewTokens),

    %% Create branch info (tracks all instances)
    BranchId = make_ref(),
    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = [TokenId || {TokenId, _Token} <- NewTokens],
        join_id = undefined,  %% Set below if sync required
        targets = [BodyIP]  %% Same IP for all instances
    },
    BranchMap0 = ExecState#exec_state.branch_map,
    BranchMap = maps:put(BranchId, BranchInfo, BranchMap0),

    %% Create join counter (if sync required)
    {JoinCounters, BranchMapFinal, NextIP} = case MIJoinPolicy of
        none ->
            %% Fire-and-forget, no join
            {ExecState#exec_state.join_counters, BranchMap, ExecState#exec_state.ip + 1};
        _SyncPolicy ->
            JoinId = make_ref(),
            NumInstances = length(Instances),
            Required = case MIJoinPolicy of
                wait_all -> NumInstances;
                {wait_n, N} -> min(N, NumInstances);  %% Guard against N > M
                first_complete -> 1
            end,
            JoinCounter = #join_counter{
                join_id = JoinId,
                completed = 0,
                required = Required,
                policy = mi_join_to_exec_join(MIJoinPolicy),
                results = []
            },
            JoinCounters0 = ExecState#exec_state.join_counters,
            JoinCounters1 = maps:put(JoinId, JoinCounter, JoinCounters0),

            %% Update branch info with join_id
            BranchMapWithJoin = maps:put(BranchId,
                BranchInfo#branch_info{join_id = JoinId}, BranchMap),

            {JoinCounters1, BranchMapWithJoin, ExecState#exec_state.ip + 1}
    end,

    %% Remove current token (replaced by instance tokens)
    TokensMapFinal = maps:remove(CurrentToken, TokensMap),

    %% Select next token to execute
    NextToken = select_next_token(TokensMapFinal),

    ExecState#exec_state{
        ip = NextIP,
        tokens = TokensMapFinal,
        branch_map = BranchMapFinal,
        join_counters = JoinCounters,
        step_count = ExecState#exec_state.step_count + 1,
        current_token = NextToken
    }.

%% @doc Convert MI join policy to wf_exec join policy
-spec mi_join_to_exec_join(wf_mi:mi_join_policy()) -> wf_vm:join_policy().
mi_join_to_exec_join(wait_all) -> all;
mi_join_to_exec_join({wait_n, N}) -> {first_n, N};
mi_join_to_exec_join(first_complete) -> first_complete;
mi_join_to_exec_join(none) -> none.

%% @doc Execute JOIN_WAIT: check join counter, block if not satisfied
execute_join_wait({_JoinWait, _Policy}, ExecState) ->
    %% Find active join (must be exactly one)
    JoinId = find_active_join(ExecState),
    JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),

    case JoinCounter#join_counter.completed >= JoinCounter#join_counter.required of
        true ->
            %% Join satisfied, merge results and continue
            %% Remove join counter and branch map entry
            JoinCounters = maps:remove(JoinId, ExecState#exec_state.join_counters),
            BranchMap = remove_branch_by_join(JoinId, ExecState#exec_state.branch_map),

            %% Merge branch contexts into global ctx
            MergedCtx = lists:foldl(fun(BranchCtx, Acc) when is_map(BranchCtx) ->
                maps:merge(Acc, BranchCtx);
            (_, Acc) -> Acc
            end, ExecState#exec_state.ctx, JoinCounter#join_counter.results),

            %% Create continuation token
            ContinuationTokenId = make_ref(),
            ContinuationToken = #token{
                token_id = ContinuationTokenId,
                ip = ExecState#exec_state.ip + 1,
                scope_id = get_current_scope(ExecState),
                value = undefined,
                status = active,
                instance_id = undefined
            },
            %% Remove completed branch tokens, add continuation
            CleanedTokens = maps:filter(fun(_TId, T) ->
                T#token.status =:= active
            end, ExecState#exec_state.tokens),
            Tokens = maps:put(ContinuationTokenId, ContinuationToken, CleanedTokens),

            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,
                ctx = MergedCtx,
                tokens = Tokens,
                join_counters = JoinCounters,
                branch_map = BranchMap,
                step_count = ExecState#exec_state.step_count + 1,
                current_token = ContinuationTokenId
            };
        false ->
            %% Join not satisfied, block
            ExecState#exec_state{
                status = blocked_join,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Find active join point
-spec find_active_join(exec_state()) -> term().
find_active_join(#exec_state{join_counters = JoinCounters}) ->
    case maps:keys(JoinCounters) of
        [] ->
            error({no_active_join, join_counters_empty});
        [JoinId | _] ->
            JoinId
    end.

%% @doc Remove branch entry by join_id
-spec remove_branch_by_join(term(), #{term() => #branch_info{}}) -> #{term() => #branch_info{}}.
remove_branch_by_join(JoinId, BranchMap) ->
    maps:filter(fun(_BranchId, BranchInfo) ->
        BranchInfo#branch_info.join_id =/= JoinId
    end, BranchMap).

%% @doc Merge results from parallel branches
-spec merge_results([term()], wf_vm:join_policy()) -> term().
merge_results(Results, all) ->
    {merged, Results};
merge_results(Results, {first_n, _N}) ->
    {merged, Results};
merge_results(Results, first_complete) ->
    {first_complete, hd(Results)};
merge_results(Results, sync_merge) ->
    {sync_merged, lists:foldl(fun merge_state/2, #{}, Results)}.

%% @doc Merge state maps
merge_state(StateMap, Acc) ->
    maps:merge(Acc, StateMap).

%% @doc Execute XOR_CHOOSE: select ONE branch via scheduler decision
execute_xor_choose({_XorChoose, TargetIPs}, ExecState, _SchedDecision) ->
    %% Scheduler decision contains selected branch index
    %% For now, always select first branch (deterministic)
    SelectedIndex = 1,
    SelectedIP = lists:nth(SelectedIndex, TargetIPs),

    %% Update current token's IP to selected branch
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{ip = SelectedIP},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    ExecState#exec_state{
        ip = SelectedIP,
        tokens = Tokens,
        step_count = ExecState#exec_state.step_count + 1
    }.

%%====================================================================
%% Loop Opcodes
%%====================================================================

%% @doc Execute LOOP_CHECK: evaluate condition, exit or continue
execute_loop_check({_LoopCheck, Policy}, ExecState) ->
    case evaluate_loop_condition(Policy, ExecState) of
        {true, NewCtx, NewExecState} ->
            %% Condition satisfied, continue to loop body
            NewExecState#exec_state{
                ip = NewExecState#exec_state.ip + 1,
                ctx = NewCtx,
                step_count = NewExecState#exec_state.step_count + 1
            };
        {false, NewCtx, NewExecState} ->
            %% Condition not satisfied, exit loop
            %% Calculate exit IP by scanning forward to find matching loop_back
            ExitIP = calculate_loop_exit(NewExecState),
            NewExecState#exec_state{
                ip = ExitIP,
                ctx = NewCtx,
                step_count = NewExecState#exec_state.step_count + 1
            }
    end.

%% @doc Execute LOOP_BACK: jump to loop head
execute_loop_back({_LoopBack, TargetIP}, ExecState) ->
    ExecState#exec_state{
        ip = TargetIP,
        step_count = ExecState#exec_state.step_count + 1
    }.

%% @doc Evaluate loop condition with per-scope counter tracking
%% Returns {bool(), ctx(), exec_state()} to pass updated counter state
-spec evaluate_loop_condition(wf_vm:loop_policy(), exec_state()) -> {boolean(), map(), exec_state()}.
evaluate_loop_condition({count, N}, ExecState) ->
    %% Use current IP as unique loop identifier (IP of loop_check instruction)
    LoopIP = ExecState#exec_state.ip,
    Counters = ExecState#exec_state.loop_counters,

    %% Initialize counter on first check (defaults to N)
    %% Subsequent iterations use stored counter
    Counter = maps:get(LoopIP, Counters, N),

    case Counter > 0 of
        true ->
            %% Decrement counter and continue to loop body
            NewCounter = Counter - 1,
            NewCounters = maps:put(LoopIP, NewCounter, Counters),
            NewExecState = ExecState#exec_state{loop_counters = NewCounters},
            {true, NewExecState#exec_state.ctx, NewExecState};
        false ->
            %% Counter exhausted, exit loop
            %% Clean up counter entry to avoid memory leaks
            NewCounters = maps:remove(LoopIP, Counters),
            NewExecState = ExecState#exec_state{loop_counters = NewCounters},
            {false, NewExecState#exec_state.ctx, NewExecState}
    end;
evaluate_loop_condition(while, ExecState) ->
    %% While loop: check condition before body
    %% Mock implementation - always continue (item 052 should implement real evaluation)
    {true, ExecState#exec_state.ctx, ExecState};
evaluate_loop_condition(until, ExecState) ->
    %% Until loop: check condition after body
    %% Mock implementation - always exit after first iteration (item 052 should implement real evaluation)
    {false, ExecState#exec_state.ctx, ExecState}.

%% @doc Calculate loop exit IP by scanning forward to find matching loop_back
%% Handles nested loops via depth counter (finds loop_back at same depth)
-spec calculate_loop_exit(exec_state()) -> non_neg_integer().
calculate_loop_exit(ExecState) ->
    %% Find next loop_back opcode at depth 0 (matching this loop_check)
    %% Start scanning from instruction after current IP (the loop body)
    Bytecode = ExecState#exec_state.bytecode,
    CurrentIP = ExecState#exec_state.ip,
    find_loop_back_exit(Bytecode, CurrentIP + 1, 0).

%% @private Scan bytecode for matching loop_back, handling nested loops
find_loop_back_exit(Bytecode, IP, Depth) ->
    case IP >= length(Bytecode) of
        true ->
            %% Reached end of bytecode without finding loop_back
            %% This shouldn't happen in well-formed bytecode
            error({missing_loop_back, IP});
        false ->
            Opcode = lists:nth(IP + 1, Bytecode),  %% IP is 0-indexed
            case Opcode of
                {loop_back, _TargetIP} when Depth =:= 0 ->
                    %% Found matching loop_back at same depth
                    %% Exit to instruction after loop_back
                    IP + 1;
                {loop_back, _TargetIP} ->
                    %% loop_back for nested loop, decrement depth and continue
                    find_loop_back_exit(Bytecode, IP + 1, Depth - 1);
                {loop_check, _Policy} ->
                    %% Entered nested loop, increment depth and continue
                    find_loop_back_exit(Bytecode, IP + 1, Depth + 1);
                _Opcode ->
                    %% Any other opcode, continue scanning
                    find_loop_back_exit(Bytecode, IP + 1, Depth)
            end
    end.

%%====================================================================
%% Cancellation Opcodes
%%====================================================================

%% @doc Execute CANCEL_SCOPE: enter or exit cancel scope
execute_cancel_scope({_CancelScope, {enter, ScopeId}}, ExecState) ->
    %% Push scope onto stack
    NewScopeStack = [ScopeId | ExecState#exec_state.scope_stack],
    ExecState#exec_state{
        scope_stack = NewScopeStack,
        ip = ExecState#exec_state.ip + 1,
        step_count = ExecState#exec_state.step_count + 1
    };

execute_cancel_scope({_CancelScope, {exit, ScopeId}}, ExecState) ->
    %% Pop scope from stack
    [_Top | Rest] = ExecState#exec_state.scope_stack,
    NewScopeStack = Rest,

    %% Check if scope is cancelled (stub for wf_cancel)
    case is_scope_cancelled(ScopeId, ExecState) of
        true ->
            %% Propagate cancellation to all tokens in scope
            Tokens = propagate_cancellation(ScopeId, ExecState#exec_state.tokens),
            ExecState#exec_state{
                scope_stack = NewScopeStack,
                tokens = Tokens,
                status = cancelled,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            };
        false ->
            %% Normal exit
            ExecState#exec_state{
                scope_stack = NewScopeStack,
                ip = ExecState#exec_state.ip + 1,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Check if scope is cancelled
-spec is_scope_cancelled(term(), exec_state()) -> boolean().
is_scope_cancelled(_ScopeId, _ExecState) ->
    %% TODO: Call wf_cancel:is_cancelled/2 when wf_exec has wf_state field
    %% Current limitation: exec_state has inline state, not wf_state:state()
    %% For now, keep stub behavior (always false)
    false.

%% @doc Propagate cancellation to all tokens in scope
-spec propagate_cancellation(term(), #{term() => #token{}}) -> #{term() => #token{}}.
propagate_cancellation(ScopeId, TokensMap) ->
    %% Delegate to wf_cancel for token cancellation
    wf_cancel:propagate(ScopeId, TokensMap).

%%====================================================================
%% Single-Token Opcodes (continued)
%%====================================================================

%% @doc Execute DONE: mark token complete, check if executor done
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Check if token is MI instance
    case Token#token.instance_id of
        undefined ->
            %% Not an MI instance, use existing branch logic
            {UpdatedTokens2, UpdatedJoinCounters} = case find_branch_for_token(CurrentToken, ExecState#exec_state.branch_map) of
                {ok, BranchId} ->
                    %% Token is part of a parallel branch, increment join counter
                    NewJoinCounters = increment_join_counter(BranchId, ExecState, ExecState#exec_state.ctx),
                    {Tokens, NewJoinCounters};
                error ->
                    %% Not part of a parallel branch
                    {Tokens, ExecState#exec_state.join_counters}
            end,

            %% Check if all tokens complete
            ActiveTokens = [T || T <- maps:values(UpdatedTokens2), T#token.status =:= active],
            case ActiveTokens of
                [] ->
                    %% No active tokens — check for satisfied join counters
                    case find_satisfied_join(UpdatedJoinCounters) of
                        {ok, JoinId, JoinCounter} ->
                            %% Join satisfied: merge branch contexts, create continuation past join_wait
                            MergedCtx = lists:foldl(fun(BranchCtx, Acc) when is_map(BranchCtx) ->
                                maps:merge(Acc, BranchCtx);
                            (_, Acc) -> Acc
                            end, ExecState#exec_state.ctx, JoinCounter#join_counter.results),

                            %% Find the join_wait opcode IP by scanning bytecode
                            JoinWaitIP = find_join_wait_ip(ExecState#exec_state.bytecode),
                            ContinuationIP = JoinWaitIP + 1,

                            %% Clean up join state
                            CleanJoinCounters = maps:remove(JoinId, UpdatedJoinCounters),
                            CleanBranchMap = remove_branch_by_join(JoinId, ExecState#exec_state.branch_map),

                            %% Create continuation token past join_wait
                            ContTokenId = make_ref(),
                            ContToken = #token{
                                token_id = ContTokenId,
                                ip = ContinuationIP,
                                scope_id = get_current_scope(ExecState),
                                value = undefined,
                                status = active,
                                instance_id = undefined
                            },
                            CleanTokens = maps:filter(fun(_TId, T) ->
                                T#token.status =:= active
                            end, UpdatedTokens2),
                            FinalTokens = maps:put(ContTokenId, ContToken, CleanTokens),

                            ExecState#exec_state{
                                ip = ContinuationIP,
                                ctx = MergedCtx,
                                tokens = FinalTokens,
                                join_counters = CleanJoinCounters,
                                branch_map = CleanBranchMap,
                                current_token = ContTokenId,
                                step_count = ExecState#exec_state.step_count + 1
                            };
                        none ->
                            %% No joins pending, executor is truly done
                            ExecState#exec_state{
                                tokens = UpdatedTokens2,
                                join_counters = UpdatedJoinCounters,
                                status = done,
                                step_count = ExecState#exec_state.step_count + 1
                            }
                    end;
                _ ->
                    %% Other tokens still active, select next token
                    NextToken = select_next_token(UpdatedTokens2),
                    NextTokenRec = maps:get(NextToken, UpdatedTokens2),
                    ExecState#exec_state{
                        tokens = UpdatedTokens2,
                        join_counters = UpdatedJoinCounters,
                        current_token = NextToken,
                        ip = NextTokenRec#token.ip,
                        step_count = ExecState#exec_state.step_count + 1
                    }
            end;
        InstanceId ->
            %% MI instance, collect result via wf_mi
            ExecStateWithTokens = ExecState#exec_state{tokens = Tokens},
            {ExecState2, JoinSatisfied} = wf_mi:collect_result(
                InstanceId, UpdatedToken#token.value, ExecStateWithTokens
            ),

            %% If join satisfied and policy is wait_n or first_complete, cancel remaining
            FinalExecState = case JoinSatisfied of
                true ->
                    %% Check join policy (from join counter)
                    JoinId = find_join_id_for_instance(InstanceId, ExecState2),
                    JoinCounter = maps:get(JoinId, ExecState2#exec_state.join_counters),
                    Policy = JoinCounter#join_counter.policy,

                    case Policy of
                        {first_n, _N} ->
                            wf_mi:cancel_remaining(InstanceId, ExecState2);
                        first_complete ->
                            wf_mi:cancel_remaining(InstanceId, ExecState2);
                        _Other ->
                            ExecState2
                    end;
                false ->
                    ExecState2
            end,

            %% Check if all tokens complete
            ActiveTokens = [T || T <- maps:values(FinalExecState#exec_state.tokens),
                                T#token.status =:= active],
            case ActiveTokens of
                [] ->
                    FinalExecState#exec_state{
                        status = done,
                        step_count = FinalExecState#exec_state.step_count + 1
                    };
                _ ->
                    NextToken = select_next_token(FinalExecState#exec_state.tokens),
                    FinalExecState#exec_state{
                        current_token = NextToken,
                        step_count = FinalExecState#exec_state.step_count + 1
                    }
            end
    end.

%% @doc Find join ID for instance (helper)
-spec find_join_id_for_instance(term(), exec_state()) -> term().
find_join_id_for_instance(InstanceId, ExecState) ->
    {ok, BranchId} = wf_mi:find_branch_for_instance(
        InstanceId, ExecState#exec_state.branch_map, ExecState#exec_state.tokens
    ),
    BranchInfo = maps:get(BranchId, ExecState#exec_state.branch_map),
    BranchInfo#branch_info.join_id.

%% @doc Increment join counter when branch completes
-spec increment_join_counter(term(), exec_state(), term()) -> #{term() => #join_counter{}}.
increment_join_counter(BranchId, ExecState, ResultValue) ->
    BranchInfo = maps:get(BranchId, ExecState#exec_state.branch_map),
    JoinId = BranchInfo#branch_info.join_id,

    %% Update join counter
    JoinCounter = maps:get(JoinId, ExecState#exec_state.join_counters),
    UpdatedJoinCounter = JoinCounter#join_counter{
        completed = JoinCounter#join_counter.completed + 1,
        results = [ResultValue | JoinCounter#join_counter.results]
    },
    maps:put(JoinId, UpdatedJoinCounter, ExecState#exec_state.join_counters).

%% @doc Find first satisfied join counter (completed >= required)
-spec find_satisfied_join(#{term() => #join_counter{}}) ->
    {ok, term(), #join_counter{}} | none.
find_satisfied_join(JoinCounters) ->
    Results = maps:fold(fun(JoinId, JC, Acc) ->
        case JC#join_counter.completed >= JC#join_counter.required of
            true -> [{JoinId, JC} | Acc];
            false -> Acc
        end
    end, [], JoinCounters),
    case Results of
        [{JoinId, JC} | _] -> {ok, JoinId, JC};
        [] -> none
    end.

%% @doc Find the IP of the join_wait opcode in bytecode
-spec find_join_wait_ip([wf_vm:opcode()]) -> non_neg_integer().
find_join_wait_ip(Bytecode) ->
    find_join_wait_ip(Bytecode, 0).

find_join_wait_ip([], _IP) ->
    error({no_join_wait_found});
find_join_wait_ip([{JW, _} | _], IP) when JW =:= join_wait; JW =:= 'JOIN_WAIT' ->
    IP;
find_join_wait_ip([_ | Rest], IP) ->
    find_join_wait_ip(Rest, IP + 1).

%% @doc Look up task function from metadata map
%% For backward compatibility with old test code that doesn't provide metadata,
%% falls back to a mock function that returns {ok, Ctx} with task_result = ok
-spec lookup_task_function(atom(), exec_state()) -> fun((map()) -> {ok, map()} | {effect, term(), map()} | {error, term()}).
lookup_task_function(TaskName, ExecState) ->
    case maps:find(TaskName, ExecState#exec_state.task_metadata) of
        {ok, Metadata} ->
            case maps:find(function, Metadata) of
                {ok, TaskFun} when is_function(TaskFun) ->
                    TaskFun;
                _ ->
                    error({badarg, {invalid_task_metadata, TaskName, "function key missing or not a fun"}})
            end;
        error ->
            %% Backward compatibility: old test code uses plain bytecode lists
            %% without metadata. Fall back to mock function.
            fun(Ctx) -> {ok, maps:put(task_result, ok, Ctx)} end
    end.

%%====================================================================
%% Effect Resume
%%====================================================================

%% @doc Resume execution after effect result
-spec resume(exec_state(), term()) -> exec_state().
resume(ExecState, EffectResult) ->
    %% Update context with effect result
    NewCtx = maps:put(effect_result, EffectResult, ExecState#exec_state.ctx),

    %% Clear blocked status, advance IP
    ExecState#exec_state{
        ctx = NewCtx,
        status = running,
        ip = ExecState#exec_state.ip + 1
    }.

