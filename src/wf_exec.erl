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
execute_opcode({ParFork, Targets}, ExecState) when ParFork =:= 'PAR_FORK'; ParFork =:= par_fork ->
    execute_par_fork({ParFork, Targets}, ExecState);
execute_opcode({JoinWait, _Policy}, ExecState) when JoinWait =:= 'JOIN_WAIT'; JoinWait =:= join_wait ->
    execute_join_wait({JoinWait, _Policy}, ExecState);
execute_opcode({XorChoose, Targets}, ExecState) when XorChoose =:= 'XOR_CHOOSE'; XorChoose =:= xor_choose ->
    execute_xor_choose({XorChoose, Targets}, ExecState, undefined);
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
            status = active
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

            %% Create continuation token with merged results
            ContinuationTokenId = make_ref(),
            ContinuationToken = #token{
                token_id = ContinuationTokenId,
                ip = ExecState#exec_state.ip + 1,
                scope_id = get_current_scope(ExecState),
                value = merge_results(JoinCounter#join_counter.results, all),
                status = active
            },
            Tokens = maps:put(ContinuationTokenId, ContinuationToken, ExecState#exec_state.tokens),

            ExecState#exec_state{
                ip = ExecState#exec_state.ip + 1,
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
    %% For simplicity, return first join_id
    %% In production, should be exactly one join
    [JoinId | _] = maps:keys(JoinCounters),
    JoinId.

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
%% Single-Token Opcodes (continued)
%%====================================================================

%% @doc Execute DONE: mark token complete, check if executor done
execute_done(ExecState) ->
    CurrentToken = ExecState#exec_state.current_token,
    Token = maps:get(CurrentToken, ExecState#exec_state.tokens),
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(CurrentToken, UpdatedToken, ExecState#exec_state.tokens),

    %% Increment join counter if part of a parallel branch
    {UpdatedTokens2, UpdatedJoinCounters} = case find_branch_for_token(CurrentToken, ExecState#exec_state.branch_map) of
        {ok, BranchId} ->
            %% Token is part of a parallel branch, increment join counter
            NewJoinCounters = increment_join_counter(BranchId, ExecState, UpdatedToken#token.value),
            {Tokens, NewJoinCounters};
        error ->
            %% Not part of a parallel branch
            {Tokens, ExecState#exec_state.join_counters}
    end,

    %% Check if all tokens complete
    ActiveTokens = [T || T <- maps:values(UpdatedTokens2), T#token.status =:= active],
    case ActiveTokens of
        [] ->
            %% No active tokens, executor is done
            ExecState#exec_state{
                tokens = UpdatedTokens2,
                join_counters = UpdatedJoinCounters,
                status = done,
                step_count = ExecState#exec_state.step_count + 1
            };
        _ ->
            %% Other tokens still active, select next token
            NextToken = select_next_token(UpdatedTokens2),
            ExecState#exec_state{
                tokens = UpdatedTokens2,
                join_counters = UpdatedJoinCounters,
                current_token = NextToken,
                step_count = ExecState#exec_state.step_count + 1
            }
    end.

%% @doc Find branch for a given token
-spec find_branch_for_token(term(), #{term() => #branch_info{}}) -> {ok, term()} | error.
find_branch_for_token(TokenId, BranchMap) ->
    case [BranchId || {BranchId, #branch_info{tokens = Tokens}} <- maps:to_list(BranchMap),
                      lists:member(TokenId, Tokens)] of
        [BranchId | _] -> {ok, BranchId};
        [] -> error
    end.

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

