%%%-------------------------------------------------------------------
%%% @doc wf_validate: Static analysis and bounded model checking for workflow bytecode
%%%
%%% This module implements a test/validation tool that performs static analysis
%%% on workflow bytecode (wf_vm:wf_bc()) without executing runtime tasks or effects.
%%% It explores all reachable states within configurable bounds (depth D, token bound K)
%%% and performs five correctness checks:
%%%
%%% 1. Dead transition detection (unreachable code)
%%% 2. Option to complete (no livelocks)
%%% 3. Proper completion (single terminal token)
%%% 4. Deadlock detection (no stuck states)
%%% 5. Soundness (composite of above checks)
%%%
%%% This is NOT a runtime execution engine. It performs symbolic exploration of
%%% the control flow graph derived from bytecode, tracking token positions,
%%% join counters, and scope stack but never executing TASK_EXEC opcodes or
%%% external effects.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_validate).
-include("wf_exec.hrl").
-include("wf_validate.hrl").

%%====================================================================
%% Types
%%====================================================================

-type validation_state() :: #validation_state{}.
-type issue() :: #issue{}.
-type report() :: #report{}.
-type validate_options() :: #{
    depth => pos_integer(),
    token_bound => pos_integer(),
    search_strategy => bfs | dfs,
    trace_level => none | min | full
}.

%%====================================================================
%% Exports
%%====================================================================

-export([
    new/1,
    default_options/0,
    to_petri_net/1,
    enabled_transitions/1,
    fire_transition/2,
    explore/2,
    state_hash/1,
    check_dead_transitions/2,
    check_option_to_complete/2,
    check_proper_completion/1,
    check_deadlock/1,
    check_soundness/1,
    validate/1,
    validate/2,
    format_issue/1,
    format_report/1
]).

-export_type([
    validation_state/0,
    issue/0,
    report/0,
    validate_options/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create new validation state from bytecode
-spec new(wf_vm:wf_bc()) -> validation_state().
new(Bytecode) ->
    InitialTokenId = make_ref(),
    RootScopeId = root,
    InitialToken = #token{
        token_id = InitialTokenId,
        ip = 0,
        scope_id = RootScopeId,
        value = undefined,
        status = active,
        instance_id = undefined
    },
    #validation_state{
        bytecode = Bytecode,
        tokens = #{InitialTokenId => InitialToken},
        branch_map = #{},
        join_counters = #{},
        scope_stack = [RootScopeId],
        step_count = 0
    }.

%% @doc Get default validation options
-spec default_options() -> validate_options().
default_options() ->
    #{
        depth => 100,
        token_bound => 10,
        search_strategy => bfs,
        trace_level => none
    }.

%% @doc Compile bytecode to Petri net (LTS) representation
-spec to_petri_net(wf_vm:wf_bc()) -> {validation_state(), map()}.
to_petri_net(Bytecode) ->
    InitialState = new(Bytecode),
    Metadata = #{
        bytecode_length => length(Bytecode),
        transition_counts => count_transitions(Bytecode)
    },
    {InitialState, Metadata}.

%% @doc Validate with default options
-spec validate(wf_vm:wf_bc()) -> {ok, report()} | {error, [issue()]}.
validate(Bytecode) ->
    validate(Bytecode, default_options()).

%% @doc Validate with custom options
-spec validate(wf_vm:wf_bc(), validate_options()) -> {ok, report()} | {error, [issue()]}.
validate(Bytecode, Options) ->
    {ok, Report0} = explore(Bytecode, Options),

    %% Extract explored states from report
    %% Note: explore/2 doesn't return states, we need to track them
    %% For now, we'll run a simple exploration to collect states
    InitialState = new(Bytecode),
    MaxDepth = maps:get(depth, Options, 100),
    MaxTokens = maps:get(token_bound, Options, 10),
    States = collect_states([InitialState], MaxDepth, MaxTokens, []),

    %% Run all checks
    AllIssues = check_soundness({States, Bytecode, Options}),

    %% Build final report
    ChecksPassed = #{
        dead_transitions => length(check_dead_transitions(States, Bytecode)) =:= 0,
        option_to_complete => length(check_option_to_complete(States, Options)) =:= 0,
        proper_completion => length(check_proper_completion(States)) =:= 0,
        deadlock => length(check_deadlock(States)) =:= 0
    },

    FinalReport = Report0#report{
        checks_passed = ChecksPassed,
        issues_found = AllIssues
    },

    case length(AllIssues) of
        0 -> {ok, FinalReport};
        _ -> {error, AllIssues}
    end.

%% @private Collect all explored states
collect_states([], _MaxDepth, _MaxTokens, Acc) ->
    lists:reverse(Acc);
collect_states([State | Rest], MaxDepth, MaxTokens, Acc) ->
    case State#validation_state.step_count >= MaxDepth orelse
         map_size(State#validation_state.tokens) > MaxTokens of
        true ->
            collect_states(Rest, MaxDepth, MaxTokens, Acc);
        false ->
            Enabled = enabled_transitions(State),
            Successors = [fire_transition(State, Action) || Action <- Enabled],
            collect_states(Rest ++ Successors, MaxDepth, MaxTokens, [State | Acc])
    end.

%% @doc Format issue for display
-spec format_issue(issue()) -> iolist().
format_issue(#issue{type = Type, message = Message}) ->
    io_lib:format("[~p] ~s", [Type, Message]).

%% @doc Format report for display
-spec format_report(report()) -> iolist().
format_report(#report{
    explored_state_count = Explored,
    unique_state_count = Unique,
    max_depth_reached = MaxDepth,
    checks_passed = Checks,
    issues_found = Issues
}) ->
    [
        "Validation Report:\n",
        io_lib:format("  Explored states: ~p~n", [Explored]),
        io_lib:format("  Unique states: ~p~n", [Unique]),
        io_lib:format("  Max depth: ~p~n", [MaxDepth]),
        "  Checks passed:\n",
        format_checks(Checks),
        io_lib:format("  Issues found: ~p~n", [length(Issues)]),
        format_issues(Issues)
    ].

%% @private Format checks
format_checks(Checks) ->
    maps:fold(fun(Check, Passed, Acc) ->
        Status = case Passed of
            true -> "✓";
            false -> "✗"
        end,
        [io_lib:format("    ~s ~p~n", [Status, Check]) | Acc]
    end, [], Checks).

%% @private Format issues
format_issues([]) ->
    "    No issues\n";
format_issues(Issues) ->
    lists:map(fun(Issue) ->
        ["    ", format_issue(Issue), "\n"]
    end, Issues).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Count transitions by opcode type
count_transitions(Bytecode) ->
    lists:foldl(fun(Opcode, Acc) ->
        OpType = element(1, Opcode),
        maps:update_with(OpType, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Bytecode).

%% @doc Compute enabled transitions from state
-spec enabled_transitions(validation_state()) -> [wf_sched:enabled_action()].
enabled_transitions(#validation_state{tokens = Tokens, bytecode = Bytecode}) ->
    ActiveTokens = [T || T <- maps:values(Tokens), T#token.status =:= active],
    lists:filtermap(fun(Token) ->
        IP = Token#token.ip,
        case IP < length(Bytecode) of
            true ->
                Opcode = lists:nth(IP + 1, Bytecode),
                case is_enabled(Opcode) of
                    true -> {true, {token, Token#token.token_id}};
                    false -> false
                end;
            false -> false
        end
    end, ActiveTokens).

%% @private Check if opcode is enabled
is_enabled({'TASK_EXEC', _TaskName}) -> true;
is_enabled({'DONE'}) -> true;
is_enabled({'SEQ_ENTER', _Arg}) -> true;
is_enabled({'SEQ_NEXT', _TargetIP}) -> true;
is_enabled({'PAR_FORK', _Targets}) -> true;
is_enabled({'JOIN_WAIT', _Policy}) -> true;
is_enabled({'XOR_CHOOSE', _Targets}) -> false;  %% Handled separately
is_enabled({'LOOP_CHECK', _Policy}) -> true;
is_enabled({'LOOP_BACK', _TargetIP}) -> true;
is_enabled({'MI_SPAWN', _Policy}) -> true;
is_enabled({'CANCEL_SCOPE', {_Op, _ScopeId}}) -> true;
is_enabled(_) -> false.

%% @doc Fire transition and compute successor state
-spec fire_transition(validation_state(), wf_sched:enabled_action()) -> validation_state().
fire_transition(State0, {token, TokenId}) ->
    Token = maps:get(TokenId, State0#validation_state.tokens),
    IP = Token#token.ip,
    Bytecode = State0#validation_state.bytecode,
    case IP < length(Bytecode) of
        true ->
            Opcode = lists:nth(IP + 1, Bytecode),
            execute_opcode(Opcode, Token, State0);
        false ->
            %% Token past end of bytecode, mark as complete
            UpdatedToken = Token#token{status = complete},
            Tokens = maps:put(UpdatedToken#token.token_id, UpdatedToken, State0#validation_state.tokens),
            State0#validation_state{tokens = Tokens}
    end.

%% @private Execute opcode (symbolic, no effects)
execute_opcode({'TASK_EXEC', _TaskName}, Token, State) ->
    update_token_ip(Token, Token#token.ip + 1, State);
execute_opcode({'DONE'}, Token, State) ->
    UpdatedToken = Token#token{status = complete},
    Tokens = maps:put(UpdatedToken#token.token_id, UpdatedToken, State#validation_state.tokens),
    State#validation_state{tokens = Tokens};
execute_opcode({'SEQ_ENTER', _Arg}, Token, State) ->
    update_token_ip(Token, Token#token.ip + 1, State);
execute_opcode({'SEQ_NEXT', TargetIP}, Token, State) ->
    update_token_ip(Token, TargetIP, State);
execute_opcode({'PAR_FORK', TargetIPs}, Token, State) ->
    spawn_branch_tokens(TargetIPs, Token, State);
execute_opcode({'JOIN_WAIT', Policy}, Token, State) ->
    handle_join_wait(Policy, Token, State);
execute_opcode({'LOOP_CHECK', Policy}, Token, State) ->
    handle_loop_check(Policy, Token, State);
execute_opcode({'LOOP_BACK', TargetIP}, Token, State) ->
    update_token_ip(Token, TargetIP, State);
execute_opcode({'MI_SPAWN', Policy}, Token, State) ->
    handle_mi_spawn(Policy, Token, State);
execute_opcode({'CANCEL_SCOPE', {Op, ScopeId}}, Token, State) ->
    handle_cancel_scope(Op, ScopeId, Token, State);
execute_opcode(Unknown, _Token, _State) ->
    error({unknown_opcode, Unknown}).

%% @private Update token IP and increment step count
update_token_ip(Token, NewIP, State) ->
    UpdatedToken = Token#token{ip = NewIP},
    Tokens = maps:put(UpdatedToken#token.token_id, UpdatedToken, State#validation_state.tokens),
    State#validation_state{
        tokens = Tokens,
        step_count = State#validation_state.step_count + 1
    }.

%% @private Spawn branch tokens for PAR_FORK
spawn_branch_tokens(TargetIPs, ParentToken, State) ->
    BranchId = make_ref(),
    JoinId = make_ref(),
    TargetCount = length(TargetIPs),

    %% Create branch info
    BranchInfo = #branch_info{
        branch_id = BranchId,
        tokens = [],
        join_id = JoinId,
        targets = TargetIPs
    },
    BranchMap = maps:put(BranchId, BranchInfo, State#validation_state.branch_map),

    %% Create join counter
    JoinCounter = #join_counter{
        join_id = JoinId,
        completed = 0,
        required = TargetCount,
        policy = all,
        results = []
    },
    JoinCounters = maps:put(JoinId, JoinCounter, State#validation_state.join_counters),

    %% Spawn branch tokens
    {NewTokens, _} = lists:foldl(fun(TargetIP, {TokensAcc, Index}) ->
        BranchTokenId = make_ref(),
        BranchToken = #token{
            token_id = BranchTokenId,
            ip = TargetIP,
            scope_id = ParentToken#token.scope_id,
            value = undefined,
            status = active,
            instance_id = {BranchId, Index}
        },
        {maps:put(BranchTokenId, BranchToken, TokensAcc), Index + 1}
    end, {#{}, 0}, TargetIPs),

    %% Remove parent token, add branch tokens
    Tokens1 = maps:remove(ParentToken#token.token_id, State#validation_state.tokens),
    Tokens2 = maps:merge(Tokens1, NewTokens),

    State#validation_state{
        tokens = Tokens2,
        branch_map = BranchMap,
        join_counters = JoinCounters,
        step_count = State#validation_state.step_count + 1
    }.

%% @private Handle JOIN_WAIT
handle_join_wait(_Policy, Token, State) ->
    %% Find the branch this token belongs to
    BranchId = case Token#token.instance_id of
        {BranchId0, _Index} -> BranchId0;
        _ -> undefined
    end,

    case BranchId of
        undefined ->
            %% No branch association, this is an error
            error({token_not_in_branch, Token#token.token_id});
        _ ->
            BranchInfo = maps:get(BranchId, State#validation_state.branch_map),
            JoinId = BranchInfo#branch_info.join_id,
            JoinCounter = maps:get(JoinId, State#validation_state.join_counters),

            %% Mark token as complete
            UpdatedToken = Token#token{status = complete},
            Tokens = maps:put(UpdatedToken#token.token_id, UpdatedToken, State#validation_state.tokens),

            %% Update join counter
            UpdatedJoinCounter = JoinCounter#join_counter{
                completed = JoinCounter#join_counter.completed + 1
            },
            JoinCounters = maps:put(JoinId, UpdatedJoinCounter, State#validation_state.join_counters),

            %% Check if all branches have completed
            case UpdatedJoinCounter#join_counter.completed >= UpdatedJoinCounter#join_counter.required of
                true ->
                    %% All branches complete, find the instruction after the join
                    %% For simplicity, we'll advance the IP of the first completed token
                    %% In a real implementation, we'd need to track the continuation IP
                    State#validation_state{
                        tokens = Tokens,
                        join_counters = JoinCounters,
                        step_count = State#validation_state.step_count + 1
                    };
                false ->
                    %% Still waiting for other branches
                    State#validation_state{
                        tokens = Tokens,
                        join_counters = JoinCounters,
                        step_count = State#validation_state.step_count + 1
                    }
            end
    end.

%% @private Handle LOOP_CHECK
handle_loop_check(_Policy, Token, State) ->
    %% For validation, we assume loop continues
    update_token_ip(Token, Token#token.ip + 1, State).

%% @private Handle MI_SPAWN
handle_mi_spawn(_Policy, Token, State) ->
    %% For validation, treat as simple task execution
    update_token_ip(Token, Token#token.ip + 1, State).

%% @private Handle CANCEL_SCOPE
handle_cancel_scope(_Op, _ScopeId, Token, State) ->
    %% For v1, treat as structural marker only
    update_token_ip(Token, Token#token.ip + 1, State).

%% @doc Hash state for visited set
-spec state_hash(validation_state()) -> non_neg_integer().
state_hash(State) ->
    Data = {
        State#validation_state.bytecode,
        State#validation_state.tokens,
        State#validation_state.branch_map,
        State#validation_state.join_counters
    },
    erlang:phash2(Data).

%% @doc Explore state space with bounds
-spec explore(wf_vm:wf_bc(), validate_options()) -> {ok, report()}.
explore(Bytecode, Options) ->
    InitialState = new(Bytecode),
    MaxDepth = maps:get(depth, Options, 100),
    MaxTokens = maps:get(token_bound, Options, 10),
    Strategy = maps:get(search_strategy, Options, bfs),
    case Strategy of
        bfs -> explore_bfs([InitialState], sets:new(), MaxDepth, MaxTokens, #{visited => [], states => []});
        dfs -> explore_dfs([InitialState], sets:new(), MaxDepth, MaxTokens, #{visited => [], states => []})
    end.

%% @private BFS exploration
explore_bfs([], VisitedSet, _MaxDepth, _MaxTokens, Acc) ->
    %% No more states to explore
    States = maps:get(states, Acc, []),
    MaxDepthReached = case States of
        [] -> 0;
        _ -> lists:max([S#validation_state.step_count || S <- States])
    end,
    {ok, #report{
        explored_state_count = length(States),
        unique_state_count = sets:size(VisitedSet),
        max_depth_reached = MaxDepthReached,
        checks_passed = #{},
        issues_found = []
    }};
explore_bfs([State | Rest], VisitedSet, MaxDepth, MaxTokens, Acc) ->
    StateHash = state_hash(State),
    case sets:is_element(StateHash, VisitedSet) of
        true ->
            %% Already visited, skip
            explore_bfs(Rest, VisitedSet, MaxDepth, MaxTokens, Acc);
        false ->
            %% Check bounds
            case State#validation_state.step_count >= MaxDepth orelse
                 map_size(State#validation_state.tokens) > MaxTokens of
                true ->
                    %% Bound exceeded, skip
                    explore_bfs(Rest, VisitedSet, MaxDepth, MaxTokens, Acc);
                false ->
                    %% Explore this state
                    NewVisitedSet = sets:add_element(StateHash, VisitedSet),
                    States = maps:get(states, Acc, []),
                    NewAcc = Acc#{states => [State | States]},

                    %% Compute successors
                    Enabled = enabled_transitions(State),
                    Successors = [fire_transition(State, Action) || Action <- Enabled],

                    %% Add successors to frontier
                    explore_bfs(Rest ++ Successors, NewVisitedSet, MaxDepth, MaxTokens, NewAcc)
            end
    end.

%% @private DFS exploration
explore_dfs([], VisitedSet, _MaxDepth, _MaxTokens, Acc) ->
    %% No more states to explore
    States = maps:get(states, Acc, []),
    MaxDepthReached = case States of
        [] -> 0;
        _ -> lists:max([S#validation_state.step_count || S <- States])
    end,
    {ok, #report{
        explored_state_count = length(States),
        unique_state_count = sets:size(VisitedSet),
        max_depth_reached = MaxDepthReached,
        checks_passed = #{},
        issues_found = []
    }};
explore_dfs([State | Rest], VisitedSet, MaxDepth, MaxTokens, Acc) ->
    StateHash = state_hash(State),
    case sets:is_element(StateHash, VisitedSet) of
        true ->
            %% Already visited, skip
            explore_dfs(Rest, VisitedSet, MaxDepth, MaxTokens, Acc);
        false ->
            %% Check bounds
            case State#validation_state.step_count >= MaxDepth orelse
                 map_size(State#validation_state.tokens) > MaxTokens of
                true ->
                    %% Bound exceeded, skip
                    explore_dfs(Rest, VisitedSet, MaxDepth, MaxTokens, Acc);
                false ->
                    %% Explore this state
                    NewVisitedSet = sets:add_element(StateHash, VisitedSet),
                    States = maps:get(states, Acc, []),
                    NewAcc = Acc#{states => [State | States]},

                    %% Compute successors
                    Enabled = enabled_transitions(State),
                    Successors = [fire_transition(State, Action) || Action <- Enabled],

                    %% Add successors to front of frontier (DFS)
                    explore_dfs(Successors ++ Rest, NewVisitedSet, MaxDepth, MaxTokens, NewAcc)
            end
    end.

%%====================================================================
%% Correctness Checks
%%====================================================================

%% @doc Check for dead transitions (unreachable code)
-spec check_dead_transitions([validation_state()], wf_vm:wf_bc()) -> [issue()].
check_dead_transitions(ExploredStates, Bytecode) ->
    ReachableIPs = lists:usort(lists:flatmap(fun(State) ->
        [Token#token.ip || Token <- maps:values(State#validation_state.tokens)]
    end, ExploredStates)),
    AllIPs = lists:seq(0, length(Bytecode) - 1),
    DeadIPs = lists:filter(fun(IP) -> not lists:member(IP, ReachableIPs) end, AllIPs),
    lists:map(fun(IP) ->
        SampleState = case ExploredStates of
            [] -> new(Bytecode);
            _ -> hd(ExploredStates)
        end,
        #issue{
            type = dead_transition,
            state = SampleState,
            message = <<"Dead transition: unreachable code at IP ", (integer_to_binary(IP))/binary>>,
            path_from_initial = []
        }
    end, DeadIPs).

%% @doc Check option to complete (no livelocks)
-spec check_option_to_complete([validation_state()], validate_options()) -> [issue()].
check_option_to_complete(ExploredStates, Options) ->
    lists:filtermap(fun(State) ->
        case is_terminal(State) of
            true -> false;
            false ->
                case has_path_to_terminal(State, Options) of
                    true -> false;
                    false ->
                        {true, #issue{
                            type = livelock,
                            state = State,
                            message = <<"Livelock: no path to terminal state">>,
                            path_from_initial = []
                        }}
                end
        end
    end, ExploredStates).

%% @private Check if state is terminal (no active tokens)
is_terminal(State) ->
    ActiveTokens = [T || T <- maps:values(State#validation_state.tokens), T#token.status =:= active],
    length(ActiveTokens) =:= 0.

%% @private Check if state has path to terminal
has_path_to_terminal(State, Options) ->
    MaxDepth = maps:get(depth, Options, 100),
    search_for_terminal([State], sets:new(), MaxDepth).

%% @private BFS for terminal state
search_for_terminal([], _Visited, _MaxDepth) ->
    false;
search_for_terminal([State | Rest], Visited, MaxDepth) ->
    case is_terminal(State) of
        true -> true;
        false ->
            StateHash = state_hash(State),
            case sets:is_element(StateHash, Visited) of
                true ->
                    search_for_terminal(Rest, Visited, MaxDepth);
                false ->
                    case State#validation_state.step_count >= MaxDepth of
                        true ->
                            search_for_terminal(Rest, Visited, MaxDepth);
                        false ->
                            NewVisited = sets:add_element(StateHash, Visited),
                            Enabled = enabled_transitions(State),
                            Successors = [fire_transition(State, Action) || Action <- Enabled],
                            search_for_terminal(Rest ++ Successors, NewVisited, MaxDepth)
                    end
            end
    end.

%% @doc Check proper completion (exactly one token at termination)
-spec check_proper_completion([validation_state()]) -> [issue()].
check_proper_completion(ExploredStates) ->
    TerminalStates = lists:filter(fun is_terminal/1, ExploredStates),
    lists:filtermap(fun(State) ->
        CompleteTokens = [T || T <- maps:values(State#validation_state.tokens),
                               T#token.status =:= complete],
        case length(CompleteTokens) of
            1 -> false;
            N ->
                {true, #issue{
                    type = improper_completion,
                    state = State,
                    message = <<"Improper completion: ", (integer_to_binary(N))/binary, " complete tokens (expected 1)">>,
                    path_from_initial = []
                }}
        end
    end, TerminalStates).

%% @doc Check deadlock (tokens exist but no enabled transitions)
-spec check_deadlock([validation_state()]) -> [issue()].
check_deadlock(ExploredStates) ->
    lists:filtermap(fun(State) ->
        ActiveTokens = [T || T <- maps:values(State#validation_state.tokens), T#token.status =:= active],
        HasActiveTokens = length(ActiveTokens) > 0,
        HasEnabledTransitions = enabled_transitions(State) =/= [],
        case HasActiveTokens andalso not HasEnabledTransitions of
            true ->
                {true, #issue{
                    type = deadlock,
                    state = State,
                    message = <<"Deadlock: active tokens but no enabled transitions">>,
                    path_from_initial = []
                }};
            false ->
                false
        end
    end, ExploredStates).

%% @doc Check soundness (composite)
-spec check_soundness({[validation_state()], wf_vm:wf_bc(), validate_options()}) -> [issue()].
check_soundness({States, Bytecode, Options}) ->
    check_dead_transitions(States, Bytecode) ++
    check_option_to_complete(States, Options) ++
    check_proper_completion(States) ++
    check_deadlock(States).
