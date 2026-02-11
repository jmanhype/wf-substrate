-module(wf_test_helpers).
-include("../src/wf_exec.hrl").
-include("../src/wf_trace.hrl").
-export([
    exec_until_done/1,
    exec_steps/2,
    get_token_statuses/1,
    count_tokens_with_status/2,
    assert_join_counter/2
]).

%%====================================================================
%% Execution Helpers
%%====================================================================

%% @doc Execute until done (max 1000 steps)
-spec exec_until_done(wf_exec:exec_state()) -> wf_exec:exec_state().
exec_until_done(ExecState) ->
    case wf_exec:run(ExecState, 1000, undefined) of
        {done, DoneState} -> DoneState;
        {yield, YieldState} -> exec_until_done(YieldState);
        {error, ErrorState} -> ErrorState
    end.

%% @doc Execute N steps
-spec exec_steps(wf_exec:exec_state(), pos_integer()) -> wf_exec:exec_state().
exec_steps(ExecState, 0) -> ExecState;
exec_steps(ExecState, N) ->
    {NewState, _} = wf_exec:step(ExecState, undefined),
    exec_steps(NewState, N - 1).

%%====================================================================
%% Token Status Helpers
%%====================================================================

%% @doc Get all token statuses
-spec get_token_statuses(wf_exec:exec_state()) -> [atom()].
get_token_statuses(ExecState) ->
    Tokens = ExecState#exec_state.tokens,
    [Token#token.status || {_Id, Token} <- maps:to_list(Tokens)].

%% @doc Count tokens with specific status
-spec count_tokens_with_status(wf_exec:exec_state(), atom()) -> non_neg_integer().
count_tokens_with_status(ExecState, Status) ->
    lists:count(fun(S) -> S =:= Status end, get_token_statuses(ExecState)).

%%====================================================================
%% Join Counter Helpers
%%====================================================================

%% @doc Assert join counter value
-spec assert_join_counter(wf_exec:exec_state(), {non_neg_integer(), non_neg_integer()}) -> boolean().
assert_join_counter(ExecState, {ExpectedCompleted, ExpectedRequired}) ->
    JoinCounters = ExecState#exec_state.join_counters,
    case maps:size(JoinCounters) of
        0 -> false;
        _ ->
            [{_JoinId, Counter}] = maps:to_list(JoinCounters),
            Counter#join_counter.completed =:= ExpectedCompleted andalso
            Counter#join_counter.required =:= ExpectedRequired
    end.
