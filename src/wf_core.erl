%%%-------------------------------------------------------------------
%%% @doc Workflow Pattern Core - Derived Patterns
%%%
%%% This module provides derived/composite workflow patterns built from
%%% the kernel primitives in wf_term. Each derived pattern is a function
%%% that returns a wf_term().
%%%
%%% Derived patterns are syntactic sugar/combinations of kernel patterns
%%% that provide convenient abstractions for common control-flow structures.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_core).

%% Export derived patterns
-export([
    simple_merge/2,
    synchronizing_merge/2,
    discriminator/2,
    n_out_of_m/3
]).

%%%===================================================================
%%% Derived Patterns
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Simple merge: XOR choice converging to single continuation.
%%
%% Takes a list of alternative branches and a continuation term.
%% Composes each alternative with the continuation using seq/2,
%% then wraps in an x_or/1 for exclusive choice.
%%
%% This is the standard "XOR-split followed by XOR-join" pattern
%% where exactly one alternative executes, then flow continues.
%%
%% Example:
%%   simple_merge([task(a, ...), task(b, ...)], task(c, ...))
%%   => x_or([seq(task(a, ...), task(c, ...)),
%%           seq(task(b, ...), task(c, ...))])
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_merge([wf_term:wf_term()], wf_term:wf_term()) -> wf_term:wf_term().
simple_merge(Alternatives, Continuation) when is_list(Alternatives) ->
    case length(Alternatives) >= 2 of
        true -> wf_term:x_or([wf_term:seq(Alt, Continuation) || Alt <- Alternatives]);
        false -> error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc Synchronizing merge: Parallel branches converging with full sync.
%%
%% Takes a list of parallel branches and a continuation term.
%% Composes each branch with the continuation using seq/2,
%% wraps in par/1 for parallel execution.
%%
%% This is the standard "AND-split followed by AND-join" pattern
%% where all branches execute in parallel, then wait for all to complete
%% before continuing. Each branch includes the continuation, so when
%% all branches complete, the continuation is the natural next step.
%%
%% Example:
%%   synchronizing_merge([task(a, ...), task(b, ...)], task(c, ...))
%%   => par([seq(task(a, ...), task(c, ...)),
%%           seq(task(b, ...), task(c, ...))])
%%
%% Note: The continuation is embedded in each branch. Since par requires
%% all branches to complete before proceeding, this naturally synchronizes.
%%
%% @end
%%--------------------------------------------------------------------
-spec synchronizing_merge([wf_term:wf_term()], wf_term:wf_term()) -> wf_term:wf_term().
synchronizing_merge(Branches, Continuation) when is_list(Branches) ->
    case length(Branches) >= 2 of
        true ->
            %% Embed continuation in each branch, execute in parallel
            BranchesWithCont = [wf_term:seq(Branch, Continuation) || Branch <- Branches],
            wf_term:par(BranchesWithCont);
        false -> error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc Discriminator: First-complete join that cancels remaining.
%%
%% Takes a list of parallel branches and a continuation term.
%% Wraps each branch in a unique cancel scope, executes in parallel,
%% and uses a {first_n, 1} join policy to proceed when ANY branch completes.
%%
%% When the first branch completes, all other branches are cancelled.
%% This is the "discriminator" pattern from workflow patterns literature.
%%
%% Example:
%%   discriminator([task(a, ...), task(b, ...), task(c, ...)], task(d, ...))
%%   => Creates 3 cancel scopes, executes in parallel,
%%      proceeds with first completion, cancels the rest
%%
%% Note: Cancel scopes are auto-generated using make_ref() to ensure
%% uniqueness and avoid scope collisions.
%%
%% @end
%%--------------------------------------------------------------------
-spec discriminator([wf_term:wf_term()], wf_term:wf_term()) -> wf_term:wf_term().
discriminator(Branches, Continuation) when is_list(Branches) ->
    case length(Branches) >= 2 of
        true ->
            %% Create unique cancel scope for each branch
            ScopedBranches = lists:map(
                fun(Branch) ->
                    ScopeId = make_ref(),
                    wf_term:cancel(ScopeId, wf_term:seq(Branch, Continuation))
                end,
                Branches
            ),
            %% Execute in parallel, proceed on first completion
            wf_term:join({first_n, 1}, ScopedBranches);
        false -> error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc N-out-of-M join: Wait for N out of M branches.
%%
%% Takes N (number of branches to wait for), a list of M branches,
%% and a continuation term. Uses join with {n_of_m, N, M} policy
%% to wait for N branches, then all branches execute the continuation.
%%
%% This is the "N-out-of-M join" pattern where execution proceeds when
%% N of the M branches complete. The continuation is added after the join.
%%
%% Constraints: 1 <= N <= M
%%
%% Example:
%%   n_out_of_m(2, [task(a, ...), task(b, ...), task(c, ...)], task(d, ...))
%%   => seq(join({n_of_m, 2, 3}, [task(a, ...), task(b, ...), task(c, ...)]),
%%          task(d, ...))
%%
%% @end
%%--------------------------------------------------------------------
-spec n_out_of_m(pos_integer(), [wf_term:wf_term()], wf_term:wf_term()) ->
    wf_term:wf_term().
n_out_of_m(N, Branches, Continuation)
    when is_integer(N), N > 0, is_list(Branches), is_tuple(Continuation) ->
    M = length(Branches),
    case N =< M of
        true ->
            %% Execute in parallel, wait for N out of M, then continue
            wf_term:seq(wf_term:join({n_of_m, N, M}, Branches), Continuation);
        false -> error(badarg)
    end;
n_out_of_m(_N, _Branches, _Continuation) ->
    error(badarg).
