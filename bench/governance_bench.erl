-module(governance_bench).

%% Benchmarks for governance overhead

-export([
    bench_budget_check/1,
    bench_allowlist_check/1,
    bench_approval_request/1,
    run_all_benchmarks/0
]).

%%%====================================================================
%%% Benchmark runners
%%%====================================================================

%% @doc Benchmark budget check overhead
%% Usage: governance_bench:bench_budget_check(10000).
bench_budget_check(Iterations) ->
    %% Setup
    {ok, _} = wf_governance:start_link(),
    {ok, _} = wf_budget:start_link(),
    CaseId = bench_case,
    wf_budget:init_budget(CaseId, [{max_effects, 1000000}, {max_time_us, 1000000000}, {max_cost, 100000}]),

    %% Warmup
    lists:foreach(fun(_) -> wf_budget:check_budget(CaseId) end, lists:seq(1, 100)),

    %% Benchmark
    {Time, _} = timer:tc(fun() ->
        [wf_budget:check_budget(CaseId) || _ <- lists:seq(1, Iterations)]
    end),

    %% Cleanup
    wf_budget:stop(),
    wf_governance:stop(),

    %% Results
    AvgUs = Time / Iterations,
    io:format("~p budget checks: ~p microseconds total, ~p microseconds average~n",
        [Iterations, Time, AvgUs]),
    ok.

%% @doc Benchmark allowlist check overhead
%% Usage: governance_bench:bench_allowlist_check(10000).
bench_allowlist_check(Iterations) ->
    %% Setup
    {ok, _} = wf_governance:start_link(),
    ScopeId = bench_scope,
    wf_governance:set_allowlist(ScopeId, [http_get, http_post, file_read, file_write, db_query]),

    %% Warmup
    lists:foreach(fun(_) -> wf_governance:get_allowlist(ScopeId) end, lists:seq(1, 100)),

    %% Benchmark
    {Time, _} = timer:tc(fun() ->
        [wf_governance:get_allowlist(ScopeId) || _ <- lists:seq(1, Iterations)]
    end),

    %% Cleanup
    wf_governance:stop(),

    %% Results
    AvgUs = Time / Iterations,
    io:format("~p allowlist checks: ~p microseconds total, ~p microseconds average~n",
        [Iterations, Time, AvgUs]),
    ok.

%% @doc Benchmark approval request/signaling latency
%% Usage: governance_bench:bench_approval_request(100).
bench_approval_request(Iterations) ->
    %% Setup
    {ok, _} = wf_approval:start_link(),

    %% Benchmark
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            ApprovalSpec = #{
                approval_id => N,
                timeout => 5000,
                timeout_action => cancel,
                detail => <<"Benchmark approval">>
            },
            %% Request approval in separate process
            Pid = spawn_link(fun() ->
                wf_approval:request_approval(task_bench, ApprovalSpec)
            end),
            %% Wait for registration
            timer:sleep(1),
            %% Signal approval
            wf_approval:signal(N, approve),
            %% Wait for completion
            timer:sleep(1)
        end, lists:seq(1, Iterations))
    end),

    %% Cleanup
    wf_approval:stop(),

    %% Results
    AvgUs = Time / Iterations,
    io:format("~p approval cycles: ~p microseconds total, ~p microseconds average~n",
        [Iterations, Time, AvgUs]),
    ok.

%% @doc Run all benchmarks
run_all_benchmarks() ->
    io:format("=== Governance Benchmarks ===~n"),
    io:format("~n"),

    io:format("Budget Check Benchmark:~n"),
    bench_budget_check(10000),
    io:format("~n"),

    io:format("Allowlist Check Benchmark:~n"),
    bench_allowlist_check(10000),
    io:format("~n"),

    io:format("Approval Request Benchmark:~n"),
    bench_approval_request(100),
    io:format("~n"),

    io:format("=== Benchmarks Complete ===~n"),
    ok.

%%%====================================================================
%%% ETS table performance benchmarks
%%%====================================================================

%% @doc Benchmark ETS table write performance
bench_ets_write(Iterations) ->
    Table = ets:new(bench_table, [set, public]),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            ets:insert(Table, {N, value})
        end, lists:seq(1, Iterations))
    end),

    ets:delete(Table),

    AvgUs = Time / Iterations,
    io:format("~p ETS writes: ~p microseconds total, ~p microseconds average~n",
        [Iterations, Time, AvgUs]),
    ok.

%% @doc Benchmark ETS table read performance
bench_ets_read(Iterations) ->
    Table = ets:new(bench_table, [set, public]),
    %% Pre-populate table
    lists:foreach(fun(N) ->
        ets:insert(Table, {N, value})
    end, lists:seq(1, Iterations)),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            ets:lookup(Table, N)
        end, lists:seq(1, Iterations))
    end),

    ets:delete(Table),

    AvgUs = Time / Iterations,
    io:format("~p ETS reads: ~p microseconds total, ~p microseconds average~n",
        [Iterations, Time, AvgUs]),
    ok.

%%%====================================================================
%%% Combined benchmark
%%%====================================================================

%% @doc Benchmark complete governance check flow (allowlist + budget)
bench_governance_check(Iterations) ->
    %% Setup
    {ok, _} = wf_governance:start_link(),
    {ok, _} = wf_budget:start_link(),
    ScopeId = bench_scope,
    CaseId = bench_case,
    wf_governance:set_allowlist(ScopeId, [http_get, http_post, file_read]),
    wf_budget:init_budget(CaseId, [{max_effects, 1000000}]),

    %% Create effect spec
    Spec = wf_effect_stub:new_spec(CaseId, 0, ScopeId, http_get, #{}),

    %% Warmup
    lists:foreach(fun(_) ->
        wf_effect_stub:yield(CaseId, 0, ScopeId, Spec)
    end, lists:seq(1, 100)),

    %% Benchmark
    {Time, Results} = timer:tc(fun() ->
        [wf_effect_stub:yield(CaseId, 0, ScopeId, Spec) || _ <- lists:seq(1, Iterations)]
    end),

    %% Cleanup
    wf_budget:stop(),
    wf_governance:stop(),

    %% Results
    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    AvgUs = Time / Iterations,
    io:format("~p governance checks (allowlist + budget): ~p microseconds total, ~p microseconds average~n",
        [Iterations, Time, AvgUs]),
    io:format("Success rate: ~p/~p (~.2f%)~n",
        [SuccessCount, Iterations, (SuccessCount * 100.0) / Iterations]),
    ok.
