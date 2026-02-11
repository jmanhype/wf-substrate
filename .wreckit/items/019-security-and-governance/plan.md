# Implement security and governance features Implementation Plan

## Implementation Plan Title

Governance and Safety Features for Workflow Execution

## Overview

Implement production-ready governance and safety features for workflow execution, including effect allowlists, resource budgets, timeout policies, mandatory approval gates, and structured error handling. These features provide essential guardrails for running workflows in production environments by constraining what effects can be executed, limiting resource consumption, enforcing time limits, requiring human approval for sensitive operations, and producing structured audit trails for all governance violations.

The implementation introduces three new modules (wf_governance, wf_budget, wf_approval) and extends existing patterns for policy management, enforcement, and error tracking. The design follows established OTP patterns in the codebase (gen_server behaviors, ETS table storage, structured records) and integrates with planned modules (wf_effect, wf_state, wf_cancel) through stub implementations that will be replaced when those items complete.

## Current State

**What Exists:**
- Basic executor (wf_exec.erl) with token-based execution, scope tracking via scope_stack, and mock task execution
- Structured trace events (wf_trace.erl) with ETS storage pattern and metadata field
- Validation errors (wf_validate.erl) with #issue{} record pattern for structured errors
- Opcode definitions (wf_vm.erl) supporting control flow patterns
- Test infrastructure with examples for basic workflows, cancellation, and parallel execution

**What's Missing:**
- No effect boundary system (wf_effect.erl - item 010 incomplete)
- No cancellation semantics (wf_cancel.erl - item 008 incomplete)
- No state store (wf_state.erl - referenced but not implemented)
- No governance enforcement mechanisms
- No allowlist checking for effects
- No budget tracking for resources
- No timeout policies (case/task/effect)
- No approval gate mechanism
- No structured governance error records

**Key Constraints:**
- Must implement stubs for wf_effect/wf_state integration since items 008/010 are incomplete
- Governance checks must be low-overhead (O(1) map lookups)
- All governance violations must produce structured errors (no silent failures)
- Policies must be enforceable per-scope (using existing scope_stack)
- Integration points must be clearly marked for future items

### Key Discoveries:

- **wf_exec.erl:28-34**: Token status enum includes `active`, `complete`, `cancelled` but NOT `blocked_approval` or `blocked_effect` - must be extended
- **wf_exec.erl:232-239**: execute_task_exec/2 is a mock that always returns ok - needs governance check integration
- **wf_exec.erl:61**: scope_stack tracks active scopes - enables per-scope policy enforcement
- **wf_trace.erl:82-93**: ETS table initialization pattern with read_concurrency - reuse for policy storage
- **wf_validate.erl:16-21**: #issue{} record pattern with type, message, state - model for governance errors
- **wf_exec.erl:134-138**: is_blocked/1 checks `blocked_effect`, `blocked_join`, `blocked_signal` - missing `blocked_approval`
- **wf_vm.erl:19**: CANCEL_SCOPE opcode exists - can add APPROVAL_WAIT opcode
- **wf_trace.erl:10-19**: #trace_event{} has metadata field - can add governance event types
- **wf_exec.erl:477-510**: CANCEL_SCOPE handlers exist - pattern for approval yielding

## Desired End State

**Functional Requirements:**
1. Effect allowlist enforcement: Each scope can define which effect types are permitted. Attempts to yield non-allowed effects produce structured allowlist_violation errors and are rejected before execution.

2. Resource budget tracking: Cases have configurable limits on effect count (max N effects), total effect execution time (max T microseconds), and total cost (sum of integer costs per effect type). Budgets are checked before effect execution and produce budget_exceeded errors when exhausted.

3. Timeout policies: Three levels of timeout enforcement - per-case (overall workflow wall time), per-task (individual task execution time), per-effect (effect response time). Timeouts produce structured timeout events and trigger cancellation.

4. Mandatory approval gates: Tasks can require explicit external approval before proceeding. Executor yields {waiting_approval, ApprovalSpec}, blocks until signal/2 called, with timeout triggering configurable action (cancel/default/escalate).

5. Structured error handling: All governance violations produce #governance_error{} records with error_type, scope, task_id, detail, timestamp. Errors are stored as receipts for audit trail.

**Non-Functional Requirements:**
- Governance checks add < 100 microseconds overhead per operation
- Budget checks are O(1) map lookups
- Policy updates do not require case restart (where possible)
- All policy violations are logged and traceable
- No silent failures - guards either allow or reject explicitly

**Integration Points:**
- wf_governance:set_allowlist/2, set_budget/2, set_timeout_policy/2, require_approval/2 exported APIs
- wf_effect:yield/4 extended with allowlist and budget checks
- wf_exec:execute_task_exec/2 extended with approval yielding
- wf_state mutations extended with budget tracking fields

## What We're NOT Doing

**Out of Scope for This Implementation:**

1. **Policy versioning**: Policies cannot be updated during active case execution. Changes apply only to new cases. Versioning deferred to v2.

2. **Allowlist inheritance**: No hierarchical allowlist model (e.g., child scopes inheriting parent allowlist). Each scope has explicit allowlist only.

3. **Dynamic policy discovery**: No runtime policy inspection or query API. Policies are set before execution and enforced.

4. **Budget carryover**: Budget limits reset per case. No accumulation or carryover between cases.

5. **Approval delegation**: Approval signals must come from external caller. No delegation or escalation workflows implemented.

6. **Policy persistence**: Policies stored in ETS tables only (in-memory). No disk persistence or recovery.

7. **Effect cost modeling**: Effect costs are explicit integers provided by caller. No automatic cost calculation or learning.

8. **Timeout grace periods**: No grace periods or warnings before timeout. Immediate enforcement when timeout expires.

9. **Governance analytics**: No metrics, dashboards, or analysis of governance violations. Raw receipts only.

10. **Policy validation**: No validation that policy settings are sensible (e.g., budget > 0). Caller responsible for valid policies.

## Implementation Approach

**High-Level Strategy**: Implement governance features incrementally in three phases: (1) Core infrastructure (error records, policy storage, basic modules), (2) Enforcement mechanisms (allowlist, budget, timeouts) with stubs for missing dependencies, (3) Approval system. Use dependency injection pattern where wf_effect/wf_state are abstracted behind interfaces, allowing stub implementations now and real integration later.

**Architectural Decisions**:

1. **Module Structure**: Three separate modules instead of monolithic:
   - **wf_governance**: Policy management (allowlists, timeouts, approval configs)
   - **wf_budget**: Budget tracking and enforcement (counters, limits, checks)
   - **wf_approval**: Approval gate management (waiting, signaling, timeouts)

2. **Storage Strategy**: ETS tables owned by gen_server processes for crash resilience:
   - wf_governance_policies: scope_id → policy records
   - wf_budgets: case_id → budget state
   - wf_approvals: approval_id → approval state

3. **Dependency Handling**: Implement stub modules that match planned interfaces:
   - wf_effect_stub: Provides yield/4 with governance hooks
   - wf_state_stub: Provides budget mutation interfaces
   - Stub modules marked with TODO comments for real integration

4. **Error Records**: Follow wf_validate #issue{} pattern:
   ```erlang
   -record(governance_error, {
       error_type :: allowlist_violation | budget_exceeded | timeout | approval_required,
       scope :: term(),
       task_id :: term() | undefined,
       detail :: binary(),
       timestamp :: erlang:timestamp()
   }).
   ```

5. **Integration Pattern**: Insert governance checks at existing integration points:
   - Allowlist check: BEFORE effect execution in wf_effect:yield/4
   - Budget check: BEFORE effect execution in wf_effect:yield/4
   - Approval yield: IN wf_exec:execute_task_exec/2 when task marked with approval
   - Timeout enforcement: Via erlang:send_after/3 and gen_statem state_timeout

6. **Testing Strategy**: Unit tests for each module, integration tests for enforcement, end-to-end tests for approval flows, property-based tests for budget invariants.

---

## Phases

### Phase 1: Core Infrastructure (Error Records and Storage)

#### Overview

Create foundational data structures and storage mechanisms for all governance features. This phase establishes the patterns used by subsequent phases.

#### Changes Required:

##### 1. Governance Error Records

**File**: `src/wf_governance.hrl` (new file)
**Changes**: Define error record and types

```erlang
%%%-------------------------------------------------------------------
%%% @doc Governance error record for structured violation reporting
%%% @end
%%%-------------------------------------------------------------------

%% Governance error record
-record(governance_error, {
    error_type :: allowlist_violation | budget_exceeded | timeout | approval_required,
    scope :: term(),
    task_id :: term() | undefined,
    detail :: binary(),
    timestamp :: erlang:timestamp()
}).

%% Governance policy types
-type scope_id() :: term().
-type case_id() :: term().
-type task_id() :: term().
-type effect_type() :: atom().

%% Budget types
-type budget_limit() :: {max_effects, non_neg_integer()} |
                      {max_time_us, non_neg_integer()} |
                      {max_cost, non_neg_integer()}.

%% Timeout types
-type timeout_policy() :: {case_timeout, non_neg_integer()} |
                        {task_timeout, non_neg_integer()} |
                        {effect_timeout, non_neg_integer()}.

%% Approval types
-type approval_id() :: term().
-type approval_action() :: cancel | default | escalate.
-type approval_spec() :: #{
    approval_id => approval_id(),
    timeout => non_neg_integer() | undefined,
    timeout_action => approval_action(),
    detail => binary()
}.

%% Exports
-export_type([
    scope_id/0,
    case_id/0,
    task_id/0,
    effect_type/0,
    budget_limit/0,
    timeout_policy/0,
    approval_id/0,
    approval_action/0,
    approval_spec/0
]).
```

##### 2. wf_governance Module (Policy Management)

**File**: `src/wf_governance.erl` (new file)
**Changes**: Implement policy storage and management

```erlang
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
    require_approval/2,
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
    approval :: approval_spec() | undefined
}).

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Start governance policy server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop governance policy server
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
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = ScopeId}
    end,
    UpdatedPolicy = Policy#policy{allowlist = EffectTypes},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),
    {reply, ok, State};

handle_call({get_allowlist, ScopeId}, _From, State) ->
    case ets:lookup(?POLICIES_TABLE, ScopeId) of
        [#policy{allowlist = AllowList}] when AllowList =/= undefined ->
            {reply, {ok, AllowList}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_budget, CaseId, Limits}, _From, State) ->
    Policy = case ets:lookup(?POLICIES_TABLE, CaseId) of
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = CaseId}
    end,
    UpdatedPolicy = Policy#policy{budget = Limits},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),
    {reply, ok, State};

handle_call({get_budget, CaseId}, _From, State) ->
    case ets:lookup(?POLICIES_TABLE, CaseId) of
        [#policy{budget = Budget}] when Budget =/= undefined ->
            {reply, {ok, Budget}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_timeout_policy, Id, Policy}, _From, State) ->
    PolicyRec = case ets:lookup(?POLICIES_TABLE, Id) of
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = Id}
    end,
    UpdatedPolicy = PolicyRec#policy{timeout = Policy},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),
    {reply, ok, State};

handle_call({get_timeout_policy, Id}, _From, State) ->
    case ets:lookup(?POLICIES_TABLE, Id) of
        [#policy{timeout = Timeout}] when Timeout =/= undefined ->
            {reply, {ok, Timeout}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({require_approval, ScopeId, TaskId, ApprovalSpec}, _From, State) ->
    Policy = case ets:lookup(?POLICIES_TABLE, ScopeId) of
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = ScopeId}
    end,
    %% Store approval config indexed by task_id
    ApprovalMap = Policy#policy.approval,
    UpdatedMap = case ApprovalMap of
        undefined -> #{TaskId => ApprovalSpec};
        Map -> Map#{TaskId => ApprovalSpec}
    end,
    UpdatedPolicy = Policy#policy{approval = UpdatedMap},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),
    {reply, ok, State};

handle_call({get_approval_config, TaskId}, _From, State) ->
    %% Scan all policies to find task approval config
    case ets:foldl(fun(#policy{approval = undefined}, Acc) -> Acc;
                       (#policy{approval = ApprovalMap}, Acc) ->
                           case ApprovalMap of
                               #{TaskId := Spec} -> {found, Spec};
                               _ -> Acc
                           end
                    end, not_found, ?POLICIES_TABLE) of
        {found, Spec} -> {reply, {ok, Spec}, State};
        not_found -> {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

##### 3. wf_budget Module (Budget Tracking)

**File**: `src/wf_budget.erl` (new file)
**Changes**: Implement budget tracking and checking

```erlang
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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ets:insert(?BUDGETS_TABLE, BudgetState),
    {reply, ok, State};

handle_call({check_budget, CaseId}, _From, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [#budget_state{limits = Limits, effect_count = Count,
                     effect_time_us = TimeUs, total_cost = Cost}] ->
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
        [BudgetState] ->
            {reply, {ok, BudgetState}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({increment_effect_count, CaseId}, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [#budget_state{effect_count = Count} = Budget] ->
            UpdatedBudget = Budget#budget_state{effect_count = Count + 1},
            ets:insert(?BUDGETS_TABLE, UpdatedBudget);
        [] -> ok
    end,
    {noreply, State};

handle_cast({add_effect_time, CaseId, TimeUs}, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [#budget_state{effect_time_us = CurrentTime} = Budget] ->
            UpdatedBudget = Budget#budget_state{effect_time_us = CurrentTime + TimeUs},
            ets:insert(?BUDGETS_TABLE, UpdatedBudget);
        [] -> ok
    end,
    {noreply, State};

handle_cast({add_effect_cost, CaseId, Cost}, State) ->
    case ets:lookup(?BUDGETS_TABLE, CaseId) of
        [#budget_state{total_cost = CurrentCost} = Budget] ->
            UpdatedBudget = Budget#budget_state{total_cost = CurrentCost + Cost},
            ets:insert(?BUDGETS_TABLE, UpdatedBudget);
        [] -> ok
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

check_limits([], _Count, _TimeUs, _Cost) ->
    ok;
check_limits([{max_effects, Max} | Rest], Count, TimeUs, Cost) when Count >= Max ->
    {error, max_effects, Count, Max};
check_limits([{max_time_us, Max} | Rest], Count, TimeUs, Cost) when TimeUs >= Max ->
    {error, max_time_us, TimeUs, Max};
check_limits([{max_cost, Max} | Rest], Count, TimeUs, Cost) when Cost >= Max ->
    {error, max_cost, Cost, Max};
check_limits([_Limit | Rest], Count, TimeUs, Cost) ->
    check_limits(Rest, Count, TimeUs, Cost).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles without errors: `erlc -I include src/wf_governance.erl`
- [ ] Module compiles without errors: `erlc -I include src/wf_budget.erl`
- [ ] Header file compiles: `erlc src/wf_governance.hrl`
- [ ] Unit tests pass: `rebar3 eunit`
- [ ] gen_server starts and stops cleanly

##### Manual Verification:

- [ ] ETS tables are created on gen_server start
- [ ] Policies can be set and retrieved via API
- [ ] Budget state can be initialized and queried
- [ ] Budget checks correctly enforce limits
- [ ] No memory leaks in ETS tables after gen_server stop

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Effect Allowlist and Budget Enforcement

#### Overview

Implement allowlist checking and budget enforcement at the effect yield point. Create stub wf_effect module with governance integration hooks.

#### Changes Required:

##### 1. wf_effect_stub Module

**File**: `src/wf_effect_stub.erl` (new file)
**Changes**: Implement stub effect module with governance checks

```erlang
-module(wf_effect_stub).

%% API
-export([
    new_spec/5,
    yield/4,
    get_effect_cost/1
]).

-include("wf_governance.hrl").

%% Effect specification record (matching item 010 plan)
-record(effect_spec, {
    effect_id :: term(),
    effect_type :: atom(),
    payload :: term(),
    idempotency_key :: term() | undefined,
    timeout :: pos_integer() | undefined,
    cost :: integer()  %% Cost for budget tracking
}).

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Create new effect specification
-spec new_spec(term(), non_neg_integer(), term(), atom(), term()) -> #effect_spec{}.
new_spec(CaseId, StepSeq, ScopeId, EffectType, Payload) ->
    EffectId = {CaseId, StepSeq, ScopeId},
    #effect_spec{
        effect_id = EffectId,
        effect_type = EffectType,
        payload = Payload,
        idempotency_key = undefined,
        timeout = undefined,
        cost = get_default_cost(EffectType)
    }.

%% @doc Yield effect with governance checks
%% NOTE: This is a stub implementation. Real implementation in item 010.
-spec yield(term(), non_neg_integer(), term(), #effect_spec{}) ->
    {ok, term()} | {error, #governance_error{}}.
yield(CaseId, StepSeq, ScopeId, EffectSpec) ->
    %% 1. Check allowlist for scope
    case wf_governance:get_allowlist(ScopeId) of
        {ok, AllowList} ->
            EffectType = EffectSpec#effect_spec.effect_type,
            case lists:member(EffectType, AllowList) of
                false ->
                    Error = #governance_error{
                        error_type = allowlist_violation,
                        scope = ScopeId,
                        task_id = undefined,
                        detail = list_to_binary(
                            io_lib:format("Effect type ~p not in allowlist",
                                [EffectType])),
                        timestamp = erlang:timestamp()
                    },
                    {error, Error};
                true ->
                    %% 2. Check budget for case
                    case wf_budget:check_budget(CaseId) of
                        {error, _} = Error ->
                            Error;
                        ok ->
                            %% 3. Execute effect (mock)
                            execute_effect_mock(EffectSpec)
                    end
            end;
        {error, not_found} ->
            %% No allowlist configured, allow all effects
            case wf_budget:check_budget(CaseId) of
                {error, _} = Error ->
                    Error;
                ok ->
                    execute_effect_mock(EffectSpec)
            end
    end.

%% @doc Get cost for effect type
-spec get_effect_cost(#effect_spec{}) -> integer().
get_effect_cost(EffectSpec) ->
    EffectSpec#effect_spec.cost.

%%%====================================================================
%%% Internal functions
%%%====================================================================

%% Mock effect execution
execute_effect_mock(EffectSpec) ->
    %% In real implementation (item 010), this would:
    %% 1. Store effect in ETS table
    %% 2. Spawn handler process
    %% 3. Return {ok, Effect}
    %% For now, return mock result
    {ok, #{result => mock_effect_done}}.

%% Default costs for common effect types
get_default_cost(http_get) -> 1;
get_default_cost(http_post) -> 2;
get_default_cost(file_write) -> 3;
get_default_cost(file_read) -> 1;
get_default_cost(db_query) -> 5;
get_default_cost(send_email) -> 10;
get_default_cost(_) -> 1.
```

##### 2. Extend wf_exec Token Record

**File**: `src/wf_exec.erl`
**Changes**: Add blocked_approval status to token

```erlang
%% Line 28-34: Update token record
-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled | blocked_effect | blocked_approval
}).
```

##### 3. Extend wf_exec is_blocked/1

**File**: `src/wf_exec.erl`
**Changes**: Add blocked_approval to status check

```erlang
%% Line 134-138: Update is_blocked function
-spec is_blocked(exec_state()) -> boolean().
is_blocked(#exec_state{status = Status}) ->
    Status =:= blocked_effect orelse
    Status =:= blocked_join orelse
    Status =:= blocked_signal orelse
    Status =:= blocked_approval.  %% NEW
```

##### 4. Update wf_vm Opcodes

**File**: `src/wf_vm.erl`
**Changes**: Add APPROVAL_WAIT opcode

```erlang
%% Line 13-21: Add approval opcode to type
-type opcode() ::
    {atom(), non_neg_integer()} |
    {atom(), [non_neg_integer()]} |
    {atom(), join_policy()} |
    {atom(), loop_policy()} |
    {atom(), mi_policy()} |
    {atom(), {enter | exit, term()}} |
    {atom(), atom()} |
    {'APPROVAL_WAIT', term()} |  %% NEW: Approval gate
    {atom()}.
```

##### 5. Integrate wf_governance and wf_budget into Application

**File**: `src/wf_substrate_app.erl`
**Changes**: Start governance servers in supervision tree

```erlang
%% Add to children list in start/2 function
children = [
    wf_governance,
    wf_budget,
    %% ... existing children ...
]
```

#### Success Criteria:

##### Automated Verification:

- [ ] wf_effect_stub compiles: `erlc -I include src/wf_effect_stub.erl`
- [ ] wf_exec compiles with updated token record
- [ ] wf_vm compiles with new opcode type
- [ ] Application starts without errors: `rebar3 shell`
- [ ] Unit tests for allowlist checking pass
- [ ] Unit tests for budget checking pass
- [ ] Integration test for effect yield with governance passes

##### Manual Verification:

- [ ] Allowlist correctly rejects non-allowed effects
- [ ] Budget checks correctly enforce limits
- [ ] Governance errors are properly structured
- [ ] Effect execution proceeds when all checks pass
- [ ] ETS tables are accessible from all modules

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Approval System

#### Overview

Implement mandatory approval gates where tasks can require external approval before proceeding. Tasks yield with approval spec, block until signal received, and handle timeout with configurable action.

#### Changes Required:

##### 1. wf_approval Module

**File**: `src/wf_approval.erl` (new file)
**Changes**: Implement approval gate management

```erlang
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
    approvals_table :: ets:tid()
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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Request approval for task (blocks until approval)
-spec request_approval(task_id(), approval_spec()) ->
    {ok, approved} | {error, #governance_error{}}.
request_approval(TaskId, ApprovalSpec) ->
    ApprovalId = maps:get(approval_id, ApprovalSpec, make_ref()),
    gen_server:call(?SERVER, {request_approval, TaskId, ApprovalSpec, self()}),
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
    {ok, #state{approvals_table = Table}}.

handle_call({request_approval, TaskId, ApprovalSpec, CallerPid}, _From, State) ->
    ApprovalId = maps:get(approval_id, ApprovalSpec, make_ref()),
    Timeout = maps:get(timeout, ApprovalSpec, undefined),
    TimeoutAction = maps:get(timeout_action, ApprovalSpec, cancel),
    Detail = maps:get(detail, ApprovalSpec, <<>>),

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
    ets:insert(?APPROVALS_TABLE, ApprovalState),

    %% Monitor caller process in case it dies
    erlang:monitor(process, CallerPid),

    {reply, waiting, State};

handle_call({signal, ApprovalId, Decision}, _From, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [#approval_state{status = pending, timer_ref = TimerRef} = Approval] ->
            %% Cancel timeout timer
            case TimerRef of
                undefined -> ok;
                _ -> erlang:cancel_timer(TimerRef)
            end,

            %% Find waiting process (stored in ets or process dict)
            %% For simplicity, we broadcast to all registered processes
            %% In production, track waiting process explicitly
            case Decision of
                approve ->
                    UpdatedApproval = Approval#approval_state{status = approved},
                    ets:insert(?APPROVALS_TABLE, UpdatedApproval),
                    %% Notify waiting process (simplified)
                    self() ! {approval_granted, ApprovalId},
                    {reply, ok, State};
                reject ->
                    UpdatedApproval = Approval#approval_state{status = rejected},
                    ets:insert(?APPROVALS_TABLE, UpdatedApproval),
                    self() ! {approval_rejected, ApprovalId, rejected_by_signaler},
                    {reply, ok, State}
            end;
        [#approval_state{status = Status}] when Status =/= pending ->
            {reply, {error, {already_processed, Status}}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({check_approval, ApprovalId}, _From, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [#approval_state{status = Status}] ->
            {reply, {ok, Status}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({cancel_approval, ApprovalId}, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [#approval_state{status = pending, timer_ref = TimerRef}] ->
            case TimerRef of
                undefined -> ok;
                _ -> erlang:cancel_timer(TimerRef)
            end,
            ets:delete(?APPROVALS_TABLE, ApprovalId);
        _ -> ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({approval_timeout, ApprovalId}, State) ->
    case ets:lookup(?APPROVALS_TABLE, ApprovalId) of
        [#approval_state{status = pending, timeout_action = Action} = Approval] ->
            UpdatedApproval = Approval#approval_state{status = timeout},
            ets:insert(?APPROVALS_TABLE, UpdatedApproval),

            %% Execute timeout action
            case Action of
                cancel ->
                    %% Notify waiting process with timeout error
                    self() ! {approval_timeout, ApprovalId};
                default ->
                    %% Auto-approve with default value
                    self() ! {approval_granted, ApprovalId};
                escalate ->
                    %% Notify escalation handler
                    self() ! {approval_escalate, ApprovalId}
            end,
            {noreply, State};
        _ ->
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    %% Handle monitored process death
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

##### 2. Add wf_approval to Application Supervisor

**File**: `src/wf_substrate_sup.erl`
**Changes**: Add wf_approval to supervision tree

```erlang
%% Add to children list
wf_approval,
```

#### Success Criteria:

##### Automated Verification:

- [ ] wf_approval compiles: `erlc -I include src/wf_approval.erl`
- [ ] Application starts with all servers: `rebar3 shell`
- [ ] Unit tests for approval request pass
- [ ] Unit tests for approval signal pass
- [ ] Unit tests for approval timeout pass
- [ ] Unit tests for timeout actions (cancel/default/escalate) pass

##### Manual Verification:

- [ ] Approval requests block until signal received
- [ ] Approval signals correctly unblock waiting tasks
- [ ] Timeout triggers configured action
- [ ] Cancel action terminates approval with error
- [ ] Default action auto-approves approval
- [ ] Escalate action sends escalation message
- [ ] Rejected approvals return structured error

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Timeout Policy Enforcement

#### Overview

Implement three levels of timeout enforcement - per-case, per-task, and per-effect. Timeouts produce structured events and can trigger cancellation.

#### Changes Required:

##### 1. Extend wf_governance with Timeout Storage

**File**: `src/wf_governance.erl`
**Changes**: Add timeout policy storage API (already exists in Phase 1, add implementation)

```erlang
%% Add to handle_call for timeout policies
handle_call({set_timeout_policy, Id, {case_timeout, Ms}}, _From, State) ->
    %% Schedule timeout for case
    PolicyRec = case ets:lookup(?POLICIES_TABLE, Id) of
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = Id}
    end,
    UpdatedPolicy = PolicyRec#policy{timeout = {case_timeout, Ms}},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),

    %% Start timeout timer
    erlang:send_after(Ms, self(), {case_timeout, Id}),
    {reply, ok, State};

handle_call({set_timeout_policy, Id, {task_timeout, Ms}}, _From, State) ->
    PolicyRec = case ets:lookup(?POLICIES_TABLE, Id) of
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = Id}
    end,
    UpdatedPolicy = PolicyRec#policy{timeout = {task_timeout, Ms}},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),
    {reply, ok, State};

handle_call({set_timeout_policy, Id, {effect_timeout, Ms}}, _From, State) ->
    PolicyRec = case ets:lookup(?POLICIES_TABLE, Id) of
        [#policy{} = P] -> P;
        [] -> #policy{scope_id = Id}
    end,
    UpdatedPolicy = PolicyRec#policy{timeout = {effect_timeout, Ms}},
    ets:insert(?POLICIES_TABLE, UpdatedPolicy),
    {reply, ok, State}.
```

##### 2. Add Timeout Handling to wf_governance gen_server

**File**: `src/wf_governance.erl`
**Changes**: Add handle_info for timeout events

```erlang
handle_info({case_timeout, CaseId}, State) ->
    %% Produce timeout event
    Error = #governance_error{
        error_type = timeout,
        scope = CaseId,
        task_id = undefined,
        detail = <<"Case timeout exceeded">>,
        timestamp = erlang:timestamp()
    },

    %% Trigger cancellation (via wf_cancel when available)
    %% For now, log error
    io:format("Case timeout: ~p~n", [CaseId]),

    {noreply, State};

handle_info({task_timeout, TaskId}, State) ->
    Error = #governance_error{
        error_type = timeout,
        scope = TaskId,
        task_id = TaskId,
        detail = <<"Task timeout exceeded">>,
        timestamp = erlang:timestamp()
    },
    io:format("Task timeout: ~p~n", [TaskId]),
    {noreply, State};

handle_info({effect_timeout, EffectId}, State) ->
    Error = #governance_error{
        error_type = timeout,
        scope = EffectId,
        task_id = undefined,
        detail = <<"Effect timeout exceeded">>,
        timestamp = erlang:timestamp()
    },
    io:format("Effect timeout: ~p~n", [EffectId]),
    {noreply, State};
```

##### 3. Extend wf_effect_stub with Timeout

**File**: `src/wf_effect_stub.erl`
**Changes**: Add timeout checking to yield/4

```erlang
yield(CaseId, StepSeq, ScopeId, EffectSpec) ->
    %% 1. Check allowlist
    case wf_governance:get_allowlist(ScopeId) of
        {ok, AllowList} ->
            EffectType = EffectSpec#effect_spec.effect_type,
            case lists:member(EffectType, AllowList) of
                false ->
                    Error = #governance_error{
                        error_type = allowlist_violation,
                        scope = ScopeId,
                        detail = list_to_binary(io_lib:format("Effect type ~p not allowed", [EffectType])),
                        timestamp = erlang:timestamp()
                    },
                    {error, Error};
                true ->
                    %% 2. Check budget
                    case wf_budget:check_budget(CaseId) of
                        {error, _} = Error ->
                            Error;
                        ok ->
                            %% 3. Check effect timeout
                            EffectTimeout = EffectSpec#effect_spec.timeout,
                            case wf_governance:get_timeout_policy(ScopeId) of
                                {ok, {effect_timeout, MaxTimeout}} when EffectTimeout > MaxTimeout ->
                                    Error = #governance_error{
                                        error_type = timeout,
                                        scope = ScopeId,
                                        detail = list_to_binary(io_lib:format(
                                            "Effect timeout ~p exceeds policy limit ~p",
                                            [EffectTimeout, MaxTimeout])),
                                        timestamp = erlang:timestamp()
                                    },
                                    {error, Error};
                                _ ->
                                    %% Execute effect
                                    execute_effect_mock(EffectSpec)
                            end
                    end
            end;
        {error, not_found} ->
            %% No allowlist, check budget only
            case wf_budget:check_budget(CaseId) of
                {error, _} = Error ->
                    Error;
                ok ->
                    execute_effect_mock(EffectSpec)
            end
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] wf_governance compiles with timeout extensions
- [ ] wf_effect_stub compiles with timeout checking
- [ ] Unit tests for timeout policies pass
- [ ] Unit tests for timeout enforcement pass
- [ ] Integration test for case timeout passes
- [ ] Integration test for task timeout passes
- [ ] Integration test for effect timeout passes

##### Manual Verification:

- [ ] Case timeout triggers cancellation
- [ ] Task timeout produces structured error
- [ ] Effect timeout prevents execution
- [ ] Timeout events are logged with timestamps
- [ ] Timeout actions are configurable

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Integration Testing and Documentation

#### Overview

Complete integration with existing modules, add comprehensive tests, and document APIs and usage patterns.

#### Changes Required:

##### 1. Create Test Modules

**File**: `test/wf_governance_tests.erl` (new file)

```erlang
-module(wf_governance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_governance.hrl").

%% Allowlist tests
allowlist_test_() ->
    {setup,
        fun() -> wf_governance:start_link() end,
        fun(_) -> wf_governance:stop() end,
        [
            fun set_get_allowlist/0,
            fun allowlist_enforcement/0
        ]
    }.

set_get_allowlist() ->
    ScopeId = test_scope,
    wf_governance:set_allowlist(ScopeId, [http_get, http_post]),
    {ok, AllowList} = wf_governance:get_allowlist(ScopeId),
    ?assertEqual([http_get, http_post], AllowList).

allowlist_enforcement() ->
    ScopeId = test_scope2,
    wf_governance:set_allowlist(ScopeId, [http_get]),

    %% Allowed effect
    AllowedSpec = wf_effect_stub:new_spec(case1, 0, ScopeId, http_get, #{}),
    Result = wf_effect_stub:yield(case1, 0, ScopeId, AllowedSpec),
    ?assertMatch({ok, _}, Result),

    %% Blocked effect
    BlockedSpec = wf_effect_stub:new_spec(case1, 1, ScopeId, http_post, #{}),
    ErrorResult = wf_effect_stub:yield(case1, 1, ScopeId, BlockedSpec),
    ?assertMatch({error, #governance_error{error_type = allowlist_violation}}, ErrorResult).

%% Budget tests
budget_test_() ->
    {setup,
        fun() ->
            wf_governance:start_link(),
            wf_budget:start_link()
        end,
        fun(_) ->
            wf_budget:stop(),
            wf_governance:stop()
        end,
        [
            fun budget_enforcement/0,
            fun budget_check_limits/0
        ]
    }.

budget_enforcement() ->
    CaseId = case1,
    wf_budget:init_budget(CaseId, [{max_effects, 2}]),

    %% First effect allowed
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:increment_effect_count(CaseId),

    %% Second effect allowed
    ?assertEqual(ok, wf_budget:check_budget(CaseId)),
    wf_budget:increment_effect_count(CaseId),

    %% Third effect blocked
    ?assertMatch({error, #governance_error{error_type = budget_exceeded}},
                wf_budget:check_budget(CaseId)).

budget_check_limits() ->
    CaseId = case2,
    wf_budget:init_budget(CaseId, [
        {max_effects, 10},
        {max_time_us, 1000000},
        {max_cost, 100}
    ]),

    ?assertEqual(ok, wf_budget:check_budget(CaseId)).

%% Approval tests
approval_test_() ->
    {setup,
        fun() -> wf_approval:start_link() end,
        fun(_) -> wf_approval:stop() end,
        [
            fun approval_request_and_signal/0,
            fun approval_timeout_test/0
        ]
    }.

approval_request_and_signal() ->
    ApprovalSpec = #{
        approval_id = test_approval,
        timeout => 5000,
        timeout_action => cancel,
        detail => <<"Test approval">>
    },

    %% Request approval in separate process
    spawn_link(fun() ->
        Result = wf_approval:request_approval(task1, ApprovalSpec),
        ?assertEqual({ok, approved}, Result)
    end),

    %% Wait for request to register
    timer:sleep(100),

    %% Signal approval
    ?assertEqual(ok, wf_approval:signal(test_approval, approve)).

approval_timeout_test() ->
    ApprovalSpec = #{
        approval_id = timeout_approval,
        timeout => 100,  %% 100ms
        timeout_action => cancel,
        detail => <<"Timeout test">>
    },

    %% Request approval
    Result = wf_approval:request_approval(task2, ApprovalSpec),

    %% Should timeout
    ?assertMatch({error, #governance_error{error_type = timeout}}, Result).
```

##### 2. Create API Documentation

**File**: `doc/governance.md` (new file)

```markdown
# Governance and Safety Features

## Overview

The governance system provides production-ready safety features for workflow execution:

1. **Effect Allowlists**: Control which effect types can be executed in a scope
2. **Resource Budgets**: Limit effect count, execution time, and total cost
3. **Timeout Policies**: Enforce time limits at case, task, and effect levels
4. **Approval Gates**: Require explicit approval for sensitive operations
5. **Structured Errors**: All violations produce audit-ready error records

## Effect Allowlists

### Setting an Allowlist

```erlang
%% Allow only http_get and file_read effects in scope1
wf_governance:set_allowlist(scope1, [http_get, file_read]).
```

### Allowlist Enforcement

Effects not in the allowlist are rejected before execution:

```erlang
%% This effect is allowed
Spec1 = wf_effect:new_spec(case1, 0, scope1, http_get, #{url => "/api/data"}),
{ok, Result} = wf_effect:yield(case1, 0, scope1, Spec1).

%% This effect is blocked (http_post not in allowlist)
Spec2 = wf_effect:new_spec(case1, 1, scope1, http_post, #{url => "/api/submit"}),
{error, #governance_error{error_type = allowlist_violation}} =
    wf_effect:yield(case1, 1, scope1, Spec2).
```

## Resource Budgets

### Setting Budget Limits

```erlang
wf_budget:init_budget(case1, [
    {max_effects, 100},           %% Max 100 effects
    {max_time_us, 60000000},     %% Max 60 seconds of effect time
    {max_cost, 1000}              %% Max total cost of 1000
]).
```

### Effect Costs

Each effect type has an associated cost:

```erlang
%% Default costs (can be customized)
get_effect_cost(http_get) -> 1.
get_effect_cost(http_post) -> 2.
get_effect_cost(send_email) -> 10.
```

### Budget Enforcement

Budgets are checked before each effect execution:

```erlang
ok = wf_budget:check_budget(case1).

%% After effect execution, update budget
wf_budget:increment_effect_count(case1),
wf_budget:add_effect_time(case1, 5000),  %% 5ms
wf_budget:add_effect_cost(case1, 1).
```

## Timeout Policies

### Setting Timeouts

```erlang
%% Case timeout (overall workflow execution)
wf_governance:set_timeout_policy(case1, {case_timeout, 300000}).  %% 5 minutes

%% Task timeout (individual task)
wf_governance:set_timeout_policy(task1, {task_timeout, 30000}).  %% 30 seconds

%% Effect timeout (effect response time)
wf_governance:set_timeout_policy(scope1, {effect_timeout, 5000}).  %% 5 seconds
```

## Approval Gates

### Requiring Approval

```erlang
%% In workflow definition, mark task as requiring approval
ApprovalSpec = #{
    approval_id => approve_payment,
    timeout => 86400000,  %% 24 hours
    timeout_action => cancel,
    detail => <<"Manual approval required for payment">>
},
wf_governance:require_approval(payment_scope, process_payment, ApprovalSpec).
```

### Signaling Approval

```erlang
%% External process approves the operation
wf_approval:signal(approve_payment, approve).
```

### Timeout Actions

Three actions when approval timeout expires:

- `cancel`: Terminate with timeout error
- `default`: Auto-approve with default value
- `escalate`: Notify escalation handler

## Error Records

All governance violations produce structured errors:

```erlang
#governance_error{
    error_type = allowlist_violation | budget_exceeded | timeout | approval_required,
    scope = ScopeId,
    task_id = TaskId | undefined,
    detail <<"Detailed message">>,
    timestamp = {MegaSecs, Secs, MicroSecs}
}
```

## Integration Notes

This implementation provides stub modules for integration with items 008 and 010:

- `wf_effect_stub`: Placeholder for wf_effect (item 010)
- Budget tracking via separate ETS table (will integrate with wf_state in item 010)
- Timeout cancellation via logging (will integrate with wf_cancel in item 008)
```

#### Success Criteria:

##### Automated Verification:

- [ ] All test modules compile: `rebar3 compile`
- [ ] All tests pass: `rebar3 eunit`
- [ ] Test coverage > 80%: `rebar3 cover`
- [ ] No dialyzer warnings: `rebar3 dialyzer`
- [ ] Documentation builds: `rebar3 edoc`

##### Manual Verification:

- [ ] Example workflows run successfully with governance
- [ ] Governance violations produce clear error messages
- [ ] API documentation is complete and accurate
- [ ] Integration points are clearly marked for future items

**Note**: This is the final phase. All automated and manual verification must pass before implementation is complete.

---

## Testing Strategy

### Unit Tests:

**wf_governance module:**
- Test setting and retrieving allowlists
- Test setting and retrieving timeout policies
- Test setting and retrieving approval configs
- Test ETS table operations
- Test concurrent access patterns

**wf_budget module:**
- Test budget initialization
- Test budget checking (all limit types)
- Test budget increments (count, time, cost)
- Test budget exhaustion detection
- Test multiple cases with independent budgets

**wf_approval module:**
- Test approval request and signal flow
- Test approval timeout with each action type
- Test approval rejection
- Test concurrent approval requests
- Test timer cleanup

**wf_effect_stub module:**
- Test effect spec creation
- Test allowlist checking
- Test budget checking
- Test effect execution (mock)
- Test error production

### Integration Tests:

**Allowlist + Budget enforcement:**
- Create case with budget and allowlist
- Execute allowed effects within budget
- Attempt disallowed effect (should reject)
- Exhaust budget and verify enforcement

**Timeout policies:**
- Set case timeout and verify timeout
- Set task timeout and verify enforcement
- Set effect timeout and verify rejection

**Approval gates:**
- Create workflow requiring approval
- Request approval and verify blocking
- Signal approval and verify resumption
- Test timeout with all action types

**Error handling:**
- Verify all governance violations produce #governance_error{}
- Verify error records contain all required fields
- Verify errors are stored as receipts (when wf_receipt available)

### Property-Based Tests:

```erlang
%% Property: Budget never exceeds configured limits
prop_budget_never_exceeds() ->
    ?FORALL({Limits, Effects},
        {budget_limits(), effect_list()},
        begin
            CaseId = make_ref(),
            wf_budget:init_budget(CaseId, Limits),
            execute_effects(CaseId, Effects),
            {ok, BudgetState} = wf_budget:get_budget_state(CaseId),
            verify_within_limits(BudgetState, Limits)
        end).

%% Property: Allowlist only allows configured types
prop_allowlist_enforcement() ->
    ?FORALL({AllowList, EffectType},
        {effect_list(), effect_type()},
        begin
            ScopeId = make_ref(),
            wf_governance:set_allowlist(ScopeId, AllowList),
            Spec = wf_effect_stub:new_spec(case1, 0, ScopeId, EffectType, #{}),
            Result = wf_effect_stub:yield(case1, 0, ScopeId, Spec),
            case lists:member(EffectType, AllowList) of
                true -> Result =:= {ok, _} orelse Result =:= {error, #governance_error{error_type = budget_exceeded}};
                false -> Result =:= {error, #governance_error{error_type = allowlist_violation}}
            end
        end).
```

### Performance Tests:

```erlang
%% Benchmark: Budget check overhead
bench_budget_check() ->
    wf_budget:init_budget(case1, [{max_effects, 1000}]),
    {Time, _} = timer:tc(fun() ->
        [wf_budget:check_budget(case1) || _ <- lists:seq(1, 10000)]
    end, 1000),
    io:format("10000 budget checks: ~p microsec~n", [Time]).

%% Benchmark: Allowlist check overhead
bench_allowlist_check() ->
    wf_governance:set_allowlist(scope1, [http_get, http_post, file_read]),
    {Time, _} = timer:tc(fun() ->
        [wf_governance:get_allowlist(scope1) || _ <- lists:seq(1, 10000)]
    end, 1000),
    io:format("10000 allowlist checks: ~p microsec~n", [Time]).
```

## Migration Notes

**No Migration Required**: This is new functionality with no existing data to migrate.

**Future Integration Points**:

When item 010 (Effect Boundary and Receipts) completes:
1. Replace `wf_effect_stub` with `wf_effect`
2. Move governance checks into real `wf_effect:yield/4`
3. Store governance errors as receipts via `wf_receipt`

When item 008 (Cancellation Semantics) completes:
1. Integrate timeout-triggered cancellation with `wf_cancel:cancel_case/1`
2. Use `wf_cancel` for approval timeout actions (cancel case on approval timeout)

When item 006 (State Store) completes:
1. Move budget tracking from ETS table to `wf_state` mutations
2. Use `wf_state:buffer_mutation/2` for budget updates
3. Persist budget state via `wf_state:commit/1`

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/019-security-and-governance/research.md`
- Existing executor: `/Users/speed/wf-substrate/src/wf_exec.erl`
- Existing tracing: `/Users/speed/wf-substrate/src/wf_trace.erl`
- Existing validation: `/Users/speed/wf-substrate/src/wf_validate.erl`
- Item 010 plan: `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/plan.md`
- Item 008 plan: `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/plan.md`
