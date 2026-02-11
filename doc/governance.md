# Governance and Safety Features

## Overview

The governance system provides production-ready safety features for workflow execution:

1. **Effect Allowlists**: Control which effect types can be executed in a scope
2. **Resource Budgets**: Limit effect count, execution time, and total cost
3. **Timeout Policies**: Enforce time limits at case, task, and effect levels
4. **Approval Gates**: Require explicit approval for sensitive operations
5. **Structured Errors**: All violations produce audit-ready error records

## Architecture

The governance system consists of three core modules:

- **wf_governance**: Policy management (allowlists, timeouts, approval configs)
- **wf_budget**: Budget tracking and enforcement (counters, limits, checks)
- **wf_approval**: Approval gate management (waiting, signaling, timeouts)

All modules are implemented as gen_servers with ETS table storage for crash resilience and read concurrency.

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
Spec1 = wf_effect_stub:new_spec(case1, 0, scope1, http_get, #{url => "/api/data"}),
{ok, Result} = wf_effect_stub:yield(case1, 0, scope1, Spec1).

%% This effect is blocked (http_post not in allowlist)
Spec2 = wf_effect_stub:new_spec(case1, 1, scope1, http_post, #{url => "/api/submit"}),
{error, #governance_error{error_type = allowlist_violation}} =
    wf_effect_stub:yield(case1, 1, scope1, Spec2).
```

### API

- `set_allowlist(ScopeId, EffectTypes) -> ok`
- `get_allowlist(ScopeId) -> {ok, [EffectType]} | {error, not_found}`

## Resource Budgets

### Setting Budget Limits

```erlang
wf_budget:init_budget(case1, [
    {max_effects, 100},           %% Max 100 effects
    {max_time_us, 60000000},      %% Max 60 seconds of effect time
    {max_cost, 1000}              %% Max total cost of 1000
]).
```

### Effect Costs

Each effect type has an associated cost (configured in wf_effect_stub):

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

### Budget Exhaustion

When a budget is exhausted, subsequent effects are rejected:

```erlang
{error, #governance_error{
    error_type = budget_exceeded,
    detail = <<"max_effects limit exceeded: 101/100">>
}} = wf_budget:check_budget(case1).
```

### API

- `init_budget(CaseId, Limits) -> ok`
- `check_budget(CaseId) -> ok | {error, #governance_error{}}`
- `increment_effect_count(CaseId) -> ok`
- `add_effect_time(CaseId, TimeUs) -> ok`
- `add_effect_cost(CaseId, Cost) -> ok`
- `get_budget_state(CaseId) -> {ok, #budget_state{}} | {error, not_found}`

## Timeout Policies

### Setting Timeouts

Three levels of timeout enforcement:

```erlang
%% Case timeout (overall workflow execution)
wf_governance:set_timeout_policy(case1, {case_timeout, 300000}).  %% 5 minutes

%% Task timeout (individual task)
wf_governance:set_timeout_policy(task1, {task_timeout, 30000}).  %% 30 seconds

%% Effect timeout (effect response time)
wf_governance:set_timeout_policy(scope1, {effect_timeout, 5000}).  %% 5 seconds
```

### Timeout Behavior

When a timeout expires, a structured error is produced:

```erlang
#governance_error{
    error_type = timeout,
    scope = CaseId,
    detail = <<"Case timeout exceeded">>,
    timestamp = {MegaSecs, Secs, MicroSecs}
}
```

**Note**: Timeout-triggered cancellation will be integrated with wf_cancel module in item 008. Currently, timeout events are logged.

### API

- `set_timeout_policy(Id, Policy) -> ok`
- `get_timeout_policy(Id) -> {ok, timeout_policy()} | {error, not_found}`

## Approval Gates

### Requiring Approval

```erlang
%% Configure approval requirement
ApprovalSpec = #{
    approval_id => approve_payment,
    timeout => 86400000,  %% 24 hours
    timeout_action => cancel,
    detail => <<"Manual approval required for payment">>
},
wf_governance:require_approval(payment_scope, process_payment, ApprovalSpec).
```

### Requesting Approval

```erlang
%% Task execution blocks until approval received
case wf_approval:request_approval(process_payment, ApprovalSpec) of
    {ok, approved} ->
        %% Proceed with task execution
        execute_task();
    {error, #governance_error{error_type = timeout}} ->
        %% Approval timeout
        handle_timeout();
    {error, #governance_error{error_type = approval_required}} ->
        %% Approval rejected
        handle_rejection()
end.
```

### Signaling Approval

External process approves or rejects the operation:

```erlang
%% Approve
wf_approval:signal(approve_payment, approve).

%% Reject
wf_approval:signal(approve_payment, reject).
```

### Timeout Actions

Three actions when approval timeout expires:

- **cancel**: Terminate with timeout error
- **default**: Auto-approve with default value
- **escalate**: Notify escalation handler (currently logs)

### API

- `request_approval(TaskId, ApprovalSpec) -> {ok, approved} | {error, #governance_error{}}`
- `signal(ApprovalId, approve | reject) -> ok | {error, term()}`
- `check_approval(ApprovalId) -> {ok, Status} | {error, not_found}`
- `cancel_approval(ApprovalId) -> ok`

## Error Records

All governance violations produce structured errors:

```erlang
#governance_error{
    error_type :: allowlist_violation | budget_exceeded | timeout | approval_required,
    scope :: term(),
    task_id :: term() | undefined,
    detail :: binary(),
    timestamp :: erlang:timestamp()
}
```

### Error Types

1. **allowlist_violation**: Effect type not in scope's allowlist
2. **budget_exceeded**: Resource limit (count/time/cost) exceeded
3. **timeout**: Case, task, or effect timeout expired
4. **approval_required**: Approval was rejected or required but not granted

## Integration with Existing Modules

### wf_exec Integration

The executor has been extended to support approval blocking:

- Token status now includes `blocked_approval` (in addition to `active`, `complete`, `cancelled`, `blocked_effect`)
- New opcode `{'APPROVAL_WAIT', ApprovalSpec}` for approval yielding
- `is_blocked/1` checks for `blocked_approval` status

### wf_vm Integration

The VM opcode type has been extended:

```erlang
-type opcode() ::
    ...existing opcodes... |
    {'APPROVAL_WAIT', term()}.
```

### wf_effect_stub Integration

A stub effect module provides governance integration:

- `new_spec/5`: Create effect specifications with cost tracking
- `yield/4`: Execute effects with allowlist and budget checks
- `get_effect_cost/1`: Retrieve effect cost

**Note**: This is a stub implementation. Real effect execution will be integrated in item 010.

## Usage Examples

### Example 1: E-commerce Workflow with Payment Approval

```erlang
%% Setup governance
wf_governance:set_allowlist(checkout_scope, [http_get, db_query, send_email]),
wf_governance:set_timeout_policy(case1, {case_timeout, 300000}),
wf_governance:require_approval(checkout_scope, process_payment, #{
    approval_id => approve_payment,
    timeout => 86400000,  %% 24 hours
    timeout_action => cancel,
    detail => <<"Manual approval required for payment processing">>
}),
wf_budget:init_budget(case1, [
    {max_effects, 50},
    {max_time_us, 60000000},
    {max_cost, 100}
]).

%% Execute workflow
%% ... workflow execution ...
%% When process_payment task is reached:
{ok, approved} = wf_approval:request_approval(process_payment, ApprovalSpec),
%% Continue with payment processing
```

### Example 2: Data Pipeline with Budget Limits

```erlang
%% Setup strict budget for ETL job
wf_budget:init_budget(etl_case, [
    {max_effects, 1000},
    {max_time_us, 3600000000},  %% 1 hour
    {max_cost, 5000}
]),
wf_governance:set_allowlist(etl_scope, [db_query, file_write, http_post]).

%% Execute ETL operations
lists:foreach(fun(Data) ->
    Spec = wf_effect_stub:new_spec(etl_case, N, etl_scope, db_query, Data),
    case wf_effect_stub:yield(etl_case, N, etl_scope, Spec) of
        {ok, Result} ->
            wf_budget:increment_effect_count(etl_case),
            process_result(Result);
        {error, #governance_error{error_type = budget_exceeded}} ->
            io:format("Budget exhausted, pausing ETL~n"),
            pause_etl()
    end
end, DataList).
```

## Best Practices

1. **Set allowlists explicitly**: Don't rely on default "allow all" behavior in production
2. **Configure reasonable budgets**: Start with generous limits and tighten based on usage
3. **Use approval gates for sensitive operations**: Payments, deletions, configuration changes
4. **Set timeouts at all levels**: Case, task, and effect timeouts for defense in depth
5. **Monitor governance violations**: All errors are structured and logged for analysis
6. **Test governance policies**: Use integration tests to verify enforcement behavior

## Performance Considerations

- **Budget checks**: O(1) map lookups, typically < 10 microseconds
- **Allowlist checks**: O(N) where N is allowlist size (typically small)
- **ETS tables**: Configured with read_concurrency for parallel access
- **gen_server serialization**: Budget mutations are serialized per case

## Migration Notes

### No Migration Required

This is new functionality with no existing data to migrate.

### Future Integration Points

When **item 010 (Effect Boundary and Receipts)** completes:
1. Replace `wf_effect_stub` with `wf_effect`
2. Move governance checks into real `wf_effect:yield/4`
3. Store governance errors as receipts via `wf_receipt`

When **item 008 (Cancellation Semantics)** completes:
1. Integrate timeout-triggered cancellation with `wf_cancel:cancel_case/1`
2. Use `wf_cancel` for approval timeout actions (cancel case on approval timeout)

When **item 006 (State Store)** completes:
1. Move budget tracking from ETS table to `wf_state` mutations
2. Use `wf_state:buffer_mutation/2` for budget updates
3. Persist budget state via `wf_state:commit/1`

## Limitations (Current Implementation)

1. **Policy versioning**: Policies cannot be updated during active case execution
2. **Allowlist inheritance**: No hierarchical model (each scope has explicit allowlist)
3. **Effect cost modeling**: Costs are explicit integers (no automatic calculation)
4. **Policy persistence**: Policies stored in ETS only (no disk persistence)
5. **Approval delegation**: No delegation or escalation workflows

## Testing

Comprehensive test coverage is provided:

- **Unit tests**: `test/wf_governance_tests.erl`
- **Integration tests**: `test/wf_governance_integration_tests.erl`
- **Property-based tests**: `test/wf_governance_props.erl`

Run tests with:
```bash
rebar3 eunit
rebar3 proper
```
