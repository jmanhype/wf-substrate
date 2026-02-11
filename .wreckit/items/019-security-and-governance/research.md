# Research: Implement security and governance features

**Date**: 2025-01-11
**Item**: 019-security-and-governance

## Research Question

Implement governance and safety features for production use:

1. Tool/effect allowlist per scope: each cancel scope (or the top-level case) can define an allowlist of permitted effect types. Effects not on the allowlist are rejected with a structured error. Implemented in wf_effect.erl as a check before yielding.

2. Maximum effect budget per case: configurable limits on effect count (max N effects per case), total effect time (max T microseconds of effect execution), and cost stub (each effect type has an integer cost, total cost capped at C). Budget tracking in wf_state.erl, enforcement in wf_effect.erl. Budget exhaustion produces a structured budget_exceeded error.

3. Timeout policies: per-case timeout (overall wall time), per-task timeout (individual task execution time), per-effect timeout (effect response time). Timeouts produce structured timeout events and can trigger cancellation. Implemented via gen_statem state_timeout and erlang:send_after.

4. Mandatory approval points: a task can be marked as requiring explicit external signal before proceeding (approval gate). The executor yields {waiting_approval, ApprovalSpec} and blocks until signal/2 is called with matching approval. Timeout on approval triggers configurable action (cancel, default, escalate).

5. Guard failure handling: structured error records for all governance violations. Each error includes: error_type, scope, task_id, detail, timestamp. Errors are recorded as receipts. Guards never silently pass — they either allow or produce a structured rejection.

Export: set_allowlist/2, set_budget/2, check_budget/1, set_timeout_policy/2, require_approval/2.

## Summary

The security and governance features require implementing three new modules (wf_governance.erl, wf_approval.erl, wf_budget.erl) and extending two existing modules (wf_effect.erl for allowlist enforcement, wf_state.erl for budget tracking). The implementation follows existing OTP patterns (gen_server for state management, ETS tables for storage, structured records for errors/receipts). Key integration points are wf_exec:execute_task_exec/2 (for approval yielding), wf_effect:yield/4 (for allowlist checks), and wf_state mutations (for budget tracking and timeout policies).

The codebase has well-established patterns for:
- ETS-based storage (wf_trace, planned wf_receipt from item 010)
- gen_server behaviors (wf_trace, planned wf_effect/wf_receipt from item 010)
- Structured error records (wf_validate #issue{}, wf_trace events)
- Receipt/mutation patterns (wf_state commit protocol)
- Scope tracking (wf_exec scope_stack, wf_state scopes)

## Current State Analysis

### Existing Implementation

**Executor State Management** (`wf_exec.erl`):
- `#exec_state{}` record (lines 54-65) has inline state with tokens, scope_stack, ctx, step_count
- `execute_task_exec/2` (lines 232-239) is currently a mock that always returns ok
- No governance checks exist in task execution path
- No approval yielding mechanism exists
- Scope tracking via scope_stack (line 61) supports per-scope policies

**State Store** (`wf_state.erl` - from item 010 plan, lines referenced but file may not exist yet):
- `#state{}` record has metadata field (line 60 in plan)
- Mutation protocol for atomic updates (lines 485-545 in plan)
- Scope tracking via `#scope{}` record (lines 83-90 in plan)
- No budget tracking fields exist yet
- No timeout policy storage exists yet

**Effect System** (planned in item 010):
- `#effect_spec{}` record (lines 225-232 in plan) has effect_type field for allowlist checking
- `yield/4` function (line 716 in plan) is integration point for allowlist enforcement
- No allowlist mechanism exists
- No budget checking exists

**Tracing/Receipts** (`wf_trace.erl`):
- `#trace_event{}` record (lines 10-19) supports metadata field for governance events
- ETS table storage pattern (lines 82-93) can be reused for budget/approval state
- No governance-specific event types exist

**Validation** (`wf_validate.erl`):
- `#issue{}` record (lines 16-21) demonstrates structured error pattern
- Error types include dead_transition, livelock, deadlock, improper_completion, unsound
- No governance error types exist yet

## Key Files

- `/Users/speed/wf-substrate/src/wf_exec.erl:54-65` - exec_state record (needs integration for approval yielding)
- `/Users/speed/wf-substrate/src/wf_exec.erl:232-239` - execute_task_exec/2 (needs governance checks)
- `/Users/speed/wf-substrate/src/wf_exec.erl:28-34` - #token{} record (status field needs blocked_approval)
- `/Users/speed/wf-substrate/src/wf_vm.erl:9-49` - Type definitions (governance opcodes needed)
- `/Users/speed/wf-substrate/src/wf_trace.erl:10-19` - #trace_event{} record (pattern for governance events)
- `/Users/speed/wf-substrate/src/wf_trace.erl:82-93` - ETS table initialization pattern
- `/Users/speed/wf-substrate/src/wf_validate.erl:16-21` - #issue{} record (pattern for governance errors)
- `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/plan.md:225-232` - #effect_spec{} record definition
- `/Users/speed/wf-substrate/.wreckit/items/010-effect-boundary-and-receipts/plan.md:716-735` - wf_effect:yield/4 integration point
- `/Users/speed/wf-substrate/.wreckit/items/008-cancellation-semantics/plan.md:186-200` - wf_state #state{} record pattern

## Technical Considerations

### Dependencies

**Required (item 010 - Effect Boundary and Receipts)**:
- `wf_effect.erl` module must exist for allowlist enforcement
- `wf_effect:#effect_spec{}` record for effect_type checking
- `wf_effect:yield/4` for allowlist integration point
- `wf_receipt.erl` module for storing governance errors as receipts

**Required (item 008 - Cancellation Semantics)**:
- `wf_cancel.erl` module for timeout-triggered cancellation
- `wf_cancel:cancel_case/1` for case timeout handling
- `wf_state` scope tracking for per-scope policies

**Optional but beneficial**:
- `wf_trace` integration for governance event logging
- `wf_validate` integration for static policy checking

### Patterns to Follow

**ETS Table Ownership** (from wf_trace.erl:82-93):
```erlang
init([]) ->
    Table = ets:new(wf_governance_policies, [
        named_table,
        set,
        public,
        {read_concurrency, true}
    ]),
    {ok, #{table => Table}}.
```

**Structured Error Records** (from wf_validate.erl:16-21):
```erlang
-record(governance_error, {
    error_type :: allowlist_violation | budget_exceeded | timeout | approval_required,
    scope :: term(),
    task_id :: term() | undefined,
    detail :: binary(),
    timestamp :: erlang:timestamp()
}).
```

**gen_server Behavior** (from wf_trace.erl:283-322):
- Use gen_server for budget/approval state management
- Implement handle_call/handle_cast/handle_info for policy updates
- Terminate callback to clean up ETS tables

**Receipt/Mutation Pattern** (from wf_state commit protocol):
- Use wf_state mutations for atomic budget updates
- Create receipts for governance violations
- Buffer mutations before commit

**Scope Stack Pattern** (from wf_exec.erl:61):
- Per-scope policies integrate with existing scope_stack
- Scope ID from scope_stack maps to governance policies

## Risks and Mitigations

| Risk     | Impact            | Mitigation       |
| -------- | ----------------- | ---------------- |
| wf_effect not implemented (item 010 incomplete) | High - cannot implement allowlist | Implement stub wf_effect with allowlist hook, integrate later |
| wf_state budget tracking conflicts with item 010 state mutations | Medium - schema changes may conflict | Coordinate with item 010, use separate budget table in v1 |
| Approval yielding blocks executor indefinitely | High - deadlock if signal never arrives | Implement timeout on approval with cancel/default/escalate actions |
| Timeout cancellation races with executor steps | Medium - inconsistent state | Use gen_statem state_timeout, handle timeout events in executor state machine |
| Budget tracking overhead on every effect | Medium - performance degradation | Use ETS with read_concurrency, budget checks are O(1) map lookups |
| Policy updates during active case | Low - inconsistent enforcement | Policy versioning, or require case restart for policy changes |
| Scope hierarchy for allowlists | Medium - unclear inheritance model | Simple model: explicit allowlist per scope, no inheritance in v1 |
| Budget exhaustion mid-effect | Medium - effect already running | Check budget BEFORE yielding effect, not after completion |

## Recommended Approach

**High-Level Strategy**: Implement governance features in three phases: (1) Core infrastructure (error records, policy storage), (2) Enforcement mechanisms (allowlist, budget, timeouts), (3) Approval system. Start with stubs for missing dependencies (wf_effect, wf_state budget fields), integrate later when items 008/010 complete.

**Architectural Decisions**:

1. **Module Structure**: Create three new modules (wf_governance, wf_budget, wf_approval) instead of single monolithic module. Separation of concerns: wf_governance (policy management), wf_budget (budget tracking), wf_approval (approval gates).

2. **Policy Storage**: Use ETS table (wf_governance_policies) mapping scope_id to policy records. Separate tables for budgets, allowlists, timeout policies. gen_server ownership for crash resilience.

3. **Error Records**: Create `#governance_error{}` record with error_type field (allowlist_violation, budget_exceeded, timeout, approval_required). Include scope, task_id, detail, timestamp. Store as receipts for audit trail.

4. **Allowlist Enforcement**: Check allowlist in wf_effect:yield/4 BEFORE executing effect. Return {error, #governance_error{}} if effect_type not in allowlist. Per-scope allowlists stored in wf_governance_policies ETS table.

5. **Budget Tracking**: Add budget fields to wf_state #state{} record (effect_count, effect_time_us, total_cost). Check budget in wf_effect:yield/4 before yielding. On budget exhaustion, return {error, #governance_error{}} and trigger cancellation.

6. **Timeout Implementation**:
   - **Per-case timeout**: Use gen_statem state_timeout in case runner process (item 012)
   - **Per-task timeout**: Use erlang:send_after in execute_task_exec/2, handle timeout message
   - **Per-effect timeout**: Add timeout field to #effect_spec{}, enforced in wf_effect execution

7. **Approval Yielding**: Add new token status `blocked_approval` to #token{} record. Add new opcode `{'APPROVAL_WAIT', ApprovalSpec}`. executor yields {waiting_approval, ApprovalSpec} and blocks. External signal/2 resumes execution.

8. **Integration Points**:
   - **wf_effect:yield/4**: Add allowlist check and budget check at entry point
   - **wf_exec:execute_task_exec/2**: Add approval yielding support, handle APPROVAL_WAIT opcode
   - **wf_state mutations**: Add budget mutation types ({increment_effect_count, {add_effect_time, Time}, {add_effect_cost, Cost})

9. **Dependency Stubs**:
   - If wf_effect not implemented, create stub module with yield/4 that performs checks
   - If wf_state budget fields not implemented, use separate ETS table for budget tracking
   - Integrate with real modules when items 008/010 complete

10. **Testing Strategy**:
   - Unit tests for each governance check (allowlist, budget, timeout, approval)
   - Integration tests for wf_effect enforcement
   - End-to-end tests for approval flows
   - Property-based tests for budget invariants (never exceed limit)
   - Performance tests for budget checking overhead

## Open Questions

1. **wf_effect Availability**: Item 010 (effect boundary) is not yet complete. Should we implement wf_effect stub module now, or coordinate implementation timing?

2. **wf_state Budget Storage**: Should budget fields be added to wf_state #state{} record (requires coordination with item 010) or stored in separate ETS table (isolated implementation)?

3. **Approval Timeout Actions**: When approval timeout expires, what are the exact semantics for "cancel" vs "default" vs "escalate"? Cancel = terminate case? Default = proceed with default value? Escalate = notify supervisor?

4. **Scope Hierarchy**: If scope1 has allowlist [http_get] and nested scope2 has allowlist [file_write], does scope2 inherit scope1's allowlist? Recommendation: no inheritance in v1, explicit allowlists only.

5. **Budget Exhaustion Action**: When budget exhausted, should case immediately cancel, or should current effect complete then stop? Recommendation: check budget BEFORE yielding effect, fail fast.

6. **Policy Update Semantics**: Can policies be updated while a case is running? If yes, how do we handle in-flight effects? Recommendation: policies immutable during case execution, updates apply to new cases only.

7. **Timeout Granularity**: Are timeouts measured in wall-clock time or CPU time? Recommendation: wall-clock time (erlang:monotonic_time/1) for consistency with effect execution tracking.

8. **Approval Signal Routing**: When multiple tokens are blocked on approval, how does signal/2 route to specific token? Recommendation: ApprovalSpec includes approval_id, signal/2 routes by approval_id.
