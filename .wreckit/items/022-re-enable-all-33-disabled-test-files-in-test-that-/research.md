# Research: Re-enable all 33 disabled test files in test/ that were moved to _build/test/lib/wf_substrate/test/disabled/. The previous fix disabled tests instead of fixing them. Move them back from disabled, fix their compilation errors (missing includes, wrong arities, undefined functions, missing records), and ensure rebar3 eunit runs ALL test files and they pass. Do NOT disable or delete any test — fix every single one.

**Date**: 2025-01-10
**Item**: 022-re-enable-all-33-disabled-test-files-in-test-that-

## Research Question

How do we re-enable all 33 disabled test files that were moved to `_build/test/lib/wf_substrate/test/disabled/` and fix all their compilation errors so that `rebar3 eunit` runs and passes all tests?

## Summary

The wf-substrate project has 33 test files that were disabled in a previous fix (item 021) instead of being repaired. These test files reside in `_build/test/lib/wf_substrate/test/disabled/` with the `.disabled` extension. The tests cover critical functionality including:

1. **Property-based tests** (3 files): wf_prop.erl, wf_governance_props.erl, wf_effect_props.erl
2. **Integration tests** (5 files): wf_case_runner_tests.erl, wf_trace_tests.erl, wf_exec_effect_tests.erl, wf_exec_integration_tests.erl, wf_governance_integration_tests.erl
3. **Unit tests** (14 files): wf_exec_tests.erl, wf_term_tests.erl, wf_core_tests.erl, wf_compile_tests.erl, wf_effect_tests.erl, wf_mi_tests.erl, wf_sched_tests.erl, wf_state_tests.erl, wf_governance_tests.erl, wf_cancel_tests.erl, wf_receipt_tests.erl, wf_validate_tests.erl, wf_substrate_api_tests.erl, wf_supervision_tree_tests.erl
4. **Legacy helper tests** (11 files): wf_test_*.erl (test patterns for seq, par, xor, join, mi, cancel, determinism, examples, receipts, replay, term, trace_helpers)

The tests need to be moved back to the `test/` directory, the `.disabled` extension removed, and compilation errors fixed. Common error patterns include: missing include files (using relative paths like `../src/` instead of module names), missing record definitions, undefined functions, and arity mismatches with current API.

## Current State Analysis

### Existing Implementation

**Test File Location:**
- **Current location**: `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/*.erl.disabled` (33 files)
- **Target location**: `/Users/speed/wf-substrate/test/*.erl` (source directory)
- **Current active test**: Only `wf_substrate_tests.erl` exists with a dummy test

**Test File Categories:**

1. **Property-based tests** (require PropEr integration):
   - `wf_prop.erl.disabled` - Custom property testing framework
   - `wf_governance_props.erl.disabled` - Governance properties
   - `wf_effect_props.erl.disabled` - Effect system properties

2. **Integration tests** (require gen_server setup/teardown):
   - `wf_case_runner_tests.erl.disabled`
   - `wf_trace_tests.erl.disabled`
   - `wf_exec_effect_tests.erl.disabled`
   - `wf_exec_integration_tests.erl.disabled`
   - `wf_governance_integration_tests.erl.disabled`

3. **Unit tests** (core module tests):
   - `wf_exec_tests.erl.disabled` - Bytecode executor tests
   - `wf_term_tests.erl.disabled` - Pattern algebra tests
   - `wf_core_tests.erl.disabled` - Derived pattern tests
   - `wf_compile_tests.erl.disabled` - Compiler tests
   - `wf_effect_tests.erl.disabled` - Effect system tests
   - `wf_mi_tests.erl.disabled` - Multiple instance tests
   - `wf_sched_tests.erl.disabled` - Scheduler tests
   - `wf_state_tests.erl.disabled` - State store tests
   - `wf_governance_tests.erl.disabled` - Governance tests
   - `wf_cancel_tests.erl.disabled` - Cancellation tests
   - `wf_receipt_tests.erl.disabled` - Receipt tests
   - `wf_validate_tests.erl.disabled` - Validation tests
   - `wf_substrate_api_tests.erl.disabled` - API tests
   - `wf_supervision_tree_tests.erl.disabled` - Supervision tests

4. **Legacy test helpers** (wf_test_*.erl.disabled):
   - `wf_test_helpers.erl.disabled` - Test helper functions
   - `wf_test_seq.erl.disabled` - Sequential pattern tests
   - `wf_test_par.erl.disabled` - Parallel pattern tests
   - `wf_test_xor.erl.disabled` - XOR choice tests
   - `wf_test_join.erl.disabled` - Join policy tests
   - `wf_test_mi.erl.disabled` - Multiple instance pattern tests
   - `wf_test_cancel.erl.disabled` - Cancellation pattern tests
   - `wf_test_determinism.erl.disabled` - Determinism tests
   - `wf_test_examples.erl.disabled` - Example workflow tests
   - `wf_test_receipts.erl.disabled` - Receipt pattern tests
   - `wf_test_replay.erl.disabled` - Replay tests
   - `wf_test_term.erl.disabled` - Term tests
   - `wf_test_trace_helpers.erl.disabled` - Trace helper tests

**Include File Patterns:**

The disabled tests use incorrect include paths:
- **Current pattern**: `-include("../src/wf_exec.hrl")` (relative path from _build)
- **Correct pattern**: `-include("wf_exec.hrl")` (include by module name)
- **Header files available**: wf_exec.hrl, wf_effect.hrl, wf_receipt.hrl, wf_governance.hrl, wf_mi.hrl, wf_validate.hrl, wf_trace.hrl

**Record Definitions Available:**

Located in `/Users/speed/wf-substrate/src/`:
- `wf_exec.hrl:7-44` - `token`, `branch_info`, `join_counter`, `exec_state` records
- `wf_effect.hrl:7-25` - `effect_spec`, `effect` records
- `wf_governance.hrl:7-40` - `governance_error` record and policy types
- `wf_mi.hrl:7-23` - `mi_instance`, `mi_config` records
- `wf_validate.hrl:7-29` - `validation_state`, `issue`, `report` records
- `wf_trace.hrl:7-29` - `trace_event`, `replay_entry`, `trace_state` records

## Key Files

### Source Modules (for API reference)
- `/Users/speed/wf-substrate/src/wf_exec.erl:1-100` - Executor API: `new/1`, `step/2`, `run/3`, `is_done/1`, `get_ip/1`, `get_ctx/1`
- `/Users/speed/wf-substrate/src/wf_term.erl:1-150` - Pattern algebra API: `task/2`, `seq/2`, `par/1`, `x_or/1`, `join/2`, `loop/2`, `cancel/2`, `mi/2`
- `/Users/speed/wf-substrate/src/wf_core.erl:1-150` - Derived patterns: `simple_merge/2`, `synchronizing_merge/2`, `discriminator/2`, `n_out_of_m/3`
- `/Users/speed/wf-substrate/src/wf_compile.erl` - Compiler API: `compile/1` (returns `{ok, Bytecode}`)
- `/Users/speed/wf-substrate/src/wf_state.erl:1-150` - State API: `new/1`, `get_ctx/1`, `put_ctx/2`, `get_tokens/1`, `add_token/3`, `commit/1`, `rollback/1`
- `/Users/speed/wf-substrate/src/wf_cancel.erl:1-150` - Cancellation API: `cancel_activity/3`, `cancel_case/1`, `cancel_region/3`, `is_cancelled/2`, `propagate/2`
- `/Users/speed/wf-substrate/src/wf_effect.erl:1-100` - Effect API: `new_spec/5`, `yield/4`, `start_link/0`
- `/Users/speed/wf-substrate/src/wf_receipt.erl:1-150` - Receipt API: `new/3`, `store/2`, `lookup/2`, `verify/2`
- `/Users/speed/wf-substrate/src/wf_governance.erl` - Governance API: `start_link/0`, `set_allowlist/2`, `set_budget/2`, `set_timeout_policy/2`, `require_approval/3`
- `/Users/speed/wf-substrate/src/wf_budget.erl` - Budget API: `init_budget/2`, `check_budget/1`, `increment_effect_count/1`
- `/Users/speed/wf-substrate/src/wf_approval.erl` - Approval API: `request_approval/2`, `signal/2`, `check_approval/1`
- `/Users/speed/wf-substrate/src/wf_validate.erl` - Validation API: `new/1`, `to_petri_net/1`, `enabled_transitions/1`
- `/Users/speed/wf-substrate/src/wf_sched_deterministic.erl` - Deterministic scheduler: `new/1`, `choose/2`
- `/Users/speed/wf-substrate/src/wf_sched_nondeterministic.erl` - Nondeterministic scheduler: `new/1`, `choose/2`
- `/Users/speed/wf-substrate/src/wf_mi.erl` - Multiple instances: `eval_instance_count/2`, `spawn_instances/4`, `check_join/1`, `collect_result/3`

### Disabled Test Files (representative samples)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_governance_integration_tests.erl.disabled:1-260` - Integration tests for governance, uses `wf_effect_stub:yield/4`, `wf_governance:set_allowlist/2`, includes `wf_governance.hrl`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_test_cancel.erl.disabled:1-100` - Cancellation pattern tests, includes `../src/wf_exec.hrl` (incorrect path)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_core_tests.erl.disabled:1-100` - Derived pattern tests, uses `wf_term:task/2`, `wf_core:simple_merge/2`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_effect_tests.erl.disabled:1-100` - Effect system tests, includes `../src/wf_effect.hrl`, `../src/wf_receipt.hrl` (incorrect paths)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_exec_tests.erl.disabled:1-150` - Executor tests, includes `../src/wf_exec.hrl` (incorrect path)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_term_tests.erl.disabled:1-100` - Pattern algebra tests, uses `wf_term:task/2`, `wf_term:seq/2`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_state_tests.erl.disabled:1-100` - State store tests, uses `wf_state:new/1`, `wf_state:get_ctx/1`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_cancel_tests.erl.disabled:1-150` - Cancellation tests, includes `wf_state.hrl`, `wf_cancel.hrl` (missing file)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_compile_tests.erl.disabled:1-100` - Compiler tests, uses `wf_compile:compile/1`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_mi_tests.erl.disabled:1-100` - Multiple instance tests, includes `../src/wf_mi.hrl` (incorrect path)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_sched_tests.erl.disabled:1-100` - Scheduler tests, uses `wf_sched_deterministic:new/1`, `wf_sched_deterministic:choose/2`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_validate_tests.erl.disabled:1-100` - Validation tests, includes `../src/wf_validate.hrl` (incorrect path)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_governance_tests.erl.disabled:1-341` - Governance unit tests, uses `wf_governance:start_link/0`, `wf_budget:init_budget/2`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_prop.erl.disabled:1-90` - Property testing framework, uses `wf_vm:wf_bc()` type
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_governance_props.erl.disabled:1-100` - Governance properties, uses PropEr `?FORALL` macro
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_effect_props.erl.disabled:1-100` - Effect properties, uses custom `wf_prop:quickcheck/2`
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_test_helpers.erl.disabled:1-62` - Test helper functions, includes `../src/wf_exec.hrl`, `../src/wf_trace.hrl` (incorrect paths)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_test_join.erl.disabled:1-100` - Join policy tests, includes `../src/wf_exec.hrl` (incorrect path)
- `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_test_par.erl.disabled:1-100` - Parallel pattern tests, includes `../src/wf_exec.hrl` (incorrect path)

### Configuration Files
- `/Users/speed/wf-substrate/rebar.config:1-35` - Rebar3 configuration, test profile includes `{erl_opts, [debug_info, {require_min_otp_vsn, 26}]}`, `{deps, [proper]}`
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:1-19` - Currently active test (dummy test only)

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- **wf_exec** - Bytecode execution engine (tests use `wf_exec:new/1`, `wf_exec:step/2`, `wf_exec:run/3`)
- **wf_term** - Pattern algebra (tests use constructors like `wf_term:task/2`, `wf_term:seq/2`)
- **wf_core** - Derived patterns (tests use `wf_core:simple_merge/2`, `wf_core:synchronizing_merge/2`)
- **wf_compile** - Term-to-bytecode compiler (tests use `wf_compile:compile/1`)
- **wf_state** - Transactional state store (tests use `wf_state:new/1`, `wf_state:commit/1`)
- **wf_cancel** - Cancellation semantics (tests use `wf_cancel:is_cancelled/2`, `wf_cancel:propagate/2`)
- **wf_effect** - Effect system boundary (tests use `wf_effect:new_spec/5`, `wf_effect:yield/4`)
- **wf_receipt** - Receipt storage (tests use `wf_receipt:new/3`, `wf_receipt:store/2`)
- **wf_governance** - Governance policies (tests use `wf_governance:start_link/0`, `wf_governance:set_allowlist/2`)
- **wf_budget** - Budget enforcement (tests use `wf_budget:init_budget/2`, `wf_budget:check_budget/1`)
- **wf_approval** - Approval gates (tests use `wf_approval:request_approval/2`, `wf_approval:signal/2`)
- **wf_validate** - Bounded model checking (tests use `wf_validate:new/1`, `wf_validate:enabled_transitions/1`)
- **wf_sched_deterministic** - Deterministic scheduling (tests use `wf_sched_deterministic:new/1`, `wf_sched_deterministic:choose/2`)
- **wf_sched_nondeterministic** - Nondeterministic scheduling (tests use `wf_sched_nondeterministic:new/1`, `wf_sched_nondeterministic:choose/2`)
- **wf_mi** - Multiple instances (tests use `wf_mi:eval_instance_count/2`, `wf_mi:spawn_instances/4`)
- **wf_vm** - Virtual machine opcodes and types (tests use `wf_vm:wf_bc()` type)

**External dependencies:**
- **eunit** - Built-in Erlang unit testing framework (already configured)
- **proper** - PropEr property-based testing framework (already in test deps)

### Patterns to Follow

**Include file patterns:**
- **Current (incorrect)**: `-include("../src/wf_exec.hrl")`
- **Correct**: `-include("wf_exec.hrl")` (include by module name, rebar3 handles include path)

**Test setup/teardown patterns:**
- **Gen_server tests**: Use `{setup, fun setup/0, fun cleanup/1, [TestFun]}`
- **Example**: `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_governance_tests.erl.disabled:34-42`

**Test assertion patterns:**
- **EUnit assertions**: `?assertEqual(Expected, Actual)`, `?assertMatch(Pattern, Actual)`, `?assert(Condition)`
- **Record field access**: Use `#record_name.field` syntax (e.g., `ExecState#exec_state.tokens`)
- **Example**: `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_state_tests.erl.disabled:56-62`

**Mock data patterns:**
- **Bytecode generation**: Create list of opcode tuples like `[{'TASK_EXEC', task}, {'DONE'}]`
- **Task metadata**: Use `#{function => fun(_Ctx) -> {ok, Ctx} end}` for mock tasks
- **Example**: `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_compile_tests.erl.disabled:14-20`

**Property testing patterns:**
- **PropEr**: Use `?FORALL(Generator, Property)` syntax from `proper.hrl`
- **Custom**: Use `wf_prop:quickcheck(Generator, Property)` for custom framework
- **Example**: `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_governance_props.erl.disabled:27-37`

### Common Compilation Error Patterns

1. **Missing include files**:
   - Error: `Include file not found ../src/wf_exec.hrl`
   - Fix: Change to `-include("wf_exec.hrl")`

2. **Missing record definitions**:
   - Error: `Record token undefined`
   - Fix: Add `-include("wf_exec.hrl")` or other appropriate header

3. **Undefined functions**:
   - Error: `function wf_exec:snapshot_exec_state/1 undefined`
   - Fix: Implement missing function or use correct API

4. **Wrong arities**:
   - Error: `function wf_term:raw_task/2 undefined`
   - Fix: Update to match current API (e.g., `wf_term:task/2`)

5. **Missing header files**:
   - Error: `wf_cancel.hrl not found`
   - Fix: Create header file or include records directly from wf_cancel.erl

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Tests rely on undefined functions/modules | High | Audit each test file for API calls against current source modules, implement missing functions or update test calls |
| Missing header files (wf_cancel.hrl, wf_state.hrl) | Medium | Create missing header files from record definitions in source modules, or include records directly in test files |
| Property tests require PropEr integration | Medium | PropEr already in test deps, ensure proper `-include_lib("proper/include/proper.hrl")` |
| Tests depend on stub modules (wf_effect_stub) | Medium | Verify wf_effect_stub exists and implements required API (`new_spec/5`, `yield/4`) |
| Gen_server lifecycle issues in integration tests | Medium | Ensure proper setup/teardown, handle already-running processes, cleanup state between tests |
| Legacy test helpers have incorrect API assumptions | Medium | Update helper functions to match current module APIs, remove obsolete helpers |
| Test interdependencies (helpers used by multiple tests) | Low | Move helper functions to separate module (e.g., wf_test_helpers.erl), export helper functions |
| OTP version compatibility | Low | Ensure tests run on OTP 26+ (as specified in rebar.config) |
| Timeout issues in approval/effect tests | Low | Adjust timeouts or use async patterns with proper synchronization |
| Determinism assumptions in scheduler tests | Low | Use fixed seeds for nondeterministic scheduler tests, verify ordering assumptions |

## Recommended Approach

### Phase 1: Preparation and Infrastructure

1. **Create missing header files**:
   - Create `wf_cancel.hrl` with `cancel_activity`, `cancel_case`, `cancel_region` records from `/Users/speed/wf-substrate/src/wf_cancel.erl:86-108`
   - Create `wf_state.hrl` with `token`, `scope`, `metadata`, `mutation`, `receipt`, `state` records from `/Users/speed/wf-substrate/src/wf_state.erl:77-129`
   - Verify all existing header files are in include path

2. **Verify wf_effect_stub module**:
   - Check if `/Users/speed/wf-substrate/src/wf_effect_stub.erl` exists
   - Verify it exports `new_spec/5` and `yield/4`
   - If missing, implement stub for testing

### Phase 2: Move and Fix Test Files (Batch 1 - Core Tests)

3. **Move unit tests to test/ directory**:
   ```bash
   cd /Users/speed/wf-substrate
   for file in _build/test/lib/wf_substrate/test/disabled/*.erl.disabled; do
       basename=$(basename "$file" .disabled)
       cp "$file" "test/$basename"
   done
   ```

4. **Fix include paths** (all test files):
   - Replace `-include("../src/wf_exec.hrl")` with `-include("wf_exec.hrl")`
   - Replace `-include("../src/wf_effect.hrl")` with `-include("wf_effect.hrl")`
   - Replace `-include("../src/wf_receipt.hrl")` with `-include("wf_receipt.hrl")`
   - Replace `-include("../src/wf_governance.hrl")` with `-include("wf_governance.hrl")`
   - Replace `-include("../src/wf_mi.hrl")` with `-include("wf_mi.hrl")`
   - Replace `-include("../src/wf_validate.hrl")` with `-include("wf_validate.hrl")`
   - Replace `-include("../src/wf_trace.hrl")` with `-include("wf_trace.hrl")`
   - Add `-include("wf_cancel.hrl")` and `-include("wf_state.hrl")` where needed

### Phase 3: Fix API Calls (Batch 2 - API Corrections)

5. **Fix term constructor calls**:
   - Update `wf_term:raw_task/2` to `wf_term:task/2`
   - Update `wf_term:raw_seq/2` to `wf_term:seq/2`
   - Update `wf_term:raw_par/1` to `wf_term:par/1`
   - Update `wf_term:raw_x_or/1` to `wf_term:x_or/1`
   - Verify all constructors match current API in `/Users/speed/wf-substrate/src/wf_term.erl`

6. **Fix executor calls**:
   - Verify `wf_exec:new/1`, `wf_exec:step/2`, `wf_exec:run/3` exist
   - Check if `wf_exec:cancel/1` is implemented (referenced in wf_test_cancel.erl.disabled:84)
   - Implement missing functions if needed

7. **Fix state management calls**:
   - Verify `wf_state:new/1`, `wf_state:put_ctx/2`, `wf_state:commit/1` exist
   - Check record field access patterns (e.g., `element(7, State)` for buffered_mutations)
   - Update to use proper accessor functions if available

8. **Fix compiler calls**:
   - Verify `wf_compile:compile/1` returns `{ok, Bytecode}`
   - Update opcode tuple format from uppercase atoms (`{'TASK_EXEC', task}`) to lowercase (`{task_exec, task}`)
   - Match current bytecode format from wf_vm module

### Phase 4: Fix Property Tests (Batch 3 - PropEr Integration)

9. **Fix PropEr includes and macros**:
   - Ensure `-include_lib("proper/include/proper.hrl")` is present
   - Verify `?FORALL`, `?ASSERT` macros are used correctly
   - Check custom property framework (`wf_prop`) compatibility

10. **Fix property generators**:
    - Verify generators produce valid input types
    - Ensure generators don't exceed resource limits (e.g., max instances = 1000)
    - Update to use current API in generator functions

### Phase 5: Fix Integration Tests (Batch 4 - Gen_server Lifecycle)

11. **Fix gen_server setup/teardown**:
    - Ensure proper start_link and stop sequences
    - Handle already-running processes (use `whereis` to check)
    - Clean up ETS tables and process state between tests

12. **Fix governance and approval tests**:
    - Verify `wf_governance:start_link/0`, `wf_budget:start_link/0`, `wf_approval:start_link/0` work
    - Check timeout values (approval tests use 100ms timeout in `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_governance_tests.erl.disabled:294`)
    - Ensure proper signal/approval flow

### Phase 6: Fix Legacy Test Helpers (Batch 5 - Helper Modules)

13. **Fix wf_test_helpers.erl**:
    - Update include paths
    - Export helper functions: `exec_until_done/1`, `exec_steps/2`, `get_token_statuses/1`, `count_tokens_with_status/2`, `assert_join_counter/2`
    - Verify API calls match current modules

14. **Fix wf_test_*.erl modules**:
    - Update exports to match function definitions
    - Fix include paths
    - Ensure helper functions are accessible

### Phase 7: Compilation and Execution

15. **Compile and fix errors iteratively**:
    ```bash
    rebar3 compile
    # Fix compilation errors one by one
    # Re-compile until clean
    ```

16. **Run tests and fix failures**:
    ```bash
    rebar3 eunit
    # Fix test failures by updating assertions or implementation
    # Re-run until all tests pass
    ```

### Phase 8: Verification

17. **Verify all tests run**:
    ```bash
    rebar3 eunit
    # Should run all 33 test files
    # Should report 0 failures
    ```

18. **Verify coverage**:
    ```bash
    rebar3 cover
    # Ensure adequate test coverage
    ```

## Open Questions

1. **wf_effect_stub module**: Does `wf_effect_stub` exist in `/Users/speed/wf-substrate/src/`? If not, should it be created or should tests use `wf_effect` directly?

2. **wf_exec:cancel/1**: Is this function implemented? Referenced in `/Users/speed/wf-substrate/_build/test/lib/wf_substrate/test/disabled/wf_test_cancel.erl.disabled:84` but not seen in initial scan of wf_exec.erl.

3. **Opcode format**: Should tests use uppercase atoms (`{'TASK_EXEC', task}`) or lowercase atoms (`{task_exec, task}`)? Current source uses lowercase, but tests use uppercase.

4. **wf_state field access**: Tests use `element(7, State2)` to access `buffered_mutations` field. Should this be replaced with a proper accessor function?

5. **PropEr integration**: Are there any PropEr-specific configuration needs beyond adding it to test deps?

6. **Test execution order**: Do any tests have interdependencies that require specific execution order?

7. **Timeout values**: Integration tests use hardcoded timeouts (e.g., 100ms for approval). Are these appropriate for CI/CD environments?

8. **Determinism in scheduler tests**: Tests assume specific ordering for deterministic scheduler. Is this guaranteed by the implementation?

9. **Resource limits**: Property tests have hardcoded limits (e.g., max 1000 instances). Should these be configurable?

10. **Legacy test helpers**: Should wf_test_*.erl files be consolidated or refactored to use common helper module?

11. **wf_cancel.hrl and wf_state.hrl**: Should these header files be created in src/ directory, or should records be included directly in test files?

12. **Trace event formats**: Tests reference trace events with specific formats. Do these match current wf_trace implementation?
