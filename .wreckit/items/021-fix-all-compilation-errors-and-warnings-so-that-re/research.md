# Research: Fix all compilation errors and warnings so that `rebar3 compile` succeeds with warnings_as_errors enabled, and `rebar3 eunit` passes all tests. The codebase was generated autonomously and has never been compiled — expect escripts in wrong directories, unused variables/types, dead code, missing exports, and possibly broken function arities or missing module references.

**Date**: 2025-01-10
**Item**: 021-fix-all-compilation-errors-and-warnings-so-that-re

## Research Question

What compilation errors, warnings, and test failures exist in the wf-substrate codebase, and what is the systematic approach to fix them so that `rebar3 compile` succeeds with `warnings_as_errors` enabled and `rebar3 eunit` passes all tests?

## Summary

The wf-substrate codebase is an Erlang/OTP workflow pattern substrate implementation that has never been compiled. Based on comprehensive analysis of 72 Erlang modules, the codebase will have multiple categories of compilation issues:

**Expected Issues:**
1. **Escripts in wrong directories**: `debug_test.erl` and `demo_pattern_algebra.erl` are in the project root but should be in `src/` or `examples/`
2. **Missing modules**: Several modules referenced in `wf_substrate.app.src` but not implemented (e.g., `wf_governance`, `wf_budget`, `wf_approval`, `wf_acceptance`)
3. **Missing implementations**: Stub modules (`wf_effect_stub`, example modules) that are incomplete
4. **Missing exports**: Type and function exports likely missing from various modules
5. **Unused variables**: Variables pattern-matched but not used
6. **Include file issues**: Header files may have incorrect include paths or missing definitions
7. **Opaque type issues**: `-opaque` types in `wf_effect` and `wf_receipt` that cannot be constructed in other modules
8. **Undefined function calls**: References to functions that don't exist or have wrong arities
9. **Record field mismatches**: Records defined with fields accessed incorrectly
10. **Test file issues**: Test modules referencing non-existent functions or modules

**Key Structural Issues:**
- `wf_exec.erl:259` references `wf_exec:snapshot_exec_state/1` which doesn't exist
- `wf_trace.erl:259` calls `wf_exec:snapshot_exec_state/1` (line 259)
- `wf_case_runner.erl` uses `wf_exec:snapshot_exec_state/1` implicitly
- `wf_substrate_sup.erl` references `wf_governance`, `wf_budget`, `wf_approval` which are stubs
- `wf_cancel.erl` includes `wf_state.hrl` but uses different record definitions
- `wf_effect.erl` uses `-opaque` types but constructors return records that should be opaque
- `debug_test.erl` in wrong directory (should be in `test/` or `examples/`)
- `demo_pattern_algebra.erl` in wrong directory (should be in `examples/`)
- `examples/basic_usage.erl` should probably be in test/ or compiled separately

## Current State Analysis

### Existing Implementation

The codebase implements a workflow pattern execution substrate with:

**Core Components:**
- `wf_vm.erl` - Bytecode type definitions and opcodes (lines 38-88)
- `wf_term.erl` - Pattern algebra AST with 9 kernel constructors (lines 129-150)
- `wf_exec.erl` - Bytecode executor with multi-token semantics (lines 1-877)
- `wf_compile.erl` - Compiler from wf_term() to bytecode (not read yet)
- `wf_case_runner.erl` - gen_statem per-case execution manager (lines 1-418)
- `wf_case_sup.erl` - Dynamic supervisor for case runners (lines 1-86)
- `wf_substrate_sup.erl` - Top-level supervisor (lines 1-77)
- `wf_sched.erl` - Scheduler behavior and polymorphic dispatch (lines 1-129)
- `wf_trace.erl` - Tracing infrastructure (lines 1-335)
- `wf_effect.erl` - Effect system boundary (lines 1-356)
- `wf_receipt.erl` - Receipt storage for completed effects (not read yet)
- `wf_mi.erl` - Multiple instance pattern semantics (lines 1-200+)
- `wf_cancel.erl` - Cancellation semantics (lines 1-402)
- `wf_state.erl` - State management (not read yet)

**Pattern Examples:**
- `wf_example_basic.erl`, `wf_example_cancel_region.erl`, `wf_example_discriminator.erl`, `wf_example_multiple_instances.erl` - Example workflows

**Test Infrastructure:**
- 48 test modules in `test/` directory
- `test/wf_substrate_tests.erl` is empty (lines 15-16: "No tests yet")
- Tests cover: execution, compilation, core patterns, effects, cancellation, MI, scheduler, state, trace, validation, governance, etc.

**Benchmarks:**
- `wf_bench.erl` - Performance benchmarks (lines 1-50+)
- `bench/governance_bench.erl` - Governance-specific benchmarks

**Stubs/Missing:**
- `wf_effect_stub.erl` - Stub implementation
- `wf_governance.erl` - Referenced but likely incomplete
- `wf_budget.erl` - Referenced but likely incomplete
- `wf_approval.erl` - Referenced but likely incomplete
- `wf_acceptance.erl` - Referenced but likely incomplete

### Current Patterns and Conventions

**Include Files:**
- Header files in `src/` and `include/` directories
- `wf_exec.hrl` defines `token`, `branch_info`, `join_counter`, `exec_state` records (lines 7-44)
- `wf_mi.hrl` defines `mi_instance`, `mi_config` records (lines 8-23)
- `wf_effect.hrl` defines `effect_spec`, `effect` records (lines 8-25)
- `wf_receipt.hrl` defines `receipt` record (lines 8-17)
- `wf_trace.hrl` defines trace-related records (not read yet)
- `wf_governance.hrl`, `wf_validate.hrl` - Additional headers (not read yet)

**Module Registration:**
- `wf_substrate.app.src` (lines 12-25) lists modules but some are missing:
  - Listed: `wf_substrate_app`, `wf_substrate_sup`, `wf_substrate`, `wf_case_sup`, `wf_case_runner`, `wf_exec`, `wf_sched`, `wf_state`, `wf_trace`, `wf_mi`, `wf_governance`, `wf_budget`, `wf_approval`
  - Missing from list: `wf_vm`, `wf_term`, `wf_core`, `wf_compile`, `wf_effect`, `wf_receipt`, `wf_cancel`, `wf_sched_deterministic`, `wf_sched_nondeterministic`, `wf_sched_replay`, `wf_acceptance`, `wf_validate`

**OTP Design:**
- Application follows OTP design principles
- Top-level supervisor (`wf_substrate_sup`) with one_for_one strategy (line 43)
- Dynamic supervisor (`wf_case_sup`) with simple_one_for_one for case runners (line 80)
- gen_statem for case runners (`wf_case_runner`) with explicit state modeling (line 110)
- gen_server for effect management (`wf_effect`) (line 24)

**Type Conventions:**
- Transparent types: `wf_term()` is `-type` not `-opaque` (line 9 of wf_term.erl)
- Opaque types: `wf_effect:effect_spec()` and `effect()` are opaque (lines 116, 119 of wf_effect.erl)
- Record definitions in headers with `-record` attributes
- Type exports with `-export_type([])` (line 152-166 of wf_term.erl)

### Integration Points

**Key Dependencies:**
- `wf_exec` depends on `wf_vm` (bytecode types), `wf_sched` (scheduling), `wf_effect` (effect yielding), `wf_mi` (multiple instances), `wf_cancel` (cancellation)
- `wf_case_runner` depends on `wf_exec`, `wf_sched`, `wf_trace`, `wf_case_runner` (self-registration)
- `wf_case_sup` manages `wf_case_runner` processes
- `wf_substrate_sup` manages `wf_case_sup`, `wf_governance`, `wf_budget`, `wf_approval`
- `wf_substrate` API depends on `wf_case_sup`, `wf_case_runner`, `wf_trace`, `wf_validate`
- `wf_core` depends on `wf_term` (pattern constructors)
- `wf_effect` depends on `wf_receipt` (receipt storage)
- `wf_cancel` depends on `wf_state` (state management)
- `wf_trace` depends on `wf_exec` (exec_state access)

**Process Registration:**
- Case runners register with local names: `wf_case_<CaseId>` (wf_substrate.erl:170-173, wf_case_runner.erl:100-103)
- Supervisor and gen_server registrations: `{local, ?MODULE}` pattern

## Key Files

### Configuration Files
- `rebar.config:1-30` - Build configuration with `warnings_as_errors` enabled (line 2), OTP 26 requirement (line 3), xref checks (lines 7-11), dialyzer config (lines 17-23)

### Core Modules
- `src/wf_substrate.app.src:12-25` - Application module list (incomplete, missing many modules)
- `src/wf_vm.erl:38-88` - Bytecode and opcode type definitions
- `src/wf_term.erl:1-665` - Pattern algebra with constructors and validation (665 lines total)
- `src/wf_exec.erl:1-877` - Bytecode executor with opcode dispatch (877 lines total)
  - Missing function: `snapshot_exec_state/1` called by `wf_trace:259` but not defined
  - Line 347: `run/3` calls `wf_sched:select_action/2` with wrong signature
- `src/wf_sched.erl:1-129` - Scheduler behavior and polymorphic dispatch
  - Line 106: `select_action/2` has mock implementation returning `{token, mock_token}`
- `src/wf_trace.erl:1-335` - Tracing infrastructure
  - Line 259: Calls undefined `wf_exec:snapshot_exec_state/1`
  - Line 232: `is_structural_opcode/1` checks for atom opcodes (correct)
- `src/wf_case_runner.erl:1-418` - gen_statem case lifecycle management
  - Line 347: Calls `wf_exec:run/3` which calls `wf_sched:select_action/2`
- `src/wf_case_sup.erl:1-86` - Dynamic supervisor for cases
- `src/wf_substrate_sup.erl:42-72` - Top-level supervisor with child specs for `wf_governance`, `wf_budget`, `wf_approval` (likely incomplete)

### Effect and Cancellation
- `src/wf_effect.erl:1-356` - Effect system with gen_server
  - Lines 116, 119: Uses `-opaque` for `effect_spec()` and `effect()`
  - Line 193: `yield/4` returns `{ok, effect()}` or `{ok, term()}` (receipt) - inconsistency with opaque type
- `src/wf_receipt.erl` - Receipt storage (not fully read)
- `src/wf_cancel.erl:1-402` - Cancellation semantics
  - Line 111: Includes `wf_state.hrl` with `-include_lib` directive
  - Line 123: Calls `wf_state:restore_from_ets/1`
  - Lines 186, 194, 235, 252, 295, 317: Calls `wf_state` functions
  - Line 349: `propagate/2` expects `#{term() => #token{}}` but wf_cancel header defines `token` differently

### Multiple Instances
- `src/wf_mi.erl:1-200+` - Multiple instance pattern
  - Lines 36-37: Includes `wf_exec.hrl` and `wf_mi.hrl`
  - Line 113: `collect_result/3` expects `ExecState` parameter
  - Line 169: `cancel_remaining/2` expects `ExecState` parameter
  - Lines 203, 221: References to `eval_instance_count` function (not seen in partial read)

### Examples and Benchmarks (Wrong Directories)
- `debug_test.erl:1-16` - Debug test script in project root (wrong location)
- `demo_pattern_algebra.erl:1-71` - Pattern algebra demo in project root (wrong location)
- `examples/basic_usage.erl:1-50+` - Usage examples (should probably be in test/)
- `src/wf_bench.erl:1-50+` - Performance benchmarks

### Test Files
- `test/wf_substrate_tests.erl:8-17` - Empty test suite
- `test/wf_exec_tests.erl` - Executor tests (not read)
- `test/wf_term_tests.erl` - Pattern algebra tests (not read)
- `test/wf_core_tests.erl` - Derived pattern tests (not read)
- `test/wf_effect_tests.erl` - Effect system tests (not read)
- `test/wf_cancel_tests.erl` - Cancellation tests (not read)
- 48 total test modules

### Header Files
- `src/wf_exec.hrl:7-44` - Defines `token`, `branch_info`, `join_counter`, `exec_state` records
- `src/wf_mi.hrl:8-23` - Defines `mi_instance`, `mi_config` records
- `src/wf_effect.hrl:8-25` - Defines `effect_spec`, `effect` records
- `src/wf_receipt.hrl:8-17` - Defines `receipt` record
- `include/wf_cancel.hrl` - Cancellation headers (not read)
- `include/wf_state.hrl` - State management headers (not read)

## Technical Considerations

### Dependencies

**Internal Modules:**
- `wf_exec` is central dependency for most modules
- `wf_vm` provides type definitions used throughout
- `wf_term` provides pattern algebra used by `wf_core`, `wf_compile`, `wf_validate`
- `wf_sched` has three implementations: `wf_sched_deterministic`, `wf_sched_nondeterministic`, `wf_sched_replay`
- `wf_effect` and `wf_receipt` are tightly coupled
- `wf_cancel` depends on `wf_state` (which may not be fully implemented)
- `wf_mi` integrates with `wf_exec` for instance lifecycle management

**External Dependencies:**
- Erlang/OTP 26+ (rebar.config:3)
- Standard library: kernel, stdlib, sasl (wf_substrate.app.src:6-9)
- No external deps (rebar.config:13 shows empty list)

### Patterns to Follow

**Include File Patterns:**
- Modules include their own header files first: `-include("wf_exec.hrl")`, `-include("wf_mi.hrl")`
- Cross-module includes: `wf_exec.erl` includes `wf_mi.hrl`, `wf_effect.hrl`, `wf_receipt.hrl`
- Library includes: `wf_cancel.erl:111` uses `-include_lib("wf_state.hrl")`

**Export Patterns:**
- Type exports before function exports: `wf_term.erl:152-166` (type exports), `wf_term.erl:169-189` (function exports)
- API exports separated from internal exports: `wf_term.erl:169-198` (raw constructors), `wf_term.erl:272-433` (smart constructors)
- Record exports in headers, not in modules (records in .hrl files)

**Type Definition Patterns:**
- Transparent types for data structures: `-type wf_term() :: ...` (wf_term.erl:129)
- Opaque types for encapsulation: `-opaque effect_spec() :: #effect_spec{}` (wf_effect.erl:116)
- Exported types listed in `-export_type([])` attribute

**Behavior Patterns:**
- gen_server: `wf_effect` implements init, handle_call, handle_cast, handle_info, terminate, code_change
- gen_statem: `wf_case_runner` implements state_functions mode with explicit state modeling
- supervisor: `wf_case_sup`, `wf_substrate_sup` implement init/1
- behavior: `wf_sched` defines callback contract (lines 7-9)

**Error Handling Patterns:**
- Validation errors: `wf_term:well_formed/1` returns `ok | {error, [validation_error()]}`
- Runtime errors: `error(badarg)` for invalid arguments (wf_term.erl:304)
- Return tuples: `{ok, Result} | {error, Reason}` pattern throughout

### Known Issues from Code Analysis

**Missing Functions:**
1. `wf_exec:snapshot_exec_state/1` - Called by `wf_trace:259` but not defined in `wf_exec`
2. `wf_state:*` - Multiple functions called by `wf_cancel` (restore_from_ets, get_tokens, get_status, buffer_mutation, commit, get_scope, etc.)
3. `wf_mi:eval_instance_count/2` - Referenced but not seen in partial read (likely exists later in file)

**Type/Opaque Issues:**
1. `wf_effect:effect_spec()` is opaque but `wf_effect:new_spec/5` returns it (line 173) - This is OK for constructor
2. `wf_effect:effect()` is opaque but `wf_effect:yield/4` returns `{ok, effect()} | {ok, term()}` - The `{ok, term()}` branch returns a receipt, breaking type consistency
3. `wf_receipt:receipt()` type not seen (needs investigation)

**Module List Incomplete:**
- `wf_substrate.app.src:12-25` missing: `wf_vm`, `wf_term`, `wf_core`, `wf_compile`, `wf_effect`, `wf_receipt`, `wf_cancel`, `wf_sched_deterministic`, `wf_sched_nondeterministic`, `wf_sched_replay`, `wf_acceptance`, `wf_validate`, `wf_trace`, `wf_governance`, `wf_budget`, `wf_approval`, plus example modules

**File Location Issues:**
1. `debug_test.erl` in project root - should be in `test/` or `examples/`
2. `demo_pattern_algebra.erl` in project root - should be in `examples/`
3. `examples/basic_usage.erl` - May need special handling or move to test/
4. `test_governance_simple.erl` in project root - should be in `test/`

**Record Field Mismatches:**
- `wf_exec.hrl:14` defines `token.current_effect :: {term(), non_neg_integer(), term()} | undefined` (3-tuple or undefined)
- `wf_exec.hrl:12` defines `token.status :: active | complete | cancelled | blocked_effect | blocked_approval`
- `wf_cancel.erl:342` calls `propagate/2` expecting `#{term() => #token{}}` but token record has different structure

**Undefined Module References:**
- `wf_substrate_sup.erl:53-70` references `wf_governance`, `wf_budget`, `wf_approval` which may be stubs
- These modules need `start_link/0` function for supervisor child specs

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|---------|------------|
| **Missing implementations cause cascade failures** | High | Identify all missing modules/stubs, implement minimal versions or remove from app.src |
| **Opaque type violations** | High | Change `wf_effect:yield/4` return type or implement proper abstraction layer |
| **Header file include path errors** | Medium | Use `-include_lib` for cross-directory includes, verify all paths |
| **Undefined function calls** | High | Implement missing functions (`wf_exec:snapshot_exec_state/1`, `wf_state` API) |
| **Module list in app.src incomplete** | High | Add all implemented modules to list, or use auto-discovery |
| **Escripts in wrong directories** | Medium | Move `debug_test.erl`, `demo_pattern_algebra.erl`, `test_governance_simple.erl` to appropriate locations |
| **Test file compilation failures** | High | Fix test modules first, ensure they can compile before running |
| **Record definition mismatches** | High | Verify all record definitions across headers, ensure field access matches |
| **Missing exports cause xref failures** | Medium | Run xref, add missing exports, remove unused exports |
| **Function arity mismatches** | High | Verify all function calls match exported arities |
| **warnings_as_errors causes build failure** | High | Fix all warnings systematically by category |
| **Circular dependencies** | Medium | Review module dependencies, refactor if needed |
| **Type inconsistencies across modules** | Medium | Ensure type exports match usage sites |

## Recommended Approach

### Phase 1: Initial Compilation Attempt
1. Run `rebar3 compile` to capture all errors and warnings
2. Categorize errors by type (missing modules, undefined functions, type errors, etc.)
3. Create prioritized backlog based on error categories

### Phase 2: Fix Structural Issues
1. **Move misplaced files:**
   - Move `debug_test.erl` to `test/debug_test.erl`
   - Move `demo_pattern_algebra.erl` to `examples/demo_pattern_algebra.erl`
   - Move `test_governance_simple.erl` to `test/test_governance_simple.erl`
2. **Update module list:**
   - Add all implemented modules to `wf_substrate.app.src`
   - Remove non-existent modules or implement stubs

### Phase 3: Fix Missing Functions
1. Implement `wf_exec:snapshot_exec_state/1` (serialize exec_state for tracing)
2. Implement or stub `wf_state` API functions:
   - `restore_from_ets/1`
   - `get_tokens/1`
   - `get_status/1`
   - `buffer_mutation/2`
   - `commit/1`
   - `get_scope/2`
   - etc.
3. Verify all `wf_mi` functions are exported correctly

### Phase 4: Fix Type/Opaque Issues
1. Review `wf_effect` opaque types:
   - Change `yield/4` to return consistent type
   - Ensure receipt handling doesn't violate opacity
2. Review `wf_receipt` type definitions
3. Ensure all type exports match actual usage

### Phase 5: Fix Include and Record Issues
1. Verify all `-include` and `-include_lib` directives use correct paths
2. Ensure record definitions are consistent across all headers
3. Verify record field access matches definitions

### Phase 6: Fix Missing Exports
1. Run `rebar3 xref` to find undefined function calls
2. Add missing exports to module `-export([])` lists
3. Remove unused exports (xref locals_not_used warnings)

### Phase 7: Fix Warnings
1. **Unused variables:** Use `_VarName` pattern or remove
2. **Unused types:** Remove or document why exported
3. **Unused functions:** Remove or add to test suite
4. **Shadowed variables:** Rename to avoid conflicts
5. **Match warnings:** Add explicit guards or catch-all clauses
6. **Deprecated functions:** Update to new APIs

### Phase 8: Fix Tests
1. Implement stubs for missing test dependencies
2. Ensure `test/wf_substrate_tests.erl` has at least one dummy test
3. Fix all test module compilation errors
4. Run `rebar3 eunit` and fix failures iteratively

### Phase 9: Verification
1. Run `rebar3 compile` with clean build
2. Run `rebar3 xref` to verify no undefined references
3. Run `rebar3 dialyzer` to verify type consistency
4. Run `rebar3 eunit` to verify all tests pass
5. Test basic workflow execution manually

### Prioritization
**Critical Path (blocks compilation):**
1. Missing modules in app.src
2. Undefined function calls
3. Missing include files
4. Syntax errors

**High Priority (warnings_as_errors):**
1. Unused variables
2. Type mismatches
3. Missing exports
4. Function arity mismatches

**Medium Priority (test failures):**
1. Stub implementations
2. Missing test dependencies
3. Test assertion errors

**Low Priority (code quality):**
1. Dead code removal
2. Documentation improvements
3. Performance optimizations

## Open Questions

1. **State Management (`wf_state` module):**
   - Is `wf_state` fully implemented?
   - What is the complete API surface?
   - How does it integrate with `wf_exec` and `wf_cancel`?
   - Should ETS-backed state be implemented or stubbed?

2. **Effect System (`wf_effect`, `wf_receipt`):**
   - What is the correct return type for `wf_effect:yield/4`?
   - Should receipts be opaque or transparent?
   - How does effect idempotency work in practice?

3. **Supervisor Child Specs:**
   - Are `wf_governance`, `wf_budget`, `wf_approval` implemented?
   - If stubs, do they export `start_link/0`?
   - Should they be removed from supervision tree if not implemented?

4. **Example Modules:**
   - Should example modules (`wf_example_*.erl`) be in app.src module list?
   - Should they be compiled or treated as escripts?
   - What is the intended use of `debug_test.erl` and `demo_pattern_algebra.erl`?

5. **Missing Functions:**
   - What should `wf_exec:snapshot_exec_state/1` return?
   - What is the signature of `wf_mi:eval_instance_count/2`?
   - Are there other undefined functions not yet discovered?

6. **Test Strategy:**
   - Should stub modules have minimal implementations for testing?
   - What is the minimum test coverage required?
   - Should integration tests be deferred?

7. **Compilation Order:**
   - Are there circular dependencies that need refactoring?
   - Should some modules be merged or split?

8. **Opaque Type Design:**
   - Should `wf_effect:effect_spec()` and `effect()` be opaque or transparent?
   - How to fix `yield/4` return type inconsistency?
   - Should receipts be opaque in `wf_receipt`?

9. **Header File Organization:**
   - Why are some headers in `src/` and some in `include/`?
   - Should they be consolidated?
   - Are there include path issues?

10. **Configuration:**
    - Should `warnings_as_errors` be disabled temporarily for incremental fixes?
    - Should OTP version requirement be relaxed?
    - Are dialyzer warnings actionable?

11. **External Dependencies:**
    - Are any external deps needed for testing (e.g., proper, meck)?
    - Should test utilities be extracted to separate module?

12. **Documentation:**
    - Are there missing @doc, @spec attributes causing warnings?
    - Should all exported functions have specs?
