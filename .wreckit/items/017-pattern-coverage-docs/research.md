# Research: Write pattern coverage and operational documentation

**Date**: 2025-01-11
**Item**: 017-pattern-coverage-docs

## Research Question

Write comprehensive documentation:

docs/PATTERNS.md: table mapping each of the 43 workflow patterns (from the van der Aalst et al. workflow pattern catalog) to its wf_substrate representation. For each pattern: pattern number, pattern name, category (basic/advanced branching/structural/multiple instance/state-based/cancellation), wf_term() expression, compilation sketch (which opcodes are used), key test module(s), known corner cases or limitations. Patterns that are directly supported as kernel primitives vs. derived from primitives should be clearly distinguished.

docs/TESTING.md: testing strategy document. Covers: test organization (unit/integration/property/benchmark), how to run tests (rebar3 eunit, rebar3 ct), test naming conventions, how to add new pattern tests, the minimal property-based framework usage, determinism/replay testing methodology, how benchmarks are structured and interpreted.

docs/OPERATIONS.md: operational guide. Covers: supervision tree structure and restart strategies, failure modes and recovery (what happens when a case runner crashes, effect timeout, state corruption detection), telemetry/tracing configuration (trace levels, sinks, log format), monitoring a running system (how to inspect active cases, check for stuck workflows), configuration options (scheduler policy, quantum size, timeouts, effect budgets).

## Summary

The wf_substrate project implements a bytecode VM-based workflow pattern executor in pure Erlang. The codebase currently has foundational infrastructure including a bytecode executor (wf_exec.erl), VM opcodes (wf_vm.erl), tracing (wf_trace.erl), validation (wf_validate.erl), and comprehensive test suites. However, **documentation is minimal** - only docs/README.md (placeholder) and docs/TESTING.md (partial, test-focused) exist.

The project needs three comprehensive documentation files:
1. **PATTERNS.md**: Mapping van der Aalst's 43 workflow patterns to wf_substrate implementation
2. **TESTING.md**: Complete testing strategy (partially exists, needs expansion)
3. **OPERATIONS.md**: New operational guide for supervision, failures, telemetry, and monitoring

The existing codebase provides rich material:
- **Opcodes defined**: SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, TASK_EXEC, DONE, MI_SPAWN (wf_vm.erl:13-21)
- **Test modules**: wf_test_seq, wf_test_par, wf_test_xor, wf_test_join, wf_test_cancel, wf_test_mi, wf_test_term, wf_test_helpers, wf_prop (test/*.erl)
- **Join policies**: all, {first_n, N}, {n_of_m, N, M}, first_complete, sync_merge (wf_vm.erl:24-29)
- **Loop policies**: {count, N}, while, until (wf_vm.erl:32-35)
- **MI policies**: {fixed, N}, {dynamic, Min, Max} (wf_vm.erl:38-40)
- **Tracing**: Three levels (none/min/full), four sink types (callback/ets/process/replay) (wf_trace.erl:38-44, 232-238)
- **Validation**: Bounded state exploration, deadlock/livelock detection, soundness checks (wf_validate.erl)

## Current State Analysis

### Existing Documentation

**docs/README.md** (lines 1-22):
- Placeholder listing planned docs: ARCHITECTURE.md, SEMANTICS.md, PATTERNS.md, TESTING.md, OPERATIONS.md
- Notes that PROMPT.md is authoritative until docs are created
- References Wreckit workflow for doc implementation items

**docs/TESTING.md** (lines 1-266):
- **Already covers**: Test organization, running tests (rebar3 eunit), coverage goals, test patterns (mock bytecode generators, setup/teardown, parameterized tests), property-based testing (wf_prop.erl), test execution times, CI
- **Test modules documented**: wf_test_seq, wf_test_par, wf_test_xor, wf_test_join, wf_test_cancel, wf_test_mi, wf_test_term, wf_test_helpers, wf_prop
- **Missing**: No mention of determinism/replay tests (wf_test_determinism.erl exists), no integration tests, no Common Test usage, limited operational testing guidance

### Existing Implementation

**Core Executor** (src/wf_exec.erl):
- Records: exec_state, token, branch_info, join_counter (lines 27-65)
- API: new/1, step/2, run/3, is_done/1, is_blocked/1 (lines 78-166)
- Opcodes implemented: SEQ_ENTER, SEQ_NEXT, TASK_EXEC, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, DONE (lines 187-573)
- **Cancellation stubbed**: is_scope_cancelled/2, propagate_cancellation/2 (lines 512-529) reference TODO for item 008
- **MI not implemented**: No MI_SPAWN opcode handler

**Bytecode VM** (src/wf_vm.erl):
- Type definitions for wf_bc(), opcode(), join_policy(), loop_policy(), mi_policy() (lines 9-49)
- 11 opcode types defined (line 13)

**Tracing** (src/wf_trace.erl):
- Records: trace_event, replay_entry, trace_state (lines 10-32)
- Three trace levels: none, min, full (line 38, 149-163)
- Four sink types: callback, ets, process, replay (line 40-44)
- Structural opcodes for level=min: PAR_FORK, JOIN_WAIT, CANCEL_SCOPE, DONE (line 234)
- State snapshots only at level=full (line 258-261)

**Validation** (src/wf_validate.erl):
- Five correctness checks: dead_transitions, option_to_complete, proper_completion, deadlock, soundness (lines 532-652)
- Bounded exploration with depth/token bounds (lines 431-526)
- BFS/DFS search strategies (lines 437-439)

**Supervision** (src/wf_substrate_sup.erl):
- **Skeleton only**: No child specs (line 46), one_for_one strategy (line 43)
- Missing: wf_case_sup, wf_effect_sup, wf_trace_sink children (referenced in PROMPT.md:182-185)

**Benchmarks** (src/wf_bench.erl):
- Six benchmarks defined, 4 implemented, 2 deferred (lines 86-176)
- Results format: name, steps, wall_time_us, steps_per_sec, memory_words (lines 65-71)
- Deferred items: mi_instances (item 009), state_store (items 006, 010)

### Test Patterns

**Test Module Structure** (all files in test/):
- Mock bytecode generators (mock_bytecode_* functions)
- EUnit test functions (*_test())
- Parameterized tests (*_test_() returning list of {Name, Fun} tuples)
- Helper functions for counting tokens, asserting state

**Property Testing** (test/wf_prop.erl):
- Random bytecode generator with depth limit (lines 9-55)
- Two runners: for_all/3 (EUnit assertions), quickcheck/2 (error reporting) (lines 60-89)
- No shrinking in quickcheck implementation

### Known Implementation Gaps

From code analysis:
1. **Cancellation**: Stub implementations in wf_exec.erl:512-529 reference item 008
2. **Multiple Instances**: wf_test_mi.erl includes wf_mi.hrl (line 11) but wf_mi.hrl doesn't exist in src/
3. **Scheduler**: wf_sched referenced but no implementation found
4. **Effect System**: wf_effect referenced in PROMPT.md but not implemented
5. **Supervision Tree**: Empty child specs in wf_substrate_sup.erl:46

## Key Files

### Source Files
- `src/wf_vm.erl:1-50` - Bytecode and opcode type definitions (wf_bc(), 11 opcodes, join/loop/MI policies)
- `src/wf_exec.erl:1-598` - Bytecode executor (exec_state record, opcode dispatch, token management, join/cancellation logic)
- `src/wf_trace.erl:1-335` - Tracing infrastructure (trace_event record, 3 trace levels, 4 sink types, replay log support)
- `src/wf_validate.erl:1-653` - Validation backend (validation_state record, 5 correctness checks, bounded exploration)
- `src/wf_substrate.erl:1-32` - Public API (placeholder, no functions exported)
- `src/wf_substrate_sup.erl:1-52` - Top-level supervisor (one_for_one strategy, empty child specs)
- `src/wf_bench.erl:1-355` - Performance benchmarks (6 benchmarks, 4 implemented, metrics reporting)

### Test Files
- `test/wf_test_seq.erl:1-186` - Sequential composition tests (2, 3, N tasks, nesting, cancellation, empty seq)
- `test/wf_test_par.erl:1-225` - Parallel patterns (2/3/N branches, mixed completion, wait_n, first_complete, join counters)
- `test/wf_test_xor.erl:1-154` - Exclusive choice (2/3/N branches, simple merge, deterministic selection)
- `test/wf_test_join.erl:1-257` - Join policies (wait_all, wait_n, first_complete, sync_merge, edge cases N=1, N=M)
- `test/wf_test_cancel.erl:1-229` - Cancellation (running task, case with PAR/XOR, nested scopes, during MI, empty scope)
- `test/wf_test_mi.erl:1-215` - Multiple instances (fixed 3/5, dynamic, wait_n, first_complete, none, instance IDs)
- `test/wf_test_term.erl` - Bytecode structure validation (property tests for random bytecode)
- `test/wf_prop.erl:1-90` - Minimal property-based testing framework (random term generator, for_all/3, quickcheck/2)
- `test/wf_test_helpers.erl` - Common utilities (exec_until_done/1, exec_steps/2, token status helpers)
- `test/wf_test_determinism.erl` - Determinism/replay tests (exists, not yet read)

### Header Files
- `src/wf_trace.hrl:1-30` - trace_event, replay_entry, trace_state records
- `src/wf_validate.hrl:1-30` - validation_state, issue, report records

### Documentation Files
- `docs/README.md:1-22` - Placeholder listing planned docs
- `docs/TESTING.md:1-266` - Partial testing guide (test organization, running tests, patterns, properties)
- `PROMPT.md:1-375` - Authoritative specification (43 patterns, OTP design, validation strategy)

## Technical Considerations

### Dependencies

**External**:
- None (pure Erlang/OTP, stdlib only)

**Internal modules**:
- wf_exec (executor) - core for all docs
- wf_vm (opcodes) - for PATTERNS.md
- wf_trace (tracing) - for OPERATIONS.md
- wf_validate (validation) - for TESTING.md
- wf_substrate_sup (supervision) - for OPERATIONS.md
- wf_bench (benchmarks) - for TESTING.md
- All test modules (wf_test_*.erl) - for PATTERNS.md and TESTING.md

### Patterns to Follow

**Documentation style**:
- docs/TESTING.md uses EDoc-style markdown with code blocks, tables, and section headers
- PROMPT.md uses numbered sections, bullet lists, and code examples

**Test naming**:
- Unit tests: *_*_test() (e.g., seq_2_tasks_executed_in_order_test)
- Parameterized tests: *_*_test_() returning {Name, Fun} list (e.g., par_n_branches_test_())

**Opcode naming**:
- UPPERCASE atoms in bytecode: 'SEQ_ENTER', 'PAR_FORK', etc.
- Lowercase also accepted (wf_exec.erl:187-191 handles both cases)

**Record definitions**:
- Centralized in .hrl files (wf_trace.hrl, wf_validate.hrl)
- Exec records in wf_exec.erl (no separate header)

### Workflow Pattern Categories

From PROMPT.md:75-91 and test modules, patterns fall into:
1. **Basic control flow**: Sequence, Task
2. **Advanced branching and synchronization**: Parallel split (AND-split), Synchronization (AND-join), Multi-choice, Synchronizing merge, Discriminator, N-out-of-M join
3. **Structural**: Exclusive choice (XOR-split), Simple merge (XOR-join)
4. **Multiple instance**: Fixed count, Dynamic count, with join policies (wait_all, wait_n, first_complete, none)
5. **State-based**: Arbitrary cycles (Loop), Deferred choice (not yet seen in code)
6. **Cancellation**: Cancel activity, Cancel case, Cancel region (scope-based)

### van der Aalst Pattern Catalog

The reference to "43 workflow patterns" is from:
**"Workflow Patterns: The Definitive Guide"** by van der Aalst, ter Hofstede, Kiepuszewski, and Barros (2003).

The 43 patterns are organized into categories:
- Control flow patterns (20): Sequence, Parallel Split, Synchronization, Exclusive Choice, Simple Merge, Multi Choice, Synchronizing Merge, Multi Merge, Discriminator, Arbitrary Cycles, Implicit Termination, etc.
- Data patterns (3): Data visibility, Data interaction, Data transformation
- Resource patterns (4): Distribution, Role-based distribution, Distribution case, Late distribution
- Exception handling patterns (6): Error boundary, Error indicator, Retry, Compensation, etc.
- Other patterns (10): Milestone, Cancel Activity, Cancel Case, Multi Instance, etc.

**Key challenge**: The codebase implements a subset. PATTERNS.md must distinguish:
- **Kernel primitives**: Direct opcode support (seq, par, xor, join, loop, cancel, task)
- **Derived patterns**: Combinations of primitives (discriminator = par + first_complete join)
- **Not implemented**: Data patterns, resource patterns, advanced exception handling

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Van der Aalst patterns not fully cataloged** | High | Research exact 43 patterns from academic sources, map only implemented ones, mark rest as TODO |
| **Incomplete implementation gaps** | Medium | Document limitations explicitly (e.g., "MI_SPAWN stubbed"), reference specific item numbers (008, 009, 010) |
| **TESTING.md already partially exists** | Low | Build upon existing docs/TESTING.md, expand with missing sections (integration tests, determinism, CT) |
| **Supervision tree is empty** | Medium | Document current skeleton state, reference PROMPT.md:182-185 for planned structure, note item 012 dependency |
| **Missing ARCHITECTURE.md and SEMANTICS.md** | Low | These are separate planned docs, don't need to write them, but can reference them as TODO |
| **Pattern categorization ambiguity** | Medium | Use test modules as ground truth (what's tested is implemented), cross-reference with PROMPT.md:75-91 |
| **Scheduler module not found** | Medium | wf_sched referenced in wf_trace.erl:21 but no implementation, document as TODO/future item |
| **Opcode naming inconsistency** | Low | Both 'UPPER' and 'lower' accepted (wf_exec.erl:187-208), document that bytecode uses UPPERCASE convention |

## Recommended Approach

### Phase 1: Pattern Inventory (PATTERNS.md)

1. **Catalog implemented patterns** from test modules:
   - Extract patterns from wf_test_seq, wf_test_par, wf_test_xor, wf_test_join, wf_test_cancel, wf_test_mi
   - Map each to van der Aalst pattern number and name
   - Categorize: Basic (sequence, task), Branching (par, xor), Structural (join policies), MI, Cancellation

2. **Create pattern table** with columns:
   - Pattern number (1-43, or "N/A" for custom)
   - Pattern name (van der Aalst terminology)
   - Category (basic/branching/structural/MI/state-based/cancellation)
   - wf_term() expression (e.g., seq(P, Q))
   - Bytecode opcodes (e.g., SEQ_ENTER, SEQ_NEXT, DONE)
   - Test modules (e.g., wf_test_seq.erl)
   - Kernel/Derived/Not Implemented
   - Corner cases/limitations

3. **Distinguish kernel vs derived**:
   - **Kernel primitives**: Opcodes with direct handlers in wf_exec:execute_opcode/2
   - **Derived patterns**: Macro combinations (e.g., discriminator = PAR_FORK + JOIN_WAIT first_complete)
   - **Not implemented**: Patterns without test coverage (e.g., deferred choice, multi-merge)

4. **Document limitations**:
   - Cancellation stubbed (wf_exec.erl:512-529, item 008)
   - MI_SPAWN not in opcode dispatch (test exists but no handler)
   - Scheduler not implemented (wf_sched referenced but missing)
   - No data/resource patterns (not in scope for control-flow substrate)

### Phase 2: Testing Guide (TESTING.md)

1. **Preserve existing content** (docs/TESTING.md:1-266) - it's good
2. **Add missing sections**:
   - Integration tests (how to test full workflows with effects)
   - Determinism/replay methodology (wf_test_determinism.erl)
   - Common Test usage (rebar3 ct, test suites)
   - Operational testing (supervision tree restart testing)
   - Coverage reporting interpretation (what >80% means, gaps)

3. **Expand property testing**:
   - Document wf_prop:random_term/1 depth semantics (lines 9-55)
   - Properties checked: executable, terminating, label resolution (TESTING.md:151-154)
   - Reproduction workflow for failures (TESTING.md:156-168)
   - Limitations: No shrinking in quickcheck (wf_prop.erl:70-89)

4. **Add test patterns**:
   - How to add new pattern tests (create wf_test_<pattern>.erl, mock_bytecode_* generators)
   - Parameterized test pattern (lists:map with {io_lib:format, fun()})
   - Helper functions (wf_test_helpers.erl)

### Phase 3: Operations Guide (OPERATIONS.md)

1. **Supervision tree** (reference PROMPT.md:182-185):
   - Current state: wf_substrate_sup with empty child specs (wf_substrate_sup.erl:46)
   - Planned structure: wf_case_sup (simple_one_for_one), wf_effect_sup, wf_trace_sink
   - Restart strategies: one_for_one for top-level, simple_one_for_one for dynamic cases
   - Item dependencies: 012 (otp-supervision-tree)

2. **Failure modes and recovery**:
   - Case runner crash: Supervisor restart strategy, state loss (no persistence yet)
   - Effect timeout: Not implemented (item 010), planned mechanism
   - State corruption: Validation checks (wf_validate), bounded exploration
   - Cancellation failure: Stubs in wf_exec.erl:512-529

3. **Telemetry/tracing** (src/wf_trace.erl):
   - Trace levels: none (zero overhead), min (structural events only), full (all events + state snapshots)
   - Sinks: callback (user function), ets (in-memory table), process (message passing), replay (deterministic replay)
   - Configuration: wf_trace:set_level/1, wf_trace:new/1
   - Log format: trace_event record (step_seq, opcode, timestamp, scope, branch_id, metadata)

4. **Monitoring running systems**:
   - Inspect active cases: Planned API (wf_substrate:status/1), not yet implemented
   - Check for stuck workflows: Look for tokens with status=active but no enabled transitions (wf_validate:check_deadlock/1)
   - Trace retrieval: wf_trace:get_events/1, wf_trace:filter/2
   - ETS table inspection: wf_trace_events (named table, line 82-86)

5. **Configuration options** (from PROMPT.md:284-289):
   - scheduler_policy: deterministic | nondeterministic | replay(ChoiceLog) - NOT IMPLEMENTED
   - step_quanta: integer() - executor burst size (wf_exec:run/3 line 152)
   - trace_level: none | min | full - IMPLEMENTED (wf_trace.erl:38)
   - effect_handler: module() - NOT IMPLEMENTED (item 010)
   - Effect budgets: Planned (PROMPT.md:296), not in code yet

### Cross-Cutting Considerations

- **Reference specific file:line numbers** for all claims
- **Distinguish "implemented" vs "planned"** (skeleton vs working code)
- **Use code examples** from actual test modules
- **List TODO items** with numbers (e.g., "awaiting item 008 for full cancellation")
- **Maintain consistency** with existing docs/TESTING.md style

## Open Questions

1. **Van der Aalst catalog access**: Do we have access to the original paper or list of 43 patterns? If not, should we reverse-engineer from PROMPT.md:75-91 list and test modules?

2. **Pattern numbering**: The 43 patterns have specific numbers in the catalog. Should we use official numbers or create our own numbering? Need source reference.

3. **Derived pattern definitions**: For patterns not directly implemented as opcodes (e.g., multi-merge, synchronizing merge), should we provide example wf_term() macros showing how to derive them from kernel primitives?

4. **TESTING.md scope**: Existing file focuses on unit/integration/property/benchmark. Should we include:
   - Manual testing procedures?
   - Load testing strategies?
   - Chaos engineering (killing processes)?

5. **OPERATIONS.md audience**: Is this for:
   - DevOps deploying wf_substrate as a service?
   - Developers integrating wf_substrate into their apps?
   - Both? (affects depth of deployment instructions)

6. **Scheduler policy documentation**: wf_sched is referenced but not implemented. Should we:
   - Document as "TODO, future item"?
   - Document the planned API from PROMPT.md:140-143?
   - Wait until implementation exists?

7. **Effect system boundaries**: wf_effect and wf_receipt mentioned (PROMPT.md:145-155) but not implemented. How much to document in OPERATIONS.md vs deferring to future items?

8. **Benchmark interpretation**: wf_bench:steps_per_sec is the key metric. What are "good" numbers? Should we provide baseline targets or just explain the metric?

9. **Example workflows**: Should PATTERNS.md include runnable examples (like wf_example_basic.erl mentioned in PROMPT.md:309-313)? Those don't exist yet.

10. **Determinism testing**: wf_test_determinism.erl exists but wasn't read. Should it be read before writing to understand the replay methodology?
