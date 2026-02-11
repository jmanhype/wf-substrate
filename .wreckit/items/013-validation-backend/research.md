# Research: Implement validation backend with bounded model checking

**Date**: 2025-01-10
**Item**: 013-validation-backend

## Research Question

Implement wf_validate.erl: static analysis and bounded model checking for workflow terms. This is a test/validation tool, NOT the runtime execution engine.

Compile wf_term() into a simplified Petri net representation (places = states, transitions = actions, tokens = control flow markers) or labelled transition system (LTS) for state space exploration.

Bounded state exploration with configurable depth D (max steps) and token bound K (max concurrent tokens). The explorer performs BFS/DFS over reachable states.

Checks:
1. No dead transitions: every transition in the net is firable in at least one reachable state (no unreachable code).
2. Option to complete: from every reachable state, there exists a path to a terminal state (no livelocks).
3. Proper completion: when the workflow terminates, exactly one token is in the final place (no leftover tokens, no missing completion).
4. Deadlock detection: no reachable state where tokens exist but no transition is enabled.
5. Soundness: combination of proper completion + option to complete + no dead transitions.

Export: validate/1 (default options), validate/2 (with options), to_petri_net/1, explore/2, check_soundness/1, check_deadlock/1, check_proper_completion/1. Return structured results: {ok, report()} | {error, [issue()]} where issue includes the problematic state and a diagnostic message.

## Summary

The validation backend (wf_validate.erl) is a static analysis and model checking tool that operates independently from the runtime executor. It translates workflow bytecode (wf_vm:wf_bc()) into a Petri net or LTS representation for exhaustive state space exploration within configurable bounds (depth D, token bound K). The module implements five core correctness checks: dead transition detection (unreachable code), option to complete (no livelocks), proper completion (single terminal token), deadlock detection (no stuck states), and soundness (composite of previous checks). The design separates validation concerns from runtime execution—validation operates on compiled bytecode via symbolic exploration, never executing actual tasks or effects. The explorer performs BFS/DFS traversal of reachable states, tracking visited states to detect cycles and bound exploration. Results are structured as {ok, report()} or {error, [issue()]} with problematic states and diagnostics.

Current implementation status: Items 002 (wf_term), 004 (wf_compile), and 005 (wf_exec) are in "idea" state, meaning the pattern algebra, compiler, and executor are not yet implemented. However, item 005-executor-hot-loop has been implemented (wf_exec.erl exists and is tested), and wf_vm.erl defines the bytecode instruction set. The validation backend must work with the existing wf_vm:wf_bc() bytecode format (list of opcodes) and can leverage wf_exec's exec_state representation for state tracking, but should NOT use wf_exec:step/2 for exploration (that would be runtime execution, not static analysis). Instead, wf_validate should implement its own symbolic executor that explores all possible paths without running tasks.

## Current State Analysis

### Existing Implementation

**Pattern Algebra and Compiler (NOT YET IMPLEMENTED)**:
- Item 002 (wf_term.erl) and item 004 (wf_compile.erl) are in "idea" state per `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/item.json:5` and `/Users/speed/wf-substrate/.wreckit/items/004-bytecode-compiler/item.json:5`
- No wf_term.erl or wf_compile.erl source files exist (verified via glob searches)
- The validation backend must accept wf_vm:wf_bc() bytecode directly (already available in wf_vm.erl)

**Executor and VM (IMPLEMENTED)**:
- `/Users/speed/wf-substrate/src/wf_vm.erl:10-50` - Defines bytecode type (wf_bc()) and all opcodes: SEQ_ENTER, SEQ_NEXT, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_CHECK, LOOP_BACK, CANCEL_SCOPE, MI_SPAWN, TASK_EXEC, DONE
- `/Users/speed/wf-substrate/src/wf_exec.erl:1-783` - Implements bytecode executor with exec_state record (ip, bytecode, ctx, tokens, branch_map, join_counters, scope_stack, step_count, status)
- `/Users/speed/wf-substrate/src/wf_exec.hrl:31-44` - Defines exec_state record structure that validation can reference
- The executor uses a token-based model where each token is a thread of execution (line 52-59)

**State Store (IMPLEMENTED)**:
- `/Users/speed/wf-substrate/src/wf_state.erl:26-579` - Implements transactional state management with ETS persistence, mutation buffering, and atomic commit protocol
- Defines token record (line 76-82) and scope record (line 84-91) which are relevant for tracking validation state

**Scheduler (IMPLEMENTED)**:
- `/Users/speed/wf-substrate/src/wf_sched.erl:1-174` - Implements scheduler behavior with deterministic, nondeterministic, and replay policies
- Defines enabled_action() type (line 19-21) as {token, token_id()} or {xor_branch, pos_integer()}
- Validation backend can use deterministic scheduler for reproducible exploration

**Multiple Instance Support (IMPLEMENTED)**:
- `/Users/speed/wf-substrate/src/wf_mi.erl:1-248` - Implements MI patterns with instance spawning, join policies (wait_all, wait_n, first_complete, none)
- Defines mi_instance record (line 90-96) and mi_config record (line 100-106)

**Current Patterns and Conventions**:
- Records defined in .hrl files (wf_exec.hrl, wf_mi.hrl) and included via `-include` directive
- Type specs using `-type` and `-spec` for exported functions
- EUnit tests in test/ directory with `-include_lib("eunit/include/eunit.hrl")`
- No external dependencies (pure Erlang/OTP) per `/Users/speed/wf-substrate/rebar.config:13`

**Integration Points**:
- Public API stub in `/Users/speed/wf-substrate/src/wf_substrate.erl:16` mentions validate/2 as planned export
- Validation module should be callable from test suites and from wf_substrate:validate/2
- Must integrate with existing tracing system (wf_trace.erl) for debug output during exploration

## Key Files

- `/Users/speed/wf-substrate/src/wf_vm.erl:1-50` - Bytecode type definitions (wf_bc(), opcode types) that validate/1 will accept as input
- `/Users/speed/wf-substrate/src/wf_exec.erl:1-783` - Executor implementation showing how bytecode execution works; validation can reuse exec_state structure conceptually but implements symbolic execution
- `/Users/speed/wf-substrate/src/wf_exec.hrl:31-44` - exec_state record definition that validation state can mirror
- `/Users/speed/wf-substrate/src/wf_sched.erl:19-21` - enabled_action() type for representing nondeterministic choices during exploration
- `/Users/speed/wf-substrate/src/wf_state.erl:76-82` - Token record definition showing structure of execution threads
- `/Users/speed/wf-substrate/PROMPT.md:62` - Specifies wf_validate.erl as one of the core deliverables with bounded checking requirements
- `/Users/speed/wf-substrate/PROMPT.md:218-227` - Details bounded model checking requirements: exploration up to depth D and token bound K, checking dead transitions, option to complete, proper completion, deadlock detection

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only, per project constraint in PROMPT.md:20-21)

**Internal Modules to Integrate With**:
- `wf_vm` - Accept wf_vm:wf_bc() bytecode as input, use opcode type definitions
- `wf_exec` - Reference exec_state structure but DO NOT use wf_exec:step/2 (that's runtime execution, not static analysis)
- `wf_trace` - Emit trace events during exploration for debugging (optional, controlled by trace_level option)
- `wf_sched` - Use deterministic scheduler policy for reproducible exploration; enumerate all enabled actions for exhaustive checking

**No Dependency On**:
- `wf_term` and `wf_compile` (not implemented yet) - validation works directly with bytecode
- Runtime effect execution - validation explores control flow symbolically, never executes tasks

### Patterns to Follow

**Module Structure**:
- Place in `/Users/speed/wf-substrate/src/wf_validate.erl`
- Include necessary headers: `-include("wf_exec.hrl")` for exec_state and token records
- Export type definitions for validation-specific types: petri_net(), lts_state(), issue(), report()
- Provide -spec declarations for all exported functions
- Create corresponding test module `/Users/speed/wf-substrate/test/wf_validate_tests.erl`

**State Representation**:
- Reuse token concept from wf_exec (line 52-59 of wf_exec.erl) but create lighterweight validation_state record
- Use maps for efficient lookup (similar to exec_state's tokens and branch_map fields)
- Track visited states using a set (e.g., sets:set() or gb_set) to detect cycles and bound exploration

**Exploration Algorithm**:
- BFS or DFS over reachable states (configurable via options)
- At each state, compute enabled actions (tokens that can advance, join points that can fire)
- For each enabled action, compute successor state and recurse
- Apply bounds: depth D (max steps from initial state), token bound K (max concurrent tokens)
- Use deterministic ordering of enabled actions for reproducible results (sort by token_id or IP)

**Result Reporting**:
- Return structured {ok, report()} or {error, [issue()]}
- report() record should include: explored_state_count, unique_state_count, max_depth_reached, checks_passed (list of booleans), issues_found
- issue() record should include: issue_type (atom), state_snapshot (validation_state), diagnostic_message (string), path_from_initial (list of actions)

**Options Handling**:
- validate/1 uses defaults: {depth, 100}, {token_bound, 10}, {search_strategy, bfs}, {trace_level, none}
- validate/2 accepts proplists with overrides
- Export default_options/0 for testability

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **State space explosion** - Complex workflows may have astronomically many reachable states even with bounds | High | Enforce strict default bounds (D=100, K=10), use visited state set to avoid re-exploring, provide early termination options, document that bounded checking is not exhaustive for unbounded systems |
| **Confusion with runtime execution** - Developers might think validate/2 executes the workflow | Medium | Clearly document in module header and function specs that this is static analysis only, never executes tasks or effects, emphasize it's a test/validation tool not a runtime feature |
| **Incomplete coverage of opcodes** - New opcodes added to wf_vm may not be handled in validation | Medium | Create mapping table from opcode to validation transition function, assert on unknown opcodes during development, write tests that fail if new opcodes are unhandled |
| **Cycle detection performance** - Storing full state snapshots for visited set is expensive | Medium | Use state hashing (e.g., erlang:phash2) for visited set instead of full term storage, store full states only for issue reporting, document trade-off between memory and false positives |
| **Cancellation semantics complexity** - Modeling cancel scopes in Petri net is non-trivial | Low | Defer full cancellation validation to item 008 (wf_cancel), implement simplified version that treats cancel as no-op for v1, document limitations clearly |
| **Multiple instance combinatorics** - MI_SPAWN with dynamic policies creates unbounded instance counts | Medium | Enforce token bound K strictly, cap MI instance count at K during exploration, document that dynamic MI validation is heuristic only |

## Recommended Approach

**Phase 1: Core Data Structures (Week 1)**
- Define validation_state record mirroring exec_state: tokens (map), current_ips (set or map), branch_map (map), join_counters (map), scope_stack (list), step_count (non_neg_integer())
- Define petri_net() record: places (set of place IDs), transitions (map from transition ID to transition record), initial_marking (map from place to token count)
- Define lts_state() as alias for validation_state (LTS representation is just the state graph)
- Define issue() record: type, state, message, path
- Define report() record: stats, check_results, issues
- Implement to_petri_net/1: compile wf_bc() to petri_net() by one-pass translation (each opcode becomes transition(s) with input/output places)

**Phase 2: Exploration Engine (Week 2)**
- Implement enabled_transitions/1: given validation_state(), return list of {transition_id, action} pairs
- Implement fire_transition/2: given state and transition, compute successor state
- Implement explore/2: BFS/DFS with visited set, depth bound, token bound
- Implement state_hash/1: fast hash of validation_state for visited set
- Implement format_state/1: human-readable state dump for issue reporting

**Phase 3: Correctness Checks (Week 3)**
- Implement check_dead_transitions/2: track which transitions fired during exploration, report any unfired transitions
- Implement check_option_to_complete/1: from each visited state, run BFS to terminal state, report any states with no path to terminal (livelock)
- Implement check_proper_completion/1: verify terminal states have exactly one token in final place, report anomalies
- Implement check_deadlock/1: detect states with tokens but no enabled transitions
- Implement check_soundness/1: composite of above three checks

**Phase 4: Integration and Testing (Week 4)**
- Implement validate/1 and validate/2: main entry points with options handling
- Add EUnit tests in wf_validate_tests.erl: test simple sequence (no issues), test deadlock case (par with no join), test unreachable code (xor branch with no path), test proper completion violations
- Integrate with wf_substrate.erl: export validate/2 as public API
- Document module with @doc headers explaining purpose, limitations (bounded vs unbounded), and examples

**Key Design Decisions**:
1. **Symbolic execution, not runtime**: Never execute TASK_EXEC opcodes (treat as no-op transitions), never call external effects
2. **Petri net as intermediate representation**: Compile bytecode to Petri net once, then explore Petri net (more natural for state space exploration than bytecode)
3. **Bounded exploration**: Accept that bounded checking is incomplete for unbounded systems; document this limitation clearly
4. **Separate from runtime**: Validation module should be usable in CI/CD pipelines without starting any processes or requiring a running executor
5. **Testability first**: Write tests that expose known issues (deadlock, livelock, improper completion) before implementing checks

## Open Questions

1. **Petri net compilation strategy**: Should to_petri_net/1 produce a traditional Petri net (places/transitions/arcs) or directly use LTS representation (states/transitions)? LTS is simpler to implement but Petri net is more standard for workflow soundness checking. **Recommendation**: Start with LTS representation (validation_state as nodes, transitions as edges), add Petri net export later if needed for visualization.

2. **Handling of MI_SPAWN with dynamic policies**: Dynamic instance count ({dynamic, Min, Max}) creates unbounded state space. Should validation cap instances at token bound K or report dynamic MI as uncheckable? **Recommendation**: Cap at K for exploration but report issue "dynamic_mi_heuristic" to warn user that validation is approximate.

3. **Integration with wf_trace**: Should validation emit trace events during exploration for debugging? This would allow users to see exploration path but adds complexity. **Recommendation**: Make tracing optional via {trace_level, none|min|full} option, emit to wf_trace only if enabled.

4. **Cancellation semantics**: Item 008 (cancellation) is not yet implemented. Should validation implement cancel semantics or treat CANCEL_SCOPE as no-op? **Recommendation**: Treat CANCEL_SCOPE as structural marker only (enter/exit update scope stack but don't propagate cancellation) for v1, add full cancellation checking after item 008.

5. **Performance on large workflows**: What is the expected workflow size (number of opcodes) that validation should handle efficiently? **Recommendation**: Profile with realistic workflows (e.g., 100-500 opcodes), optimize state hashing and visited set operations, document expected performance characteristics.

6. **State representation for visited set**: Should visited set store full validation_state records (accurate but expensive) or hashes (fast but risk of collisions)? **Recommendation**: Use hashes (erlang:phash2/1) with 64-bit or 128-bit width, accept small collision risk for v1, add option to store full states if needed for debugging.
