You are a 20-agent "code swarm" implementing a production-grade **pure Erlang** control substrate whose *only* authoring primitives are the **Workflow Patterns** (the "43 workflow patterns" family: control-flow focus first, with an extension path to data/resource/cancellation patterns). The goal is NOT to build "a workflow engine that interprets workflow data." The goal is to make workflow-pattern control **native**: patterns compile into executable control structures with minimal indirection and tight latency, fully testable, replayable, observable, and safe under failure.

You will produce a complete Erlang/OTP application with:
- A pattern-term algebra (AST) and kernel combinators
- A compiler from pattern terms into an executable representation (bytecode or continuation network) that does NOT "walk a graph and dispatch per node" at runtime
- A reducer/executor that applies pattern semantics as direct operations
- Deterministic scheduling option (Lambda policy)
- Structured cancellation (activity/case/region) and compensation hooks
- Multiple instance support (fixed and dynamic)
- A rigorous validation suite: property-based tests, model checks (bounded), determinism tests, replay tests, and performance tests
- Full OTP supervision and resilience, no NIFs, no ports, no external dependencies beyond standard Erlang/OTP libs

Your output must include design docs, code, and tests. Assume OTP 26+.

====================================================================
0. GLOBAL CONSTRAINTS (NON-NEGOTIABLE)
====================================================================

0.1 Pure Erlang Only
- Do not use Rust, NIFs, ports, external executables, or non-stdlib deps.
- Use OTP behaviours (gen_server/gen_statem/supervisor) as needed.

0.2 No "Workflow As Data Interpreted By Engine"
- The workflow term may exist at authoring time and compile time.
- Runtime must not repeatedly interpret node types in a slow dispatch loop.
- Runtime must operate on a compiled executable form (bytecode + direct reducer OR compiled continuation network).
- Avoid per-step "case NodeType of ..." in the hot loop.

0.3 One Coherent State Store per Case/Shard
- Avoid shadow state duplication across multiple engines.
- State updates are atomic at commit boundaries ("no partials").
- Every side effect must be mediated by explicit effect boundaries with receipts.

0.4 Pattern Algebra is the Source of Truth
- Users author processes only using pattern constructors / derived macros.
- No arbitrary Petri net graphs exposed to users.
- Internal compilation to Petri nets is allowed ONLY as a validation backend, not as the primary runtime model.

0.5 Observability and Replay Are First-Class
- Every reduction step produces structured trace events.
- A run can be replayed deterministically (when configured).

====================================================================
1. DELIVERABLES
====================================================================

Deliverable A: Erlang/OTP Application
- app: wf_substrate
- modules:
  - wf_term.erl           : AST + constructors + smart constructors
  - wf_core.erl           : kernel pattern basis + derived patterns library
  - wf_compile.erl        : compiler from wf_term() to executable form
  - wf_vm.erl             : bytecode definition + interpreter (if VM path)
  - wf_exec.erl           : reducer/executor (hot loop)
  - wf_sched.erl          : scheduling policies (Lambda)
  - wf_state.erl          : case state store + atomic commit protocol
  - wf_cancel.erl         : cancellation semantics (activity/case/region)
  - wf_mi.erl             : multiple instance semantics
  - wf_effect.erl         : effect system (tool calls / IO) boundary
  - wf_receipt.erl        : receipts, hashing, causal ids
  - wf_trace.erl          : structured tracing & replay log
  - wf_validate.erl       : structural checks, bounded checks, soundness checks
  - wf_test_*.erl         : unit + property tests + determinism tests
  - wf_bench.erl          : microbench harness
  - wf_example_*.erl      : examples implementing patterns

Deliverable B: Documentation
- docs/ARCHITECTURE.md: semantics, reduction model, compilation strategy
- docs/SEMANTICS.md: small-step semantics for kernel patterns
- docs/PATTERNS.md: mapping of the 43 patterns to terms to compiled forms
- docs/TESTING.md: what is tested, how replay is validated, bounded checks
- docs/OPERATIONS.md: supervision, failure modes, telemetry events

Deliverable C: Pattern Coverage
- Implement at least these control-flow patterns fully, with executable semantics and tests:
  - Sequence
  - Parallel Split (AND-split)
  - Synchronization (AND-join)
  - Exclusive Choice (XOR-split)
  - Simple Merge (XOR-join)
  - Multi-choice
  - Synchronizing Merge
  - Discriminator
  - N-out-of-M Join
  - Deferred Choice
  - Arbitrary Cycles (Loop)
  - Cancel Activity
  - Cancel Case
  - Cancel Region
  - Multiple Instances (fixed, dynamic) with and without sync
  - (Provide scaffolding for remaining patterns with explicit TODO + test stubs)

====================================================================
2. CONCEPTUAL MODEL (FORMALIZE THIS)
====================================================================

2.1 Kernel Types / Sorts (Erlang terms)
- place(), trsn() may exist for validation backend
- proc() is the user-authored pattern term
- exec() is compiled executable form
- ctx() is case context (user state, token data, etc.)
- scope_id() identifies cancellation scopes
- case_id() identifies cases
- receipt() for effects and commits

2.2 Terms: The Pattern Algebra
Define wf_term() as a closed algebra generated only by constructors:

Required primitives (kernel basis):
- task(Name :: atom(), Fun :: fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, EffectSpec, ContCtx}))
- seq(P,Q)
- par(ListOfP)                      % AND-split/AND-join skeleton
- xor(ListOfP)                      % exclusive choice
- join(Policy, ListOfP)             % generalized join, policy includes all/first/n_of_m/sync_merge
- loop(Policy, P)                   % arbitrary cycles; Policy determines exit condition
- defer(ListOfP)                    % deferred choice (race)
- cancel(ScopeSpec, P)              % scope/region cancellation wrapper
- mi(Policy, P)                     % multiple instances wrapper

Derived patterns (library macros):
- simple_merge = join(xor_merge, ...)
- synchronizing_merge = join(sync_merge, ...)
- discriminator = join({first_n,1}, ...) + cancel semantics for remaining branches (configurable)
- n_out_of_m = join({n_of_m,N}, ...)
- etc.

2.3 Semantics: Small-Step Reduction
Write the semantics as rules (in docs/SEMANTICS.md) for each primitive:
- seq reduces left; on completion moves to right
- par spawns logical branches; join policy collects completions
- xor selects a branch; cancels others (or never spawns them, depending on form)
- defer races on first external event or branch completion
- cancel propagates cancellation signals to scoped subterms
- mi spawns instances and joins per policy
- loop repeats until condition satisfied

IMPORTANT: define explicit representation of "in-progress" state in exec() so the reducer doesn't need to interpret AST each step.

2.4 Determinism (Lambda)
Provide scheduler wf_sched:
- deterministic policy: stable ordering of enabled actions, no random picks
- nondeterministic policy: allowed for exploration tests, but record choices for replay
- replay mode: feed recorded choices; must reproduce trace exactly

2.5 Effects Boundary
task/2 can request external effects:
- {effect, Spec, ContCtx} means yield to wf_effect manager
- wf_effect executes Spec, returns Result
- reducer resumes continuation with Result, producing a receipt

All effects must:
- have unique causal ids
- be cancelable when supported
- be idempotent or have explicit "at most once" semantics via receipts

====================================================================
3. IMPLEMENTATION STRATEGY (CHOOSE ONE, DOCUMENT WHY)
====================================================================

You must pick one of these runtime strategies and implement it. You may implement both if time allows, but at least one must be complete.

Strategy S1: Bytecode VM
- Compile wf_term() to wf_bc() list of opcodes
- Opcodes are pattern primitives (SEQ_ENTER, PAR_FORK, JOIN_WAIT, XOR_CHOOSE, LOOP_BACK, CANCEL_SCOPE, MI_SPAWN, EFFECT_YIELD, etc.)
- wf_exec runs a tight loop executing opcodes and mutating an explicit exec_state record
- No per-step AST dispatch

Strategy S2: Continuation Network
- Compile wf_term() to a network of closures / continuation frames
- Execution is a reducer stepping frames
- Must avoid "interpret node type" in hot path by precomputing continuation functions

Whichever is chosen must:
- Support cancellation propagation efficiently (O(size of cancelled scope) or better)
- Support join policies without scanning whole graphs each step (use indexes)

====================================================================
4. OTP DESIGN REQUIREMENTS
====================================================================

4.1 Supervision Tree
- wf_substrate_sup supervises:
  - wf_case_sup (simple_one_for_one / dynamic supervisor) supervising per-case runners
  - wf_effect_sup supervising effect workers (optional)
  - wf_trace_sink (optional) for logging aggregation

4.2 Per-Case Runner
- A case is a process (gen_server or gen_statem):
  - holds exec_state
  - steps reducer in bursts (avoid monopolizing scheduler)
  - handles inbound messages: signals, effect results, cancel requests
  - produces trace events

4.3 Backpressure and Scheduling
- Step execution in configurable quanta (e.g., N reductions per tick) then hibernate/yield
- Avoid busy loops
- Provide stats: steps/sec, effects/sec, cancel latency, memory usage

====================================================================
5. VALIDATION AND TEST PLAN (MANDATORY)
====================================================================

5.1 Unit Tests
- Test each primitive constructor semantics in isolation
- Test join policies (all, xor_merge, sync_merge, first_n, n_of_m)
- Test cancellation (activity/case/region) correctness

5.2 Property-Based Tests (use proper:quickcheck if available? If not, write your own minimal generator OR use common_test + random with seeds)
Since stdlib doesn't include PropEr, implement a minimal generator framework or embed lightweight property testing:
- generate random small pattern terms within bounds
- run under deterministic scheduler
- assert invariants:
  - no negative tokens / no invalid states
  - replay reproduces trace
  - cancellation eventually stops cancelled scopes
  - completed case reaches terminal state (or reports deadlock explicitly)

5.3 Bounded Model Checks (Validation Backend)
Implement a "validation backend" that compiles a term into a simplified Petri net or LTS for bounded checking:
- bounded state exploration up to depth D and token bound K
- check:
  - no dead transitions (when applicable)
  - option to complete (bounded)
  - proper completion (bounded)
  - deadlock detection
This backend is NOT the runtime; it is a test/validation tool.

5.4 Determinism & Replay Tests
- Run same case twice with deterministic policy: identical trace
- Run with nondeterministic policy but record choices; replay must match
- Verify receipts stable across replay for pure steps (effects may be mocked)

5.5 Performance Tests
- Microbench: sequence of 10k task steps (pure)
- Microbench: par of 100 branches with join
- Microbench: repeated discriminator patterns
Targets:
- show overhead per step is bounded and does not scale with AST size in a naive way
- show cancellation is efficient

====================================================================
6. PATTERN IMPLEMENTATION MAPPING (YOU MUST PRODUCE THIS TABLE)
====================================================================

In docs/PATTERNS.md create a table:
- Pattern name
- Term form (kernel + derived)
- Compilation sketch (opcodes or continuation frames)
- Key tests
- Known corner cases

At minimum fully implement & test these:
- Sequence
- Parallel Split
- Synchronization
- Exclusive Choice
- Simple Merge
- Multi-choice
- Synchronizing Merge
- Discriminator
- N-out-of-M Join
- Deferred Choice
- Arbitrary Cycles
- Cancel Activity
- Cancel Case
- Cancel Region
- Multiple Instances (fixed, dynamic) + join variants

====================================================================
7. API SURFACE (PUBLIC MODULES)
====================================================================

Expose a minimal public API in wf_substrate.erl:

- new_case(ProcTerm, InitCtx, Options) -> {ok, CasePid, CaseId}
- signal(CasePidOrId, Msg) -> ok
- cancel(CasePidOrId) -> ok
- cancel_region(CasePidOrId, RegionSpec) -> ok
- await(CasePidOrId, Timeout) -> {ok, ResultCtx} | {error, Reason} | timeout
- status(CasePidOrId) -> #{state := running|completed|cancelled|failed, steps := N, ...}
- trace(CasePidOrId, FromSeq, ToSeq) -> [TraceEvent]
- validate(ProcTerm, Options) -> ok | {error, [ValidationIssue]}

Options include:
- scheduler_policy: deterministic | nondeterministic | replay(ChoiceLog)
- step_quanta: integer()
- trace_level: none|min|full
- effect_handler: module()

====================================================================
8. SECURITY / SAFETY / GOVERNANCE FEATURES (GCP-READY)
====================================================================

Because the eventual product is intended for a marketplace, implement:
- Tool/effect allowlist per scope
- Maximum effect budget per case (count, time, cost stub)
- Timeout policies
- Mandatory "approval points" as a pattern (task that requires explicit signal)
- Guard failures produce structured errors and receipts

====================================================================
9. EXAMPLES (MUST INCLUDE)
====================================================================

Provide example modules:
- wf_example_basic.erl: vending machine like example using patterns
- wf_example_discriminator.erl
- wf_example_cancel_region.erl
- wf_example_multiple_instances.erl
Each example must have:
- a runnable demo function
- expected trace output
- tests that assert the trace structure

====================================================================
10. SWARM COORDINATION: 20 AGENT ROLES
====================================================================

Split the work among 20 roles; each role produces code + tests + docs segments.

Roles:
1. Algebra/AST designer (wf_term, smart constructors, invariants)
2. Kernel semantics author (docs/SEMANTICS.md, rule definitions)
3. Compiler lead (wf_compile: term to exec)
4. VM/opcode designer (wf_vm) OR continuation lead
5. Reducer hot loop implementer (wf_exec)
6. Scheduler policy implementer (wf_sched) incl replay
7. State store + atomic commit (wf_state)
8. Cancellation semantics (wf_cancel)
9. Multiple instances semantics (wf_mi)
10. Effect boundary + receipts (wf_effect, wf_receipt)
11. Tracing + structured events (wf_trace)
12. Validator backend (wf_validate) incl bounded exploration
13. Core unit tests for primitives
14. Join policy correctness tests
15. Cancellation tests
16. MI tests
17. Determinism/replay tests
18. Performance bench harness
19. Example implementations + docs
20. Build/rebar3 config, CI scripts, packaging, style checks

Swarm outputs must integrate cleanly with rebar3:
- rebar.config
- app.src
- eunit/common_test suites

====================================================================
11. ACCEPTANCE CRITERIA
====================================================================

The implementation is accepted only if:
- `rebar3 compile` succeeds
- `rebar3 eunit` succeeds
- `rebar3 ct` succeeds
- Determinism/replay tests pass
- Validator can detect at least one known deadlock case
- Cancellation correctly terminates targeted scopes without corrupting unrelated scopes
- Benchmarks run and report steps/sec metrics
- Docs explain compilation strategy and semantics clearly

====================================================================
12. START NOW
====================================================================

Start by writing:
1) docs/ARCHITECTURE.md (high-level module graph + runtime strategy)
2) docs/SEMANTICS.md (small-step rules for kernel patterns)
3) wf_term.erl (AST + constructors)
4) wf_compile.erl + wf_exec.erl skeleton
5) One fully working pattern: Sequence + task
6) Tests for Sequence determinism + trace

Then iterate pattern by pattern until coverage goals met.
