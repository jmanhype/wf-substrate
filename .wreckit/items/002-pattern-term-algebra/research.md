# Research: Implement pattern term algebra and derived patterns

**Date**: 2025-01-10
**Item**: 002-pattern-term-algebra

## Research Question

Implement wf_term.erl: the closed pattern algebra AST with constructors: task/2 (named task with metadata), seq/2 (sequential composition), par/1 (parallel split, list of branches), xor/1 (exclusive choice, list of branches), join/2 (synchronization with policy), loop/2 (structured loop with condition and body), defer/1 (deferred/lazy evaluation), cancel/2 (cancel scope with region and body), mi/2 (multiple instances with config). Include full -type and -spec declarations for every constructor and the wf_term() union type. Provide smart constructors that validate structural invariants (e.g., par requires at least 2 branches, join policy is valid). Implement structural validation: well_formed/1 that checks nesting rules, no orphan joins, cancel scopes are properly nested.

Also implement wf_core.erl with derived/composite patterns built from the kernel primitives: simple_merge (xor converging to single continuation), synchronizing_merge (par branches converging with full sync), discriminator (par with first-complete join that cancels remaining), n_out_of_m (par with wait-n join policy). Each derived pattern is a function that returns a wf_term().

## Summary

This task involves creating the foundational algebraic data types (ADT) for the workflow pattern substrate. The implementation requires two main modules: `wf_term.erl` defining the core AST with 9 kernel constructors (task, seq, par, xor, join, loop, defer, cancel, mi), and `wf_core.erl` providing 4 derived pattern functions that compose these primitives. The item.json overview specifies `task/2` as having "metadata" (not just a name), suggesting the task constructor should be `task/2` with parameters for task identifier/name and associated metadata map.

The project currently has a complete rebar3 scaffold (from item 001) but no workflow logic exists yet. This is the first implementation item that introduces the actual domain logic. The implementation must follow Erlang/OTP conventions with full type specifications (`-type` and `-spec`), provide smart constructors that validate invariants (e.g., par requires ≥2 branches, xor requires ≥2 branches, join policies must be valid atoms or tuples), and implement structural validation (`well_formed/1`) to ensure terms respect nesting rules and cancellation scope boundaries.

Key architectural constraints from PROMPT.md:19-26 mandate pure Erlang only (no NIFs/ports), compiled execution (not interpreted AST walking), and explicit representation of "in-progress" state. The term algebra serves as the authoring surface that will be compiled to bytecode in item 004. The context type (`ctx()`) represents user state and token data, serving as the "data" parameter flowing through workflow transformations.

## Current State Analysis

### Existing Implementation

**Current State**: The project has a complete rebar3 scaffold from item 001 but **no workflow pattern implementation exists yet**. The repository contains only OTP boilerplate:

- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration with OTP 26+ requirement, warnings_as_errors enabled, no external dependencies
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file with empty modules list (auto-discovery)
- `/Users/speed/wf-substrate/src/wf_substrate_app.erl:1-43` - Application callback (starts supervisor)
- `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:1-51` - Empty top-level supervisor
- `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder public API (commented exports)
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:1-16` - Empty EUnit test suite
- `/Users/speed/wf-substrate/docs/README.md:1-22` - Documentation placeholder

**Evidence from glob searches**:
- No `wf_term.erl` or `wf_core.erl` modules exist
- No workflow-related source files in `src/`
- No pattern implementation tests
- Pattern algebra is entirely unimplemented

**Project Requirements Context** (from PROMPT.md):

The specification defines the pattern algebra in section 2.2 (lines 106-125):

```
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
```

**Key Discrepancy Noted**: The item.json overview specifies `task/2` with "named task with metadata", but PROMPT.md:110 shows `task/2` with `Name :: atom()` and `Fun :: function()`. This suggests the metadata should be the second parameter, possibly as a map containing the function and other metadata fields. The research recommends resolving this by defining `task/2` as `task(Name, Metadata)` where `Metadata` is a map containing required fields like `function`, optional timeout, retry policy, etc.

**Module Role Definition** (PROMPT.md:50-51, 322):
- Role 1 (this item): "Algebra/AST designer (wf_term, smart constructors, invariants)"
- This is the foundational module that all other components depend on
- wf_compile.erl (item 004) will consume wf_term() types
- wf_exec.erl (item 005) will execute compiled forms, not raw terms

## Key Files

### Existing Files (Dependencies)

- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration already set up with OTP 26+, warnings_as_errors, no deps. New modules will be auto-discovered.
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:12` - `{modules, []}` uses auto-discovery, so new wf_term.erl and wf_core.erl will be automatically included
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:9` - Already includes `-include_lib("eunit/include/eunit.hrl")`, ready for test additions
- `/Users/speed/wf-substrate/PROMPT.md:106-125` - Authoritative definition of kernel primitives and derived patterns
- `/Users/speed/wf-substrate/PROMPT.md:97-104` - Definition of kernel types: ctx(), scope_id(), case_id(), receipt()

### Files to Create

- `src/wf_term.erl` - Core pattern algebra AST module (primary deliverable)
- `src/wf_core.erl` - Derived pattern functions (primary deliverable)
- `test/wf_term_tests.erl` - Unit tests for wf_term constructors and validation
- `test/wf_core_tests.erl` - Unit tests for derived patterns

## Technical Considerations

### Dependencies

**External Dependencies**: None (pure Erlang/OTP only per PROMPT.md:19-21)

**Standard OTP Applications Needed**:
- `kernel` - Core types and processes
- `stdlib` - Data structures, lists, maps, proplists
- No additional OTP apps beyond what's already in wf_substrate.app.src

**Internal Module Dependencies**:
- `wf_term.erl` has **no dependencies** on other wf_ modules (it's the foundation)
- `wf_core.erl` depends on `wf_term.erl` (imports wf_term() type and constructors)
- Both modules are standalone and can be developed/tested in isolation
- Future items will depend on these: wf_compile (item 004), wf_validate (item 013)

### Type System Design

**Core Type Definitions Needed** (based on PROMPT.md:97-104, 106-118):

```erlang
%% Context type - user state and token data flowing through workflow
-type ctx() :: map().

%% Scope identifier for cancellation regions
-type scope_id() :: term().

%% Case identifier for workflow instances
-type case_id() :: term().

%% Receipt for effects and commits
-type receipt() :: term().

%% Task function signature
-type task_fun() :: fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, term(), ctx()}).

%% Task metadata - map containing function and optional fields
-type task_metadata() :: #{
    function => task_fun(),
    timeout => timeout() | undefined,
    retry => non_neg_integer() | undefined,
    description => binary() | undefined
}.

%% Join policies
-type join_policy() :: all | sync_merge | {first_n, pos_integer()} | {n_of_m, pos_integer(), pos_integer()}.

%% Loop policies
-type loop_policy() :: while | until | {count, non_neg_integer()}.

%% Multiple instance policies
-type mi_policy() :: {fixed, pos_integer()} | {dynamic, pos_integer(), pos_integer()}.

%% Cancellation scope specification
-type cancel_scope() :: scope_id() | {scope_id(), [cancel_option()]}.
-type cancel_option() :: {timeout, timeout()} | {notify, pid()}.

%% Pattern algebra - the union type
-type wf_term() ::
    {task, atom(), task_metadata()} |
    {seq, wf_term(), wf_term()} |
    {par, [wf_term()]} |
    {xor, [wf_term()]} |
    {join, join_policy(), [wf_term()]} |
    {loop, loop_policy(), wf_term()} |
    {defer, [wf_term()]} |
    {cancel, cancel_scope(), wf_term()} |
    {mi, mi_policy(), wf_term()}.
```

**Rationale**:
- Using tagged tuples (not records) for immutable, structural equality
- Open-ended types (ctx(), scope_id(), case_id()) as `term()` to allow flexibility
- Explicit policy types as unions of atoms and tuples for clear semantics
- Task metadata as map for extensibility (matches "metadata" description in item.json)

### Patterns to Follow

**Erlang/OTP Conventions**:
- Use `-opaque` for wf_term() to hide internal representation (enforces use of smart constructors)
- Provide `-spec` for every exported function
- Use `-callback` attributes if defining behaviour (not applicable here)
- Follow naming convention: constructor functions same as tag name (e.g., `task/2` creates `{task, ...}`)
- Export both raw constructors (for internal use) and smart constructors (with validation)

**Validation Pattern** (to be implemented):
```erlang
%% Smart constructor with validation
-spec task(Name :: atom(), Metadata :: task_metadata()) -> wf_term().
task(Name, Metadata) ->
    %% Validate Name is atom
    true = is_atom(Name),
    %% Validate Metadata has required 'function' key
    true = maps:is_key(function, Metadata),
    %% Validate function is a fun
    Fun = maps:get(function, Metadata),
    true = is_function(Fun),
    {task, Name, Metadata}.
```

**Structural Validation Pattern** (well_formed/1):
- Recursive traversal of wf_term() tree
- Check invariants at each node:
  - par: length(Branches) >= 2
  - xor: length(Branches) >= 2
  - join: length(Branches) >= 2, policy is valid
  - defer: length(Branches) >= 2
  - cancel: scope_id() is valid term
  - mi: policy is valid tuple
- Track nesting context for cancel scopes (parent-child relationships)
- Return `ok | {error, [validation_error()]}`

**Derived Pattern Pattern** (wf_core.erl):
```erlang
%% simple_merge: XOR choice converging to single continuation
-spec simple_merge([wf_term()], wf_term()) -> wf_term().
simple_merge(Alternatives, Continuation) ->
    %% Validate at least 2 alternatives
    true = length(Alternatives) >= 2,
    xor([seq(Alt, Continuation) || Alt <- Alternatives]).

%% synchronizing_merge: Parallel branches converging with full sync
-spec synchronizing_merge([wf_term()], wf_term()) -> wf_term().
synchronizing_merge(Branches, Continuation) ->
    true = length(Branches) >= 2,
    seq(par([seq(Branch, Continuation) || Branch <- Branches]),
        %% Join waits for all branches
        join(all, [])).

%% discriminator: First-complete join that cancels remaining
-spec discriminator([wf_term()], wf_term()) -> wf_term().
discriminator(Branches, Continuation) ->
    true = length(Branches) >= 2,
    %% Wrap each branch in cancel scope
    ScopedBranches = [cancel({scope, make_ref()}, seq(Branch, Continuation))
                      || Branch <- Branches],
    %% Join on first completion
    seq(join({first_n, 1}, ScopedBranches),
        %% Cancel all remaining scopes
        cancel_all_remaining(ScopedBranches)).

%% n_out_of_m: Wait for N out of M branches
-spec n_out_of_m(pos_integer(), [wf_term()], wf_term()) -> wf_term().
n_out_of_m(N, Branches, Continuation) when N > 0, N =< length(Branches) ->
    seq(par([seq(Branch, Continuation) || Branch <- Branches]),
        join({n_of_m, N, length(Branches)}, [])).
```

**Integration with Future Items**:
- Item 003 (architecture docs): Will document these types and their semantics
- Item 004 (compiler): Will consume wf_term() and compile to bytecode
- Item 013 (validation): Will expand well_formed/1 with bounded model checking
- Items 014-016 (tests): Will add comprehensive test coverage

### Smart Constructor vs Raw Constructor Design

**Two-Tier API Pattern**:

1. **Raw Constructors** (private/internal):
   - Simply create the tagged tuple without validation
   - Fast, no overhead
   - Used internally by derived patterns and compiler
   - Exported but prefixed with underscore (e.g., `_task/2`)

2. **Smart Constructors** (public API):
   - Validate all invariants before creating term
   - Provide clear error messages for invalid inputs
   - Used by external code and user-facing APIs
   - Exported normally (e.g., `task/2`)

**Rationale**: This pattern balances safety (validation for external input) with performance (no validation overhead for internal operations). The compiler (item 004) can use raw constructors since it generates valid terms by construction.

### Validation Error Reporting

**Structured Error Type**:
```erlang
-type validation_error() ::
    {invalid_branch_count, node_type(), pos_integer(), pos_integer()} |
    {invalid_join_policy, join_policy()} |
    {invalid_loop_policy, loop_policy()} |
    {invalid_mi_policy, mi_policy()} |
    {orphan_join, join_policy(), [wf_term()]} |
    {malformed_cancel_scope, cancel_scope()} |
    {nested_cancel_scope, scope_id(), scope_id()}.
```

**Error Accumulation Pattern**:
```erlang
-spec well_formed(wf_term()) -> ok | {error, [validation_error()]}.
well_formed(Term) ->
    case validate_node(Term, #{}) of
        {ok, _} -> ok;
        {error, Errors} -> {error, lists:usort(Errors)}
    end.
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **Task/2 Metadata Ambiguity** | Medium | The item.json says "named task with metadata" but PROMPT.md shows `task(Name, Fun)`. Research recommends: task/2 takes (Name, MetadataMap) where MetadataMap must contain `function` key. Document this decision clearly in module docs. |
| **Join Policy Complexity** | Medium | Join policies (all, sync_merge, first_n, n_of_m) have subtle semantic differences. Mitigation: Define clear -type for join_policy(), provide examples in EUnit tests, document semantics in comments. Note that full semantics will be in item 003 (docs/SEMANTICS.md). |
| **Circular Dependency Risk** | Low | wf_term and wf_core must not depend on future modules (wf_compile, wf_exec). Keep them pure data structures with no process spawning or side effects. |
| **Overly Strict Validation** | Medium | well_formed/1 could reject valid but unusual terms. Mitigation: Start with basic invariants (branch counts, type checks), expand validation incrementally in item 013. Document what is NOT checked. |
| **Opaque Type vs Pattern Matching** | Low | If wf_term() is -opaque, external code cannot pattern match. Mitigation: Use -type (transparent) initially for flexibility, consider -opaque in future if encapsulation needed. Export accessors if needed. |
| **Performance of Validation** | Low | Recursive well_formed/1 could be expensive on large terms. Mitigation: Validation is compile-time only (not in hot path). Use tail recursion where possible. |
| **Derived Pattern Composition** | Medium | Complex derived patterns (discriminator with cancel semantics) may be hard to implement correctly. Mitigation: Start with simple_merge and synchronizing_merge (simpler), implement discriminator and n_out_of_m after join policies are tested. |
| **Test Coverage Gap** | High | Without comprehensive tests, invalid terms could pass validation. Mitigation: Write EUnit tests for all invariants, including negative cases (invalid terms should fail). Property-based tests in items 014-016 will catch edge cases. |

## Recommended Approach

**High-Level Strategy**: Implement wf_term.erl first (foundation), then wf_core.erl (depends on wf_term), with parallel test development. Follow a test-driven approach where each constructor has corresponding unit tests covering valid and invalid inputs.

### Phase 1: Core Type Definitions (wf_term.erl - Types)

1. **Define Context and Support Types**:
   - Create opaque type declarations for ctx(), scope_id(), case_id(), receipt()
   - Define task_fun(), task_metadata() as maps with required 'function' key
   - Define join_policy(), loop_policy(), mi_policy() as union types
   - Define cancel_scope() and cancel_option()

2. **Define wf_term() Union Type**:
   - Create -type wf_term() as tagged union of all 9 constructors
   - Use explicit tag names matching constructor names
   - Document each variant in comments

3. **Add -opaque Decision**:
   - Use -type (transparent) initially for flexibility
   - Document rationale in module header
   - Can change to -opaque in future if needed

### Phase 2: Raw Constructors (wf_term.erl - Internal)

1. **Implement Raw Constructors**:
   - Create `task/2`, `seq/2`, `par/1`, `xor/1`, `join/2`, `loop/2`, `defer/1`, `cancel/2`, `mi/2`
   - Prefix with underscore or document as "raw" (e.g., `-export([task/2, ...])` with "Use smart constructors for validation")
   - Simply create and return tagged tuples
   - No validation (fast path for internal use)

2. **Add Type Specs**:
   - Every raw constructor gets full -spec declaration
   - Use types defined in Phase 1
   - Ensure Dialyzer will pass

### Phase 3: Smart Constructors (wf_term.erl - Public)

1. **Implement Smart Constructors**:
   - Export same function names as raw constructors (shadow them)
   - Each function validates inputs before creating term
   - Return wf_term() or throw `badarg` with descriptive message
   - Use guards for type checks (is_atom, is_list, is_map, etc.)

2. **Validation Logic per Constructor**:
   - `task/2`: Check Name is atom, Metadata is map with 'function' key containing fun
   - `seq/2`: Validate both terms are wf_term() (recursive check or defer to well_formed)
   - `par/1`: Validate list length >= 2, all elements are wf_term()
   - `xor/1`: Validate list length >= 2, all elements are wf_term()
   - `join/2`: Validate list length >= 2, policy is valid join_policy()
   - `loop/2`: Validate policy is valid loop_policy(), body is wf_term()
   - `defer/1`: Validate list length >= 2, all elements are wf_term()
   - `cancel/2`: Validate scope is term(), body is wf_term()
   - `mi/2`: Validate policy is valid mi_policy(), body is wf_term()

3. **Error Messages**:
   - Use `erlang:error(badarg, [Reason, Input])` for clear failure
   - Example: `error({badarg, {invalid_branch_count, par, 1, 2}}, [Branches])`

### Phase 4: Structural Validation (wf_term.erl - well_formed)

1. **Implement well_formed/1**:
   - Recursive function traversing wf_term() tree
   - Accumulates validation errors in list
   - Returns `ok | {error, [validation_error()]}`

2. **Validation Checks**:
   - Branch count invariants (par, xor, defer require ≥2)
   - Join policy validity
   - Cancel scope nesting (track scope stack, ensure proper parent-child)
   - No orphan joins (joins not matching corresponding splits)
   - Type consistency (all list elements are wf_term())

3. **Helper Functions**:
   - `validate_node/2`: Validates single node, tracks context
   - `validate_join_policy/1`: Checks policy format
   - `validate_loop_policy/1`: Checks policy format
   - `validate_mi_policy/1`: Checks policy format
   - `check_cancel_nesting/2`: Ensures cancel scopes don't cross

### Phase 5: Derived Patterns (wf_core.erl)

1. **Implement simple_merge/2**:
   - Input: [wf_term()] alternatives, wf_term() continuation
   - Output: wf_term() using xor + seq composition
   - Validate ≥2 alternatives
   - Pattern: `xor([seq(Alt, Continuation) || Alt <- Alternatives])`

2. **Implement synchronizing_merge/2**:
   - Input: [wf_term()] branches, wf_term() continuation
   - Output: wf_term() using par + join(all) + seq
   - Validate ≥2 branches
   - Pattern: `seq(par([...]), join(all, []))` with continuations embedded

3. **Implement discriminator/2**:
   - Input: [wf_term()] branches, wf_term() continuation
   - Output: wf_term() using par + join({first_n, 1}) + cancel
   - Most complex derived pattern
   - Each branch gets unique cancel scope (use make_ref())
   - After first completion, cancel remaining scopes

4. **Implement n_out_of_m/3**:
   - Input: N (pos_integer), [wf_term()] branches, wf_term() continuation
   - Output: wf_term() using par + join({n_of_m, N, M})
   - Validate 1 ≤ N ≤ M
   - Pattern: `seq(par([...]), join({n_of_m, N, M}, []))`

5. **Add -specs and Documentation**:
   - Each derived pattern has full -spec
   - Module doc explains relationship to kernel primitives
   - Function docs describe semantics (informal, formal in item 003)

### Phase 6: Testing (wf_term_tests.erl and wf_core_tests.erl)

1. **wf_term_tests.erl**:
   - Test each smart constructor with valid inputs
   - Test each smart constructor with invalid inputs (should fail)
   - Test well_formed/1 with valid terms
   - Test well_formed/1 with invalid terms (branch counts, policies, nesting)
   - Test raw constructors (ensure they create correct tuples)

2. **wf_core_tests.erl**:
   - Test simple_merge/2 with 2+ alternatives
   - Test simple_merge/2 with 0-1 alternatives (should fail)
   - Test synchronizing_merge/2 with 2+ branches
   - Test discriminator/2 with 2+ branches
   - Test n_out_of_m/3 with various N/M combinations
   - Test edge cases (N=1, N=M, N=M-1)

3. **Property-Based Tests** (foundational for items 014-016):
   - Generate random wf_term() terms within bounds
   - Assert well_formed/1 passes for generated terms
   - Assert derived patterns produce well_formed terms

### Phase 7: Integration and Verification

1. **Compilation Check**:
   - `rebar3 compile` must succeed
   - No warnings (warnings_as_errors is enabled)
   - Dialyzer should pass (configured in rebar.config:17-23)

2. **Test Execution**:
   - `rebar3 eunit` must pass
   - All tests should cover positive and negative cases

3. **Documentation**:
   - Module docs explain purpose and usage
   - Function docs have -spec and clear descriptions
   - Comments explain non-obvious decisions

4. **Preparation for Next Items**:
   - Ensure wf_term() type is ready for wf_compile (item 004)
   - Ensure derived patterns can be compiled
   - No dependencies on future modules

**Rationale**: This phased approach isolates type definitions, constructors, validation, derived patterns, and testing into manageable steps. Each phase produces verifiable output (types compile, specs pass, tests run). Starting with raw then smart constructors allows internal flexibility while maintaining external safety. Derived patterns are implemented last since they compose kernel primitives.

## Open Questions

1. **Task Metadata Format**: The item.json specifies "task/2 (named task with metadata)" but PROMPT.md:110 shows `task(Name :: atom(), Fun :: fun(...))`. Should the second parameter be a pure function or a metadata map containing the function?
   - **Recommendation**: Use `task(Name, Metadata)` where `Metadata` is a map `#{function => fun(), ...}`. This provides extensibility for timeout, retry policy, description, etc. The 'function' key is required. This aligns with "metadata" description and allows future expansion.

2. **Context Type Definition**: Should `ctx()` be an opaque type or just `map()`?
   - **Recommendation**: Keep it as `-type ctx() :: map().` for maximum flexibility. Users can structure their context arbitrarily. Can add opaque wrapper later if needed for validation.

3. **Join Policy Representation**: Should join policies be atoms only or allow structured tuples like `{first_n, 1}`?
   - **Recommendation**: Use union type `-type join_policy() :: all | sync_merge | {first_n, pos_integer()} | {n_of_m, pos_integer(), pos_integer()}.` This matches PROMPT.md:114 and provides necessary expressiveness.

4. **Cancellation Scope Identification**: Should scope_id() be a specific type or generic term()?
   - **Recommendation**: Use `-type scope_id() :: term().` to allow users to define meaningful scope identifiers (atoms, integers, refs, tuples). Validation only checks that scopes are properly nested, not their format.

5. **Raw Constructor Export Policy**: Should raw constructors be exported (with underscore prefix) or kept private?
   - **Recommendation**: Export them normally but document as "raw constructors without validation - use smart constructors for public API". Internal modules (wf_compile) can use raw constructors for performance. Alternatively, don't export raw constructors at all and only expose smart constructors.

6. **Validation Strictness**: Should well_formed/1 check only local invariants (branch counts, types) or also global properties (no deadlock, option to complete)?
   - **Recommendation**: Check only local structural invariants in this item. Global properties (deadlock, soundness) require bounded model checking and will be implemented in item 013 (wf_validate). Document this scope boundary clearly.

7. **Derived Pattern Implementation Details**: For discriminator, should the cancel scope be user-specified or auto-generated?
   - **Recommendation**: Auto-generate unique scope IDs using `make_ref()` for each branch in discriminator. This ensures no scope collisions and simplifies the API. Users who need manual control can use the raw cancel/2 constructor directly.

8. **Error Reporting Format**: Should validation return `{error, [validation_error()]}` or throw exceptions?
   - **Recommendation**: `well_formed/1` should return `{error, [...]}` for programmatic checking (no throws). Smart constructors should throw `badarg` with descriptive messages for immediate failures (invalid inputs at construction time). This follows Erlang conventions (let it crash for bad input, but provide structured validation API).

9. **Test Coverage for Negative Cases**: How comprehensive should negative test coverage be for well_formed/1?
   - **Recommendation**: Test all specified invariants (branch counts, policy formats) but don't exhaustive-test every possible invalid term. Property-based tests in items 014-016 will catch edge cases. Focus on clear error messages for common mistakes.

10. **Interaction with Future Compiler**: Should wf_term include any compilation hints or metadata to help wf_compile?
    - **Recommendation**: Keep wf_term as pure data structure. No compilation hints needed. The compiler (item 004) will do a single recursive pass over the term. Adding metadata now would couple the modules prematurely.
