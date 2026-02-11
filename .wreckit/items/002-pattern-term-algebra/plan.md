# Implement pattern term algebra and derived patterns Implementation Plan

## Implementation Plan Title

Pattern Term Algebra: Core AST, Smart Constructors, and Derived Patterns for Workflow Pattern Substrate

## Overview

This implementation creates the foundational algebraic data types (ADT) for the workflow pattern substrate. We will implement two core modules:

1. **wf_term.erl**: Defines the closed pattern algebra AST with 9 kernel constructors (task, seq, par, xor, join, loop, defer, cancel, mi), complete with type declarations, smart constructors with validation, and structural validation via `well_formed/1`.

2. **wf_core.erl**: Implements 4 derived/composite patterns (simple_merge, synchronizing_merge, discriminator, n_out_of_m) built from kernel primitives.

This is the first implementation item introducing actual domain logic. The term algebra serves as the authoring surface that will be compiled to bytecode in item 004. All components must follow Erlang/OTP conventions with full type specifications (`-type` and `-spec`), pure functional design (no side effects), and comprehensive test coverage.

## Current State

**Existing State**: The project has a complete rebar3 scaffold from item 001 but **no workflow pattern implementation exists yet**. The repository contains only OTP boilerplate:

- `/Users/speed/wf-substrate/rebar.config:1-30` - Build configuration with OTP 26+ requirement, warnings_as_errors enabled, Dialyzer configured
- `/Users/speed/wf-substrate/src/wf_substrate.app.src:1-15` - Application resource file with `{modules, []}` (auto-discovery enabled)
- `/Users/speed/wf-substrate/src/wf_substrate_app.erl:1-43` - Application callback starting supervisor
- `/Users/speed/wf-substrate/src/wf_substrate_sup.erl:1-51` - Empty top-level supervisor
- `/Users/speed/wf-substrate/src/wf_substrate.erl:1-31` - Placeholder public API
- `/Users/speed/wf-substrate/test/wf_substrate_tests.erl:1-16` - Empty EUnit test suite with eunit header included

**Missing Components**:
- No `wf_term.erl` or `wf_core.erl` modules exist
- No workflow-related source files in `src/`
- No pattern implementation tests
- Pattern algebra is entirely unimplemented

**Key Constraints from PROMPT.md**:
- Lines 19-21: Pure Erlang only (no NIFs, ports, external dependencies)
- Lines 23-26: No "workflow as data interpreted by engine" - must compile to executable form
- Lines 106-125: Authoritative definition of kernel primitives and derived patterns
- Lines 97-104: Definition of kernel types (ctx, scope_id, case_id, receipt)

## Desired End State

**Specification**: Two fully implemented, tested, and documented modules providing the pattern term algebra:

1. **wf_term.erl** with:
   - Complete type definitions for all kernel types (ctx, scope_id, case_id, receipt, task_fun, task_metadata, join_policy, loop_policy, mi_policy, cancel_scope)
   - wf_term() union type as tagged tuples for all 9 constructors
   - Smart constructors with full validation (task/2, seq/2, par/1, xor/1, join/2, loop/2, defer/1, cancel/2, mi/2)
   - Structural validation function `well_formed/1` checking branch counts, policy validity, cancel scope nesting
   - Full -spec declarations on all exported functions
   - Module documentation explaining design decisions

2. **wf_core.erl** with:
   - simple_merge/2: XOR choice converging to single continuation
   - synchronizing_merge/2: Parallel branches with full synchronization
   - discriminator/2: First-complete join that cancels remaining branches
   - n_out_of_m/3: Wait for N out of M branches
   - Full -spec declarations and documentation

3. **Test suites**:
   - test/wf_term_tests.erl: Unit tests for all constructors and validation
   - test/wf_core_tests.erl: Unit tests for all derived patterns

4. **Verification criteria**:
   - `rebar3 compile` succeeds with no warnings
   - `rebar3 eunit` passes all tests
   - Dialyzer analysis passes
   - All constructors validate invariants correctly
   - well_formed/1 catches structural errors
   - Derived patterns compose kernel primitives correctly

### Key Discoveries:

- **Task Metadata Resolution** (research.md:63): The item.json specifies "task/2 (named task with metadata)" but PROMPT.md:110 shows `task(Name, Fun)`. Decision: Use `task(Name, Metadata)` where `Metadata` is a map `#{function => fun(), ...}` with required 'function' key. This provides extensibility for timeout, retry policy, description while matching the "metadata" description.

- **Transparent vs Opaque Type** (research.md:168-169): Initially use `-type` (transparent) for wf_term() to allow pattern matching in tests and compiler. Can change to `-opaque` in future if encapsulation needed. Document rationale in module header.

- **Two-Tier Constructor API** (research.md:245-259): Smart constructors (public, validated) vs raw constructors (internal, fast). Smart constructors throw `badarg` for invalid input; raw constructors simply create tuples. Compiler (item 004) can use raw constructors for performance.

- **Validation Scope** (research.md:471-472): well_formed/1 checks ONLY local structural invariants (branch counts, type formats, scope nesting). Global properties (deadlock, soundness) will be implemented in item 013 (wf_validate) with bounded model checking.

- **Pattern from PROMPT.md:110**: `task(Name :: atom(), Fun :: fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, EffectSpec, ContCtx}))` - Note the function signature includes effect handling for async operations.

## What We're NOT Doing

- **NOT implementing compilation or execution** (item 004): wf_term and wf_core are pure data structures with no runtime behavior
- **NOT implementing process spawning or side effects**: No gen_server, no message passing, no I/O
- **NOT implementing global validation properties** (item 013): Deadlock detection, soundness checks, bounded model checking
- **NOT implementing detailed semantics documentation** (item 003): Formal reduction rules, operational semantics
- **NOT implementing effect system or receipts** (future items): wf_effect, wf_receipt modules
- **NOT implementing cancellation execution**: Only defining the cancel/2 constructor, not the runtime cancellation propagation
- **NOT implementing multiple instances execution**: Only defining the mi/2 constructor, not the instance spawning logic
- **NOT adding compilation hints**: Keep wf_term as pure data structure; compiler will do its own analysis
- **NOT using records**: Using tagged tuples for structural equality and pattern matching

## Implementation Approach

**High-Level Strategy**: Implement wf_term.erl first (foundation), then wf_core.erl (depends on wf_term), with parallel test development. Follow test-driven development where each constructor has corresponding unit tests covering valid and invalid inputs.

**Architectural Principles**:
1. **Pure Functional Design**: All constructors are pure functions with no side effects
2. **Type-First Development**: Define all types before implementing functions
3. **Validation at Boundaries**: Smart constructors validate inputs; internal operations assume valid terms
4. **Transparent Implementation**: Use `-type` (not `-opaque`) for flexibility in testing and compilation
5. **Erlang/OTP Conventions**: Full -spec declarations, module docs, EUnit tests

**Key Design Decisions**:

1. **Task Constructor Signature**: `task(Name :: atom(), Metadata :: map())` where Metadata must contain `function => fun((ctx()) -> {ok, ctx()} | {error, term()} | {effect, term(), ctx()})`. Optional keys: `timeout`, `retry`, `description`.

2. **Join Policies**: Union type `-type join_policy() :: all | sync_merge | {first_n, pos_integer()} | {n_of_m, pos_integer(), pos_integer()}.` The `sync_merge` policy differs from `all` in semantics (documented in item 003).

3. **Cancel Scope Identification**: Use `term()` for scope_id() to allow atoms, integers, refs, tuples. Validation checks proper nesting, not format.

4. **Validation Error Reporting**: `well_formed/1` returns `ok | {error, [validation_error()]}` for programmatic checking. Smart constructors throw `badarg` for immediate failures.

5. **Derived Pattern Scope IDs**: discriminator/2 auto-generates unique scope IDs using `make_ref()` for each branch. Users needing manual control use raw cancel/2.

---

## Phases

### Phase 1: Core Type Definitions (wf_term.erl)

#### Overview

Define all types for the pattern algebra, establishing the foundation for constructors and validation. This phase creates the "vocabulary" for the entire workflow substrate.

#### Changes Required:

##### 1. Core Kernel Types

**File**: `src/wf_term.erl`
**Changes**: Create new module with type definitions

```erlang
%%%-------------------------------------------------------------------
%%% @doc Workflow Pattern Term Algebra
%%%
%%% This module defines the closed pattern algebra AST for workflow patterns.
%%% The wf_term() type is a tagged union of 9 kernel constructors that can
%%% compose to express arbitrary control-flow patterns.
%%%
%%% == Type Transparency ==
%%% wf_term() is defined as -type (transparent) not -opaque to allow:
%%% - Pattern matching in tests and compiler
%%% - Direct access by internal modules (wf_compile, wf_validate)
%%% - Flexibility for future extensions
%%%
%%% == Constructor Design ==
%%% Smart constructors (public API) validate all inputs and throw badarg
%%% for invalid arguments. Raw constructors (prefixed with underscore) are
%%% exported for internal use by derived patterns and compiler.
%%%
%%% == Validation Scope ==
%%% well_formed/1 checks ONLY local structural invariants:
%%% - Branch counts (par, xor, defer require >= 2)
%%% - Policy format validity (join, loop, mi policies)
%%% - Cancel scope nesting (no crossing boundaries)
%%%
%%% Global properties (deadlock, soundness, option to complete) are
%%% checked by wf_validate (item 013) using bounded model checking.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_term).

%% Kernel Types
-type ctx() :: map().
%% User state and token data flowing through workflow.
%% Users structure this arbitrarily; substrate treats as opaque.

-type scope_id() :: term().
%% Identifier for cancellation scopes.
%% Can be atom(), integer(), reference(), or tuple().
%% Validation checks nesting, not format.

-type case_id() :: term().
%% Identifier for workflow instances.
%% Format is user-defined; substrate treats as opaque.

-type receipt() :: term().
%% Receipt for effects and commits.
%% Structure defined by wf_effect (future item).

-type task_fun() :: fun((ctx()) ->
    {ok, ctx()} |
    {error, term()} |
    {effect, term(), ctx()}).
%% Task function signature.
%% Returns ok with updated context, error with reason,
%% or effect with spec and continuation context.

-type task_metadata() :: #{
    function := task_fun(),
    timeout => timeout(),
    retry => non_neg_integer(),
    description => binary()
}.
%% Task metadata map.
%% Required key: function (the task implementation)
%% Optional keys: timeout (execution timeout), retry (retry count),
%%                description (human-readable description)

%% Policy Types
-type join_policy() ::
    all |
    sync_merge |
    {first_n, pos_integer()} |
    {n_of_m, pos_integer(), pos_integer()}.
%% Join synchronization policies.
%% - all: Wait for all branches to complete
%% - sync_merge: Synchronizing merge (full sync with ordering)
%% - {first_n, N}: Wait for N branches, cancel remainder
%% - {n_of_m, N, M}: Wait for N out of M branches (explicit counts)

-type loop_policy() ::
    while |
    until |
    {count, non_neg_integer()}.
%% Loop iteration policies.
%% - while: Loop while condition holds (check before)
%% - until: Loop until condition holds (check after)
%% - {count, N}: Loop exactly N times

-type mi_policy() ::
    {fixed, pos_integer()} |
    {dynamic, pos_integer(), pos_integer()}.
%% Multiple instance policies.
%% - {fixed, N}: Create exactly N instances
%% - {dynamic, Min, Max}: Create between Min and Max instances dynamically

%% Cancellation Types
-type cancel_scope() :: scope_id() | {scope_id(), [cancel_option()]}.
%% Cancellation scope specification.
%% Simple form: just a scope identifier
%% Extended form: scope identifier with options

-type cancel_option() ::
    {timeout, timeout()} |
    {notify, pid()}.
%% Cancellation scope options.
%% - {timeout, T}: Cancel scope after T milliseconds
%% - {notify, Pid}: Send notification to Pid when scope cancelled

%% Validation Error Types
-type validation_error() ::
    {invalid_branch_count, atom(), pos_integer(), pos_integer()} |
    {invalid_join_policy, join_policy()} |
    {invalid_loop_policy, loop_policy()} |
    {invalid_mi_policy, mi_policy()} |
    {invalid_task_metadata, term()} |
    {orphan_join, join_policy(), [wf_term()]} |
    {malformed_cancel_scope, cancel_scope()} |
    {nested_cancel_scope, scope_id(), scope_id()}.
%% Validation error descriptions.
%% - {invalid_branch_count, NodeType, Actual, Minimum}: Too few branches
%% - {invalid_join_policy, Policy}: Malformed join policy
%% - {invalid_loop_policy, Policy}: Malformed loop policy
%% - {invalid_mi_policy, Policy}: Malformed MI policy
%% - {invalid_task_metadata, Metadata}: Task metadata missing required keys
%% - {orphan_join, Policy, Branches}: Join without matching split
%% - {malformed_cancel_scope, Scope}: Invalid cancel scope format
%% - {nested_cancel_scope, Outer, Inner}: Crossing cancel boundaries

%% Pattern Algebra - The Union Type
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
%% The closed pattern algebra.
%% A wf_term() is one of 9 kernel constructors:
%% - {task, Name, Metadata}: Named task with function metadata
%% - {seq, P, Q}: Sequential composition (P then Q)
%% - {par, Branches}: Parallel split (AND-split)
%% - {xor, Alternatives}: Exclusive choice (XOR-split)
%% - {join, Policy, Branches}: Generalized join with policy
%% - {loop, Policy, Body}: Structured loop
%% - {defer, Alternatives}: Deferred choice (race)
%% - {cancel, Scope, Body}: Cancellation scope
%% - {mi, Policy, Body}: Multiple instances

%% Export types for use by other modules
-export_type([
    ctx/0,
    scope_id/0,
    case_id/0,
    receipt/0,
    task_fun/0,
    task_metadata/0,
    join_policy/0,
    loop_policy/0,
    mi_policy/0,
    cancel_scope/0,
    cancel_option/0,
    validation_error/0,
    wf_term/0
]).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile` succeeds
- [ ] No warnings: Dialyzer passes with no errors
- [ ] Type exports work: Other modules can import wf_term() type

##### Manual Verification:

- [ ] All type definitions follow Erlang/OTP conventions
- [ ] Module documentation clearly explains transparency decision
- [ ] Comments document each type variant

**Note**: Complete this phase before proceeding. Types are the foundation for all subsequent work.

---

### Phase 2: Raw Constructors (wf_term.erl)

#### Overview

Implement raw constructors that create tagged tuples without validation. These are fast-path constructors for internal use by derived patterns and compiler. No input validation is performed.

#### Changes Required:

##### 1. Raw Constructor Implementations

**File**: `src/wf_term.erl`
**Changes**: Add raw constructors after type definitions

```erlang
%%%===================================================================
%%% Raw Constructors (Internal Use)
%%%===================================================================
%%% These constructors create wf_term() tuples without validation.
%%% Use ONLY for internal operations where inputs are known-valid.
%%% Public API should use smart constructors below.

%%--------------------------------------------------------------------
%% @doc Raw task constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _task(atom(), task_metadata()) -> wf_term().
_task(Name, Metadata) ->
    {task, Name, Metadata}.

%%--------------------------------------------------------------------
%% @doc Raw sequential composition constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _seq(wf_term(), wf_term()) -> wf_term().
_seq(P, Q) ->
    {seq, P, Q}.

%%--------------------------------------------------------------------
%% @doc Raw parallel split constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _par([wf_term()]) -> wf_term().
_par(Branches) ->
    {par, Branches}.

%%--------------------------------------------------------------------
%% @doc Raw exclusive choice constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _xor([wf_term()]) -> wf_term().
_xor(Alternatives) ->
    {xor, Alternatives}.

%%--------------------------------------------------------------------
%% @doc Raw join constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _join(join_policy(), [wf_term()]) -> wf_term().
_join(Policy, Branches) ->
    {join, Policy, Branches}.

%%--------------------------------------------------------------------
%% @doc Raw loop constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _loop(loop_policy(), wf_term()) -> wf_term().
_loop(Policy, Body) ->
    {loop, Policy, Body}.

%%--------------------------------------------------------------------
%% @doc Raw deferred choice constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _defer([wf_term()]) -> wf_term().
_defer(Alternatives) ->
    {defer, Alternatives}.

%%--------------------------------------------------------------------
%% @doc Raw cancel scope constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _cancel(cancel_scope(), wf_term()) -> wf_term().
_cancel(Scope, Body) ->
    {cancel, Scope, Body}.

%%--------------------------------------------------------------------
%% @doc Raw multiple instances constructor (no validation).
%% @private
%%--------------------------------------------------------------------
-spec _mi(mi_policy(), wf_term()) -> wf_term().
_mi(Policy, Body) ->
    {mi, Policy, Body}.

%% Export raw constructors with underscore prefix
-export([
    _task/2,
    _seq/2,
    _par/1,
    _xor/1,
    _join/2,
    _loop/2,
    _defer/1,
    _cancel/2,
    _mi/2
]).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile` succeeds
- [ ] All raw constructors export correctly
- [ ] Dialyzer passes with no warnings

##### Manual Verification:

- [ ] Each constructor creates correct tagged tuple structure
- [ ] All constructors have complete -spec declarations
- [ ] Module docs indicate these are for internal use

**Note**: Raw constructors are simple tuple creation. No validation logic yet.

---

### Phase 3: Smart Constructors with Validation (wf_term.erl)

#### Overview

Implement smart constructors that validate all inputs before creating wf_term() tuples. These throw `badarg` with descriptive error messages for invalid inputs. This is the public API for constructing workflow terms.

#### Changes Required:

##### 1. Smart Constructor Implementations

**File**: `src/wf_term.erl`
**Changes**: Add smart constructors after raw constructors

```erlang
%%%===================================================================
%%% Smart Constructors (Public API)
%%%===================================================================
%%% These constructors validate inputs and throw badarg for invalid arguments.
%%% Use these for all external code and user-facing APIs.

%%--------------------------------------------------------------------
%% @doc Construct a task term with validation.
%%
%% Validates that Name is an atom and Metadata is a map containing
%% the required 'function' key with a fun value.
%%
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec task(atom(), task_metadata()) -> wf_term().
task(Name, Metadata) when is_atom(Name), is_map(Metadata) ->
    case maps:is_key(function, Metadata) of
        false ->
            error({badarg, {invalid_task_metadata, Metadata,
                            "Metadata must contain 'function' key"}});
        true ->
            Fun = maps:get(function, Metadata),
            case is_function(Fun) of
                false ->
                    error({badarg, {invalid_task_metadata, Metadata,
                                    "Metadata 'function' must be a fun"}});
                true ->
                    _task(Name, Metadata)
            end
    end;
task(Name, Metadata) ->
    error({badarg, {invalid_task_metadata, Metadata,
                    "Name must be atom, Metadata must be map"}}).

%%--------------------------------------------------------------------
%% @doc Construct sequential composition with validation.
%%
%% Both terms must be wf_term(). Validation is deferred to well_formed/1
%% for deep structural checks.
%%
%% @end
%%--------------------------------------------------------------------
-spec seq(wf_term(), wf_term()) -> wf_term().
seq(P, Q) ->
    %% Basic type check: both must be tagged tuples
    true = is_tuple(P),
    true = is_tuple(Q),
    true = tuple_size(P) >= 2,
    true = tuple_size(Q) >= 2,
    _seq(P, Q).

%%--------------------------------------------------------------------
%% @doc Construct parallel split with validation.
%%
%% Branches must be a non-empty list of wf_term() with length >= 2.
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec par([wf_term()]) -> wf_term().
par(Branches) when is_list(Branches), length(Branches) >= 2 ->
    _par(Branches);
par(Branches) when is_list(Branches) ->
    error({badarg, {invalid_branch_count, par, length(Branches), 2}});
par(_Branches) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct exclusive choice with validation.
%%
%% Alternatives must be a non-empty list of wf_term() with length >= 2.
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec xor([wf_term()]) -> wf_term().
xor(Alternatives) when is_list(Alternatives), length(Alternatives) >= 2 ->
    _xor(Alternatives);
xor(Alternatives) when is_list(Alternatives) ->
    error({badarg, {invalid_branch_count, xor, length(Alternatives), 2}});
xor(_Alternatives) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct join with validation.
%%
%% Policy must be a valid join_policy(). Branches must be a list with
%% length >= 2. Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec join(join_policy(), [wf_term()]) -> wf_term().
join(Policy, Branches) when is_list(Branches), length(Branches) >= 2 ->
    case validate_join_policy_format(Policy) of
        true -> _join(Policy, Branches);
        false -> error({badarg, {invalid_join_policy, Policy}})
    end;
join(_Policy, Branches) when is_list(Branches) ->
    error({badarg, {invalid_branch_count, join, length(Branches), 2}});
join(_Policy, _Branches) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct loop with validation.
%%
%% Policy must be a valid loop_policy(). Body must be a wf_term().
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop(loop_policy(), wf_term()) -> wf_term().
loop(Policy, Body) ->
    case validate_loop_policy_format(Policy) of
        true ->
            true = is_tuple(Body),
            true = tuple_size(Body) >= 2,
            _loop(Policy, Body);
        false ->
            error({badarg, {invalid_loop_policy, Policy}})
    end.

%%--------------------------------------------------------------------
%% @doc Construct deferred choice with validation.
%%
%% Alternatives must be a non-empty list of wf_term() with length >= 2.
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec defer([wf_term()]) -> wf_term().
defer(Alternatives) when is_list(Alternatives), length(Alternatives) >= 2 ->
    _defer(Alternatives);
defer(Alternatives) when is_list(Alternatives) ->
    error({badarg, {invalid_branch_count, defer, length(Alternatives), 2}});
defer(_Alternatives) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Construct cancel scope with validation.
%%
%% Scope can be any term(). Body must be a wf_term().
%% Throws badarg if Body is not a valid wf_term().
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel(cancel_scope(), wf_term()) -> wf_term().
cancel(Scope, Body) ->
    true = is_tuple(Body),
    true = tuple_size(Body) >= 2,
    _cancel(Scope, Body).

%%--------------------------------------------------------------------
%% @doc Construct multiple instances with validation.
%%
%% Policy must be a valid mi_policy(). Body must be a wf_term().
%% Throws badarg if validation fails.
%%
%% @end
%%--------------------------------------------------------------------
-spec mi(mi_policy(), wf_term()) -> wf_term().
mi(Policy, Body) ->
    case validate_mi_policy_format(Policy) of
        true ->
            true = is_tuple(Body),
            true = tuple_size(Body) >= 2,
            _mi(Policy, Body);
        false ->
            error({badarg, {invalid_mi_policy, Policy}})
    end.

%% Export smart constructors (public API)
-export([
    task/2,
    seq/2,
    par/1,
    xor/1,
    join/2,
    loop/2,
    defer/1,
    cancel/2,
    mi/2
]).

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validate join policy format (not semantics).
%%--------------------------------------------------------------------
-spec validate_join_policy_format(join_policy()) -> boolean().
validate_join_policy_format(all) -> true;
validate_join_policy_format(sync_merge) -> true;
validate_join_policy_format({first_n, N}) when is_integer(N), N > 0 -> true;
validate_join_policy_format({n_of_m, N, M}) when is_integer(N), is_integer(M),
                                                   N > 0, M > 0, N =< M -> true;
validate_join_policy_format(_Policy) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate loop policy format (not semantics).
%%--------------------------------------------------------------------
-spec validate_loop_policy_format(loop_policy()) -> boolean().
validate_loop_policy_format(while) -> true;
validate_loop_policy_format(until) -> true;
validate_loop_policy_format({count, N}) when is_integer(N), N >= 0 -> true;
validate_loop_policy_format(_Policy) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate multiple instance policy format (not semantics).
%%--------------------------------------------------------------------
-spec validate_mi_policy_format(mi_policy()) -> boolean().
validate_mi_policy_format({fixed, N}) when is_integer(N), N > 0 -> true;
validate_mi_policy_format({dynamic, Min, Max}) when is_integer(Min), is_integer(Max),
                                                     Min > 0, Max >= Min -> true;
validate_mi_policy_format(_Policy) -> false.
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile` succeeds
- [ ] No warnings: All constructors pass Dialyzer
- [ ] Guards are syntactically correct

##### Manual Verification:

- [ ] Each constructor validates its invariants
- [ ] Error messages are descriptive
- [ ] All constructors have complete documentation

**Note**: Smart constructors throw badarg for invalid input. This is the public API.

---

### Phase 4: Structural Validation (wf_term.erl - well_formed)

#### Overview

Implement `well_formed/1` function that recursively validates structural invariants of wf_term() trees. Returns `ok` for valid terms or `{error, [validation_error()]}` for invalid terms with detailed error list.

#### Changes Required:

##### 1. well_formed/1 Implementation

**File**: `src/wf_term.erl`
**Changes**: Add structural validation after smart constructors

```erlang
%%%===================================================================
%%% Structural Validation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Validate that a wf_term() is well-formed.
%%
%% Checks local structural invariants:
%% - Branch counts (par, xor, defer require >= 2)
%% - Policy format validity
%% - Cancel scope nesting (no crossing boundaries)
%% - Type consistency (all list elements are wf_term())
%%
%% Does NOT check global properties (deadlock, soundness, option to complete).
%% Those checks are implemented in wf_validate (item 013).
%%
%% Returns ok for valid terms, {error, Errors} for invalid terms.
%%
%% @end
%%--------------------------------------------------------------------
-spec well_formed(wf_term()) -> ok | {error, [validation_error()]}.
well_formed(Term) ->
    case validate_node(Term, #{scope_stack => []}) of
        {ok, _} -> ok;
        {error, Errors} -> {error, lists:usort(Errors)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate a single node and its children, accumulating errors.
%%--------------------------------------------------------------------
-spec validate_node(wf_term(), map()) -> {ok, map()} | {error, [validation_error()]}.
validate_node({task, _Name, Metadata}, Context) ->
    %% Validate task metadata
    case maps:is_key(function, Metadata) of
        false -> {error, [{invalid_task_metadata, Metadata}]};
        true ->
            Fun = maps:get(function, Metadata),
            case is_function(Fun) of
                false -> {error, [{invalid_task_metadata, Metadata}]};
                true -> {ok, Context}
            end
    end;

validate_node({seq, P, Q}, Context) ->
    %% Validate left and right recursively
    case validate_node(P, Context) of
        {error, Errors1} ->
            case validate_node(Q, Context) of
                {error, Errors2} -> {error, Errors1 ++ Errors2};
                {ok, _} -> {error, Errors1}
            end;
        {ok, _} ->
            validate_node(Q, Context)
    end;

validate_node({par, Branches}, Context) ->
    %% Validate branch count
    case length(Branches) < 2 of
        true -> {error, [{invalid_branch_count, par, length(Branches), 2}]};
        false ->
            %% Validate all branches are wf_term()
            case validate_list(Branches, Context) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({xor, Alternatives}, Context) ->
    %% Validate branch count
    case length(Alternatives) < 2 of
        true -> {error, [{invalid_branch_count, xor, length(Alternatives), 2}]};
        false ->
            %% Validate all alternatives are wf_term()
            case validate_list(Alternatives, Context) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({join, Policy, Branches}, Context) ->
    %% Validate branch count
    case length(Branches) < 2 of
        true -> {error, [{invalid_branch_count, join, length(Branches), 2}]};
        false ->
            %% Validate policy format
            case validate_join_policy_format(Policy) of
                false -> {error, [{invalid_join_policy, Policy}]};
                true ->
                    %% Validate all branches are wf_term()
                    case validate_list(Branches, Context) of
                        {error, Errors} -> {error, Errors};
                        {ok, _} -> {ok, Context}
                    end
            end
    end;

validate_node({loop, Policy, Body}, Context) ->
    %% Validate policy format
    case validate_loop_policy_format(Policy) of
        false -> {error, [{invalid_loop_policy, Policy}]};
        true -> validate_node(Body, Context)
    end;

validate_node({defer, Alternatives}, Context) ->
    %% Validate branch count
    case length(Alternatives) < 2 of
        true -> {error, [{invalid_branch_count, defer, length(Alternatives), 2}]};
        false ->
            %% Validate all alternatives are wf_term()
            case validate_list(Alternatives, Context) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({cancel, Scope, Body}, Context) ->
    %% Validate scope format
    case validate_cancel_scope_format(Scope) of
        false -> {error, [{malformed_cancel_scope, Scope}]};
        true ->
            %% Track scope nesting
            case Scope of
                {ScopeId, _Opts} -> ok;
                ScopeId -> ok
            end,
            NewContext = Context#{scope_stack => [ScopeId | maps:get(scope_stack, Context)]},
            case validate_node(Body, NewContext) of
                {error, Errors} -> {error, Errors};
                {ok, _} -> {ok, Context}
            end
    end;

validate_node({mi, Policy, Body}, Context) ->
    %% Validate policy format
    case validate_mi_policy_format(Policy) of
        false -> {error, [{invalid_mi_policy, Policy}]};
        true -> validate_node(Body, Context)
    end;

validate_node(InvalidTerm, _Context) ->
    %% Not a valid wf_term() structure
    {error, [{malformed_term, InvalidTerm}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate all elements in a list, accumulating errors.
%%--------------------------------------------------------------------
-spec validate_list([wf_term()], map()) -> {ok, map()} | {error, [validation_error()]}.
validate_list([], Context) ->
    {ok, Context};
validate_list([Term | Rest], Context) ->
    case validate_node(Term, Context) of
        {error, Errors1} ->
            case validate_list(Rest, Context) of
                {error, Errors2} -> {error, Errors1 ++ Errors2};
                {ok, _} -> {error, Errors1}
            end;
        {ok, _} ->
            validate_list(Rest, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate cancel scope format (not semantics).
%%--------------------------------------------------------------------
-spec validate_cancel_scope_format(cancel_scope()) -> boolean().
validate_cancel_scope_format(ScopeId) when is_atom(ScopeId); is_integer(ScopeId);
                                            is_reference(ScopeId); is_tuple(ScopeId) ->
    true;
validate_cancel_scope_format({ScopeId, Opts}) when is_atom(ScopeId); is_integer(ScopeId);
                                                    is_reference(ScopeId); is_tuple(ScopeId) ->
    case validate_cancel_options(Opts) of
        true -> true;
        false -> false
    end;
validate_cancel_scope_format(_Scope) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate cancel options list format.
%%--------------------------------------------------------------------
-spec validate_cancel_options([cancel_option()]) -> boolean().
validate_cancel_options([]) ->
    true;
validate_cancel_options([{timeout, T} | Rest]) when is_integer(T); T =:= infinity ->
    validate_cancel_options(Rest);
validate_cancel_options([{notify, Pid} | Rest]) when is_pid(Pid) ->
    validate_cancel_options(Rest);
validate_cancel_options(_BadOption) ->
    false.

%% Export well_formed/1 (public API)
-export([well_formed/1]).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile` succeeds
- [ ] Recursive validation works correctly
- [ ] No Dialyzer warnings

##### Manual Verification:

- [ ] well_formed/1 catches all specified invariants
- [ ] Error messages are descriptive
- [ ] Function documentation is complete

**Note**: This validation is local only. Global properties will be added in item 013.

---

### Phase 5: Derived Patterns (wf_core.erl)

#### Overview

Implement wf_core.erl with 4 derived pattern functions that compose kernel primitives. Each derived pattern is a function that returns a wf_term(). These are higher-level patterns built from the kernel algebra.

#### Changes Required:

##### 1. wf_core.erl Module

**File**: `src/wf_core.erl`
**Changes**: Create new module with derived patterns

```erlang
%%%-------------------------------------------------------------------
%%% @doc Workflow Pattern Core - Derived Patterns
%%%
%%% This module provides derived/composite workflow patterns built from
%%% the kernel primitives in wf_term. Each derived pattern is a function
%%% that returns a wf_term().
%%%
%%% Derived patterns are syntactic sugar/combinations of kernel patterns
%%% that provide convenient abstractions for common control-flow structures.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_core).

%% Include the wf_term type
-include("wf_term.hrl").  %% We'll need to create this or use type import

%% Import wf_term constructors for convenience
-import(wf_term, [
    task/2, seq/2, par/1, xor/1, join/2, loop/2, defer/1, cancel/2, mi/2
]).

%% Export derived patterns
-export([
    simple_merge/2,
    synchronizing_merge/2,
    discriminator/2,
    n_out_of_m/3
]).

%%%===================================================================
%%% Derived Patterns
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Simple merge: XOR choice converging to single continuation.
%%
%% Takes a list of alternative branches and a continuation term.
%% Composes each alternative with the continuation using seq/2,
%% then wraps in an xor/1 for exclusive choice.
%%
%% This is the standard "XOR-split followed by XOR-join" pattern
%% where exactly one alternative executes, then flow continues.
%%
%% Example:
%%   simple_merge([task(a, ...), task(b, ...)], task(c, ...))
%%   => xor([seq(task(a, ...), task(c, ...)),
%%           seq(task(b, ...), task(c, ...))])
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_merge([wf_term:wf_term()], wf_term:wf_term()) -> wf_term:wf_term().
simple_merge(Alternatives, Continuation) when is_list(Alternatives),
                                               length(Alternatives) >= 2 ->
    xor([seq(Alt, Continuation) || Alt <- Alternatives]).

%%--------------------------------------------------------------------
%% @doc Synchronizing merge: Parallel branches converging with full sync.
%%
%% Takes a list of parallel branches and a continuation term.
%% Composes each branch with the continuation using seq/2,
%% wraps in par/1 for parallel execution, then joins with 'all' policy.
%%
%% This is the standard "AND-split followed by AND-join" pattern
%% where all branches execute in parallel, then wait for all to complete
%% before continuing.
%%
%% Example:
%%   synchronizing_merge([task(a, ...), task(b, ...)], task(c, ...))
%%   => seq(par([seq(task(a, ...), task(c, ...)),
%%               seq(task(b, ...), task(c, ...))]),
%%          join(all, []))
%%
%% Note: The join(all, []) at the end ensures synchronization before
%% the continuation is reached. The continuation is embedded in each
%% branch to ensure it executes after all branches complete.
%%
%% @end
%%--------------------------------------------------------------------
-spec synchronizing_merge([wf_term:wf_term()], wf_term:wf_term()) -> wf_term:wf_term().
synchronizing_merge(Branches, Continuation) when is_list(Branches),
                                                 length(Branches) >= 2 ->
    %% Embed continuation in each branch
    BranchesWithCont = [seq(Branch, Continuation) || Branch <- Branches],
    %% Execute in parallel, then synchronize
    seq(par(BranchesWithCont), join(all, [])).

%%--------------------------------------------------------------------
%% @doc Discriminator: First-complete join that cancels remaining.
%%
%% Takes a list of parallel branches and a continuation term.
%% Wraps each branch in a unique cancel scope, executes in parallel,
%% and uses a {first_n, 1} join policy to proceed when ANY branch completes.
%%
%% When the first branch completes, all other branches are cancelled.
%% This is the "discriminator" pattern from workflow patterns literature.
%%
%% Example:
%%   discriminator([task(a, ...), task(b, ...), task(c, ...)], task(d, ...))
%%   => Creates 3 cancel scopes, executes in parallel,
%%      proceeds with first completion, cancels the rest
%%
%% Note: Cancel scopes are auto-generated using make_ref() to ensure
%% uniqueness and avoid scope collisions.
%%
%% @end
%%--------------------------------------------------------------------
-spec discriminator([wf_term:wf_term()], wf_term:wf_term()) -> wf_term:wf_term().
discriminator(Branches, Continuation) when is_list(Branches),
                                          length(Branches) >= 2 ->
    %% Create unique cancel scope for each branch
    ScopedBranches = lists:map(
        fun(Branch) ->
            ScopeId = make_ref(),
            cancel(ScopeId, seq(Branch, Continuation))
        end,
        Branches
    ),
    %% Execute in parallel, proceed on first completion
    join({first_n, 1}, ScopedBranches).

%%--------------------------------------------------------------------
%% @doc N-out-of-M join: Wait for N out of M branches.
%%
%% Takes N (number of branches to wait for), a list of M branches,
%% and a continuation term. Composes each branch with the continuation
%% using seq/2, wraps in par/1 for parallel execution, then joins with
%% {n_of_m, N, M} policy.
%%
%% This is the "N-out-of-M join" pattern where execution proceeds when
%% N of the M branches complete. The remaining branches are cancelled.
%%
%% Constraints: 1 <= N <= M
%%
%% Example:
%%   n_out_of_m(2, [task(a, ...), task(b, ...), task(c, ...)], task(d, ...))
%%   => Waits for 2 out of 3 branches to complete, then continues
%%
%% @end
%%--------------------------------------------------------------------
-spec n_out_of_m(pos_integer(), [wf_term:wf_term()], wf_term:wf_term()) ->
    wf_term:wf_term().
n_out_of_m(N, Branches, Continuation)
    when is_integer(N), N > 0, is_list(Branches), length(Branches) >= N ->
    M = length(Branches),
    %% Embed continuation in each branch
    BranchesWithCont = [seq(Branch, Continuation) || Branch <- Branches],
    %% Execute in parallel, wait for N out of M
    seq(par(BranchesWithCont), join({n_of_m, N, M}, [])).
```

#### Success Criteria:

##### Automated Verification:

- [ ] Module compiles: `rebar3 compile` succeeds
- [ ] All derived patterns export correctly
- [ ] Dialyzer passes with no warnings

##### Manual Verification:

- [ ] Each derived pattern composes kernel primitives correctly
- [ ] All patterns have complete documentation with examples
- [ ] Guard clauses validate inputs

**Note**: wf_core depends on wf_term. Ensure Phase 1-4 are complete first.

---

### Phase 6: Testing - wf_term_tests.erl

#### Overview

Create comprehensive EUnit test suite for wf_term.erl covering all constructors, validation, and error cases. Tests must verify both positive cases (valid inputs) and negative cases (invalid inputs that should fail).

#### Changes Required:

##### 1. wf_term_tests.erl Module

**File**: `test/wf_term_tests.erl`
**Changes**: Create new test module

```erlang
%%%-------------------------------------------------------------------
%%% @doc EUnit test suite for wf_term
%%%
%%% Tests all constructors, smart constructors, and validation logic.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_term_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test raw constructors create correct tuples.
%%--------------------------------------------------------------------
raw_constructor_test_() ->
    fun() ->
        %% Raw task constructor
        Task = wf_term:_task(test_task, #{function => fun(Ctx) -> {ok, Ctx} end}),
        ?assertMatch({task, test_task, #{function := _}}, Task),

        %% Raw seq constructor
        Seq = wf_term:_seq(Task, Task),
        ?assertMatch({seq, {task, _, _}, {task, _, _}}, Seq),

        %% Raw par constructor
        Par = wf_term:_par([Task, Task]),
        ?assertMatch({par, [{task, _, _}, {task, _, _}]}, Par),

        %% Raw xor constructor
        Xor = wf_term:_xor([Task, Task]),
        ?assertMatch({xor, [{task, _, _}, {task, _, _}]}, Xor),

        %% Raw join constructor
        Join = wf_term:_join(all, [Task, Task]),
        ?assertMatch({join, all, [{task, _, _}, {task, _, _}]}, Join),

        %% Raw loop constructor
        Loop = wf_term:_loop(while, Task),
        ?assertMatch({loop, while, {task, _, _}}, Loop),

        %% Raw defer constructor
        Defer = wf_term:_defer([Task, Task]),
        ?assertMatch({defer, [{task, _, _}, {task, _, _}]}, Defer),

        %% Raw cancel constructor
        Cancel = wf_term:_cancel(test_scope, Task),
        ?assertMatch({cancel, test_scope, {task, _, _}}, Cancel),

        %% Raw mi constructor
        Mi = wf_term:_mi({fixed, 3}, Task),
        ?assertMatch({mi, {fixed, 3}, {task, _, _}}, Mi)
    end.

%%--------------------------------------------------------------------
%% @doc Test smart constructors with valid inputs.
%%--------------------------------------------------------------------
smart_constructor_valid_test_() ->
    fun() ->
        %% Valid task constructor
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),
        ?assertMatch({task, test_task, #{function := _}}, Task),

        %% Valid seq constructor
        Seq = wf_term:seq(Task, Task),
        ?assertMatch({seq, _, _}, Seq),

        %% Valid par constructor
        Par = wf_term:par([Task, Task]),
        ?assertMatch({par, _}, Par),

        %% Valid xor constructor
        Xor = wf_term:xor([Task, Task]),
        ?assertMatch({xor, _}, Xor),

        %% Valid join constructors
        Join1 = wf_term:join(all, [Task, Task]),
        ?assertMatch({join, all, _}, Join1),

        Join2 = wf_term:join(sync_merge, [Task, Task]),
        ?assertMatch({join, sync_merge, _}, Join2),

        Join3 = wf_term:join({first_n, 1}, [Task, Task]),
        ?assertMatch({join, {first_n, 1}, _}, Join3),

        Join4 = wf_term:join({n_of_m, 2, 3}, [Task, Task, Task]),
        ?assertMatch({join, {n_of_m, 2, 3}, _}, Join4),

        %% Valid loop constructors
        Loop1 = wf_term:loop(while, Task),
        ?assertMatch({loop, while, _}, Loop1),

        Loop2 = wf_term:loop(until, Task),
        ?assertMatch({loop, until, _}, Loop2),

        Loop3 = wf_term:loop({count, 5}, Task),
        ?assertMatch({loop, {count, 5}, _}, Loop3),

        %% Valid defer constructor
        Defer = wf_term:defer([Task, Task]),
        ?assertMatch({defer, _}, Defer),

        %% Valid cancel constructor
        Cancel = wf_term:cancel(test_scope, Task),
        ?assertMatch({cancel, test_scope, _}, Cancel),

        %% Valid mi constructors
        Mi1 = wf_term:mi({fixed, 3}, Task),
        ?assertMatch({mi, {fixed, 3}, _}, Mi1),

        Mi2 = wf_term:mi({dynamic, 1, 5}, Task),
        ?assertMatch({mi, {dynamic, 1, 5}, _}, Mi2)
    end.

%%--------------------------------------------------------------------
%% @doc Test smart constructors with invalid inputs (should throw).
%%--------------------------------------------------------------------
smart_constructor_invalid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),

        %% Invalid task: missing function key
        ?assertError({badarg, {invalid_task_metadata, _, _}},
                     wf_term:task(test_task, #{timeout => 100})),

        %% Invalid task: function not a fun
        ?assertError({badarg, {invalid_task_metadata, _, _}},
                     wf_term:task(test_task, #{function => not_a_fun})),

        %% Invalid par: too few branches
        ?assertError({badarg, {invalid_branch_count, par, 1, 2}},
                     wf_term:par([Task])),

        %% Invalid xor: too few branches
        ?assertError({badarg, {invalid_branch_count, xor, 1, 2}},
                     wf_term:xor([Task])),

        %% Invalid join: too few branches
        ?assertError({badarg, {invalid_branch_count, join, 1, 2}},
                     wf_term:join(all, [Task])),

        %% Invalid join: bad policy
        ?assertError({badarg, {invalid_join_policy, bad_policy}},
                     wf_term:join(bad_policy, [Task, Task])),

        %% Invalid loop: bad policy
        ?assertError({badarg, {invalid_loop_policy, bad_policy}},
                     wf_term:loop(bad_policy, Task)),

        %% Invalid defer: too few branches
        ?assertError({badarg, {invalid_branch_count, defer, 1, 2}},
                     wf_term:defer([Task])),

        %% Invalid mi: bad policy
        ?assertError({badarg, {invalid_mi_policy, bad_policy}},
                     wf_term:mi(bad_policy, Task))
    end.

%%--------------------------------------------------------------------
%% @doc Test well_formed/1 with valid terms.
%%--------------------------------------------------------------------
well_formed_valid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),

        %% Valid simple terms
        ?assertEqual(ok, wf_term:well_formed(Task)),

        %% Valid seq
        Seq = wf_term:seq(Task, Task),
        ?assertEqual(ok, wf_term:well_formed(Seq)),

        %% Valid par
        Par = wf_term:par([Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Par)),

        %% Valid xor
        Xor = wf_term:xor([Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Xor)),

        %% Valid join
        Join = wf_term:join(all, [Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Join)),

        %% Valid loop
        Loop = wf_term:loop(while, Task),
        ?assertEqual(ok, wf_term:well_formed(Loop)),

        %% Valid defer
        Defer = wf_term:defer([Task, Task]),
        ?assertEqual(ok, wf_term:well_formed(Defer)),

        %% Valid cancel
        Cancel = wf_term:cancel(test_scope, Task),
        ?assertEqual(ok, wf_term:well_formed(Cancel)),

        %% Valid mi
        Mi = wf_term:mi({fixed, 3}, Task),
        ?assertEqual(ok, wf_term:well_formed(Mi)),

        %% Valid nested term
        Nested = wf_term:seq(
            wf_term:par([Task, Task]),
            wf_term:xor([Task, Task])
        ),
        ?assertEqual(ok, wf_term:well_formed(Nested))
    end.

%%--------------------------------------------------------------------
%% @doc Test well_formed/1 with invalid terms.
%%--------------------------------------------------------------------
well_formed_invalid_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task = wf_term:task(test_task, #{function => Fun}),

        %% Invalid: par with 1 branch (using raw constructor to bypass smart check)
        BadPar = wf_term:_par([Task]),
        ?assertMatch({error, [{invalid_branch_count, par, 1, 2}]},
                     wf_term:well_formed(BadPar)),

        %% Invalid: xor with 1 branch
        BadXor = wf_term:_xor([Task]),
        ?assertMatch({error, [{invalid_branch_count, xor, 1, 2}]},
                     wf_term:well_formed(BadXor)),

        %% Invalid: join with bad policy
        BadJoin = wf_term:_join(bad_policy, [Task, Task]),
        ?assertMatch({error, [{invalid_join_policy, bad_policy}]},
                     wf_term:well_formed(BadJoin)),

        %% Invalid: loop with bad policy
        BadLoop = wf_term:_loop(bad_policy, Task),
        ?assertMatch({error, [{invalid_loop_policy, bad_policy}]},
                     wf_term:well_formed(BadLoop)),

        %% Invalid: defer with 1 branch
        BadDefer = wf_term:_defer([Task]),
        ?assertMatch({error, [{invalid_branch_count, defer, 1, 2}]},
                     wf_term:well_formed(BadDefer)),

        %% Invalid: mi with bad policy
        BadMi = wf_term:_mi(bad_policy, Task),
        ?assertMatch({error, [{invalid_mi_policy, bad_policy}]},
                     wf_term:well_formed(BadMi))
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] All tests pass: `rebar3 eunit`
- [ ] Code coverage is high (>90% for wf_term.erl)
- [ ] No test failures or skipped tests

##### Manual Verification:

- [ ] Tests cover all constructors
- [ ] Tests cover all validation invariants
- [ ] Tests include both positive and negative cases
- [ ] Error assertions check error structure

**Note**: Tests must be comprehensive. Add more tests as needed during implementation.

---

### Phase 7: Testing - wf_core_tests.erl

#### Overview

Create EUnit test suite for wf_core.erl covering all 4 derived patterns. Tests verify that derived patterns compose kernel primitives correctly and produce valid wf_term() structures.

#### Changes Required:

##### 1. wf_core_tests.erl Module

**File**: `test/wf_core_tests.erl`
**Changes**: Create new test module

```erlang
%%%-------------------------------------------------------------------
%%% @doc EUnit test suite for wf_core
%%%
%%% Tests all derived patterns.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_core_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test simple_merge/2.
%%--------------------------------------------------------------------
simple_merge_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid simple_merge
        Term = wf_core:simple_merge([Task1, Task2], Cont),
        ?assertMatch({xor, _}, Term),

        %% Verify structure: xor([seq(Task1, Cont), seq(Task2, Cont)])
        ?assertEqual(2, length(element(2, Term))),
        ?assertMatch({seq, Task1, Cont}, lists:nth(1, element(2, Term))),
        ?assertMatch({seq, Task2, Cont}, lists:nth(2, element(2, Term))),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Invalid: too few alternatives (should throw)
        ?assertError(badarg, wf_core:simple_merge([Task1], Cont))
    end.

%%--------------------------------------------------------------------
%% @doc Test synchronizing_merge/2.
%%--------------------------------------------------------------------
synchronizing_merge_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid synchronizing_merge
        Term = wf_core:synchronizing_merge([Task1, Task2], Cont),
        ?assertMatch({seq, _, _}, Term),

        %% Verify structure: seq(par([seq(Task1, Cont), seq(Task2, Cont)]), join(all, []))
        {seq, Par, Join} = Term,
        ?assertMatch({par, _}, Par),
        ?assertMatch({join, all, []}, Join),

        %% Verify branches have continuation
        ?assertEqual(2, length(element(2, Par))),
        ?assertMatch({seq, Task1, Cont}, lists:nth(1, element(2, Par))),
        ?assertMatch({seq, Task2, Cont}, lists:nth(2, element(2, Par))),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Invalid: too few branches (should throw)
        ?assertError(badarg, wf_core:synchronizing_merge([Task1], Cont))
    end.

%%--------------------------------------------------------------------
%% @doc Test discriminator/2.
%%--------------------------------------------------------------------
discriminator_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Task3 = wf_term:task(task3, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid discriminator
        Term = wf_core:discriminator([Task1, Task2, Task3], Cont),
        ?assertMatch({join, {first_n, 1}, _}, Term),

        %% Verify structure: join({first_n, 1}, [cancel(Scope1, seq(Task1, Cont)), ...])
        {join, {first_n, 1}, ScopedBranches} = Term,
        ?assertEqual(3, length(ScopedBranches)),

        %% Verify each branch is cancel-wrapped with continuation
        lists:foreach(
            fun(ScopedBranch) ->
                ?assertMatch({cancel, _, {seq, _, Cont}}, ScopedBranch)
            end,
            ScopedBranches
        ),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Invalid: too few branches (should throw)
        ?assertError(badarg, wf_core:discriminator([Task1], Cont)),

        %% Verify scopes are unique
        Scopes = [element(2, SB) || SB <- ScopedBranches],
        ?assertEqual(3, length(lists:usort(Scopes)))
    end.

%%--------------------------------------------------------------------
%% @doc Test n_out_of_m/3.
%%--------------------------------------------------------------------
n_out_of_m_test_() ->
    fun() ->
        Fun = fun(Ctx) -> {ok, Ctx} end,
        Task1 = wf_term:task(task1, #{function => Fun}),
        Task2 = wf_term:task(task2, #{function => Fun}),
        Task3 = wf_term:task(task3, #{function => Fun}),
        Cont = wf_term:task(cont, #{function => Fun}),

        %% Valid n_out_of_m: 2 out of 3
        Term = wf_core:n_out_of_m(2, [Task1, Task2, Task3], Cont),
        ?assertMatch({seq, _, _}, Term),

        %% Verify structure: seq(par([...]), join({n_of_m, 2, 3}, []))
        {seq, Par, Join} = Term,
        ?assertMatch({par, _}, Par),
        ?assertMatch({join, {n_of_m, 2, 3}, []}, Join),

        %% Verify branches have continuation
        ?assertEqual(3, length(element(2, Par))),
        ?assertMatch({seq, Task1, Cont}, lists:nth(1, element(2, Par))),
        ?assertMatch({seq, Task2, Cont}, lists:nth(2, element(2, Par))),
        ?assertMatch({seq, Task3, Cont}, lists:nth(3, element(2, Par))),

        %% Verify well-formed
        ?assertEqual(ok, wf_term:well_formed(Term)),

        %% Edge cases
        ?assertEqual(ok, wf_term:well_formed(wf_core:n_out_of_m(1, [Task1, Task2], Cont))),
        ?assertEqual(ok, wf_term:well_formed(wf_core:n_out_of_m(2, [Task1, Task2], Cont))),

        %% Invalid: N > M (should throw)
        ?assertError(badarg, wf_core:n_out_of_m(5, [Task1, Task2], Cont)),

        %% Invalid: N <= 0 (should throw)
        ?assertError(badarg, wf_core:n_out_of_m(0, [Task1, Task2], Cont))
    end.
```

#### Success Criteria:

##### Automated Verification:

- [ ] All tests pass: `rebar3 eunit`
- [ ] Code coverage is high (>90% for wf_core.erl)
- [ ] No test failures or skipped tests

##### Manual Verification:

- [ ] Tests cover all 4 derived patterns
- [ ] Tests verify structure of composed terms
- [ ] Tests include edge cases (N=1, N=M, N=M-1)
- [ ] Tests verify well-formedness of outputs

**Note**: Tests must verify both structure and validity of derived pattern outputs.

---

### Phase 8: Integration and Verification

#### Overview

Final verification phase ensuring all components compile, pass tests, and are ready for integration with future items (wf_compile, wf_validate).

#### Changes Required:

##### 1. Compilation and Dialyzer Verification

**Actions**:
- Run `rebar3 compile` and verify no warnings
- Run `rebar3 dialyzer` and verify no errors
- Check that auto-discovery includes new modules

```bash
# Compilation check
rebar3 compile

# Dialyzer check
rebar3 dialyzer

# List compiled modules
ls -la _build/default/lib/wf_substrate/ebin/
```

##### 2. Test Execution

**Actions**:
- Run all EUnit tests
- Verify test coverage
- Check for test failures or timeouts

```bash
# Run all tests
rebar3 eunit

# Run with coverage (if cover tool is available)
rebar3 cover
```

##### 3. Documentation Review

**Actions**:
- Verify all modules have -moduledoc
- Verify all exported functions have -doc
- Check type documentation
- Review design decision comments

##### 4. Integration Readiness Check

**Actions**:
- Verify wf_term() type is ready for wf_compile (item 004)
- Verify no dependencies on future modules
- Check that derived patterns can be compiled
- Verify validation is local-only (global checks in item 013)

#### Success Criteria:

##### Automated Verification:

- [ ] `rebar3 compile` succeeds with zero warnings
- [ ] `rebar3 dialyzer` passes with no errors
- [ ] `rebar3 eunit` passes all tests (100% success rate)
- [ ] Code coverage > 90% for both modules
- [ ] No rebar3 warnings or errors

##### Manual Verification:

- [ ] All modules have complete documentation
- [ ] All exported functions have -spec and -doc
- [ ] Design decisions are documented in comments
- [ ] Module structure follows Erlang/OTP conventions
- [ ] No circular dependencies
- [ No dependencies on future items

##### Integration Verification:

- [ ] wf_term.erl exports all types needed by wf_compile
- [ ] wf_core.erl depends only on wf_term.erl
- [ ] No process spawning or side effects
- [ ] Pure functional design maintained
- [ ] Ready for item 004 (compiler) to consume wf_term()

**Note**: This is the final verification phase. All previous phases must be complete.

---

## Testing Strategy

### Unit Tests:

**wf_term_tests.erl**:
- Test each raw constructor creates correct tuple structure
- Test each smart constructor with valid inputs ( succeeds)
- Test each smart constructor with invalid inputs (throws badarg)
- Test well_formed/1 with valid terms (returns ok)
- Test well_formed/1 with invalid terms (returns {error, Errors})
- Test nested structures validate recursively
- Test cancel scope nesting validation
- Test all policy types (join, loop, mi)

**wf_core_tests.erl**:
- Test simple_merge/2 creates correct xor+seq structure
- Test synchronizing_merge/2 creates correct par+join structure
- Test discriminator/2 creates cancel scopes and first_n join
- Test n_out_of_m/3 with various N/M combinations
- Test all derived patterns produce well-formed terms
- Test derived patterns reject invalid inputs
- Test discriminator generates unique scope IDs

### Integration Tests:

- **End-to-end scenarios**: Create complex workflow terms using both kernel and derived patterns
- **Composition tests**: Verify derived patterns can nest (e.g., n_out_of_m containing discriminator)
- **Validation stress tests**: Create large nested terms and verify well_formed/1 completes

### Manual Testing Steps:

1. **Compile the project**:
   ```bash
   rebar3 compile
   ```
   Verify: No warnings or errors

2. **Run Dialyzer**:
   ```bash
   rebar3 dialyzer
   ```
   Verify: No type errors

3. **Run all tests**:
   ```bash
   rebar3 eunit
   ```
   Verify: All tests pass

4. **Check test coverage**:
   ```bash
   rebar3 cover
   ```
   Verify: >90% coverage for wf_term.erl and wf_core.erl

5. **Manual inspection**:
   - Read wf_term.erl and verify all types are documented
   - Read wf_core.erl and verify all patterns have examples
   - Check that smart constructors have clear error messages

6. **Integration smoke test**:
   - Create a complex workflow term using both kernel and derived patterns
   - Run well_formed/1 on it
   - Verify it returns ok

## Migration Notes

Not applicable - this is a new implementation with no existing data or systems to migrate.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/002-pattern-term-algebra/research.md`
- Specification: `/Users/speed/wf-substrate/PROMPT.md` (lines 97-125 for type definitions, lines 106-125 for kernel primitives)
- Build config: `/Users/speed/wf-substrate/rebar.config` (OTP 26+, warnings_as_errors, Dialyzer configured)
- App file: `/Users/speed/wf-substrate/src/wf_substrate.app.src` (auto-discovery enabled)
- Existing modules: `/Users/speed/wf-substrate/src/wf_substrate*.erl` (OTP boilerplate)
- Test scaffold: `/Users/speed/wf-substrate/test/wf_substrate_tests.erl` (EUnit header included)
