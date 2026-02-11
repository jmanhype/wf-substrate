# Research: Set up rebar3 project scaffold

**Date**: 2025-01-10
**Item**: 001-rebar3-project-scaffold

## Research Question

Set up rebar3 project structure for the wf_substrate OTP application. Create rebar.config (no external deps, OTP 26+ minimum), app.src with proper application metadata, and standard directory layout: src/ for source modules, test/ for EUnit and CT suites, docs/ for documentation. Include a .gitignore appropriate for Erlang/rebar3 projects (_build/, .rebar3, erl_crash.dump, etc.). The project should compile cleanly with `rebar3 compile` and run `rebar3 eunit` with zero tests. No external dependencies — pure Erlang/OTP only.

## Summary

The project is currently a blank slate with no existing Erlang/OTP infrastructure. Based on the comprehensive project specification in PROMPT.md, this is a greenfield OTP application called `wf_substrate` that will implement a workflow pattern control substrate. The rebar3 scaffold needs to be established from scratch with no external dependencies, requiring OTP 26+ as the minimum version.

The task involves creating a complete rebar3 project structure including: rebar.config with compiler and shell options (no deps), src/wf_substrate.app.src with application metadata, standard directory layout (src/, test/, docs/), and updating the minimal .gitignore to include Erlang/OTP build artifacts. The project must compile with `rebar3 compile` and pass `rebar3 eunit` with zero tests (empty test suite). The PROMPT.md outlines 20+ modules that will eventually be implemented, but for this initial scaffold we only need the basic OTP application structure.

## Current State Analysis

### Existing Implementation

**Current State**: The project directory `/Users/speed/wf-substrate` contains only Git repository files, Wreckit workflow metadata (`.wreckit/`), and a minimal `.gitignore` (lines 1-4) with only Wreckit-specific exclusions. There are **no existing Erlang source files**, no rebar3 configuration, and no OTP application structure.

**Evidence**:
- Glob searches for `*.erl`, `src/*`, `test/*`, `docs/*`, `rebar.config` all returned no results
- `.gitignore` at `/Users/speed/wf-substrate/.gitignore:1-4` contains only `.wreckit/config.local.json`
- The only substantive documentation is `/Users/speed/wf-substrate/PROMPT.md` which defines the entire project architecture

**Project Requirements from PROMPT.md**:

The application will implement a pure Erlang workflow pattern substrate with these modules (lines 50-65):
- Core modules: wf_term.erl, wf_core.erl, wf_compile.erl, wf_vm.erl (or continuation path), wf_exec.erl
- Runtime modules: wf_sched.erl, wf_state.erl, wf_cancel.erl, wf_mi.erl, wf_effect.erl, wf_receipt.erl
- Observability: wf_trace.erl
- Validation: wf_validate.erl
- Testing: wf_test_*.erl modules, wf_bench.erl
- Examples: wf_example_*.erl modules
- Public API: wf_substrate.erl (line 273)

**Technical Constraints** (from PROMPT.md lines 19-26):
- Pure Erlang only - no NIFs, ports, or external dependencies
- OTP 26+ minimum (line 13)
- No "workflow as data interpreted by engine" - compiled executable form required
- OTP behaviors: gen_server, gen_statem, supervisor (lines 181-186)

## Key Files

- `/Users/speed/wf-substrate/PROMPT.md` - Complete project specification defining the workflow pattern substrate, including module list (lines 50-65), OTP requirements (lines 179-198), and acceptance criteria (lines 350-360)
- `/Users/speed/wf-substrate/.gitignore:1-4` - Current minimal gitignore with only Wreckit local config exclusion
- `/Users/speed/wf-substrate/.wreckit/config.json:1-24` - Wreckit workflow configuration (not directly relevant to rebar3 setup)

**Files to be Created**:
- `rebar.config` - Build configuration with no deps, OTP 26+ requirement
- `src/wf_substrate.app.src` - OTP application resource file
- `src/wf_substrate.erl` - Main application module (initially minimal)
- `src/wf_substrate_app.erl` - Application callback module (optional but recommended)
- `src/wf_substrate_sup.erl` - Top-level supervisor (per OTP conventions)
- `test/wf_substrate_tests.erl` - Empty EUnit test suite
- `docs/` directory (empty or with README)

## Technical Considerations

### Dependencies

**External Dependencies**: None required. The specification explicitly states "pure Erlang/OTP only" and "no external dependencies beyond standard Erlang/OTP libs" (PROMPT.md:11).

**Standard OTP Applications Needed**:
- `kernel` - Core OTP (always available)
- `stdlib` - Standard library (always available)
- `sasl` - System Application Support Libraries (for logging, supervision tree visibility)
- `compiler` - For compile-time features (if needed)

**Minimum OTP Version**: 26+ (PROMPT.md:13)

### Patterns to Follow

**rebar.config Conventions**:
- Set `{require_min_otp_vsn, 26}` in erl_opts section
- Configure {erl_opts, [debug_info, warnings_as_errors]} for strict compilation
- No deps section required (stdlib only)
- Include {shell_apps, [sasl]} for better REPL experience
- Configure {plugins, []} explicitly (no plugins needed initially)

**app.src Conventions**:
- Application name: `wf_substrate`
- Description: "Workflow pattern substrate for compiled pattern execution"
- Modules list should include all planned modules (even if not yet created) or use wildcard
- Registered: process names for top-level supervisor
- Applications: [kernel, stdlib, sasl]
- Mod: {wf_substrate_app, []} for application callback
- Vsn: "0.1.0"

**Directory Layout** (Standard OTP/rebar3):
```
wf-substrate/
├── rebar.config
├── src/
│   ├── wf_substrate.app.src
│   ├── wf_substrate.erl (public API)
│   ├── wf_substrate_app.erl (application callback)
│   └── wf_substrate_sup.erl (top supervisor)
├── test/
│   └── wf_substrate_tests.erl (empty EUnit suite)
├── docs/
│   └── (empty or README.md)
└── _build/ (generated, not committed)
```

**Supervision Tree Pattern** (from PROMPT.md:181-186):
- Top-level: wf_substrate_sup
- Children eventually: wf_case_sup (dynamic), wf_effect_sup, wf_trace_sink
- For initial scaffold: minimal empty supervisor

**Testing Strategy** (from PROMPT.md:202-240):
- EUnit for unit tests (test/ directory)
- Common Test suites for integration tests (test/ directory with *_SUITE.erl)
- Property-based tests (minimal generator framework, no PropEr dependency)
- Initially: zero tests passing

### OTP 26+ Features Available

Since we're targeting OTP 26+, we can leverage:
- Updated logger API (instead of error_logger)
- Modern gen_statem features
- Improved type specifications
- Latest EUnit features

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| **OTP Version Mismatch** | Medium | Set explicit `require_min_otp_vsn` in rebar.config; document OTP 26+ requirement in README |
| **Missing Module Warnings** | Low | Use `{erl_opts, [nowarn_missing_all]}` temporarily or list all planned modules in app.src |
| **Gitignore Incomplete** | Medium | Add comprehensive Erlang/OTP build artifacts: _build/, .rebar3/, *.beam, erl_crash.dump, *.dump, rebar3.crashdump |
| **Test Suite Not Found** | Low | Ensure at least one minimal test file exists so `rebar3 eunit` passes with "0 tests, 0 failures" |
| **App.src Metadata Incomplete** | Medium | Follow standard OTP application resource file format; include all required fields (description, applications, mod) |
| **Supervision Tree Structure** | Low | Start with minimal empty supervisor; expand in later items as per PROMPT.md supervision tree design |

## Recommended Approach

**Phase 1: Core Configuration Files**
1. Create `rebar.config` with:
   - `{erl_opts, [debug_info, warnings_as_errors, {require_min_otp_vsn, 26}]}`
   - `{shell_apps, [sasl]}`
   - `{xref_checks, [undefined_function_calls]}`
   - `{deps, []}` (explicitly empty)

2. Update `.gitignore` to add standard Erlang/OTP exclusions:
   ```
   _build/
   .rebar3/
   *.beam
   *.dump
   erl_crash.dump
   rebar3.crashdump
   *.log
   log/
   ```

**Phase 2: OTP Application Structure**
3. Create `src/wf_substrate.app.src` with:
   - Application metadata (description, vsn, modules, registered, applications, mod)
   - Include planned modules list or use wildcard

4. Create minimal application behavior modules:
   - `wf_substrate_app.erl`: Implements application start/2 and stop/1
   - `wf_substrate_sup.erl`: Empty supervisor using one_for_one strategy
   - `wf_substrate.erl`: Placeholder public API module

**Phase 3: Test Infrastructure**
5. Create `test/wf_substrate_tests.erl` with:
   - Empty test suite (no test functions) to pass `rebar3 eunit`
   - EUnit include header

6. Create `docs/` directory structure (empty or with placeholder README)

**Phase 4: Verification**
7. Run `rebar3 compile` - should succeed with warnings about missing modules
8. Run `rebar3 eunit` - should pass with "0 tests" output
9. Verify .gitignore prevents build artifacts from being committed

**Rationale**: This approach follows OTP/rebar3 conventions from the official rebar3 documentation and ensures the project starts with a solid foundation that can accommodate the 20+ modules planned in the specification. The minimal supervisor and application callbacks follow standard OTP patterns while leaving room for expansion.

## Open Questions

1. **Module Listing in app.src**: Should we list all 20+ planned modules explicitly in the app.src file (causing warnings until implemented), or use a wildcard approach that will discover them as they're created?
   - *Recommendation*: List core modules explicitly (wf_substrate, wf_substrate_app, wf_substrate_sup) and rely on rebar3's module auto-discovery for the rest

2. **Initial Supervisor Depth**: Should the top-level supervisor (`wf_substrate_sup`) immediately include the children specified in PROMPT.md:181-186 (wf_case_sup, wf_effect_sup, wf_trace_sink), or should we add them incrementally as those components are implemented?
   - *Recommendation*: Start with empty supervisor and add children in later items to avoid warnings about undefined modules

3. **Documentation Structure**: Should docs/ contain only .md files as specified in PROMPT.md:67-72, or should we include Edoc/ExDoc configuration for generated API documentation?
   - *Recommendation*: Create empty docs/ directory now; add documentation generation configuration in item 003 (architecture and semantics docs)

4. **Test Directory Organization**: Should we separate EUnit tests (test/) from Common Test suites (test/ with _SUITE.erl suffix) from the start?
   - *Recommendation*: Use single test/ directory; add CT suites later when complex integration tests are needed

5. **Warnings Policy**: Should we enable `warnings_as_errors` immediately, or ease into it?
   - *Recommendation*: Enable now to enforce code quality from the start; can be temporarily disabled during rapid prototyping
