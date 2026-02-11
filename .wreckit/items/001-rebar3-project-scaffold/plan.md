# Set up rebar3 project scaffold Implementation Plan

## Implementation Plan Title

rebar3 Project Scaffold for wf_substrate OTP Application

## Overview

Establish the foundational rebar3 project structure for the wf_substrate application. This greenfield OTP application will implement a compiled workflow pattern substrate with pure Erlang/OTP only (no external dependencies). The scaffold includes rebar.config with OTP 26+ requirement, OTP application metadata files, standard directory layout (src/, test/, docs/), and updated .gitignore. The project must compile cleanly and pass an empty EUnit suite with zero tests.

## Current State

**What Exists Now:**
- Git repository initialized at `/Users/speed/wf-substrate`
- Minimal `.gitignore` containing only `.wreckit/config.local.json` (lines 1-4)
- Comprehensive project specification in `PROMPT.md` defining the entire workflow pattern substrate architecture
- Wreckit workflow metadata in `.wreckit/` directory

**What's Missing:**
- No `rebar.config` file
- No `src/` directory or Erlang source files
- No `test/` directory or test files
- No `docs/` directory
- No OTP application structure (app.src, application callback, supervisor)
- No build system configuration

**Key Constraints Discovered:**
- Pure Erlang/OTP only - no external dependencies (PROMPT.md:19-21)
- OTP 26+ minimum version requirement (PROMPT.md:13)
- Must use OTP behaviors: gen_server, gen_statem, supervisor (PROMPT.md:21, 181-186)
- Application name: `wf_substrate` (PROMPT.md:48)
- Eventually 20+ modules but scaffold needs only core OTP structure (PROMPT.md:50-65)

## Desired End State

A complete rebar3 project structure that:

1. **Builds Successfully**: `rebar3 compile` completes without errors
2. **Tests Pass**: `rebar3 eunit` runs successfully with "0 tests, 0 failures" output
3. **OTP Compliant**: Standard application resource file, application callback module, and top-level supervisor
4. **Properly Configured**: rebar.config enforces OTP 26+, enables strict compilation (warnings_as_errors), includes no external dependencies
5. **Clean Repository**: .gitignore prevents build artifacts (_build/, .rebar3/, *.beam, dumps, logs) from being committed

### Key Discoveries:

- The specification defines 20+ modules (PROMPT.md:50-65) but for the scaffold we only create 3 core modules to avoid warnings
- OTP 26+ provides modern logger API and improved type specifications (PROMPT.md:13)
- Supervision tree will eventually have wf_case_sup, wf_effect_sup, wf_trace_sink but starts empty (PROMPT.md:181-186)
- EUnit and Common Test will both use test/ directory (PROMPT.md:202-240)
- Documentation will be .md files in docs/ (PROMPT.md:67-72)

## What We're NOT Doing

- **NOT** implementing any workflow pattern logic (deferred to items 002-019)
- **NOT** adding external dependencies beyond kernel, stdlib, sasl
- **NOT** creating Common Test suites yet (only empty EUnit file)
- **NOT** generating API documentation (Edoc/ExDoc configuration deferred)
- **NOT** populating docs/ with content (empty directory only)
- **NOT** adding any actual test cases (zero tests requirement)
- **NOT** implementing the supervision tree children (empty supervisor only)
- **NOT** creating any of the 20+ planned modules beyond the core 3 OTP modules

## Implementation Approach

**Strategy:** Create a minimal but complete OTP application scaffold following rebar3 conventions. The approach is incremental and testable:

1. **Phase 1 (Configuration)**: Establish build configuration and repository hygiene
   - Create rebar.config with OTP 26+ requirement and strict compilation
   - Update .gitignore to exclude all Erlang/OTP build artifacts
   - This phase is independently verifiable by file existence checks

2. **Phase 2 (Application Structure)**: Implement core OTP application modules
   - Create wf_substrate.app.src with proper application metadata
   - Create wf_substrate_app.erl (application callback)
   - Create wf_substrate_sup.erl (empty top-level supervisor)
   - Create wf_substrate.erl (placeholder public API)
   - This phase is independently verifiable by `rebar3 compile`

3. **Phase 3 (Test Infrastructure)**: Establish testing foundation
   - Create test/ directory with empty EUnit test suite
   - Create docs/ directory (empty)
   - This phase is independently verifiable by `rebar3 eunit`

4. **Phase 4 (Verification)**: Complete end-to-end validation
   - Verify compilation succeeds
   - Verify test suite runs with zero tests
   - Verify gitignore prevents artifact commits
   - Verify OTP 26+ requirement is enforced

**Rationale:** This phased approach isolates concerns, allows for incremental rollback if issues arise, and follows the principle of building the simplest thing that works. The empty supervisor and placeholder API module provide extensibility points for future items without introducing complexity or warnings.

---

## Phases

### Phase 1: Build Configuration and Repository Hygiene

#### Overview

Create the rebar3 build configuration and update .gitignore to establish a clean, maintainable project foundation. This phase ensures the build system is properly configured for OTP 26+ with strict quality checks, and that build artifacts won't be committed to version control.

#### Changes Required:

##### 1. rebar.config

**File**: `/Users/speed/wf-substrate/rebar.config`
**Changes**: Create new file with build configuration

```erlang
{erl_opts, [debug_info,
            warnings_as_errors,
            {require_min_otp_vsn, 26}]}.

{shell_apps, [sasl]}.

{xref_checks, [undefined_function_calls,
               undefined_functions,
               locals_not_used,
               deprecated_function_calls,
               deprecated_functions]}.

{deps, []}.

{plugins, []}.

{dialyzer, [
    {warnings, [
        error_handling,
        race_conditions,
        unmatched_returns
    ]}
]}.

{profiles, [
    {test, [
        {erl_opts, [debug_info, {require_min_otp_vsn, 26}]}
    ]}
]}.
```

**Rationale:**
- `debug_info` enables debugging and tooling support
- `warnings_as_errors` enforces code quality from the start
- `require_min_otp_vsn: 26` ensures OTP 26+ is used
- `shell_apps: [sasl]` provides better REPL experience with supervision tree visibility
- `xref_checks` catch cross-module issues early
- Empty `deps` and `plugins` explicitly declare no external dependencies
- `dialyzer` configuration enables optional static analysis
- Test profile allows warnings during testing (can be adjusted)

##### 2. .gitignore

**File**: `/Users/speed/wf-substrate/.gitignore`
**Changes**: Add standard Erlang/OTP build artifacts after existing Wreckit exclusion

```gitignore
# Wreckit local config (may contain secrets)
.wreckit/config.local.json

# Erlang/OTP build artifacts
_build/
.rebar3/
*.beam
erl_crash.dump
rebar3.crashdump
*.dump
*.log
log/

# OS files
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db

# Editor files
*~
.*.swp
.*.swo
.*.sw?
.*.bak
```

**Rationale:**
- `_build/` contains all rebar3 compile output
- `.rebar3/` contains rebar3 cache and state
- `*.beam` files are compiled Erlang modules
- `erl_crash.dump` and `rebar3.crashdump` are VM crash dumps
- `*.dump` catches any other dump files
- `*.log` and `log/` exclude log files
- OS and editor exclusions prevent common accidental commits
- Preserves existing Wreckit exclusion at top

#### Success Criteria:

##### Automated Verification:

- [ ] File exists: `/Users/speed/wf-substrate/rebar.config`
- [ ] File exists: `/Users/speed/wf-substrate/.gitignore`
- [ ] rebar.config contains `{require_min_otp_vsn, 26}`
- [ ] rebar.config contains `{deps, []}` (no external dependencies)
- [ ] .gitignore contains `_build/` and `.rebar3/`
- [ ] .gitignore contains `*.beam` and `erl_crash.dump`

##### Manual Verification:

- [ ] rebar.config follows Erlang syntax (can be parsed)
- [ ] .gitignore preserves existing Wreckit exclusion
- [ ] No unintended file patterns are excluded

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: OTP Application Structure

#### Overview

Create the core OTP application structure including application resource file (.app.src), application callback module, top-level supervisor, and placeholder public API module. This establishes the standard OTP application skeleton that will be expanded in subsequent items.

#### Changes Required:

##### 1. wf_substrate.app.src

**File**: `/Users/speed/wf-substrate/src/wf_substrate.app.src`
**Changes**: Create new application resource file

```erlang
{application, wf_substrate,
 [{description, "Workflow pattern substrate for compiled pattern execution"},
  {vsn, "0.1.0"},
  {registered, [wf_substrate_sup]},
  {mod, {wf_substrate_app, []}},
  {applications,
   [kernel,
    stdlib,
    sasl
   ]},
  {env,[]},
  {modules, []},
  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.
```

**Rationale:**
- `description` summarizes the application's purpose
- `vsn "0.1.0"` follows semantic versioning for initial release
- `registered` declares the top-level supervisor name
- `mod` declares the application callback module
- `applications` lists OTP dependencies (kernel, stdlib, sasl)
- `modules` left empty for rebar3 auto-discovery (avoids warnings)
- `licenses` and `links` provide standard application metadata

##### 2. wf_substrate_app.erl

**File**: `/Users/speed/wf-substrate/src/wf_substrate_app.erl`
**Changes**: Create application callback module

```erlang
%%%-------------------------------------------------------------------
%%% @doc wf_substrate application callback
%%% @end
%%%-------------------------------------------------------------------
-module(wf_substrate_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    wf_substrate_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
```

**Rationale:**
- Implements `application` behaviour as required by OTP
- `start/2` initiates the top-level supervisor
- `stop/1` performs cleanup (currently empty, but ready for expansion)
- Standard OTP documentation comments
- Clean separation of concerns (application delegates to supervisor)

##### 3. wf_substrate_sup.erl

**File**: `/Users/speed/wf-substrate/src/wf_substrate_sup.erl`
**Changes**: Create top-level supervisor (empty, ready for expansion)

```erlang
%%%-------------------------------------------------------------------
%%% @doc Top-level supervisor for wf_substrate
%%% @end
%%%-------------------------------------------------------------------
-module(wf_substrate_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child specifications.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
```

**Rationale:**
- Implements `supervisor` behaviour as required by OTP
- `start_link/0` registers the supervisor locally as `wf_substrate_sup`
- `init/1` defines supervision strategy and children
- Uses modern map-based supervisor syntax (OTP 26+ compatible)
- `one_for_one` strategy is appropriate for the planned independent children
- Empty `ChildSpecs` avoids warnings about undefined modules
- Intensity 10, period 60 seconds provides standard restart limits
- Ready to add wf_case_sup, wf_effect_sup, wf_trace_sink in future items

##### 4. wf_substrate.erl

**File**: `/Users/speed/wf-substrate/src/wf_substrate.erl`
**Changes**: Create placeholder public API module

```erlang
%%%-------------------------------------------------------------------
%%% @doc Public API for wf_substrate
%%%
%%% This module provides the main entry point for the workflow pattern
%%% substrate. Initially a placeholder, it will be expanded to provide
%%% the full API defined in the specification.
%%%
%%% Planned API functions (from PROMPT.md:273-282):
%%% - new_case/3: Create a new workflow case
%%% - signal/2: Send a signal to a running case
%%% - cancel/1: Cancel a running case
%%% - cancel_region/2: Cancel a specific region
%%% - await/2: Await case completion
%%% - status/1: Query case status
%%% - trace/3: Retrieve trace events
%%% - validate/2: Validate a workflow term
%%% @end
%%%-------------------------------------------------------------------
-module(wf_substrate).

%% Public API (to be implemented in future items)
%% -export([new_case/3, signal/2, cancel/1, cancel_region/2,
%%          await/2, status/1, trace/3, validate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
```

**Rationale:**
- Serves as the public API facade for the entire application
- Documents the planned API from the specification (PROMPT.md:273-282)
- Currently empty with no exports (avoids compiler warnings)
- Provides clear roadmap for future implementation
- Establishes module purpose through documentation

#### Success Criteria:

##### Automated Verification:

- [ ] File exists: `/Users/speed/wf-substrate/src/wf_substrate.app.src`
- [ ] File exists: `/Users/speed/wf-substrate/src/wf_substrate_app.erl`
- [ ] File exists: `/Users/speed/wf-substrate/src/wf_substrate_sup.erl`
- [ ] File exists: `/Users/speed/wf-substrate/src/wf_substrate.erl`
- [ ] All files are valid Erlang syntax
- [ ] Command succeeds: `rebar3 compile`
- [ ] No compiler errors or warnings
- [ ] Compiled .beam files exist in `_build/default/lib/wf_substrate/ebin/`

##### Manual Verification:

- [ ] Application metadata is correct (name, description, vsn)
- [ ] Application callback module starts the supervisor
- [ ] Supervisor uses map-based syntax (OTP 26+ compatible)
- [ ] No "undefined module" warnings
- [ ] Application can be loaded with `application:load(wf_substrate)`

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Test Infrastructure and Documentation Directory

#### Overview

Establish the testing foundation with an empty EUnit test suite and create the documentation directory. This ensures the project can run `rebar3 eunit` successfully with zero tests, and provides a location for future documentation files.

#### Changes Required:

##### 1. wf_substrate_tests.erl

**File**: `/Users/speed/wf-substrate/test/wf_substrate_tests.erl`
**Changes**: Create empty EUnit test suite

```erlang
%%%-------------------------------------------------------------------
%%% @doc EUnit test suite for wf_substrate
%%%
%%% This module provides unit tests for the wf_substrate application.
%%% Initially empty, tests will be added as functionality is implemented.
%%% @end
%%%-------------------------------------------------------------------
-module(wf_substrate_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% No tests yet - this module exists to satisfy rebar3 eunit
%% Test cases will be added as functionality is implemented
```

**Rationale:**
- Includes EUnit header for test framework
- Contains no test functions (zero tests requirement)
- Documents intent to add tests in future items
- Prevents "no test files found" errors from rebar3 eunit
- Establishes testing structure for the project

##### 2. docs/ Directory

**Directory**: `/Users/speed/wf-substrate/docs/`
**Changes**: Create empty directory

**Rationale:**
- Provides location for future documentation (ARCHITECTURE.md, SEMANTICS.md, PATTERNS.md, TESTING.md, OPERATIONS.md from PROMPT.md:67-72)
- Empty directory commits intent without adding content
- Git will track the directory structure

#### Success Criteria:

##### Automated Verification:

- [ ] Directory exists: `/Users/speed/wf-substrate/test/`
- [ ] Directory exists: `/Users/speed/wf-substrate/docs/`
- [ ] File exists: `/Users/speed/wf-substrate/test/wf_substrate_tests.erl`
- [ ] Test file is valid Erlang syntax
- [ ] Command succeeds: `rebar3 eunit`
- [ ] Output includes "0 tests, 0 failures" or similar

##### Manual Verification:

- [ ] EUnit test file includes eunit.hrl
- [ ] No test functions are defined (zero tests)
- [ ] docs/ directory is tracked by Git
- [ ] Running `rebar3 eunit` produces expected output

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Final Verification and Validation

#### Overview

Perform comprehensive end-to-end verification that the rebar3 scaffold is complete and meets all requirements. This phase validates the entire setup and ensures the project is ready for subsequent development items.

#### Verification Steps:

##### 1. Compilation Verification

**Command**: `rebar3 compile`

**Expected Results:**
- Compiles successfully with no errors
- No warnings (warnings_as_errors is enabled)
- .beam files generated in `_build/default/lib/wf_substrate/ebin/`
- Output shows " wf_substrate" compilation

##### 2. Test Execution Verification

**Command**: `rebar3 eunit`

**Expected Results:**
- Runs successfully
- Output shows "0 tests, 0 failures" or "Test passed"
- No errors about missing test files
- Completes quickly (no actual tests to run)

##### 3. Gitignore Verification

**Commands**:
```bash
git status
ls -la _build/ 2>/dev/null || echo "_build/ not tracked (correct)"
```

**Expected Results:**
- `git status` does not show `_build/`, `.rebar3/`, or `*.beam` files
- Build artifacts are correctly ignored
- Only source files are tracked

##### 4. OTP Version Verification

**Command**: `erl -version`

**Expected Results:**
- Shows OTP 26 or higher
- If lower version, rebar3 compile should fail with "require_min_otp_vsn" error

##### 5. Application Loading Verification

**Commands** (in Erlang shell):
```erlang
application:load(wf_substrate).
application:which_applications().
```

**Expected Results:**
- `application:load/1` returns `{ok, wf_substrate}` or `{error, {already_loaded, wf_substrate}}`
- Application metadata is accessible
- No errors about missing modules or dependencies

##### 6. Dependency Verification

**Check**: Verify no external dependencies

**Expected Results:**
- `rebar.config` contains `{deps, []}`
- `rebar3 tree` shows only kernel, stdlib, sasl
- No external packages are downloaded

##### 7. Directory Structure Verification

**Command**: `find . -type d -not -path '*/\.*' -not -path '*/_build/*' | head -20`

**Expected Structure**:
```
.
./src
./test
./docs
```

**Expected Results:**
- Standard OTP/rebar3 directory layout
- src/ contains .erl and .app.src files
- test/ contains test files
- docs/ directory exists (empty or with future content)

#### Success Criteria:

##### Automated Verification:

- [ ] Command succeeds: `rebar3 compile` (no errors, no warnings)
- [ ] Command succeeds: `rebar3 eunit` (shows "0 tests")
- [ ] Command succeeds: `rebar3 tree` (shows only stdlib deps)
- [ ] Git status shows no build artifacts
- [ ] Directory structure matches expected layout
- [ ] All source files are tracked by Git

##### Manual Verification:

- [ ] OTP version is 26+ (checked with `erl -version`)
- [ ] Application can be loaded in Erlang shell
- [ ] Project compiles cleanly from scratch (delete _build/ and recompile)
- [ ] .gitignore excludes all expected build artifacts
- [ ] No external dependencies are configured or downloaded

**Note**: This is the final phase. Complete all verification steps before marking the item complete.

---

## Testing Strategy

### Unit Tests:

- **Zero Tests**: As per requirements, the initial EUnit suite contains zero test functions
- **Test Infrastructure**: Test file exists and includes eunit.hrl to satisfy rebar3
- **Future Expansion**: Structure is ready for test additions in items 014-016

### Integration Tests:

- **None Yet**: Common Test suites will be added in future items
- **Test Directory**: Single test/ directory will accommodate both EUnit and CT

### Manual Testing Steps:

1. **Fresh Build Test**:
   ```bash
   rm -rf _build/ .rebar3/
   rebar3 compile
   ```
   Expected: Clean compilation with no errors or warnings

2. **Application Start Test**:
   ```bash
   rebar3 shell
   > application:start(wf_substrate).
   > application:stop(wf_substrate).
   ```
   Expected: Application starts and stops successfully

3. **Git Clean Test**:
   ```bash
   git status
   git add -A
   git status
   ```
   Expected: Only source files staged, no build artifacts

4. **Version Enforcement Test** (if OTP < 26 available):
   ```bash
   # On OTP 25 or lower, this should fail
   rebar3 compile
   ```
   Expected: Error about OTP version requirement

## Migration Notes

Not applicable - this is a greenfield project with no existing code or data to migrate.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/001-rebar3-project-scaffold/research.md`
- Project Specification: `/Users/speed/wf-substrate/PROMPT.md`
  - Module list: lines 50-65
  - OTP requirements: lines 179-198
  - Acceptance criteria: lines 350-360
  - API surface: lines 270-289
- rebar3 Documentation: https://rebar3.org/docs/
- OTP Design Principles: https://erlang.org/doc/design_principles/applications.html
