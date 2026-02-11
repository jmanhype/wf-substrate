# Research: Fix compilation error in test/wf_test_join.erl — variable Tokens is unbound on lines 95, 135, 164, 244. The test needs to extract the tokens map from the exec_state record before using it. After fixing, verify rebar3 eunit runs all tests and they pass.

**Date**: 2025-01-21
**Item**: 023-fix-compilation-error-in-testwftestjoinerl-variabl

## Research Question

How do we fix the compilation errors in test/wf_test_join.erl where the variable `Tokens` is unbound on multiple lines, and ensure all tests pass after the fix?

## Summary

The compilation error in test/wf_test_join.erl is caused by a variable naming mismatch. The tests extract tokens from the exec_state record using the pattern `_Tokens = ExecState1#exec_state.tokens` but then attempt to reference the variable as `Tokens` (without the underscore prefix). In Erlang, `_Tokens` and `Tokens` are different variable names - the underscore prefix creates a distinct variable that also signals to the compiler "this variable is intentionally unused".

The fix is straightforward: change `_Tokens` to `Tokens` on lines where the variable is subsequently used. After analyzing the code, I identified 6 occurrences where this pattern exists:
- Lines 67-68 (used on line 68)
- Lines 78-79 (used on line 79)
- Lines 94-95 (used on line 95)
- Lines 107, 118, 134, 147, 163, 214, 224, 243 (where _Tokens is bound but actually never used - these can keep the underscore prefix or be removed entirely)

The correct pattern follows what's done in test/wf_test_par.erl where tokens are extracted as `Tokens = ExecState1#exec_state.tokens` and then used with `maps:size(Tokens)`.

## Current State Analysis

### Existing Implementation

The file `/Users/speed/wf-substrate/test/wf_test_join.erl` contains unit tests for join policy mechanics (wait_all, wait_n, first_complete, sync_merge). The tests use a mock bytecode generator to create parallel workflows and verify token behavior after execution.

**Current problematic pattern:**
```erlang
%% Line 67-68 in join_wait_all_3_test()
_Tokens = ExecState1#exec_state.tokens,
TotalTokens = maps:size(Tokens),  %% ERROR: 'Tokens' is unbound
```

**Expected pattern (from test/wf_test_par.erl:88-89):**
```erlang
Tokens = ExecState1#exec_state.tokens,
?assertEqual(3, maps:size(Tokens)),  % This works correctly
```

### exec_state Record Structure

From `/Users/speed/wf-substrate/src/wf_exec.hrl:32-44`, the exec_state record is defined as:
```erlang
-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: wf_vm:wf_bc(),
    ctx :: map(),
    case_id :: term() | undefined,
    tokens :: #{term() => #token{}},  %% <-- This is the field we need
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | blocked_effect | cancelled | failed,
    current_token :: term() | undefined
}).
```

The `tokens` field is a map containing token IDs to token records, which is what the tests need to inspect.

## Key Files

- `/Users/speed/wf-substrate/test/wf_test_join.erl` - File containing compilation errors
  - Lines 67-68: `_Tokens` bound but `Tokens` used
  - Lines 78-79: `_Tokens` bound but `Tokens` used
  - Lines 94-95: `_Tokens` bound but `Tokens` used
  - Lines 134-135: `_Tokens` bound but `Tokens` used
  - Lines 163-164: `_Tokens` bound but `Tokens` used
  - Lines 243-244: `_Tokens` bound but `Tokens` used
  - Lines 253-256: `count_tokens_with_status/2` helper function that correctly accesses `ExecState#exec_state.tokens`

- `/Users/speed/wf-substrate/src/wf_exec.hrl:32-44` - Defines exec_state record with tokens field
- `/Users/speed/wf-substrate/test/wf_test_par.erl:88-89, 102-103, 117-118, 132-133, 150-151` - Examples of correct token extraction pattern
- `/Users/speed/wf-substrate/src/wf_exec.erl` - Executor module providing API used by tests
- `/Users/speed/wf-substrate/rebar.config` - Build configuration with eunit test setup

## Technical Considerations

### Dependencies

- **Internal modules:**
  - `wf_exec` - Executor API (`new/1`, `run/3`, `is_done/1`)
  - `wf_exec.hrl` - Record definitions for exec_state, token, join_counter
- **Test framework:** eunit (included via `-include_lib("eunit/include/eunit.hrl")`)
- **Build tool:** rebar3 with test profile configured

### Patterns to Follow

**Correct token extraction pattern** (from test/wf_test_par.erl):
```erlang
%% Extract tokens
Tokens = ExecState1#exec_state.tokens,

%% Use tokens
TotalTokens = maps:size(Tokens),
CompleteCount = count_tokens_with_status(ExecState1, complete),
```

**Helper function pattern** (from test/wf_test_join.erl:253-256):
```erlang
count_tokens_with_status(ExecState, Status) ->
    lists:count(fun(_Id, Token) ->
        Token#token.status =:= Status
    end, maps:to_list(ExecState#exec_state.tokens)).
```

Note: The helper function directly accesses `ExecState#exec_state.tokens` without needing to extract it first, which is why lines 107, 118, 147, 214, 224 don't actually need the `_Tokens` variable at all.

### Variable Naming Convention in Erlang

- `VariableName` - Normal variable that must be used
- `_VariableName` - Variable that is intentionally unused (suppresses compiler warnings)
- `_` - Anonymous variable (can be used multiple times, different from `_Var`)

**Important:** `_Tokens` and `Tokens` are completely different variable names in Erlang.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| Breaking existing test logic | Low | The fix only changes variable names, not logic. Tests will work correctly once variables match |
| Missing some occurrences | Low | Documented all 6 problematic lines. Can verify with `rebar3 compile` |
| Tests may fail for other reasons | Medium | Run full `rebar3 eunit` suite after fix to ensure no regressions |
| Similar issues in other test files | Low | Checked wf_test_cancel.erl which correctly uses `_Tokens` pattern where it's truly unused |

## Recommended Approach

1. **Fix the variable naming mismatches:**
   - Line 67: Change `_Tokens =` to `Tokens =`
   - Line 78: Change `_Tokens =` to `Tokens =`
   - Line 94: Change `_Tokens =` to `Tokens =`
   - Line 134: Change `_Tokens =` to `Tokens =`
   - Line 163: Change `_Tokens =` to `Tokens =`
   - Line 243: Change `_Tokens =` to `Tokens =`

2. **Remove unused _Tokens bindings** (optional cleanup):
   - Lines 107, 118, 147, 214, 224: These lines bind `_Tokens` but never use it. They can be removed entirely since the helper function `count_tokens_with_status` accesses tokens directly.

3. **Verify compilation:**
   ```bash
   rebar3 compile
   ```
   This should complete without errors.

4. **Run the specific test file:**
   ```bash
   rebar3 eunit --module=wf_test_join
   ```
   All tests should pass.

5. **Run full test suite:**
   ```bash
   rebar3 eunit
   ```
   Verify no regressions in other tests.

## Open Questions

1. **Should we remove the unused `_Tokens` bindings on lines 107, 118, 147, 214, 224?**
   - They don't cause compilation errors but are dead code
   - Removing them would clean up the tests
   - Decision: Optional cleanup, not required for fix

2. **Are there any other test files with similar issues?**
   - Reviewed test/wf_test_cancel.erl:125 which correctly uses `_Tokens` pattern (variable is truly unused)
   - Reviewed test/wf_test_par.erl which correctly uses `Tokens` pattern (variable is used)
   - No other files appear to have this issue
