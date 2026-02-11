# Fix compilation error in test/wf_test_join.erl — variable Tokens is unbound on lines 95, 135, 164, 244. The test needs to extract the tokens map from the exec_state record before using it. After fixing, verify rebar3 eunit runs all tests and they pass. Implementation Plan

## Implementation Plan Title

Fix Variable Naming Mismatch in wf_test_join.erl Token Extraction

## Overview

The test file `test/wf_test_join.erl` contains compilation errors due to a variable naming mismatch. Tests extract tokens from the exec_state record using the pattern `_Tokens = ExecState1#exec_state.tokens` but then attempt to reference the variable as `Tokens` (without the underscore prefix). In Erlang, `_Tokens` and `Tokens` are different variable names - the underscore prefix creates a distinct variable that also signals to the compiler "this variable is intentionally unused".

This fix corrects the variable naming to match the pattern used successfully in `test/wf_test_par.erl` and ensures all tests compile and pass.

## Current State

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

### Key Discoveries:

- **Lines with compilation errors**: 6 locations where `_Tokens` is bound but `Tokens` is used
  - Lines 67-68 in `join_wait_all_3_test()`
  - Lines 78-79 in `join_wait_all_5_test()`
  - Lines 94-95 in `join_wait_n_2_of_3_test()`
  - Lines 134-135 in `join_first_complete_4_test()`
  - Lines 163-164 in `join_sync_merge_test()`
  - Lines 243-244 in parameterized test `join_wait_all_m_branches_test_()`

- **Lines with unused bindings**: 5 locations where `_Tokens` is bound but never used (these can keep the underscore or be removed)
  - Lines 107, 118, 147, 214, 224

- **Correct pattern exists**: `test/wf_test_par.erl:88-89` shows the proper way to extract and use tokens

- **exec_state structure**: From `src/wf_exec.hrl:37`, the tokens field is defined as `tokens :: #{term() => #token{}}`

- **Helper function pattern**: The `count_tokens_with_status/2` function at line 253-256 correctly accesses `ExecState#exec_state.tokens` directly, which is why some tests don't need the extracted variable

## Desired End State

The test file `test/wf_test_join.erl` compiles without errors and all tests pass when run with `rebar3 eunit`. The variable naming follows the established pattern from `test/wf_test_par.erl` where tokens are extracted as `Tokens = ExecState1#exec_state.tokens` (without underscore prefix) when the variable is subsequently used.

**Verification:**
- `rebar3 compile` completes without errors
- `rebar3 eunit --module=wf_test_join` runs all 17 tests successfully
- `rebar3 eunit` runs the full test suite without regressions

## What We're NOT Doing

- **NOT modifying the exec_state record structure**: The record definition is correct; this is purely a test file variable naming issue
- **NOT changing test logic**: The fix only changes variable names, not the test assertions or behavior
- **NOT modifying production code**: This change is isolated to the test file only
- **NOT removing unused _Tokens bindings (optional cleanup)**: Lines 107, 118, 147, 214, 224 bind `_Tokens` but never use it. While this is dead code, removing it is not required for fixing the compilation error. These lines will be left as-is since they don't cause errors and removing them could be considered scope creep.

## Implementation Approach

The fix is straightforward and low-risk: change `_Tokens` to `Tokens` on the 6 lines where the variable is subsequently used. This matches the pattern used in `test/wf_test_par.erl` and follows Erlang variable naming conventions.

**Why this approach:**
1. **Minimal change**: Only fixes what's broken - the variable naming mismatch
2. **Follows existing patterns**: Matches the correct pattern in `wf_test_par.erl`
3. **No logic changes**: Tests will work correctly once variables match
4. **Easy to verify**: Compilation will succeed immediately after the fix

**Risks and Mitigations:**

| Risk | Impact | Mitigation |
| ---- | ---- | ---- |
| Breaking existing test logic | Low | The fix only changes variable names, not logic. Tests will work correctly once variables match |
| Missing some occurrences | Low | All 6 problematic lines documented. Can verify with `rebar3 compile` |
| Tests may fail for other reasons | Medium | Run full `rebar3 eunit` suite after fix to ensure no regressions |
| Similar issues in other test files | Low | Reviewed `wf_test_cancel.erl` which correctly uses `_Tokens` pattern where it's truly unused |

---

## Phases

### Phase 1: Fix Variable Naming Mismatches

#### Overview

Correct the variable naming on 6 lines where `_Tokens` is bound but `Tokens` is used, causing compilation errors.

#### Changes Required:

##### 1. test/wf_test_join.erl

**File**: `test/wf_test_join.erl`
**Changes**: Change `_Tokens` to `Tokens` on lines where the variable is subsequently used

**Line 67** (in `join_wait_all_3_test()`):
```erlang
Tokens = ExecState1#exec_state.tokens,
```

**Line 78** (in `join_wait_all_5_test()`):
```erlang
Tokens = ExecState1#exec_state.tokens,
```

**Line 94** (in `join_wait_n_2_of_3_test()`):
```erlang
Tokens = ExecState1#exec_state.tokens,
```

**Line 134** (in `join_first_complete_4_test()`):
```erlang
Tokens = ExecState1#exec_state.tokens,
```

**Line 163** (in `join_sync_merge_test()`):
```erlang
Tokens = ExecState1#exec_state.tokens,
```

**Line 243** (in `join_wait_all_m_branches_test_()`):
```erlang
Tokens = ExecState1#exec_state.tokens,
```

**Note**: Lines 107, 118, 147, 214, 224 bind `_Tokens` but never use it. These are left unchanged as they don't cause compilation errors and are out of scope for this fix.

#### Success Criteria:

##### Automated Verification:

- [ ] Compilation succeeds: `rebar3 compile` completes without errors
- [ ] Specific test module passes: `rebar3 eunit --module=wf_test_join` runs all 17 tests successfully
- [ ] Full test suite passes: `rebar3 eunit` completes without regressions

##### Manual Verification:

- [ ] No compiler warnings about unbound variables
- [ ] No compiler warnings about unused variables (the existing `_Tokens` bindings on lines 107, 118, 147, 214, 224 suppress these warnings as intended)
- [ ] All test assertions pass with expected token counts

**Note**: Complete all automated verification, then confirm the fix is complete.

---

## Testing Strategy

### Unit Tests:

The fix is for the test file itself. The tests verify:
- **wait_all policy**: All M branches complete (tests with M=3, M=5, and parameterized M=2,3,5,10)
- **wait_n policy**: N of M branches complete, rest cancelled (tests with 2/3, 3/5, 1/4)
- **first_complete policy**: Only 1 branch completes, rest cancelled (tests with M=4, M=10)
- **sync_merge policy**: All branches complete (test with M=3)
- **Join counter mechanics**: Verify counter increments and threshold checking
- **Edge cases**: N=M (all complete), N=1 (only 1 completes)

### Integration Tests:

- Full test suite execution via `rebar3 eunit` to ensure no regressions in other test modules

### Manual Testing Steps:

1. **Verify compilation**:
   ```bash
   cd /Users/speed/wf-substrate
   rebar3 compile
   ```
   Expected: No errors about unbound variable `Tokens`

2. **Run specific test module**:
   ```bash
   rebar3 eunit --module=wf_test_join
   ```
   Expected: All 17 tests pass (13 named tests + 4 parameterized tests)

3. **Run full test suite**:
   ```bash
   rebar3 eunit
   ```
   Expected: All tests across all modules pass, no regressions

4. **Verify test output**:
   - Check that token counts match expectations (root + branch tokens)
   - Verify that join counters are tracked correctly
   - Confirm that cancelled tokens are properly counted for wait_n and first_complete policies

## Migration Notes

Not applicable - this is a test-only fix with no data migration or backwards compatibility concerns.

## References

- Research: `/Users/speed/wf-substrate/.wreckit/items/023-fix-compilation-error-in-testwftestjoinerl-variabl/research.md`
- Problem file: `/Users/speed/wf-substrate/test/wf_test_join.erl`
- Correct pattern example: `/Users/speed/wf-substrate/test/wf_test_par.erl:88-89, 102-103, 117-118`
- Record definition: `/Users/speed/wf-substrate/src/wf_exec.hrl:32-44`
