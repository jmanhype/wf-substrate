# Workflow Pattern Coverage

## Overview

This document maps workflow patterns from the van der Aalst et al. catalog to wf_substrate implementation. It distinguishes:
- **Kernel primitives**: Direct opcode support in wf_exec:execute_opcode/2
- **Derived patterns**: Combinations of kernel primitives
- **Not implemented**: Patterns without test coverage or stubbed implementations

## Pattern Categories

1. **Basic Control Flow**: Sequence, Task
2. **Advanced Branching**: Parallel split, Synchronization, Multi-choice, Synchronizing merge, Discriminator, N-out-of-M join
3. **Structural**: Exclusive choice, Simple merge
4. **Multiple Instance**: Fixed count, Dynamic count, with join policies
5. **State-Based**: Arbitrary cycles (Loop)
6. **Cancellation**: Cancel activity, Cancel case, Cancel region

## Pattern Table

| Pattern # | Pattern Name | Category | wf_term() | Bytecode Opcodes | Test Modules | Implementation | Limitations |
|-----------|--------------|----------|-----------|------------------|--------------|----------------|-------------|
| 1 | Sequence | Basic | seq(P, Q) | SEQ_ENTER, SEQ_NEXT, DONE | wf_test_seq.erl | Kernel | - |
| 2 | Parallel Split (AND-split) | Advanced Branching | par([P, ...]) | PAR_FORK | wf_test_par.erl | Kernel | - |
| 3 | Synchronization (AND-join) | Advanced Branching | join(all, [P, ...]) | JOIN_WAIT(all) | wf_test_par.erl, wf_test_join.erl | Kernel | - |
| 4 | Exclusive Choice (XOR-split) | Structural | xor([P, ...]) | XOR_CHOOSE | wf_test_xor.erl | Kernel | - |
| 5 | Simple Merge (XOR-join) | Structural | N/A (implicit) | None | wf_test_xor.erl | Kernel | No explicit opcode |
| 6 | Multi-choice | Advanced Branching | xor([P, ...]) | XOR_CHOOSE | wf_test_xor.erl | Derived | Reuses XOR |
| 7 | Synchronizing Merge | Advanced Branching | join(sync_merge, [P, ...]) | JOIN_WAIT(sync_merge) | wf_test_join.erl | Kernel | wf_exec.erl:358-365 |
| 8 | Discriminator | Advanced Branching | join({first_n,1}, [P, ...]) | JOIN_WAIT(first_complete) | wf_test_join.erl | Kernel | First completion joins |
| 9 | N-out-of-M Join | Advanced Branching | join({n_of_m,N}, [P, ...]) | JOIN_WAIT({n_of_m,N,M}) | wf_test_join.erl | Kernel | - |
| 10 | Arbitrary Cycles (Loop) | State-Based | loop(Policy, P) | LOOP_CHECK, LOOP_BACK | wf_test_seq.erl (nested) | Kernel | wf_exec.erl:396-433 |
| 11 | Cancel Activity | Cancellation | cancel(task, P) | CANCEL_SCOPE({enter,task}) | wf_test_cancel.erl | Stubbed | wf_exec.erl:512-529 (TODO item 008) |
| 12 | Cancel Case | Cancellation | cancel(case, P) | CANCEL_SCOPE({enter,case}) | wf_test_cancel.erl | Stubbed | wf_exec.erl:512-529 (TODO item 008) |
| 13 | Cancel Region | Cancellation | cancel(scope, P) | CANCEL_SCOPE({enter,ScopeId}) | wf_test_cancel.erl | Stubbed | wf_exec.erl:512-529 (TODO item 008) |
| 14 | Multiple Instances (Fixed) | Multiple Instance | mi({fixed,N}, P) | MI_SPAWN({fixed,N}) | wf_test_mi.erl | Not Implemented | Test exists, no opcode handler |
| 15 | Multiple Instances (Dynamic) | Multiple Instance | mi({dynamic,Min,Max}, P) | MI_SPAWN({dynamic,Min,Max}) | wf_test_mi.erl | Not Implemented | Test exists, no opcode handler |
| 16 | Task | Basic | task(Name, Fun) | TASK_EXEC | wf_test_seq.erl | Kernel | - |
| 17 | Deferred Choice | State-Based | defer([P, ...]) | Not implemented | None | Not Implemented | No test coverage |
| 18 | Multi Merge | Advanced Branching | Derived | Multiple JOIN_WAIT | None | Not Implemented | No test coverage |
| 19-43 | Remaining Patterns | Various | N/A | N/A | None | Not Implemented | Out of scope for control-flow engine |

## Kernel Primitives (Opcodes)

From src/wf_vm.erl:13-21, the following opcodes have direct handlers in wf_exec:execute_opcode/2:

- **SEQ_ENTER**: Enter sequence (wf_exec.erl:229-236)
- **SEQ_NEXT**: Advance to next sequence element (wf_exec.erl:238-245)
- **PAR_FORK**: Fork parallel branches (wf_exec.erl:247-283)
- **JOIN_WAIT**: Wait for branch completion with policy (wf_exec.erl:285-365)
- **XOR_CHOOSE**: Choose exclusive branch (wf_exec.erl:367-394)
- **LOOP_CHECK**: Check loop exit condition (wf_exec.erl:396-419)
- **LOOP_BACK**: Jump back to loop start (wf_exec.erl:421-433)
- **CANCEL_SCOPE**: Enter/exit cancellation scope (wf_exec.erl:435-446)
- **TASK_EXEC**: Execute task (wf_exec.erl:448-451)
- **DONE**: Complete workflow (wf_exec.erl:453-456)

**Missing Handler:**
- **MI_SPAWN**: Spawn multiple instances - referenced in tests (wf_test_mi.erl) but no handler in wf_exec:execute_opcode/2

## Join Policies

From src/wf_vm.erl:24-29, implemented in wf_exec.erl:285-365:

- **all**: Wait for all M branches (wf_exec.erl:319-327)
- **{first_n, N}**: Wait for N branches (wf_exec.erl:329-337)
- **{n_of_m, N, M}**: Wait for N of M branches (wf_exec.erl:339-347)
- **first_complete**: Join on first completion (discriminator) (wf_exec.erl:349-356)
- **sync_merge**: Synchronizing merge (wf_exec.erl:358-365)

## Loop Policies

From src/wf_vm.erl:32-35, implemented in wf_exec.erl:396-433:

- **{count, N}**: Loop N times (wf_exec.erl:398-407)
- **while**: Loop while condition true (wf_exec.erl:409-413)
- **until**: Loop until condition true (wf_exec.erl:415-419)

## Multiple Instance Policies

From src/wf_vm.erl:38-40:

- **{fixed, N}**: Spawn N instances (test exists, no handler)
- **{dynamic, Min, Max}**: Spawn between Min and Max instances (test exists, no handler)

## Derived Pattern Examples

### Discriminator (Pattern 8)
```erlang
%% Derived from kernel primitives
Bytecode = [
    {'PAR_FORK', [1, 3, 5, 7]},  % Fork 3 branches
    {'TASK_EXEC', task_a},
    {'TASK_EXEC', task_b},
    {'TASK_EXEC', task_c},
    {'JOIN_WAIT', first_complete},  % Join on first completion
    {'DONE'}
]
```

### N-out-of-M Join (Pattern 9)
```erlang
%% Wait for 2 of 3 branches
Bytecode = [
    {'PAR_FORK', [1, 3, 5]},
    {'TASK_EXEC', task_a},
    {'TASK_EXEC', task_b},
    {'TASK_EXEC', task_c},
    {'JOIN_WAIT', {n_of_m, 2, 3}},
    {'DONE'}
]
```

## Known Limitations

### Cancellation (Patterns 11-13)
- Stubbed implementation in wf_exec.erl:512-529
- Functions `is_scope_cancelled/2` and `propagate_cancellation/2` return false/noop
- Awaiting item 008 for full implementation
- Tests exist but only verify bytecode structure, not runtime behavior

### Multiple Instances (Patterns 14-15)
- wf_test_mi.erl includes non-existent header: `-include("../src/wf_mi.hrl")` (line 11)
- MI_SPAWN opcode has no handler in wf_exec:execute_opcode/2
- Tests cannot run until item 009 (multiple-instance-support) is complete

### Scheduler
- wf_sched module does not exist (referenced in wf_trace.erl:24, wf_test_trace_helpers.erl:67)
- Scheduler policy parameter in wf_test_trace_helpers:run_with_trace/3 is ignored (line 69)
- Deterministic/nondeterministic/replay modes planned (PROMPT.md:140-143) but not implemented

### Effect System
- wf_effect module does not exist
- Effect boundary and receipt system pending item 010
- No way to execute external effects from tasks

## Test Coverage

Patterns with test coverage (from test/ directory):
- Sequence: wf_test_seq.erl (7 tests)
- Parallel: wf_test_par.erl (9 tests)
- XOR: wf_test_xor.erl (7 tests)
- Join: wf_test_join.erl (11 tests)
- Cancel: wf_test_cancel.erl (7 tests)
- MI: wf_test_mi.erl (6 tests - cannot run)
- Determinism: wf_test_determinism.erl (7 tests)

For detailed test methodology, see docs/TESTING.md.
