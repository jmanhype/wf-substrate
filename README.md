# wf-substrate

Erlang/OTP library that compiles workflow patterns into bytecode for execution. Implements 43 control-flow workflow patterns (from the van der Aalst taxonomy) using 9 kernel primitives.

## Numbers

| Metric | Value |
|--------|-------|
| Source modules | 25 |
| Test files | 33 (active) + 33 (disabled) |
| Kernel primitives | 9 (seq, par, choice, loop, cancel, mi, case, effect, noop) |
| Workflow patterns covered | 43 |
| OTP version required | >= 26 |
| Dependencies | 0 (stdlib/kernel/sasl only; proper for tests) |

## What it does

1. **Compile**: Workflow pattern terms (Erlang data structures) compile to a flat bytecode instruction list
2. **Execute**: A virtual machine (`wf_vm`) runs bytecode with explicit state: instruction pointer, branch map, join counters
3. **Cancel**: Structured cancellation at 3 scope levels (activity, case, region)
4. **Trace**: Every reduction step emits structured trace events for replay and debugging
5. **Validate**: Petri-net-based validation checks workflow definitions for structural correctness

## Building

```bash
rebar3 compile
rebar3 eunit
```

## Module layout

```
src/
  wf_compile.erl      - Pattern term to bytecode compiler
  wf_vm.erl           - Bytecode interpreter / executor
  wf_exec.erl         - Execution engine (gen_statem per case)
  wf_state.erl        - Explicit execution state with buffer/commit/rollback
  wf_cancel.erl       - Structured cancellation semantics
  wf_trace.erl        - Structured trace event emission
  wf_validate.erl     - Petri-net structural validation
  wf_sched.erl        - Scheduler (deterministic, nondeterministic, replay)
  wf_effect.erl       - Effect boundary for external I/O
  wf_governance.erl   - Budget, approval, audit controls
  wf_substrate_app.erl - OTP application entry point
```

Detailed architecture: `docs/ARCHITECTURE.md`. Operational semantics: `docs/SEMANTICS.md`.

## Status

Test suite has known issues. 33 test files are disabled (in `test/disabled/`). The active test files compile and run but some have failures related to cancellation timing and trace sink initialization. Several debug scripts (`test_trace*.erl`, `debug_par.erl`) sit in the repo root -- cleanup pending.

## License

Apache-2.0