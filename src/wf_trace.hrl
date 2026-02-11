%%%-------------------------------------------------------------------
%%% @doc wf_trace record definitions
%%% This header file exports trace_event, replay_entry, and
%%% trace_state records for use in tests and other modules.
%%%-------------------------------------------------------------------

-record(trace_event, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode(),
    state_before :: binary() | undefined,
    state_after :: binary() | undefined,
    timestamp :: erlang:monotonic_time(),
    scope :: [term()],
    branch_id :: term() | undefined,
    metadata :: map()
}).

-record(replay_entry, {
    step_seq :: non_neg_integer(),
    opcode :: wf_vm:opcode() | undefined,
    scheduler_choice :: wf_sched:choice_entry() | undefined,
    effect_result :: {term(), term()} | undefined
}).

-record(trace_state, {
    level :: wf_trace:trace_level(),
    sink :: wf_trace:trace_sink(),
    case_id :: term()
}).
