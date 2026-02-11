%%%-------------------------------------------------------------------
%%% @doc wf_validate record definitions
%%% This header file exports validation_state, issue, and report
%%% records for use in tests and other modules.
%%%-------------------------------------------------------------------

-record(validation_state, {
    bytecode :: wf_vm:wf_bc(),
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer()
}).

-record(issue, {
    type :: dead_transition | livelock | improper_completion | deadlock | unsound,
    state :: #validation_state{},
    message :: binary(),
    path_from_initial :: [wf_sched:enabled_action()]
}).

-record(report, {
    explored_state_count :: non_neg_integer(),
    unique_state_count :: non_neg_integer(),
    max_depth_reached :: non_neg_integer(),
    checks_passed :: #{atom() => boolean()},
    issues_found :: [#issue{}]
}).
