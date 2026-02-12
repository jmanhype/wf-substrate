%%%-------------------------------------------------------------------
%%% @doc wf_exec record definitions
%%% This header file exports exec_state, token, branch_info, and
%%% join_counter records for use in tests and other modules.
%%%-------------------------------------------------------------------

-record(token, {
    token_id :: term(),
    ip :: non_neg_integer(),
    scope_id :: term(),
    value :: term(),
    status :: active | complete | cancelled | blocked_effect | blocked_approval,
    instance_id :: term() | undefined,  %% MI instance ID (undefined for non-MI tokens)
    current_effect :: {term(), non_neg_integer(), term()} | undefined  %% Current pending effect
}).

-record(branch_info, {
    branch_id :: term(),
    tokens :: [term()],
    join_id :: term(),
    targets :: [non_neg_integer()]
}).

-record(join_counter, {
    join_id :: term(),
    completed :: non_neg_integer(),
    required :: non_neg_integer(),
    policy :: wf_vm:join_policy(),
    results :: [term()]
}).

-record(exec_state, {
    ip :: non_neg_integer(),
    bytecode :: [wf_vm:opcode()],  %% Opcodes only (metadata stored separately)
    ctx :: map(),
    case_id :: term() | undefined,  %% Case ID for effect ID generation
    tokens :: #{term() => #token{}},
    branch_map :: #{term() => #branch_info{}},
    join_counters :: #{term() => #join_counter{}},
    scope_stack :: [term()],
    step_count :: non_neg_integer(),
    status :: running | done | blocked | blocked_effect | cancelled | failed,
    current_token :: term() | undefined,
    task_metadata :: wf_vm:task_metadata_map(),  %% Task metadata for function lookup
    loop_counters :: #{term() => non_neg_integer()}  %% Per-scope loop iteration counters
}).
