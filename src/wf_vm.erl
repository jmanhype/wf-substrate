-module(wf_vm).

%% Bytecode type definitions for workflow executor

%%====================================================================
%% Types
%%====================================================================

%% Bytecode is a list of opcodes
-type wf_bc() :: [opcode()].

%% Opcodes are tagged tuples
-type opcode() ::
    {atom(), non_neg_integer()} |             %% Generic opcode with integer arg (SEQ_ENTER, SEQ_NEXT, LOOP_BACK)
    {atom(), [non_neg_integer()]} |           %% Generic opcode with list arg (PAR_FORK, XOR_CHOOSE)
    {atom(), join_policy()} |                 %% JOIN_WAIT with policy
    {atom(), loop_policy()} |                 %% LOOP_CHECK with policy
    {atom(), mi_policy()} |                   %% MI_SPAWN with policy
    {atom(), {enter | exit, term()}} |        %% CANCEL_SCOPE with enter/exit
    {atom(), atom()} |                        %% TASK_EXEC with task name
    {atom()}.                                 %% DONE (no args)

%% Join policies
-type join_policy() ::
    all |
    {first_n, pos_integer()} |
    {n_of_m, pos_integer(), pos_integer()} |  %% {N, M}
    first_complete |
    sync_merge.

%% Loop policies
-type loop_policy() ::
    {count, non_neg_integer()} |
    while |
    until.

%% Multiple instance policies
-type mi_policy() ::
    {fixed, pos_integer()} |
    {dynamic, pos_integer(), pos_integer()}.  %% {min, max}

%% Export types
-export_type([
    wf_bc/0,
    opcode/0,
    join_policy/0,
    loop_policy/0,
    mi_policy/0
]).
