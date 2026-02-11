%%%-------------------------------------------------------------------
%%% @doc wf_mi record definitions
%%% This header file exports MI instance and config records for use
%%% in other modules (tests, wf_exec, etc.)
%%%-------------------------------------------------------------------

%% Instance metadata
-record(mi_instance, {
    instance_id :: term(),                      %% Unique instance identifier
    token_id :: term(),                         %% Associated token ID
    instance_num :: pos_integer(),              %% Sequential instance number (1..N)
    status :: active | complete | cancelled,    %% Instance status
    result :: term() | undefined                %% Instance result (undefined until complete)
}).

%% MI configuration
-record(mi_config, {
    policy :: wf_vm:mi_policy(),                %% {fixed, N} | {dynamic, Min, Max}
    join_policy :: wf_mi:mi_join_policy(),      %% wait_all | {wait_n, N} | first_complete | none
    body_ip :: non_neg_integer(),               %% Bytecode IP of body to execute
    scope_id :: term(),                         %% Scope ID for instances
    instance_ids :: [term()]                    %% List of instance IDs
}).
