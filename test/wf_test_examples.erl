-module(wf_test_examples).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Example Basic Tests
%%====================================================================

example_basic_runs_test() ->
    ?assertMatch({ok, executed}, wf_example_basic:run()).

example_basic_bytecode_valid_test() ->
    Bytecode = wf_example_basic:term(),
    ?assert(is_list(Bytecode)),
    ?assert(length(Bytecode) > 0).

%%====================================================================
%% Example Discriminator Tests
%%====================================================================

example_discriminator_runs_test() ->
    ?assertMatch({ok, executed}, wf_example_discriminator:run()).

example_discriminator_bytecode_valid_test() ->
    Bytecode = wf_example_discriminator:term(),
    ?assert(is_list(Bytecode)),
    ?assert(length(Bytecode) > 0).

%%====================================================================
%% Example Cancel Region Tests
%%====================================================================

example_cancel_region_runs_test() ->
    ?assertMatch({ok, executed}, wf_example_cancel_region:run()).

example_cancel_region_bytecode_valid_test() ->
    Bytecode = wf_example_cancel_region:term(),
    ?assert(is_list(Bytecode)),
    ?assert(length(Bytecode) > 0).

%%====================================================================
%% Example Multiple Instances Tests
%%====================================================================

example_multiple_instances_runs_test() ->
    ?assertMatch({ok, executed}, wf_example_multiple_instances:run()).

example_multiple_instances_bytecode_valid_test() ->
    Bytecode = wf_example_multiple_instances:term(),
    ?assert(is_list(Bytecode)),
    ?assert(length(Bytecode) > 0).

