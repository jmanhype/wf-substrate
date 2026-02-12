%%%-------------------------------------------------------------------
%%% @doc Acceptance tests for end-to-end workflow execution
%%%
%%% These tests verify that workflows compile and execute correctly with
%%% real task functions (not mocks). They serve as integration tests
%%% that validate the complete compilation and execution pipeline.
%%% @end
-module(wf_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test that real task functions are called during execution
%%
%% This test verifies:
%% 1. Compiler preserves task metadata through compilation
%% 2. Executor looks up task functions from metadata map
%% 3. Real functions execute (not the mock)
%% 4. Context updates from task functions propagate correctly
%%--------------------------------------------------------------------
task_function_dispatch_test_() ->
    {"Tasks execute their real functions from metadata",
     fun() ->
         %% Build workflow with two tasks having distinct functions
         %% Each function sets a unique key in the context
         Term = wf_term:seq(
             wf_term:task(a, #{
                 function => fun(Ctx) ->
                     {ok, maps:put(a_ran, true, Ctx)}
                 end
             }),
             wf_term:task(b, #{
                 function => fun(Ctx) ->
                     {ok, maps:put(b_ran, true, Ctx)}
                 end
             })
         ),

         %% Compile to bytecode (returns {ok, {Bytecode, Metadata}})
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),

         %% Verify metadata was collected
         ?assertEqual(2, map_size(Metadata)),
         ?assert(maps:is_key(a, Metadata)),
         ?assert(maps:is_key(b, Metadata)),

         %% Execute workflow
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         {done, ExecState1} = wf_exec:run(ExecState0, 100, deterministic),

         %% Verify both tasks ran (context has both keys)
         FinalCtx = wf_exec:get_ctx(ExecState1),
         ?assertEqual(true, maps:get(a_ran, FinalCtx)),
         ?assertEqual(true, maps:get(b_ran, FinalCtx)),
         ?assertEqual(2, map_size(FinalCtx))
     end}.

%%--------------------------------------------------------------------
%% @doc Test that task functions receive and return context correctly
%%
%% This test verifies context propagation through task execution.
%%--------------------------------------------------------------------
context_propagation_test_() ->
    {"Task functions receive and update context correctly",
     fun() ->
         %% Build workflow with task that reads and writes context
         Term = wf_term:task(context_test, #{
             function => fun(Ctx) ->
                 InitialValue = maps:get(initial, Ctx, 0),
                 {ok, maps:put(result, InitialValue * 2, Ctx)}
             end
         }),

         %% Compile and execute
         {ok, {Bytecode, Metadata}} = wf_compile:compile(Term),
         ExecState0 = wf_exec:new({Bytecode, Metadata}),
         ExecState1 = wf_exec:set_ctx(ExecState0, #{initial => 5}),
         {done, ExecState2} = wf_exec:run(ExecState1, 100, deterministic),

         %% Verify context was read and updated
         FinalCtx = wf_exec:get_ctx(ExecState2),
         ?assertEqual(10, maps:get(result, FinalCtx))
     end}.
