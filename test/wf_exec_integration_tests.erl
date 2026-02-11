-module(wf_exec_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("wf_exec.hrl").
-include("wf_effect.hrl").
-include("wf_receipt.hrl").

%%====================================================================
%% Integration Tests: Effect Yielding and Blocking
%%====================================================================

%% Test that executor can handle task yielding effect
task_yields_effect_test_() ->
    {setup,
     fun() ->
         %% Start gen_servers
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         %% Create bytecode with task that yields effect
         TaskFun = fun(Ctx) ->
             EffectSpec = wf_effect:new_spec(
                 make_ref(), 0, root, mock_effect, #{}
             ),
             {effect, EffectSpec, Ctx}
         end,

         %% Create mock bytecode
         Bytecode = [
             {task_exec, task},
             {done}
         ],

         %% Create executor with custom task registry
         ExecState0 = wf_exec:new(Bytecode),
         ExecState1 = ExecState0#exec_state{
             ctx = #{task_function => TaskFun}
         },

         %% Override lookup_task_function for testing
         meck:new(wf_exec, [passthrough]),
         meck:expect(wf_exec, lookup_task_function, 2,
             fun(_TaskName, _ExecState) ->
                 fun(Ctx) -> (maps:get(task_function, Ctx))(Ctx) end
             end),

         try
             %% Execute first step (task that yields effect)
             {ExecState2, _Trace1} = wf_exec:step(ExecState1, #{}),

             %% Verify executor is blocked on effect
             ?assertEqual(blocked_effect, ExecState2#exec_state.status),

             %% Wait for effect to complete
             timer:sleep(50),

             %% Execute second step (should resume)
             {ExecState3, Trace3} = wf_exec:step(ExecState2, #{}),

             %% Verify executor resumed
             ?assertEqual(running, ExecState3#exec_state.status),
             ?assertEqual(effect_complete, maps:get(type, Trace3))
         after
             meck:unload(wf_exec)
         end
     end}.

%% Test idempotent effect resumes immediately
idempotent_effect_resumes_immediately_test_() ->
    {setup,
     fun() ->
         {ok, EffectPid} = wf_effect:start_link(),
         {ok, ReceiptPid} = wf_receipt:start_link(),
         {EffectPid, ReceiptPid}
     end,
     fun({EffectPid, ReceiptPid}) ->
         gen_server:stop(ReceiptPid),
         gen_server:stop(EffectPid)
     end,
     fun({_EffectPid, _ReceiptPid}) ->
         CaseId = make_ref(),
         Key = make_ref(),

         %% Create first task that yields effect with idempotency key
         TaskFun1 = fun(Ctx) ->
             EffectSpec = wf_effect:new_spec(CaseId, 0, root, mock_effect, #{}),
             EffectSpecWithKey = wf_effect:set_idempotency_key(EffectSpec, Key),
             {effect, EffectSpecWithKey, Ctx}
         end,

         %% Create bytecode
         Bytecode = [
             {task_exec, task},
             {done}
         ],

         %% Create executor
         ExecState0 = wf_exec:new(Bytecode),
         ExecState1 = ExecState0#exec_state{
             ctx = #{task_function => TaskFun1, case_id => CaseId}
         },

         meck:new(wf_exec, [passthrough]),
         meck:expect(wf_exec, lookup_task_function, 2,
             fun(_TaskName, _ExecState) ->
                 fun(Ctx) -> (maps:get(task_function, Ctx))(Ctx) end
             end),

         try
             %% Execute first task (yields effect)
             {ExecState2, _Trace1} = wf_exec:step(ExecState1, #{}),
             timer:sleep(50),  %% Wait for effect to complete
             {ExecState3, _Trace2} = wf_exec:step(ExecState2, #{}),

             %% Create second task with same idempotency key
             TaskFun2 = fun(Ctx) ->
                 EffectSpec = wf_effect:new_spec(CaseId, 1, root, mock_effect, #{}),
                 EffectSpecWithKey = wf_effect:set_idempotency_key(EffectSpec, Key),
                 {effect, EffectSpecWithKey, Ctx}
             end,

             ExecState4 = ExecState3#exec_state{
                 ctx = #{task_function => TaskFun2, case_id => CaseId},
                 ip = 0
             },

             %% Execute second task (should resume immediately with cached result)
             {ExecState5, _Trace3} = wf_exec:step(ExecState4, #{}),

             %% Verify executor didn't block (cached result)
             ?_assertEqual(running, ExecState5#exec_state.status)
         after
             meck:unload(wf_exec)
         end
     end}.
