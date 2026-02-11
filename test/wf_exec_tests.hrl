%%%-------------------------------------------------------------------
%%% @doc Mock bytecode generators for testing
%%% Exported as header file for reuse across test modules
%%%-------------------------------------------------------------------

-define(MOCK_BYTECODE_SIMPLE_TASK, [
    {'TASK_EXEC', mock_task},
    {'DONE'}
]).

-define(MOCK_BYTECODE_SEQ, [
    {'SEQ_ENTER', 0},
    {'TASK_EXEC', task_a},
    {'SEQ_NEXT', 3},
    {'TASK_EXEC', task_b},
    {'DONE'}
]).

-define(MOCK_BYTECODE_PAR, [
    {'PAR_FORK', [1, 3]},
    {'TASK_EXEC', task_a},
    {'DONE'},
    {'TASK_EXEC', task_b},
    {'DONE'},
    {'JOIN_WAIT', all}
]).
