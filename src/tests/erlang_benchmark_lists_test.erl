-module(erlang_benchmark_lists_test).

-export([start/0, sum_list/2]).

start() -> 
    List = lists:seq(1, 1000000),
    List1 = lists:seq(1, 200000),    
    {Time1, _} = timer:tc(?MODULE, sum_list, [List, List]),
    {Time5, _} = timer:tc(?MODULE, sum_list, [List1, List1]),
    {Time2, _} = timer:tc(?MODULE, sum_list, [List, [1]]),
    {Time4, _} = timer:tc(?MODULE, sum_list, [List1, [1]]),
    {Time3, _} = timer:tc(?MODULE, sum_list, [[1], List]),
    Res = {lists_test, [{{"lists_test","sum_1000000_1000000"}, Time1},
                        {{"lists_test","sum_200000_200000"}, Time5},
                        {{"lists_test","sum_1000000_1"}, Time2},
                        {{"lists_test","sum_200000_1"}, Time4},
                        {{"lists_test","sum_1_1000000"}, Time3}]},
    gen_server:cast(tester, {test_res, Res}).


sum_list(A, B) ->
    A++B.
