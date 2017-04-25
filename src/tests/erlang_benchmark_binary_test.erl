-module(erlang_benchmark_binary_test).

-export([start/0, sum_bin/2]).

start() -> 
    List = lists:duplicate(10000000, 255),
    Bin = list_to_binary(List),
    {Time1, _} = timer:tc(?MODULE, sum_bin, [Bin, Bin]),
    {Time2, _} = timer:tc(?MODULE, sum_bin, [Bin, <<"1">>]),
    {Time3, _} = timer:tc(?MODULE, sum_bin, [<<"1">>, Bin]),
    Res =  {binary_test, [{{"binary_tests","sum_10000000_10000000"}, Time1},
                          {{"binary_tests","sum_10000000_1"}, Time2},
                          {{"binary_tests","sum_1_10000000"}, Time3}]},
    gen_server:cast(tester, {test_res, Res}).


sum_bin(A,B) ->
    <<A/binary, B/binary>>.
