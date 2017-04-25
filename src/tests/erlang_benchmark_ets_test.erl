-module(erlang_benchmark_ets_test).

-export([start/0, ets_insert/2, ets_lookup/2]).

start() -> 
    Tid = ets:new(table, []),
    Keys = lists:seq(1, 100000),
    {Time1, _} = timer:tc(?MODULE, ets_insert, [Keys, Tid]), 
    {Time2, _} = timer:tc(?MODULE, ets_lookup, [Keys, Tid]), 
    Res = {ets_test, [{{"ets_test","insert 100000"}, Time1},
     {{"ets_test","lookup 100000"}, Time2}]},
    gen_server:cast(tester, {test_res, Res}).
%
ets_insert([Key|Keys], Tid) ->
    ets:insert(Tid, {Key, <<"123456789">>, 25, [1,2,3]}),
    ets_insert(Keys, Tid);
ets_insert([], _Tid) -> 
    ok.
%
ets_lookup([Key|Keys], Tid) ->
    _ = ets:lookup(Tid, Key),
    ets_lookup(Keys, Tid);
ets_lookup([], _Tid) -> 
    ok.
