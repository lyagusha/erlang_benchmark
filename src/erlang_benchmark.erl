-module(erlang_benchmark).
-export([start/0]).

start() ->
    sync:go(),
    ok = application:start(sasl),
    ok = application:start(os_mon),
    ok = application:start(erlang_benchmark).
