-module(erlang_benchmark).
-export([start/0]).

start() ->
    sync:go(),
%    application:start(erlang_benchmark).
    erlang_benchmark_app:start(1,2).
