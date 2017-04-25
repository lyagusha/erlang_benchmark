-module(erlang_benchmark_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    _ = erlang_benchmark_windows:start(),
%    erlang_benchmark_handler:start(WindowsPid),
	erlang_benchmark_sup:start_link().

stop(_State) ->
	ok.
