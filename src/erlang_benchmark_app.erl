-module(erlang_benchmark_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(EB, erlang_benchmark).

start(_Type, _Args) ->
    RefreshTime = application:get_env(?EB, monitor_refresh_time, 500),
    Store = application:get_env(?EB, store_results, true),
    StoreFile = application:get_env(?EB, result_file, result),
    StoreDir = application:get_env(?EB, result_dir, results),
    Button = application:get_env(?EB, default_button, test_all),
    TestNumber = application:get_env(?EB, tests_number, 30),
    State = [
             {monitor_refresh_time, RefreshTime},
             {store_results, Store},
             {result_file, StoreFile},
             {result_dir, StoreDir},
             {default_button, Button},
             {tests_number, TestNumber}
            ],
    _ = erlang_benchmark_windows:start_link(State),
	_ = erlang_benchmark_sup:start_link().

stop(_State) ->
	ok.
