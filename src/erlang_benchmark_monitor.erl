-module(erlang_benchmark_monitor).

-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         cast/1]).

cast(Req) ->
    gen_server:cast(?MODULE, Req).

start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).

init([State]) ->
    RefreshTime = proplists:get_value(monitor_refresh_time, State, 500),
    cast({refresh_info, RefreshTime}),
    %io:format("~n ~nmonitor:24~n~nState: ~p~n~n", [State]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
   
handle_cast({refresh_info, Time}, State) ->
    Cpu = erlang:round(cpu_sup:util()),
    UsageMem = erlang:round(erlang:memory(total)/(1024*1024)),
    ok = erlang_benchmark_windows:cast({set_mem_gauge_value, UsageMem}),
    ok = erlang_benchmark_windows:cast({set_cpu_gauge_value, Cpu}),
    Tref0 = proplists:get_value(mem_timer, State),
    Tuple = {mem_timer, Tref0},
    State0 = State -- [Tuple],
    _ = timer:cancel(Tref0),
    {ok, Tref} = timer:apply_after(Time, ?MODULE, cast, [{refresh_info, Time}]),
    {noreply, [{mem_timer, Tref}|State0]};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
