-module(erlang_benchmark_tester).

-behaviour(gen_server).

-export([start/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


cast(Req) ->
    gen_server:cast(tester, Req).

start(all, AllTests, Operations) ->
    gen_server:start({local, tester}, ?MODULE, [AllTests, Operations], []);

start(TestsList, AllTests, Operations) ->
    gen_server:start({local, tester}, ?MODULE, [TestsList, AllTests, Operations], []).
%==============================================================================
% gen_server
%==============================================================================
init([AllTests, Operations]) ->
    Tests = [Test||{_, Test} <- AllTests],
    cast(start),
    {ok, [{tests, Tests}, {tests_list, Tests}, {operations, Operations}]};

init([TestsList, AllTests, Operations]) ->
    Tests = get_test_list(TestsList, AllTests),
    init([Tests, Operations]).

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({test_res, {Test, Res}}, State) ->
    Operations = proplists:get_value(operations, State),
    Results = proplists:get_value({results, Test}, State, []),
    NewResults = [Res|Results],
    NewState0 = State -- [{{results, Test}, Results}],
    NewState = NewState0 ++ [{{results, Test}, NewResults}],
    case length(NewResults) < Operations of
        true ->
            {noreply, NewState};
        false ->
            io:format("tester:49 results: ~p~n",[NewResults]),
            cast(start),
            {noreply, NewState}
    end;

handle_cast(start, State) ->
    Operations = proplists:get_value(operations, State),
    Tuple = proplists:lookup(tests, State),
    {tests, Tests} = Tuple,
%    GaugeValue = Operations - length(Tests),
%    erlang_benchmark_windows:cast(set_gauge_value),
    NewState0 = State -- [Tuple],
    Tail = case Tests of
        [] -> 
            cast(make_result), 
            [];
        [Test|TestsTail] -> 
            run_test(Test, Operations), 
            TestsTail
    end,
    NewState = [{tests, Tail}|NewState0],
    {noreply, NewState};

handle_cast(make_result, State) ->
    Tests = proplists:get_value(tests_list, State),
    AverageResult = make_result(Tests, State),
    erlang_benchmark_windows:cast({show_result, AverageResult}),
    io:format("tester:72 state: ~p~n AverageResult: ~p~n",[State,AverageResult]),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("tester:76 Msg: ~p~n",[Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("~ntester 84~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%==============================================================================
get_test_list(TestsList, AllTests) ->
    get_test_list(TestsList, AllTests, []).

get_test_list([Test|TestsList], AllTests, Res) ->
    Tuple = proplists:lookup(Test, AllTests),
    get_test_list(TestsList, AllTests, [Tuple|Res]);

get_test_list([], _AllTests, Res) ->
    Res.

run_test({module, _Module}, 0) ->
    ok;

run_test({module, Module}, Operations) ->
    Module:start(),
    erlang_benchmark_windows:cast(set_gauge_value),
    run_test({module, Module}, Operations - 1);

run_test(Test, Operations) ->
    Module = list_to_atom("erlang_benchmark_"++Test),
    run_test({module, Module}, Operations).
%==============================================================================
make_result(Tests, State) ->
    make_result(Tests, State, []).

make_result([Test|Tests], State, Res) ->
    TestRes = proplists:get_value({results,list_to_atom(Test)}, State),
    Average = average_result(TestRes),
    make_result(Tests, State, [Average|Res]);

make_result([], _State, Res) ->
    NewRes = lists:flatten(Res),
    make_result_file(NewRes),
    NewRes.
%
average_result([Res|ListOfRes]) ->
    average_result(ListOfRes, Res).

average_result([Res|ListOfRes], Sum) ->
    Keys = proplists:get_keys(Res),
    Sum1 = sum_pl(Keys, Res, Sum),
    average_result(ListOfRes, Sum1);

average_result([], Sum) ->
    division(Sum). 
%
division(PL) ->
    Keys = proplists:get_keys(PL),
    division(Keys, PL, []).

division([Key|Keys], PL, Res) ->
    V = proplists:get_value(Key, PL),
    division(Keys, PL, [{Key, V/10}|Res]);

division([], _PL, Res) ->
    Res.
%
sum_pl([Key|Keys], Res, Sum) ->
    V1 = proplists:get_value(Key, Res),
    V2 = proplists:get_value(Key, Sum),
    Tuple = proplists:lookup(Key, Sum),
    Sum1 = Sum -- [Tuple],
    Sum2 = [{Key, V1+V2}|Sum1],
    sum_pl(Keys, Res, Sum2);

sum_pl([], _Res, Sum) ->
    Sum.

make_result_file(Res) ->
    {ok, Store} = application:get_env(erlang_benchmark, store_results),
    case Store of 
        false -> 
            ok;
        true ->
            DateTime = erlang:localtime(),
            {{Y,M,D},{H,Mi,S}} = DateTime,
            DateTimeString = lists:flatten(io_lib:format("_~p.~p.~p_~p:~p:~p", [Y,M,D,H,Mi,S])),
            {ok, FileAtom} = application:get_env(erlang_benchmark, result_file),
            {ok, DirAtom} = application:get_env(erlang_benchmark, result_dir),
            File = atom_to_list(FileAtom),
            Dir = atom_to_list(DirAtom),
            _ = file:make_dir(Dir),
            FileName = Dir++"/"++File++DateTimeString,
            {ok, F} = file:open(FileName, write),
            io:format(F, "~p~n", [{DateTime, Res}]),
            file:close(F)
    end.
