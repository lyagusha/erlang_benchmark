-module(erlang_benchmark_handler).

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start(Pid) ->
    gen_server:start(?MODULE, [Pid], []).

init([Pid]) ->
    io:format("~nhandler init: ~p", [1]),
    WindowsState = gen_server:call(Pid, get_state),
    {MainForm, CheckBoxes} = proplists:get_value(main_form, WindowsState),
    ok = wxFrame:connect(MainForm, command_button_clicked),
    Fun =
	    fun(Item) ->
    		wxCheckBox:connect(Item, command_checkbox_clicked)
    	end,
    wx:foreach(Fun, CheckBoxes),
    listen([]),
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    io:format("~nhandler call: ~p", [Msg]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    io:format("~nhandler cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("~nhandler info: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


listen(TestsList) ->
    receive
        {wx,1,_,_,_} -> 
           % erlang_benchmark_tests:start(all),
            io:format("~nhandler listen: ~p", [1]),
            listen(TestsList);
        {wx,2,_,_,_} -> 
           % erlang_benchmark_tests:start(TestsList),
            io:format("~nhandler listen: ~p", [2]),
            listen(TestsList);
        {wx,CheckBox,_,_,{_,_,_,0,_}} -> 
            io:format("~nhandler listen: ~p", [3]),
            NewTestsList = make_tests_list(CheckBox, 0, TestsList),
            listen(NewTestsList);
        {wx,CheckBox,_,_,{_,_,_,1,_}} -> 
            io:format("~nhandler listen: ~p", [4]),
            NewTestsList = make_tests_list(CheckBox, 1, TestsList),
            listen(NewTestsList);
        Msg -> 
            io:format("~n unknown event ~p ~n",[Msg]),
            listen(TestsList)
    end.

make_tests_list(CheckBox, 1, TestsList) ->
    TestsList ++ [CheckBox - 100];
make_tests_list(CheckBox, 0, TestsList) ->
    TestsList -- [CheckBox - 100].


