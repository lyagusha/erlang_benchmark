-module(erlang_benchmark_windows).

-behaviour(gen_server).

-export([start/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         cast/1]).

-include_lib("wx/include/wx.hrl").

cast(Req) ->
    gen_server:cast(windows, Req).

start() ->
    _ = gen_server:start({local, windows}, ?MODULE, [], []).
%==============================================================================
% gen_server
%==============================================================================
init([]) ->
    {ok, Operations} = application:get_env(erlang_benchmark, tests_number),
%%Main form
% preparation
    {ok, Modules} = application:get_key(erlang_benchmark, modules),
    Tests = is_test(Modules),
    GetNames = (fun(Name) ->
        BaseName = filename:basename(Name, ".erl"), 
        BaseNameBin = list_to_binary(BaseName),
        <<"erlang_benchmark_", TestName/binary>> = BaseNameBin,
        binary_to_list(TestName)        
    end),
    TestNames = [GetNames(X)|| X <- Tests],
    TestNunbers = lists:seq(100, length(TestNames)+99),
    ZipList = lists:zip(TestNunbers, TestNames),
    
% wx
    Wx = wx:new(),
    MainForm = wxFrame:new(Wx, 0, "Erlang benchmark", [{size, {330,100+25*length(TestNunbers)}}]),   
    _B1 = wxButton:new(MainForm, 1, [{label,"Test all"}, {pos, {10,10}}, {size, {150,30}}]), 
    _B2 = wxButton:new(MainForm, 2, [{label,"Test selected"}, {pos, {170,10}}, {size, {150,30}}]), 
    Gauge = wxGauge:new(MainForm, 3, 100, [{size, {310, 30}}, {pos, {10,60+25*length(TestNunbers)}}, {style, ?wxGA_HORIZONTAL}]),
    CheckBoxes =
        [wxCheckBox:new(MainForm, N, Name, [{pos, {10,50+(N-100)*25}}])||{N, Name} <- ZipList],
    wxFrame:show(MainForm),
    ok = wxFrame:connect(MainForm, command_button_clicked),
    ok = wxFrame:connect(MainForm, close_window),
    Fun =
	    fun(Item) ->
    		wxCheckBox:connect(Item, command_checkbox_clicked)
    	end,
    wx:foreach(Fun, CheckBoxes),
    State = [{tests_list, []}, {all_tests, ZipList}, {gauge, Gauge}, {main_form, MainForm}, {operations, Operations}],
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(_, _From, State) ->
    {reply, undefined, State}.
%==============================================================================
handle_cast({show_result, Res}, State) ->
    NumbersOfString = length(proplists:get_keys(Res)),
    Wx = wx:new(),
    ResForm = wxFrame:new(Wx, 0, "Results", [{size, {510,40+NumbersOfString*25}}]),

%_Panel = wxPanel:new(ResForm, []),
A= application:get_env(erlang_benchmark, result_file),
io:format("~n 73 ~p",[A]),
    Grid = wxGrid:new(ResForm, 1, []),
    wxGrid:createGrid(Grid, NumbersOfString, 3),
    Strings = [{Group, Test, Time}||{{Group, Test}, Time} <- Res],
    Rows = (fun({Row, {Group, Test, Time}}) ->
        wxGrid:setCellAlignment(Grid, Row, 0, ?wxALIGN_CENTER, ?wxALIGN_CENTER),
        wxGrid:setCellAlignment(Grid, Row, 1, ?wxALIGN_CENTER, ?wxALIGN_CENTER),
        wxGrid:setCellAlignment(Grid, Row, 2, ?wxALIGN_CENTER, ?wxALIGN_CENTER),
        wxGrid:setCellValue(Grid, Row, 0, Group),
        wxGrid:setCellValue(Grid, Row, 1, Test),
        wxGrid:setCellValue(Grid, Row, 2, integer_to_list(round(Time)))
    end),
    RowsList = lists:zip(lists:seq(0,NumbersOfString-1), Strings),
    wx:foreach(Rows, RowsList),
    _ = wxGrid:setColSize(Grid, 0, 130),
    _ = wxGrid:setColSize(Grid, 1, 220),
    _ = wxGrid:setColSize(Grid, 2, 70),
    _ = wxGrid:setColLabelValue(Grid, 0, "Group"),
    _ = wxGrid:setColLabelValue(Grid, 1, "Test"),
    _ = wxGrid:setColLabelValue(Grid, 2, "Res"),
    wxFrame:show(ResForm),
    MainForm = proplists:get_value(main_form, State),
    wxFrame:hide(MainForm),
    wxFrame:connect(ResForm, close_window),
    {noreply, [{res_form, ResForm}|State]};
%==============================================================================
handle_cast(set_gauge_value, State) ->
    Gauge = proplists:get_value(gauge, State),
    V0 = wxGauge:getValue(Gauge),
    wxGauge:setValue(Gauge, V0+1),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({wx, Id, _, _, {_,_,_, Check, _}}, State) ->
    TestsList = proplists:get_value(tests_list, State),
    AllTests = proplists:get_value(all_tests, State),
    Operations = proplists:get_value(operations, State),
    Gauge = proplists:get_value(gauge, State),
    case {Id, Check} of
        {1, _} ->   
            L = length(AllTests),  
            wxGauge:setRange(Gauge,L*Operations), 
            erlang_benchmark_tester:start(all, AllTests, Operations);
        {2, _} ->  
            L = length(TestsList),  
            wxGauge:setRange(Gauge, L*Operations), 
            erlang_benchmark_tester:start(TestsList, AllTests, Operations);
        {CheckBox, 1} -> make_tests_list(CheckBox, 1, TestsList);
        {CheckBox, 0} -> make_tests_list(CheckBox, 0, TestsList)
    end,
    {noreply, State};

handle_info({set_state, {Key, Value}}, State) ->
    Tuple = proplists:lookup(Key, State),
    NewState0 = State -- [Tuple],    
    NewState = [{Key, Value}|NewState0],
    {noreply, NewState};

handle_info({wx, _, Object, _, {wxClose,close_window}}, State) ->
    wxFrame:hide(Object),
    init:stop(),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%==============================================================================
make_tests_list(CheckBox, 1, TestsList) ->
    NewTestsList = TestsList ++ [CheckBox],
    self()!{set_state, {tests_list, NewTestsList}};
make_tests_list(CheckBox, 0, TestsList) ->
    NewTestsList = TestsList -- [CheckBox],
    self()!{set_state, {tests_list, NewTestsList}}.
%==============================================================================
is_test(Modules) ->
    is_test(Modules, []).

is_test([Module|Modules], Res)->
    NewRes = case re:run(atom_to_list(Module), "test$", [global]) of
        nomatch -> Res;
        {match, _} -> [Module|Res]
        end,
    is_test(Modules, NewRes);

is_test([], Res) ->
    Res.









































