%%%-------------------------------------------------------------------
%%% File    : gui.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : GUI process
%%%-------------------------------------------------------------------
-module(gui).
-author('olivier@biniou.info').

-include("debug.hrl").
-include("gui.hrl").

-export([start/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
start() ->
    process_flag(trap_exit, true),
    Wx = wx:new(),
    win:new(Wx),
    spline:new(),
    %% recorder:new(Wx),
    rec:new(),
    rec:record(8000),
    ?D_REGISTER(?SERVER, self()),
    tick(?IFPS),
    loop(),
    rec:stop(),
    rec:destroy(),
    spline:destroy(),
    recorder:destroy(),
    wx:destroy().


%%====================================================================
%% Internal functions
%%====================================================================
loop() ->
    receive
	draw ->
	    draw(),
	    tick(?IFPS),
	    loop();

	{'EXIT', Pid, Reason} ->
	    ?D_F("got EXIT from ~p with reason: ~p", [Pid, Reason]);

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop()
    end.


draw() ->
    screen:draw().


tick(T) ->
    erlang:send_after(T, ?SERVER, draw).
