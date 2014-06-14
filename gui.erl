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
    particle_system:start_link(wx:get_env(), win:get_gl()),
    spline:new(),
    %% recorder:new(Wx),
    rec:new(),
    rec:record(4410),
    ?D_REGISTER(?SERVER, self()),

    tick(?IFPS),
    loop(),

    particle_system:stop(),
    rec:stop(),
    rec:destroy(),
    spline:destroy(),
    %% recorder:destroy(),
    wx:destroy(),
    ok.


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
	    ?D_F("got EXIT from ~p with reason: ~p~n", [Pid, Reason]);

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop()
    end.


draw() ->
    screen:draw().


tick(T) ->
    erlang:send_after(T, ?SERVER, draw).
