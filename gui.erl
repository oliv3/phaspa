%%%-------------------------------------------------------------------
%%% File    : gui.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : GUI process
%%%-------------------------------------------------------------------
-module(gui).
-author('olivier@biniou.info').

%% -include("ec.hrl").
-include("debug.hrl").
-include("gui.hrl").

-export([start/0]).

-define(SERVER, ?MODULE).

-record(state,  {ifps=?IFPS}).

%%====================================================================
%% API
%%====================================================================
start() ->
    raw_file:start_link(),
    raw_file:test(),
    init(),
    tick(?IFPS),
    loop(#state{}).

init() ->
    %% process_flag(trap_exit, true),
    Wx = wx:new(),
    win:new(Wx),
    ?D_REGISTER(?SERVER, self()).


%%====================================================================
%% Internal functions
%%====================================================================
loop(#state{ifps=IFPS} = State) ->
    receive
	draw ->
	    draw(),
	    tick(IFPS),
	    loop(State);

	{'EXIT', Pid, Reason} ->
	    ?D_F("got EXIT from ~p with reason: ~p", [Pid, Reason]);
	    %% ?D_F("got EXIT from ~p with reason: ~p", [Pid, Reason]),
            %% case unregister(Pid, State) of
	    %% 	{ok, NewState} ->
	    %% 	    loop(Parent, Debug, NewState);

	    %% 	{false, State} ->
            %%         wxFrame:destroy(State#state.frame),
            %%         wx:destroy(),
            %%         ?D_F("wxWidgets destroyed", []),
            %%         exit(Reason)
            %% end;

	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(State)
    end.


draw() ->
    screen:draw().


tick(T) ->
    erlang:send_after(T, ?SERVER, draw).
