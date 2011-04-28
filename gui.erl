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

%% wx_object API
-export([start_link/0]).

%% wx_object callbacks
-export([init/1]).

%% proc_lib callbacks
%%-export([system_code_change/4, system_continue/3]).

-define(SERVER, ?MODULE).

-record(state,  {ifps, gl, size}).

%%====================================================================
%% API
%%====================================================================
start() ->
    start_link().

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).


%%====================================================================
%% wx_object Callbacks
%%====================================================================
init(Parent) ->
    %% process_flag(trap_exit, true),
    Size = {640, 480},%%ec:get_env(size),
    Wx = wx:new(),
    win:new(Wx, Size),
    %%?D_F("*** Frame= ~p", [Frame]),
    GL = win:gl(),
    %%?D_F("*** GL= ~p", [GL]),
    ?D_REGISTER(?SERVER, self()),
    Debug = sys:debug_options([]),
    %% IFPS = trunc(1000 / ec:get_env(fps)),
    proc_lib:init_ack(Parent, {ok, self()}),
    tick(?IFPS),
    loop(Parent, Debug, #state{ifps=?IFPS, gl=GL, size=Size}).


%%====================================================================
%% proc_lib callbacks
%%====================================================================
%% system_code_change(State, _Module, _OldVsn, _Extra) ->
%%     {ok, State}.

%% system_continue(Parent, Debug, State) ->
%%     %% ?D_F("system_continue(~p, ~p, ~p)", [Parent, Debug, State]),
%%     loop(Parent, Debug, State).


%%====================================================================
%% Internal functions
%%====================================================================
loop(Parent, Debug, #state{ifps=IFPS} = State) ->
    receive
	draw ->
	    draw(),
	    tick(IFPS),
	    loop(Parent, Debug, State);

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

        {system, From, Request} ->
	    ?D_F("system message: From ~p Request: ~p", [From, Request]),
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
	
	_Other ->
	    ?D_UNHANDLED(_Other),
	    loop(Parent, Debug, State)
    end.


draw() ->
    screen:draw().


tick(T) ->
    erlang:send_after(T, ?SERVER, draw).
