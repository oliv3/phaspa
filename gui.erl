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

%% module API
%% -export([get_env/0, register/1, unregister/1]).
%% -export([register_texture/2, unregister_texture/2, unregister_textures/1]).

%% wx_object callbacks
-export([init/1]).

%% proc_lib callbacks
%%-export([system_code_change/4, system_continue/3]).

-define(SERVER, ?MODULE).

-define(ZMAX, 5).

-record(state,  {ifps, frame, gl, size}).

%%====================================================================
%% API
%%====================================================================
start() ->
    start_link().

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).


%% get_env() ->
%%     Ref = make_ref(),
%%     ?SERVER ! {self(), Ref, get_env},
%%     receive
%% 	{Ref, Env} ->
%% 	    Env
%%     end.


%% register(Pid) ->
%%     ?SERVER ! {register, Pid}.


%% unregister(Pid) ->
%%     ?SERVER ! {unregister, Pid}.


%% register_texture(Pid, Tid) ->
%%     ?SERVER ! {register_texture, Pid, Tid}.


%% unregister_texture(Pid, Tid) ->
%%     ?SERVER ! {unregister_texture, Pid, Tid}.


%% unregister_textures(Pid) ->
%%     ?SERVER ! {unregister_textures, Pid}.


%%====================================================================
%% wx_object Callbacks
%%====================================================================
init(Parent) ->
    %% process_flag(trap_exit, true),
    Size = {640, 480},%%ec:get_env(size),
    Wx = wx:new(),
    Frame = win:new(Wx, Size),
    %%?D_F("*** Frame= ~p", [Frame]),
    GL = win:gl(),
    %%?D_F("*** GL= ~p", [GL]),
    ?D_REGISTER(?SERVER, self()),
    Debug = sys:debug_options([]),
    %% IFPS = trunc(1000 / ec:get_env(fps)),
    proc_lib:init_ack(Parent, {ok, self()}),
    tick(?IFPS),
    loop(Parent, Debug, #state{ifps=?IFPS, frame=Frame, gl=GL, size=Size}).


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
    %%?D_F("*** LOOP", []),
    receive
	%% {Pid, Ref, get_env} ->
	%%     Pid ! {Ref, wx:get_env()},
	%%     loop(Parent, Debug, State);

	%% {register, Pid} ->
	%%     link(Pid),
        %%     Plugins = [Pid|State#state3.plugins],
	%%     loop(Parent, Debug, State#state3{plugins=Plugins});

	%% {unregister, Pid} ->
	%%     unlink(Pid),
	%%     {ok, NewState} = unregister(Pid, State),
	%%     loop(Parent, Debug, NewState);

	%% {register_texture, Pid, Tid} ->
	%%     ets:insert(Texs, {Pid, Tid}),
	%%     loop(Parent, Debug, State);

	%% {unregister_texture, Pid, Tid} ->
	%%     ets:delete_object(Texs, {Pid, Tid}),
	%%     wxGLCanvas:setCurrent(GL),
	%%     gl:deleteTextures([Tid]),
	%%     loop(Parent, Debug, State);

	%% {unregister_textures, Pid} ->
	%%     Records = ets:lookup(Texs, Pid),
	%%     ets:delete(Texs, Pid),
	%%     Tids = [Tid || {_Pid, Tid} <- Records],
	%%     ?D_F("freeing textures owned by pid ~p: ~p", [Pid, Tids]),
	%%     wxGLCanvas:setCurrent(GL),
	%%     gl:deleteTextures(Tids),
	%%     loop(Parent, Debug, State);

	draw ->
	    %%?D_F("+++ draw~n", []),
	    draw(State),
	    %%?D_F("+++ tick~n", []),
	    tick(IFPS),
	    %%?D_F("+++ loop~n", []),
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


draw(#state{gl=GL, size=Size} = _State) ->
    %%?D_F("*** draw(~p)~n", [_State]),
    wxGLCanvas:setCurrent(GL),
    set_view(Size),
    wirecube:draw(),
    %%NewState = draw2(State),
    wxGLCanvas:swapBuffers(GL).
    %%NewState.


%% draw2(State) ->
%%     draw2(State, State#state.plugins, []).
%% draw2(State, [], Acc) ->
%%     State#state{plugins=lists:reverse(Acc)};
%% draw2(State, [Pid|Pids], Acc) ->
%%     Ref = make_ref(),
%%     Pid ! {self(), Ref, {draw, State#state.gl}},
%%     receive
%%         {Ref, _Result} ->
%%             draw2(State, Pids, [Pid|Acc]);

%%         {'EXIT', Pid, _Reason} ->
%%             ?D_F("plugin ~p crashed, reason: ~p", [Pid, _Reason]),
%%             {ok, NewState} = unregister(Pid, State),
%%             draw2(NewState, Pids, Acc)
%%     end.


%% TODO: bouger dans le screen
set_view({Width, Height}) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:clearColor(0.0, 0.0, 0.0, 1.0),
    gl:clearDepth(?ZMAX),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    Ratio = Width / Height,

    %% FIXME TODO: dans la conf du screen
    glu:perspective(?DEFAULT_FOV, Ratio, 0.1, ?ZMAX),
    glu:lookAt(0, 0, 3.14,
	       0, 0, -3.14,
	       0, 1, 0),

    %% FIXME
    {RotX, RotY, RotZ} = ?DEFAULT_ROT, %%ec_cf:rot(),
    gl:rotatef(RotX, 1.0, 0.0, 0.0),
    gl:rotatef(RotY, 0.0, 1.0, 0.0),
    gl:rotatef(RotZ, 0.0, 0.0, 1.0),

    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).


tick(T) ->
    erlang:send_after(T, ?SERVER, draw).


%% unregister(Pid, #state{plugins=Plugins} = State) ->
%%     case lists:member(Pid, Plugins) of
%%         true ->
%%             NPlugins = Plugins -- [Pid],
%%             unregister_textures(Pid),
%% 	    {ok, State#state3{plugins=NPlugins}};

%%         false ->
%% 	    {false, State}
%%     end.


%% ets() ->
%%     ets:new(?SERVER, [duplicate_bag, private]).
