%%%-------------------------------------------------------------------
%%% File    : screen.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : OpenGL widget
%%%-------------------------------------------------------------------
-module(screen).
-author('olivier@biniou.info').

-include("gui.hrl").
-include("debug.hrl").

-behaviour(wx_object).

%% API
-export([draw/0]).

%% wx_object API
-export([new/2]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_event/2, terminate/2]).

-define(SERVER,  ?MODULE).
-define(SIZE,    1.5).
-define(SPAN,    5).


%% GL widget state
-record(state, {
	  size,
	  rot = ?DEFAULT_ROT,
	  fov = ?DEFAULT_FOV,

	  frame, gl, mouse,

	  %% scaling
	  scale = 1.5,

	  %% drawing mode
	  mode = ?GL_LINE_STRIP,
	  color = true,
	  spline = true
	 }).

-define(ZMAX, 1000.0).
-define(SCALE_STEP, 0.2).

-define(MONO_C, {1.0, 1.0, 1.0}).


draw() ->
    wx_object:call(?SERVER, draw).

handle_call(draw, _From, #state{size=Size, rot=Rot, fov=FOV,
				gl=GL, scale=Scale} = State) ->
    wxGLCanvas:setCurrent(GL),
    set_view(Size, Rot, FOV),
    wirecube:draw(),
    gl:scalef(Scale, Scale, Scale),
    draw_cb(State),
    wxGLCanvas:swapBuffers(GL),
    {reply, ok, State}.

new(Frame, Size) ->
    wx_object:start_link(?MODULE, [Frame, Size], []).


init([Frame, Size]) ->
    Opts = [{size, Size}],
    GLAttrib = [{attribList, [?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_DEPTH_SIZE, 24,
			      0]}],
    GL = wxGLCanvas:new(Frame, Opts ++ GLAttrib),

    wxFrame:connect(GL, left_down),
    wxFrame:connect(GL, mousewheel),
    wxFrame:connect(GL, motion),
    wxFrame:connect(GL, enter_window),
    wxFrame:connect(GL, key_up),

    ?D_REGISTER(?SERVER, self()),

    {GL, #state{size=Size, frame=Frame, gl=GL}}.


handle_event(#wx{event=#wxMouse{type=left_down, x=X, y=Y}}, State) ->
    {noreply, State#state{mouse={X, Y}}};

handle_event(#wx{event=#wxMouse{type=motion, leftDown=true, x=X, y=Y}}, #state{rot=Rot} = State) ->
    {OldX, OldY} = State#state.mouse,
    DX = X - OldX,
    DY = Y - OldY,
    {RX, RY, RZ} = Rot,
    NRX = trunc(RX+DY+360) rem 360,
    NRY = trunc(RY+DX+360) rem 360,
    NewRot = {NRX, NRY, RZ},
    %% io:format("New Rot: ~p~n", [NewRot]),
    {noreply, State#state{rot=NewRot, mouse={X, Y}}};

handle_event(#wx{event=#wxMouse{type=motion}}, State) ->
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=mousewheel, wheelRotation=R}}, #state{fov=FOV} = State) when R < 0 ->
    NewFOV = FOV+1,
    {noreply, State#state{fov=NewFOV}};

handle_event(#wx{event=#wxMouse{type=mousewheel}}, #state{fov=FOV} = State) ->
    NewFOV = FOV-1,
    {noreply, State#state{fov=NewFOV}};

handle_event(#wx{event=#wxMouse{type=enter_window}}, State) ->
    wxFrame:setFocus(State#state.gl),
    {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=?O_FS}}, State) ->
%%     Frame = State#state.frame,
%%     ec_cf:toggle(?O_FS),
%%     New = ec_cf:opt(?O_FS),
%%     wxTopLevelWindow:showFullScreen(Frame, New),
%%     {noreply, State};

handle_event(#wx{event=#wxKey{keyCode=$ }}, State) ->
    rec:switch(),
    {noreply, State};

%% +/-: Change shape scale
%% +
%% XXXX macros MOUSE WHEEL
handle_event(#wx{event=#wxKey{keyCode=61}}, #state{scale=Scale} = State) ->
    {noreply, State#state{scale=Scale+?SCALE_STEP}};
%% -
handle_event(#wx{event=#wxKey{keyCode=45}}, #state{scale=Scale} = State) ->
    {noreply, State#state{scale=Scale-?SCALE_STEP}};

handle_event(#wx{event=#wxKey{keyCode=$M}}, #state{mode=Mode} = State) ->
    NewMode = case Mode of
		  ?GL_POINTS ->
		      ?GL_LINE_STRIP;
		  ?GL_LINE_STRIP ->
		      ?GL_POINTS
	      end,
    {noreply, State#state{mode=NewMode}};

handle_event(#wx{event=#wxKey{keyCode=$S}}, #state{spline = false} = State) ->
    {noreply, State#state{spline = true}};
handle_event(#wx{event=#wxKey{keyCode=$S}}, #state{spline = true} = State) ->
    {noreply, State#state{spline = false}};

handle_event(#wx{event=#wxKey{keyCode=$C}}, #state{color = false} = State) ->
    {noreply, State#state{color = true}};
handle_event(#wx{event=#wxKey{keyCode=$C}}, #state{color = true} = State) ->
    {noreply, State#state{color = false}};

handle_event(#wx{event=#wxKey{keyCode=_KC}}, State) ->
    %% ?D_F("Unhandled key: ~p~n", [_KC]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ?D_TERMINATE(_Reason).


%% TODO aspect ratio dans le state
set_view({Width, Height}, Rot, FOV) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:clearColor(10/255, 30/255, 10/255, 1.0),
    gl:clearDepth(?ZMAX),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    Ratio = Width / Height,

    glu:perspective(FOV, Ratio, 0.1, ?ZMAX),
    glu:lookAt(0.0, 0.0, 3.14,
	       0.0, 0.0, -3.14,
	       0.0, 1.0, 0.0),

    {RotX, RotY, RotZ} = Rot,
    gl:rotatef(RotX, 1.0, 0.0, 0.0),
    gl:rotatef(RotY, 0.0, 1.0, 0.0),
    gl:rotatef(RotZ, 0.0, 0.0, 1.0),

    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).


draw_cb(State) ->
    %% FIXME remove from rec. case rec:data(Last) of
    %% FIXME remove _New
    {_New, Samples} = rec:data(undefined),
    %% io:format("~p samples~n", [length(Samples)]),
    draw_cb2(Samples, State).


draw_cb2(Samples, #state{mode=Mode, spline=Spline, color=Color}) ->
    Mono1 = embed3(Samples),
    Mono2 = case Spline of
		true ->
		    spline:spline(?SPAN, Mono1);
		false ->
		    Mono1
	    end,
    gl:pointSize(?SIZE),
    gl:lineWidth(?SIZE),
    gl:'begin'(Mode),
    case Color of
	false ->
	    draw_cb3(Mono2, ?MONO_C);
	true ->
	    draw_cb3(Mono2, Mono2)
    end,
    gl:'end'().


draw_cb3([Point | Points], [Color | Colors]) ->
    add_point(Point, color(Color)),
    draw_cb3(Points, Colors);
draw_cb3([], []) ->
    ok;
draw_cb3(Points, Color) ->
    [add_point(Point, Color) || Point <- Points].


add_point(Point, Color) ->
    gl:color3fv(Color),
    gl:vertex3fv(Point).


color({R, G, B}) ->
    {rescale(R), rescale(G), rescale(B)}.

%% Val in [-1..+1] -> [0..1]
rescale(Val) ->
    (Val + 1.0) / 2.0 + 0.1.


%% Takens embedding (dim=3, delay=1)
embed3(List) when is_list(List) ->
    embed3(List, []).

embed3([X,Y,Z|Tail], Acc) ->
    Point = {X,Y,Z},
    embed3([Y,Z|Tail], [Point|Acc]);
embed3(_Rest, Acc) ->
    Acc.
