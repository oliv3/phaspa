%%%-------------------------------------------------------------------
%%% File    : screen.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : OpenGL widget
%%%-------------------------------------------------------------------
-module(screen).
-author('olivier@biniou.info').

-include("gui.hrl").
-include("debug.hrl").
-include("point3d.hrl").

-behaviour(wx_object).

%% API
-export([draw/0]).

%% wx_object API
-export([new/2]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_event/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(PSIZE, 1.5).
-define(SPAN, 5).


%% GL widget state
-record(state, {size, rot=?DEFAULT_ROT, fov=?DEFAULT_FOV,
		frame, gl, mouse,
		%% scaling
		scale=1.5,
		%% drawing mode
		mode=?GL_POINTS,
		%% display-list stuff
		last, base=0}).

-define(ZMAX, 5.0).
-define(SCALE_STEP, 0.2).

-define(MONO,  0).
-define(LEFT,  1).
-define(RIGHT, 2).

-define(O, 1.0).
-define(Z, 0.0).
-define(MONO_C,  {?O, ?O, ?O}).
-define(LEFT_C,  {?Z, ?Z, ?O}).
-define(RIGHT_C, {?O, ?Z, ?Z}).


draw() ->
    wx_object:call(?SERVER, draw).

handle_call(draw, _From, #state{size=Size, rot=Rot, fov=FOV, gl=GL, scale=Scale} = State) ->
    %% ?D_F("======= screen / call~n", []),
    wxGLCanvas:setCurrent(GL),
    set_view(Size, Rot, FOV),
    wirecube:draw(),
    gl:scalef(Scale, Scale, Scale),
    NewState = make_lists(State),
    draw_list(State, ?MONO),
    wxGLCanvas:swapBuffers(GL),
    {reply, ok, NewState}.

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

    ?D_REGISTER(?SERVER, self()), %% not needed ?

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

%% +/-: Change shape scale
%% +
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
    {noreply, State#state{last=make_ref(), mode=NewMode}};

%% handle_event(#wx{event=#wxKey{keyCode=$A}}, State) ->
%%     ec_cf:toggle(?O_AXES),
%%     {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=$S}}, State) ->
%%     ec_cf:toggle(?O_SPIN),
%%     {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=$Z}}, State) ->
%%     ec_cf:no_spin(),
%%     ec_cf:reset_rot(),
%%     {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=$E}}, State) ->
%%     ec_cf:toggle(?O_EDGES),
%%     {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=$T}}, State) ->
%%     ec_cf:toggle(?O_TEXT),
%%     {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=$M}}, State) ->
%%     ec_cf:toggle(?O_MUTE),
%%     {noreply, State};

%% handle_event(#wx{event=#wxKey{keyCode=$O}}, State) ->
%%     ec_cf:toggle(?O_OSD),
%%     {noreply, State};

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


make_lists(#state{last=Last, base=Base, mode=Mode} = State) ->
    case rec:data(Last) of
	Last ->
	    State;
	{New, Channels} ->
	    gl:deleteLists(Base, 3),
	    %% ?D_F("Channels: ~p~n", [Channels]),
	    NewBase = gl:genLists(3),
	    %% ?D_F("New base: ~p~n", [NewBase]),
	    make_lists2(Mode, NewBase, Channels),
	    State#state{last=New, base=NewBase}
    end.

draw_list(#state{last=_Last, base=Base, mode=_Mode} = _State, Id) ->
    List = Base+Id,
    %% case rec:data(Last) of
    %% 	Last ->
    gl:callList(List).
    %% 	    State;
    %% 	{New, Channels} ->
    %% 	    %% ?D_F("Channels: ~p~n", [Channels]),
    %% 	    NewBase = make_lists(Mode, List, Channels),
    %% 	    State#state{last=New, base=NewBase}
    %% end.


%% make_lists(Mode, OldBase, Channels) ->
%% %%    gl:deleteLists(OldBase, 3),
%%     make_lists2(Mode, Channels).

make_lists2(Mode, Base, {Mono, _Left, _Right}) ->
    %% NewBase = gl:genLists(3),
    %% FIXME: check que gl:genLists ne renvoie pas 0
    %% make_list3(Mode, Base+?MONO, takens:embed3(Mono, ?SPAN), ?MONO_C),
    Mono1 = takens:embed3(Mono),
    %% ?D_F("yaaaaa ~p~n", [Mono1]),
    Mono2 = spline:spline(?SPAN, Mono1),
    %% ?D_F("yaaaaa 2~n", []),
    make_list3(Mode, Base+?MONO, Mono2, ?MONO_C),
    %% ?D_F("yaaaaa 3~n", []),
    %% make_list3(Mode, NewBase+?LEFT, Left, ?LEFT_C),
    %% make_list3(Mode, NewBase+?RIGHT, Right, ?RIGHT_C),
    ok.

make_list3(Mode, List, Points, Color) ->
    gl:newList(List, ?GL_COMPILE_AND_EXECUTE),
    prepare_gl(),
    gl:'begin'(Mode),
    add_points(Points, Color),
    gl:'end'(),
    gl:endList().


prepare_gl() ->
    gl:pointSize(?PSIZE),
    gl:enable(?GL_POINT_SMOOTH),
    gl:disable(?GL_BLEND),
    gl:enable(?GL_ALPHA_TEST),
    gl:alphaFunc(?GL_GREATER, 0.5).


add_points([], _Color) ->
    ok;
add_points([Point|Points], Color) ->
    %% ?D_F("Point= ~p~n", [Point]),
    %% ?D_F("Color= ~p~n", [Color]),
    gl:color3fv(Color),
    gl:vertex3fv(Point),
    add_points(Points, Color).
