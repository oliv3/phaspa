-module(recorder).
-author('olivier@biniou.info').

-include_lib("wx/include/wx.hrl").

-include("debug.hrl").
%% -include("point3d.hrl").

-behaviour(wx_object).

%% module API
%% -export([load/1, data/1]).

%% wx_object API
-export([new/1]). %% start_link/1]).
-export([destroy/0]).

%% wx_object callbacks
-export([init/1]). %%, handle_call/3, handle_info/2,
-export([handle_cast/2]).
-export([handle_event/2, terminate/2]). %%, code_change/3]).

-define(SERVER, ?MODULE). 

%% %% GUI macros
%% -define(QUIT,   ?wxID_EXIT).
%% -define(ABOUT,  ?wxID_ABOUT).

-record(state, {frame}).

new(Wx) ->
    wx_object:start_link(?MODULE, Wx, []).

init(Wx) ->
    %% process_flag(trap_exit, true),
    Size = {200, 200},
    Frame = wx:batch(fun() -> create_window(Wx, Size) end),
    ?D_REGISTER(?SERVER, self()),
    {Frame, #state{frame=Frame}}.

create_window(Wx, Size) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Recorder",
			[%%{pos, {0, 0}},
			 {size, Size},
			 {style, ?wxFRAME_NO_WINDOW_MENU}]),
    Panel = wxPanel:new(Frame),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    SpinSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Frequency"}]),
    Options = [{border, 4}, {flag, ?wxALL}],

    SpinCtrl = wxSpinCtrl:new(Panel), %% , []),
    wxSpinCtrl:setRange(SpinCtrl, 1000, 44100),
    wxSpinCtrl:setToolTip(SpinCtrl, "Sampling frequency"),
    wxSpinCtrl:connect(SpinCtrl, command_spinctrl_updated),
    wxSizer:add(SpinSizer, SpinCtrl, Options),

    wxSizer:add(MainSizer, SpinSizer, Options),
    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Frame),
    Frame.

destroy() ->
    wx_object:cast(?SERVER, destroy).

%% handle_event
%%handle_event(#wx{event = #wxFileDirPicker{type = command_filepicker_changed,
%%                                          path = Path}}, #state{last=Last, data=Data} = State) ->
handle_event(#wx{event=#wxSpin{type=command_spinctrl_updated, commandInt=Value}}, State) ->
    ?D_F("Spinctrl value: ~p~n", [Value]),
    rec:stop(),
    rec:record(Value),
    {noreply, State}.

handle_cast(destroy, State) ->
    {stop, normal, State}.


terminate(_Reason, _State) ->
    ok.
