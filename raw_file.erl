%%%-------------------------------------------------------------------
%%% @author Olivier <olivier@biniou.info>
%%% @copyright (C) 2011, Olivier
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2011 by Olivier <olivier@r2d2.biniou.net>
%%%-------------------------------------------------------------------
-module(raw_file).

-include_lib("wx/include/wx.hrl").

-include("debug.hrl").
-include("point3d.hrl").

-behaviour(wx_object).
%% -behaviour(gen_server).

%% module API
-export([load/1, data/1]).

%% tests
-define(TEST, "raw/irish11b.raw").
-export([test/0]).

%% wx_object API
-export([new/1]). %% start_link/1]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%% %% GUI macros
%% -define(QUIT,   ?wxID_EXIT).
%% -define(ABOUT,  ?wxID_ABOUT).

-record(state, {frame, last, data=[]}).

%%%===================================================================
%%% API
%%%===================================================================
load(File) ->
    wx_object:call(?SERVER, {load, File}).

data(Last) ->
    wx_object:call(?SERVER, {data, Last}).

test() ->
    load(?TEST).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
new(Wx) ->
    wx_object:start_link(?MODULE, Wx, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Wx) ->
    %% process_flag(trap_exit, true),
    Size = {200, 30},
    Frame = wx:batch(fun() -> create_window(Wx, Size) end),
    ?D_REGISTER(?SERVER, self()),
    {Frame, #state{frame=Frame}}.
%%    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({load, File}, _From, #state{last=Last, data=Data} = State) ->
    {Reply, NewLast, NewData} = load_file(Last, File, Data),
    {reply, Reply, State#state{last=NewLast, data=NewData}};

handle_call({data, Last}, _From, #state{last=Last} = State) ->
    {reply, Last, State};

handle_call({data, _Last}, _From, #state{last=New, data=Data} = State) ->
    {reply, {New, Data}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_event
handle_event(#wx{event = #wxFileDirPicker{type = command_filepicker_changed,
                                          path = Path}}, #state{last=Last, data=Data} = State) ->
    %% io:format("Filepicker changed to ~p.\n", [Path]),
    %% load(Path),
    {_Reply, NewLast, NewData} = load_file(Last, Path, Data),
    {noreply, State#state{last=NewLast, data=NewData}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?D_F("Unhandled info: ~p~n", [Info]),
    {noreply, State}.

%% handle_info(_Info, State) ->
%%     {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-define(MINSIZE, 6).

%% Assume 8-bit unsigned file
process_file(Bin) when size(Bin) < ?MINSIZE ->
    [];
process_file(Bin) ->
    process_file2(binary_to_list(Bin), []).

process_file2([X,Y,Z,R,G,B|Tail], Acc) ->
    X2 = rescale(X),
    Y2 = rescale(Y),
    Z2 = rescale(Z),
    Point = #point3d{x=X2, y=Y2, z=Z2, r=R, g=G, b=B},
    process_file2([Y,Z,R,G,B|Tail], [Point|Acc]);
process_file2(_Rest, Acc) ->
    Acc.


rescale(V) ->
    (V/255)-0.5.

create_window(Wx, Size) ->
    Frame = wxFrame:new(Wx, ?wxID_ANY, "raw_file",
			[%%{pos, {0, 0}},
			 {size, Size},
			 {style, ?wxFRAME_NO_WINDOW_MENU}]),

    FP = wxFilePickerCtrl:new(Frame, 1, [{path, "./raw/"}, {message, "Select a raw file"}]),
    wxFilePickerCtrl:connect(FP, command_filepicker_changed),

    wxFrame:show(Frame),
    Frame.


%% {Reply, NewLast, NewData}
load_file(Last, File, Data) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    {ok, make_ref(), process_file(Bin)};
	{error, _Reason} = Error ->
	    {Error, Last, Data}
    end.
