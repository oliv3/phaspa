%%%-------------------------------------------------------------------
%%% @author Olivier <olivier@biniou.info>
%%% @copyright (C) 2011, Olivier
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2011 by Olivier <olivier@r2d2.biniou.net>
%%%-------------------------------------------------------------------
-module(raw_file).

-include("point3d.hrl").

-behaviour(gen_server).

%% module API
-export([load/1, data/0]).

%% tests
-define(TEST, "raw/irish11b.raw").
-export([test/0]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {data=[]}).

%%%===================================================================
%%% API
%%%===================================================================
load(File) ->
    gen_server:call(?SERVER, {load, File}).

data() ->
    gen_server:call(?SERVER, data).

test() ->
    load(?TEST).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{}}.

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
handle_call({load, File}, _From, #state{data=Data} = State) ->
    {Reply, NewData} = case file:read_file(File) of
			   {ok, Bin} ->
			       {ok, process_file(Bin)};
			   {error, _Reason} = Error ->
			       {Error, Data}
		       end,
    {reply, Reply, State#state{data=NewData}};

handle_call(data, _From, #state{data=Data} = State) ->
    {reply, Data, State};

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
handle_info(_Info, State) ->
    {noreply, State}.

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
