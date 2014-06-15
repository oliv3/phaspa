-module(particle_system).

-behaviour(gen_server).

%% -include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

%% API
-export([start_link/2]).
-export([stop/0]).

-export([draw/0]).
-export([create/1]).
-export([remove/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([cloud/1]).

-define(SERVER, ?MODULE).
-define(TABLE, clouds).

-record(state, {pids = []}).

-define(TTL, 2000). %% Time To Live
-define(STEP, 300).
-define(FACT, 0.5).

-record(cloud, {age = 0, particles}).

%%%===================================================================
%%% API
%%%===================================================================
draw() ->
    gen_server:call(?SERVER, draw).

create(Particles) ->
    gen_server:cast(?SERVER, {create, Particles}).

remove(Pid) ->
    gen_server:cast(?SERVER, {remove, Pid}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Env, GL) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Env, GL], []).


stop() ->
    gen_server:call(?SERVER, stop).

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
init([Env, GL]) ->
    Tid = ets:new(?TABLE, [named_table, public]),
    io:format("Starting particle system, Env= ~p Tid= ~p~n", [Env, Tid]),
    wx:set_env(Env),
    wxGLCanvas:setCurrent(GL),
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
handle_call(draw, _From, #state{pids = Pids} = State) ->
    gl:pointSize(1.0),
    gl:'begin'(?GL_POINTS),
    [draw_cb(Pid) || Pid <- Pids],
    gl:'end'(),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    io:format("Stopping particle system~n"),
    Reply = ok,
    {stop, normal, Reply, State};

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
handle_cast({create, Particles0}, #state{pids = Pids} = State) ->
    Particles = make_particles(Particles0),
    Pid = spawn(?MODULE, cloud, [Particles]),
    ets:insert(?TABLE, {Pid, Particles}),
    NewPids = [Pid | Pids],
    %% io:format("New cloud, pids= ~p~n", [length(NewPids)]),
    {noreply, State#state{pids = NewPids}};

handle_cast({remove, Pid}, #state{pids = Pids} = State) ->
    NewPids = Pids -- [Pid],
    ets:delete(?TABLE, Pid),
    %% io:format("Done cloud, pids= ~p~n", [length(NewPids)]),
    {noreply, State#state{pids = NewPids}};

handle_cast(_Msg, State) ->
    io:format("unknown cast ~w~n", [_Msg]),
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
    io:format("~s: Unhandled info ~p~n", [?MODULE, _Info]),
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
%% draw_cb_old() ->
%%     gl:pushMatrix(),
%%     R = random:uniform() * 0.1 + 1,
%%     gl:scalef(R, R, R),
%%     wirecube:draw({0.0, 0.0, 1.0}),
%%     gl:popMatrix().


draw_cb(Pid) ->
    %% draw_cb_old(),
    case ets:lookup(?TABLE, Pid) of
	[{Pid, Particles}] ->
	    %% io:format("found ~p particles for pid ~p~n", [length(Particles), Pid]),
	    [draw_particle(P) || P <- Particles];
	Other ->
	    io:format("Other in draw_cb: ~p~n", [Other]),
	    halt(1)
    end.


draw_particle(#{pos := Pos, col := Col}) ->
    gl:color3fv(Col),
    gl:vertex3fv(Pos).


make_particles(P) ->
    make_particles(P, []).
make_particles([], Acc) ->
    Acc;
make_particles([{Pos, Col} | Ps], Acc) ->
    P = #{pos => Pos, vel => vel(Pos), col => Col},
    make_particles(Ps, [P | Acc]).


vel({X, Y, Z}) ->
    {X * ?FACT, Y * ?FACT, Z * ?FACT}.


%% TODO: virer le particles du state,
%% Le process recupere la liste de particles avec un ets:lookup
cloud(Particles) when is_list(Particles) ->
    cloud(#cloud{particles = Particles});
cloud(#cloud{age = Age}) when Age > ?TTL ->
    Me = self(),
    %% io:format("--- Cloud ~p exiting~n", [Me]),
    remove(Me),
    ok;
cloud(#cloud{age = Age, particles = Particles}) ->
    %% io:format("new cloud ~p~n", [Particles]),
    timer:sleep(?STEP),
    NewAge = Age + ?STEP,
    %% io:format("*** Cloud ~p age ~p~n", [self(), NewAge]),
    NewParticles = [move(P) || P <- Particles],
    ets:insert(?TABLE, {self(), NewParticles}),
    cloud(#cloud{age = NewAge, particles = NewParticles}).


move(#{pos := Pos, vel := Vel} = P) ->
    P#{pos => add(Pos, Vel)}.


add({A, B, C}, {D, E, F}) ->
    {A+D, B+E, C+F}.
