%%%-------------------------------------------------------------------
%%% File    : rec.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Recorder port
%%%-------------------------------------------------------------------
-module(rec).
-author('olivier@biniou.info').

-include("debug.hrl").
-include("point3d.hrl").

%% Module API
-export([new/0, destroy/0]).
-export([record/1, stop/0]).
-export([data/1]).

-export([phase/1, antiphase/1]).

%% DEBUG
-export([data/0]).

%% Internal exports
-export([boot/1]).

-define(SERVER, ?MODULE).

-record(state, {port, last=make_ref(), data={[], [], []}}).


new() ->
    %% process_flag(trap_exit, true),
    spawn(?MODULE, boot, [self()]),
    receive
	started ->
	    ok
    end.


boot(Parent) ->
    %% TODO use code:priv_dir() later
    Cmd = "./rec",
    Port = open_port({spawn_executable, Cmd}, [{packet, 4}, binary]),
    register(?SERVER, self()),
    Parent ! started,
    loop(#state{port=Port}).


do(Msg) ->
    Ref = make_ref(),
    ?SERVER ! {self(), Ref, Msg},
    receive
	{Ref, Result} ->
	    Result
    end.


record(Freq) ->
    do({record, Freq}).

stop() ->
    do(stop).

data() ->
    do(data).

data(Last) ->
    do({data, Last}).

destroy() ->
    do(destroy).


loop(#state{port=Port, last=Last, data=Data} = State) ->
    receive
	{Pid, Ref, data} ->
	    Pid ! {Ref, Data},
	    loop(State);

	{Pid, Ref, {data, Last}} ->
	    Pid ! {Ref, Last},
	    loop(State);
	{Pid, Ref, {data, _Older}} ->
	    Pid ! {Ref, {Last, Data}},
	    loop(State);

	{'EXIT', Port, _Reason} ->
	    ?D_F("~p exiting with reason: ~p", [Port, _Reason]);

	{Pid, Ref, destroy} ->
	    port_close(Port),
	    Pid ! {Ref, ok};

	{Pid, Ref, {record, _Freq} = Cmd} ->
	    port_command_wrapper(Port, Pid, Ref, Cmd),
	    loop(State);

	{Pid, Ref, stop = Cmd} ->
	    port_command_wrapper(Port, Pid, Ref, Cmd, ok),
	    loop(State);

	{Port, {data, PortData}} ->
	    Samples = binary_to_term(PortData),

	    Mono = mono(Samples, fun phase/1),
	    %% Mono = mono(Samples, fun antiphase/1),

	    NewData = {Mono, left(Samples), right(Samples)},

	    loop(State#state{last=make_ref(), data=NewData});

	_Other ->
	    ?D_UNHANDLED(_Other)
	    %% end
    end.


port_command_wrapper(Port, Pid, Ref, Cmd) ->
    port_command(Port, term_to_binary(Cmd)),
    receive
	{Port, {data, BinResult}} ->
	    Result = binary_to_term(BinResult),
	    %% ?D_F("Cmd: ~p Result: ~p~n", [Cmd, Result]),
	    Pid ! {Ref, Result}
    end.

port_command_wrapper(Port, Pid, Ref, Cmd, Expected) ->
    port_command(Port, term_to_binary(Cmd)),
    port_command_wrapper2(Port, Pid, Ref, Expected).
port_command_wrapper2(Port, Pid, Ref, Expected) ->
    receive
	{Port, {data, BinResult}} ->
	    Result = binary_to_term(BinResult),
	    %% ?D_F("Cmd: ~p Result: ~p~n", [Cmd, Result]),
	    case Result of
		Expected ->
		    Pid ! {Ref, Result};
		_Other ->
		    port_command_wrapper2(Port, Pid, Ref, Expected)
	    end
    end.


%% phase/antiphase mean
phase({Left, Right}) ->
    (Left+Right)/2.
antiphase({Left, Right}) ->
    (Left-Right)/2.


mono(Data, F) ->
    [F(Sample) || Sample <- Data].

left(Samples) ->
    [L || {L, _R} <- Samples].

right(Samples) ->
    [R || {_L, R} <- Samples].
