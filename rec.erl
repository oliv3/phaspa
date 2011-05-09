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

-record(state, {port, last=make_ref(), data=[]}).


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
    %% Port = open_port({spawn_executable, Cmd}, [{packet, 4}, in, out, binary]),
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
    %% receive
    %% 	{Port, {data, Data}} ->
    %% 	    %% ?D_F("Got data: ~p~n", [Data]),
    %% 	    loop(State#state{last=make_ref(), data=binary_to_term(Data)})

    %% after 0 ->
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
	    port_command_wrapper(Port, Pid, Ref, Cmd),
	    loop(State);

	{Port, {data, PortData}} ->
	    RawData0 = binary_to_term(PortData),
	    %% ?D_F("RawData0= ~p~n", [RawData0]),
	    %% RawData1 = mix(RawData0, fun phase/1),
	    RawData1 = mix(RawData0, fun antiphase/1),
	    %% ?D_F("RawData1= ~p~n", [RawData1]),
	    NewData = process(RawData1, []),
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
	    ?D_F("Cmd: ~p Result: ~p~n", [Cmd, Result]),
	    Pid ! {Ref, Result}
    end.


process([X,Y,Z|Tail], Acc) ->
    Point = #point3d{x=X, y=Y, z=Z},
    process([Y,Z|Tail], [Point|Acc]);
process(_Rest, Acc) ->
    Acc.


%% phase/antiphase mean
phase({Left, Right}) ->
    (Left+Right)/2.
antiphase({Left, Right}) ->
    (Left-Right)/2.


mix(Data, F) ->
    [F(Sample) || Sample <- Data].
