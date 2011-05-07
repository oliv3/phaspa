%%%-------------------------------------------------------------------
%%% File    : rec.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Recorder port
%%%-------------------------------------------------------------------
-module(rec).
-author('olivier@biniou.info').

-include("debug.hrl").

%% Module API
-export([new/0, destroy/0]).
-export([record/1, stop/0]).
-export([data/1]).

%% DEBUG
-export([data/0]).

%% Internal exports
-export([boot/0]).

%% -define(STOP_CHAR, <<"S">>).

-define(SERVER, ?MODULE).

-record(state, {port, last=make_ref(), data=[]}).

 %%, frame=0, data}).

%% -define(BINIOU, "lebiniou").
%% -define(INPUT,  "pulseaudio").
%% -define(OUTPUT, "erlang").
%% -define(I2L(X), integer_to_list(X)).

%% Available Biniou input plugins
%% -define(INPUTS, ["alsa", "devurandom", "esound", "oss", "pulseaudio"]).


%% args(Id) ->
%%     Input = case ec:get_env(input) of
%% 		undefined ->
%% 		    ?INPUT;
%% 		I ->
%% 		    check_input(I)
%% 	    end,
%%     ?D_F("input= ~s", [Input]),
%%     {W, H} = ec:get_env(biniou_size),
%%     lists:flatten([" -i ", Input, " -o ", ?OUTPUT
%% 		   " -x ", ?I2L(W), " -y ", ?I2L(H),
%% 		   " -m ", ?I2L(ec:get_env(biniou_fps)), " -r 2 ",
%% 		   " -p /tmp/biniou" ++ ?I2L(Id),
%% 		   " > /dev/null 2>&1"]).


new() ->
    %% process_flag(trap_exit, true),
    spawn(?MODULE, boot, []).

boot() ->
    %% TODO use code:priv_dir() later
    Cmd = "./rec",
    Port = open_port({spawn_executable, Cmd}, [{packet, 4}, in, out, binary]),
    register(?SERVER, self()),
    loop(#state{port=Port}).

destroy() ->
    do(destroy).

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
		    loop(State#state{last=make_ref(), data=binary_to_term(PortData)});
		
		_Other ->
		    %%stop_biniou(Port),
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
