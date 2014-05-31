%%%-------------------------------------------------------------------
%%% File    : spline.erl
%%% Author  : Olivier <olivier@biniou.info>
%%% Description : Compute 3D splines
%%%-------------------------------------------------------------------
-module(spline).
-author('olivier@biniou.info').

-include("debug.hrl").
-include("point3d.hrl").

%% Module API
-export([new/0, destroy/0]).
-export([spline/2]).

%% Internal exports
-export([boot/1]).

%% Test
-export([test/0]).

-define(SERVER, ?MODULE).

-record(state, {port}).


test() ->
    List = [
	    {0.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 0.0, 0.0},
	    {0.0, 1.0, 0.0}, {0.0, 0.0, 0.0}
%%	    {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 0.0, 1.0}
	   ],
    spline(3, List).

new() ->
    %% process_flag(trap_exit, true),
    spawn(?MODULE, boot, [self()]),
    receive
	started ->
	    ok
    end.

boot(Parent) ->
    %% TODO use code:priv_dir() later
    Cmd = "./spline",
    %%Cmd = "valgrind ./spline",
    %% Port = open_port({spawn_executable, Cmd}, [{packet, 4}, in, out, binary]),
    Port = open_port({spawn, Cmd}, [{packet, 4}, binary]),
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


spline(_Span, List) when length(List) < 3 ->
    List;
spline(Span, List) ->
    do({spline, Span, List}).


destroy() ->
    do(destroy).


loop(#state{port=Port} = State) ->
    receive
	{'EXIT', Port, _Reason} ->
	    ?D_F("~p exiting with reason: ~p", [Port, _Reason]);

	{Pid, Ref, destroy} ->
	    port_close(Port),
	    Pid ! {Ref, ok};

	{Pid, Ref, {spline, Span, List} = Cmd} when is_integer(Span), is_list(List) ->
	    port_command_wrapper(Port, Pid, Ref, Cmd),
	    loop(State);

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
