-module(takens).
-author('olivier@biniou.info').

-export([embed3/1]).
-export([msplit/2]). %% TODO: msplit/3 avec une taille minimum ?


%% Takens embedding (dim=3, delay=1)
embed3(List) when is_list(List) ->
    embed3(List, []).

embed3([X,Y,Z|Tail], Acc) ->
    Point = {X,Y,Z},
    embed3([Y,Z|Tail], [Point|Acc]);
embed3(_Rest, Acc) ->
    Acc.


%% multi-split
msplit(Size, List) ->
    msplit(Size, List, []).

msplit(Size, List, Acc) when length(List) >= Size ->
    {Chunk, Rest} = lists:split(Size, List),
    msplit(Size, Rest, [Chunk|Acc]);
msplit(_Size, [], Acc) ->
    lists:reverse(Acc);
msplit(_Size, Rest, Acc) ->
    lists:reverse([Rest|Acc]).
