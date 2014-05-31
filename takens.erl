-module(takens).
-author('olivier@biniou.info').

-export([embed3/1]).


%% Takens embedding (dim=3, delay=1)
embed3(List) when is_list(List) ->
    embed3(List, []).

embed3([X,Y,Z|Tail], Acc) ->
    Point = {X,Y,Z},
    embed3([Y,Z|Tail], [Point|Acc]);
embed3(_Rest, Acc) ->
    Acc.
