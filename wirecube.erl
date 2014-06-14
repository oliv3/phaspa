-module(wirecube).
-author('olivier@biniou.info').

%%-include_lib("wx/include/gl.hrl"). 
-include("gui.hrl").

-export([draw/0, draw/1]). %% axes/1, plane/1, cube/1, textured/2]).


-define(ZERO, 0.0).
-define(ONE,  1.0).
%%-define(MAXP, 1.5).
%%-define(MAXA, 2.0).
%%-define(MC,   64).

-define(CUBE, {{ ?ONE,  ?ONE, -?ONE},   %1
	       { ?ONE, -?ONE, -?ONE},   %2
	       {-?ONE, -?ONE, -?ONE},
	       {-?ONE,  ?ONE, -?ONE},   %4
	       {-?ONE,  ?ONE,  ?ONE},
	       { ?ONE,  ?ONE,  ?ONE},   %6
	       { ?ONE, -?ONE,  ?ONE},
	       {-?ONE, -?ONE,  ?ONE}}). %8

-define(E(X), gl:vertex3fv(element(X, ?CUBE))).


set_model_view() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().


draw() ->
    draw({?ZERO, ?ONE/2, ?ZERO}).

draw(Color) ->
    set_model_view(),
    gl:lineWidth(1.0),
    gl:'begin'(?GL_LINES),
    gl:color3fv(Color),
    ?E(1), ?E(2), ?E(2), ?E(3), ?E(3), ?E(4),
    ?E(4), ?E(5), ?E(5), ?E(8), ?E(8), ?E(3),
    ?E(1), ?E(6), ?E(6), ?E(7), ?E(7), ?E(2),
    ?E(6), ?E(5), ?E(5), ?E(8), ?E(8), ?E(7),
    ?E(6), ?E(1), ?E(1), ?E(4), ?E(4), ?E(5),
    ?E(7), ?E(2), ?E(3), ?E(8),
    gl:'end'().
