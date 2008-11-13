
-module(point).
-compile(export_all).

-include("probix_db.hrl").


read_all() ->
	Points = probix_db:read_all(point),
	lists:map(fun(F) -> {struct, [{<<"x">>, F#point.x}, {<<"y">>, F#point.y}]} end, Points).


create(S) ->
	{struct, L} = S,
	X = proplists:get_value(<<"x">>, L),
	Y = proplists:get_value(<<"y">>, L),
	{atomic, ok} = probix_db:write({point, X, Y}).


