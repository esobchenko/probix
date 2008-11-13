-module(probix_db).
-compile(export_all).

-include("probix_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

reset() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(point, [{disc_copies, [node()]}, {attributes, record_info(fields, point)}]).

find(Q) ->
	F = fun() ->
		qlc:e(Q)
	end,
	transaction(F).

transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, _Reason} ->
			[]
	end.

read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),
	find(Q).

write(Rec) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

%%% eof
