-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

init() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(counter, type),
		mnesia:table_info(object, type),
		mnesia:table_info(probe, type)
	catch
		exit: _ ->
			mnesia:create_table(counter,
				[
					{disc_copies, [node()]},
					{attributes, record_info(fields, counter)}
				]
			),
			mnesia:create_table(object,
				[
					{disc_copies, [node()]},
					{attributes, record_info(fields, object)}
				]
			),
			mnesia:create_table(probe,
				[
					{disc_copies, [node()]},
					{attributes, record_info(fields, probe)}
				]
			)
	end.

reset() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	init().

new_id(Key) ->
	mnesia:dirty_update_counter({counter, Key}, 1).

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

read(Oid) ->
	F = fun() ->
		mnesia:read(Oid)
	end,
	transaction(F).

write(Rec) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	mnesia:transaction(F).

delete(Oid) ->
	F = fun() ->
		mnesia:delete(Oid)
	end,
	mnesia:transaction(F).

