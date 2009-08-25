-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MNESIA_TABLES, [counter, object, probe, object_probe]).

stop() -> mnesia:stop().

start() ->
	start([node()]).

start(Nodes) ->
	ok = mnesia:start(),
	case is_fresh_startup() of
		true ->
			case mnesia:system_info(is_running) of
				yes -> mnesia:stop();
				_ -> pass
			end,
			mnesia:create_schema(Nodes),
			mnesia:start(),
			create_tables(Nodes);
		{exists, _Tables} ->
			case mnesia:wait_for_tables(?MNESIA_TABLES, 20000) of
				{timeout, Remaining} -> erlang:error({missing_required_tables, Remaining});
				ok -> ok
			end
	end.

is_fresh_startup() ->
	yes = mnesia:system_info(is_running),
	Node = node(),
	case mnesia:system_info(tables) of
		[schema] -> true;
		Tables ->
			case mnesia:table_info(schema, cookie) of
				{_, Node} -> {exists, Tables};
				_ -> true
			end
	end.

create_tables(Nodes) ->
	mnesia:create_table(counter,
		[
			{disc_copies, Nodes},
			{attributes, record_info(fields, counter)}
		]
	),
	mnesia:create_table(object,
		[
			{disc_copies, Nodes},
			{attributes, record_info(fields, object)}
		]
	),
	mnesia:create_table(probe,
		[
			{disc_copies, Nodes},
			{attributes, record_info(fields, probe)}
		]
	),
	mnesia:create_table(object_probe,
		[
			{type, bag},
			{disc_copies, Nodes},
			{attributes, record_info(fields, object_probe)}
		]
	).

reset() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	start().

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
		{aborted, {throw, Exception}} ->
			erlang:throw(Exception);
		{aborted, Reason} ->
			erlang:error({db_error, Reason})
	end.

read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),
	find(Q).

read(Oid) ->
	F = fun() ->
		mnesia:read(Oid)
	end,
	case transaction(F) of
		[ Object ] ->
			Object;
		[] ->
			throw(probix_error:create(not_found, "not found"))
	end.

create(Rec) when is_tuple(Rec) ->
	F = fun () ->
			mnesia:write(Rec),
			Rec
	end,
	transaction(F);

create(List) when is_list(List) -> lists:map( fun(R) -> create(R) end, List ).

update(Rec) ->
	F =	fun () ->
			[ Record, Id | _Tail ] = tuple_to_list(Rec),
			read({Record, Id}), %% foreign key check
			mnesia:write(Rec),
			Rec
	end,
	transaction(F).

delete(Oid) ->
	F = fun() ->
		read(Oid),
		mnesia:delete(Oid),
		Oid
	end,
	transaction(F).

