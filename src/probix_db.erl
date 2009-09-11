-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MNESIA_TABLES, [counter, object, probe, object_probe]).

stop() -> mnesia:stop().

start_disc_only() -> start(disc_only_copies, [node()]).
start_disc() -> start(disc_copies, [node()]).
start_ram() -> start(ram_copies, [node()]).

start_replica(Storage_type, Master_node) when is_atom(Storage_type), is_atom(Master_node) ->
	ok = mnesia:start(),

	case mnesia:change_config(extra_db_nodes, [Master_node]) of
		{ok, []} ->
			%% If mnesia on the current node already knew to link up with MasterNode,
			%% change_config says 'ok' but with an empty list. This is however exactly
			%% the same thing that happens if we fail to connect to the remote node
			%% because of an distribution mechanism failure so we need to make sure
			%% we are online...
			case lists:member(Master_node, mnesia:system_info(running_db_nodes)) of
				true -> ok;
				false -> erlang:error({change_config_failed, "failed connecting to master node"})
			end;
		{ok, [Master_node]} -> ok;
		{error, E} -> erlang:error({change_config_failed, E})
	end,

	mnesia:change_table_copy_type(schema, node(), Storage_type),

	Tables = mnesia:system_info(tables),
	ok = replicate_tables(Storage_type, Tables),

	case mnesia:wait_for_tables(Tables, 200000) of
		{timeout, Remaining} -> erlang:error({missing_required_tables, Remaining});
		ok -> ok
	end.

replicate_tables(Storage_type, [H|T]) when is_atom(Storage_type) ->
	{atomic, ok} = case lists:member(node(), mnesia:table_info(H, Storage_type)) of
		true -> {atomic, ok};
		false -> mnesia:add_table_copy(H, node(), Storage_type)
	end,
	replicate_tables(Storage_type, T);

replicate_tables(_, []) -> ok.

start(Storage_type, Nodes) when is_atom(Storage_type) ->
	ok = mnesia:start(),
	case is_fresh_startup() of
		true ->
			mnesia:change_table_copy_type(schema, node(), Storage_type),
			create_tables(Storage_type, Nodes);
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

create_tables(Storage_type, Nodes) when is_atom(Storage_type) ->
	yes = mnesia:system_info(is_running),
	mnesia:create_table(counter,
		[
			{Storage_type, Nodes},
			{attributes, record_info(fields, counter)}
		]
	),
	mnesia:create_table(object,
		[
			{Storage_type, Nodes},
			{attributes, record_info(fields, object)}
		]
	),
	mnesia:create_table(probe,
		[
			{Storage_type, Nodes},
			{attributes, record_info(fields, probe)}
		]
	),
	mnesia:create_table(object_probe,
		[
			{Storage_type, Nodes},
			{type, bag},
			{attributes, record_info(fields, object_probe)}
		]
	).

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

