-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MNESIA_TABLES, [counter, object]).

stop() -> mnesia:stop().

start_ram() -> start_master(ram_copies, [node()]).

stop_replica(Node) when is_atom(Node) ->
	rpc:call(Node, probix_db, stop, []).

remove_replica(Node) when is_atom(Node) ->
	stop_replica(Node),
	mnesia:del_table_copy(schema, Node).

start_replica(Master_node) when is_atom(Master_node) ->
	ok = mnesia:start(),

	case mnesia:change_config(extra_db_nodes, [Master_node]) of
		{ok, []} ->
			%% If mnesia on the current node already knew to link up with Master_node,
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

	{atomic, ok} = create_schema(disc_copies),

	Tables = mnesia:system_info(tables),
	ok = replicate_tables(Tables),

	case mnesia:wait_for_tables(Tables, 200000) of
		{timeout, Remaining} -> erlang:error({missing_required_tables, Remaining});
		ok -> ok
	end.

replicate_tables([H|T]) ->
	{atomic, ok} = case lists:member(node(), mnesia:table_info(H, disc_copies)) of
		true -> {atomic, ok};
		false ->
			case H of
				schema -> {atomic, ok}; %% schema is already created
				_ -> mnesia:add_table_copy(H, node(), disc_copies)
			end
	end,
	replicate_tables(T);

replicate_tables([]) -> ok.

start_master(Storage_type, Nodes) when is_atom(Storage_type) ->
	ok = mnesia:start(),
	case is_fresh_startup() of
		yes ->
			{atomic, ok} = create_schema(Storage_type),
			ok = create_tables(Storage_type, Nodes);
		no ->
			case mnesia:wait_for_tables(?MNESIA_TABLES, 20000) of
				{timeout, Remaining} -> erlang:error({missing_required_tables, Remaining});
				ok -> ok
			end
	end.

create_schema(Storage_type) -> mnesia:change_table_copy_type(schema, node(), Storage_type).

is_fresh_startup() ->
	yes = mnesia:system_info(is_running),
	Node = node(),
	case mnesia:system_info(tables) of
		[schema] -> yes;
		_Tables ->
			case mnesia:table_info(schema, cookie) of
				{_, Node} -> no;
				_ -> yes
			end
	end.

create_tables(Storage_type, Nodes) when is_atom(Storage_type) ->
	yes = mnesia:system_info(is_running),
	F = fun() ->
		ok = create_table(counter,
			[
				{Storage_type, Nodes},
				{attributes, record_info(fields, counter)}
			]
		),
		ok = create_table(object,
			[
				{Storage_type, Nodes},
				{attributes, record_info(fields, object)}
			]
		)
	end,
	transaction(F).

new_id(Key) ->
	mnesia:dirty_update_counter({counter, Key}, 1).

create_table(Name, Properties) ->
	%% because mnesia:create_table/2 is not allowed in mnesia:transaction/1
	Cs = mnesia_schema:list2cs([{name, Name}|Properties]),
	mnesia_schema:do_create_table(Cs).

delete_table(Name) ->
	%% because mnesia:delete_table/1 is not allowed in mnesia:transaction/1
	mnesia_schema:do_delete_table(Name).

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

create(Tab, Rec) when is_atom(Tab), is_tuple(Rec) ->
	F = fun () ->
			mnesia:write(Tab, Rec, write),
			Rec
	end,
	transaction(F);

create(Tab, List) when is_atom(Tab), is_list(List) -> lists:map( fun(R) -> create(Tab, R) end, List ).

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

