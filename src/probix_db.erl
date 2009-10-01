-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MNESIA_TABLES, [series, probe]).

stop() -> mnesia:stop().

%%stop_replica(Node) when is_atom(Node) ->
%%	rpc:call(Node, probix_db, stop, []).

%%remove_replica(Node) when is_atom(Node) ->
%%	stop_replica(Node),
%%	mnesia:del_table_copy(schema, Node).

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

	ok = create_schema(disc_copies),

	%% Tables = mnesia:system_info(tables),
	Tables = ?MNESIA_TABLES,
	ok = replicate_tables(Tables),

	case mnesia:wait_for_tables(Tables, 20000) of
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
			ok = create_schema(Storage_type),
			ok = create_tables(Storage_type, Nodes);
		no ->
			case mnesia:wait_for_tables(?MNESIA_TABLES, 20000) of
				{timeout, Remaining} -> erlang:error({missing_required_tables, Remaining});
				ok -> ok
			end
	end.

create_schema(Storage_type) ->
	mnesia:change_table_copy_type(schema, node(), Storage_type),
	ok.

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
	{atomic, ok} = mnesia:create_table(series,
		[
			{Storage_type, Nodes},
			{attributes, record_info(fields, series)}
		]
	),
	{atomic, ok} = mnesia:create_table(probe,
		[
			{Storage_type, Nodes},
			{attributes, record_info(fields, probe)},
			{index, [timestamp]},
			{type, ordered_set}
		]
	),
	ok.

new_series() ->
	F = fun() ->
		Now = probix_format:now_to_gregorian_epoch(),
		Id = probix_util:random_string(10),
		Series = #series{id = Id, time_created = Now},
		%% generated identifier may not be unique: in this case a function will fail.
		case series(Series) of
			not_found ->
				mnesia:write(Series),
				Series;
			_Rec -> transaction:abort(duplicate_id)
		end
	end,
	{atomic, Series} = mnesia:transaction(F),
	{ok, Series}.

all_series() ->
	F = fun() ->
		Q = qlc:q([X || X <- mnesia:table(Table)]),
		qlc:e(Q) %% XXX what happens if qlc:e/1 return Error?
	end,
	{atomic, List} = mnesia:transaction(F),
	{ok, List}.

delete_series(Oid) ->
	F = fun() ->
		{series, Id} = Oid,
		ok = delete_probes(Id),
		mnesia:delete(Oid),
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

series(Oid) ->
	F = fun() ->
		mnesia:read(Oid)
	end,
	case mnesia:transaction(F) of
		{atomic, []} -> {error, not_found};
		{atomic, [Rec]} -> {ok, Rec}
	end.

%% probe functions

add_probe(Rec) when is_record(probe, Rec) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

add_probes(List) when is_list(List) ->
	F = fun() ->
		lists:foreach( fun mnesia:write/1, List ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

get_probes(Series_id) ->
	F = fun() ->
		case series({series, Series_id}) of
			{error, not_found} -> {error, bad_series};
			_Rec ->
				Q = qlc:q([ P || P <- mnesia:table(Table) ]),
				qlc:e(Q)
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

get_probes(Series_id, {from, Timestamp}) ->
	F = fun() ->
		case series({series, Series_id}) of
			not_found -> bad_series;
			_Rec ->
				Q = qlc:q([ P ||
					P <- mnesia:table(Table),
					P#probe.timestamp =< To ]),
				qlc:e(Q)
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result;

get_probes(Series_id, {to, Timestamp}) ->
	F = fun() ->
		case series({series, Series_id}) of
			not_found -> bad_series;
			_Rec ->
				Q = qlc:q([ P || P <- mnesia:table(Table),
					P#probe.timestamp >= From ]),
				qlc:e(Q)
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result;

get_probes(Series_id, {From, To}) ->
	F = fun() ->
		case series({series, Series_id}) of
			not_found -> bad_series;
			_Rec ->
				Q = qlc:q([ P || P <- mnesia:table(Table),
					P#probe.timestamp >= From,
					P#probe.timestamp =< To ]),
				qlc:e(Q)
		end
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

delete_probe(Oid) ->
	F = fun() ->
		mnesia:delete(Oid)
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

delete_probes(Series_id, {from, Timestamp}) ->
	F = fun() ->
		
delete_probes(Series_id, {to, Timestamp}) -> ok;
delete_probes(Series_id, {From, To}) -> ok.


