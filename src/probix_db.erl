-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MNESIA_TABLES, [series, probe]).

stop() -> mnesia:stop().

%stop_replica(Node) when is_atom(Node) ->
%	rpc:call(Node, probix_db, stop, []).

%remove_replica(Node) when is_atom(Node) ->
%	stop_replica(Node),
%	mnesia:del_table_copy(schema, Node).

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

	%Tables = mnesia:system_info(tables),
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
				schema -> {atomic, ok}; %% schema is created in start_replica/1
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
			{type, ordered_set}
		]
	),
	ok.

new_series() ->
	F = fun() ->
		Series = #series{id = probix_util:random_string(10),
			time_created = probix_format:now_to_gregorian_epoch()},
		%% generated identifier may not be unique: in this case a function will fail.
		case series(Series) of
			not_found ->
				ok = mnesia:write(Series),
				Series;
			_Rec -> transaction:abort(duplicate_id)
		end
	end,
	{atomic, Series} = mnesia:transaction(F),
	Series.

all_series() ->
	F = fun() ->
		Q = qlc:q([X || X <- mnesia:table(series)]),
		%% XXX I know that qlc:eval/1 may return error, but
		%% hardly can imagine in what cases it can happen.
		qlc:e(Q)
	end,
	{atomic, List} = mnesia:transaction(F),
	{ok, List}.

delete_series(Id) ->
	F = fun() ->
		ok = delete_probes(Id),
		mnesia:delete({series, Id})
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

series(Id) ->
	F = fun() ->
		mnesia:read({series, Id})
	end,
	case mnesia:transaction(F) of
		{atomic, []} -> not_found;
		{atomic, [Rec]} -> Rec
	end.

%% XXX I do not check the existence of the series in add_probes/1 and other probe functions
%% because it's expensive and should be done outside e.g. probix_format:probe_record_from/3

add_probe(Rec) when is_record(Rec, probe) ->
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
		Q = qlc:q([ P || P <- mnesia:table(probe), element(1, P#probe.id) == Series_id ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

get_probes(Series_id, {from, From}) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id,
			element(2, P#probe.id) >= From ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result;

get_probes(Series_id, {to, To}) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id,
			element(2, P#probe.id) =< To ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result;

get_probes(Series_id, {From, To}) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id,
			element(2, P#probe.id) >= From,
			element(2, P#probe.id) =< To ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

delete_probe(Id) ->
	F = fun() ->
		mnesia:delete({probe, Id})
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

delete_probes(Series_id) ->
	F = fun() ->
		Q = qlc:q([ {probe, P#probe.id} || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

delete_probes(Series_id, {from, From}) ->
	F = fun() ->
		Q = qlc:q([ {probe, P#probe.id} || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id,
			element(2, P#probe.id) >= From ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok;

delete_probes(Series_id, {to, To}) ->
	F = fun() ->
		Q = qlc:q([ {probe, P#probe.id} || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id,
			element(2, P#probe.id) =< To ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok;

delete_probes(Series_id, {From, To}) ->
	F = fun() ->
		Q = qlc:q([ {probe, P#probe.id} || P <- mnesia:table(probe),
			element(1, P#probe.id) == Series_id,
			element(2, P#probe.id) >= From,
			element(2, P#probe.id) =< To ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.


