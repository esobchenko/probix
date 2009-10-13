-module(probix_db).
-compile(export_all).

-include("probix.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MNESIA_TABLES, [series, tick]).

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
	{atomic, ok} = mnesia:create_table(tick,
		[
			{Storage_type, Nodes},
			{attributes, record_info(fields, tick)},
			{type, ordered_set}
		]
	),
	ok.

new_series(Label) ->
	F = fun() ->
		Series = #series{
			%% XXX identifier may not be unique, then the function will overwrite the existing series
			id = list_to_binary(probix_util:random_string(10)),
			time_created = probix_format:now_to_gregorian_epoch(),
			label = Label
		},
		ok = mnesia:write(Series),
        Series
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
	List.

delete_series(Id) ->
	F = fun() ->
		ok = delete_ticks(Id),
		mnesia:delete({series, Id})
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

series(Id) ->
	mnesia:dirty_read({series, Id}).

%% XXX I do not check the existence of the series in add_ticks/1 and other tick functions
%% because it's expensive. It should be done outside e.g. in http handle functions.

add_ticks(Rec) when is_record(Rec, tick) ->
	F = fun() ->
		mnesia:write(Rec)
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok;

add_ticks(List) when is_list(List) ->
	F = fun() ->
		lists:foreach( fun mnesia:write/1, List ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

get_ticks(Series_id) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(tick), element(1, P#tick.id) == Series_id ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

get_ticks(Series_id, {from, From}) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id,
			element(2, P#tick.id) >= From ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result;

get_ticks(Series_id, {to, To}) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id,
			element(2, P#tick.id) =< To ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result;

get_ticks(Series_id, {From, To}) ->
	F = fun() ->
		Q = qlc:q([ P || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id,
			element(2, P#tick.id) >= From,
			element(2, P#tick.id) =< To ]),
		qlc:e(Q)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

%% XXX not sure this function is needed
delete_tick(Id) ->
	F = fun() ->
		mnesia:delete({tick, Id})
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

delete_ticks(Series_id) ->
	F = fun() ->
		Q = qlc:q([ {tick, P#tick.id} || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

delete_ticks(Series_id, {from, From}) ->
	F = fun() ->
		Q = qlc:q([ {tick, P#tick.id} || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id,
			element(2, P#tick.id) >= From ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok;

delete_ticks(Series_id, {to, To}) ->
	F = fun() ->
		Q = qlc:q([ {tick, P#tick.id} || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id,
			element(2, P#tick.id) =< To ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok;

delete_ticks(Series_id, {From, To}) ->
	F = fun() ->
		Q = qlc:q([ {tick, P#tick.id} || P <- mnesia:table(tick),
			element(1, P#tick.id) == Series_id,
			element(2, P#tick.id) >= From,
			element(2, P#tick.id) =< To ]),
		lists:foreach( fun mnesia:delete/1, qlc:e(Q) ),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.


