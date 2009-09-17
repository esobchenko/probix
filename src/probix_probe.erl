-module(probix_probe).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% acceptable value type checking functions
acceptable_value(none) ->
	true;
acceptable_value({_K, V}) when is_integer(V) ->
	true;
acceptable_value(_Pair) ->
	false.

required_fields() ->
	[timestamp, value].

record_name() ->
	probe.

record_fields() ->
	record_info(fields, probe).

%% primary method to create probes
create(Id_object, List) when is_list(List), is_integer(Id_object) ->
	F = fun() ->
		probix_object:read(Id_object), %% foreign key constraint check
		Table = probix_object:oid2table(Id_object),
		probix_db:create(Table, List)
	end,
	probix_db:transaction(F); %% returns list of newly created probes on success

create(Id_object, R) when is_record(R, probe), is_integer(Id_object) ->
	F = fun() ->
		probix_object:read(Id_object), %% foreign key constraint check
		Table = probix_object:oid2table(Id_object),
		probix_db:create(Table, R)
	end,
	probix_db:transaction(F). %% returns list of newly created probes on success

probes_by_object_id(Id) when is_integer(Id) ->
	F = fun() ->
		probix_object:read(Id), %% foreign key constraint check
		Table = probix_object:oid2table(Id),
		probix_db:read_all(Table)
	end,
	probix_db:transaction(F).

probes_by_object_id(Id, {to, To}) ->
	F = fun() ->
		probix_object:read(Id), %% foreign key constraint check
		Table = probix_object:oid2table(Id),
		Q = qlc:q(
			[ P ||
				P <- mnesia:table(Table),
				P#probe.timestamp =< To
			]
		),
		probix_db:find(Q)
	end,
	probix_db:transaction(F);

probes_by_object_id(Id, {from, From}) ->
	F = fun() ->
		probix_object:read(Id), %% foreign key constraint check
		Table = probix_object:oid2table(Id),
		Q = qlc:q(
			[ P ||
				P <- mnesia:table(Table),
				P#probe.timestamp >= From
			]
		),
		probix_db:find(Q)
	end,
	probix_db:transaction(F).

probes_by_object_id(Id, From, To) ->
	F = fun() ->
		probix_object:read(Id), %% foreign key constraint check
		Table = probix_object:oid2table(Id),
		Q = qlc:q(
			[ P ||
				P <- mnesia:table(Table),
				P#probe.timestamp >= From,
				P#probe.timestamp =< To
			]
		),
		probix_db:find(Q)
	end,
	probix_db:transaction(F).

