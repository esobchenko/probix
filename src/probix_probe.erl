-module(probix_probe).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% acceptable value type checking functions
acceptable_value(none) ->
	true;
acceptable_value({_K, V}) when is_binary(V); is_integer(V) ->
	true;
acceptable_value(_Pair) ->
	false.

required_fields() ->
	[id_object, timestamp, value].

record_name() ->
	probe.

record_fields() ->
	record_info(fields, probe).

probes_by_object_id(Id) when is_integer(Id) ->
	Q = qlc:q(
		[ P || P <- mnesia:table(probe), P#probe.id_object =:= Id ]
	),
	probix_db:find(Q).

probes_by_object_id_as_json(Id) when is_integer(Id) ->
	probix_utils:list_to_json(probes_by_object_id(Id), ?MODULE).

create_from_json(Json) ->
	R = probix_utils:json_to_record(Json, ?MODULE),
	Probe = create(R),
	probix_utils:record_to_json(Probe, ?MODULE).

create(R) when is_record(R, probe) ->
	Id = probix_db:new_id(probe),
	Probe = R#probe{ id = Id },
	{atomic, ok} = probix_db:write(Probe),
	Probe.

read_as_json(Id) when is_integer(Id) ->
	Probe = read(Id),
	probix_utils:record_to_json(Probe, ?MODULE).

read(Id) when is_integer(Id) ->
	case probix_db:read({probe, Id}) of
		[ Probe ] ->
			Probe;
		[] ->
			throw({not_found, Id})
	end.

delete(Id) when is_integer(Id) ->
	{atomic, ok} = probix_db:delete({probe, Id}),
	Id.


