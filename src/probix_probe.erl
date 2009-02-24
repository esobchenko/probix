-module(probix_probe).
-author('Eugen Sobchenko <eugen@sobchenko.com>').
-compile(export_all).

-include("probix.hrl").

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
	record_info(fields,probe).

read_all() ->
	probix_db:read_all(probe).

create(R) when is_record(R, probe) ->
	Id = probix_db:new_id(probe),
	Probe = R#probe{ id = Id },
	{atomic, ok} = probix_db:write(Probe),
	Probe.

read(Id) when is_integer(Id) ->
	case probix_db:read({object, Id}) of
		[ Object ] ->
			Object;
		[] ->
			throw({not_found, Id})
	end.
