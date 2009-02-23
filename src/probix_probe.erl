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


