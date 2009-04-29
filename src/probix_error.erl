-module(probix_error).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% acceptable value type checking functions
acceptable_value(none) ->
	true;
acceptable_value({_K, V}) when is_binary(V) ->
	true;
acceptable_value(_Pair) ->
	false.

required_fields() ->
	[request, error].

record_name() ->
	error.

record_fields() ->
	record_info(fields, error).

create_as(json, {Request, Error}) when is_list(Request); is_list(Error) ->
	R = #error{request = list_to_binary(Request), error = list_to_binary(Error)},
	probix_utils:record_to_json(R, ?MODULE).

%% response_error_as/3 returns an argument for mochiweb's Req:respond/1 fun
response_error_as(json, Status, Error) when is_tuple(Error) ->
	{Status, [{"Content-Type", "text/json"}], create_as(json, Error)}.

