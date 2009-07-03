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

create(Code, Message) when is_binary(Message) ->
	#error{
		code = Code,
		message = Message
	};

create(Code, Message) when is_list(Message) -> create(Code, list_to_binary(Message)).

