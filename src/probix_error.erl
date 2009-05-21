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

%% output handler is used by http module to convert
%% from erlang record to given representation format.
output_handler_for(json) ->
	fun(Data) ->
			probix_utils:record_to_json(Data, ?MODULE)
	end.

create(Code, Message) when is_binary(Message) ->
	#error{
		code = Code,
		message = Message
	};

create(Code, Message) when is_list(Message) -> create(Code, list_to_binary(Message)).

