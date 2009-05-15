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

get_message_for_code(Code) ->
	Message = case Code of
		'NOT_FOUND' ->
			"object with specified doesn't exist";
		'OTHER_CODE' ->
			"something happened";
		'BAD_REQUEST' ->
			"please write message for me";
		'BAD_INPUT' ->
			"hey! Watcha sending to me?";
		'UNKNOWN_FORMAT' ->
			"are you okay?"
	end,
	list_to_binary(Message).

create(Method, Url, Error) ->
	#error{
		method = Method,
		url = list_to_binary(Url),
		error_code = Error,
		error_message = get_message_for_code(Error)
	}.
