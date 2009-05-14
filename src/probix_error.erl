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

output_handler_for(json) ->
	fun(Data) ->
			probix_utils:record_to_json(Data, ?MODULE)
	end.

get_message_for_code(Code) ->
	case Code of 
		'OBJECT_NOT_FOUND' ->
			"Object with specified doesn't exist";
		'OTHER_CODE' ->
			"Something happened";
		'BAD_REQUEST' ->
			"Please write message for me";
		'BAD_INPUT' ->
			"Hey! Watcha sending to me?";
		'UNKNOWN_FORMAT' ->
			"Are you okay?"
	end.

create(Method, Url, Error) ->
	#error{method = Method, 
		   url = Url,
		   error_code = Error,
		   error_message = get_message_for_code(Error)
		  }.
