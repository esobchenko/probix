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


%% returns http numeric response code 
%% for error record
get_http_code(Error) when is_record(Error, error) ->
	case Error#error.code of
		not_found ->
			404;
		unknown_format ->
			406;
		internal_error ->
			500;
		_Other ->
			400
	end.

add_http_values(Error, Method, Url) ->
	Tmp = Error#error{url = list_to_binary(Url)},
	Tmp#error{method = Method}.

create(Error_Code, Message) ->
	#error{
		code = Error_Code,
		message = list_to_binary(Message)
	}.
