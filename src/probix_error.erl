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

%%
%% create/2 with different Request and Error types can be implemented here.
%%

create(Request, Error) when is_list(Request); is_list(Error) ->
	R = #error{request = list_to_binary(Request), error = list_to_binary(Error)},
	probix_utils:record_to_json(R, ?MODULE).

http_error(Status, Request, Error) ->
	{Status, [{"Content-Type", "text/json"}], create(Request, Error)}.

