-module(probix_response).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

response_content_as(json, Fun, Content) ->
	Response = Fun(Content),
	{200, [{"Content-Type", "text/json"}], Response};

response_content_as(xml, _Fun, _Content) ->
	not_implemented.

response_error_as(json, Code, Content) ->
	Fun = probix_error:output_handler_for(json),
	Response = Fun(Content),
	{Code, [{"Content-Type", "text/json"}], Response};

response_error_as(xml, _Code, _Content) ->
	not_implemented.
