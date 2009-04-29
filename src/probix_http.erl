-module(probix_http).
-export([start/1, stop/0, dispatch_requests/1]).

-include_lib("eunit/include/eunit.hrl").

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	[_Slash | Path ] = re:split(Req:get(path),"/",[notempty, trim]),
	Method = Req:get(method),
	Post = Req:recv_body(),
	Query = Req:parse_qs(),
%	io:format("~p request for ~p with post: ~p, query: ~p~n", [Method, Path, Post, Query]),
	Response = handle(Method, Path, Query, Post),
	Req:respond(Response).

handle('GET', [<<"objects">>], _,  _) ->
	try probix_object:read_all_as(json) of
		Objects -> {200, [{"Content-Type", "text/json"}], Objects}
	catch
		_Exception -> probix_error:response_error_as(json, 500, {"/objects", "something bad happened"})
	end;

handle('GET', [<<"object">>, Id_string], _, _) ->
	Id = list_to_integer(binary_to_list(Id_string)),
	try probix_object:read_as(json, Id) of
		Json -> {200, [{"Content-Type", "text/json"}], Json}
	catch
		{not_found, {_, Id}} -> probix_error:response_error_as(
			json, 404, {"/object/" ++ Id_string, "no object with that id found"}
		);
		_Exception -> probix_error:response_error_as(
			json, 500, {"/object/" ++ Id_string, "something bad happened"}
		)
	end;

handle('PUT', [<<"object">>, Id_string], _, Post) ->
	Id = list_to_integer(binary_to_list(Id_string)),
	try 
		{200, [{"Content-Type", "text/json"}], probix_object:update_from(json, Id, Post)}
	catch 
		{not_found, {_, Id}} -> probix_error:response_error_as(
			json, 404, {"/object/" ++ Id_string, "no object with that id found"}
		);
		{bad_input, _Message} -> probix_error:response_error_as(
			json, 400, {"/object/" ++ Id_string, "bad input"}
		);
		_Exception -> probix_error:response_error_as(
			json, 500, {"/object/" ++ Id_string, "something bad happened"}
		)
	end;

handle('DELETE', [<<"object">>, Id_string], _, _) ->
	Id = list_to_integer(binary_to_list(Id_string)),
	try 
		Res = probix_object:delete(Id),
		{200, [{"Content-Type", "text/json"}], integer_to_list(Res)}
	catch
		{not_found, {_, Id}} -> probix_error:response_error_as(
			json, 404, {"/object/" ++ Id_string, "no object with that id found"}
		)
	end;

handle('POST', [<<"object">>], _, Post) ->
	try probix_object:create_from(json, Post) of
		Json -> {200, [{"Content-Type", "text/json"}], Json}
	catch
		{bad_input, _Message} -> probix_error:response_error_as(
			json, 400, {"/object", "bad input"}
		);
		_Exception -> probix_error:response_error_as(
			json, 500, {"/object", "something bad happened"}
		)
	end;

handle(_, Path, _,  _) -> probix_error:response_error_as( json, 400, {Path, "unknown request"} ).



