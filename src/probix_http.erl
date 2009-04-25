-module(probix_http).
-export([start/1, stop/0, dispatch_requests/1]).

-include_lib("eunit/include/eunit.hrl").

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	Path = Req:get(path),
	Method = Req:get(method),
	Post = Req:recv_body(),
%%	io:format("~p request for ~p with post: ~p~n", [Method, Path, Post]),
	Response = handle(Method, Path, Post),
	Req:respond(Response).

handle('GET', "/objects", _) ->
	try probix_object:read_all_as_json() of
		Objects -> {200, [{"Content-Type", "text/json"}], Objects}
	catch
		_Exception -> probix_error:http_error( 500, "/objects", "something bad happened" )
	end;

handle('GET', "/object/" ++ Id_string, _) ->
	Id = list_to_integer(Id_string),
	try probix_object:read_as_json(Id) of
		Json -> {200, [{"Content-Type", "text/json"}], Json}
	catch
		{not_found, Id} -> probix_error:http_error(
			404, "/object/" ++ Id_string, "no object with that id found"
		);
		_Exception -> probix_error:http_error(
			500, "/object/" ++ Id_string, "something bad happened"
		)
	end;

handle('PUT', "/object/" ++ Id_string, Post) ->
	Id = list_to_integer(Id_string),
	try 
		probix_object:read(Id),
		{200, [{"Content-Type", "text/json"}], probix_object:update_from_json(Id, Post)}
	catch 
		{not_found, Id} -> probix_error:http_error(
			404, "/object/" ++ Id_string, "no object with that id found" 
		);
		{bad_input, _Message} -> probix_error:http_error( 
			400, "/object/" ++ Id_string, "bad input"
		);
		_Exception -> probix_error:http_error( 
			500, "/object/" ++ Id_string, "something bad happened" 
		)
	end;

handle('DELETE', "/object/" ++ Id_string, _) ->
	Id = list_to_integer(Id_string),
	try probix_object:read(Id) of
		_Object ->
			{200, [{"Content-Type", "text/json"}], integer_to_list(probix_object:delete(Id))}
	catch
		{not_found, Id} -> probix_error:http_error(
			404, "/object/" ++ Id_string, "no object with that id found"
		)
	end;

handle('POST', "/object", Post) ->
	try probix_object:create_from_json(Post) of
		Json -> {200, [{"Content-Type", "text/json"}], Json}
	catch
		{bad_input, _Message} -> probix_error:http_error(
			400, "/object", "bad input"
		);
		_Exception -> probix_error:http_error(
			500, "/object", "something bad happened"
		)
	end;

handle(_, Path, _) -> probix_error:http_error( 400, Path, "unknown request" ).


