-module(probix_http).
-export([start/1,stop/0,dispatch_requests/1]).

-include_lib("eunit/include/eunit.hrl").

%% start() ->
%%	mochiweb_http:start([{port, 8888},{loop, fun dispatch_requests/1}]).

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	Path = Req:get(path),
	Method = Req:get(method),
	Post = Req:recv_body(),
	io:format("~p request for ~p with post: ~p~n", [Method, Path, Post]),
	Response = handle(Method, Path, Post),
	Req:respond(Response).

handle('GET', "/objects", _) ->
	try probix_object:read_all_as_json() of 
		Objects -> {200, [], Objects}
	catch
		Exception ->
			{400, [], Exception}
	end;

handle('GET', "/object/" ++ IdString, _) ->
	Id = list_to_integer(IdString),
	try probix_object:read_as_json(Id) of
		Json -> {200, [], Json}
	catch 
		{not_found,Id} ->
			{404, [], "Object with ID: " ++ IdString ++ " not found"};
		  _Exception  ->
			{500, [], "Something happened"}
	end;

handle('PUT', "/object/" ++ Id, Post) ->
	try probix_object:update_from_json(list_to_integer(Id),Post) of
		Json -> {200, [], Json}
	catch
		Exception ->
			{400, [], Exception}
	end;

handle('DELETE', "/object/" ++ IdString, _) ->
	Id = list_to_integer(IdString),
	try probix_object:read(Id) of
		_Object ->
			{200, [], integer_to_list(probix_object:delete(Id))}
	catch
		{not_found, Id} ->
			{404, [], "Object with ID: " ++ IdString ++ " not found"}
	end;

handle('POST', "/object", Post) ->
	try probix_object:create_from_json(Post) of
		Json -> {200, [], Json}
	catch
		%% todo - differentiate bad json and internal error
		error:_Any -> {400, [], "Bad request"};
		  
		_Other -> {500, [], "Something happened"}
	end;

handle(_, _, _) ->
	{400, [{"Content-Type", "text/plain"}], <<"Unknown Request">>}.
