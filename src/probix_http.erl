-module(probix_http).
-export([start/1, stop/0, dispatch_requests/1, handle/4]).

-include_lib("eunit/include/eunit.hrl").

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	Path = Req:get(path),
	Splitted = string:tokens(Path,"/"),
	Method = Req:get(method),
	Post = Req:recv_body(),
	Query = Req:parse_qs(),
%	io:format("~p request for ~p with post: ~p, query: ~p~n", [Method, Splitted, Post, Query]),

	try 
		Response = handle(Method, Splitted, Query, Post),
		Req:respond(Response)
	catch 
		{not_found, {_Object, _Id}} -> 
			Req:respond(probix_error:response_error_as(
						  json, 404, {Path, "no object with that id found"})
		);
		{bad_input, _Message} -> 
			Req:respond(probix_error:response_error_as(
						  json, 400, {Path, "bad input"})
		);
		_Exception -> 
			Req:respond(probix_error:response_error_as(
						  json, 500, {Path, "something bad happened"})
		)
    end.
	

handle('GET', ["objects"], _,  _) ->
	Objects = probix_object:read_all_as(json),
	{200, [{"Content-Type", "text/json"}], Objects};

handle('GET', ["object", Id_string], _, _) ->
	Id = list_to_integer(Id_string),
	Json = probix_object:read_as(json, Id),
	{200, [{"Content-Type", "text/json"}], Json};

handle('PUT', ["object", Id_string], _, Post) ->
	Id = list_to_integer(Id_string),
	{200, [{"Content-Type", "text/json"}], probix_object:update_from(json, Id, Post)};

handle('DELETE', ["object", Id_string], _, _) ->
	Id = list_to_integer(Id_string),
	Res = probix_object:delete(Id),
	{200, [{"Content-Type", "text/json"}], integer_to_list(Res)};

handle('POST', ["object"], _, Post) ->
	Json = probix_object:create_from(json, Post),
	{200, [{"Content-Type", "text/json"}], Json};

%% Getting all probes for object limited by timestamp
handle('GET', [ "object", Id_string, "probes" ], Args, _) ->
	Id = list_to_integer(Id_string),
	
	case [proplists:get_value("from", Args), 
		  proplists:get_value("to", Args)] of
		[undefined, undefined] ->
			Probes = probix_probe:probes_by_object_id_as(json, Id);
		[undefined, To] ->
			Probes = probix_probe:probes_by_object_id_as(json, Id, {to, list_to_integer(To)});
		[From, undefined] ->
			Probes = probix_probe:probes_by_object_id_as(json, Id, {from, list_to_integer(From)});
		[From, To] ->
			Probes = probix_probe:probes_by_object_id_as(json, Id, list_to_integer(From), list_to_integer(To))
	end,
	{200, [{"Content-Type", "text/json"}], Probes};

%% Creating list of probes for object
handle('POST', [ "object", Id_string, "probes"], _, Post) ->
	Id = list_to_integer(Id_string),
	probix_object:read(Id),
	Json = probix_probe:create_for_object_from(json, Id, Post),
	{200, [{"Content-Type", "text/json"}], Json};

handle(_, Path, _,  _) -> probix_error:response_error_as( json, 400, {Path, "unknown request"} ).



