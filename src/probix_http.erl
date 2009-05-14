-module(probix_http).
-export([start/1, stop/0, dispatch_requests/1, handle/5]).

-include_lib("eunit/include/eunit.hrl").

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	Path = Req:get(path),
	
	%% defining format
	Format = case string:tokens(Path, ".") of
				 [ _Path ] ->
					 json;
				 %% feel free to add more formats here
				 [ _Path, "json" ] ->
					 json;
				 _Other ->
					 throw({bad_format, "Bad format"})
			 end,

	%% preparing all required data
	Splitted = string:tokens(Path, "/"),
	Method = Req:get(method),
	Post = Req:recv_body(),
	Query = Req:parse_qs(),

	%%	io:format("~p request for ~p with post: ~p, query: ~p~n", [Method, Splitted, Post, Query]),

	try
		Response = handle(Format, Method, Splitted, Query, Post),
		Req:respond(Response)
	catch
		{not_found, {_Object, _Id}} ->
			Error = probix_error:create(Method, Path, 'OBJECT_NOT_FOUND'),
			Req:respond(
			  error(Format, 404, Error )
			 );
		  {bad_input, _Message} ->
			Error = probix_error:create(Method, Path, 'BAD_INPUT'),
			Req:respond(
			  error(Format, 400, Error)
			 );
		  {bad_request, _Message} ->
			Error = probix_error:create(Method, Path, 'BAD_REQUEST'),
			Req:respond(
			  error( Format, 400, Error ) );
		  {bad_format, _Message} ->
			Error = probix_error:create(Method, Path, 'UNKNOWN_FORMAT'),
			Req:respond(
			  error( Format, 406, Error ) );
		  _Exception ->
			Error = probix_error:create(Method, Path, 'INTERNAL_ERROR'),
			Req:respond(
			  error( Format, 500, Error)
			 )
	end.


handle(Format, 'GET', ["objects"], _,  _) ->
	Objects = probix_object:read_all(),
	Output = probix_object:output_handler_for(Format),
	ok(Format, Output, Objects);

handle(Format, 'GET', ["object", Id_string], _, _) ->
	Id = list_to_integer(Id_string),
	Object = probix_object:read(Id), 
	Output = probix_object:output_handler_for(Format),
	ok(Format, Output, Object);

handle(Format, 'PUT', ["object", Id_string], _, Post) ->
	Id = list_to_integer(Id_string),
	Input = probix_object:input_handler_for(Format),
	Output = probix_object:output_handler_for(Format),
	Record = Input(Post),
	Result = probix_object:update(Id, Record),	
	ok(Format, Output, Result);

handle(_Format, 'DELETE', ["object", Id_string], _, _) ->
	Id = list_to_integer(Id_string),
	probix_object:delete(Id),
	ok();

handle(Format, 'POST', ["object"], _, Post) ->
	Input = probix_object:input_handler_for(Format),
	Record = Input(Post),
	Output = probix_object:output_handler_for(Format),
	Result = probix_object:create(Record),
	ok(Format, Output, Result);

%% Getting all probes for object limited by timestamp
handle(Format, 'GET', [ "object", Id_string, "probes" ], Args, _) ->
	Id = list_to_integer(Id_string),
	Output = probix_probe:output_handler_for(Format),
	Probes = case [proplists:get_value("from", Args), 
		  proplists:get_value("to", Args)] of
		[undefined, undefined] ->
			probix_probe:probes_by_object_id(Id);
		[undefined, To] ->
			probix_probe:probes_by_object_id(Id, {to, list_to_integer(To)});
		[From, undefined] ->
			probix_probe:probes_by_object_id(Id, {from, list_to_integer(From)});
		[From, To] ->
			probix_probe:probes_by_object_id(Id, list_to_integer(From), list_to_integer(To))
	end,
	ok(Format, Output, Probes);

%% Creating list of probes for object
handle(Format, 'POST', [ "object", Id_string, "probes"], _, Post) ->
	Id = list_to_integer(Id_string),
	Input = probix_probe:input_handler_for(Format),
	Output = probix_probe:output_handler_for(Format),
	Records = Input(Post),
	Result = probix_probe:create(Id, Records),
	ok(Format, Output, Result);

handle(_, _, _, _,  _) -> 
	throw({bad_request, "unknown request"}).


ok(json, Fun, Content) ->
	Response = Fun(Content),
	{200, [{"Content-Type", "text/json"}], Response};

ok(xml, _Fun, _Content) ->
	not_implemented.

ok() ->
	{200, [{"Content-Type", "text/plain"}], ""}.

error(json, Code, Content) ->
	Fun = probix_error:output_handler_for(json),
	Response = Fun(Content),
	{Code, [{"Content-Type", "text/json"}], Response};

error(xml, _Code, _Content) ->
	not_implemented.


