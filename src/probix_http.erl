-module(probix_http).
-export([start/1, stop/0, dispatch_requests/1, handle/4]).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	%% http method
	Method = Req:get(method),
	%% uri path
	Path = Req:get(path),
	%% uri query string as proplist
	Query = Req:parse_qs(),
	%% post body
	Post = Req:recv_body(),
	%% split path string for handy request handling
	Splitted = string:tokens(Path, "/"),

	R = try
        handle(Method, Splitted, Query, Post)
	catch	
		%% regular throw exceptions
		throw:Error when is_record(Error, error) -> error(Error);
		%% erlang errors and other exceptions
		_Exception ->
			Error = probix_error:create(internal_error, "something bad happened"),
			error(Error)
	end,
	Req:respond(R).

handle('GET', ["objects"], _,  _) ->
	Objects = probix_object:read_all(),
	Output = probix_utils:record_to_json(Objects, probix_object),
	ok(Output);

handle('GET', ["object", Id_string], _, _) ->
	Id = to_integer(Id_string),
	Object = probix_object:read(Id), 
	Output = probix_utils:record_to_json(Object, probix_object),
	ok(Output);

handle('PUT', ["object", Id_string], _, Post) ->
	Id = to_integer(Id_string),
	Record = probix_utils:json_to_record(Post, probix_object),
	Result = probix_object:update(Id, Record),
	Output = probix_utils:record_to_json(Result, probix_object),
	ok(Output);

handle('DELETE', ["object", Id_string], _, _) ->
	Id = to_integer(Id_string),
	probix_object:delete(Id),
	ok();

handle('POST', ["object"], _, Post) ->
	Record = probix_utils:json_to_record(Post, probix_object),
	Result = probix_object:create(Record),
	Output = probix_utils:record_to_json(Result, probix_object),
	ok(Output);

%% to get probes for object by timestamp
handle('GET', [ "object", Id_string, "probes" ], Args, _) ->
	Id = to_integer(Id_string),
	Probes = case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
		[undefined, undefined] ->
			probix_probe:probes_by_object_id(Id);
		[undefined, To] when is_list(To) ->
			probix_probe:probes_by_object_id(
				Id,
				{to, to_integer(To)}
			);
		[From, undefined] when is_list(From) ->
			probix_probe:probes_by_object_id(
				Id,
				{from, to_integer(From)}
			);
		[From, To] when is_list(From); is_list(To) ->
			probix_probe:probes_by_object_id(
				Id,
				to_integer(From),
				to_integer(To)
			)
	end,
	Output = probix_utils:record_to_json(Probes, probix_probe),
	ok(Output);

%% creating probes for object
handle('POST', [ "object", Id_string, "probes" ], Args, Post) ->
	Id = to_integer(Id_string),
	Records = probix_utils:json_to_record(Post, probix_probe),
	Result = probix_probe:create(Id, Records),

	%% return newly added probes using the "?return=1" in URI
	case proplists:get_value("return", Args) of
		"1" -> 	Output = probix_utils:record_to_json(Result, probix_probe),
                ok(Output);
		_Other -> ok()
	end;

handle(_, _, _, _) ->
	throw(
		probix_error:create(bad_request, "unknown request")
	).

%%
%% ok and error functions help to construct mochiweb's http response tuples;
%% they used in handle/5 functions.
%%

ok(Content) ->
	{200, [{"Content-Type", "application/json"}], Content}.

%% empty response
ok() ->
	{200, [{"Content-Type", "application/json"}], ""}.

error(Error) ->
	{http_code(Error), [{"Content-Type", "application/json"}], 
     probix_utils:record_to_json(Error, probix_error)}.

%% returns http numeric response code for
%% given error according to specification
http_code(Error) when is_record(Error, error) ->
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

%% this function is used to convert some strings to integers (e.g. object id) because
%% bad_request exception must be raised to inform user about the problem.
to_integer(L) when is_list(L) ->
	try erlang:list_to_integer(L)
	catch
		error:badarg -> throw(
			probix_error:create(bad_request, "got string where integer is expected")
		)
	end.

