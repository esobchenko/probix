-module(probix_http).
-export([start/1, stop/0, dispatch_requests/1, handle/5]).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

parse_format(Path) ->
	case string:tokens(Path, ".") of
		[ _Path ] ->
			json;
		%% add more data representation formats here
		[ _Path, "json" ] ->
			json;
		_Other ->
			unknown_format
	end.

	
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
	%% parse format from path
	Format = parse_format(Path),

	R = try
		unknown_format =:= Format andalso throw(
			probix_error:create(
				unknown_format,
				"you requested format that isn't supported yet or never will be"
			)
		),
		%% each handle function returns mochiweb's http response tuple
		handle(Format, Method, Splitted, Query, Post)
	catch
		%% unknown_format exception (it should be displayed in json)
		throw:Unknown = #error{code=unknown_format} -> error(json, Unknown);
		%% regular throw exceptions
		throw:Error when is_record(Error, error) -> error(Format, Error);
		%% erlang errors and other exceptions
		_Exception ->
			Error = probix_error:create(internal_error, "something bad happened"),
			error(Format, Error)
	end,
	Req:respond(R).

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

%% getting all probes for object limited by timestamp
handle(Format, 'GET', [ "object", Id_string, "probes" ], Args, _) ->
	Id = list_to_integer(Id_string),
	Output = probix_probe:output_handler_for(Format),
	Probes = case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
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

%% creating probes for object
handle(Format, 'POST', [ "object", Id_string, "probes" ], Args, Post) ->
	Id = list_to_integer(Id_string),
	Input = probix_probe:input_handler_for(Format),
	Output = probix_probe:output_handler_for(Format),
	Records = Input(Post),
	Result = probix_probe:create(Id, Records),
	%% return newly added probes using the "?return=1" in URI
	case proplists:get_value("return", Args) of
		"1" -> ok(Format, Output, Result);
		_Other -> ok()
	end;

handle(_, _, _, _, _) ->
	throw(
		probix_error:create(bad_request, "unknown request")
	).

%%
%% ok and error functions help to construct mochiweb's http response tuples;
%% they used in handle/5 functions.
%%

ok(json, Fun, Content) ->
	Response = Fun(Content),
	{200, [{"Content-Type", "application/json"}], Response}.

%% empty response
ok() ->
	{200, [{"Content-Type", "application/json"}], ""}.

error(json, Error) ->
	Fun = probix_error:output_handler_for(json),
	{http_code(Error), [{"Content-Type", "application/json"}], Fun(Error)}.

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

