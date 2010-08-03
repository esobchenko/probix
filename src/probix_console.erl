-module(probix_console).
-export([start/1, stop/0, dispatch_requests/1, handle/5]).

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

	try
        log4erl:info("Request-> Method: ~p, Path: ~p, Query: ~p, Post: ~p, Splitted: ~p", [Method, Path, Query, Post, Splitted]),
		handle(Req, Method, Splitted, Query, Post)
	catch
		%% regular throw exceptions
		throw:Error when is_atom(Error) -> error(Error);

		%% erlang errors and other exceptions
		error:Exception ->
			log4erl:info("exception caught: ~p", [Exception]),
			error(internal_error)
	end.

handle(Req, 'GET', [], _Query, _Post) ->
    {ok, Docroot} = application:get_env(probix, probix_docroot),
    Req:serve_file("index.html", Docroot);

handle(Req, 'GET', ["series", _Id], [], undefined) ->
    {ok, Docroot} = application:get_env(probix, probix_docroot),
    Req:serve_file("series.html", Docroot);

handle(Req, 'GET', FilePath, _Query, _Post) ->   
    {ok, Docroot} = application:get_env(probix, probix_docroot),
    Req:serve_file(FilePath, Docroot);

handle(Req, _, _, _, _) ->
    Req:respond(ok("Foo")).

error(Error) ->
	log4erl:error(Error),
	{http_code(Error), [], ""}.

ok(Content) ->
	{200, [{"Content-Type", "text/html"}], Content}.

%% returns http numeric response code for
%% given error according to specification
http_code(Error) when is_atom(Error) ->
	case Error of
		not_found ->
			404;
		unknown_format ->
			406;
		internal_error ->
			500;
		_Other ->
			400
	end.
