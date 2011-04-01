-module(probix_http).

-export([dispatch_request/2, ok/0, ok/1, error/1, redirect/2]).

% common methods for mochiweb servers
dispatch_request(Req, Module) ->
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
        log4erl:debug(http_logger, "~p: ~p ~p -> Query: ~p, Post: ~p", [Module, Method, Path, Query, Post]),
		 Module:handle(Method, Splitted, Query, Post)
	catch
		%% regular throw exceptions
		throw:Error when is_atom(Error) -> error(Error);

		%% erlang errors and other exceptions
		error:Exception ->
			log4erl:info(rest_logger, "exception caught: ~p", [Exception]),
			error(internal_error)
	end,
	Req:respond(R).


%%
%% ok and error functions help to construct mochiweb's http response tuples;
%% they used in handle/5 functions.
%%

%% empty response
ok() ->
	{200, [{"Content-Type", "application/json"}], ""}.

ok(Content) ->
	{200, [ { "Access-Control-Allow-Origin", "*" },
            { "Access-Control-Allow-Methods", "OPTIONS, GET, POST, PUT, DELETE" },
            { "Access-Control-Allow-Headers", "Accept, X-Requested-With" },
            {"Content-Type", "application/json"},
            {"Cache-Control", "no-cache"}], Content }.

error(Error) ->
	log4erl:error(rest_logger, Error),
	{http_code(Error), [], ""}.

redirect(Location, Content) ->
	{301, [{"Location", Location}], Content}.

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
