-module(probix_rest).
-export([start/1, stop/0, dispatch_request/1]).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

start(Options) ->
    log4erl:info(http_logger, "~p", [Options]),
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_request/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_request(Req) ->
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
        log4erl:info(rest_logger, "Request-> Method: ~p, Path: ~p, Query: ~p, Post: ~p, Splitted: ~p", [Method, Path, Query, Post, Splitted]),
		handle(Method, Splitted, Query, Post)
	catch
		%% regular throw exceptions
		throw:Error when is_atom(Error) -> error(Error);

		%% erlang errors and other exceptions
		error:Exception ->
			log4erl:info(rest_logger, "exception caught: ~p", [Exception]),
			error(internal_error)
	end,
	Req:respond(R).

%% create series
handle('POST', ["series"], Args, Post) ->
	%% creating series
	Label = proplists:get_value("label", Args),

    {ok, Hostname} = application:get_env(probix, probix_rest_hostname),
	%% adding ticks if passed in json
	case probix_format:ticks_from_json(Post) of
		%% adding series with data
		{ok, Ticks} ->
			log4erl:info(rest_logger, "creating series"),
			Series = probix_series:new_series(Label),
			log4erl:info(rest_logger, "adding ticks to series: ~s", [ proplists:get_value(id, Series) ]),
			probix_series:add_ticks(proplists:get_value(id, Series), Ticks),
			redirect(Hostname ++ "/series/" ++ proplists:get_value(id, Series), probix_format:series_to_json([Series]));

		%% adding series without data
		{error, empty_json} ->
			log4erl:info(rest_logger, "creating series"),
			Series = probix_series:new_series(Label),
			redirect(Hostname ++ "/series/" ++ proplists:get_value(id, Series), probix_format:series_to_json([Series]));

		%% wrong data, doing nothing
		{error, Error} ->
			error(Error)
	end;

%% update series with data
handle('POST', ["series", Id], [], Post) ->
	log4erl:info(rest_logger, "updating series ~s", [ Id ]),
	probix_series:series(Id) == {error, not_found} andalso throw(not_found),

	case probix_format:ticks_from_json(Post) of
		{ok, Ticks} ->
			probix_series:add_ticks(Id, Ticks);
		{error, Error} ->
			throw(Error)
	end,
	ok();

%% get all existing series
handle('GET', ["series"], [], undefined) ->
	log4erl:info(rest_logger, "getting all series"),
	Series = probix_series:all_series(),
	Content = probix_format:series_to_json(Series),
	ok(Content);

handle('GET', ["series", Id], Args, undefined) ->
	probix_series:series(Id) == {error, not_found} andalso throw(not_found),

	Range = case [ proplists:get_value("from", Args), proplists:get_value("to", Args) ] of
		[undefined, undefined] ->
			log4erl:info(rest_logger, "selecting all data for series ~s", [ Id ]),
			{};

		[undefined, To] when is_list(To) ->
			log4erl:info(rest_logger, "selecting all data for series ~s, to: ~s", [Id, To]),
			case probix_format:parse_timestamp(To) of
				{ok, Ts} -> {to, Ts};
				_ -> throw(bad_arguments)
			end;

		[From, undefined] when is_list(From) ->
			log4erl:info(rest_logger, "selecting all data for series ~s, from: ~s", [Id, From]),
			case probix_format:parse_timestamp(From) of
				{ok, Ts} -> {from, Ts};
				_ -> throw(bad_arguments)
			end;

		[From, To] when is_list(From); is_list(To) ->
			log4erl:info(rest_logger, "selecting all data for series ~s, from: ~s, to: ~s", [Id, From, To]),
			case [probix_format:parse_timestamp(From), probix_format:parse_timestamp(To)] of
				[{ok, F}, {ok, T}] -> {F, T};
				_ -> throw(bad_arguments)
			end
	end,

	Ticks = probix_series:get_ticks(Id, Range),
	Content = probix_format:ticks_to_json(Ticks),
	ok(Content);


%% removing data from series
handle('DELETE', ["series", Id], Args, undefined) ->

	probix_series:series(Id) == {error, not_found} andalso throw(not_found),

	Range = case [ proplists:get_value("from", Args), proplists:get_value("to", Args) ] of
		[undefined, undefined] ->
			log4erl:info(rest_logger, "deleting all data for series ~s", [ Id ]),
			{};

		[undefined, To] when is_list(To) ->
			log4erl:info(rest_logger, "deleting all data for series ~s, to: ~s", [Id, To]),
			case probix_format:parse_timestamp(To) of
				{ok, Ts} -> {to, Ts};
				_ -> throw(bad_arguments)
			end;

		[From, undefined] when is_list(From) ->
			log4erl:info(rest_logger, "deleting all data for series ~s, from: ~s", [Id, From]),
			case probix_format:parse_timestamp(From) of
				{ok, Ts} -> {from, Ts};
				_ -> throw(bad_arguments)
			end;

		[From, To] when is_list(From); is_list(To) ->
			log4erl:info(rest_logger, "deleting all data for series ~s, from: ~s, to: ~s", [Id, From, To]),
			case [probix_format:parse_timestamp(From), probix_format:parse_timestamp(To)] of
				[{ok, F}, {ok, T}] -> {F, T};
				_ -> throw(bad_arguments)
			end
	end,

	probix_series:delete_ticks(Id, Range),
	ok();

handle('OPTIONS', _Any, _Query, _Post) ->
    ok("");

handle(_, _, _, _) ->
	error(bad_request).

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

% redirect(Location) ->
% {301, [{"Location", Location}], ""}.

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
