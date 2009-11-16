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
		throw:Error when is_atom(Error) -> error(Error);

		%% erlang errors and other exceptions
		_Exception ->
			log4erl:info("Exception: ~p", [_Exception]),
			error(internal_error)
	end,
	Req:respond(R).

%% create series
handle('POST', ["series"], Args, Post) ->
	%% creating series
	Label = proplists:get_value("label", Args),

	%% adding ticks if passed in json
	case probix_format:ticks_from_json(Post) of
		%% adding series with data
		{ok, Ticks} ->
			log4erl:info("creating series"),
			Series = probix_series:new_series(Label),
			log4erl:info("adding ticks to series: ~p", [ Series#series.id ]),
			probix_series:add_ticks(Series#series.id, Ticks),
			redirect("/series/" ++ Series#series.id);

		%% adding series without data
		{error, empty_json} ->
			log4erl:info("creating series"),
			Series = probix_series:new_series(Label),
			redirect("/series/" ++ Series#series.id);

		%% wrong data, doing nothing
		{error, Error} ->
			error(Error)
	end;

%% update series with data
handle('POST', ["series", Id], [], Post) ->
	log4erl:info("updating series ~s", [ Id ]),
	probix_series:series(Id) == {error, not_found} andalso throw(not_found),

	case probix_format:ticks_from_json(Post) of
		{ok, Ticks} ->
			probix_series:add_ticks(Id, Ticks);
		{error, Error} ->
			error(Error)
	end,
	ok();

%% get all existing series
handle('GET', ["series"], [], undefined) ->
	log4erl:info("getting all series"),
	Series = probix_series:all_series(),
	Content = probix_format:series_to_json(Series),
	ok(Content);

handle('GET', ["series", Id], Args, undefined) ->
	probix_series:series(Id) == {error, not_found} andalso throw(not_found),

	From = proplists:get_value("from", Args) =:= undefined
		orelse probix_format:parse_timestamp( proplists:get_value("from", Args) ),

	To = proplists:get_value("to", Args) =:= undefined
		orelse probix_format:parse_timestamp( proplists:get_value("from", Args) ),

	case [ From, To ] of
		[true, true] ->
			log4erl:info("selecting all data for series ~s", [ Id ]),
			Ticks = probix_series:get_ticks(Id),
			Content = probix_format:ticks_to_json(Ticks),
			ok(Content);

		[true, {ok, To}] when is_record(To, timestamp) ->
			log4erl:info("selecting all data for series ~s, to: ~p", [Id, To]),
			Ticks = probix_series:get_ticks(Id, {to, To}),
			Content = probix_format:ticks_to_json(Ticks),
			ok(Content);

		[{ok, From}, true] when is_record(From, timestamp) ->
			log4erl:info("selecting all data for series ~s, from: ~p", [Id, From]),
			Ticks = probix_series:get_ticks(Id, {from, From}),
			Content = probix_format:ticks_to_json(Ticks),
			ok(Content);

		[{ok, From}, {ok, To}] when is_record(From, timestamp); is_record(To, timestamp) ->
			log4erl:info("selecting all data for series ~s, from: ~p, to: ~p", [Id, From, To]),
			probix_format:parse_timestamp(To),
			Ticks = probix_series:get_ticks(Id, {From, To}),
			Content = probix_format:ticks_to_json(Ticks),
			ok(Content);

		_ ->
			error(bad_arguments)
	end;

%% removing data from series
handle('DELETE', ["series", Id], Args, undefined) ->

	probix_series:series(Id) == {error, not_found} andalso throw(not_found),

	From = proplists:get_value("from", Args) =:= undefined
		orelse probix_format:parse_timestamp( proplists:get_value("from", Args) ),

	To = proplists:get_value("to", Args) =:= undefined
		orelse probix_format:parse_timestamp( proplists:get_value("from", Args) ),

	case [From, To] of
		%% empty args
		[true, true] ->
			log4erl:info("deleting series with id: ~s", [ Id ]),
			probix_series:delete_series(Id),
			ok();

		[true, {ok, To}] when is_record(To, timestamp) ->
			log4erl:info("deleting data for series ~s, to: ~p", [Id, To]),
			probix_series:delete_ticks(Id, {to, To}),
			ok();

		[{ok, From}, true] when is_record(From, timestamp) ->
			log4erl:info("deleting data for series ~s, from: ~p", [Id, From]),
			probix_series:delete_ticks(Id, {from, From}),
			ok();

		[{ok, From}, {ok, To}] when is_record(From, timestamp); is_record(To, timestamp) ->
			log4erl:info("deleting data for series ~s, from: ~p, to: ~p", [Id, From, To]),
			probix_series:delete_ticks(Id, {From, To}),
			ok();

		_ ->
			error("bad_arguments")
	end;

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
	{200, [{"Content-Type", "application/json"}], Content}.

error(Error) ->
	log4erl:error(Error),
	{http_code(Error), [], ""}.

redirect(Location) ->
	{301, [{"Location", Location}], ""}.

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
