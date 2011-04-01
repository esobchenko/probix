-module(probix_rest).
-export([start/1, stop/0, dispatch_request/1]).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

start(Options) ->
    log4erl:info(http_logger, "~p", [Options]),
	mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, dispatch_request}} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_request(Req) ->
    probix_http:dispatch_request(Req, ?MODULE).

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
			probix_http:redirect(Hostname ++ "/series/" ++ proplists:get_value(id, Series), probix_format:series_to_json([Series]));

		%% adding series without data
		{error, empty_json} ->
			log4erl:info(rest_logger, "creating series"),
			Series = probix_series:new_series(Label),
			probix_http:redirect(Hostname ++ "/series/" ++ proplists:get_value(id, Series), probix_format:series_to_json([Series]));

		%% wrong data, doing nothing
		{error, Error} ->
			probix_http:error(Error)
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
	probix_http:ok();

%% get all existing series
handle('GET', ["series"], [], undefined) ->
	log4erl:info(rest_logger, "getting all series"),
	Series = probix_series:all_series(),
	Content = probix_format:series_to_json(Series),
	probix_http:ok(Content);

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
	probix_http:ok(Content);


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
	probix_http:ok();

handle('OPTIONS', _Any, _Query, _Post) ->
    probix_http:ok("");

handle(_, _, _, _) ->
	probix_http:error(bad_request).

