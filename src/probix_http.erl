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

    %% headers
    %%    Content_length = Req:get_header_value("Content-Length"),
    %%  Content_type = Req:get_header_value("Content-Type"),

    log4erl:info("Request-> Method: ~p, Path: ~p, Query: ~p, Post: ~p, Splitted: ~p", [Method, Path, Query, Post, Splitted]),

    R = try
        handle(Method, Splitted, Query, Post)
    catch
        %% regular throw exceptions
        throw:Error when is_record(Error, error) -> error(Error);
        %% erlang errors and other exceptions
        _Exception ->
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
            log4erl:info("Creating series"),
            Series = probix_series:new_series(Label),
            log4erl:info("Adding ticks to series: ~p", [ Series#series.id ]),
            probix_series:add_ticks(Series#series.id, Ticks),
            redirect("/series/" ++ Series#series.id);

        %% adding series without data
        {error, empty_json} ->
            log4erl:info("Creating series"),
            Series = probix_series:new_series(Label),
            redirect("/series/" ++ Series#series.id);

        %% wrong data, doing nothing
        {error, Error} ->
            error(Error)
    end;

%% Update series with data
handle('POST', ["series", Id], [], Post) ->
    log4erl:info("Updating series ~s", [ Id ]),
    {ok, Series} = probix_series:series(Id),
    case probix_format:ticks_from_json(Post) of
        {ok, Ticks} ->
            probix_series:add_ticks(Series#series.id, Ticks);
        {error, Error} ->
            error(Error)
    end,
    ok();

%% Get all existing series
handle('GET', ["series"], [], undefined) ->
    log4erl:info("Getting all series"),
    Series = probix_series:all_series(),
    Content = probix_format:series_to_json(Series),
    ok(Content);

handle('GET', ["series", Id], Args, undefined) ->
    case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
        [undefined, undefined] ->
            log4erl:info("Selecting all data for series ~s", [ Id ]),
            Ticks = probix_series:get_ticks(Id),
            Content = probix_format:ticks_to_json(Ticks),
            ok(Content);

        [undefined, To] when is_list(To) ->
            log4erl:info("Selecting all data for series ~s, to: ~p",
                         [Id, To]),
            Ticks = probix_series:get_ticks(Id, {to, To}),
            Content = probix_format:ticks_to_json(Ticks),
            ok(Content);

        [From, undefined] when is_list(From) ->
            log4erl:info("Selecting all data for series ~s, from: ~p",
                         [Id, From]),
            Ticks = probix_series:get_ticks(Id, {from, From}),
            Content = probix_format:ticks_to_json(Ticks),
            ok(Content);

        [From, To] when is_list(From); is_list(To) ->
            log4erl:info("Selecting all data for series ~s, from: ~p, to: ~p",
                         [Id, From, To]),
            Ticks = probix_series:get_ticks(Id, {From, To}),
            Content = probix_format:ticks_to_json(Ticks),
            ok(Content);

        _ ->
            error("Wrong args")
    end;

%% Removing data from series
%%
handle('DELETE', ["series", Id], Args, undefined) ->
    case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
        [undefined, undefined] ->
            log4erl:info("Deleting series with id: ~s", [ Id ]),
            probix_series:delete_ticks(Id), %% dunno if we need this?
            probix_series:delete_series(Id);

        [undefined, To] when is_list(To) ->
            log4erl:info("Deleting data for series ~s, to: ~p",
                         [Id, To]),
            probix_series:delete_ticks(Id, {to, To});

        [From, undefined] when is_list(From) ->
            log4erl:info("Deleting data for series ~s, from: ~p",
                         [Id, From]),
            probix_series:delete_ticks(Id, {from, From});

        [From, To] when is_list(From); is_list(To) ->
            log4erl:info("Deleting data for series ~s, from: ~p, to: ~p",
                         [Id, From, To]),
            probix_series:delete_ticks(Id, {From, To}),
            ok();
        _ ->
            error("Wrong args")
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
