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
    %% adding empty list in case we have undefined post
    Post = case Req:recv_body() of
               undefined ->
                   "[]";
               P ->
                   P
           end,
                   
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
			Error = probix_error:create(internal_error, "something bad happened"),
			error(Error)
	end,
	Req:respond(R).

%% TODO: check Args and create different prototypes depending if Args/Post is passed

%% Create a new series with or without data
handle('POST', ["series"], [], Post) ->
    log4erl:info("Creating series without label"),
    Series = probix_series:new_series(),
    redirect("/series/" ++ Series#series.id);

handle('POST', ["series"], [{"label", Label}], Post) ->
    log4erl:info("Creating series with label ~s", [ Label ]),
    Series = probix_series:new_series(Label),
    case probix_format:ticks_from_json(Series#series.id, Post) of
        {ok, Ticks} ->
            probix_series:add_ticks(Ticks);
        {error, Error} ->
            error(Error)
    end,
    redirect("/series/" ++ Series#series.id);

%% Update series with data
handle('POST', ["series", Id], [], Post) ->
    log4erl:info("Updating series ~s", [ Id ]),
    Series = probix_series:get_series(Id),
    case probix_format:ticks_from_json(Series#series.id, Post) of
        {ok, Ticks} ->
            probix_series:add_ticks(Ticks);
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

%% Get a list of ticks in this series
handle('GET', ["series", Id], [], undefined) ->
    log4erl:info("Selecting all data for series ~s", [ Id ]),
    Ticks = probix_series:get_ticks(Id),
    Content = probix_format:ticks_to_json(Ticks),
    ok(Content);


handle('GET', ["series", Id], [{from, From}], undefined) ->
    log4erl:info("Selecting all data for series ~s, from: ~p", 
                 [Id, From]),
    Ticks = probix_series:get_ticks(Id, {from, From}),
    Content = probix_format:ticks_to_json(Ticks),
    ok(Content);

handle('GET', ["series", Id], [{to, To}], undefined) ->
    log4erl:info("Selecting all data for series ~s, to: ~p", 
                 [Id, To]),
    Ticks = probix_series:get_ticks(Id, {to, To}),
    Content = probix_format:ticks_to_json(Ticks),
    ok(Content);

handle('GET', ["series", Id], Args, undefined) ->
    case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
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
handle('DELETE', ["series", Id], [], undefined) ->
    log4erl:info("Deleting series with id: ~s", [ Id ]),
    probix_series:delete_ticks(Id), %% dunno if we need this?
    probix_series:delete_series(Id),
    ok();

handle('DELETE', ["series", Id], [{from, From}], undefined) ->
    log4erl:info("Deleting data for series ~s, from: ~p", 
                 [Id, From]),
    Ticks = probix_series:delete_ticks(Id, {from, From}),
    ok();

handle('DELETE', ["series", Id], [{to, To}], undefined) ->
    log4erl:info("Deleting data for series ~s, to: ~p", 
                 [Id, To]),
    Ticks = probix_series:delete_ticks(Id, {to, To}),
    ok();

handle('DELETE', ["series", Id], Args, undefined) ->
    case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
        [From, To] when is_list(From); is_list(To) ->
            log4erl:info("Deleting data for series ~s, from: ~p, to: ~p", 
                         [Id, From, To]),
            Ticks = probix_series:delete_ticks(Id, {From, To}),
            ok();
        _ -> 
            error("Wrong args")
    end;


handle(_, _, _, _) ->
    error("Unknown request").

%%
%% ok and error functions help to construct mochiweb's http response tuples;
%% they used in handle/5 functions.
%%


%% empty response
ok() ->
	{200, [{"Content-Type", "application/json"}], ""}.

ok(Content) ->
	{200, [{"Content-Type", "application/json"}], Content}.

%% ok(Content, Headers) ->
%%    {200, Headers, Content}.

error(Error) ->
    log4erl:error(Error),
	{400, [], ""}.


redirect(Location) ->
    {301, [{"Location", Location}], ""}.

    
%% content_type() ->
%%     {"Content-Type", "application/json"}.
%% content_type(json) ->
%%     {"Content-Type", "application/json"};
%% content_type(csv) ->
%%     {"Content-Type", "text/csv"}.


%% returns http numeric response code for
%% given error according to specification
%% http_code(Error) when is_record(Error, error) ->
%%	case Error#error.code of
%%		not_found ->
%% 			404;
%% 		unknown_format ->
%% 			406;
%% 		internal_error ->
%% 			500;
%% 		_Other ->
%% 			400
%% 	end.

%% do we need this here?

%% this function is used to convert some strings to integers (e.g. object id) because
%% bad_request exception must be raised to inform user about the problem.
%% to_integer(L) when is_list(L) ->
%% 	try erlang:list_to_integer(L)
%%	catch
%%		error:badarg -> throw(
%%			probix_error:create(bad_request, "got string where integer is expected")
%%		)
%%	end.

