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

%% Create a new series without data
handle('POST', ["series"], [], undefined) ->
    log4erl:info("Creating series without label"),
    %% probix_series:new_series();
    ok();

handle('POST', ["series"], [{"label", Label}], undefined) ->
    log4erl:info("Creating series with label ~s", [ Label ]),
    %% probix_series:new_series(Label);
    ok();

%% Create a new series with data
handle('POST', ["series"], [], _Post) ->
    log4erl:info("Creating series without label and adding data"),
    %% probix_series:new_series(),
    %% probix_series:add_ticks(Post);
    ok();

handle('POST', ["series"], [{"label", Label}], _Post) ->
    log4erl:info("Creating series with label: ~s and adding data", [ Label ]),
    %% Id = probix_series:new_series(Label),
    %% probix_series:add_ticks(Id, Post);
    ok();

%% Update series with data
handle('POST', ["series", Id], [], _Post) ->
    log4erl:info("Updating series ~s", [ Id ]),
    %% probix_series:add_ticks(Id, Post);
    ok();

%% Get all existing series
handle('GET', ["series"], [], undefined) ->
    log4erl:info("Getting all series"),
    %% probix_series:all_series();
    ok();

%% Get a list of ticks in this series
handle('GET', ["series", Id], [], undefined) ->
    log4erl:info("Selecting all data for series ~s", [ Id ]),
    %% probix_series:get_ticks(Id);
    ok();


handle('GET', ["series", Id], [{from, From}], undefined) ->
    log4erl:info("Selecting all data for series ~s, from: ~p", 
                 [Id, From]),
    %% probix_series:get_ticks(Id);
    ok();


handle('GET', ["series", Id], [{to, To}], undefined) ->
    log4erl:info("Selecting all data for series ~s, to: ~p", 
                 [Id, To]),
    %% probix_series:get_ticks(Id);
    ok();

handle('GET', ["series", Id], Args, undefined) ->
    case [proplists:get_value("from", Args), proplists:get_value("to", Args)] of
        [From, To] when is_list(From); is_list(To) ->
            log4erl:info("Selecting all data for series ~s, from: ~p, to: ~p", 
                         [Id, From, To]);
        _ -> 
            error("Wrong args")
    end,
    %% probix_series:get_ticks(Id,{From, To});
    ok();

%% Get a slice of ticks for this series

%% Removing data from series
%%
handle('DELETE', ["series", Id], [], undefined) ->
    log4erl:info("Deleting series with id: ~s", [ Id ]),
    %% probix_series:delete_ticks();
    ok();

handle(_, _, _, _) ->
    log4erl:error("Unknown request").

%%
%% ok and error functions help to construct mochiweb's http response tuples;
%% they used in handle/5 functions.
%%

%% ok(Content) ->
%%	{200, [{"Content-Type", "application/json"}], Content}.

%% empty response
ok() ->
	{200, [{"Content-Type", "application/json"}], ""}.

error(Error) ->
    log4erl:error(Error).

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

