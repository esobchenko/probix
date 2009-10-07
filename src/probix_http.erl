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

%% TODO: check Args and create different prototypes depending if Args/Post is passed

%% Create a new series
handle('POST', ["series"], _, Post) ->
    probix_series:new_series();

%% Add probes to existing series
handle('POST', ["series", Id_string], _, Post) ->
    probix_series:add_probes();

%% Get all existing series
handle('GET', ["series"], _, _) ->
    probix_series:all_series();

%% Get a list of probes in this series
%% Get a slice of probes for this series
handle('GET', ["series", Id_string], Args, _) ->
    probix_series:get_probes();

%% Removing data from series
%%
handle('DELETE', ["series", Id_string], Args, _) ->
    probix_series:delete_probes();

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

