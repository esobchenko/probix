%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for probix.

-module(probix_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).
-include("probix_db.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
				"probix" ->
					Data = point:read_all(),
					%% io:format("~ndata:~p~n", [Data]),
					Json = mochijson2:encode(Data),
					Req:ok({"application/json", [], [Json]});
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
				"probix" ->
					Data = Req:parse_post(),
					Json = proplists:get_value("json", Data),
					Struct = mochijson2:decode(Json),
					%% io:format("~nstructure:~p~n", [Struct]),
					point:create(Struct),
					Req:ok({"application/json", [], [Json]});
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
