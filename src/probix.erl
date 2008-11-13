%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(probix).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the probix server.
start() ->
    probix_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    application:start(probix).

%% @spec stop() -> ok
%% @doc Stop the probix server.
stop() ->
    Res = application:stop(probix),
    application:stop(mesia),
    application:stop(crypto),
    Res.
