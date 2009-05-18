-module(probix).
-export([start/0, stop/0]).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

start() ->
	ensure_started(crypto),
	probix_db:init(), %% schema should be created before mnesia is started
	ensure_started(mnesia),
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(mesia),
	application:stop(crypto).
