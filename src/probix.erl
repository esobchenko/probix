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
	probix_db:start(),
	application:start(probix).

stop() ->
	application:stop(probix),
	probix_db:stop(),
	application:stop(crypto).
