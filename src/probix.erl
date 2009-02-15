-module(probix).
-author('Eugen Sobchenko <eugen@sobchenko.com>').
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
	ensure_started(mnesia),
	probix_db:init(),
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(mesia),
	application:stop(crypto).
