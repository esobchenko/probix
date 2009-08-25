-module(probix).
-export([start/0, stop/0]).

start() ->
	probix_db:start({disc_copies, [node()]}),
	application:start(crypto), %% required by mochiweb
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(crypto),
	probix_db:stop().
