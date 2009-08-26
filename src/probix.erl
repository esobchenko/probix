-module(probix).
-export([start/0, stop/0]).

start() ->
	application:start(log4erl),
	log4erl:conf("conf/log4erl.conf"),
	probix_db:start({disc_copies, [node()]}),
	application:start(crypto), %% required by mochiweb
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(crypto),
	probix_db:stop(),
	application:stop(log4erl).
