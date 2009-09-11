-module(probix).
-export([start/0, stop/0, start_replica/1]).

start_replica([Master]) ->
	application:start(log4erl),
	log4erl:conf("conf/log4erl.conf"),
	probix_db:start_replica(ram_copies, Master),
	application:start(crypto), %% required by mochiweb
	application:start(probix).

start() ->
	application:start(log4erl),
	log4erl:conf("conf/log4erl.conf"),
	probix_db:start_disc(),
	application:start(crypto), %% required by mochiweb
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(crypto),
	probix_db:stop(),
	application:stop(log4erl).
