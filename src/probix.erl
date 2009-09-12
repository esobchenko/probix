-module(probix).
-export([start_master/1, start_replica/1, stop/0]).

start_replica([Storage_type, Master]) ->
	application:start(log4erl),
	log4erl:conf("conf/log4erl.conf"),
	probix_db:start_replica(Storage_type, Master),
	application:start(crypto), %% required by mochiweb
	application:start(probix).

start_master([Storage_type]) ->
	application:start(log4erl),
	log4erl:conf("conf/log4erl.conf"),
	probix_db:start_master(Storage_type, [node()]),
	application:start(crypto), %% required by mochiweb
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(crypto),
	probix_db:stop(),
	application:stop(log4erl).
