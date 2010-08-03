-module(probix).
-export([start_master/1, start_replica/1, stop/0]).

start_replica([Master]) ->
	application:start(log4erl),
	log4erl:conf("conf/log4erl.conf"), %% XXX this should be moved to probix_app.erl
	application:start(mnesia),
	probix_db:start_replica(Master), %% XXX this should be moved to probix_app.erl
	application:start(crypto), %% required by mochiweb
	application:start(probix).

start_master([Storage_type]) ->
	application:start(log4erl),
	application:start(mnesia),
	log4erl:conf("conf/log4erl.conf"), %% XXX
	probix_db:start_master(Storage_type, [node()]), %% XXX
	application:start(crypto), %% required by mochiweb
	application:start(probix).

stop() ->
	application:stop(probix),
	application:stop(crypto),
	application:stop(mnesia),
	application:stop(log4erl).
