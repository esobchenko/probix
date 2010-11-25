-module(start_probix).
-export([start/0]).


start() ->
	application:start(inets),
	application:start(log4erl),
	application:start(crypto),
	application:start(mochiweb),
	application:start(mnesia),
	application:start(emongo),
	application:start(probix).
