-module(probix_app).
-author('Eugen Sobchenko <eugen@sobchenko.com>').

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) -> probix_sup:start_link().

stop(_State) -> ok.

