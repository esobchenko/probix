-module(probix_probe).
-author('Eugen Sobchenko <eugen@sobchenko.com>').
-compile(export_all).

-include("probix.hrl").

read_all() ->
	probix_db:read_all(probe).

