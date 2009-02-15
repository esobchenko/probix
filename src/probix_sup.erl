-module(probix_sup).
-author('Eugen Sobchenko <eugen@sobchenko.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,
	WebConfig = [
		{ip, Ip},
		{port, 8000}
	],
	Web = {
		probix_http,
		{probix_http, start, [WebConfig]},
		permanent, 5000, worker, dynamic
	},

	Processes = [Web],
	{ok, {{one_for_one, 10, 10}, Processes}}.
