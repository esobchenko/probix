-module(probix_sup).
-author('Eugen Sobchenko <eugen@sobchenko.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Ip = case os:getenv("PROBIX_IP") of false -> "0.0.0.0"; Env_ip -> Env_ip end,
	Port = case os:getenv("PROBIX_PORT") of false -> "8000"; Env_port -> Env_port end,
	Config = [
		{ip, Ip},
		{port, Port}
	],
	Web = {
		probix_http,
		{probix_http, start, [Config]},
		% we’ll set our maximum to 100k connections. (default: 2048)
		permanent, 100000, worker, dynamic
	},

	Processes = [Web],
	{ok, {{one_for_one, 10, 10}, Processes}}.
