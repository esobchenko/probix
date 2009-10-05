-module(probix_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%	Ip = case os:getenv("PROBIX_SERVER_IP") of false -> "0.0.0.0"; Env_ip -> Env_ip end,
%	Port = case os:getenv("PROBIX_SERVER_PORT") of false -> "8000"; Env_port -> Env_port end,

%	Http_config = [
%		{ip, Ip},
%		{port, Port},
%		{max, 100000}
%	],

%	Http = {
%		probix_http,
%		{probix_http, start, [Http_config]},
%		permanent, 2048, worker, dynamic
%	},

	Series = {
		probix_series,
		{probix_series, start_link, []},
		permanent, 2048, worker, dynamic
	},

%	Processes = [Http, Series],
	Processes = [Series],
	{ok, {{one_for_one, 10, 10}, Processes}}.
