-module(probix_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case application:get_env(probix, log4erl_conf) of 
        {ok, Logger_Conf} ->
            log4erl:conf(Logger_Conf);
        undefined ->
            false
    end,

    case application:get_env(emongo, pools) of 
        undefined ->
            emongo:add_pool(test_pool, "localhost", 27017, "test_db", 1);
        {ok, _Pools} ->
            false
    end,

	case os:getenv("PROBIX_SERVER_IP") of
        false -> false;
        Env_ip -> application:set_env(probix, probix_host, Env_ip)
    end,
	case os:getenv("PROBIX_SERVER_PORT") of
        false -> false;
        Env_port -> application:set_env(probix, probix_port, Env_port)
    end,

    {ok, Ip} = application:get_env(probix, probix_host),
    {ok, Port} = application:get_env(probix, probix_port),

    case Port of
        "80" ->
            application:set_env(probix, probix_hostname, "http://" ++ Ip);
        Port ->
            application:set_env(probix, probix_hostname, "http://" ++ Ip ++ ":" ++ Port)
    end,

%    application:set_env(probix, probix_docroot, "priv/www"),

	Http_config = [
		{ip, Ip},
		{port, Port}
	],

	Http = {
		probix_http,
		{probix_http, start, [Http_config]},
		permanent, 2048, worker, dynamic
	},

	Series = {
		probix_series,
		{probix_series, start_link, []},
		permanent, 2048, worker, dynamic
	},

	Processes = [Http, Series],
	{ok, {{one_for_one, 10, 10}, Processes}}.
