-module(probix_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-ifndef(TEST).
-define(REST_PORT_KEY, probix_rest_port).
-else.
-define(REST_PORT_KEY, probix_rest_test_port).
-endif.


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% init logger
    case application:get_env(probix, log4erl_conf) of 
        {ok, Logger_Conf} ->
            log4erl:conf(Logger_Conf);
        undefined ->
            false
    end,

    %% configuring rest server
    {ok, Rest_port} = application:get_env(probix, ?REST_PORT_KEY),
    application:set_env(probix, probix_rest_hostname, "http://127.0.0.1:" ++ Rest_port),
	Rest = {
		probix_rest,
		{probix_rest, start, [[{port, Rest_port}]]},
		permanent, 2048, worker, dynamic
	},

    %% configuring series gen_server
	Series = {
		probix_series,
		{probix_series, start_link, []},
		permanent, 2048, worker, dynamic
	},

	Processes = [Rest, Series],
	{ok, {{one_for_one, 10, 10}, Processes}}.
