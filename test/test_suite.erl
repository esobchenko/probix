-module(test_suite).
-export([acceptance/0]).

-include_lib("eunit/include/eunit.hrl").

acceptance() ->
	eunit:test([probix_format, probix_series, probix_http, probix_time]).

