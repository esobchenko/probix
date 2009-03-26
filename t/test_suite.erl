-module(test_suite).
-export([run_tests/0]).

-include_lib("eunit/include/eunit.hrl").

run_tests() ->
	eunit:test([probix_utils, probix_object, probix_probe, probix_http]).

