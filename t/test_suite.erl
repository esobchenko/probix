-module(test_suite).
-export([acceptance/0]).

-include_lib("eunit/include/eunit.hrl").

acceptance() ->
	eunit:test([probix_utils, probix_object, probix_probe, probix_http]).

