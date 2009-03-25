-module(test_suite).
-export([run_tests/0]).

run_tests() ->
	probix_utils:test(),
	probix_object:test(),
	probix_probe:test().

