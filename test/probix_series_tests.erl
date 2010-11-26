-module(probix_series_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
	{
		setup,
		fun() ->
                ok
		end,
		fun generate_basic_tests/1
	}.

generate_basic_tests(_) ->
	[
	].

