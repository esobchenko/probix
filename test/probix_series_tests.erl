-module(probix_series_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
	{
		setup,
		fun() ->
			probix_db:stop(),
			probix_db:start_master(ram_copies, [node()])
		end,
		fun generate_basic_tests/1
	}.

generate_basic_tests(_) ->
	[
	].

