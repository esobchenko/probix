-module(probix_format_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

atom_to_binary_test_() ->
	[
		?_assertEqual(<<"foo">>, probix_format:atom_to_binary(foo))
	].

