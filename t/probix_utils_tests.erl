-module(probix_utils_tests).

-include_lib("eunit/include/eunit.hrl").

atom_to_binary_test_() ->
	[
		?_assert(<<"foo">> =:= probix_utils:atom_to_binary(foo))
	].



