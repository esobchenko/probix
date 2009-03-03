-module(probix_utils_tests).

-include_lib("eunit/include/eunit.hrl").

atom_to_binary_test_() ->
	[
		?_assertEqual(<<"foo">>, probix_utils:atom_to_binary(foo))
	].

json_object_to_record_test_() ->
	[
		?_assertMatch(
			{object,1,2,3},
			probix_utils:json_object_to_record({struct,[{<<"id">>,1},{<<"name">>,2},{<<"info">>,3}]}, probix_object)
		),
		?_assertThrow(
			{missing_params, [name]},
			probix_utils:json_object_to_record({struct,[{<<"info">>,3}]}, probix_object)
		),
		?_assertThrow(
			{bad_values, [name]},
			probix_utils:json_object_to_record({struct,[{<<"name">>,{struct, [<<"foo">>,1]}}]}, probix_object)
		)
	].

record_to_json_object_test_() ->
	[
		?_assertMatch(
			{struct,[{<<"id">>,1},{<<"name">>,2},{<<"info">>,3}]},
			probix_utils:record_to_json_object({object,1,2,3}, probix_object)
		),
		?_assertError(
			{invalid_record, {foo,1,2,3}},
			probix_utils:record_to_json_object({foo,1,2,3}, probix_object)
		),
		?_assertError(
			{invalid_record, {object,1,2}},
			probix_utils:record_to_json_object({object,1,2}, probix_object)
		)
	].

