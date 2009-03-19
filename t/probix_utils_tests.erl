-module(probix_utils_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-record(foo, {foo, bar}).

acceptable_value(none) ->
	true;
acceptable_value({_K, V}) when is_binary(V); is_integer(V) ->
	true;
acceptable_value(_Pair) ->
	false.

required_fields() ->
	[foo].

record_name() ->
	foo.

record_fields() ->
	record_info(fields, foo).


atom_to_binary_test_() ->
	[
		?_assertEqual(<<"foo">>, probix_utils:atom_to_binary(foo))
	].

json_object_to_record_test_() ->
	[
		?_assertMatch(
			{foo,1,2},
			probix_utils:json_object_to_record({struct,[{<<"foo">>,1},{<<"bar">>,2}]}, ?MODULE)
		),
		?_assertThrow(
			{missing_params, [foo]},
			probix_utils:json_object_to_record({struct,[{<<"bar">>,3}]}, ?MODULE)
		),
		?_assertThrow(
			{bad_values, [foo]},
			probix_utils:json_object_to_record({struct,[{<<"foo">>,{struct,[<<"baz">>,1]}}]}, ?MODULE)
		)
	].

record_to_json_object_test_() ->
	[
		?_assertMatch(
			{struct,[{<<"foo">>,1},{<<"bar">>,2}]},
			probix_utils:record_to_json_object({foo,1,2}, ?MODULE)
		),
		?_assertError(
			{invalid_record, {baz,1,2,3}},
			probix_utils:record_to_json_object({baz,1,2,3}, ?MODULE)
		),
		?_assertError(
			{invalid_record, {foo,1}},
			probix_utils:record_to_json_object({foo,1}, ?MODULE)
		)
	].

