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

json_term_to_record_test_() ->
	[
		?_assertMatch(
			{foo,1,2},
			probix_utils:json_term_to_record({struct,[{<<"foo">>,1},{<<"bar">>,2}]}, ?MODULE)
		),
		?_assertThrow(
			{bad_input, _}, %% missing parameters
			probix_utils:json_term_to_record({struct,[{<<"bar">>,3}]}, ?MODULE)
		),
		?_assertThrow(
			{bad_input, _}, %% bad values
			probix_utils:json_term_to_record({struct,[{<<"foo">>,{struct,[<<"baz">>,1]}}]}, ?MODULE)
		)
	].

record_to_json_term_test_() ->
	[
		?_assertMatch(
			{struct,[{<<"foo">>,1},{<<"bar">>,2}]},
			probix_utils:record_to_json_term({foo,1,2}, ?MODULE)
		),
		?_assertError(
			{invalid_record, {baz,1,2,3}},
			probix_utils:record_to_json_term({baz,1,2,3}, ?MODULE)
		),
		?_assertError(
			{invalid_record, {foo,1}},
			probix_utils:record_to_json_term({foo,1}, ?MODULE)
		)
	].

record_to_json_test_() ->
	[
		?_assertMatch(
			<<"{\"foo\":1,\"bar\":2}">>,
			probix_utils:record_to_json({foo,1,2}, ?MODULE)
		)
	].

json_to_record_test_() ->
	[
		?_assertThrow(
			{bad_input, _}, %% improper json term
			probix_utils:json_to_record(<<"[1]">>, ?MODULE)
		),
		?_assertThrow(
			{bad_input, _}, %% bad json
			probix_utils:json_to_record(<<"bad json">>, ?MODULE)
		),
		?_assertMatch(
			{foo,1,2},
			probix_utils:json_to_record(
				<<"{\"foo\":1,\"bar\":2}">>,
				?MODULE
			)
		),
		?_assertMatch(
			<<"[{\"foo\":1,\"bar\":2},{\"foo\":3,\"bar\":4}]">>,
			probix_utils:record_to_json([{foo,1,2},{foo,3,4}], ?MODULE)
		)
	].

reversibility_test_() ->
	[
		?_assertEqual(
			<<"{\"foo\":\"bar\",\"bar\":\"baz\"}">>,
			probix_utils:record_to_json(
				probix_utils:json_to_record(
					"{\"foo\":\"bar\",\"bar\":\"baz\"}",
					?MODULE
				),
				?MODULE
			)
		),
		?_assertEqual(
			<<"{\"foo\":\"bar\",\"bar\":\"baz\"}">>,
			probix_utils:record_to_json(
				probix_utils:json_to_record(
					<<"{\"foo\":\"bar\",\"bar\":\"baz\"}">>,
					?MODULE
				),
				?MODULE
			)
		),
		?_assertEqual(
			#foo{foo = <<"bar">>, bar = <<"baz">>},
			probix_utils:json_to_record(
				probix_utils:record_to_json(
					#foo{foo = <<"bar">>, bar = <<"baz">>},
					?MODULE
				),
				?MODULE
			)
		)
	].
