-module(probix_object_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(O1, #object{id = 1, name = <<"foo">>, info = <<"bar">>}).
-define(O2, #object{id = 1, name = <<"bar">>, info = <<"baz">>}).
-define(J1, probix_utils:record_to_json(?O1, probix_object)).
-define(J2, probix_utils:record_to_json(?O2, probix_object)).

basic_crud_test_() ->
	{
		setup,
		fun() -> probix_db:reset() end,
		fun generate_basic_crud_tests/1
	}.

generate_basic_crud_tests(_) ->
	[
		?_assertMatch(
			?O1,
			probix_object:create(?O1)
		),
		?_assertMatch(
			?O1,
			probix_object:read(1)
		),
		?_assertThrow(
			{not_found, 2},
			probix_object:read(2)
		),
		?_assertMatch(
			?O2,
			probix_object:update(1, ?O2)
		),
		?_assertMatch(
			?O2,
			probix_object:read(1)
		),
		?_assertMatch(
			[?O2],
			probix_object:read_all()
		),
		?_assertMatch(
			1,
			probix_object:delete(1)
		),
		?_assertMatch(
			[],
			probix_object:read_all()
		)
	].

basic_json_test_() ->
	{
		setup,
		fun() -> probix_db:reset() end,
		fun generate_basic_json_tests/1
	}.

generate_basic_json_tests(_) ->
	[
		?_assertEqual(
			?J1,
			probix_object:create_from_json(?J1)
		),
		?_assertEqual(
			?J1,
			probix_object:read_as_json(1)
		),
		?_assertEqual(
			?J2,
			probix_object:update_from_json(1, ?J2)
		),
		?_assertEqual(
			probix_utils:record_to_json([?O2], probix_object),
			probix_object:read_all_as_json()
		),
		?_assertEqual(
			1,
			probix_object:delete(1)
		),
		?_assertEqual(
			probix_utils:record_to_json([], probix_object),
			probix_object:read_all_as_json()
		)
	].

