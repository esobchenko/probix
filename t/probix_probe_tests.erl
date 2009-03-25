-module(probix_probe_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(P1, #probe{id = 1, id_object = 1, timestamp = <<"1237923724">>, value = <<"0">>}).
-define(J1, probix_utils:record_to_json(?P1, probix_probe)).

basic_crud_test_() ->
	{
		setup,
		fun() -> probix_db:reset() end,
		fun generate_basic_crud_tests/1
	}.

generate_basic_crud_tests(_) ->
	[
		?_assertMatch(
			?P1,
			probix_probe:create(?P1)
		),
		?_assertMatch(
			?P1,
			probix_probe:read(1)
		),
		?_assertThrow(
			{not_found, 2},
			probix_probe:read(2)
		),
		?_assertMatch(
			[?P1],
			probix_probe:probes_by_object_id(1)
		),
		?_assertMatch(
			1,
			probix_probe:delete(1)
		),
		?_assertMatch(
			[],
			probix_probe:probes_by_object_id(1)
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
			probix_probe:create_from_json(?J1)
		),
		?_assertEqual(
			?J1,
			probix_probe:read_as_json(1)
		),
		?_assertEqual(
			probix_utils:list_to_json([?P1], probix_probe),
			probix_probe:probes_by_object_id_as_json(1)
		),
		?_assertEqual(
			1,
			probix_probe:delete(1)
		),
		?_assertEqual(
			probix_utils:list_to_json([], probix_probe),
			probix_probe:probes_by_object_id_as_json(1)
		)
	].

