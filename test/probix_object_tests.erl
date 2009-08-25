-module(probix_object_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(O1, #object{id = 1, name = <<"foo">>, info = <<"bar">>}).
-define(O2, #object{id = 1, name = <<"bar">>, info = <<"baz">>}).
-define(J1, probix_utils:record_to_json(?O1, probix_object)).
-define(J2, probix_utils:record_to_json(?O2, probix_object)).

basic_test_() ->
	{
		setup,
		fun() -> probix_db:test_start() end,
		fun generate_basic_tests/1
	}.

generate_basic_tests(_) ->
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
			#error{code = not_found},
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

