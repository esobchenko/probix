-module(probix_probe_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(O1, #object{id = 1, name = <<"foo">>, info = <<"bar">>}).

-define(P1, #probe{timestamp = 1237923724, value = <<"10">>}).
-define(P2, #probe{timestamp = 1237923725, value = <<"20">>}).
-define(P3, #probe{timestamp = 1237923726, value = <<"30">>}).
-define(P4, #probe{timestamp = 1237923727, value = <<"40">>}).
-define(P5, #probe{timestamp = 1237923728, value = <<"50">>}).
-define(J1, probix_utils:record_to_json(?P1, probix_probe)).
-define(J2, probix_utils:record_to_json(?P2, probix_probe)).
-define(J3, probix_utils:record_to_json(?P3, probix_probe)).
-define(J4, probix_utils:record_to_json(?P4, probix_probe)).
-define(J5, probix_utils:record_to_json(?P5, probix_probe)).

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
		?_assertThrow(
			#error{code = not_found}, %% foreign key constraint check raise exception
			probix_probe:create(1, ?P1)
		),
		?_assertMatch(
			?O1,
			probix_object:create(?O1)
		),
		?_assertMatch(
			?P1,
			probix_probe:create(1, ?P1)
		),
		?_assertMatch(
			[?P1],
			probix_probe:probes_by_object_id(1)
		),
		?_assertMatch(
			?P2,
			probix_probe:create(1, ?P2)
		),
		?_assertMatch(
			?P3,
			probix_probe:create(1, ?P3)
		),
		?_assertMatch(
			?P4,
			probix_probe:create(1, ?P4)
		),
		?_assertMatch(
			?P5,
			probix_probe:create(1, ?P5)
		),
		?_assertEqual(
			[?P1, ?P2, ?P3, ?P4, ?P5],
			probix_probe:probes_by_object_id(1)
		),
		?_assertEqual(
			[?P4, ?P5],
			probix_probe:probes_by_object_id(1, {from, 1237923727})
		),
		?_assertEqual(
			[?P1, ?P2, ?P3],
			probix_probe:probes_by_object_id(1, {to, 1237923726})
		),
		?_assertEqual(
			[?P3],
			probix_probe:probes_by_object_id(1, 1237923726, 1237923726)
		),
		?_assertThrow(
			#error{code = not_found}, %% foreign key constraint check raise exception
			probix_probe:probes_by_object_id(666)
		)
	].

