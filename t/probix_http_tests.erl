-module(probix_http_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("probix.hrl").

-define(O1, #object{id = 1, name = <<"foo">>, info = <<"bar">>}).
-define(J1, probix_utils:record_to_json(?P1, probix_object)).

basic_object_crud_test_() ->
	{
		setup,
		fun() ->
			probix_db:reset(), %% clean database contents
			application:start(inets),
			probix:start()
		end,
		fun(_) ->
			probix:stop(),
			application:stop(inets),
			probix_db:reset()
		end,
		fun generate_basic_object_crud_tests/1
	}.

generate_basic_object_crud_tests(_) ->
	[
		?_assertMatch(
			{ok, { {"HTTP/1.1",200,"OK"}, _Headers, "[]" }},
			http:request("http://localhost:8000/objects")
		)
	].

