-module(probix_http_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("probix.hrl").

-define(URL, "http://127.0.0.1:8000").

-define(O1, #object{id = 1, name = <<"foo">>, info = <<"bar">>}).
-define(J1, probix_utils:record_to_json(?O1, probix_object)).

rest_req('GET', Path) ->
	Result = http:request(get, {?URL ++ Path, []}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, Body};
rest_req('DELETE', Path) ->
	Result = http:request(delete, {?URL ++ Path, []}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, Body}.

rest_req('POST', Path, Data) ->
	Result = http:request(post, {?URL ++ Path, [], [], Data}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, Body};
rest_req('PUT', Path, Data) ->
	Result = http:request(put, {?URL ++ Path, [], [], Data}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, Body}.

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
			{200, "[]"},
			rest_req('GET', "/objects")
		),
		?_assertMatch(
			{404, _},
			rest_req('GET', "/object/1")
		),
		?_assertMatch(
			{400, _},
			rest_req('POST', "/object", "improper json")
		),
		?_assertEqual(
			{200, ?J1},
			rest_req('POST', "/object", ?J1)
		)
	].

