-module(probix_http_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("probix.hrl").

-define(URL,
	"http://" ++
		case os:getenv("PROBIX_SERVER_IP") of false -> "127.0.0.1"; Env_ip -> Env_ip end
	++ ":" ++
		case os:getenv("PROBIX_SERVER_PORT") of false -> "8000"; Env_port -> Env_port end
).

-define(O1, #object{id = 1, name = <<"foo">>, info = <<"bar">>}).
-define(O2, #object{id = 1, name = <<"bar">>, info = <<"baz">>}).
-define(O3, #object{id = 2, name = <<"bar">>, info = <<"baz">>}).

-define(J1, probix_utils:record_to_json(?O1, probix_object)).
-define(J2, probix_utils:record_to_json(?O2, probix_object)).
-define(J3, probix_utils:record_to_json(?O3, probix_object)).

-define(P1, #probe{timestamp = 1237923724, value = 10}).
-define(P2, #probe{timestamp = 1237923725, value = 20}).
-define(P3, #probe{timestamp = 1237923726, value = 30}).
-define(P4, #probe{timestamp = 1237923727, value = 40}).
-define(P5, #probe{timestamp = 1237923728, value = 50}).
-define(P6, #probe{timestamp = 1237923729, value = 60}).
-define(JP1, probix_utils:record_to_json(?P1, probix_probe)).
-define(JP2, probix_utils:record_to_json(?P2, probix_probe)).
-define(JP3, probix_utils:record_to_json(?P3, probix_probe)).
-define(JP4, probix_utils:record_to_json(?P4, probix_probe)).
-define(JP5, probix_utils:record_to_json(?P5, probix_probe)).
-define(JP6, probix_utils:record_to_json(?P6, probix_probe)).

%% converting all body answers to binary, cause http:requests returns string
%% and probix_utils:*_to_json return binary
rest_req('GET', Path) ->
	Result = http:request(get, {?URL ++ Path, []}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, list_to_binary(Body)};
rest_req('DELETE', Path) ->
	Result = http:request(delete, {?URL ++ Path, []}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, list_to_binary(Body)}.

rest_req('POST', Path, Data) ->
	Result = http:request(post, {?URL ++ Path, [], [], Data}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, list_to_binary(Body)};
rest_req('PUT', Path, Data) ->
	Result = http:request(put, {?URL ++ Path, [], [], Data}, [], []),
	{ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
	{Status, list_to_binary(Body)}.

basic_object_crud_test_() ->
	{
		setup,
		fun() ->
			probix_db:stop(),
			application:start(inets),
			probix:start_master([ram_copies])
		end,
		fun(_) ->
			probix:stop(),
			application:stop(inets)
		end,
		fun generate_basic_object_crud_tests/1
	}.

generate_basic_object_crud_tests(_) ->
	[
		?_assertMatch( %% issue #9
			{400, _},
			rest_req('GET', "/")
		),
		?_assertMatch(
			{200, <<"[]">>},
			rest_req('GET', "/objects")
		),
		?_assertMatch(
			{404, _},
			rest_req('GET', "/object/1")
		),
		?_assertMatch(
			{400, _},
			rest_req('POST', "/object", <<"improper json">>)
		),
		?_assertMatch(
			{400, _},
			rest_req('POST', "/object", <<"1">>) %% proper json but invalid data
		),
		?_assertEqual(
			{200, ?J1},
			rest_req('POST', "/object", ?J1)
		),
		?_assertEqual(
			{200, ?J1},
			rest_req('GET', "/object/1")
		),
		?_assertMatch(
			{404, _},
			rest_req('GET', "/object/2")
		),
		?_assertMatch(
			{400, _},
			rest_req('PUT', "/object/1", <<"improper json">>)
		),
		?_assertMatch(
			{400, _},
			rest_req('PUT', "/object/1", <<"1">>) %% proper json but invalid data
		),
		?_assertEqual(
			{200, ?J2},
			rest_req('PUT', "/object/1", ?J2)
		),
		?_assertMatch(
			{404, _},
			rest_req('PUT', "/object/2", ?J2)
		),
		?_assertEqual(
			{200, ?J2},
			rest_req('GET', "/object/1")
		),
		?_assertMatch(
			{404, _},
			rest_req('DELETE', "/object/2")
		),
		?_assertMatch(
			{200, _},
			rest_req('DELETE', "/object/1")
		),
		?_assertMatch(
			{404, _},
			rest_req('GET', "/object/1")
		),
		?_assertEqual(
			{200, ?J3},
			rest_req('POST',"/object", ?J3)
		),
		%% we don't have any probes for object 2 yet
		?_assertMatch(
			{200, <<"[]">>},
			rest_req('GET',"/object/2/probes")
		),
		%% adding 2 probes to object 2
		?_assertMatch(
			{200, _},
			rest_req('POST',"/object/2/probes","[" ++ binary_to_list(?JP1) ++ ", " ++ binary_to_list(?JP2) ++ "]")
		),
		%% getting all probes
		?_assertEqual(
			{200, list_to_binary("[" ++ binary_to_list(?JP1) ++ "," ++ binary_to_list(?JP2) ++ "]")},
			rest_req('GET',"/object/2/probes")
		),
		%% testing "to" limiter
		?_assertEqual(
			{200, list_to_binary("[" ++ binary_to_list(?JP1) ++ "]")},
			rest_req('GET',"/object/2/probes?to=1237923724")
		),
		%% testing "from" limiter
		?_assertEqual(
			{200, list_to_binary("[" ++ binary_to_list(?JP2) ++ "]")},
			rest_req('GET',"/object/2/probes?from=1237923725")
		),
		%% testing both timestamp limiters
		?_assertEqual(
			{200, list_to_binary("[" ++  binary_to_list(?JP1) ++","++ binary_to_list(?JP2) ++ "]")},
			rest_req('GET',"/object/2/probes?from=1237923724&to=1237923725")
		),
		%% add probes list and return it
		?_assertEqual(
			{200, list_to_binary("[" ++ binary_to_list(?JP3) ++ "]")},
			rest_req('POST',"/object/2/probes?return=1","[" ++ binary_to_list(?JP3) ++ "]")
		),
		%% add single probe and return it
		?_assertEqual(
			{200, list_to_binary(binary_to_list(?JP6))},
			rest_req('POST',"/object/2/probes?return=1",binary_to_list(?JP6))
		),
		%% request should fail because object id should be integer
		?_assertMatch(
			{400, _},
			rest_req('GET', "/object/foo")
		)
	].

