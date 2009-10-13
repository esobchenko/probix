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
        rest_req('GET', "/series")
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series")
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series?label=Foo")
       )
   

	].

