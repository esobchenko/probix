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

-define(JS1, "{\"time_created\":1,\"label\":1}").

-define(JT1, "{\"timestamp\": 1, \"value\":1}").
-define(JT2, "[{\"timestamp\": 1, \"value\":1},{\"timestamp\":2, \"value\":2},{\"timestamp\":3, \"value\":3}]").


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

basic_rest_test_() ->
    {
        setup,
        fun() ->                
                application:start(sasl),
                application:start(inets),
                application:start(crypto),
                application:start(mochiweb),
                application:start(emongo),
                application:start(log4erl),
                application:start(probix),
                emongo:delete(test_pool, "series"),
                emongo:delete(test_pool, "ticks")
        end,
        fun(_) ->
                application:stop(probix),
                application:stop(emongo),
                application:stop(mochiweb),
                application:stop(log4erl)

        end,
        fun generate_basic_rest_tests/1
    }.

generate_basic_rest_tests(_) ->
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
        rest_req('POST', "/series", "")
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series?label=Empty", "")
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series?label=", "")
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series?label=Single", ?JT1)
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series?label=Multiple", ?JT2)
       ),
     ?_assertMatch(
        {301, _},
        rest_req('POST', "/series", ?JT2)
       ),
     ?_assertMatch(
        {200, _},
        rest_req('GET', "/series")
       )
    ].

series_update_test_() ->
    {
        setup,
        fun() ->
                application:start(sasl),
                application:start(inets),
                application:start(crypto),
                application:start(emongo),
                application:start(log4erl),
                application:start(mochiweb),
                application:start(probix),
                Series = probix_series:new_series(),
                proplists:get_value(id, Series)
        end,
        fun(_) ->
                application:stop(probix),
                application:stop(emongo),
                application:stop(mochiweb),
                application:stop(log4erl)
                
        end,
        fun generate_series_update_test/1
    }.

generate_series_update_test(Id) ->
    [
     ?_assertMatch(
        {200, _},
        rest_req('POST', "/series/" ++ Id, ?JT2)
     ),
     ?_assertMatch(
        {200, _},
        rest_req('GET', "/series/" ++ Id)
     ),
     ?_assertMatch(
        {404, _},
        rest_req('POST', "/series/Foobar", ?JT2)
     ),
     ?_assertMatch(
        {404, _},
        rest_req('DELETE', "/series/Foobar")
     ),
     ?_assertMatch(
        {200, _},
        rest_req('DELETE', "/series/" ++ Id)
     ),
     ?_assertMatch(
        {404, _},
        rest_req('DELETE', "/series/" ++ Id)
     )
    ].

