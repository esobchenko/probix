-module(probix_rest_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("probix.hrl").

-define(JS1, "{\"time_created\":1,\"label\":1}").

-define(JT1, "{\"timestamp\": 1, \"value\":1}").
-define(JT2, "[{\"timestamp\": 1, \"value\":1},{\"timestamp\":2, \"value\":2},{\"timestamp\":3, \"value\":3}]").


%% converting all body answers to binary, cause http:requests returns string
%% and probix_utils:*_to_json return binary
rest_req('GET', Path) ->
    {ok, Url} = application:get_env(probix, probix_hostname),
    Result = http:request(get, {Url ++ Path, []}, [], []),
    {ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
    {Status, list_to_binary(Body)};
rest_req('DELETE', Path) ->
    {ok, Url} = application:get_env(probix, probix_hostname),
    Result = http:request(delete, {Url ++ Path, []}, [], []),
    {ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
    {Status, list_to_binary(Body)}.

rest_req('POST', Path, Data) ->
    {ok, Url} = application:get_env(probix, probix_hostname),
    Result = http:request(post, {Url ++ Path, [], [], Data}, [], []),
    {ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
    {Status, list_to_binary(Body)};
rest_req('PUT', Path, Data) ->
    {ok, Url} = application:get_env(probix, probix_hostname),
    Result = http:request(put, {Url ++ Path, [], [], Data}, [], []),
    {ok, {{"HTTP/1.1", Status, _Reason}, _Headers, Body}} = Result,
    {Status, list_to_binary(Body)}.

basic_rest_test_() ->
    {
        setup,
        fun() ->
                error_logger:tty(false),
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
                error_logger:tty(false),
                application:start(inets),
                application:start(crypto),
                application:start(emongo),
                application:start(log4erl),
                application:start(mochiweb),
                application:start(probix),
                emongo:delete(test_pool, "series"),
                emongo:delete(test_pool, "ticks"),
                Series = probix_series:new_series(),
                binary_to_list(proplists:get_value(id, Series))
        end,
        fun(_) ->
                application:stop(probix),
                application:stop(emongo),
                application:stop(mochiweb),
                application:stop(log4erl)
        end,
        fun generate_series_update_test/1
    }.

generate_series_update_test(Id) when is_list(Id) ->
    [
     ?_assertMatch(
        {200, _},
        rest_req('POST', "/series/" ++ Id, ?JT2)
     ),
     ?_assertMatch(
        {200, _},
        rest_req('GET', "/series/" ++ Id ++ "?from=1")
       ),
     ?_assertMatch(
        {200, _},
        rest_req('GET', "/series/" ++ Id ++ "?to=3")
       ),
     ?_assertMatch(
        {200, _},
        rest_req('GET', "/series/" ++ Id ++ "?from=1&to=3")
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
        rest_req('DELETE', "/series/" ++ Id ++ "?from=1")
     ),
     ?_assertMatch(
        {200, _},
        rest_req('GET', "/series/" ++ Id)
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
