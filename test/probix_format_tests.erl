-module(probix_format_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SJ1, [[{id, 1}, {time_created,{timestamp,1970,1,1,0,0,1,0,{timezone,0,0}}}, {label,1}]]).
-define(JS1, <<"[{\"id\":1,\"time_created\":\"1970-01-01 00:00:01Z\",\"label\":1}]">>).

-define(JTL1, <<"[{\"timestamp\":1,\"value\":1}]">>).
-define(TJL1, [ [ {series_id, undefined}, {timestamp, {timestamp,1970,1,1,0,0,1,0,{timezone,0,0}} }, {value, 1} ] ].

-define(JSON_TO_TICKS, <<"[{\"series_id\":\"foo\",\"timestamp\":\"1970-01-01 00:00:01Z\",\"value\":1}]">>).
-define(TICKS_TO_JSON, [ [ {series_id, <<"foo">>}, {timestamp, {timestamp,1970,1,1,0,0,1,0,{timezone,0,0}} }, {value, 1} ] ].

series_to_json_test_() ->
    {
      setup,
      fun() ->
                application:start(inets),
                application:start(crypto),
                application:start(log4erl),
                application:start(mochiweb),
                application:start(emongo),
                application:start(probix)
      end,
      fun(_) ->
              application:stop(probix),
              application:stop(emongo),
              application:stop(mochiweb),
              application:stop(log4erl)
      end,
      [
       ?_assertEqual(?JS1, list_to_binary(probix_format:series_to_json(?SJ1)))
      ]}.

tick_transform_test_() -> {
                      setup,
                      fun() ->
                              application:start(inets),
                              application:start(crypto),
                              application:start(log4erl),
                              application:start(mochiweb),
                              application:start(emongo),
                              application:start(probix)
                      end,
                      fun(_) ->
                              application:stop(probix),
                              application:stop(emongo),
                              application:stop(mochiweb),
                              application:stop(log4erl)
                      end,
                      [
     ?_assertEqual(?JSON_TO_TICKS, list_to_binary(probix_format:ticks_to_json(?TICKS_TO_JSON))),
     ?_assertEqual({ok, ?TJL1}, probix_format:ticks_from_json(?JTL1)),
     ?_assertEqual({error, bad_json}, probix_format:ticks_from_json(1))
	]}.
