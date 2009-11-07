-module(probix_format_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SJ1, #series{id=1,time_created=1,label=1}).
-define(JS1, <<"[{\"id\":1,\"time_created\":1,\"label\":1}]">>).

-define(JTL1, <<"[{\"timestamp\":1,\"value\":1}]">>).

-define(TJL1, [#tick{id={undefined,{timestamp,1970,1,1,0,0,1,0,{timezone,0,0}}},value=1}]).
-define(TICKS_TO_JSON_LIST, <<"[{\"timestamp\":\"1970-01-01 00:00:01Z\",\"value\":1}]">>).
-define(TJR1, #tick{id={undefined,{timestamp,1970,1,1,0,0,1,0,{timezone,0,0}}},value=1}).
-define(TICKS_TO_JSON_RECORD, <<"{\"timestamp\":\"1970-01-01 00:00:01Z\",\"value\":1}">>).



series_to_json_test_() ->
	[
		?_assertEqual(?JS1, list_to_binary(probix_format:series_to_json(?SJ1)))
	].

tick_transform_test_() ->
	[
		?_assertEqual(?TICKS_TO_JSON_LIST, list_to_binary(probix_format:ticks_to_json(?TJL1))),
		?_assertEqual(?TICKS_TO_JSON_LIST, list_to_binary(probix_format:ticks_to_json(?TJR1))),
		?_assertEqual({ok, ?TJL1}, probix_format:ticks_from_json(?JTL1)),
		?_assertEqual({ok, ?TJL1}, probix_format:ticks_from_json(?TICKS_TO_JSON_RECORD)),
		?_assertEqual({error, bad_json}, probix_format:ticks_from_json(1))
	].
