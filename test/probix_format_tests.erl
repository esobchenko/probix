-module(probix_format_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SJ1, #series{id=1,time_created=1,label=1}).
-define(JS1, <<"[{\"id\":1,\"time_created\":1,\"label\":1}]">>).

-define(JTL1, <<"[{\"timestamp\":1,\"value\":1}]">>).
-define(TJL1, [#tick{id={1,1},value=1}]).

-define(JTR1, <<"{\"timestamp\":1,\"value\":1}">>).
-define(TJR1, #tick{id={1,1},value=1}).


series_to_json_test_() ->
	[
		?_assertEqual(?JS1, list_to_binary(probix_format:series_to_json(?SJ1)))
	].

tick_transform_test_() ->
	[
		?_assertEqual(?JTL1, list_to_binary(probix_format:ticks_to_json(?TJL1))),
		?_assertEqual(?JTL1, list_to_binary(probix_format:ticks_to_json(?TJR1))),
		?_assertEqual({ok, ?TJL1}, probix_format:ticks_from_json(1, ?JTL1)),
		?_assertEqual({ok, ?TJL1}, probix_format:ticks_from_json(1, ?JTR1)),
		?_assertEqual({error, bad_json}, probix_format:ticks_from_json(1, 1))
	].
