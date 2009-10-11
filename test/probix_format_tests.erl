-module(probix_format_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EPOCH_SECONDS, 62167219200).
-define(EPOCH_ISO8601, "1970-01-01 00:00:00").

-define(SJ1, #series{id=1,time_created=1,label=1}).
-define(JS1, <<"{\"id\":1,\"time_created\":1,\"label\":1}">>).

-define(JT1, <<"{\"timestamp\":1,\"value\":1}">>).
-define(TJ1, #tick{id={1,1},value=1}).

atom_to_binary_test_() ->
	[
		?_assertEqual(<<"foo">>, probix_format:atom_to_binary(foo))
	].

time_transform_test_() ->
	[
		?_assertEqual(?EPOCH_ISO8601, probix_format:gregorian_epoch_to_iso_8601(?EPOCH_SECONDS)),
		?_assertEqual(?EPOCH_SECONDS, probix_format:iso_8601_to_gregorian_epoch(?EPOCH_ISO8601)),
		?_assertEqual(bad_timestamp, probix_format:iso_8601_to_gregorian_epoch("foo"))
	].

series_to_json_test_() ->
	[
		?_assertEqual(?JS1, probix_format:series_to_json(?SJ1))
	].

tick_transform_test_() ->
	[
		?_assertEqual(?JT1, probix_format:tick_to_json(?TJ1)),
		?_assertEqual([?TJ1], probix_format:tick_from_json(1, ?JT1)),
		?_assertEqual(bad_json, probix_format:tick_from_json(1, 1))
	].
