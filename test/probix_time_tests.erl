-module(probix_time_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(timestamp, {year, month, day, hour, minute, second, fraction, timezone}).
-record(timezone, {hour, minute}).

t() -> probix_time:new().

from_iso8601_test_() ->
	[
		?_assertEqual(
			{ok, (t())#timestamp{ year=1997 }},
			probix_time:from_iso8601("1997")
		),
		?_assertEqual(
			{ok, (t())#timestamp{ year=1997, month=7 }},
			probix_time:from_iso8601("1997-07")
		),
		?_assertEqual(
			{ok, (t())#timestamp{ year=1997, month=7, day=16 }},
			probix_time:from_iso8601("1997-07-16")
		),
		?_assertEqual(
			{ok, (t())#timestamp{
				year=1997,
				month=7,
				day=16,
				hour=19,
				minute=20,
				timezone= #timezone{ hour=1, minute=0 }
			}},
			probix_time:from_iso8601("1997-07-16T19:20+01:00")
		),
		?_assertEqual(
			{ok, (t())#timestamp{
				year=1997,
				month=7,
				day=16,
				hour=19,
				minute=20,
				second=30,
				timezone= #timezone{ hour=1, minute=0 }
			}},
			probix_time:from_iso8601("1997-07-16T19:20:30+01:00")
		),
		?_assertEqual(
			{ok, (t())#timestamp{
				year=1997,
				month=7,
				day=16,
				hour=19,
				minute=20,
				second=30,
				fraction=45,
				timezone= #timezone{ hour=1, minute=0 }
			}},
			probix_time:from_iso8601("1997-07-16T19:20:30.45+01:00")
		),
		?_assertEqual(
			{error, bad_input},
			probix_time:from_iso8601("foobar")
		),
		?_assertEqual(
			{error, bad_input},
			probix_time:from_iso8601("2007-02-29")
		)
	].

from_dateime_test_() ->
	[
		?_assertEqual(
			{ok, (t())#timestamp{
				year=2009,
				month=7,
				day=16,
				hour=13
			}},
			probix_time:from_datetime({{2009,7,16},{13,0,0}})
		)
	].

from_unix_epoch_test_() ->
	[
		?_assertEqual(
			{ok, (t())#timestamp{
				year=1970,
				month=1,
				day=1
			}},
			probix_time:from_unix_epoch("0.0")
		)
	].

to_datetime_test_() ->
	[
		?_assertEqual(
			{{2009,7,16},{13,0,0}},
			probix_time:to_datetime((t())#timestamp{
				year=2009,
				month=7,
				day=16,
				hour=13
			})
		)
	].

to_unix_epoch_test_() ->
	[
		?_assertEqual(
			"0.0",
			probix_time:to_unix_epoch((t())#timestamp{
				year=1970,
				month=1,
				day=1
			})
		)
	].

to_tz_test_() ->
	[
		?_assertEqual(
			(t())#timestamp{
				year=2000,
				month=1,
				day=1,
				hour=2,
                timezone = #timezone{hour=2,minute=0}
			},
			probix_time:to_tz(
				(t())#timestamp{
					year=2000,
					month=1,
					day=1
				},
				#timezone{hour=2,minute=0}
			)
		)
	].

