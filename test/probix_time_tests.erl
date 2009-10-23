-module(probix_time_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(timestamp, {year, month, day, hour, minute, second, fraction, timezone}).
-record(timezone, {hour, minute}).

t() ->
	#timestamp{
		year = 0,
		month = 1,
		day = 1,
		hour = 0,
		minute = 0,
		second = 0,
		fraction = 0,
		timezone = #timezone{ hour = 0, minute = 0}
	}.

-define(CI1, "1997").
-define(CR1, (t())#timestamp{ year=1997 }).
-define(CI2, "1997-07").
-define(CR2, (t())#timestamp{ year=1997, month=7 }).
-define(CI3, "1997-07-16").
-define(CR3, (t())#timestamp{ year=1997, month=7, day=16 }).
-define(CI4, "1997-07-16T19:20+01:00").
-define(CR4,
		(t())#timestamp{
			year=1997,
			month=7,
			day=16,
			hour=19,
			minute=20,
			timezone= #timezone{ hour=1, minute=0 }
		}
).
-define(CI5, "1997-07-16T19:20:30+01:00").
-define(CR5,
		(t())#timestamp{
			year=1997,
			month=7,
			day=16,
			hour=19,
			minute=20,
			second=30,
			timezone= #timezone{ hour=1, minute=0 }
		}
).
-define(CI6, "1997-07-16T19:20:30.45+01:00").
-define(CR6,
		(t())#timestamp{
			year=1997,
			month=7,
			day=16,
			hour=19,
			minute=20,
			second=30,
			fraction=45,
			timezone= #timezone{ hour=1, minute=0 }
		}
).

from_iso8601_test_() ->
	[
		?_assertEqual(
			{ok, ?CR1},
			probix_time:from_iso8601(?CI1)
		),
		?_assertEqual(
			{ok, ?CR2},
			probix_time:from_iso8601(?CI2)
		),
		?_assertEqual(
			{ok, ?CR3},
			probix_time:from_iso8601(?CI3)
		),
		?_assertEqual(
			{ok, ?CR4},
			probix_time:from_iso8601(?CI4)
		),
		?_assertEqual(
			{ok, ?CR5},
			probix_time:from_iso8601(?CI5)
		),
		?_assertEqual(
			{ok, ?CR6},
			probix_time:from_iso8601(?CI6)
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

to_datetime_test_() ->
	[
	].

