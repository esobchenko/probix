-module(probix_time).
-compile(export_all).

%% R1 = probix_time:parse_iso8601(Iso_8601_date).
%% R2 = probix_time:parse_unix_epoch(Unix_epoch_date).

%% {Date, Time} = probix_time:to_datetime(R1).
%% Gregorian_seconds = probix_time:to_gregorian_seconds(R1).
%% Unix_seconds = probix_time:to_unix_seconds(R1).

%% String = probix_time:format("%Y-%M-%d %h:%m:%s", R1).

%% R3 = probix_time:tz_convert(R1, Tz). % Tz = {-2, 0}

%% probix_time:cmp(R1, R2). % returns 1 or 0 or -1

parse_iso8601(_Time) -> ok.

parse_unix_epoch(_Time) -> ok.

to_datetime(_Time) -> ok.

to_gregorian_seconds(_Time) -> ok.

to_unix_seconds(_Time) -> ok.

tz_convert(_R, _Tz) -> ok.

cmp(_R1, _R2) -> ok.

format(_Format, _R) -> ok.

