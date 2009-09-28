-module(probix_formats).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% there are no atom_to_binary function in old versions of Erlang;
%% erlang:atom_to_binary/2 is available in Erlang R13A and newer.
atom_to_binary(A) when is_atom(A) -> list_to_binary( atom_to_list(A) ).

series_record_to(json, Red) when is_record(series, Rec) -> ok.
series_record_to(csv, Red) when is_record(series, Rec) -> ok.

probe_record_to(json, Rec) when is_record(probe, Rec) -> ok.
probe_record_to(csv, Rec) when is_record(probe, Rec) -> ok.

probe_record_from(json, Series_id, Json) -> ok.
probe_record_from(csv, Series_id, Csv) -> ok.

%% there are no Erlang functions for the unix epoch, but there are functions
%% for gregorean epoch in Erlang's calendar module. We will use this
%% offset to convert grigorean epoch to the unix epoch and vice versa.
unix_epoch_offset() -> 62167219200.

unix_to_gregorian_epoch(Epoch) -> Epoch + unix_epoch_offset().
gregorian_to_unix_epoch(Epoch) -> Epoch - unix_epoch_offset().

now_to_unix_epoch() ->
	calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time( now() ) ) - unix_epoch_offset().

gregorian_epoch_to_iso_8601(Epoch) ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = gregorian_seconds_to_datetime(Epoch),
	io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		[Year, Month, Day, Hour, Min, Sec]).

%% Fuck. It will be difficult.
iso_8601_to_gregorian_epoch(Iso_8601) -> ok.
