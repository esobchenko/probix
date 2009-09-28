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

probe_record_from(json, Json, Series_id) -> ok.
probe_record_from(csv, Csv, Series_id) -> ok.


%% There are no Erlang functions to work with the unix epoch, but there
%% are functions for gregorean epoch in calendar module. We will use this
%% offset to convert grigorean epoch to the unix epoch and vice versa.
unix_epoch_offset() -> 62167219200.

unix_epoch() ->
	calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time( now() ) ) - unix_epoch_offset().

