-module(probix_series).
-behaviour(gen_server).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% series functions

create_series() -> ok.
get_series() -> ok.
delete_series(Series_id) -> ok.

%% probe functions

add_probes(Series_id, List) when is_list(List) -> ok;

add_probe(Series_id, Rec) when is_record(probe, Rec) -> ok.

get_probes(Series_id, {from, Timestamp}) -> ok.
get_probes(Series_id, {to, Timestamp}) -> ok.
get_probes(Series_id, {From, To}) -> ok.

delete_probes({from, Timestamp})-> ok;
delete_probes({to, Timestamp}) -> ok;
delete_probes({From, To}) -> ok.

delete_probe(Timestamp) when is_integer(Timestamp) -> ok.
