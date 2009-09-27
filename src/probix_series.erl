-module(probix_series).
-behaviour(gen_server).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% series functions

create() -> ok.
delete() -> ok.

read() -> ok.
read_all() -> ok.

%% probe functions

add_probes(List) when is_list(List) -> ok;
add_probes(Rec) -> ok.

del_probes(Timestamp) when is_integer(Timestamp) -> ok.
del_probes({from, Timestamp})-> ok.
del_probes({to, Timestamp}) -> ok.
del_probes({From, To}) -> ok.

get_probes({from, Timestamp}) -> ok.
get_probes({to, Timestamp}) -> ok.
get_probes({From, To}) -> ok.

probe_record_from(json, Rec) when is_record(probe, Rec) -> ok.
probe_record_to(json, Rec) when is_record(probe, Rec) -> ok.

