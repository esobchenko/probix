-module(probix_db).
-compile(export_all).
-include("probix.hrl").

start() ->
    probix_db_mnesia:start().

stop() ->
    probix_db_mnesia:stop().

new_series(Label) when is_binary(Label) ->
    probix_db_mnesia:new_series(Label).

all_series() ->
    probix_db_mnesia:all_series().

delete_series(Id) when is_binary(Id) ->
    probix_db_mnesia:delete_series(Id).

series(Id) when is_binary(Id) ->
    probix_db_mnesia:series(Id).

add_ticks(Series_id, Values) ->
    probix_db_mnesia:add_ticks(Series_id, Values).

get_ticks(Series_id) when is_binary(Series_id) ->
    probix_db_mnesia:get_ticks(Series_id).

get_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    probix_db_mnesia:get_ticks(Series_id, {from, From});

get_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    probix_db_mnesia:get_ticks(Series_id, {to, To});

get_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    probix_db_mnesia:get_ticks(Series_id, {From, To}).

delete_tick(Id) ->
    probix_db_mnesia:delete_tick(Id).

delete_ticks(Series_id) when is_binary(Series_id) ->
    probix_db_mnesia:delete_ticks(Series_id).

delete_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    probix_db_mnesia:delete_ticks(Series_id, {from, From});

delete_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    probix_db_mnesia:delete_ticks(Series_id, {to, To});

delete_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    probix_db_mnesia:delete_ticks(Series_id, {From, To}).




