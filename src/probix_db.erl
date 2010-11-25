-module(probix_db).
-compile(export_all).
-include("probix.hrl").

start() ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:start().

stop() ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:stop().

new_series(Label) when is_binary(Label) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:new_series(Label).

all_series() ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:all_series().

delete_series(Id) when is_binary(Id) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:delete_series(Id).

series(Id) when is_binary(Id) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:series(Id).

add_ticks(Series_id, Values) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:add_ticks(Series_id, Values).

get_ticks(Series_id) when is_binary(Series_id) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:get_ticks(Series_id).

get_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:get_ticks(Series_id, {from, From});

get_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:get_ticks(Series_id, {to, To});

get_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:get_ticks(Series_id, {From, To}).

delete_tick(Id) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:delete_tick(Id).

delete_ticks(Series_id) when is_binary(Series_id) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:delete_ticks(Series_id).

delete_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:delete_ticks(Series_id, {from, From});

delete_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:delete_ticks(Series_id, {to, To});

delete_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    {ok, Backend} = application:get_env(probix, db_backend),
    Backend:delete_ticks(Series_id, {From, To}).




