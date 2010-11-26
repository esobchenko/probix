-module(probix_db_mongo).
-compile(export_all).
-include("probix.hrl").

init() ->
    {ok, undefined}.

terminate() ->
    ok.

new_series(Label) when is_binary(Label) ->
    Series = [ { "id", probix_util:random_string(10) },
               { "time_created", probix_time:to_secs_tuple(probix_time:now()) },
               { "label", Label } ],
    emongo:insert(pool1, "series", Series).

all_series() ->
    emongo:find(pool1, "series").

delete_series(Id) when is_binary(Id) ->
    emongo:delete(pool1, "series", [{"id", Id}]).

series(Id) when is_binary(Id) ->
    emongo:find(pool1, "series", [{"id", Id}]).

add_ticks(Series_id, Rec) when is_record(Rec, tick) ->
    Timestamp = element(2, Rec#tick.id),
    Tick = [ { "series_id", Series_id },
             { "timestamp", probix_time:to_secs_tuple(Timestamp) },
             { "value", Rec#tick.value } ],
    emongo:insert(pool1, "ticks", Tick),
    ok;

add_ticks(Series_id, List) when is_list(List) ->
    Ticks = lists:foreach(
      fun(T) ->
              Timestamp = element(2, T#tick.id),
              [ { "series_id", Series_id },
                { "timestamp", probix_time:to_secs_tuple(Timestamp) },
                { "value", T#tick.value } ]
      end,
      List),
    emongo:insert(pool1, "ticks", Ticks),
    ok.

get_ticks(Series_id) when is_binary(Series_id) ->
    emongo:find(pool1, "ticks", [{ "series_id", Series_id }]).

get_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    emongo:find(pool1, "ticks", [ { "series_id", Series_id },
                                  { "timestamp", [ {'>=', probix_time:to_secs_tuple(From) } ] } ]);

get_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    emongo:find(pool1, "ticks", [ { "series_id", Series_id },
                                  { "timestamp", [ {'<', probix_time:to_secs_tuple(To) } ] } ]);

get_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    emongo:find(pool1, "ticks", [ { "series_id", Series_id },
                                  { "timestamp", [ {'>=', probix_time:to_secs_tuple(From) }, {'<', probix_time:to_secs_tuple(To) } ] } ] ).

% delete_ticks(Series_id) when is_binary(Series_id) ->
%    probix_db_mnesia:delete_ticks(Series_id).

% delete_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
%     probix_db_mnesia:delete_ticks(Series_id, {from, From});

% delete_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
%     probix_db_mnesia:delete_ticks(Series_id, {to, To});

%delete_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
%    probix_db_mnesia:delete_ticks(Series_id, {From, To}).
