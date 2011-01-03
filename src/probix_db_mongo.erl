-module(probix_db_mongo).
-compile(export_all).
-include("probix.hrl").

-ifdef(TEST).
-define(POOL, test_pool).
-else.
-define(POOL, production_pool).
-endif.

init() ->
    {ok, undefined}.

terminate() ->
    ok.

new_series(Label) when is_binary(Label) ->
    Series = [ { id, probix_util:random_string(10) },
               { time_created, probix_time:to_secs_tuple(probix_time:now()) },
               { label, Label } ],
    emongo:insert(?POOL, "series", Series),
    Series.

all_series() ->
    Series = emongo:find(?POOL, "series"),
    [
     [ 
       { id, proplists:get_value(<<"id">>, S) },
       { time_created, probix_time:from_secs_tuple(proplists:get_value(<<"time_created">>, S)) },
       { label, proplists:get_value(<<"label">>, S) } 
     ] || S <- Series
    ].

delete_series(Id) when is_binary(Id) ->
    emongo:delete(?POOL, "series", [{"id", Id}]).

series(Id) when is_binary(Id) ->
    case emongo:find(?POOL, "series", [{"id", Id}]) of
        [] ->
            {error, not_found};
        Res ->
            {ok, Res}
    end.

add_ticks(Series_id, List) when is_list(List) ->
    Ticks = lists:map(
              fun(T) ->
                      [ 
                        { series_id, Series_id },
                        { timestamp, probix_time:to_secs_tuple(proplists:get_value(timestamp, T)) },
                        { value, proplists:get_value(value, T) } 
                      ]
              end,
              List),
    emongo:insert(?POOL, "ticks", Ticks).

get_ticks(Series_id) when is_binary(Series_id) ->
    Ticks = emongo:find(?POOL, "ticks", [{ "series_id", Series_id }]),
    lists:map(
      fun(T) ->
              [
               { series_id, proplists:get_value(<<"series_id">>, T)},
               { timestamp, probix_time:from_secs_tuple(proplists:get_value(<<"timestamp">>, T)) },
               { value, proplists:get_value(<<"value">>, T) }
              ]
      end, Ticks).

get_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    emongo:find(?POOL, "ticks", [ { "series_id", Series_id },
                                  { "timestamp", [ {'>=', probix_time:to_secs_tuple(From) } ] } ]);

get_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    emongo:find(?POOL, "ticks", [ { "series_id", Series_id },
                                  { "timestamp", [ {'<', probix_time:to_secs_tuple(To) } ] } ]);

get_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    emongo:find(?POOL, "ticks", [ { "series_id", Series_id },
                                  { "timestamp", [ {'>=', probix_time:to_secs_tuple(From) }, {'<', probix_time:to_secs_tuple(To) } ] } ] ).

% delete_ticks(Series_id) when is_binary(Series_id) ->
%    probix_db_mnesia:delete_ticks(Series_id).

% delete_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
%     probix_db_mnesia:delete_ticks(Series_id, {from, From});

% delete_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
%     probix_db_mnesia:delete_ticks(Series_id, {to, To});

%delete_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
%    probix_db_mnesia:delete_ticks(Series_id, {From, To}).
