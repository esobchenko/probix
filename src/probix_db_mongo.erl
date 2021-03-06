-module(probix_db_mongo).
-compile(export_all).
-include("probix.hrl").

-ifdef(TEST).
-define(POOL, test_db_pool).
-else.
-define(POOL, prod_db_pool).
-endif.

init() ->
    %% adding default pool if not pools set up in config
    %% otherwise emongo will init by itself
    case application:get_env(emongo, pools) of 
        undefined ->
            emongo:add_pool(test_db_pool, "localhost", 27017, "probix_test_db", 1);
        {ok, _Pools} ->
            false
    end,
    {ok, undefined}.

terminate() ->
    ok.

clean_db() ->
    emongo:delete(?POOL, "series"),
    emongo:delete(?POOL, "ticks").

new_series(Label) when is_binary(Label) ->
    Id = probix_util:random_string(10),
    Series = [ { id, Id },
               { time_created, probix_time:to_secs_tuple(probix_time:now()) },
               { label, Label } ],
    emongo:insert(?POOL, "series", Series),
    [ S ] = emongo:find_one(?POOL, "series", [{"id", Id}]),
    series_to_proplist(S).

all_series() ->
    Series = emongo:find_all(?POOL, "series"),
    [
     series_to_proplist(S) || S <- Series
    ].

delete_series(Id) when is_binary(Id) ->
    emongo:delete(?POOL, "series", [{"id", Id}]).

series(Id) when is_binary(Id) ->
    case emongo:find_one(?POOL, "series", [{"id", Id}]) of
        [] ->
            {error, not_found};
        [ Res ] ->
            {ok, series_to_proplist(Res)}
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
    ticks_to_proplist(emongo:find_all(?POOL, "ticks", [{ "series_id", Series_id }])).

get_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    ticks_to_proplist(emongo:find_all(?POOL, "ticks",
        [
          { "series_id", Series_id },
          { "timestamp", [
                           {'>=', probix_time:to_secs_tuple(From) }
                         ] }
        ]));

get_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    ticks_to_proplist(
      emongo:find_all(?POOL, "ticks",
        [
          { "series_id", Series_id },
          { "timestamp", [
                           {'<', probix_time:to_secs_tuple(To) }
                         ] }
        ]));

get_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    ticks_to_proplist(emongo:find_all(?POOL, "ticks",
        [
          { "series_id", Series_id },
          { "timestamp", [
                           {'>=', probix_time:to_secs_tuple(From) },
                           {'<', probix_time:to_secs_tuple(To) }
                         ] }
        ])).

delete_ticks(Series_id) when is_binary(Series_id) ->
    emongo:delete(?POOL, "ticks",
        [
          { "series_id", Series_id }
        ]),
    emongo:delete(?POOL, "series",
        [
          { "id", Series_id }
        ]).

delete_ticks(Series_id, {from, From}) when is_binary(Series_id), is_record(From, timestamp) ->
    emongo:delete(?POOL, "ticks",
        [
          { "series_id", Series_id },
          { "timestamp", [
                           {'>=', probix_time:to_secs_tuple(From) }
                         ] }
        ]);

delete_ticks(Series_id, {to, To}) when is_binary(Series_id), is_record(To, timestamp) ->
    emongo:delete(?POOL, "ticks",
        [
          { "series_id", Series_id },
          { "timestamp", [
                           {'<', probix_time:to_secs_tuple(To) }
                         ] }
        ]);

delete_ticks(Series_id, {From, To}) when is_binary(Series_id), is_record(From, timestamp), is_record(To, timestamp) ->
    emongo:delete(?POOL, "ticks",
        [
          { "series_id", Series_id },
          { "timestamp", [
                           {'>=', probix_time:to_secs_tuple(From) },
                           {'<', probix_time:to_secs_tuple(To) }
                         ] }
        ]).

ticks_to_proplist(Ticks) when is_list(Ticks) ->
    lists:map(
      fun(T) ->
              [
               { series_id, proplists:get_value(<<"series_id">>, T)},
               { timestamp, probix_time:from_secs_tuple(proplists:get_value(<<"timestamp">>, T)) },
               { value, proplists:get_value(<<"value">>, T) }
              ]
      end, Ticks).

series_to_proplist(Series) when is_list(Series) ->
    [ 
       { id, proplists:get_value(<<"id">>, Series) },
       { time_created, probix_time:from_secs_tuple(proplists:get_value(<<"time_created">>, Series)) },
       { label, proplists:get_value(<<"label">>, Series) } 
    ].

create_user(User) ->
    emongo:insert(?POOL, "users", [User]).
    
