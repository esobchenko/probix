-module(probix_series_tests).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

series_crud_test_() ->
    {
      setup,
      fun() ->
              error_logger:tty(false),
              application:start(inets),
              application:start(crypto),
              application:start(emongo),
              application:start(log4erl),
              application:start(mochiweb),
              application:start(probix),
              emongo:delete(test_pool, "series"),
              emongo:delete(test_pool, "ticks")
      end,
      fun(_) ->
              ok
      end,
      fun(_) ->
            [% no series so far
             ?_assertMatch(
                [],
                probix_series:all_series()
               ),
             % creating new
             ?_assertMatch(
                [{id, _}, {time_created, _}, {label, <<>>}],
                probix_series:new_series()
               ),
             % creating new with label
             ?_assertMatch(
                [{id, _}, {time_created, _}, {label, <<"Label">>}],
                probix_series:new_series("Label")
               ),
             % all series should now have 2 series
             ?_assertMatch(
                [
                 [{id, _}, {time_created, _}, {label, <<>>}],
                 [{id, _}, {time_created, _}, {label, <<"Label">>}]
                ],
                probix_series:all_series()
               ),
             % deleting series
             ?_assertMatch(
                ok,
                probix_series:delete_series(
                  proplists:get_value(id,
                                      hd(probix_series:all_series())))
               ),
             %% after delete we should have one
             ?_assertMatch(
                [
                 [{id, _}, {time_created, _}, {label, _}]
                ],
                probix_series:all_series()
               ),
             %% deleting ticks without range should delete series
             ?_assertMatch(
                ok,
                probix_series:delete_ticks(
                  proplists:get_value(id,
                                      hd(probix_series:all_series())), {})
               ),
             %% after final delete we should have no series
             ?_assertMatch(
                [],
                probix_series:all_series()
               )
            ]
      end
    }.

tick_crud_test_() ->
    {
      setup,
      fun() ->
              error_logger:tty(false),
              application:start(inets),
              application:start(crypto),
              application:start(emongo),
              application:start(log4erl),
              application:start(mochiweb),
              application:start(probix),
              emongo:delete(test_pool, "series"),
              emongo:delete(test_pool, "ticks"),
              Series = probix_series:new_series(),
              proplists:get_value(id, Series)
      end,
      fun(_) ->
              ok
      end,
      fun(Series_id) ->
              [
               %% checking that no ticks returned
               ?_assertMatch(
                  [],
                  probix_series:get_ticks(Series_id)
                 ),
               %% adding ticks
               ?_assertMatch(
                  ok,
                  probix_series:add_ticks(Series_id, [[{timestamp, {timestamp,1970,1,1,0,0,1,0,{timezone,0,0}} }, {value, 1}]])
                 ),
               %% checking ticks added
               ?_assertMatch(
                  [[{series_id, Series_id}, {timestamp, {timestamp,1970,1,1,0,0,1,0,{timezone,0,0}} }, {value, 1}]],
                  probix_series:get_ticks(Series_id)
                 ),
               %% adding more ticks
               ?_assertMatch(
                  ok,
                  probix_series:add_ticks(Series_id, [[{timestamp, {timestamp,1970,1,1,0,0,2,0,{timezone,0,0}} }, {value, 1}],
                                                      [{timestamp, {timestamp,1970,1,1,0,0,3,0,{timezone,0,0}} }, {value, 1}],
                                                      [{timestamp, {timestamp,1970,1,1,0,0,4,0,{timezone,0,0}} }, {value, 1}]])
                 ),

               %% checking ranges - from
               ?_assertMatch(
                  [[{series_id, Series_id}, {timestamp, {timestamp,1970,1,1,0,0,4,0,{timezone,0,0}} }, {value, 1}]],
                  probix_series:get_ticks(Series_id, {from, {timestamp,1970,1,1,0,0,4,0,{timezone,0,0}} })
                 ),
               %% to
               ?_assertMatch(
                  [[{series_id, Series_id}, {timestamp, {timestamp,1970,1,1,0,0,1,0,{timezone,0,0}} }, {value, 1}]],
                  probix_series:get_ticks(Series_id, {to, {timestamp,1970,1,1,0,0,2,0,{timezone,0,0}} })
                 ),
               %% from - to
               ?_assertMatch(
                  [[{series_id, Series_id}, {timestamp, {timestamp,1970,1,1,0,0,2,0,{timezone,0,0}} }, {value, 1}]],
                  probix_series:get_ticks(Series_id, {{timestamp,1970,1,1,0,0,2,0,{timezone,0,0}}, {timestamp,1970,1,1,0,0,3,0,{timezone,0,0}} })
                 ),

               %% deleting ticks - from
               ?_assertMatch(
                  ok,
                  probix_series:delete_ticks(Series_id, {from, {timestamp,1970,1,1,0,0,4,0,{timezone,0,0}} })
                 ),
               %% get ticks should not return deleted
               ?_assertMatch(
                  [],
                  probix_series:get_ticks(Series_id, {from, {timestamp,1970,1,1,0,0,4,0,{timezone,0,0}} })
                 ),

               %% deleting ticks - to
               ?_assertMatch(
                  ok,
                  probix_series:delete_ticks(Series_id, {to, {timestamp,1970,1,1,0,0,2,0,{timezone,0,0}} })
                 ),
               %% get ticks should not return deleted
               ?_assertMatch(
                  [],
                  probix_series:get_ticks(Series_id, {to, {timestamp,1970,1,1,0,0,2,0,{timezone,0,0}} })
                 ),

               %% deleting ticks from-to
               ?_assertMatch(
                  ok,
                  probix_series:delete_ticks(Series_id, {{timestamp,1970,1,1,0,0,2,0,{timezone,0,0}}, {timestamp,1970,1,1,0,0,3,0,{timezone,0,0}} })
                 ),
               ?_assertMatch(
                  [],
                  probix_series:get_ticks(Series_id, {{timestamp,1970,1,1,0,0,2,0,{timezone,0,0}}, {timestamp,1970,1,1,0,0,3,0,{timezone,0,0}} })
                 ),

               %% now we should have one tick
               ?_assertMatch(
                  [[{series_id, Series_id}, {timestamp, {timestamp,1970,1,1,0,0,3,0,{timezone,0,0}} }, {value, 1}]],
                  probix_series:get_ticks(Series_id)
                 ),

               %% deleting everything else
               ?_assertMatch(
                  ok,
                  probix_series:delete_ticks(Series_id)
                 ),
               %% should not have anything at all
               ?_assertMatch(
                  [],
                  probix_series:get_ticks(Series_id)
                 ),
               %% and no series 
               ?_assertMatch(
                  {error, not_found},
                  probix_series:series(Series_id)
                 )
               
              ]
      end
    }.
