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
                probix_series:delete_series(proplists:get_value(id, hd(probix_series:all_series())))
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
                probix_series:delete_ticks(proplists:get_value(id, hd(probix_series:all_series())), {})
               ),
             %% after delete we should have one
             ?_assertMatch(
                [],
                probix_series:all_series()
               )
            ]
      end
	}.


