-module(probix_format).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

series_to_json_object(Rec) when is_record(Rec, series) ->
	Keys = record_info(fields, series),
	[_Name | Values] = tuple_to_list(Rec),
	{struct, lists:zip(Keys, Values)}.

series_to_json(Series) ->
	List = case Series of
		R when is_record(R, series) ->
			[ series_to_json_object(R) ];
		L when is_list(L) ->
			[ series_to_json_object(R) || R <- L ]
		end,
	Encode = mochijson2:encoder([{utf8, true}]),
	Encode(List).

tick_to_json_object(Rec) when is_record(Rec, tick) ->
	Keys = [ <<"timestamp">>, <<"value">> ],
	{_Series_id, Timestamp} = Rec#tick.id,
	Value = Rec#tick.value,
	{struct, lists:zip(Keys, [Timestamp, Value])}.

ticks_to_json(Ticks) ->
	List = case Ticks of
		R when is_record(R, tick) ->
			[ tick_to_json_object(R) ];
		L when is_list(L) ->
			[ tick_to_json_object(R) || R <- L ]
	end,
	Encode = mochijson2:encoder([{utf8, true}]),
	Encode(List).

ticks_from_json(Series_id, Json) ->
	try
		List = case mochijson2:decode(Json) of
			L when is_list(L) ->
				[ json_object_to_tick(Series_id, S) || S <- L ];
			T when is_tuple(T) ->
				[ json_object_to_tick(Series_id, T) ]
		end,
		{ok, List}
	catch
		error:_ ->
			{error, bad_json}
	end.

json_object_to_tick(Series_id, {struct, Proplist}) ->
	%% XXX check input value correctness here
	#tick{
		id = {Series_id, proplists:get_value(<<"timestamp">>, Proplist)},
		value = proplists:get_value(<<"value">>, Proplist)
	}.

ticks_to_csv(_Ticks) -> ok.
ticks_from_csv(_Series_id, _Csv) -> ok.

