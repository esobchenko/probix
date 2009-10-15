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

%% there are no Erlang functions for the unix epoch, but there are functions
%% for gregorean epoch in Erlang's calendar module. We will use this
%% offset to convert grigorean epoch to the unix epoch and vice versa.
unix_epoch_offset() -> 62167219200.

unix_to_gregorian_epoch(Epoch) -> Epoch + unix_epoch_offset().
gregorian_to_unix_epoch(Epoch) -> Epoch - unix_epoch_offset().

now_to_unix_epoch() ->
	now_to_gregorian_epoch() - unix_epoch_offset().

now_to_gregorian_epoch() ->
	calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time( now() ) ).

gregorian_epoch_to_iso_8601(Epoch) when is_integer(Epoch) ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Epoch),
	Deeplist = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		[Year, Month, Day, Hour, Min, Sec]),
	lists:flatten(Deeplist);

gregorian_epoch_to_iso_8601(Epoch) when is_float(Epoch) ->
	Int = trunc(Epoch),
	Timestamp = gregorian_epoch_to_iso_8601(Int),
	Frac = string:sub_string(
		lists:flatten(
			io_lib:format("~.3f", [Epoch - Int])
		),
	2),
	string:concat(Timestamp, Frac).

iso_8601_to_gregorian_epoch(Date) when is_binary(Date) ->
	iso_8601_to_gregorian_epoch( binary_to_list(Date) );

iso_8601_to_gregorian_epoch([
	Y1, Y2, Y3, Y4,
	$-,
	Mon1, Mon2,
	$-,
	D1, D2,
	_, % T or space
	H1, H2,
	$:,
	Min1, Min2,
	$:,
	S1, S2 | Frac ]) ->
	Ymd = list_to_tuple([ list_to_integer(L) || L <- [
			[Y1, Y2, Y3, Y4],
			[Mon1, Mon2],
			[D1, D2]
		]]),
	Hms = list_to_tuple([ list_to_integer(L) || L <- [
			[H1, H2],
			[Min1, Min2],
			[S1, S2]
		]]),
	calendar:datetime_to_gregorian_seconds({Ymd, Hms}) + frac(Frac).

frac([]) ->
	0.0;

frac(Frac) ->
	{Float, _} = string:to_float( string:concat("0", Frac) ),
	Float.

