-module(probix_format).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% there are no atom_to_binary function in old versions of Erlang;
%% erlang:atom_to_binary/2 is available in Erlang R13A and newer.
atom_to_binary(A) when is_atom(A) -> list_to_binary( atom_to_list(A) ).

series_to_json(Rec) when is_record(Rec, series) ->
	Keys = [ atom_to_binary(K) || K <- record_info(fields, series) ],
	%% XXX undefined record labels are stored as undefined, but
	%% should be set to null according to json specification.
	[_Name | Values] = tuple_to_list(Rec),
	Encode = mochijson2:encoder([{utf8, true}]),
	list_to_binary( Encode({struct, lists:zip(Keys, Values)}) ).

series_to_csv(_Rec) -> ok.

tick_to_json_term(Rec) when is_record(Rec, tick) ->
	Keys = [ <<"timestamp">>, <<"value">> ],
	{_Series_id, Timestamp} = Rec#tick.id,
	Value = Rec#tick.value,
	{struct, lists:zip(Keys, [Timestamp, Value])};

tick_to_json_term(List) when is_list(List) ->
	[ tick_to_json_term(R) || R <- List ].

tick_to_json(Tick) ->
	Encode = mochijson2:encoder([{utf8, true}]),
	list_to_binary( Encode( tick_to_json_term(Tick) ) ).

tick_to_csv(_Rec) -> ok.

tick_from_json(Series_id, Json) ->
	try
		%json_term_to_tick( Series_id, mochijson2:decode(Json) )
		case mochijson2:decode(Json) of
			List when is_list(List) ->
				[ json_term_to_tick(Series_id, S) || S <- List ];
			Struct when is_tuple(Struct) ->
				[ json_term_to_tick(Series_id, Struct) ]
		end
	catch
		error:_ ->
			bad_json
	end.

json_term_to_tick(Series_id, {struct, Proplist}) ->
	%% XXX check input value correctness here
	#tick{
		id = {Series_id, proplists:get_value(<<"timestamp">>, Proplist)},
		value = proplists:get_value(<<"value">>, Proplist)
	}.

%json_term_to_tick(Series_id, List) when is_list(List) ->
%	[ json_term_to_tick(Series_id, S) || S <- List ].


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

gregorian_epoch_to_iso_8601(Epoch) ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Epoch),
	Deeplist = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		[Year, Month, Day, Hour, Min, Sec]),
	lists:flatten(Deeplist).

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
	S1, S2 | _Rest ]) ->
	Year = list_to_integer([Y1, Y2, Y3, Y4]),
	Month = list_to_integer([Mon1, Mon2]),
	Day = list_to_integer([D1, D2]),
	Hour = list_to_integer([H1, H2]),
	Min = list_to_integer([Min1, Min2]),
	Sec = list_to_integer([S1, S2]),
	calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}});

iso_8601_to_gregorian_epoch(_Whatever) -> bad_timestamp.
