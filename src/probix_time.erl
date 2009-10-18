-module(probix_time).
-compile(export_all).

%% R1 = probix_time:parse_iso8601(Iso_8601_date).
%% R2 = probix_time:parse_unix_epoch(Unix_epoch_date).

%% {Date, Time} = probix_time:to_datetime(R1).
%% Gregorian_seconds = probix_time:to_gregorian_seconds(R1).
%% Unix_seconds = probix_time:to_unix_seconds(R1).

%% String = probix_time:format("%Y-%M-%d %h:%m:%s", R1).

%% R3 = probix_time:tz_convert(R1, Tz). % Tz = {-2, 0}

%% probix_time:cmp(R1, R2). % returns 1 or 0 or -1

-record(timestamp, {year, month, day, hour, minute, second}).

parse_iso8601(Time) when is_list(Time) ->
	parse_iso8601( list_to_binary(Time) );

parse_iso8601(Time) when is_binary(Time) ->
	try
		{ok, iso_to_rec(
				year,
				Time,
				#timestamp{
					year = 0,
					day = 0,
					month = 0,
					hour = 0,
					minute = 0,
					second = 0
				}
			)
		}
	catch
		error:_ -> {error, not_iso8601}
	end.

iso_to_rec( _State, <<>>, R ) -> R;

iso_to_rec( State, <<"-", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );
iso_to_rec( State, <<" ", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );
iso_to_rec( State, <<"T", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );
iso_to_rec( State, <<":", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );

iso_to_rec( year, <<Year:32/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( month, Rest, R#timestamp{ year = Year } );
iso_to_rec( month, <<Month:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( day, Rest, R#timestamp{ month = Month } );
iso_to_rec( day, <<Day:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( hour, Rest, R#timestamp{ day = Day } );

iso_to_rec( hour, <<Hour:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( minute, Rest, R#timestamp{ hour = Hour } );
iso_to_rec( minute, <<Minute:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( second, Rest, R#timestamp{ minute = Minute } );
iso_to_rec( second, <<Second:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( fraction, Rest, R#timestamp{ second = Second } ).


parse_unix_epoch(_Time) -> ok.

to_datetime(_Time) -> ok.

to_gregorian_seconds(_Time) -> ok.

to_unix_seconds(_Time) -> ok.

tz_convert(_R, _Tz) -> ok.

cmp(_R1, _R2) -> ok.

format(_Format, _R) -> ok.

