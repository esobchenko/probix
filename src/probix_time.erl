-module(probix_time).
-compile(export_all).

-record(timestamp, {year, month, day, hour, minute, second, fraction, timezone}).
-record(timezone, {hour, minute}).

binary_to_integer(Binary) when is_binary(Binary) ->
	list_to_integer(binary_to_list(Binary)).

%% We support a subset of ISO 8601: http://www.w3.org/TR/NOTE-datetime

from_iso8601(Time) when is_list(Time) ->
	from_iso8601( list_to_binary(Time) );

from_iso8601(Time) when is_binary(Time) ->
	try
		{ok, parse_iso8601(
			year,
			Time,
			#timestamp{
				year = 0,
				month = 0,
				day = 0,
				hour = 0,
				minute = 0,
				second = 0,
				fraction = 0,
				timezone = #timezone{ hour = 0, minute = 0}
			})
		}
	catch
		error:_ -> {error, bad_input}
	end.

parse_iso8601( _State, <<>>, R ) -> R;

%% these are used to come from state of parsing time seconds or
%% fraction seconds to parsing timezone
parse_iso8601(second, <<"-", Rest/bitstring>>, R) ->
	parse_iso8601(tz_hour_minus, Rest, R );
parse_iso8601(fraction, <<"-", Rest/bitstring>>, R) ->
	parse_iso8601(tz_hour_minus, Rest, R );
parse_iso8601(_State, <<"+", Rest/bitstring>>, R) ->
	parse_iso8601(tz_hour_plus, Rest, R );
parse_iso8601(second, <<"Z", Rest/bitstring>>, R) ->
	parse_iso8601(second, Rest, R);
parse_iso8601(fraction, <<"Z", Rest/bitstring>>, R) ->
	parse_iso8601(second, Rest, R);

%% separator between date
parse_iso8601( State, <<"-", Rest/bitstring>>, R ) ->
	parse_iso8601( State, Rest, R );

%% separators between date and time
parse_iso8601( separator, <<" ", Rest/bitstring>>, R ) ->
	parse_iso8601( hour, Rest, R );
parse_iso8601( separator, <<"T", Rest/bitstring>>, R ) ->
	parse_iso8601( hour, Rest, R );

%% separator between time parts
parse_iso8601( State, <<":", Rest/bitstring>>, R ) ->
	parse_iso8601( State, Rest, R );

%% separators of fractional part
parse_iso8601( fraction, <<".", Rest/bitstring>>, R ) ->
	parse_iso8601( fraction, Rest, R );
parse_iso8601( fraction, <<",", Rest/bitstring>>, R ) ->
	parse_iso8601( fraction, Rest, R );

%% parsing date
parse_iso8601( year, <<Year:32/bitstring, Rest/bitstring>>, R ) ->
	parse_iso8601( month, Rest, R#timestamp{ year = binary_to_integer(Year) } );
parse_iso8601( month, <<Month:16/bitstring, Rest/bitstring>>, R ) ->
	parse_iso8601( day, Rest, R#timestamp{ month = binary_to_integer(Month) } );
parse_iso8601( day, <<Day:16/bitstring, Rest/bitstring>>, R ) ->
	parse_iso8601( separator, Rest, R#timestamp{ day = binary_to_integer(Day) } );

%% parsing time
parse_iso8601( hour, <<Hour:16/bitstring, Rest/bitstring>>, R ) ->
	parse_iso8601( minute, Rest, R#timestamp{ hour = binary_to_integer(Hour) } );
parse_iso8601( minute, <<Minute:16/bitstring, Rest/bitstring>>, R ) ->
	parse_iso8601( second, Rest, R#timestamp{ minute = binary_to_integer(Minute) } );
parse_iso8601( second, <<Second:16/bitstring, Rest/bitstring>>, R ) ->
	parse_iso8601( fraction, Rest, R#timestamp{ second = binary_to_integer(Second) } );

%% parsing fraction by character, cause we don't know its length
parse_iso8601( fraction, <<C:8/bitstring, Rest/bitstring>>, R) ->
	parse_iso8601( fraction, Rest, R#timestamp{ fraction = R#timestamp.fraction * 10 + binary_to_integer(C)});

%% parsing timezone info
parse_iso8601( tz_hour_plus, <<Tz_hour:16/bitstring, Rest/bitstring>>, R) ->
	parse_iso8601(
		tz_minute,
		Rest,
		R#timestamp{ timezone = (R#timestamp.timezone)#timezone{ hour = binary_to_integer(Tz_hour) }}
	);
parse_iso8601( tz_hour_minus, <<Tz_hour:16/bitstring, Rest/bitstring>>, R) ->
	parse_iso8601(
		tz_minute,
		Rest,
		R#timestamp{ timezone = (R#timestamp.timezone)#timezone{ hour = - binary_to_integer(Tz_hour) }}
	);
parse_iso8601( tz_minute, <<Tz_min:16/bitstring, Rest/bitstring>>, R) ->
	parse_iso8601(
		minute,
		Rest,
		R#timestamp{ timezone = (R#timestamp.timezone)#timezone{ minute = binary_to_integer(Tz_min) }}
	).

from_unix_epoch(_Time) -> ok.

to_datetime(_Time) -> ok.

to_gregorian_seconds(_Time) -> ok.

to_unix_seconds(_Time) -> ok.

to_utc(R) -> to_tz(R, #timezone{ hour = 0, minute = 0 }).

to_tz(_R, _Tz) -> ok.

cmp(_R1, _R2) -> ok.

format(_Format, _R) -> ok.

