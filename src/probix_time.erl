-module(probix_time).
-compile(export_all).

%% We were obliged to write the own format for timestamps, since the standard {Date, Time}
%% format is not applicable for fractional seconds and timezone handling.

-include("probix.hrl").

%% there are no Erlang functions for the unix epoch, but there are functions
%% for gregorean epoch in Erlang's calendar module. We will use this
%% offset to convert grigorean epoch to the unix epoch and vice versa.
unix_seconds_offset() -> 62167219200.

unix_to_gregorian_seconds(Epoch) when is_integer(Epoch) -> Epoch + unix_seconds_offset().
gregorian_to_unix_seconds(Epoch) when is_integer(Epoch) -> Epoch - unix_seconds_offset().

validate(T) when is_record(T, timestamp) ->
	true = calendar:valid_date(
		T#timestamp.year,
		T#timestamp.month,
		T#timestamp.day
	),
	true = T#timestamp.hour >= 0,
	true = T#timestamp.hour =< 23,
	true = T#timestamp.second >= 0,
	true = T#timestamp.second =< 59,
	true = is_record(T#timestamp.timezone, timezone),
	%% timezone
	true = (T#timestamp.timezone)#timezone.hour >= -11,
	true = (T#timestamp.timezone)#timezone.hour =< 11,
	true = (T#timestamp.timezone)#timezone.minute >= 0,
	true = (T#timestamp.timezone)#timezone.minute =< 59,
	T.

binary_to_integer(Binary) when is_binary(Binary) ->
	list_to_integer(binary_to_list(Binary)).

new() ->
	#timestamp{
		year = 0,
		month = 1,
		day = 1,
		hour = 0,
		minute = 0,
		second = 0,
		fraction = 0,
		timezone = #timezone{ hour = 0, minute = 0}
	}.

%% We support a subset of ISO 8601: http://www.w3.org/TR/NOTE-datetime

from_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	try
		{ok, validate((new())#timestamp{
			year = Year,
			month = Month,
			day = Day,
			hour = Hour,
			minute = Minute,
			second = Second
		})}
	catch
		error:_ -> {error, bad_input}
	end.

from_iso8601(Time) when is_list(Time) ->
	from_iso8601( list_to_binary(Time) );

from_iso8601(Time) when is_binary(Time) ->
	try
		true = Time =/= <<"">>,
		{ok, parse_iso8601( year, Time, new() ) }
	catch
		error:_ -> {error, bad_input}
	end.

parse_iso8601( _State, <<>>, R ) -> validate(R);

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

from_unix_epoch(Epoch) when is_list(Epoch) ->
	from_unix_epoch(list_to_binary(Epoch));

from_unix_epoch(Epoch) when is_binary(Epoch) ->
	try
		true = Epoch =/= <<"">>,
		{ok, parse_unix_epoch(int, Epoch, 0, 0) }
	catch
		error:_ -> {error, bad_input}
	end.

parse_unix_epoch(_State, <<>>, Int, Frac) ->
	{ok, R} = from_datetime(
		calendar:gregorian_seconds_to_datetime(
			unix_to_gregorian_seconds(Int)
		)
	),
	validate( R#timestamp{ fraction = Frac } );

parse_unix_epoch(int, <<".", Rest/bitstring>>, Int, Frac) ->
	parse_unix_epoch(frac, Rest, Int, Frac);

parse_unix_epoch(int, <<C:8/bitstring, Rest/bitstring>>, Int, Frac) ->
	parse_unix_epoch(int, Rest, Int * 10 + binary_to_integer(C), Frac);

parse_unix_epoch(frac, <<C:8/bitstring, Rest/bitstring>>, Int, Frac) ->
	parse_unix_epoch(frac, Rest, Int, Frac * 10 + binary_to_integer(C)).

to_datetime(T) when is_record(T, timestamp) ->
	{
		{ T#timestamp.year, T#timestamp.month, T#timestamp.day },
		{ T#timestamp.hour, T#timestamp.minute, T#timestamp.second }
	}.

to_gregorian_seconds(R) when is_record(R, timestamp) ->
	calendar:datetime_to_gregorian_seconds( to_datetime(R) ).

to_unix_epoch(R) when is_record(R, timestamp) ->
	%% XXX should we support negative values for unix epoch?
	Seconds = integer_to_list( gregorian_to_unix_seconds( to_gregorian_seconds(R) ) ),
	Fraction = integer_to_list( R#timestamp.fraction ),
	string:join([Seconds, Fraction], ".").

to_utc(R) when is_record(R, timestamp) -> to_tz(R, #timezone{ hour = 0, minute = 0 }).

to_tz(R, Tz) when is_record(R, timestamp), is_record(Tz, timezone) ->
	Tz_offset = ((Tz#timezone.hour * 60) + Tz#timezone.minute) * 60,
	R_offset = (((R#timestamp.timezone)#timezone.hour * 60) + (R#timestamp.timezone)#timezone.minute) * 60,
	R_seconds = calendar:datetime_to_gregorian_seconds(to_datetime(R)),
	New_seconds = R_seconds - R_offset + Tz_offset,
	Datetime = calendar:gregorian_seconds_to_datetime(New_seconds),
	{ok, New_R} = from_datetime(Datetime),
	New_R#timestamp{timezone = Tz, fraction = R#timestamp.fraction}.

cmp(R1, R2) when is_record(R1, timestamp), is_record(R2, timestamp) ->
	R1_utc = to_utc(R1),
	R2_utc = to_utc(R2),
	if
		R1_utc < R2_utc -> -1;
		R1_utc > R2_utc -> 1;
		R1_utc =:= R2_utc -> 0
	end.

format(_Format, _R) -> ok.

to_iso8601(R) when is_record(R, timestamp) -> format("%Y%m%dT%H%M%S.%f%Z", R).

