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

-record(timestamp, {year, month, day, hour, minute, second, fraction, timezone}).
-record(timezone, {hour, minute}).

binary_to_integer(Binary) when is_binary(Binary) ->
    list_to_integer(binary_to_list(Binary)).

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
                second = 0,
                fraction = 0,
                timezone = #timezone{ hour = 0, minute = 0}
            }
        )
        }
    catch
        error:Err -> {error, not_iso8601, Err}
    end.

iso_to_rec( _State, <<>>, R ) -> R;

%% these are used to come from state of parsing time seconds or
%% fraction seconds to parsing timezone
iso_to_rec(second, <<"-", Rest/bitstring>>, R) ->
    iso_to_rec(tz_hour_minus, Rest, R );
iso_to_rec(fraction, <<"-", Rest/bitstring>>, R) ->
    iso_to_rec(tz_hour_minus, Rest, R );
iso_to_rec(_State, <<"+", Rest/bitstring>>, R) ->
    iso_to_rec(tz_hour_plus, Rest, R );

%% separator between date
iso_to_rec( State, <<"-", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );

%% separators between date and time
iso_to_rec( State, <<" ", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );
iso_to_rec( State, <<"T", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );

%% separator between time parts
iso_to_rec( State, <<":", Rest/bitstring>>, R ) ->
	iso_to_rec( State, Rest, R );

%% separators of fractional part
iso_to_rec( fraction, <<".", Rest/bitstring>>, R ) ->
    iso_to_rec( fraction, Rest, R );
iso_to_rec( fraction, <<",", Rest/bitstring>>, R ) ->
    iso_to_rec( fraction, Rest, R );

%% parsing date
iso_to_rec( year, <<Year:32/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( month, Rest, R#timestamp{ year = binary_to_integer(Year) } );
iso_to_rec( month, <<Month:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( day, Rest, R#timestamp{ month = binary_to_integer(Month) } );
iso_to_rec( day, <<Day:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( hour, Rest, R#timestamp{ day = binary_to_integer(Day) } );

%% parsing time
iso_to_rec( hour, <<Hour:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( minute, Rest, R#timestamp{ hour = binary_to_integer(Hour) } );
iso_to_rec( minute, <<Minute:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( second, Rest, R#timestamp{ minute = binary_to_integer(Minute) } );
iso_to_rec( second, <<Second:16/bitstring, Rest/bitstring>>, R ) ->
	iso_to_rec( fraction, Rest, R#timestamp{ second = binary_to_integer(Second) } );

%% parsing fraction by character, cause we don't know its length
iso_to_rec( fraction, <<C:8/bitstring, Rest/bitstring>>, R) ->
    iso_to_rec( fraction, Rest, R#timestamp{ fraction = R#timestamp.fraction * 10 + binary_to_integer(C)});

%% parsing timezone info
iso_to_rec( tz_hour_plus, <<Tz_hour:16/bitstring, Rest/bitstring>>, R) ->
    iso_to_rec(tz_minute, Rest, R#timestamp{ timezone = (R#timestamp.timezone)#timezone{ hour = binary_to_integer(Tz_hour) }} );
iso_to_rec( tz_hour_minus, <<Tz_hour:16/bitstring, Rest/bitstring>>, R) ->
    iso_to_rec(tz_minute, Rest, R#timestamp{ timezone = (R#timestamp.timezone)#timezone{ hour = - binary_to_integer(Tz_hour) }} );
iso_to_rec( tz_minute, <<Tz_min:16/bitstring, Rest/bitstring>>, R) ->
    iso_to_rec(minute, Rest, R#timestamp{ timezone = (R#timestamp.timezone)#timezone{ minute = binary_to_integer(Tz_min) }} ).



parse_unix_epoch(_Time) -> ok.

to_datetime(_Time) -> ok.

to_gregorian_seconds(_Time) -> ok.

to_unix_seconds(_Time) -> ok.

tz_convert(_R, _Tz) -> ok.

cmp(_R1, _R2) -> ok.

format(_Format, _R) -> ok.

