-module(probix_format).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

series_to_json_object(Rec) when is_record(Rec, series) ->
    { struct,
      [
        { <<"id">>, Rec#series.id },
        { <<"time_created">>, probix_time:to_iso8601(Rec#series.time_created) },
        { <<"label">>, Rec#series.label }
      ]
    }.

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
	{struct, lists:zip(Keys, [probix_time:to_iso8601(Timestamp), Value])}.

ticks_to_json(Ticks) ->
	List = case Ticks of
		R when is_record(R, tick) ->
			[ tick_to_json_object(R) ];
		L when is_list(L) ->
			[ tick_to_json_object(R) || R <- L ]
	end,
	Encode = mochijson2:encoder([{utf8, true}]),
	Encode(List).

ticks_from_json(<<>>) ->
	{error, empty_json};

ticks_from_json(undefined) ->
	{error, empty_json};

ticks_from_json(Json) ->
	try
		List = case mochijson2:decode(Json) of
			L when is_list(L) ->
				[ json_object_to_tick(S) || S <- L ];
			T when is_tuple(T) ->
				[ json_object_to_tick(T) ]
		end,
		{ok, List}
	catch
		error:_ ->
			{error, bad_json}
	end.

json_object_to_tick({struct, Proplist}) ->
	try
		{ok, Value} = parse_value(proplists:get_value(<<"value">>, Proplist)),
		{ok, Timestamp} = parse_timestamp(proplists:get_value(<<"timestamp">>, Proplist)),
		#tick{
			id = {undefined, Timestamp},
			value = Value
		}
	catch
		error:Error ->
			{error, Error}
	end.

parse_value(Value) when is_binary(Value) ->
	parse_value(binary_to_list(Value));

parse_value(Value) when is_list(Value) ->
	case [string:to_float(Value), string:to_integer(Value)] of
		[{error, _}, {error, _}] ->
			{error, bad_value};
		[{Float, _Rest}, {error, _}] ->
			{ok, Float};
		[{error, _}, {Integer, _Rest}] ->
			{ok, Integer};
        [{Float, _Rest}, {0, _}] ->
            {ok, Float}
	end;

parse_value(Value) when is_number(Value) ->
	{ok, Value};

parse_value(_Value) ->
	{error, bad_value}.

parse_timestamp(Timestamp) when is_list(Timestamp);is_binary(Timestamp) ->
	case [probix_time:from_iso8601(Timestamp), probix_time:from_unix_epoch(Timestamp)] of
		[{error, _}, {error, _}] ->
			{error, bad_timestamp};
		[R, {error, _}] ->
			R;
		[{error,_}, R] ->
			R;
        %% corner case when we can't detect
        %% if only year in ISO passed or
        %% very small unix timestamp.
        %% Treating passed value as ISO year
        [R_iso, _R_unix] ->
            R_iso
	end;

parse_timestamp(Timestamp) when is_number(Timestamp) ->
	probix_time:from_unix_epoch(Timestamp);

parse_timestamp(_Value) ->
	{error, bad_timestamp}.

ticks_to_csv(_Ticks) -> ok.
ticks_from_csv(_Series_id, _Csv) -> ok.

