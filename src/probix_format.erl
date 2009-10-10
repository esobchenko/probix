-module(probix_format).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% there are no atom_to_binary function in old versions of Erlang;
%% erlang:atom_to_binary/2 is available in Erlang R13A and newer.
atom_to_binary(A) when is_atom(A) -> list_to_binary( atom_to_list(A) ).

series_record_to_json(Rec) ->
	Keys = [ atom_to_binary(K) || K <- record_info(fields, series) ],
	[_Name | Values] = tuple_to_list(Rec),
	Encode = mochijson2:encoder([{utf8, true}]),
	list_to_binary( Encode({struct, lists:zip(Keys, Values)}) ).

series_record_csv(_Rec) -> ok.

tick_record_to_json(Rec) ->
	Keys = [ <<"timestamp">>, <<"value">> ],
	{_Series_id, Timestamp} = Rec#tick.id,
	Value = Rec#tick.value,
	Encode = mochijson2:encoder([{utf8, true}]),
	list_to_binary( Encode({struct, lists:zip(Keys, [Timestamp, Value])}) ).

tick_record_to_csv(_Rec) -> ok.

tick_record_from_json(Series_id, Json) ->
	case probix_series:series(Series_id) of
		not_found ->
			{error, not_found};
		_Series ->
			case json_to_json_term(Json) of
				bad_json -> {error, bad_json};
				Term -> tick_record_from_json_term(Series_id, Term)
			end
	end.

tick_record_from_json_term(Series_id, {struct, Proplist}) ->
	#tick{
		id = {Series_id, proplists:get_value(<<"timestamp">>, Proplist)},
		value = proplists:get_value(<<"value">>)
	};

tick_record_from_json_term(Series_id, {array, List}) ->
	[tick_record_from_json_term(Series_id, P) || P <- List].

json_to_json_term(Json) ->
	try mochijson2:decode(Json) of %% because mochijson2:decode/1 doesn't handle bad json input
		Json_term -> Json_term
	catch
		error:_Any ->
			bad_json
	end.

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
	io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		[Year, Month, Day, Hour, Min, Sec]).

%% Fuck.
iso_8601_to_gregorian_epoch(_Iso_8601) -> ok.
