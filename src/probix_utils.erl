-module(probix_utils).
-compile(export_all).

atom_to_binary(A) when is_atom(A) -> list_to_binary( atom_to_list(A) ).

missing_params([]) -> true;
missing_params(Missing) -> throw( {missing_params, Missing} ).

bad_values([]) -> true;
bad_values(Bad) -> throw( {bad_values, Bad} ).

json_to_record(Json, Module) when is_list(Json), is_atom(Module) ->
	Required = Module:required_fields(), %% list of required fields 
	Fields = Module:record_fields(),
	{struct, Proplist} = mochijson2:decode( Json ),
	%% get list of fields that missing but required
	Missing = lists:filter(
		fun(Key) ->
			not proplists:is_defined( atom_to_binary(Key), Proplist )
		end,
		Required
	),
	missing_params(Missing),
	%% get list of keys that have unacceptable values
	Bad = lists:filter(
		fun(Key) ->
			not Module:acceptable_value( proplists:lookup( atom_to_binary(Key), Proplist ) )
		end,
		Fields
	),
	bad_values(Bad),
	Values = [ proplists:get_value( atom_to_binary(Key), Proplist ) || Key <- Fields ],
	list_to_tuple( [Module:record_name()|Values] ).

correct_record(R, Module) when is_tuple(R), is_atom(Module) ->
	Tag = Module:record_name(),
	case element(1, R) =:= Tag of
		true -> true;
		false -> throw({incorrect_record, R}) %% it's probably should be error, not throw?
	end.

record_to_json(R, Module) when is_tuple(R), is_atom(Module) ->
	correct_record(R, Module),
	Keys = [ atom_to_binary(X) || X <- Module:record_fields() ],
	L = tuple_to_list(R),
	[_Name | Values] = L,
	Pairs = [ { lists:nth(N, Keys), lists:nth(N, Values) } || N <- lists:seq(1, length(Keys)) ],
	mochijson2:encode( {struct, Pairs} ).

