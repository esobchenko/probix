-module(probix_utils).
-compile(export_all).

atom_to_binary(A) when is_atom(A) -> list_to_binary( atom_to_list(A) ).

missing_params([]) -> true;
missing_params(Missing) -> throw( {missing_params, Missing} ).

bad_values([]) -> true;
bad_values(Bad) -> throw( {bad_values, Bad} ).

json_object_to_record({struct, Proplist}, Module) when is_list(Proplist) ->
	Required = Module:required_fields(), %% list of required fields 
	Fields = Module:record_fields(),
	
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

json_to_record(Json, Module) when is_list(Json), is_atom(Module) ->
	case mochijson2:decode( Json ) of
		{struct, Proplist} ->
			json_object_to_record({struct, Proplist}, Module);
		List when is_list(List) ->
			lists:map(fun(X) ->
							  json_object_to_record(X, Module)
					  end,
					  List);
		Object -> erlang:throw({unknown_json_type, Object})
	end.

correct_record(R, Module) when is_tuple(R), is_atom(Module) ->
	Tag = Module:record_name(),
	case element(1, R) =:= Tag of
		true -> true;
		false -> erlang:error({incorrect_record, R})
	end.

record_to_json_object(R, Module) when is_tuple(R), is_atom(Module) ->
	correct_record(R, Module),
	Keys = [ atom_to_binary(X) || X <- Module:record_fields() ],
	L = tuple_to_list(R),
	[_Name | Values] = L,
	Pairs = [ { lists:nth(N, Keys), lists:nth(N, Values) } || N <- lists:seq(1, length(Keys)) ],
	{struct, Pairs}.

record_to_json(R, Module) when is_tuple(R), is_atom(Module) ->
	mochijson2:encode( record_to_json_object(R, Module) ).

list_to_json(L, Module) when is_list(L), is_atom(Module) ->
	List = lists:map(fun(X)->
							 record_to_json_object(X,Module) end,
					 L
					),
	mochijson2:encode(List).
				
