-module(probix_utils).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% probix_utils:atom_to_binary/1 is for old erlang versions.
%% erlang:atom_to_binary/2 is available starting from R13A.
atom_to_binary(A) when is_atom(A) -> list_to_binary( atom_to_list(A) ).

json_term_to_record({struct, Proplist}, Module) when is_list(Proplist) ->
	Required = Module:required_fields(), %% list of required fields
	Fields = Module:record_fields(),
	Missing = lists:filter(
		fun(Key) ->
			not proplists:is_defined( atom_to_binary(Key), Proplist )
		end,
		Required
	),
	[] =:= Missing orelse erlang:throw( {bad_input, "missing required parameters"} ),
	%% get list of keys that have unacceptable values
	Bad = lists:filter(
		fun(Key) ->
			not Module:acceptable_value( proplists:lookup( atom_to_binary(Key), Proplist ) )
		end,
		Fields
	),
	[] =:= Bad orelse throw( {bad_input, "unacceptable values detected"} ),
	Values = [ proplists:get_value( atom_to_binary(Key), Proplist ) || Key <- Fields ],
	list_to_tuple( [Module:record_name()|Values] );

json_term_to_record(List, Module) when is_list(List) -> lists:map(
		fun
			({struct, X}) ->
				json_term_to_record({struct, X}, Module);
			(_Term) ->
				erlang:throw({bad_input, "improper json term in list"})
		end,
		List
	);

json_term_to_record(_Term, _Module) -> erlang:throw({bad_input, "bad input"}).

is_valid_record(R, Module) when is_tuple(R), is_atom(Module) ->
	Tag = Module:record_name(),
	Fields = Module:record_fields(),
	element(1, R) =:= Tag andalso length(Fields) =:= (tuple_size(R) - 1).

record_to_json_term(R, Module) when is_tuple(R), is_atom(Module) ->
	is_valid_record(R, Module) orelse erlang:error({invalid_record, R}),
	Keys = [ atom_to_binary(X) || X <- Module:record_fields() ],
	L = tuple_to_list(R),
	[_Name | Values] = L,
	Pairs = [ { lists:nth(N, Keys), lists:nth(N, Values) } || N <- lists:seq(1, length(Keys)) ],
	{struct, Pairs}.

json_to_record(Json, Module) when is_atom(Module) ->
	try mochijson2:decode(Json) of
		Term -> json_term_to_record(Term, Module)
	catch
		error:_Any -> erlang:throw({bad_input, "bad json argument"})
	end.

record_to_json(R, Module) when is_tuple(R), is_atom(Module) ->
	Encode = mochijson2:encoder([{utf8, true}]),
	list_to_binary(Encode(record_to_json_term(R, Module)));

record_to_json(L, Module) when is_list(L), is_atom(Module) ->
	Encode = mochijson2:encoder([{utf8, true}]),
	Json = lists:map( fun(X)-> record_to_json_term(X, Module) end, L ),
	case Encode(Json) of
		List when is_list(List) ->
			list_to_binary(List);
		Any ->
			Any
	end.


