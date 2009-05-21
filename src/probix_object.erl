-module(probix_object).
-compile(export_all).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

%% acceptable value type checking functions
acceptable_value(none) ->
	true;
acceptable_value({_K, V}) when is_binary(V); is_integer(V) ->
	true;
acceptable_value(_Pair) ->
	false.

required_fields() ->
	[name].

record_name() ->
	object.

record_fields() ->
	record_info(fields, object).

%% input and output handlers are used by http module
%% to convert between representation formats
output_handler_for(json) ->
	fun(Data) ->
			probix_utils:record_to_json(Data, ?MODULE)
	end.

input_handler_for(json) ->
	fun(Data) ->
			probix_utils:json_to_record(Data, ?MODULE)
	end.

create(R) when is_record(R, object) ->
	Id = probix_db:new_id(object),
	Object = R#object{ id = Id },
	probix_db:create(Object),
	Object.

read_all() ->
	probix_db:read_all(object).

read(Id) when is_integer(Id) ->
	try
		probix_db:read({object, Id})
	catch
		_Error = #error{code = not_found} ->
			throw(
				probix_error:create(
					not_found,
					"object with id " ++ integer_to_list(Id) ++ " doesn't exist"
				)
			)
	end.


update(Id, R) when is_record(R, object), is_integer(Id) ->
	Object = R#object{id = Id},
	try
		probix_db:update(Object)
	catch
		#error{code = not_found} ->
			throw(
				probix_error:create(
					not_found,
					"object with id " ++ integer_to_list(Id) ++ " doesn't exist"
				)
			)
	end,
	Object.

delete(Id) when is_integer(Id) ->
	try
		probix_db:delete({object, Id})
	catch
		#error{code = not_found} ->
			throw(
				probix_error:create(
					not_found,
					"object with id " ++ integer_to_list(Id) ++ " doesn't exist"
				)
			)
	end,
	Id.

