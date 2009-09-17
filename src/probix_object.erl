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

oid2table(Id) -> list_to_atom( "probe_" ++ integer_to_list(Id) ).

create(R) when is_record(R, object) ->
	F = fun() ->
		%% create object record
		Id = probix_db:new_id(object),
		Object = R#object{ id = Id },
		Object = probix_db:create(Object),
		%% create table for probes
		Table_name = oid2table(Id),
		Storage_type = case os:getenv("PROBIX_TEST_MODE") of
			false -> disc_copies;
			"0" -> disc_copies;
			"1" -> ram_copies
		end,
		Nodes = mnesia:system_info(running_db_nodes),
		ok = probix_db:create_table(Table_name,
			[
				{Storage_type, Nodes},
				{attributes, record_info(fields, probe)},
				{record_name, probe},
				{type, bag}
			]
		),
		Object
	end,
	probix_db:transaction(F).

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
		F = fun() ->
			probix_db:delete({object, Id}),
			Name = oid2table(Id),
			probix_db:delete_table(Name)
		end,
		probix_db:transaction(F)
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

