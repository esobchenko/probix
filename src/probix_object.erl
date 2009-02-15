-module(probix_object).
-author('Eugen Sobchenko <eugen@sobchenko.com>').
-compile(export_all).

-include("probix.hrl").

read_all() ->
	probix_db:read_all(object).

read_all_json() ->
	Objects = read_all(),
	Json_objects = lists:map(
		fun (Object) ->
			utils:record_to_json_object(
				record_info(fields, object),
				Object
			)
		end,
		Objects
	),
	mochijson2:encode(Json_objects).

create(R) when is_record(R, object) ->
	Id = probix_db:new_id(object),
	Object = R#object{ id = Id },
	{atomic, ok} = probix_db:write(Object),
	Object.

create_json(Json) ->
	In = mochijson2:decode(Json),
	Fields = record_info(fields, object),
	R = utils:json_object_to_record(object, Fields, In),
	Object = create(R),
	Out = utils:record_to_json_object(Fields, Object),
	mochijson2:encode(Out).

read(Id) when is_integer(Id) ->
	case probix_db:read({object, Id}) of
		[ Object ] ->
			Object;
		[] ->
			throw("object not found: " ++ integer_to_list(Id))
	end.

read_json(I) when is_list(I) ->
	{Id, _} = string:to_integer(I),
	Object = read(Id),
	Out = utils:record_to_json_object(
		record_info(fields, object),
		Object
	),
	mochijson2:encode(Out).

update(R) when is_record(R, object) ->
	{atomic, ok} = probix_db:write(R),
	R.

update_json(Json) ->
	In = mochijson2:decode(Json),
	Fields = record_info(fields, object),
	R = utils:json_object_to_record(object, Fields, In),
	Object = update(R),
	Out = utils:record_to_json_object(Fields, Object),
	mochijson2:encode(Out).

delete(Id) when is_integer(Id) ->
	{atomic, ok} = probix_db:delete({object, Id}),
	Id.

delete_json(I) when is_list(I) ->
	{I_int, _} = string:to_integer(I),
	Id = delete(I_int),
	mochijson2:encode({struct, [{id, Id}]}).

