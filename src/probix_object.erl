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

create_from(json, Json) ->
	R = probix_utils:json_to_record(Json, ?MODULE),
	Object = create(R),
	probix_utils:record_to_json(Object, ?MODULE).

read_all_as(json) ->
	probix_utils:record_to_json(read_all(), ?MODULE).

read_as(json, Id) when is_integer(Id) ->
	Object = read(Id),
	probix_utils:record_to_json(Object, ?MODULE).

update_from(json, Id, Json) when is_integer(Id) ->
	Object = probix_utils:json_to_record(Json, ?MODULE),
	Updated = update(Id, Object),
	probix_utils:record_to_json(Updated, ?MODULE).

create(R) when is_record(R, object) ->
	Id = probix_db:new_id(object),
	Object = R#object{ id = Id },
	{atomic, ok} = probix_db:write(Object),
	Object.

read_all() ->
	probix_db:read_all(object).

read(Id) when is_integer(Id) ->
	case probix_db:read({object, Id}) of
		[ Object ] ->
			Object;
		[] ->
			throw({not_found, Id})
	end.

update(Id, R) when is_record(R, object), is_integer(Id) ->
	Object = R#object{id = Id},
	{atomic, ok} = probix_db:write(Object),
	Object.

delete(Id) when is_integer(Id) ->
	{atomic, ok} = probix_db:delete({object, Id}),
	Id.

