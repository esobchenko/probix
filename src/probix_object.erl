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
	probix_db:read({object, Id}).

update(Id, R) when is_record(R, object), is_integer(Id) ->
	Object = R#object{id = Id},
	probix_db:update(Object),
	Object.

delete(Id) when is_integer(Id) ->
	probix_db:delete({object, Id}),
	Id.

%% create_from(json, Json) ->
%%	R = probix_utils:json_to_record(Json, ?MODULE),
%%	Object = create(R),
%%	probix_utils:record_to_json(Object, ?MODULE).

%% read_all_as(json) ->
%%	 probix_utils:record_to_json(read_all(), ?MODULE).

%% read_as(json, Id) when is_integer(Id) ->
%%	 Object = read(Id),
%%	 probix_utils:record_to_json(Object, ?MODULE).

%%update_from(json, Id, Json) when is_integer(Id) ->
%%	Object = probix_utils:json_to_record(Json, ?MODULE),
%%	Updated = update(Id, Object),
%%	probix_utils:record_to_json(Updated, ?MODULE).
