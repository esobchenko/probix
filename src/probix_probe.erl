-module(probix_probe).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
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
	[id_object, timestamp, value].

record_name() ->
	probe.

record_fields() ->
	record_info(fields, probe).

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

%% primary method to create probes
create(Id_object, List) when is_list(List), is_integer(Id_object) ->
	T = fun() ->
		probix_db:read({object, Id_object}), %% foreign key constraint check

		Bad = lists:filter(
			fun(R) ->
				Id_object =/= R#probe.id_object
			end,
			List
		),
		[] =:= Bad orelse throw(
			probix_error:create(bad_input, "some probes have wrong object_id")
		),

		Probes = lists:map(
			fun(R) ->
				Id = probix_db:new_id(probe),
				Probe = R#probe{ id = Id, id_object = Id_object },
				Probe
			end,
			List
		),

		probix_db:create(Probes)
	end,
	probix_db:transaction(T); %% returns list of newly created probes on success

%% will be rarely used i think
create(Id, R) when is_record(R, probe), is_integer(Id) -> create(Id, [ R ] ).

probes_by_object_id(Id) when is_integer(Id) ->
	Q = qlc:q(
		[ P || P <- mnesia:table(probe), P#probe.id_object =:= Id ]
	),
	probix_db:find(Q).

probes_by_object_id(Id, {to, To}) ->
	Q = qlc:q(
		[ P || P <- mnesia:table(probe),
		P#probe.id_object =:= Id,
		P#probe.timestamp =< To ]
	),
	probix_db:find(Q);

probes_by_object_id(Id, {from, From}) ->
	Q = qlc:q(
		[ P || P <- mnesia:table(probe),
		P#probe.id_object =:= Id,
		P#probe.timestamp >= From ]
	),
	probix_db:find(Q).

probes_by_object_id(Id, From, To) ->
	Q = qlc:q(
		[ P || P <- mnesia:table(probe),
		P#probe.id_object =:= Id,
		P#probe.timestamp >= From,
		P#probe.timestamp =< To ]
	),
	probix_db:find(Q).

read(Id) when is_integer(Id) ->
	try
		probix_db:read({probe, Id})
	catch
		throw:#error{code = not_found} ->
			throw(
				probix_error:create(
					not_found,
					"probe with id " ++ integer_to_list(Id) ++ "doesn't exist"
				)
			)
	end.

delete(Id) when is_integer(Id) ->
	probix_db:delete({probe, Id}),
	Id.

