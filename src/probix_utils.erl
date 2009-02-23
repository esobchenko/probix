-module(probix_utils).
-compile(export_all).

atom_to_binary(A) when is_atom(A) ->
	list_to_binary(atom_to_list(A)).


