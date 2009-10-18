-module(probix_util).
-export([random_string/1, very_unique_id/0]).

%% random strings are used for series ids
random_string(Length) ->
	{A1, A2, A3} = now(),
	random:seed( A1, A2, A3 ),
	lists:foldl( fun(_I, Acc) -> [do_rand(0) | Acc] end, [], lists:seq(1, Length) ).

do_rand(R) when R >= $1, R =< $9; R >= $A, R =< $Z; R >= $a, R =< $z ->
	R;

do_rand(_R) ->
	do_rand($0 + random:uniform($z - $0)).

%% truly unique id for series
very_unique_id() -> mochihex:to_hex(crypto:sha(term_to_binary({make_ref(), now()}))).

