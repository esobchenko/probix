-module(probix_util).
-export([random_string/1, very_unique_id/0]).

%% random strings are used for series ids
random_string(Length) ->
	{A1, A2, A3} = now(),
	random:seed( A1, A2, A3 ),
	lists:foldl( fun(_I, Acc) -> [do_rand(0) | Acc] end, [], lists:seq(1, Length) ).

do_rand(R) when R > 46, R < 58; R > 64, R < 91; R > 96 ->
	R;

do_rand(_R) ->
	do_rand(48 + random:uniform(74)).

very_unique_id() -> mochihex:to_hex(crypto:sha(term_to_binary({make_ref(), now()}))).

