-module(probix_http).
-author('Eugen Sobchenko <eugen@sobchenko.com>').
-export([start/1,stop/0,dispatch_requests/1]).


%% start() ->
%%	mochiweb_http:start([{port, 8888},{loop, fun dispatch_requests/1}]).

start(Options) ->
	mochiweb_http:start([{name, ?MODULE}, {loop, fun dispatch_requests/1} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_requests(Req) ->
	Path = Req:get(path),
	Method = Req:get(method),
	Post = Req:parse_post(),
	io:format("~p request for ~p with post: ~p~n", [Method, Path, Post]),
	Response = handle(Method, Path, Post),
	Req:respond(Response).

handle(_, _, _) ->
	{404, [{"Content-Type", "text/plain"}], <<"Unknown Request">>}.

