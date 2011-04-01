-module(probix_web).

-export([start/1, stop/0, dispatch_request/1, handle/4]).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

start(Options) ->
    log4erl:info(http_logger, "~p", [Options]),
	mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, dispatch_request}} | Options]).

stop() ->
	mochiweb_http:stop(?MODULE).

dispatch_request(Req) ->
    probix_http:dispatch_request(Req, ?MODULE).

handle('GET', [], _, _) ->
    erlydtl:compile("src/templates/index.html", index_template),
    index_template:render([name, "foobar"]);

handle('POST', ["signup"], _, _) ->
    ok.
