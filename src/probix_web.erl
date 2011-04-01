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

handle('GET', [], Query, _) ->
    {ok, Content} = index_tpl:render(Query),
    probix_http:ok_html(Content);

handle('GET', ["signup"], _, _) ->
    {ok, Content} = signup_tpl:render([]),
    probix_http:ok_html(Content);

handle('POST', ["signup"], _, Post) ->
    Params = mochiweb_util:parse_qs(Post),
    {ok, Content} = try 
                        log4erl:info("Foo"),
        probix_user:create(Params),
                        log4erl:info("Bar"),
        signup_success_tpl:render([])
    catch
        error:{badmatch, Error} -> 
            log4erl:info("Bad ~p", [Error]),
            signup_tpl:render([{error, Error}])
    end,
    probix_http:ok_html(Content);

handle(_, _, _, _) ->
    probix_http:error(not_found).
