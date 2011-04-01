-module(probix_user).

-export([create/1]).

create(Params) ->
    log4erl:info("~p", [Params]),
    {ok, Backend} = application:get_env(probix, db_backend),   
    %% validating params
    {ok, Login} = probix_util:not_empty(proplists:get_value("login", Params)),
    {ok, Password} = probix_util:not_empty(proplists:get_value("password", Params)),
    log4erl:info("Creating user"),
    Backend:create_user([{login, Login},
                          {password, Password}]).
