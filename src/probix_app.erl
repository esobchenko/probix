%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the probix application.

-module(probix_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for probix.
start(_Type, _StartArgs) ->
    probix_deps:ensure(),
    probix_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for probix.
stop(_State) ->
    ok.
