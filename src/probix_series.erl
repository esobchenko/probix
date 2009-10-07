-module(probix_series).
-behaviour(gen_server).

-compile(export_all).

-export([
	start_link/0, new_series/0, new_series/1,
	all_series/0, delete_series/1, series/1,
	add_probes/1, get_probes/1, get_probes/2,
	delete_probes/1, delete_probes/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("probix.hrl").
-include_lib("eunit/include/eunit.hrl").

new_series() ->
	gen_server:call(?MODULE, {new_series, undefined}).
new_series(Label) ->
	gen_server:call(?MODULE, {new_series, Label}).

all_series() ->
	gen_server:call(?MODULE, all_series).
delete_series(Id) ->
	gen_server:call(?MODULE, {delete_series, Id}).
series(Id) ->
	gen_server:call(?MODULE, {series, Id}).

add_probes(Probes) ->
	gen_server:call(?MODULE, {add_probes, Probes}).

get_probes(Series_id) ->
	gen_server:call(?MODULE, {get_probes, Series_id}).
get_probes(Series_id, Range) ->
	gen_server:call(?MODULE, {get_probes, Series_id, Range}).

delete_probes(Series_id) ->
	gen_server:call(?MODULE, {delete_probes, Series_id}).
delete_probes(Series_id, Range) ->
	gen_server:call(?MODULE, {delete_probes, Series_id, Range}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, null}.

handle_call({new_series, Label}, _From, State) ->
	Reply = probix_db:new_series(Label),
	{reply, Reply, State};

handle_call(all_series, _From, State) ->
	Reply = probix_db:all_series(),
	{reply, Reply, State};

handle_call({delete_series, Id}, _From, State) ->
	Reply = probix_db:delete_series(Id),
	{reply, Reply, State};

handle_call({series, Id}, _From, State) ->
	Reply = probix_db:series(Id),
	{reply, Reply, State};


handle_call({add_probes, Probes}, _From, State) ->
	Reply = probix_db:add_probes(Probes),
	{reply, Reply, State};


handle_call({get_probes, Series_id}, _From, State) ->
	Reply = probix_db:get_probes(Series_id),
	{reply, Reply, State};

handle_call({get_probes, Series_id, Range}, _From, State) ->
	Reply = probix_db:get_probes(Series_id, Range),
	{reply, Reply, State};


handle_call({delete_probes, Series_id}, _From, State) ->
	Reply = probix_db:delete_probes(Series_id),
	{reply, Reply, State};

handle_call({delete_probes, Series_id, Range}, _From, State) ->
	Reply = probix_db:delete_probes(Series_id, Range),
	{reply, Reply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old_version, State, _Extra) ->
	{ok, State}.

