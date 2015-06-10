%% Copyright (c) 2015 Llaisdy

-module(ddbn_sup).
-include("types.hrl").

-behaviour(supervisor).

-export([start_link/0,
	 start_node/1
	]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_node(any()) -> {ok, pid()}.
start_node(Name) ->
    supervisor:start_child(?MODULE, [Name]).

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSeconds = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSeconds},

    ChildId = ddbn_node,
    StartFunc = {ddbn_node, start_link, []},
    Restart = transient,
    Shutdown = 1000,
    Type = worker,
    Modules = [ddbn_node],

    ChildSpec = {ChildId, StartFunc, Restart, Shutdown, Type, Modules},

    {ok, {SupFlags, [ChildSpec]}}.
