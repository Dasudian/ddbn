%% -------------------------------------------------------------------
%% Dasudian Distributed Bayesian Network
%%
%% @author Ivan Uemlianin <ivan@llaisdy.com>, <ivan@dasudian.com>
%% @copyright (c) 2015-2016 Dasudian.com.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
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
