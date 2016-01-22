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
-module(kvlist).
-include("types.hrl").

-export([get_value/2, get_values/2,
	 pop_value/2]).

-spec get_value(kvlist(key(), val()), key()) -> val().
get_value(KVs, K) ->
    case lists:keyfind(K, 1, KVs) of
        {K,V} -> V;
	false -> undefined
    end.

-spec get_values(kvlist(key(), val()), list(key())) -> list(val()).
get_values(KVs, Ks) ->
    lists:map(fun(K) -> get_value(KVs, K) end, Ks).

-spec pop_value(kvlist(key(), val()), key()) -> {val(), kvlist(key(), val())}. 
pop_value(KVs, K) ->
    V = kvlist:get_value(KVs, K),
    {V, lists:keydelete(K, 1, KVs)}.

