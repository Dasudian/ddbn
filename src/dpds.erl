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
-module(dpds).
-include("types.hrl").
-export([normalise/1, scale/2, sum/1]).

-spec normalise([disc_prob_dist()]) -> [disc_prob_dist()].
normalise(Ds) ->
    S = lists:sum([N || {_,N} <- Ds]),
    scale(Ds, 1/S).

-spec scale([disc_prob_dist()], float()) -> [disc_prob_dist()].
scale(Ds, S) ->
    lists:map(fun({K,V}) -> {K, V*S} end, Ds).

-spec sum([disc_prob_dist()]) -> disc_prob_dist().
sum(Ds) ->
    lists:foldl(fun(D, Acc) ->
			orddict:merge(fun(_, I, J) -> I+J end, D, Acc)
		end, 
		[], Ds).

