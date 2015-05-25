%% Copyright (c) 2015 Llaisdy, Cyf.

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

