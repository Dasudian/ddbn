%% Copyright (c) 2015 Llaisdy, Cyf.

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

