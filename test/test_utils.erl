%% Copyright (c) 2015 Llaisdy, Cyf.

-module(test_utils).

-export([similar_kvlist/2]).

-define(ofl, orddict:from_list).

similar_kvlist(Xs, Ys) ->
    all_true(lists:zipwith(fun({K1,X}, {K2,Y}) ->
				   (K1 =:= K2) andalso similar_float(X, Y)
			   end,
			   ?ofl(Xs), ?ofl(Ys))).

similar_float(X, Y) when is_float(X), is_float(Y) ->
    abs(X - Y) < 0.000001.

all_true([])       -> true;
all_true([true|T]) -> all_true(T);
all_true([_|_])    -> false.
