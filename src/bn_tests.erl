%% Copyright (c) 2015 Llaisdy, Cyf.

-module(bn_tests).
-include("types.hrl").
-export([all/0, cgp/0, pgc/3, p/1]).

-compile([export_all]).

all() ->
    cgp(),
    pgc(sp, st, n),
    pgc(sp, st, y),
    pgc(fu, st, n),
    pgc(fu, st, y),
    ok.    

cgp() ->
    Parents = [p(fu), p(sp)],
    Child = p(st),
    {st,Exp} = {st,[{n,0.06821599999999998},{y,0.931784}]},
    {st,Act} = bn:child_given_parents(Child, Parents),
    similar_kvlist(Exp, Act).

pgc(Plab, Clab, Cval) ->
    Parents = [p(fu), p(sp)],
    CP = p(Clab),
    {Plab,Exp} = pgc_check(Plab, Clab, Cval),
    {Plab,Act} = bn:parent_given_child(CP, Parents, Plab, Cval),
    similar_kvlist(Exp, Act).

%%%% private

ofl(L) ->
     orddict:from_list(L).

-spec p(atom()) -> probability().
p(fu) ->
    {fu, [{[], ofl([{y,0.98},{n,0.02}])}]};

p(sp) ->
    {sp, [{[], ofl([{y,0.96},{n,0.04}])}]};

p(st) ->
    {st, [{ofl([{fu,y},{sp,y}]), ofl([{y,0.99},{n,0.01}])},
	  {ofl([{fu,y},{sp,n}]), ofl([{y,0.01},{n,0.99}])},
	  {ofl([{fu,n},{sp,y}]), ofl([{y,0.0},{n,1.0}])},
	  {ofl([{fu,n},{sp,n}]), ofl([{y,0.0},{n,1.0}])}
	 ]}.

pgc_check(sp, st, n) ->
    {sp,[{n,0.5806262460419842},{y,0.4193737539580157}]};
pgc_check(fu, st, n) ->
    {fu,[{n,0.2931863492435792},{y,0.7068136507564208}]};
pgc_check(fu, st, y) ->
    {fu,[{n,0.0},{y,1.0}]};
pgc_check(sp, st, y) ->
    {sp,[{n,4.206983592763989e-4},{y,0.9995793016407236}]}.

similar_kvlist(Xs, Ys) ->
    all_true(lists:zipwith(fun({K1,X}, {K2,Y}) ->
				   (K1 =:= K2) andalso similar_float(X, Y)
			   end,
			   ofl(Xs), ofl(Ys))).

similar_float(X, Y) when is_float(X), is_float(Y) ->
    abs(X - Y) < 0.000001.

all_true([])       -> true;
all_true([true|T]) -> all_true(T);
all_true([_|_])    -> false.
