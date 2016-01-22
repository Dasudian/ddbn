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

-module(bn).
-include("types.hrl").
-export([child_given_parents/2, parent_given_child/4]).

-compile([export_all]).

-spec child_given_parents(probability(), [probability()]) ->
				 probability().
child_given_parents({Label, CRows}, Parents) ->
    Probs = get_probs(Parents, CRows),
    {Label, dpds:sum(Probs)}.

-spec parent_given_child(probability(), [probability()], pvar(), pval()) ->
				prob_dist().
parent_given_child({_Cl,CRows}, Parents, Pl,  Cv) -> 
    {Parent, OtherPs} = kvlist:pop_value(Parents, Pl),
    A = sum_over_others(CRows, OtherPs),
    B = sum_same(Cv, A),
    C = times_prob_par(Parent, B),
    {Pl, dpds:normalise(C)}.

%%%% private

-spec get_probs([probability()], kvlist(context(), prob_dist())) -> [prob].
get_probs(Parents, CRows) ->
    lists:map(fun({Context, CPs}) ->
		      X = product(Context, Parents),
		      dpds:scale(CPs, X)
	      end, 
	      CRows).

-spec product(context(), [probability()]) -> float().
product(Context, Parents) ->
    lists:foldl(fun({X,Y}, A) ->
			[{[], Pd}] = kvlist:get_value(Parents, X),
			P  = kvlist:get_value(Pd, Y),
			A*P
		end,
		1, Context).

-spec sum_over_others(kvlist(context(), prob_dist()), [probability()]) -> probability().
sum_over_others(CRows, Ps) ->
    lists:foldl(fun({Lab, P}, Acc) ->
			do_am(Lab, P, Acc)
		end,
		CRows, Ps).

-spec do_am(pvar(), {context(), prob_dist()}, list()) -> list().
do_am(Lab, [{[], P}], Acc) -> % independent
    lists:map(fun({Pars, Probs}) ->
		      {Val, Rest} = kvlist:pop_value(Pars, Lab),
		      VP = kvlist:get_value(P, Val), 
		      New = dpds:scale(Probs, VP),
		      {Rest, New}
	      end, 
	      Acc).

-spec sum_same(pval(), list()) -> list().
sum_same(Cv, A) ->
    lists:foldl(fun({K, Vs}, Acc) -> 
			In = kvlist:get_value(Vs, Cv),
			orddict:update(K, fun(X) -> X+In end, In, Acc)
		end, 
		[], A).

-spec times_prob_par(probability(), list()) -> list().
times_prob_par([{[], Parent}], B) -> % independent
    lists:map(fun({[{_,Pv}],PP}) ->
		      VP = kvlist:get_value(Parent, Pv),
		      {Pv, PP*VP}
	      end,
	      B).
