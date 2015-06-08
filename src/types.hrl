%% Copyright (c) 2015 Llaisdy, Cyf.

-type kvlist(K,V) :: list({K,V}).
-type key() :: any().
-type val() :: any().

%% (un)conditional probability
-type probability() :: cond_prob() | uncond_prob().
-type cond_prob() :: {pvar(), kvlist(context(), prob_dist())}.
-type uncond_prob() :: {pvar(), [{[], prob_dist()}]}.
%% An unconditional probability is a conditional probability with a single 'empty' context.

-type context()   :: kvlist(pvar(), pval()).

%% probability distribution
%% for now, only dealing with discrete probability distributions
-type prob_dist() :: disc_prob_dist().

%% discrete probability distribution
-type disc_prob_dist() :: kvlist(pval(), pflt()). %% constraint: sum of pflts == 1.0

%% P(pvar = pval) = pflt
-type pvar() :: any().
-type pval() :: any().
-type pflt() :: float(). %% constraint: 0.0 =< pflt =< 1.0

