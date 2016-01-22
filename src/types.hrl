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

