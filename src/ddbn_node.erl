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
-module(ddbn_node).
-behaviour(gen_fsm).
-include("types.hrl").

-export([start_link/1,
	 stop/1, 
	 report/1,
	 set_cp/2,
	 intercom/2
	]).

-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-type fsm_state() :: unobserved | observed | descendant_observed.
-type pointer() :: any().  %% pid or other identifier

-record(loop_data, {
	  name     :: pvar(),
	  cp       :: probability(),
	  parents  :: kvlist(pvar(), {pointer(), probability()}),
	  children :: kvlist(pvar(), {pointer(), probability()})
	 }).

%%%% api

-spec start_link(Name :: any()) -> {ok, pid()}.
start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

stop(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, stop).

report(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, report).

set_cp(Pid, CP) ->
    gen_fsm:send_all_state_event(Pid, {set_cp, CP}).

intercom(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, Message).

%%%% gen_fsm

init([Name]) ->
    {ok, unobserved, #loop_data{name=Name, parents=[], children=[]}}.

state_name(_Event, LoopData) ->
    {next_state, state_name, LoopData}.

state_name(_Event, _From, LoopData) ->
    Reply = ok,
    {reply, Reply, state_name, LoopData}.

handle_event({set_cp, CP}, StateName, 
	     LoopData=#loop_data{name=Name,
				 parents=Parents,
				 children=Children}) ->
    NewState = case is_definite_observation(CP) of
		   true ->
		       publish(descendant_observed, Parents),
		       observed;
		   false ->
		       not_observed(StateName)
	       end,
    publish({update, Name, CP}, Parents),
    publish({update, Name, CP}, Children),
    {next_state, NewState, LoopData#loop_data{cp=CP}};

handle_event(_Event, StateName, LoopData) ->
    {next_state, StateName, LoopData}.

handle_sync_event(stop, _From, _StateName, LoopData) ->
    {stop, normal, stopped, LoopData};

handle_sync_event(report, _From, StateName, LoopData) ->
    Reply = make_report(StateName, LoopData),
    {reply, Reply, StateName, LoopData};

handle_sync_event(_Event, _From, StateName, LoopData) ->
    Reply = ok,
    {reply, Reply, StateName, LoopData}.

handle_info(_Info, StateName, LoopData) ->
    {next_state, StateName, LoopData}.

terminate(_Reason, _StateName, _LoopData) ->
    ok.

code_change(_OldVsn, StateName, LoopData, _Extra) ->
    {ok, StateName, LoopData}.

%%%% private

has_a_one([{_,1.0}|_]) -> true;
has_a_one([])          -> false;
has_a_one([_|KVs])     -> has_a_one(KVs).

-spec is_definite_observation(probability()) -> true | false.
is_definite_observation({_, [{[], KVs}]}) ->
    has_a_one(KVs);
is_definite_observation(_) ->
    false.

-spec make_report(fsm_state(), #loop_data{}) -> kvlist(atom(), term()).
make_report(State, #loop_data{name=Name,
			      cp=CP,
			      parents=Parents,
			      children=Children}) ->
    [{name, Name}, {state, State}, {cp, CP}, 
     {parents, Parents}, {children, Children}].

-spec not_observed(fsm_state()) -> fsm_state().
not_observed(observed) -> unobserved;
not_observed(X)        -> X.

publish(Message, Recipients) ->
    lists:foreach(fun({_,{Id,_}}) -> 
			  ddbn_node:intercom(Id, Message)
		  end,
		  Recipients).

