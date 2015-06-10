%% Copyright (c) 2015 Llaisdy

-module(ddbn_node).
-behaviour(gen_server).
-include("types.hrl").

-export([start_link/1,
	 stop/1, 
	 report/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  name :: any()
	 }).

%%%% api

-spec start_link(Name :: any()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

-spec stop(Pid :: pid()) -> stopped.
stop(Pid) ->
    gen_server:call(Pid, stop).


-spec report(Pid :: pid()) -> tuple().
report(Pid) ->
    gen_server:call(Pid, report).

%%%% gen_server

init([Name]) ->
    {ok, #state{name = Name}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(report, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% private

