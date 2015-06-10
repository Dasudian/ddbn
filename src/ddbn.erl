%% Copyright (c) 2015 Llaisdy

-module(ddbn).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ddbn_sup:start_link().

stop(_State) ->
	ok.
