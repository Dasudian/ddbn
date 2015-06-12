%% Copyright (c) 2015 Llaisdy

-module(ddbn_car_start_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2
	 ]).

-export([single_node/1]).

-define(ofl, orddict:from_list).

all() ->
    [single_node
    ].

%%%% set up

init_per_suite(Config) ->
    {ok, _Apps} = application:ensure_all_started(ddbn),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(ddbn).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%% tests

single_node(_Config) ->
    {ok, Pid} = ddbn_sup:start_node(st),
    ddbn_node:set_cp(Pid, cp(st)),
    {st, Exp} = p(st, orphan),
    ct:pal("Node report:~n~n~p~n~n", [ddbn_node:report(Pid)]),
    {st, Act} = ddbn_node:get_p(Pid),
    test_utils:similar_kvlist(Exp, Act).

%%%% helpers

cp(st) ->
    {st, [{?ofl([{fu,y},{sp,y}]), ?ofl([{y,0.99},{n,0.01}])},
	  {?ofl([{fu,y},{sp,n}]), ?ofl([{y,0.01},{n,0.99}])},
	  {?ofl([{fu,n},{sp,y}]), ?ofl([{y,0.0},{n,1.0}])},
	  {?ofl([{fu,n},{sp,n}]), ?ofl([{y,0.0},{n,1.0}])}
	 ]}.

p(st, orphan) ->
    {st, [{[], ?ofl([{y,0.5},{n,0.5}])}]}.
