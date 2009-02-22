-module(emx_test).

-export([test/0]).

-include_lib("emx.hrl").

test() ->
	application:start(emx),
	lists:foreach(fun(Id) ->
		io:format("Running for ~p~n", [ Id ]),
		run_for(util_string:format("official/system.~p", [Id]))
		end, lists:seq(0, 100)).
	
run_for(Prefix) ->
	lists:foreach(fun(Id) ->
		emx_http:put_data(util_string:format("~s/~p", [ Prefix, Id]), util_string:format("<data>~p</data>", [Id]))
		end, lists:seq(0, 100)).
		
	
	
	