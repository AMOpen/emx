-module(emx_test).

-export([test/0]).

-include_lib("records.hrl").

test() ->
	application:start(emx),
	lists:foreach(fun(Id) ->
		run_for(util_string:format("official/system.~p", [Id]))
		end, lists:seq(0, 20)).
	
run_for(Prefix) ->
	lists:foreach(fun(Id) ->
		emx_admin:put_data(util_string:format("~s/~p", [ Prefix, Id]), util_string:format("<data>~p</data>", [Id]))
		end, lists:seq(0, 2000)).
		
	
	
	