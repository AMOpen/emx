-module(emx_test).

-export([test/2]).

-include_lib("emx.hrl").

test(Count, Count2) ->
    application:start(emx),
    lists:foreach(fun (Id) ->
			  io:format("Running for ~p~n", [Id]),
			  run_for(Count2, util_string:format("official/system.~p",
						     [Id]))
		  end,
		  lists:seq(0, Count)).

run_for(Count2, Prefix) ->
    lists:foreach(fun (Id) ->
			  emx_admin:put_data(util_string:format("~s/~p",
							       [Prefix, Id]),
					    util_string:format("<data>~p</data>",
							       [Id]))
		  end,
		  lists:seq(0, Count2)).
