-module(emx_test).

-export([test/0]).

-include_lib("records.hrl").

test() ->
	application:start(emx).
	
	
	