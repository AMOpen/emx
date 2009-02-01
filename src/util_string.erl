%% @author Alan Moore amkimian@mac.com
%% @copyright 2008 Alan Moore

-module(util_string).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-export([format/2]).

format(DisplayString, Args) ->
	lists:flatten(io_lib:format(DisplayString, Args)).
	
	
