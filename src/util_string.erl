%% @author Alan Moore amkimian@mac.com
%% @copyright 2008 Alan Moore

-module(util_string).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-export([format/2, join/1, join/2]).

format(DisplayString, Args) ->
    lists:flatten(io_lib:format(DisplayString, Args)).

join(List) -> join(List, ",").

join(List, Delim) ->
    lists:foldl(fun (Elem, []) -> [Elem];
		    (Elem, Acc) -> [Elem, Delim | Acc]
		end,
		[], lists:reverse(List)).
