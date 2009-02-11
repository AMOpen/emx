%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(util_randomext).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-export([pickCount/2, randomString/1, rangeInt/2]).

%% Return random int betweem low and high
rangeInt(Low, High) ->
    Low - 1 + random:uniform(1 + High - Low).

%% pick N out of List at random

pickCount(List, 1) ->
    {Val, _NewList} = genEntry(List), Val;
pickCount(List, N) -> pickCountAcc(List, N, []).

pickCountAcc(_List, 0, Acc) -> Acc;
pickCountAcc(List, N, []) ->
    {Val, NewList} = genEntry(List),
    pickCountAcc(NewList, N - 1, [Val]);
pickCountAcc(List, N, Acc) ->
    {Val, NewList} = genEntry(List),
    pickCountAcc(NewList, N - 1, lists:merge(Acc, [Val])).

genEntry(List) ->
    Index = random:uniform(length(List)),
    Val = lists:nth(Index, List),
    NewList = [V1 || V1 <- List, genEntry_1(V1, Val)],
    {Val, NewList}.

genEntry_1(Elem, Val) -> Elem /= Val.

randomString(Length) -> randomStringAcc(Length, "").

randomStringAcc(0, Acc) -> Acc;
randomStringAcc(Remainder, Acc) ->
    randomStringAcc(Remainder - 1,
		    string:chars(64 + random:uniform(26), 1, Acc)).
