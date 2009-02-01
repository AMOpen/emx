-module(run_test).

-export([runTest/0]).

%% Run the tests for all of the modules that are compiled
runTest() ->
    Modules = [getModuleName(FileName) || FileName <- filelib:wildcard("*.beam")],
    eunit:test(Modules).

getModuleName(FileName) ->
    list_to_atom(hd(string:tokens(FileName, "."))).
