-module(util_bfile).

%% A simple module to persist large content using the filesystem

-export([save_content/2,load_content/1]).

%% TODO hardcoded paths here

get_real_file(Key) ->
	{ok, DataDir } = application:get_env(datadir),
	DataDir ++ Key.

save_content(Key, Content) ->
	%% Assume Key can be converted to a filename
	RealFile = get_real_file(Key),
	filelib:ensure_dir(RealFile),
	file:write_file(RealFile, Content).
	
load_content(Key) ->
	RealFile = get_real_file(Key),
	{ok, Data } = file:read_file(RealFile),
	Data.
