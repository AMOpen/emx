-module(util_bfile).

%% A simple module to persist large content using the filesystem

-export([save_content/2,load_content/1]).

save_content(Key, Content) ->
	%% Assume Key can be converted to a filename
	RealFile = "C:\\clouddata\\" ++ Key,
	filelib:ensure_dir(RealFile),
	file:write_file(RealFile, Content).
	
load_content(Key) ->
	RealFile = "C:\\clouddata\\" ++ Key,
	{ok, Data } = file:read_file(RealFile),
	Data.
