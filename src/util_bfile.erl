-module(util_bfile).

%% A simple module to persist large content using the filesystem

-export([save_content/2,load_content/1,delete_content/1]).

get_real_file(Key) when is_list(Key) ->
	{ok, DataDir } = application:get_env(datadir),
	DataDir ++ Key.

save_content(Key, Content) when is_list(Key) ->
	%% Assume Key can be converted to a filename
	RealFile = get_real_file(Key),
	Res1 = filelib:ensure_dir(RealFile),
	Res = file:write_file(RealFile, Content),
	RealFile.
	
load_content(Key) when is_list(Key) ->
	RealFile = get_real_file(Key),
	case file:read_file(RealFile) of	
		{ok, Data } -> Data;
		{error, _ } -> baddata
	end.
	
delete_content(Key) when is_list(Key) ->
	RealFile = get_real_file(Key),
	file:delete(RealFile).
