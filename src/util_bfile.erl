-module(util_bfile).

%% A simple module to persist large content using the filesystem

-export([save_content/2,load_content/1,delete_content/1]).

%% TODO hardcoded paths here

get_real_file(Key) ->
	{ok, DataDir } = application:get_env(datadir),
	DataDir ++ Key.

save_content(Key, Content) ->
	%% Assume Key can be converted to a filename
	RealFile = get_real_file(Key),
	util_flogger:logMsg(self(), ?MODULE, debug, "File name to write out is ~p", [ RealFile]),
	Res1 = filelib:ensure_dir(RealFile),
	util_flogger:logMsg(self(), ?MODULE, debug, "File dir create response is ~p", [ Res1 ]),
	Res = file:write_file(RealFile, Content),
	util_flogger:logMsg(self(), ?MODULE, debug, "File write response is ~p", [ Res ]),
	RealFile.
	
load_content(Key) ->
	RealFile = get_real_file(Key),
	{ok, Data } = file:read_file(RealFile),
	Data.
	
delete_content(Key) ->
	RealFile = get_real_file(Key),
	file:delete(RealFile).
