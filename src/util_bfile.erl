-module(util_bfile).

-include_lib("kernel/include/file.hrl").
-include("emx.hrl").

%% A simple module to persist large content using the filesystem

-export([save_content/2,load_content/1,delete_content/1,get_table_keys/1]).

get_table_keys(TablePrefix) ->
	%% TablePrefix implies a root directory for content, we need to recurse through the directories
	%% collecting keys
	{ok, DataDir } = application:get_env(datadir),
	%%DataDir = "c:\\clouddata\\",
	PrefixAsStringFolder = string:join(string:tokens(atom_to_list(TablePrefix), "_"), "\\"),
	Prefix = string:join(string:tokens(atom_to_list(TablePrefix), "_"), "/"),
	StartDirectory = atom_to_list(DataDir) ++ "/" ++ PrefixAsStringFolder,
	collectIn(StartDirectory, Prefix).

collectIn(Folder, Prefix) ->
	%% Collect all files at this folder, array returned is Folder + FileName
	%% Then go through each sub folder. Append on the list formed from collectIn(Folder + SubFolder, Prefix + SubFolder),
	?LOG(debug, "Folder is ~p", [ Folder ]),
	Res = file:list_dir(list_to_atom(Folder)),
	case Res of
		{ error, _ } -> [];
		{ ok, FilesHere } ->
			{ Files, Folders } = lists:foldl(fun(File, { Fi, Fo}) -> 
						FullFile = Folder ++ "\\" ++ File,
						{ok, FileInfo} = file:read_file_info(list_to_atom(FullFile)),
						case FileInfo#file_info.type of
							regular -> { Fi ++ [ File], Fo };
							directory -> { Fi, Fo ++ [File] };
							_ -> { Fi, Fo}
						end
						end, { [], [] }, FilesHere),
			RootRet = [ util_string:format("~s/~s", [ Prefix, File]) || File <- Files ],
			LowerRet = lists:foldl(fun(Directory, AccIn) -> 
				DownFolder = Folder ++ "\\" ++ Directory,
				DownPrefix = Prefix ++ "/" ++ Directory,
				Results = collectIn(DownFolder, DownPrefix),
				AccIn ++ Results
				end, RootRet, Folders)
	end.
	
	
get_real_file(Key) when is_list(Key) ->
	{ok, DataDir } = application:get_env(datadir),
	atom_to_list(DataDir) ++ "/" ++ Key.

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
