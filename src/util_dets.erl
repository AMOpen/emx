-module(util_dets).

-export([getByKey/2, getAllByKey/2, saveByKey/2]).

%% An equivalent to util_mnesia but using dets directly (for performance I hope)

getByKey(TableName, Key) ->
	open_table(TableName),
	Res = dets:lookup(TableName, Key),
	case Res of
		{ error, Reason} -> nodata;
		[] -> nodata;
		[ Ret | _] -> Ret
	end.
	
getAllByKey(TableName, Key) ->
	open_table(TableName),
	dets:lookup(TableName, Key).

saveByKey(TableName, Content) ->
	open_table(TableName),
	dets:insert(TableName, Content).
	
open_table(TableName) ->
	dets:open_file(TableName, [{keypos, 2},{file, "c:\\clouddata\\" ++ TableName}]).
	
	