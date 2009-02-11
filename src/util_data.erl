-module(util_data).

-export([get_handle/3, put_data/2, get_data/2, close_handle/1, foldl/3, get_size/1, delete_data/2]).

put_data({ets, TableId}, Value) ->
	ets:insert(TableId, Value);
put_data({dets, TableId}, Value) ->
	dets:insert(TableId, Value).
	
get_data({ets, TableId}, Key) ->
	ets:lookup(TableId, Key);
get_data({dets, TableId}, Key) ->
	dets:lookup(TableId, Key).

delete_data({ets, TableId}, Key) ->
	ets:delete(TableId, Key);
	
delete_data({dets, TableId}, Key) ->
	dets:delete(TableId, Key).
	
get_handle(ets, TableName, Options) ->
	{ ets, ets:new(TableName, Options) };
get_handle(dets, TableName, Options) ->
	{ ok, Handle } = dets:open_file(TableName, Options), 
	{ dets,  Handle}.
	
close_handle({ets, TableId}) ->
	ets:delete(TableId);
close_handle({dets, TableId}) ->
	dets:close(TableId).
	
foldl(Function, Acc0, {ets, TableId}) ->
	ets:foldl(Function, Acc0, TableId);
foldl(Function, Acc0, {dets, TableId}) ->
	dets:foldl(Function, Acc0, TableId).
	
%% Returns { #of records, #memory used }	
get_size({ets, TableId}) ->
	{ets:info(TableId, size), ets:info(TableId, memory)};
	
get_size({dets, TableId}) ->
	{dets:info(TableId, no_keys), dets:info(TableId, memory)};

get_size(_) ->
	{ 0, 0}.