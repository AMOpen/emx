-module(util_data).

-export([get_handle/3, put_data/2, get_data/2, close_handle/1]).

put_data({ets, TableId}, Value) ->
	ets:insert(TableId, Value);
put_data({dets, TableId}, Value) ->
	dets:insert(TableId, Value).
	
get_data({ets, TableId}, Key) ->
	ets:lookup(TableId, Key);
get_data({dets, TableId}, Key) ->
	dets:lookup(TableId, Key).
	
get_handle(ets, TableName, Options) ->
	{ ets, ets:new(TableName, Options) };
get_handle(dets, TableName, Options) ->
	{ ok, Handle } = dets:open_file(TableName, Options), 
	{ dets,  Handle}.
	
close_handle({ets, TableId}) ->
	ets:close(TableId);
close_handle({dets, TableId}) ->
	dets:close(TableId).
	

