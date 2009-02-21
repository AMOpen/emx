-module(util_data).

-export([get_handle/3, put_data/2, get_data/2, close_handle/1, foldl/3, get_size/1, delete_data/2, get_type/1, convert/2]).

put_data({ets, TableId}, Value) ->
	ets:insert(TableId, Value);
put_data({dets, TableId}, Value) ->
	dets:insert(TableId, Value);
put_data(remote, Value) ->
	nothing.
	
get_data({ets, TableId}, Key) ->
	ets:lookup(TableId, Key);
get_data({dets, TableId}, Key) ->
	dets:lookup(TableId, Key).

delete_data({ets, TableId}, Key) ->
	ets:delete(TableId, Key);
	
delete_data({dets, TableId}, Key) ->
	dets:delete(TableId, Key).

get_dets_filename(TableName) ->
	{ok, DataDir} = application:get_env(datadir),
	atom_to_list(DataDir) ++ TableName.
	
get_handle(ets, TableName, Options) ->
	{ ets, ets:new(TableName, Options) };
get_handle(dets, TableName, Options) ->
	{ ok, Handle } = dets:open_file(get_dets_filename(TableName), Options), 
	{ dets,  Handle}.
	
close_handle({ets, TableId}) ->
	ets:delete(TableId);
close_handle({dets, TableId}) ->
	dets:close(TableId);
close_handle(remote) ->
	nothing.
	
foldl(Function, Acc0, {ets, TableId}) ->
	ets:foldl(Function, Acc0, TableId);
foldl(Function, Acc0, {dets, TableId}) ->
	dets:foldl(Function, Acc0, TableId);
foldl(Function, Acc0, Token) when is_atom(Token) ->
	Acc0.
	
%% Returns { #of records, #memory used }	
get_size({ets, TableId}) ->
	{ets:info(TableId, size), ets:info(TableId, memory), 0};
	
get_size({dets, TableId}) ->
	{dets:info(TableId, no_keys), 0, dets:info(TableId, memory)};

get_size(_) ->
	{ 0, 0, 0}.

get_type({Type, _}) ->
	Type;
get_type(undefined) ->
	system;
get_type(_) ->
	remote.
	
convert({ets, TableId}, Name) ->
	NewId = util_data:get_handle(dets, Name, [{keypos, 2}]),
	{dets, Id} = NewId,
	dets:from_ets(Id, TableId),
	close_handle({ets, TableId}),
	NewId;
	
convert({dets, TableId}, Name) ->
	NewId = util_data:get_handle(ets, Name, [{keypos, 2}]),
	{ets, Id} = NewId,
	dets:to_ets(TableId, Id),
	close_handle({dets, TableId}),
	NewId.
	