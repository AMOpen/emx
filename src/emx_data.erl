%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([put_data/2, get_data/2, get_datakeys/1, run_capacity/1, get_tables/0]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%-define(GD1,{global, ?MODULE}).
%%-define(GD2,{global, ?MODULE}).

-define(GD1, {local, ?MODULE}).

-define(GD2, ?MODULE).

%% Hold a config table id for passing around. The storage options for the 
%% config table are passed as startup parameters

start_link(_Arg) ->
    gen_server:start_link(?GD1, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    io:format("Starting emx_data~n"),
    {ok, {StorageType, Config}} = application:get_env(emxconfig),
    io:format("Storage type ~p, Config ~p~n", [ StorageType, Config]),
    {ok, Setup} = application:get_env(setupstore),
    {ok, DefaultStore} = application:get_env(defaultstore),

    io:format("Calling get_handle ~p emxconfig, ~p", [ StorageType, Config]),
    ConfigHandle = util_data:get_handle(StorageType, emxconfig, Config),
    io:format("Done~n"),
    case Setup of 
    	true ->
		util_data:put_data(ConfigHandle, DefaultStore);
	_ ->
		ok
    end,
    {ok, ConfigHandle}.

closer(Record, AccIn) ->
	case Record#emxstoreconfig.tableid of
		undefined -> AccIn;
		null -> AccIn;
		TableId -> util_data:close_handle(TableId), AccIn
	end.
	
terminate(_Reason, ConfigHandle) ->
    util_data:foldl(fun closer/2, [], ConfigHandle),
    util_data:close_handle(ConfigHandle),
    io:format("~p stopping ~n", [?MODULE]), 
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.
   
put_data(TableId, Data) ->
    gen_server:call(?GD2, {putData, TableId, Data}, infinity).
    
get_data(TableId, Key) ->
    gen_server:call(?GD2, {getData, TableId, Key}, infinity).
    
get_datakeys(TableId) ->
    gen_server:call(?GD2, {getDataKeys, TableId }, infinity).
   
get_tables() ->
    gen_server:call(?GD2, { getTables }, infinity).
    
run_capacity(TableId) when is_atom(TableId) ->
    gen_server:call(?GD2, {runCapacity, TableId }, infinity);

run_capacity(TableId) when is_list(TableId) ->
    null.
    
%% 
handle_call({putData, TableId, Data}, _From, ConfigHandle) ->
%% Resolve tableid into an access token for util_data
%% Access tokens are actually { ets or dets, tableatom}
    %% Use ConfigHandle to get at the config table for the TableId (which is actually a string that we should find in the ConfigHandle).
    %% If we cannot find the TableId that way, create a new one and add it to ConfigHandle
    %%io:format("Getting table id~n"),
    NewTableId = getTableId(TableId, ConfigHandle),
    %%io:format("Id is ~p~n", [ NewTableId] ),
    Res = util_data:put_data(NewTableId, Data),
    {reply, {datainfo, Res}, ConfigHandle};
    
handle_call({getData, TableId, Key}, _From, ConfigHandle) ->
    NewTableId = getTableId(TableId, ConfigHandle),
    Res = util_data:get_data(NewTableId, Key),
    {reply, {datainfo, Res}, ConfigHandle};
    
handle_call({getDataKeys, TableId}, _From, ConfigHandle) ->
    NewTableId = getTableId(TableId, ConfigHandle),
    Keys = util_data:foldl(fun collectKeys/2, [], NewTableId),
    {reply, {datainfo, Keys}, ConfigHandle};

handle_call({getTables}, _From, ConfigHandle) ->
    Tables = util_data:foldl(fun(Record, AccIn) -> AccIn ++ [ Record ] end, [], ConfigHandle),
    {reply, Tables, ConfigHandle};
    
handle_call({runCapacity, TableId}, _From, ConfigHandle) ->
	%% Load the capacity constraints for the given tablename, then run them...
	TableInfo = getTableInfo(TableId, ConfigHandle),
	run_constraints(TableInfo, TableInfo#emxstoreconfig.capacityconstraints),
	{reply, ok, ConfigHandle}.
    
collectKeys(Record, AccIn) ->
	AccIn ++ [ Record # emxcontent.displayname ].
	
handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

getTableId(TableName, ConfigHandle) ->
	X = getTableInfo(TableName, ConfigHandle),
	X#emxstoreconfig.tableid.
	
getTableInfo(TableId, ConfigHandle) ->
	%%io:format("Config handle is ~p, Table id is ~p~n", [ ConfigHandle, TableId]),
	TableInfo = util_data:get_data(ConfigHandle, TableId),
	%%io:format("Table info is ~p~n", [ TableInfo]),
	case TableInfo of
		[] ->
			[ DefaultTableInfo | _ ] = util_data:get_data(ConfigHandle, "default"),
	%%		io:format("Default table info is ~p~n", [ DefaultTableInfo]),
			NewTableId = util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype, TableId, DefaultTableInfo#emxstoreconfig.storageoptions),
	%		io:format("New Table Id is ~p~n", [ NewTableId]),
			NewTableInfo = DefaultTableInfo#emxstoreconfig { typename = TableId, tableid = NewTableId },
	%%		io:format("Putting config data~n"),
			util_data:put_data(ConfigHandle, NewTableInfo),
	%%		io:format("Returning~n"),
			NewTableInfo;
		[ Info | _ ] ->
			case Info#emxstoreconfig.tableid of
				undefined ->
					TableId = util_data:get_handle(Info#emxstoreconfig.storagetype, Info#emxstoreconfig.storageoptions),
					util_data:put_data(ConfigHandle, Info#emxstoreconfig{tableid = TableId}),
					Info#emxstoreconfig{ tableid = TableId};
				Id ->
					Info
			end
	end.

%% The following functions are to handle the removal of data from a cache to keep it within certain constraints
%% Constraints are by memory use, number of records or age of documents.

run_constraints(TableConfig, []) ->
	ok;
run_constraints(TableConfig, [ H | T]) ->
	run_constraint(TableConfig, H),
	run_constraints(TableConfig, T).
	
run_constraint(TableConfig, { records, MaxCount }) ->
	%% The table should have no more than MaxCount records. If there are more than that,
	%% remove them in age order until the number of records is below MaxCount
	{ RecordCount, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	io:format("Record count is ~p~n", [ RecordCount]),
	case RecordCount > MaxCount of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				{ NewRecordCount, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
				NewRecordCount > MaxCount
				end, SortedList),
			%% Run garbage collection after removing the data to ensure that future tests on memory use the correct figures
			erlang:garbage_collect();				
		false ->
			nothingtodo
	end;
	
run_constraint(TableConfig, { age, MaxAge }) ->
	%% Remove all records older than MaxAge
	TestTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - MaxAge,	
	io:format("Test time is ~p~n", [ TestTime]),
	util_data:foldl(fun(Record, AccIn) ->
		WriteTime = calendar:datetime_to_gregorian_seconds(Record#emxcontent.writetime),
		case WriteTime < TestTime of
			true ->
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname);
			false ->
				ok
		end,
		erlang:garbage_collect(),
		AccIn end, [], TableConfig#emxstoreconfig.tableid);
		
run_constraint(TableConfig, { size, MaxSize }) ->
	%% The table should be smaller than MaxSize. If it is greater, remove records
	%% in age order until the size is reduced below MaxSize
	{ _, Memory } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	io:format("Memory use is ~p~n", [ Memory]),
	case Memory > MaxSize of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				%% Run garbage collect after deletion or the get_size method below will not return the correct
				%% and up to date value. It makes the whole loop slower though
				erlang:garbage_collect(),
				{ _, NewMemory } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
				NewMemory > MaxSize
				end, SortedList);
		false ->
			nothingtodo
	end.

%% Get data from a table and sort it by age

get_all_sorted(TableConfig) ->
	UnsortedList = util_data:foldl(fun(Record, AccIn) -> AccIn ++ [ Record ] end, [], TableConfig#emxstoreconfig.tableid),
	lists:sort(fun(Elem1, Elem2) -> 
					First = calendar:datetime_to_gregorian_seconds(Elem1#emxcontent.writetime),
					Second = calendar:datetime_to_gregorian_seconds(Elem2#emxcontent.writetime),
					Second > First end, UnsortedList). 
			