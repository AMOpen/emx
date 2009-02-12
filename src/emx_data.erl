%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([put_data/2, get_data/2, get_datakeys/2, run_capacity/1, get_tables/0, update_constraints/2, create_local_table/1,
	add_remote_table/1, update_table_info/3]).

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
    
    %% Also register interest in node information
    {ok, Nodes} = application:get_env(nodes),
    ThisNode = node(),
    lists:foreach(fun(Node) ->
    		case Node of
			local -> nothing;
			ThisNode -> nothing;
			Node -> 
				%% Attempt to populate the ConfigHandle from this node
				populate_from_node(Node, ConfigHandle),
				erlang:monitor_node(Node, true)
		end
		end, Nodes),
    {ok, ConfigHandle}.

populate_from_node(Node, ConfigHandle) ->
	Res = rpc:call(Node, emx_data, get_tables, []),
	io:format("Response from populate_from_node is ~p~n", [ Res ]),
	%% We need to add any interesting table to our record, but also
	%% change the tableid to remote
	case Res of
		{badrpc, _ } -> nothing;
		_ ->
			lists:foreach(fun(TableInfo) ->
				case TableInfo#emxstoreconfig.typename of
					"default" -> dontdothis;
					_ ->  
						NewTableInfo = TableInfo#emxstoreconfig { tableid = remote },
						util_data:put_data(ConfigHandle, NewTableInfo)
				end
				end, Res)
	end.
		
	
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
    
get_datakeys(TableId, EpochNumber) ->
    gen_server:call(?GD2, {getDataKeys, TableId, EpochNumber }, infinity).
   
get_tables() ->
    gen_server:call(?GD2, { getTables }, infinity).

add_remote_table(TableInfo) ->
    gen_server:cast(?GD2, { addRemoteTable, TableInfo }).
    
update_table_info(TableId, Change, Info) ->
    gen_server:cast(?GD2, { updateTableInfo, TableId, Change, Info }).
    
update_constraints(TableId, Constraints) ->
    gen_server:call(?GD2, { updateConstraints, TableId, Constraints}, infinity).
    
create_local_table(TableId) ->
    io:format("Create local table for ~p~n", [ TableId]),
    gen_server:call(?GD2, { createLocalTable, TableId}, infinity).
    
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
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    %% Save it to each node in the configuration
    lists:foreach(fun(Node) ->
    	case Node of
    		MyNode ->
			%% Data is an emxstoreconfig record
			CompressedData = util_zip:compress_record(Data),
			%% Update epoch
			UpdatedEpoch = NewTableInfo#emxstoreconfig{ epoch = NewTableInfo#emxstoreconfig.epoch + 1 },  
			Res = util_data:put_data(NewTableInfo#emxstoreconfig.tableid, CompressedData#emxcontent{ epoch = UpdatedEpoch#emxstoreconfig.epoch }),
			util_data:put_data(ConfigHandle, UpdatedEpoch);
		Node ->
			Res = rpc:call(Node, emx_data, put_data, [ TableId, Data])
	end
	end,
	NewTableInfo#emxstoreconfig.location),
    {reply, {datainfo, ok}, ConfigHandle};

handle_call({updateConstraints, TableId, Constraints}, _From, ConfigHandle) ->
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
    UpdatedInfo = NewTableInfo#emxstoreconfig{ capacityconstraints = Constraints },
    util_data:put_data(ConfigHandle, UpdatedInfo),
    {reply, ok, ConfigHandle};
    
handle_call({getData, TableId, Key}, _From, ConfigHandle) ->
    TableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    Location = util_randomext:pickCount(TableInfo#emxstoreconfig.location, 1),
    case Location of
        MyNode -> Res = util_data:get_data(TableInfo#emxstoreconfig.tableid, Key),
		 RealRes = lists:map(fun(Record) -> util_zip:decompress_record(Record) end, Res);
	Location ->  {datainfo, RealRes} = rpc:call(Location, emx_data, get_data, [ TableId, Key])
    end,
    {reply, {datainfo, RealRes}, ConfigHandle};
    
handle_call({getDataKeys, TableId, EpochNumber}, _From, ConfigHandle) ->
    TableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    Location = util_randomext:pickCount(TableInfo#emxstoreconfig.location, 1),
    case Location of
    	MyNode -> {MaxEpoch, _, Keys} = util_data:foldl(fun collectKeys/2, {EpochNumber, EpochNumber, []}, TableInfo#emxstoreconfig.tableid);
	Location -> {datainfo, {MaxEpoch, Keys}} = rpc:call(Location, emx_data, get_datakeys, [ TableId, EpochNumber])
    end,
    {reply, {datainfo, {MaxEpoch, Keys}}, ConfigHandle};

handle_call({getTables}, _From, ConfigHandle) ->
    Tables = util_data:foldl(fun(Record, AccIn) -> AccIn ++ [ Record ] end, [], ConfigHandle),
    {reply, Tables, ConfigHandle};
    
handle_call({createLocalTable, TableId}, _From, ConfigHandle) ->
    {reply,  low_create_local_table(TableId, ConfigHandle), ConfigHandle};
    
handle_call({runCapacity, TableId}, _From, ConfigHandle) ->
	%% Load the capacity constraints for the given tablename, then run them...
	TableInfo = getTableInfo(TableId, ConfigHandle),
	run_constraints(TableInfo, TableInfo#emxstoreconfig.capacityconstraints),
	{reply, ok, ConfigHandle}.
    
collectKeys(Record, {MaxEpoch, TestEpoch, Keys}) ->
	case Record#emxcontent.epoch > TestEpoch of
		true ->
			NewKeys = Keys ++ [ util_zip:decompress_record(Record) ],
			NewMaxEpoch = Record#emxcontent.epoch;
		false ->
			NewKeys = Keys,
			NewMaxEpoch = MaxEpoch
	end,
	{ NewMaxEpoch, TestEpoch, NewKeys}.

	
handle_cast({addRemoteTable, TableInfo}, ConfigHandle) ->
	util_data:put_data(ConfigHandle, TableInfo),
	lists:foreach(fun(Node) -> erlang:monitor_node(Node, true) end, TableInfo#emxstoreconfig.location),
	{noreply, ConfigHandle};

handle_cast({updateTableInfo, "default", nodedown, Node }, ConfigHandle) ->
	{noreply, ConfigHandle};

handle_cast({updateTableInfo, TableId, nodedown, Node }, ConfigHandle) ->
	%% The node passed is no longer hosting this table, so remove it from the list
	TableInfo = getTableInfo(TableId, ConfigHandle),
	NewTableInfo = TableInfo#emxstoreconfig { location = 
			lists:filter(fun(NodeInfo) -> NodeInfo /= Node end, TableInfo#emxstoreconfig.location) },
	case NewTableInfo#emxstoreconfig.location of
		[] ->
			util_data:delete_data(ConfigHandle, NewTableInfo#emxstoreconfig.typename);
		_ ->
			util_data:put_data(ConfigHandle, NewTableInfo)
	end,
	{noreply, ConfigHandle};

handle_cast({updateTableInfo, TableId, nodeup, Node }, ConfigHandle) ->
	TableInfo = getTableInfo(TableId, ConfigHandle),
	NewTableInfo = TableInfo#emxstoreconfig { location = TableInfo#emxstoreconfig.location ++ [ Node ] },
	util_data:put_data(ConfigHandle, NewTableInfo),
	%% There is no harm in monitoring this node again
	erlang:monitor_node(Node, true),
	{noreply, ConfigHandle};
	
handle_cast(Msg, N) -> 
	io:format("Received cast ~p~n", [ Msg ]),
	{noreply, N}.

handle_info({nodedown, Node}, ConfigHandle) ->
	io:format("Node ~p is down~n", [ Node]),
	%% Remove node from each table that has it
	util_data:foldl(fun(TableInfo, AccIn) -> emx_data:update_table_info(TableInfo#emxstoreconfig.typename, nodedown, Node), AccIn end, [], ConfigHandle), 
	{noreply, ConfigHandle};
	
handle_info(Info, N) -> 
	io:format("Received info ~p~n", [ Info ]),
	{noreply, N}.
	
getTableInfo(TableId, ConfigHandle) ->
	%%io:format("Config handle is ~p, Table id is ~p~n", [ ConfigHandle, TableId]),
	TableInfo = util_data:get_data(ConfigHandle, TableId),
	%%io:format("Table info is ~p~n", [ TableInfo]),
	case TableInfo of
		[] ->
			get_location_for_new_table(TableId, ConfigHandle);
		[ Info | _ ] ->
			Info
	end.

%% The following functions are to handle the removal of data from a cache to keep it within certain constraints
%% Constraints are by memory use, number of records or age of documents.

run_constraints(_TableConfig, []) ->
	ok;
run_constraints(TableConfig, [ H | T]) ->
	case TableConfig#emxstoreconfig.location of
		local -> run_constraint(TableConfig, H);
		_ -> null
	end,
	run_constraints(TableConfig, T).
	
run_constraint(TableConfig, { records, MaxCount }) ->
	%% The table should have no more than MaxCount records. If there are more than that,
	%% remove them in age order until the number of records is below MaxCount
	{ RecordCount, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	%%io:format("Record count is ~p~n", [ RecordCount]),
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
	TestTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - MaxAge,	
	%%io:format("Test time is ~p~n", [ TestTime]),
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
	%%io:format("Memory use is ~p~n", [ Memory]),
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

low_create_local_table(TableId, ConfigHandle) ->
        io:format("Getting low_create_local_table~n"),
	[ DefaultTableInfo | _ ] = util_data:get_data(ConfigHandle, "default"),
	NewTableInfo = DefaultTableInfo#emxstoreconfig{ typename = TableId, epoch=0, location = [ node() ] },
	NewTableId = util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype, TableId, DefaultTableInfo#emxstoreconfig.storageoptions),
	RetTableInfo = NewTableInfo#emxstoreconfig { tableid = NewTableId },
	io:format("Ret Table info is ~p~n", [ RetTableInfo]),
	util_data:put_data(ConfigHandle, RetTableInfo),
	{ok, PossibleNodes} = application:get_env(nodes),
	MyNode = node(),
	lists:foreach(fun(Node) -> 
		case Node of
			MyNode -> null;
			_ -> 
				rpc:call(Node, emx_data, add_remote_table, [RetTableInfo])
		end
		end, PossibleNodes),
	RetTableInfo.

%%% Given a tableId (e.g. 'official_trade.aladdin'), where should we put this table?
%%% Return an emxstoreconfig record to represent it.

get_location_for_new_table(TableId, ConfigHandle) ->
	get_location_for_new_table(TableId, ConfigHandle, []).
	
get_location_for_new_table(TableId, ConfigHandle, ExcludeNodes) ->
	%% Now get the nodes that could host this
	{ok, Nodes} = application:get_env(nodes),
	RealList = lists:filter(fun(Node) -> lists:all(fun(Exclude) -> Exclude /= Node end, ExcludeNodes) end, Nodes),
	%% Now pick one (at random initially)
	NodeToHost  = util_randomext:pickCount(RealList, 1),
	MyNode = node(),
	io:format("Node to host is ~p~n", [ NodeToHost] ),
	case NodeToHost of
		MyNode -> low_create_local_table(TableId, ConfigHandle);
		Node  ->  
			io:format("Making remote call to ~p~n", [ Node]),
			Resp = rpc:call(Node, emx_data, create_local_table, [ TableId]),
			case Resp of
				{badrpc, _} ->
						get_location_for_new_table(TableId, ConfigHandle, ExcludeNodes ++ [ Node ]);
				Resp ->
					io:format("Remote Response is ~p~n", [ Resp]),
					ModifiedResp = Resp#emxstoreconfig { location = [ Node ], tableid = { remote }},
					util_data:put_data(ConfigHandle, ModifiedResp),
					ModifiedResp
			end
	end.
	
	
