%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([put_data/3, get_data/2, get_datakeys/2, run_capacity/1, get_tables/0, update_constraints/2, create_local_table/1,
	add_remote_table/1, update_table_info/3, run_balancer/1]).

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
    util_flogger:logMsg(self(), ?MODULE, debug, "Starting emx_data"),
    {ok, {StorageType, Config}} = application:get_env(emxconfig),
    util_flogger:logMsg(self(), ?MODULE, debug, "Storage type ~p, Config ~p", [ StorageType, Config]),
    {ok, Setup} = application:get_env(setupstore),
    {ok, DefaultStore} = application:get_env(defaultstore),

    ConfigHandle = util_data:get_handle(StorageType, emxconfig, Config),
    case Setup of 
    	true ->
		util_data:put_data(ConfigHandle, DefaultStore);
	_ ->
		ok
    end,    
    
    %% Also register interest in node information
    {ok, Nodes} = application:get_env(nodes),
    ThisNode = node(),
    lists:any(fun(Node) ->
    		case Node of
			local -> false;
			ThisNode -> false;
			Node -> 
				%% Attempt to populate the ConfigHandle from this node
				Res = populate_from_node(Node, ConfigHandle),
				erlang:monitor_node(Node, true),
				Res
		end
		end, Nodes),
    {ok, ConfigHandle}.

populate_from_node(Node, ConfigHandle) ->
	Res = rpc:call(Node, emx_data, get_tables, []),
	util_flogger:logMsg(self(), ?MODULE, debug, "Response from populate_from_node is ~p", [ Res ]),
	%% We need to add any interesting table to our record, but also
	%% change the tableid to remote
	case Res of
		{badrpc, _ } -> false;
		_ ->
			lists:foreach(fun(TableInfo) ->
				case TableInfo#emxstoreconfig.typename of
					"default" -> dontdothis;
					_ ->  
						NewTableInfo = TableInfo#emxstoreconfig { tableid = remote },
						util_data:put_data(ConfigHandle, NewTableInfo)
				end
				end, Res),
			true
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
   
put_data(TableId, Data, local) ->
    gen_server:call(?GD2, {putData, TableId, Data}, infinity);
put_data(TableId, Data, remote) ->
    gen_server:cast(?GD2, {putData, TableId, Data}).
    
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
    util_flogger:logMsg(self(), ?MODULE, debug, "Create local table for ~p", [ TableId]),
    gen_server:call(?GD2, { createLocalTable, TableId}, infinity).
    
run_capacity(TableId) when is_atom(TableId) ->
    gen_server:call(?GD2, {runCapacity, TableId }, infinity);

run_capacity(TableId) when is_list(TableId) ->
    null.
    
run_balancer(TableId) when is_atom(TableId) ->
    gen_server:call(?GD2, {runBalancer, TableId}, infinity);
    
run_balancer(TableId) when is_list(TableId) ->
    null.
    
%% 
handle_call({putData, TableId, Data}, _From, ConfigHandle) ->
%% Resolve tableid into an access token for util_data
%% Access tokens are actually { ets or dets, tableatom}
    %% Use ConfigHandle to get at the config table for the TableId (which is actually a string that we should find in the ConfigHandle).
    %% If we cannot find the TableId that way, create a new one and add it to ConfigHandle
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
			Res = rpc:call(Node, emx_data, put_data, [ TableId, Data, remote])
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
    case lists:any(fun(Node) -> Node == MyNode end, TableInfo#emxstoreconfig.location) of
    	true ->
		Res = util_data:get_data(TableInfo#emxstoreconfig.tableid, Key),
		RealRes = lists:map(fun(Record) -> util_zip:decompress_record(Record) end, Res);
	false ->
		RealRes = get_remote_data(TableInfo#emxstoreconfig.location, [TableId, Key])
    end,
    {reply, {datainfo, RealRes}, ConfigHandle};
    
handle_call({getDataKeys, TableId, EpochNumber}, _From, ConfigHandle) ->
    TableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    case lists:any(fun(Node) -> Node == MyNode end, TableInfo#emxstoreconfig.location) of
    	true ->
		{MaxEpoch, _, Keys} = util_data:foldl(fun collectKeys/2, {EpochNumber, EpochNumber, []}, TableInfo#emxstoreconfig.tableid);
	false ->
		{MaxEpoch, Keys} = get_remote_keys(TableInfo#emxstoreconfig.location, [ TableId, EpochNumber])
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
	{reply, ok, ConfigHandle};
    
handle_call({runBalancer, TableId}, _From, ConfigHandle) ->
	%% Some simple rules
	%% Determine whether we host the table or not
	%% If we do not, and the table has only one host, make a copy and host it ourselves
	%% If we do not, and the table has more than one host, do nothing
	%% If we do, and the table has more than two hosts, retire our copy
	TableInfo = getTableInfo(TableId, ConfigHandle),
	OurNode = node(),
	HasLocalCopy = lists:any(fun(Node) -> Node == OurNode end, TableInfo#emxstoreconfig.location),
	Count = length(TableInfo#emxstoreconfig.location),
	
	processBalanceNode(ConfigHandle, TableInfo, HasLocalCopy, Count),
	
	{reply, ok, ConfigHandle}.
	
processBalanceNode(ConfigHandle, TableInfo, false, Count) when Count < 2 ->
	NewTableId = util_data:get_handle(TableInfo#emxstoreconfig.storagetype, TableInfo#emxstoreconfig.typename, TableInfo#emxstoreconfig.storageoptions),
	NodeList = TableInfo#emxstoreconfig.location ++ [ node() ],
	UpdatedTableInfo = TableInfo#emxstoreconfig { location = NodeList, tableid = NewTableId },
	util_data:put_data(ConfigHandle, UpdatedTableInfo),
	%% Now we also need to copy over data from the other node
	[TestNode | _ ] = TableInfo#emxstoreconfig.location,
	
	DataList = rpc:call(TestNode, emx_data, get_datakeys, [ TableInfo#emxstoreconfig.typename, 0]),
	case DataList of
		{badrpc, _ } -> someerror;
		{datainfo, { MaxEpoch, List}} ->
			lists:foreach(fun(Record) -> 
				util_flogger:logMsg(self(), ?MODULE, debug, "Copying ~p", [ Record#emxcontent.displayname ]),
				util_data:put_data(NewTableId, Record) end, List)
	end,
	%% Now inform all of the other nodes that have this table that we have it too
	{ok, Nodes} = application:get_env(nodes),
	MyNode = node(),
	lists:foreach(
		fun(Node) ->
			util_flogger:logMsg(self(), ?MODULE, debug, "Informing ~p of the new node", [ Node]),
			case Node of
				MyNode -> nothing;
				_ -> rpc:call(Node, emx_data, update_table_info, [ TableInfo#emxstoreconfig.typename, nodeup, MyNode]) 
			end
		end, 
		Nodes);		
	
processBalanceNode(ConfigHandle, TableInfo, true, Count) when Count > 2 ->
	util_flogger:logMsg(self(), ?MODULE, debug, "Should remove balance ~p", [ TableInfo]);
processBalanceNode(ConfigHandle, TableInfo, true, _) ->
	%% Check number of records. If there are none, remove us from the table
	{ Size, Memory } = util_data:get_size(TableInfo#emxstoreconfig.tableid),
	case Size of
		0 -> 
		     util_flogger:logMsg(self(), ?MODULE, debug, "No data in table ~p, removing from my interest", [ TableInfo#emxstoreconfig.typename]);
		     %% Something different here
		     %% emx_data:update_table_info(TableInfo#emxstoreconfig.typename, nodedown, node());	     
		_ -> util_flogger:logMsg(self(), ?MODULE, debug, "Do nothing with ~p", [ TableInfo])
	end;
processBalanceNode(ConfigHandle, TableInfo, _, _) ->
	donothing.
	
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
			util_flogger:logMsg(self(), ?MODULE, debug, "Would clean up obsolete table");
			%%util_data:delete_data(ConfigHandle, NewTableInfo#emxstoreconfig.typename),
			%%util_data:close_handle(NewTableInfo#emxstoreconfig.tableid);
		_ ->
			util_data:put_data(ConfigHandle, NewTableInfo)
	end,
	{noreply, ConfigHandle};

handle_cast({updateTableInfo, TableId, nodeup, Node }, ConfigHandle) ->
	util_flogger:logMsg(self(), ?MODULE, debug, "Processing node up for ~p", [Node]),
	TableInfo = getTableInfo(TableId, ConfigHandle),
	NewTableInfo = TableInfo#emxstoreconfig { location = TableInfo#emxstoreconfig.location ++ [ Node ] },
	util_data:put_data(ConfigHandle, NewTableInfo),
	%% There is no harm in monitoring this node again
	erlang:monitor_node(Node, true),
	{noreply, ConfigHandle};
	

	
handle_cast({putData, TableId, Data}, ConfigHandle) ->
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    case lists:any(fun(Node) -> Node == MyNode end, NewTableInfo#emxstoreconfig.location) of
    	true ->
		CompressedData = util_zip:compress_record(Data),
		%% Update epoch
		UpdatedEpoch = NewTableInfo#emxstoreconfig{ epoch = NewTableInfo#emxstoreconfig.epoch + 1 },  
		Res = util_data:put_data(NewTableInfo#emxstoreconfig.tableid, CompressedData#emxcontent{ epoch = UpdatedEpoch#emxstoreconfig.epoch }),
		util_data:put_data(ConfigHandle, UpdatedEpoch);
	false ->
		nothingtodo
	end,
    {noreply, ConfigHandle};
    
handle_cast(Msg, N) -> 
	util_flogger:logMsg(self(), ?MODULE, debug, "Received cast ~p", [ Msg ]),
	{noreply, N}.


handle_info({nodedown, Node}, ConfigHandle) ->
	util_flogger:logMsg(self(), ?MODULE, debug, "Node ~p is down", [ Node]),
	%% Remove node from each table that has it
	util_data:foldl(fun(TableInfo, AccIn) -> emx_data:update_table_info(TableInfo#emxstoreconfig.typename, nodedown, Node), AccIn end, [], ConfigHandle), 
	{noreply, ConfigHandle};
	
handle_info(Info, N) -> 
	util_flogger:logMsg(self(), ?MODULE, debug, "Received info ~p", [ Info ]),
	{noreply, N}.
	
getTableInfo(TableId, ConfigHandle) ->
	TableInfo = util_data:get_data(ConfigHandle, TableId),
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
		system -> null;
		_ ->
			case lists:any(fun(Node) -> Node == node() end, TableConfig#emxstoreconfig.location) of
				true -> run_constraint(TableConfig, H);
				false -> null
			end
	end,
	run_constraints(TableConfig, T).
	
run_constraint(TableConfig, { records, MaxCount }) ->
	%% The table should have no more than MaxCount records. If there are more than that,
	%% remove them in age order until the number of records is below MaxCount
	{ RecordCount, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	case RecordCount > MaxCount of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p to keep number of records in limit", [ Record#emxcontent.displayname]),
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
	util_data:foldl(fun(Record, AccIn) ->
		WriteTime = calendar:datetime_to_gregorian_seconds(Record#emxcontent.writetime),
		case WriteTime < TestTime of
			true ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p as it is old", [ Record#emxcontent.displayname]),
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
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p to free up memory", [ Record#emxcontent.displayname]),
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
        util_flogger:logMsg(self(), ?MODULE, debug, "Getting low_create_local_table ~p", [TableId]),
	[ DefaultTableInfo | _ ] = util_data:get_data(ConfigHandle, "default"),
	NewTableInfo = DefaultTableInfo#emxstoreconfig{ typename = TableId, epoch=0, location = [ node() ] },
	NewTableId = util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype, TableId, DefaultTableInfo#emxstoreconfig.storageoptions),
	RetTableInfo = NewTableInfo#emxstoreconfig { tableid = NewTableId },
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
	case NodeToHost of
		MyNode -> low_create_local_table(TableId, ConfigHandle);
		Node  ->  
			Resp = rpc:call(Node, emx_data, create_local_table, [ TableId]),
			case Resp of
				{badrpc, _} ->
						get_location_for_new_table(TableId, ConfigHandle, ExcludeNodes ++ [ Node ]);
				Resp ->
					ModifiedResp = Resp#emxstoreconfig { location = [ Node ], tableid = { remote }},
					util_data:put_data(ConfigHandle, ModifiedResp),
					ModifiedResp
			end
	end.
	
get_remote_data([], Params) ->
	#emxcontent{ };
	
get_remote_data([Node | T], Params) ->
	util_flogger:logMsg(self(), ?MODULE, debug, "Trying ~p", [Node]),
	Res = rpc:call(Node, emx_data, get_data, Params),
	case Res of
		{badrpc, _ } -> 
				util_flogger:logMsg(self(), ?MODULE, debug, "failed, trying next"),
				get_remote_data(T, Params);
		{datainfo, Resp } -> Resp
	end.

get_remote_keys([], Params) ->
	{ 0, [] };
get_remote_keys([Node | T], Params) ->
	util_flogger:logMsg(self(), ?MODULE, debug, "Trying ~p", [Node]),
	Res = rpc:call(Node, emx_data, get_datakeys, Params),
	case Res of 
		{badrpc, _ } ->
				util_flogger:logMsg(self(), ?MODULE, debug, "failed, trying next"),
				get_remote_keys(T, Params);
		{datainfo, Resp} -> Resp
	end.
	
