%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, start_link/1, terminate/2]).

-export([add_remote_table/1, create_local_table/1, get_config_handle/0,
	 get_data/2, get_datakeys/2, get_tables/0, put_data/3,
	 run_balancer/1, run_capacity/1, update_constraints/2,
	 update_table_info/3]).

-include_lib("emx.hrl").

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
    ?LOG(debug, "Starting emx_data", []),
    {ok, {StorageType, Config}} =
	application:get_env(emxconfig),
    ?LOG(debug, "Storage type ~p, Config ~p",
	 [StorageType, Config]),
    {ok, Setup} = application:get_env(setupstore),
    {ok, DefaultStore} = application:get_env(defaultstore),
    ConfigHandle = util_data:get_handle(StorageType,
					emxconfig, Config),
    case Setup of
      true -> util_data:put_data(ConfigHandle, DefaultStore);
      _ -> ok
    end,
    %% Also register interest in node information
    {ok, Nodes} = application:get_env(nodes),
    ThisNode = node(),
    lists:any(fun (Node) ->
		      case Node of
			local -> false;
			ThisNode -> false;
			Node ->
			    %% Attempt to populate the ConfigHandle from this node
			    Res = populate_from_node(Node, ConfigHandle),
			    erlang:monitor_node(Node, true),
			    Res
		      end
	      end,
	      Nodes),
    %% Probably also need to look at the local archive and setup some tables (and content) from that
    %% (But maybe people should be using dets instead?)
    {ok, ConfigHandle}.

populate_from_node(Node, ConfigHandle) ->
    Res = rpc:call(Node, emx_data, get_tables, []),
    %% We need to add any interesting table to our record, but also
    %% change the tableid to remote
    case Res of
      {badrpc, _} -> false;
      _ ->
	  lists:foreach(fun (TableInfo) ->
				case TableInfo#emxstoreconfig.typename of
				  "default" -> dontdothis;
				  _ ->
				      NewTableInfo =
					  TableInfo#emxstoreconfig{tableid =
								       remote},
				      ?LOG(debug, "Copying info about ~p",
					   [TableInfo#emxstoreconfig.typename]),
				      util_data:put_data(ConfigHandle,
							 NewTableInfo)
				end
			end,
			Res),
	  true
    end.

closer(Record, AccIn) ->
    case Record#emxstoreconfig.tableid of
      undefined -> AccIn;
      null -> AccIn;
      TableId ->
	  case lists:any(fun (Node) -> Node == node() end,
			 Record#emxstoreconfig.location)
	      of
	    true -> util_data:close_handle(TableId);
	    false -> nothing
	  end,
	  AccIn
    end.

terminate(_Reason, ConfigHandle) ->
    util_data:foldl(fun closer/2, [], ConfigHandle),
    util_data:close_handle(ConfigHandle),
    io:format("~p stopping ~n", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

get_config_handle() ->
    gen_server:call(?GD2, {getConfigHandle}, infinity).
    
put_data(TableId, Data, local) ->
    gen_server:call(?GD2, {putData, TableId, Data},
		    infinity);
put_data(TableId, Data, remote) ->
    gen_server:cast(?GD2, {putData, TableId, Data}).

get_data(TableId, Key) ->
    gen_server:call(?GD2, {getData, TableId, Key},
		    infinity).

get_datakeys(TableId, EpochNumber) ->
    gen_server:call(?GD2,
		    {getDataKeys, TableId, EpochNumber}, infinity).

get_tables() ->
    gen_server:call(?GD2, {getTables}, infinity).

add_remote_table(TableInfo) ->
    gen_server:cast(?GD2, {addRemoteTable, TableInfo}).

update_table_info(TableId, Change, Info) ->
    gen_server:cast(?GD2,
		    {updateTableInfo, TableId, Change, Info}).

update_constraints(TableId, Constraints) ->
    gen_server:call(?GD2,
		    {updateConstraints, TableId, Constraints}, infinity).

create_local_table(TableId) ->
    ?LOG(debug, "Create local table for ~p", [TableId]),
    gen_server:call(?GD2, {createLocalTable, TableId},
		    5000).

run_capacity(TableId) when is_atom(TableId) ->
    gen_server:call(?GD2, {runCapacity, TableId}, infinity);
run_capacity(TableId) when is_list(TableId) -> null.

run_balancer(TableId) when is_atom(TableId) ->
    gen_server:call(?GD2, {runBalancer, TableId}, infinity);
run_balancer(TableId) when is_list(TableId) -> null.

%%

handle_call({getConfigHandle}, _From, ConfigHandle) ->
    {reply, ConfigHandle, ConfigHandle};
    
handle_call({putData, TableId, Data}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    %% Resolve tableid into an access token for util_data
    %% Access tokens are actually { ets or dets, tableatom}
    %% Use ConfigHandle to get at the config table for the TableId (which is actually a string that we should find in the ConfigHandle).
    %% If we cannot find the TableId that way, create a new one and add it to ConfigHandle
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    %% Save it to each node in the configuration
    lists:foreach(fun (Node) ->
			  case Node of
			    MyNode ->
				%% Data is an emxstoreconfig record
				CompressedData = util_zip:compress_record(Data),
				%% Update epoch
				UpdatedEpoch = NewTableInfo#emxstoreconfig{epoch
									       =
									       NewTableInfo#emxstoreconfig.epoch
										 +
										 1},
				util_data:put_data(NewTableInfo#emxstoreconfig.tableid,
						   CompressedData#emxcontent{epoch
										 =
										 UpdatedEpoch#emxstoreconfig.epoch}),
				util_data:put_data(ConfigHandle, UpdatedEpoch);
			    Node ->
				%% The final atom remote here will initiate a different path of execution on the remote server,
				%% basically being a cast instead of a call, and without the fan out to other nodes.
				Res = rpc:call(Node, emx_data, put_data,
					       [TableId, Data, remote]),
				case Res of
					{ error, Res } -> ?LOG(debug, "Failed to update remote node with error ~p", [ Res ]);
					_ -> nothing
				end
			  end
		  end,
		  NewTableInfo#emxstoreconfig.location),
    {reply, {datainfo, ok}, ConfigHandle};
handle_call({updateConstraints, TableId, Constraints},
	    _From, ConfigHandle) ->
    util_check:check_message_queue(),
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
    UpdatedInfo =
	NewTableInfo#emxstoreconfig{capacityconstraints =
					Constraints},
    util_data:put_data(ConfigHandle, UpdatedInfo),
    {reply, ok, ConfigHandle};
handle_call({getData, TableId, Key}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    TableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    case lists:any(fun (Node) -> Node == MyNode end,
		   TableInfo#emxstoreconfig.location)
	of
      true ->
	  Res =
	      util_data:get_data(TableInfo#emxstoreconfig.tableid,
				 Key),
	  RealRes = [decompress(Record) || Record <- Res];
      false ->
	  RealRes =
	      get_remote_data(TableInfo#emxstoreconfig.location,
			      [TableId, Key])
    end,
    {reply, {datainfo, RealRes}, ConfigHandle};
handle_call({getDataKeys, TableId, EpochNumber}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    TableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    case lists:any(fun (Node) -> Node == MyNode end,
		   TableInfo#emxstoreconfig.location)
	of
      true ->
	  {MaxEpoch, _, Keys} = util_data:foldl(fun collectKeys/2,
						{EpochNumber, EpochNumber, []},
						TableInfo#emxstoreconfig.tableid);
      false ->
	  {MaxEpoch, Keys} =
	      get_remote_keys(TableInfo#emxstoreconfig.location,
			      [TableId, EpochNumber])
    end,
    {reply, {datainfo, {MaxEpoch, Keys}}, ConfigHandle};
handle_call({getTables}, _From, ConfigHandle) ->
    util_check:check_message_queue(),
    Tables = util_data:foldl(fun (Record, AccIn) ->
				     AccIn ++ [Record]
			     end,
			     [], ConfigHandle),
    {reply, Tables, ConfigHandle};
handle_call({createLocalTable, TableId}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    {reply, low_create_local_table(TableId, ConfigHandle),
     ConfigHandle};
handle_call({runCapacity, TableId}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    %% Load the capacity constraints for the given tablename, then run them...	
    TableInfo = getTableInfo(TableId, ConfigHandle),
    emx_data_constraints:run_constraints(TableInfo,
					 TableInfo#emxstoreconfig.capacityconstraints),
    {reply, ok, ConfigHandle};
handle_call({runBalancer, TableId}, _From,
	    ConfigHandle) ->
    %% Some simple rules
    %% Determine whether we host the table or not
    %% If we do not, and the table has only one host, make a copy and host it ourselves
    %% If we do not, and the table has more than one host, do nothing
    %% If we do, and the table has more than two hosts, retire our copy
    util_check:check_message_queue(),
    TableInfo = getTableInfo(TableId, ConfigHandle),
    OurNode = node(),
    HasLocalCopy = lists:any(fun (Node) -> Node == OurNode
			     end,
			     TableInfo#emxstoreconfig.location),
    Count = length(TableInfo#emxstoreconfig.location),
    emx_data_balancer:processBalanceNode(ConfigHandle,
					 TableInfo, HasLocalCopy, Count),
    {reply, ok, ConfigHandle}.

decompress(Record) ->
    util_zip:decompress_record(Record).

collectKeys(Record, {MaxEpoch, TestEpoch, Keys}) ->
    case Record#emxcontent.epoch > TestEpoch of
      true ->
	  NewKeys = Keys ++ [util_zip:decompress_record(Record)],
	  case Record#emxcontent.epoch > MaxEpoch of
	    true -> NewMaxEpoch = Record#emxcontent.epoch;
	    false -> NewMaxEpoch = MaxEpoch
	  end;
      false -> NewKeys = Keys, NewMaxEpoch = MaxEpoch
    end,
    {NewMaxEpoch, TestEpoch, NewKeys}.

handle_cast({addRemoteTable, TableInfo},
	    ConfigHandle) ->
    util_check:check_message_queue(),
    NewTableInfo = TableInfo#emxstoreconfig{tableid =
						remote},
    util_data:put_data(ConfigHandle, NewTableInfo),
    lists:foreach(fun (Node) ->
			  erlang:monitor_node(Node, true)
		  end,
		  TableInfo#emxstoreconfig.location),
    {noreply, ConfigHandle};
handle_cast({updateTableInfo, "default", nodedown,
	     Node},
	    ConfigHandle) ->
    util_check:check_message_queue(),
    ?LOG(debug, "Node down ~p", [ Node ]),
    {noreply, ConfigHandle};
handle_cast({updateTableInfo, TableId, nodedown, Node},
	    ConfigHandle) ->
    %% The node passed is no longer hosting this table, so remove it from the list
    %% Only remove it if we know about it
    util_check:check_message_queue(),
    case tableExists(TableId, ConfigHandle) of
      true ->
	  TableInfo = getTableInfo(TableId, ConfigHandle),
	  NewTableInfo = TableInfo#emxstoreconfig{location =
						      [V1
						       || V1
							      <- TableInfo#emxstoreconfig.location,
							  handle_cast_1(V1,
									Node)]},
	  case NewTableInfo#emxstoreconfig.location of
	    [] ->
		?LOG(debug, "Would clean up obsolete table", []),
		util_data:put_data(ConfigHandle, NewTableInfo);
	    %%util_data:delete_data(ConfigHandle, NewTableInfo#emxstoreconfig.typename),
	    %%util_data:close_handle(NewTableInfo#emxstoreconfig.tableid);
	    _ -> util_data:put_data(ConfigHandle, NewTableInfo)
	  end;
      false -> donothing
    end,
    {noreply, ConfigHandle};
handle_cast({updateTableInfo, TableId, nodeup, Node},
	    ConfigHandle) ->
    util_check:check_message_queue(),
    ?LOG(debug, "Processing node up for ~p", [Node]),
    TableInfo = getTableInfo(TableId, ConfigHandle),
    NewTableInfo = TableInfo#emxstoreconfig{location =
						TableInfo#emxstoreconfig.location
						  ++ [Node]},
    util_data:put_data(ConfigHandle, NewTableInfo),
    %% There is no harm in monitoring this node again
    erlang:monitor_node(Node, true),
    {noreply, ConfigHandle};
handle_cast({putData, TableId, Data}, ConfigHandle) ->
    util_check:check_message_queue(),
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
    MyNode = node(),
    case lists:any(fun (Node) -> Node == MyNode end,
		   NewTableInfo#emxstoreconfig.location)
	of
      true ->
	  CompressedData = util_zip:compress_record(Data),
	  %% Update epoch
	  UpdatedEpoch = NewTableInfo#emxstoreconfig{epoch =
							 NewTableInfo#emxstoreconfig.epoch
							   + 1},
	  util_data:put_data(NewTableInfo#emxstoreconfig.tableid,
				 CompressedData#emxcontent{epoch =
							       UpdatedEpoch#emxstoreconfig.epoch}),
	  util_data:put_data(ConfigHandle, UpdatedEpoch);
      false -> nothingtodo
    end,
    {noreply, ConfigHandle};
handle_cast(Msg, N) ->
    ?LOG(debug, "Received cast ~p", [Msg]), {noreply, N}.

handle_cast_1(NodeInfo, Node) -> NodeInfo /= Node.

handle_info({nodedown, Node}, ConfigHandle) ->
    util_check:check_message_queue(),
    ?LOG(debug, "Node ~p is down", [Node]),
    %% Remove node from each table that has it
    util_data:foldl(fun (TableInfo, AccIn) ->
			    emx_data:update_table_info(TableInfo#emxstoreconfig.typename,
						       nodedown, Node),
			    AccIn
		    end,
		    [], ConfigHandle),
    {noreply, ConfigHandle};
handle_info(Info, N) ->
    ?LOG(debug, "Received info ~p", [Info]), {noreply, N}.

tableExists(TableId, ConfigHandle) ->
    TableInfo = util_data:get_data(ConfigHandle, TableId),
    case TableInfo of
      [] -> false;
      _ -> true
    end.

getTableInfo(TableId, ConfigHandle) ->
    TableInfo = util_data:get_data(ConfigHandle, TableId),
    case TableInfo of
      [] -> get_location_for_new_table(TableId, ConfigHandle);
      [Info | _] ->
	  case Info#emxstoreconfig.location of
	    [] -> get_location_for_new_table(TableId, ConfigHandle);
	    _ -> Info
	  end
    end.

low_create_local_table(TableId, ConfigHandle) ->
    ?LOG(debug, "Getting low_create_local_table ~p",
	 [TableId]),
    [DefaultTableInfo | _] =
	util_data:get_data(ConfigHandle, "default"),
    NewTableInfo = DefaultTableInfo#emxstoreconfig{typename
						       = TableId,
						   epoch = 0,
						   location = [node()]},
    NewTableId =
	util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype,
			     TableId,
			     DefaultTableInfo#emxstoreconfig.storageoptions),
    RetTableInfo = NewTableInfo#emxstoreconfig{tableid =
						   NewTableId},
    %%RealTableInfo = populate_from_archive(RetTableInfo),
    util_data:put_data(ConfigHandle, RetTableInfo),
    {ok, PossibleNodes} = application:get_env(nodes),
    MyNode = node(),
    lists:foreach(fun (Node) ->
			  case Node of
			    MyNode -> null;
			    _ ->
				rpc:call(Node, emx_data, add_remote_table,
					 [RetTableInfo])
			  end
		  end,
		  PossibleNodes),
    RetTableInfo.

%%% Given a tableId (e.g. 'official_trade.aladdin'), where should we put this table?
%%% Return an emxstoreconfig record to represent it.

get_location_for_new_table(TableId, ConfigHandle) ->
    get_location_for_new_table(TableId, ConfigHandle, []).

get_location_for_new_table(TableId, ConfigHandle,
			   ExcludeNodes) ->
    %% Now get the nodes that could host this
    {ok, Nodes} = application:get_env(nodes),
    RealList = [Node
		|| Node <- Nodes,
		   is_not_excluded(Node, ExcludeNodes)],
    %% Now pick one (at random initially) - this will always eventually end up at the local node if
    %% none are online.
    NodeToHost = util_randomext:pickCount(RealList, 1),
    MyNode = node(),
    case NodeToHost of
      MyNode -> low_create_local_table(TableId, ConfigHandle);
      Node ->
          %% Could this caused a deadlock if the other node is in the same point as us?
	  Resp = rpc:call(Node, emx_data, create_local_table,
			  [TableId]),
	  case Resp of
	    {badrpc, _} ->
		get_location_for_new_table(TableId, ConfigHandle,
					   ExcludeNodes ++ [Node]);
	    Resp ->
		ModifiedResp = Resp#emxstoreconfig{location = [Node],
						   tableid = {remote}},
		util_data:put_data(ConfigHandle, ModifiedResp),
		ModifiedResp
	  end
    end.

%% Return true if the given Node is not in the list of Excluded Nodes
is_not_excluded(Node, ExcludeNodes) ->
    lists:all(fun (Exclude) -> Exclude /= Node end,
	      ExcludeNodes).

get_remote_data([], _Params) -> #emxcontent{};
get_remote_data([Node | T], Params) ->
    ?LOG(debug, "Trying ~p", [Node]),
    Res = rpc:call(Node, emx_data, get_data, Params),
    case Res of
      {badrpc, _} ->
	  ?LOG(debug, "failed, trying next", []),
	  get_remote_data(T, Params);
      {datainfo, Resp} -> Resp
    end.

get_remote_keys([], _Params) -> {0, []};
get_remote_keys([Node | T], Params) ->
    ?LOG(debug, "Trying ~p", [Node]),
    Res = rpc:call(Node, emx_data, get_datakeys, Params),
    case Res of
      {badrpc, _} ->
	  ?LOG(debug, "failed, trying next", []),
	  get_remote_keys(T, Params);
      {datainfo, Resp} -> Resp
    end.
