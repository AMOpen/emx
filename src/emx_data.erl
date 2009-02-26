%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, start_link/1, terminate/2]).

-export([add_remote_table/1, create_local_table/1,
	 get_data/2, get_datakeys/2, get_tables/0, put_data/3,
	 run_capacity/1, update_constraints/2,
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
    {ok, ConfigHandle}.

closer(Record, AccIn) ->
    case Record#emxstoreconfig.tableid of
      undefined -> AccIn;
      null -> AccIn;
      TableId -> util_data:close_handle(TableId)
      end,
     AccIn.

terminate(_Reason, ConfigHandle) ->
    util_data:foldl(fun closer/2, [], ConfigHandle),
    util_data:close_handle(ConfigHandle),
    io:format("~p stopping ~n", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

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

%%
handle_call({putData, TableId, Data}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    %% Resolve tableid into an access token for util_data
    %% Access tokens are actually { ets or dets, tableatom}
    %% Use ConfigHandle to get at the config table for the TableId (which is actually a string that we should find in the ConfigHandle).
    %% If we cannot find the TableId that way, create a new one and add it to ConfigHandle
    NewTableInfo = getTableInfo(TableId, ConfigHandle),
%% Data is an emxstoreconfig record
CompressedData = util_zip:compress_record(Data),
%% Update epoch
UpdatedEpoch = NewTableInfo#emxstoreconfig{
epoch=NewTableInfo#emxstoreconfig.epoch
										 +
										 1},
				util_data:put_data(NewTableInfo#emxstoreconfig.tableid,
						   CompressedData#emxcontent{epoch
										 =
										 UpdatedEpoch#emxstoreconfig.epoch}),
				util_data:put_data(ConfigHandle, UpdatedEpoch),
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
    Res = util_data:get_data(TableInfo#emxstoreconfig.tableid,
				 Key),
	  RealRes = [decompress(Record) || Record <- Res],
    {reply, {datainfo, RealRes}, ConfigHandle};
handle_call({getDataKeys, TableId, EpochNumber}, _From,
	    ConfigHandle) ->
    util_check:check_message_queue(),
    TableInfo = getTableInfo(TableId, ConfigHandle),
	  {MaxEpoch, _, Keys} = util_data:foldl(fun collectKeys/2,
						{EpochNumber, EpochNumber, []},
						TableInfo#emxstoreconfig.tableid),
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
      [Info | _] -> Info
    end.

low_create_local_table(TableId, ConfigHandle) ->
    ?LOG(debug, "Getting low_create_local_table ~p",
	 [TableId]),
    [DefaultTableInfo | _] =
	util_data:get_data(ConfigHandle, "default"),
    NewTableInfo = DefaultTableInfo#emxstoreconfig{typename
						       = TableId,
						   epoch = 0},
    NewTableId =
	util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype,
			     TableId,
			     DefaultTableInfo#emxstoreconfig.storageoptions),
    RetTableInfo = NewTableInfo#emxstoreconfig{tableid =
						   NewTableId},
    %%RealTableInfo = populate_from_archive(RetTableInfo),
    util_data:put_data(ConfigHandle, RetTableInfo),
    RetTableInfo.

%%% Given a tableId (e.g. 'official_trade.aladdin'), where should we put this table?
%%% Return an emxstoreconfig record to represent it.

get_location_for_new_table(TableId, ConfigHandle) ->
    get_location_for_new_table(TableId, ConfigHandle, []).

get_location_for_new_table(TableId, ConfigHandle,
			   ExcludeNodes) ->
    %% Now get the nodes that could host this
      low_create_local_table(TableId, ConfigHandle).

