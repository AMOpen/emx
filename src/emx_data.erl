%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([put_data/2, get_data/2]).

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

terminate(_Reason, ConfigHandle) ->
    util_data:close_handle(ConfigHandle),
    io:format("~p stopping ~n", [?MODULE]), 
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.
   
put_data(TableId, Data) ->
    gen_server:call(?GD2, {putData, TableId, Data}, infinity).
    
get_data(TableId, Key) ->
    gen_server:call(?GD2, {getData, TableId, Key}, infinity).
    
%% 

handle_call({putData, TableId, Data}, _From, ConfigHandle) ->
%% Resolve tableid into an access token for util_data
%% Access tokens are actually { ets or dets, tableatom}
    %% Use ConfigHandle to get at the config table for the TableId (which is actually a string that we should find in the ConfigHandle).
    %% If we cannot find the TableId that way, create a new one and add it to ConfigHandle
    io:format("Getting table id~n"),
    NewTableId = getTableInfo(TableId, ConfigHandle),
    io:format("Id is ~p~n", [ NewTableId] ),
    Res = util_data:put_data(NewTableId, Data),
    {reply, {datainfo, Res}, ConfigHandle};
    
handle_call({getData, TableId, Key}, _From, ConfigHandle) ->
    NewTableId = getTableInfo(TableId, ConfigHandle),
    Res = util_data:get_data(NewTableId, Key),
    {reply, {datainfo, Res}, ConfigHandle}.
    
handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

getTableInfo(TableId, ConfigHandle) ->
	io:format("Config handle is ~p, Table id is ~p~n", [ ConfigHandle, TableId]),
	TableInfo = util_data:get_data(ConfigHandle, TableId),
	io:format("Table info is ~p~n", [ TableInfo]),
	case TableInfo of
		[] ->
			[ DefaultTableInfo | _ ] = util_data:get_data(ConfigHandle, "default"),
			io:format("Default table info is ~p~n", [ DefaultTableInfo]),
			NewTableId = util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype, TableId, DefaultTableInfo#emxstoreconfig.storageoptions),
			io:format("New Table Id is ~p~n", [ NewTableId]),
			NewTableInfo = DefaultTableInfo#emxstoreconfig { typename = TableId, tableid = NewTableId },
			io:format("Putting config data~n"),
			util_data:put_data(ConfigHandle, NewTableInfo),
			io:format("Returning~n"),
			NewTableInfo#emxstoreconfig.tableid;
		[ Info | _ ] ->
			case Info#emxstoreconfig.tableid of
				undefined ->
					TableId = util_data:get_handle(Info#emxstoreconfig.storagetype, Info#emxstoreconfig.storageoptions),
					util_data:put_data(ConfigHandle, Info#emxstoreconfig{tableid = TableId}),
					TableId;
				Id ->
					Id
			end
	end.
					
					
			