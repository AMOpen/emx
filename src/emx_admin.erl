%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_admin).

%% Top level access to put and get data into the cache

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([put_data/2, get_data/1, get_datakeys/1, housekeep/0]).

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
    {ok, []}.

terminate(_Reason, ConfigHandle) ->
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.
   
put_data(Key, Content) ->
    gen_server:call(?GD2, {putData, string:join(string:tokens(Key, " "), "_"), Content}, infinity).
    
get_data(Key) ->
    gen_server:call(?GD2, {getData, Key}, infinity).
   
get_datakeys(Prefix) ->
    gen_server:call(?GD2, {getDataKeys, Prefix}, infinity).
%% 

housekeep() ->
    gen_server:call(?GD2, {housekeep}, infinity).

getTableId(Key) ->
    [ Protocol, Type | _ ] = string:tokens(Key, "/"),
    list_to_atom(string:join([ Protocol, Type ], "_")).

handle_call({putData, Key, Content}, _From, N) ->
    %% The Key is the key we want to use, and Content is the xml content we wish to store. We need to 
    %% get the first 2 parts of the Key (parsed by /) to form the table_id to store the content in
    Data = #emxcontent{ displayname = Key, writetime = calendar:local_time(), writeuser = anon, content = Content },
    Res = emx_data:put_data(getTableId(Key), Data),
    {reply, {datainfo, Res}, N};
   
handle_call({getDataKeys, Prefix}, _From, N) ->
    {datainfo, Keys} = emx_data:get_datakeys(getTableId(Prefix)),
    {reply, {datainfo, Keys}, N};
    
handle_call({getData, Key}, _From, N) ->
    {datainfo, Res} = emx_data:get_data(getTableId(Key), Key),
    %%io:format("Res is ~p~n", [ Res ]),
    case Res of 
    	[] -> { reply, nodata, N};
	[ R | _] -> { reply, {datainfo, R}, N}
    end;

handle_call({housekeep}, _From, N) ->
	%% Get all tables, then run housekeep on each one
	Tables = emx_data:get_tables(),
	lists:foreach(fun(Table) ->
		%%io:format("Running for ~p~n", [ Table]),
		emx_data:run_capacity(Table#emxstoreconfig.typename)
		end, Tables),
	{ reply, ok, N }.
    
handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

getTableInfo(TableId, ConfigHandle) ->
	%%io:format("Config handle is ~p, Table id is ~p~n", [ ConfigHandle, TableId]),
	TableInfo = util_data:get_data(ConfigHandle, TableId),
	%%io:format("Table info is ~p~n", [ TableInfo]),
	case TableInfo of
		[] ->
			%% THIS IS WHERE WE NEED TO WORK OUT AN APPROPRIATE PLACE TO PUT THE TABLE
			%% WHICH MAY NOT BE THIS NODE
			[ DefaultTableInfo | _ ] = util_data:get_data(ConfigHandle, "default"),
			%%io:format("Default table info is ~p~n", [ DefaultTableInfo]),
			NewTableId = util_data:get_handle(DefaultTableInfo#emxstoreconfig.storagetype, TableId, DefaultTableInfo#emxstoreconfig.storageoptions),
			%%io:format("New Table Id is ~p~n", [ NewTableId]),
			NewTableInfo = DefaultTableInfo#emxstoreconfig { typename = TableId, tableid = NewTableId },
			%%io:format("Putting config data~n"),
			util_data:put_data(ConfigHandle, NewTableInfo),
			%%io:format("Returning~n"),
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
					
					
			