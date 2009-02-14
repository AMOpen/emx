%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data_balancer).

-author('Alan Moore <amkimian@mac.com>').

-export([processBalanceNode/4]).

-include_lib("records.hrl").

processBalanceNode(ConfigHandle, TableInfo, false, Count) when Count == 0 ->
	util_data:delete_data(ConfigHandle, TableInfo#emxstoreconfig.typename);
	
processBalanceNode(ConfigHandle, TableInfo, false, Count) when Count < 2 ->
	NewTableId = util_data:get_handle(TableInfo#emxstoreconfig.storagetype, TableInfo#emxstoreconfig.typename, TableInfo#emxstoreconfig.storageoptions),
	NodeList = TableInfo#emxstoreconfig.location ++ [ node() ],
	UpdatedTableInfo = TableInfo#emxstoreconfig { location = NodeList, tableid = NewTableId },
	%% Now we also need to copy over data from the other node
	[TestNode | _ ] = TableInfo#emxstoreconfig.location,
	
	util_flogger:logMsg(self(), ?MODULE, debug, "Asking for getkeys from node ~p", [ TestNode ]),
	DataList = rpc:call(TestNode, emx_data, get_datakeys, [ TableInfo#emxstoreconfig.typename, 0]),
	case DataList of
		{badrpc, _ } -> 
			util_flogger:logMsg(self(), ?MODULE, debug, "Could not obtain information from ~p", [TestNode]);
		{datainfo, { MaxEpoch, [] }} ->
				%% Don't copy anything or inform of anything - this is an empty table!
				donothing;
		{datainfo, { MaxEpoch, List}} ->
			NewEpoch = lists:foreach(fun(Record) -> 
				util_flogger:logMsg(self(), ?MODULE, debug, "Copying ~p with ~p records", [ Record#emxcontent.displayname, length(List) ]),
				util_data:put_data(NewTableId, Record)
				end, List),
			%% Run the constraints so we don't keep data that is out of date
			emx_data_constraints:run_constraints(UpdatedTableInfo, UpdatedTableInfo#emxstoreconfig.capacityconstraints),
			util_flogger:logMsg(self(), ?MODULE, debug, "New epoch is ~p", [ MaxEpoch ]),
			util_data:put_data(ConfigHandle, UpdatedTableInfo#emxstoreconfig{ epoch = MaxEpoch }),
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
			Nodes)
	end;
	
processBalanceNode(ConfigHandle, TableInfo, true, Count) when Count > 2 ->
	util_flogger:logMsg(self(), ?MODULE, debug, "Should remove balance ~p", [ TableInfo]);
processBalanceNode(ConfigHandle, TableInfo, true, _) ->
	%% Check number of records. If there are none, remove us from the table
	{ Size, Memory } = util_data:get_size(TableInfo#emxstoreconfig.tableid),
	case Size of
		0 -> 
		     util_flogger:logMsg(self(), ?MODULE, debug, "No data in table ~p, removing from my interest", [ TableInfo#emxstoreconfig.typename]),
		     %% Something different here
		     MyNode = node(),
		     {ok, Nodes} = application:get_env(nodes),
		     lists:foreach(fun(Node) ->
		     	case Node of
				MyNode -> NewTableInfo = TableInfo#emxstoreconfig { location = lists:filter(fun(N) -> N /= MyNode end, TableInfo#emxstoreconfig.location) },
					  util_data:put_data(ConfigHandle, NewTableInfo);
				_ ->  rpc:call(Node, emx_data, update_table_info, [ TableInfo#emxstoreconfig.typename, nodedown, MyNode ])
			end
		     end, Nodes);
		_ -> util_flogger:logMsg(self(), ?MODULE, debug, "Do nothing with ~p", [ TableInfo])
	end;
processBalanceNode(ConfigHandle, TableInfo, _, _) ->
	donothing.
