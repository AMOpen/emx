%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Displays information about all tables in this cache

-module(widget_tableList).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("records.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([show_widget/1]).

show_widget(Arg) ->
        %% Display a list of the current clouds
	Tables = emx_data:get_tables(),
	Rows = lists:map(fun(C) -> transformRow(C) end,Tables),
	{TotalRecords, TotalEpoch, TotalMemory } = lists:foldl(fun(Table, { CurrentRecords, CurrentEpoch, CurrentMemory }) ->
				{Records, Memory} = util_data:get_size(Table#emxstoreconfig.tableid),
				{CurrentRecords + Records, CurrentEpoch + Table#emxstoreconfig.epoch, CurrentMemory + Memory }
				end, { 0, 0, 0}, Tables),
	{ehtml,
		{ table,
		   [ { id, "tableList"}, {class, "tablesorter"}],
		      [
			{thead, [],
				[ {tr, [], [
					{th, [], "Name"},
					{th, [], "Storage"},
					{th, [], "Location"},
					{th, [], "Records"},
					{th, [], "Epoch"},
					{th, [], "Memory"},
					{th, [], "Max Records"},
					{th, [], "Archive Age"},
					{th, [], "Max Age"},	
					{th, [], "Max Size"},
					{th, [], "Action" }
					]
				   }
				 ]
			},
			{tbody, [], [ Rows]},
			{thead, [],
				[ {tr, [], [
					{th, [], ""},
					{th, [], ""},
					{th, [], ""},
					{th, [], util_string:format("~p", [TotalRecords])},
					{th, [], util_string:format("~p", [TotalEpoch])},
					{th, [], util_string:format("~p", [TotalMemory])},
					{th, [], ""},
					{th, [], ""},
					{th, [], ""},	
					{th, [], ""},
					{th, [], ""}
					]
				   }
				 ]
			}
		     ]		   
		}
	}.
	
transformRow(Table) ->
	{Records, Memory} = util_data:get_size(Table#emxstoreconfig.tableid),
	MaxRecords = proplists:get_value(records, Table#emxstoreconfig.capacityconstraints),
	MaxAge = proplists:get_value(age, Table#emxstoreconfig.capacityconstraints),
	MaxMemory = proplists:get_value(size, Table#emxstoreconfig.capacityconstraints),
	ArchiveAge = proplists:get_value(archive, Table#emxstoreconfig.capacityconstraints),
	{ tr, [], [ 
		{ td, [], util_string:format("~p", [Table#emxstoreconfig.typename]) },
		{ td, [], getStorage(Table#emxstoreconfig.storagetype) },
		{ td, [], util_string:format("~p", [Table#emxstoreconfig.location]) },
		{ td, [], util_string:format("~p", [Records]) },
		{ td, [], util_string:format("~p", [Table#emxstoreconfig.epoch]) },
		{ td, [], util_string:format("~p", [Memory]) },
		{ td, [], util_string:format("~p", [MaxRecords]) },
		{ td, [], util_string:format("~p", [ArchiveAge]) },
		{ td, [], util_string:format("~p", [MaxAge]) },
		{ td, [], util_string:format("~p", [MaxMemory]) },
		{ td, [
			{ class, "clickable" },
			{ rel, tablelink },
			{ id, util_string:format("~s", [Table#emxstoreconfig.typename])  }
		      ], "View" }
		]}.

getStorage(ets) ->
	"Memory";
getStorage(dets) ->
	"Disk".