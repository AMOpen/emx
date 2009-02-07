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
	{ehtml,
		{ table,
		   [ { id, "tableList"}, {class, "tablesorter"}],
		      [
			{thead, [],
				[ {tr, [], [
					{th, [], "Name"},
					{th, [], "Storage"},
					{th, [], "Records"},
					{th, [], "Memory"},
					{th, [], "Max Records"},
					{th, [], "Max Age"},	
					{th, [], "Max Size"},
					{th, [], "Action" }
					]
				   }
				 ]
			},
			{tbody, [], [ Rows]}
		     ]		   
		}
	}.
	
transformRow(Table) ->
	{Records, Memory} = util_data:get_size(Table#emxstoreconfig.tableid),
	MaxRecords = proplists:get_value(records, Table#emxstoreconfig.capacityconstraints),
	MaxAge = proplists:get_value(age, Table#emxstoreconfig.capacityconstraints),
	MaxMemory = proplists:get_value(size, Table#emxstoreconfig.capacityconstraints),
	{ tr, [], [ 
		{ td, [], util_string:format("~p", [Table#emxstoreconfig.typename]) },
		{ td, [], util_string:format("~p", [Table#emxstoreconfig.storagetype]) },
		{ td, [], util_string:format("~p", [Records]) },
		{ td, [], util_string:format("~p", [Memory]) },
		{ td, [], util_string:format("~p", [MaxRecords]) },
		{ td, [], util_string:format("~p", [MaxAge]) },
		{ td, [], util_string:format("~p", [MaxMemory]) },
		{ td, [], [
				{ a, [
					{ rel, tablelink },
					{ href, "dummy" }
				     ], "View" 
				}
		          ] }
		]}.

