%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Displays keys in a table

-module(widget_keyList).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("emx.hrl").

-export([show_widget/1]).

show_widget(Arg) ->
	%% Get the id parameter which will be the table list to show the keys for
	Args = yaws_api:parse_query(Arg),
	%%io:format("Args are ~p~n", [ Args]),
	TableName = list_to_atom(proplists:get_value("id", Args)),
	%% Now get the keys for this table and show them
	{datainfo, {_MaxEpoch, DataKeys}}  = emx_data:get_datakeys(TableName, 0),
	%%io:format("Data Keys for ~p is ~p~n", [ TableName, DataKeys]),
	Rows = lists:map(fun(C) -> transformRow(C) end,DataKeys), 
	{ehtml,
		{ table,
		   [ { id, "tableList"}, {class, "tablesorter"}, {width, "50%"}],
		      [
			{thead, [],
				[ {tr, [], [
					{th, [], "Key"},
					{th, [], "WriteTime"},
					{th, [], "WriteUser"},
					{th, [], "Epoch"},
					{th, [], "Action"}
					]
				   }
				 ]
			},
			{tbody, [], [ Rows]}
		     ]		   
		}
	}.
	
transformRow(Record) ->
	{ tr, [], [ 
		{ td, [], Record#emxcontent.displayname },
		{ td, [], httpd_util:rfc1123_date(Record#emxcontent.writetime)},
		{ td, [], util_string:format("~p", [ Record#emxcontent.writeuser])},
		{ td, [], util_string:format("~p", [ Record#emxcontent.epoch])},
		{ td, [
			{ class, "clickable" },
			{ rel, keylink },
			{ id, util_string:format("~s", [Record#emxcontent.displayname])  }
		      ], "View" }
		]}.

