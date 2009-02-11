-module(widget_dataList).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("records.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([show_widget/1]).

show_widget(Arg) ->
        %% Display a list of the current clouds
	%% Get the id parameter which will be the table list to show the keys for
	Args = yaws_api:parse_query(Arg),
	%%io:format("Args are ~p~n", [ Args]),
	KeyName = proplists:get_value("id", Args),
	case KeyName of
		"null" -> Val = "";
		_ ->   {datainfo, Content}  = emx_admin:get_data(KeyName),
		       Val = Content#emxcontent.content
	end,
	{ehtml,
		{ table,
		   [ { id, "tableList"}, {class, "tablesorter"}, {width, "50%"}],
		      [
			{thead, [],
				[ {tr, [], [
					{th, [], KeyName}
					]
				   }
				 ]
			},
			{tbody, [], 
				[ {tr, [], [
					{td, [],
						[
						{ textarea, [
							{rows, "25"}, {cols, "80"}], Val}
						]
					}
					]
				   }
				 ]
			}
		     ]		   
		}
	}.
	
	
