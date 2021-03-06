%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Web widget to display data for a record

-module(widget_dataList).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("emx.hrl").

-export([show_widget/1]).

show_widget(Arg) ->
    %% Get the id parameter which will be the table list to show the keys for
    Args = yaws_api:parse_query(Arg),
    %%io:format("Args are ~p~n", [ Args]),
    KeyName = proplists:get_value("id", Args),
    case KeyName of
      "null" -> Val = "";
      _ ->
	  {datainfo, Content} = emx_admin:get_data(KeyName),
	  Val = Content#emxcontent.content
    end,
    {ehtml,
     {table,
      [{id, "tableList"}, {class, "tablesorter"},
       {width, "50%"}],
      [{thead, [], [{tr, [], [{th, [], KeyName}]}]},
       {tbody, [],
	[{tr, [],
	  [{td, [],
	    [{textarea, [{rows, "25"}, {cols, "80"}], Val}]}]}]}]}}.
