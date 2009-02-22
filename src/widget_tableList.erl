%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Displays information about all tables in this cache

-module(widget_tableList).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("emx.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([show_widget/1]).

show_widget(Arg) ->
    Tables = emx_data:get_tables(),
    PreRows = [show_widget_1(V1) || V1 <- Tables],
    Rows = [V2 || V2 <- PreRows, show_widget_2(V2)],
    {TotalRecords, TotalEpoch, TotalMemory, InMemory,
     DiskMemory, InArchive} =
	lists:foldl(fun (Table,
			 {CurrentRecords, CurrentEpoch, CurrentMemory,
			  CurrentInMemory, CurrentDiskMemory,
			  CurrentInArchive}) ->
			    {Records, Memory, DiskMemory} =
				util_data:get_size(Table#emxstoreconfig.tableid),
			    {RecMemory, RecArchive} =
				util_data:foldl(fun collectStatus/2, {0, 0},
						Table#emxstoreconfig.tableid),
			    {CurrentRecords + Records,
			     CurrentEpoch + Table#emxstoreconfig.epoch,
			     CurrentMemory + Memory,
			     CurrentInMemory + RecMemory,
			     CurrentDiskMemory + DiskMemory,
			     CurrentInArchive + RecArchive}
		    end,
		    {0, 0, 0, 0, 0, 0}, Tables),
    {ehtml,
     {table, [{id, "tableList"}, {class, "tablesorter"}],
      [{thead, [],
	[{tr, [],
	  [{th, [], "Name"}, {th, [], "Storage"},
	   {th, [], "Location"}, {th, [], "Records"},
	   {th, [], "InMem"}, {th, [], "InArc"}, {th, [], "Epoch"},
	   {th, [], "Memory"}, {th, [], "Disk"},
	   {th, [], "Max Records"}, {th, [], "Archive Age"},
	   {th, [], "Max Age"}, {th, [], "Max Size"},
	   {th, [], "Action"}]}]},
       {tbody, [], [Rows]},
       {thead, [],
	[{tr, [],
	  [{th, [], ""}, {th, [], ""}, {th, [], ""},
	   {th, [], util_string:format("~p", [TotalRecords])},
	   {th, [], util_string:format("~p", [InMemory])},
	   {th, [], util_string:format("~p", [InArchive])},
	   {th, [], util_string:format("~p", [TotalEpoch])},
	   {th, [], util_string:format("~p", [TotalMemory])},
	   {th, [], util_string:format("~p", [DiskMemory])},
	   {th, [], ""}, {th, [], ""}, {th, [], ""}, {th, [], ""},
	   {th, [], ""}]}]}]}}.

show_widget_1(C) -> transformRow(C).

show_widget_2(R) -> is_tuple(R).

getColorClass(Value) when Value > 100 ->
    [{class, purple}];
getColorClass(Value) when Value > 50 -> [{class, blue}];
getColorClass(Value) when Value > 0 -> [{class, green}];
getColorClass(_Value) -> [].

getBackColorClass(Value) when Value > 0 -> [];
getBackColorClass(Value) -> [].

transformRow(Table) ->
    {Records, Memory, DiskMemory} =
	util_data:get_size(Table#emxstoreconfig.tableid),
    {RecMemory, RecArchive} =
	util_data:foldl(fun collectStatus/2, {0, 0},
			Table#emxstoreconfig.tableid),
    MaxRecords = proplists:get_value(records,
				     Table#emxstoreconfig.capacityconstraints),
    MaxAge = proplists:get_value(age,
				 Table#emxstoreconfig.capacityconstraints),
    MaxMemory = proplists:get_value(size,
				    Table#emxstoreconfig.capacityconstraints),
    ArchiveAge = proplists:get_value(archive,
				     Table#emxstoreconfig.capacityconstraints),
    {tr, [],
     [{td, getBackColorClass(Records),
       util_string:format("~p",
			  [Table#emxstoreconfig.typename])},
      {td, getBackColorClass(Records),
       getStorage(util_data:get_type(Table#emxstoreconfig.tableid))},
      {td, getBackColorClass(Records),
       util_string:format("~p",
			  [Table#emxstoreconfig.location])},
      {td, getColorClass(Records),
       util_string:format("~p", [Records])},
      {td, getColorClass(RecMemory),
       util_string:format("~p", [RecMemory])},
      {td, getColorClass(RecArchive),
       util_string:format("~p", [RecArchive])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [Table#emxstoreconfig.epoch])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [Memory])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [DiskMemory])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [MaxRecords])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [ArchiveAge])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [MaxAge])},
      {td, getBackColorClass(Records),
       util_string:format("~p", [MaxMemory])},
      {td,
       [{class, "clickable"}, {rel, tablelink},
	{id,
	 util_string:format("~s",
			    [Table#emxstoreconfig.typename])}],
       "View"}]}.

getStorage(ets) -> "Memory";
getStorage(dets) -> "Disk";
getStorage(system) -> "System";
getStorage(remote) -> "Remote".

collectStatus(Record, {InMem, InArc}) ->
    case Record#emxcontent.content of
      {archived} -> {InMem, InArc + 1};
      _ -> {InMem + 1, InArc}
    end.
