%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(util_emxtype).

-author('Alan Moore <amkimian@mac.com>').

-export([create_type/1]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

create_type(Type) when is_record(Type, emxtypeinfo) ->
	%% 1. Create the underlying tables for the content
	%% 2. Update the record in the typeinfo table
	%% 3. Create the default index for this type as well (using a util_emxindex:create_index call as we are already in the emx_admin context)
	mnesia:create_table(get_table_name(content, Type#emxtypeinfo.tableprefix), [
			{ attributes, record_info(fields, tcontent) },
			{ index, [] },
			{ Type#emxtypeinfo.tabletype, [node() | nodes()] },
			{ record_name, tcontent}
			]
			),
	mnesia:create_table(get_table_name(header, Type#emxtypeinfo.tableprefix), [
			{ attributes, record_info(fields, theader) },
			{ index, [] },
			{ Type#emxtypeinfo.tabletype, [node() | nodes()] },
			{ record_name, theader}
			]
			),

	mnesia:transaction(fun() -> mnesia:write(Type) end),
	DefaultIndexRecord = #emxindexinfo{ indexname = list_to_atom("def" + Type#emxtypeinfo.typename),
					    description = "Default Index for Type " ++ Type#emxtypeinfo.typename,
					    fielddefinition = get_standard_fielddef(Type),
					    typemappings = [ { Type#emxtypeinfo.typename, "/"}]},
	util_emxindex:create_index(DefaultIndexRecord),
	ok.

get_standard_fielddef(#emxtypeinfo{ displayinfo = DisplayInfo}) ->
	lists:foldl(fun(Entry, {Output, Count}) ->
		{Output ++ [ {Entry, Count}], Count+1} end,
		{[], 0}, DisplayInfo).
		
get_table_name(content, Prefix) ->
	list_to_atom(util_string:format("emxc~p", [Prefix]));
get_table_name(header, Prefix) ->
	list_to_atom(util_string:format("emxh~p", [Prefix])).
	
