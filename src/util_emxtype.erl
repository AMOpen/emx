%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(util_emxtype).

-author('Alan Moore <amkimian@mac.com>').

-export([create_type/1, load_type_information/1]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

create_type(Type) when is_record(Type, emxtypeinfo) ->
	%% 1. Create the underlying tables for the content
	%% 2. Update the record in the typeinfo table
	%% 3. Create the default index for this type as well (using a util_emxindex:create_index call as we are already in the emx_admin context)
	io:format("In create type~n"),

	mnesia:transaction(fun() -> mnesia:write(Type) end),
	DefaultIndexRecord = #emxindexinfo{ indexname = list_to_atom("def" ++ Type#emxtypeinfo.typename),
					    description = "Default Index for Type " ++ Type#emxtypeinfo.typename,
					    fielddefinition = get_standard_fielddef(Type),
					    typemappings = [ { Type#emxtypeinfo.typename, "/"}]},
	util_emxindex:create_index(DefaultIndexRecord),
	ok.

load_type_information(TypeName) ->
	Ret = util_mnesia:getData(emxtypeinfo, #emxtypeinfo{typename = TypeName, _ = '_'}),
	case Ret of 
		[ R | _] -> R;
		[] -> nodata
	end.

get_standard_fielddef(#emxtypeinfo{ displayinfo = DisplayInfo}) ->
	{Ret, _} = lists:foldl(fun(Entry, {Output, Count}) ->
		{Output ++ [ {Entry, { displayname, Count}}], Count+1} end,
		{[], 1}, DisplayInfo),
	Ret.
		
	
