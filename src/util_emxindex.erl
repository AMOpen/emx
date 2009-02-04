%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(util_emxindex).

-author('Alan Moore <amkimian@mac.com>').

-export([create_index/1, get_index_records/3, get_index/1]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

create_index(Index) when is_record(Index	, emxindexinfo) ->
	%% 1. Create the underlying tables for the index
	%% 2. Update the record in the indexinfo table
	NewIndex = Index#emxindexinfo{ tablename = get_table_name(index, Index#emxindexinfo.indexname) },
	
	mnesia:create_table(NewIndex#emxindexinfo.tablename, [
			{ attributes, record_info(fields, icontent) },
			{ index, [] },
			{ disc_only_copies, [node() | nodes()] },
			{ record_name, icontent}
			]
			),

	mnesia:transaction(fun() -> mnesia:write(NewIndex) end).
		
get_table_name(index, IndexName) ->
	list_to_atom(util_string:format("emxi~s", [IndexName])).

get_index_records(Content, TypeInfo, DisplayNameParts) when is_record(Content, emxcontent), is_record(TypeInfo, emxtypeinfo) ->
	%% Given some content and associated (already worked out) type information, locate the index records
	%% that reference this type and apply the index to this content, returning an array of tuples of the
	%% form { indextablename, icontent }
	%% The caller will then save them in a transaction...
	IndexRecords = get_referenced_indices(TypeInfo),
	%% io:format("Index records is ~p~n", [ IndexRecords]),
	%% Now, for each of the above, create new index records to return
	lists:foldl(fun(IndexRecord, AccIn) ->
			AccIn ++ [ get_indexrecord(IndexRecord, Content, TypeInfo, DisplayNameParts) ] 
			end,
			[],
			IndexRecords).

generator({displayname, Count}, _IndexRecord, _Content, _TypeInfo, DisplayNameParts) ->
	lists:nth(Count, DisplayNameParts);
	
generator({xpath, Xpath}, IndexRecord, Content, TypeInfo, DisplayNameParts) ->
	util_xml:go(Content#emxcontent.content, Xpath).
	
get_indexrecord(IndexRecord, Content, TypeInfo, DisplayNameParts) ->
	IndexData = lists:foldl(fun({Key, Generator},AccIn) ->
		AccIn ++ [{ Key, generator(Generator, IndexRecord, Content, TypeInfo, DisplayNameParts) }]
		end,
		[],
		IndexRecord#emxindexinfo.fielddefinition),
	%% Need to fill in
	Record = #icontent {
			typename = TypeInfo#emxtypeinfo.typename,
			displayname = Content#emxcontent.displayname,
			version = Content#emxcontent.version,
			indexdata = IndexData
			},
	{ IndexRecord#emxindexinfo.tablename, Record }.
	
get_index(IndexName) ->
	Indices = util_mnesia:getData(emxindexinfo, #emxindexinfo{ indexname = IndexName, _ = '_' }),
	case Indices of 
		[] -> noindex;
		[ Index | _ ] -> Index
	end.
	
get_referenced_indices(TypeInfo) when is_record(TypeInfo, emxtypeinfo) ->
	AllRecords = util_mnesia:getAllData(emxindexinfo, #emxindexinfo{ _ = '_' }),
	%%io:format("All index Records is ~p~n", [ AllRecords]),
	lists:filter(fun(Record) -> 
		lists:any(fun({ TypeName, _}) ->
			%%io:format("Typename = ~p, Test against ~p~n", [ TypeName, TypeInfo#emxtypeinfo.typename]),
			TypeName == TypeInfo#emxtypeinfo.typename end, Record#emxindexinfo.typemappings) end,
		AllRecords).
			
