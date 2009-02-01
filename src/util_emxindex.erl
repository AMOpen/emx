%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(util_emxindex).

-author('Alan Moore <amkimian@mac.com>').

-export([create_index/1]).

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
	list_to_atom(util_string:format("emxi~p", [IndexName])).

