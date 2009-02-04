%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(util_emxdata).

-author('Alan Moore <amkimian@mac.com>').

-export([put_data/1, get_data/2, perform_query/1]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

get_data(DisplayName, Version) ->
    [Protocol, TypeName | DisplayNameParts] = string:tokens(DisplayName, "/"),
    %% NB getByKey is blindingly fast compared to select
    HeaderRecord = util_mnesia:getByKey(emxheader, DisplayName),
    Content = low_get_data(DisplayName, Version, HeaderRecord),
    Content#emxcontent{content = get_content(Content#emxcontent.content, decompress)}.	
    
low_get_data(DisplayName, latest, HeaderRecord) ->
    low_get_data(DisplayName, HeaderRecord#emxheader.latestversion, HeaderRecord);

low_get_data(DisplayName, Version, HeaderRecord) ->
   %%io:format("Looking for ~p and ~p~n", [HeaderRecord#emxheader.id, Version]),
   ExistingRecords = util_mnesia:getAllByKey(emxcontent, DisplayName),
   io:format("Existing records ~p, looking for version ~p~n", [ ExistingRecords, Version]),
   Output = lists:filter(fun(Record) -> Record#emxcontent.version == Version end, ExistingRecords),
   case Output of 
   	[] -> nodata;
	[ Rec | _] -> Rec
   end.

put_data(Data) when is_record(Data, putcontent) ->
%% Get the type from the display name, and extract out the display name into its parts
%% The key in the record is the item after the type name, look to see if there is already
%% data for that key. If so, update the version and save the content, if not, this is version 1
        [Protocol, TypeName | DisplayNameParts] = string:tokens(Data#putcontent.displayname, "/"),
	%%io:format("Protocol is ~p, TypeName is ~p, DisplayNameParts is ~p~n", [ Protocol, TypeName, DisplayNameParts]),
	TypeInfo = util_emxtype:load_type_information(TypeName),
	%%io:format("Type information is ~p~n", [ TypeInfo]),
	%% Attempt to locate the record in the header table
	ExistingRecord = util_mnesia:getByKey(emxheader,Data#putcontent.displayname), 
	case ExistingRecord of
		nodata ->
				%% no record
				NewHeader = #emxheader{ 
						      latestversion = 1,
						      typename = TypeName,
						      displayname = Data#putcontent.displayname};
		_  ->
				NewHeader = ExistingRecord#emxheader { latestversion = ExistingRecord#emxheader.latestversion + 1 }
	end,
	%% Now create the content record
	
	UncompressedContent = #emxcontent { displayname = NewHeader#emxheader.displayname,
				 version = NewHeader#emxheader.latestversion,
				 writetime = Data#putcontent.writetime,
				 writeuser = Data#putcontent.writeuser,
				 content = Data#putcontent.content 
				},
				
	NewContent = UncompressedContent#emxcontent { content = get_content(Data#putcontent.content, compress) 
				},
				
	%% Now generate the index information for this content
	
	IndexRecords = util_emxindex:get_index_records(UncompressedContent, TypeInfo, DisplayNameParts),
	%%io:format("Index records is ~p~n", [ IndexRecords]),
	mnesia:transaction(fun() ->
			mnesia:write(NewHeader),
			mnesia:write(NewContent),
			lists:foreach(fun({Table, IndexRecord}) ->
				mnesia:write(Table, IndexRecord, write) end
				, IndexRecords)
			end),			
	ok.
	
perform_query(Query) ->
	%% Query is something like  { xand, [ { index, "system.config", "application", eq, "app" } ]}
	%% Each query part (the bit in the []) will return a list of { content_table, key } entries
	%% We then xand them together to generate a unique set of records that match the query
	get_query_data(Query).

get_query_data( { xand, Tests }) ->
	[ get_query_data(X) || X <- Tests ];
	
get_query_data( { index, IndexName, Test } ) ->
	%% Look in the index given for content that matches the given test, but only for the latest version
	Index = util_emxindex:get_index(IndexName),
	%% io:format("Index is ~p~n", [ Index ]),
	%% That gives us the table to qlc against to get the latest version
	%% Our query is initially
	%% theader.id == index.id && theader.latestversion == index.version
	%% Then filter on index.indexdata that matches the above criteria
	 Entries = util_mnesia:do(qlc:q([{X,Y}
				    || X  <- mnesia:table(Index#emxindexinfo.tablename,
							   [{lock, read},
							    {n_objects, 1000}]),
					Y <- mnesia:table(emxheader),
					X#icontent.displayname =:= Y#emxheader.displayname,
					X#icontent.version =:= Y#emxheader.latestversion,
					passesTest(X, Test)
					])).	
					
passesTest(IndexRecord, { Key, eq, Value }) ->
	proplists:get_value(Key, IndexRecord#icontent.indexdata) == Value.
	
get_content(RawString, compress) ->
	zlib:compress(RawString);
	
get_content(RawData, decompress) ->
	zlib:uncompress(RawData).
	
