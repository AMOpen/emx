%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Wrapper around zipping emxcontent records (could also do util_bfile extraction if necessary)

-module(util_zip).

-export([compress_record/1, decompress_record/1, archive_record/1, cleanup_record/1]).

-include_lib("records.hrl").

compress_record(Data) ->
	case Data#emxcontent.content of
		{ compressed, Value } -> Data;
		{ archived } -> Data;
		Content -> Data#emxcontent { content = { compressed, zlib:compress(Content)} }
	end.

decompress_record(Data) ->
	case Data#emxcontent.content of
		{ compressed, Value } -> Data#emxcontent { content = zlib:uncompress(Value) };
		{ archived } -> Data#emxcontent { content = zlib:uncompress(util_bfile:load_content(Data#emxcontent.displayname)) };
		_ -> Data
	end.
	
archive_record(Data) ->	
	%% Make sure data is compressed
	case Data#emxcontent.content of
		{ compressed, Value } -> 
			util_flogger:logMsg(self(), ?MODULE, debug, "Writing file out"),
			util_bfile:save_content(Data#emxcontent.displayname, Value),
			Data#emxcontent { content = { archived }};
		{ archived } ->
			Data;
		Data -> archive_record(compress_record(Data))
	end.

cleanup_record(Data) ->
	case Data#emxcontent.content of
		{ archived } -> util_bfile:delete_content(Data#emxcontent.displayname);
		_ -> nothing
	end.
