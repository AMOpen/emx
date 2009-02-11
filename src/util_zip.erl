%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Wrapper around zipping emxcontent records (could also do util_bfile extraction if necessary)

-module(util_zip).

-export([compress_record/1, decompress_record/1]).

-include_lib("records.hrl").

compress_record(Data) ->
	Data#emxcontent { content = { compressed, zlib:compress(Data#emxcontent.content)} }.

decompress_record(Data) ->
	case Data#emxcontent.content of
		{ compressed, Value } -> Data#emxcontent { content = zlib:uncompress(Value) };
		_ -> Data
	end.
