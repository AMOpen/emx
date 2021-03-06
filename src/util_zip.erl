%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Wrapper around zipping emxcontent records (could also do util_bfile extraction if necessary)

-module(util_zip).

-export([archive_record/1, cleanup_record/1,
	 compress_record/1, decompress_record/1]).

-include_lib("emx.hrl").

compress_record(Data)
    when is_record(Data, emxcontent) ->
    case Data#emxcontent.content of
      {compressed, Value} -> Data;
      {archived} -> Data;
      Content ->
	  Data#emxcontent{content =
			      {compressed, zlib:compress(Content)}}
    end.

decompress_record(Data)
    when is_record(Data, emxcontent) ->
    case Data#emxcontent.content of
      {compressed, Value} ->
	  Data#emxcontent{content = zlib:uncompress(Value)};
      {archived} ->
	  Content =
	      util_bfile:load_content(Data#emxcontent.displayname),
	  case Content of
	    baddata -> Data#emxcontent{content = undefined};
	    _ -> Data#emxcontent{content = zlib:uncompress(Content)}
	  end;
      _ -> Data
    end.

archive_record(Data) when is_record(Data, emxcontent) ->
    %% Make sure data is compressed
    case Data#emxcontent.content of
      {compressed, Value} ->
	  ?LOG(debug, "Writing file out", []),
	  util_bfile:save_content(Data#emxcontent.displayname,
				  Value),
	  Data#emxcontent{content = {archived}};
      {archived} -> Data;
      _ -> archive_record(compress_record(Data))
    end.

cleanup_record(Data) when is_record(Data, emxcontent) ->
    case Data#emxcontent.content of
      {archived} ->
	  util_bfile:delete_content(Data#emxcontent.displayname);
      _ -> nothing
    end.
