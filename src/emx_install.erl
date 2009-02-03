%% @author Alan Moore amkimian@mac.com
%% @copyright 2008 Alan Moore
-module(emx_install).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-export([
	 clean/0, 
	 setup_mnesia/0]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

clean() ->
        mnesia:stop(),
        mnesia:create_schema([node()]),
        mnesia:start(),
	setup_mnesia().
	
setup_mnesia() ->
    lists:foreach(fun ({RecordName, Fields}) ->
			  util_flogger:logMsg(self(), ?MODULE, debug,
					      "CreateTable ~p",
					      [mnesia:create_table(RecordName,
								   [{attributes,
								     Fields},
								    {disc_only_copies,
								     [node()]}])])
		  end,
		  [{emxtypeinfo, record_info(fields, emxtypeinfo)},
		   {emxindexinfo, record_info(fields, emxindexinfo)},
		   {emxheader, record_info(fields, emxheader)},
		   {emxcontent, record_info(fields, emxcontent)}]).

