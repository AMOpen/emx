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
ok.

