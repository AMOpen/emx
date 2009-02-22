%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_admin).

%% Top level access to put and get data into the cache

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([put_data/2, put_file/3, put_data/3, get_data/1, get_datakeys/2, get_datakeys/1, housekeep/0, get_prefixes/0]).

-include_lib("emx.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%-define(GD1,{global, ?MODULE}).
%%-define(GD2,{global, ?MODULE}).

-define(GD1, {local, ?MODULE}).

-define(GD2, ?MODULE).

%% Hold a config table id for passing around. The storage options for the 
%% config table are passed as startup parameters

%% GEN_SERVER FUNCTIONS

start_link(_Arg) ->
    gen_server:start_link(?GD1, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, []}.

terminate(_Reason, _ConfigHandle) ->
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

%% API FUNCTIONS

put_file(Key, FileName, FileType) ->
    {ok, Content} = file:read_file(FileName),
    put_data(Key, Content, FileType).
    
put_data(Key, Content) ->
    put_data(Key, Content, "application/xml").
    
put_data(Key, Content, Encoding) ->
    %% We convert any spaces in the key to _
    gen_server:call(?GD2, {putData, normaliseKey(Key), Content, Encoding}, infinity).
    
get_data(Key) ->
    gen_server:call(?GD2, {getData, normaliseKey(Key)}, 30000).
   
get_prefixes() ->
    gen_server:call(?GD2, {getPrefixes}, infinity).
    
get_datakeys(Prefix, EpochNumber) ->
    gen_server:call(?GD2, {getDataKeys, Prefix, EpochNumber}, infinity).
    
get_datakeys(Prefix) ->
    get_datakeys(Prefix, 0).

housekeep() ->
    gen_server:call(?GD2, {housekeep}, infinity).

%% LOCAL FUNCTIONS

%% First two parts of key construct the table name
%% e.g. official/fred/billy -> official_fred

getTableId(Key) ->
    [ Protocol, Type | _ ] = string:tokens(Key, "/"),
    list_to_atom(string:join([ Protocol, Type ], "_")).

normaliseKey(Key) ->
	string:join(string:tokens(Key, " "), "~").
	
denormaliseTable(Table) when is_atom(Table) ->
	One = string:join(string:tokens(atom_to_list(Table), "_"), "/"),
	string:join(string:tokens(One, "~"), " ");
denormaliseTable(Table) ->
	Table.
	
%% API HANDLING FUNCTIONS

%% TODO: Add writeUser to this call
handle_call({putData, Key, Content, Encoding}, _From, N) ->
    %% emx_admin is responsible for working out the table name given a key, emx_data is responsible for storing that data and optionally
    %% creating that table.
    util_check:check_message_queue(),
    Data = #emxcontent{ displayname = Key, writetime = calendar:local_time(), writeuser = anon, content = Content, encoding = Encoding },
    Res = emx_data:put_data(getTableId(Key), Data, local),
    {reply, {datainfo, Res}, N};

handle_call({getPrefixes}, _From, N) ->
    util_check:check_message_queue(),
    Prefixes = [ denormaliseTable(Record#emxstoreconfig.typename) || Record <- emx_data:get_tables() ],	
    {reply, Prefixes, N};
    
%% The epoch number implies that the caller has already seen all of the changes in the cache up to that point, and therefore
%% would like to see the changes since that point. An Epoch number of 0 means everything.

handle_call({getDataKeys, Prefix, EpochNumber}, _From, N) ->
    util_check:check_message_queue(),
    {datainfo, {MaxEpoch, Keys}} = emx_data:get_datakeys(getTableId(Prefix), EpochNumber),
    {reply, {datainfo, {MaxEpoch, Keys}}, N};
    
handle_call({getData, Key}, _From, N) ->
    util_check:check_message_queue(),
    {datainfo, Res} = emx_data:get_data(getTableId(Key), Key),
    case Res of 
    	[] -> { reply, nodata, N};
	[ R | _] -> { reply, {datainfo, R}, N}
    end;

handle_call({housekeep}, _From, N) ->
        util_check:check_message_queue(),
	%% Get all tables, then run housekeep on each one, also see if we should be taking a copy of this table and hosting
	%% it ourselves, or perhaps giving up a table to other nodes
	{message_queue_len, Len } = erlang:process_info(self(), message_queue_len),
	case Len > 100 of
		true ->
			?LOG(debug, "Too busy to run housekeeping",[]);
		false ->
			lists:foreach(fun(Table) ->
				?LOG(debug, "Housekeep for ~p", [ Table#emxstoreconfig.typename]),
					emx_data:run_capacity(Table#emxstoreconfig.typename),
					emx_data:run_balancer(Table#emxstoreconfig.typename)
					end, 
					emx_data:get_tables())
		end,
	{ reply, ok, N }.
    
handle_cast(Msg, N) ->
	?LOG(debug, "Unexpected cast message ~p", [ Msg ]),
	{noreply, N}.

handle_info(Info, N) ->
	?LOG(debug, "Unexpected info message ~p", [ Info ]),
	{noreply, N}.

			