%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_admin).

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%-define(GD1,{global, ?MODULE}).
%%-define(GD2,{global, ?MODULE}).

-define(GD1, {local, ?MODULE}).

-define(GD2, ?MODULE).

start_link(_Arg) ->
    gen_server:start_link(?GD1, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, []}.

terminate(_Reason, _N) ->
    io:format("~p stopping ~n", [?MODULE]), ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

create_type(Type) when is_record(Type, emxtypeinfo) ->
    gen_server:call(?GD2, {createType, Type}, infinity).

%% Main gen_server activity here...

handle_call({createType, Type}, _From, N) ->
    %% Pass on to a util function
    {reply, {typeinfo, util_emxtype:create_type(Type)}, N}.

handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.
