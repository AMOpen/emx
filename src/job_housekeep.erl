%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-9 Alan Moore

-module(job_housekeep).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_fsm).

-export([checkStartup/0, start_link/1]).

-export([code_change/4, handle_event/3, handle_info/3,
	 handle_sync_event/4, terminate/3]).

-export([init/1, paused/2, running/2, stopped/2, housekeep/0]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

%%-define(GD1,{global, ?MODULE}).
%%-define(GD2,{global, ?MODULE}).

-define(GD1, {local, ?MODULE}).

-define(GD2, ?MODULE).

start_link(_Arg) ->
    gen_fsm:start_link(?GD1, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    io:format("~p starting ~n", [?MODULE]),
    {ok, paused, {[], null}}.

checkStartup() ->
    %% Send a ping event, state should be paused
    gen_fsm:send_event(?GD2, ping).

paused(start, {[], Dummy}) ->
    {next_state, stopped, {[], Dummy}, 20000};
paused(timeout, State) ->
    {next_state, stopped, State, 30000};
    
paused(ping, State) ->
    {next_state, stopped, State, 30000}.

stopped(timeout, State) ->
    %% Spawn a thread to do the deliver run
    spawn(?MODULE, housekeep, []),
    {next_state, running, State}.

running(finished, State) ->
    {next_state, stopped, State, 30000}.

housekeep() ->
    emx_admin:housekeep(),
    gen_fsm:send_event(?GD2, finished).

handle_info(_Info, State, N) -> {nextstate, State, N}.

handle_event(_Event, _StateName, _StateData) ->
    {paused, start, []}.

handle_sync_event(_Event, _StateName, _Stuff,
		  _StateData) ->
    {paused, start, []}.

terminate(_Reason, _StateName, _N) ->
    io:format("~p stopping ~n", [?MODULE]), ok.

code_change(_OldVsn, State, N, _Extra) ->
    {nextstate, State, N}.
