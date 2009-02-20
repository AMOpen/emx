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
    util_flogger:logMsg(self(), ?MODULE, debug, "starting"),
    timer:apply_after(1000, job_housekeep, checkStartup, []),
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
    spawn_link(?MODULE, housekeep, []),
    {next_state, running, State, 120000}.

running(timeout, State) ->
    util_flogger:logMsg(self(), ?MODULE, debug, "Housekeeping timeout"),
    {next_state, stopped, State, 30000};
    
running(finished, State) ->
    {next_state, stopped, State, 30000}.

housekeep() ->
    emx_admin:housekeep(),
    util_flogger:logMsg(self(), ?MODULE, debug, "Signal finished state"),
    gen_fsm:send_event(?GD2, finished).

handle_info(Info, State, N) -> 
    util_flogger:logMsg(self(), ?MODULE, debug, "Received info message ~p", [ Info ]),
    {next_state, stopped, State, 30000}.

handle_event(Event, _StateName, _StateData) ->
    util_flogger:logMsg(self(), ?MODULE, debug, "Received even ~p", [ Event ]),
    {paused, start, []}.

handle_sync_event(_Event, _StateName, _Stuff,
		  _StateData) ->
    {paused, start, []}.

terminate(_Reason, _StateName, _N) ->
    io:format("~p stopping ~n", [?MODULE]), ok.

code_change(_OldVsn, State, N, _Extra) ->
    {nextstate, State, N}.
