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
    %% Call checkStartup after 20 seconds, our default state is paused
    timer:apply_after(20000, ?MODULE, checkStartup, []),
    {ok, paused, {[], null}}.

checkStartup() ->
    %% Send a ping event, state should be paused
    gen_fsm:send_event(?GD2, ping).

paused(start, {[], Dummy}) ->
    %%	io:format("~p in state paused and starting~n", [?MODULE]),
    {next_state, stopped, {[], Dummy}, 20000};
paused(timeout, State) ->
    %%	io:format("~p in state paused, and checking timeout~n", [?MODULE]),
    %% after a timeout we check our config again which returns the next state
    {next_state, stopped, State, 30000};
    
paused(ping, State) ->
    %%	io:format("~p in state paused, and checking ping~n", [?MODULE]),
    {next_state, stopped, State, 30000}.

stopped(timeout, State) ->
    %% Spawn a thread to do the deliver run
    %%	io:format("~p in state stopped and timeout~n", [?MODULE]),
    %% Really need to check the state, as we could already be running?
    spawn(?MODULE, housekeep, []),
    {next_state, running, State}.

running(finished, State) ->
    %%	io:format("~p in state running and finished~n", [?MODULE]),
    %% here we should clear the state
    {next_state, stopped, State, 30000}.

housekeep() ->
    io:format("Housekeeping~n"),
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
