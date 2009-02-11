%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Application starter for emx

-module(emx_app).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-behaviour(application).

-export([start/2, start_phase/3, stop/1]).

%% Main application entry point - start the OTP cloud supervisor.

start_phase(Phase, Mode, Args) ->
    util_flogger:logMsg(self(), ?MODULE, debug,
			"Running start phase ~p, ~p, ~p", [Phase, Mode, Args]),
    start_inner_phase(Phase, Mode, Args).
    
start_inner_phase(jobs, _Mode, _Args) ->
  job_housekeep:checkStartup(),
  ok.
  
start(_Type, StartArgs) ->
    Ret = emx_sup:start_link(StartArgs),
    util_flogger:logMsg(self(), ?MODULE, normal,
			"EMX Initial Start"),
    Ret.

stop(_State) -> ok.
