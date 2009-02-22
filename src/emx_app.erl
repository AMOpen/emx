%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Application starter for emx

-module(emx_app).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-behaviour(application).

-export([start/2, start_phase/3, stop/1]).

-include("emx.hrl").

%% Main application entry point - start the OTP cloud supervisor.

start_phase(Phase, Mode, Args) ->
    ?LOG(debug,
			"Running start phase ~p, ~p, ~p", [Phase, Mode, Args]),
    start_inner_phase(Phase, Mode, Args).
    
start_inner_phase(jobs, _Mode, _Args) ->
  ok.
  
start(_Type, StartArgs) ->
    Ret = emx_sup:start_link(StartArgs),
    ?LOG(normal,
			"EMX Initial Start",[]),
    Ret.

stop(_State) -> ok.
