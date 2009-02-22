%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Application starter for emx

-module(emx).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

%% External exports
-export([start/0, stop/0]).

%% Simple module to start and stop the emx cache

%% @doc Start the ecloud application
%% @spec start() -> ok

start() -> application:start(emx).

%% @doc Stop the ecloud application
%% @spec stop() -> ok

stop() -> application:stop(emx).
