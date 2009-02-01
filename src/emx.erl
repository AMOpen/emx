%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Application starter for emx

-module(emx).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

%% External exports
-export([start/0, stop/0]).

%% Simple module to start and stop the cloud

%% @spec start() -> ok
%% @doc Start the ecloud application

start() ->
    application:start(emx).

%% @spec stop() -> ok
%% @doc Stop the ecloud application

stop() -> application:stop(emx).
