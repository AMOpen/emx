%% @author Alan Moore amkimian@mac.com
%% @copyright 2008 Alan Moore

-module(emx_sup).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-behaviour(supervisor).         % see erl -man supervisor

-export([init/1, start/0, start_in_shell_for_testing/0,
	 start_link/1]).

-include_lib("records.hrl").

-include_lib("stdlib/include/qlc.hrl").

start() ->
    spawn(fun () ->
		  supervisor:start_link({local, ?MODULE}, ?MODULE,
					_Arg = [])
	  end).

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE},
				      ?MODULE, _Arg = []),
    unlink(Pid).

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    mnesia:start(),
    util_flogger:logMsg(self(), ?MODULE, debug,
			"EMX starting, waiting for mnesia"),
    Args = [],
  %%  mnesia:wait_for_tables([cloudconfig], 600000),
  %%  Clouds = util_cloud:getCloudTables(),
    %%
  %%  util_flogger:logMsg(self(), ?MODULE, debug,
  %%			"Waiting for ~p", [Clouds]),
  %%  mnesia:wait_for_tables(Clouds, 600000),
    util_flogger:logMsg(self(), ?MODULE, debug,
			"Mnesia ready"),
    ModuleArray = lists:map(fun(Module) -> {
    			Module, 
			{Module, start_link, [Args]},
			permanent, 10000, worker, [Module] } end,
			[ 
			  util_flogger,
			  emx_admin,
			  emx_restserver]),
			
    {ok, {{one_for_one, 3, 1}, ModuleArray}}.

init_1(Var) ->
    {ok, Value} = application:get_env(Var), Value.
