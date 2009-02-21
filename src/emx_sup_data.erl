%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-9 Alan Moore

-module(emx_sup_data).

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
    Args = [],
    ModuleArray = lists:map(fun(Module) -> {
    			Module, 
			{Module, start_link, [Args]},
			permanent, 10000, worker, [Module] } end,
			[ 
			  job_housekeep
			]),
    {ok, {{one_for_all, 3, 1}, ModuleArray}}.

