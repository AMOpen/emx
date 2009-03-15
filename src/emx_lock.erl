%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_lock).

%% Locking across nodes

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, start_link/1, terminate/2]).

-export([get_lock/2, release_lock/2, run_coordinated/5]).

-include_lib("emx.hrl").

%%-define(GD1,{global, ?MODULE}).
%%-define(GD2,{global, ?MODULE}).

-define(GD1, {local, ?MODULE}).

-define(GD2, ?MODULE).

%% Hold a config table id for passing around. The storage options for the
%% config table are passed as startup parameters

%% GEN_SERVER FUNCTIONS

start_link(_Arg) ->
    gen_server:start_link(?GD1, ?MODULE, [], []).

init(_) -> 
	process_flag(trap_exit, true),
	ConfigHandle = util_data:get_handle(ets, emxlock, [ { keypos, 2}]),
	{ok, ConfigHandle}.

terminate(_Reason, _ConfigHandle) -> ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

%% API FUNCTIONS

get_lock(Node, Purpose) ->
    gen_server:call(?GD2, { getLock, Node, Purpose }, infinity).

release_lock(Node, Purpose) ->
    gen_server:call(?GD2, { releaseLock, Node, Purpose }, infinity).

%% API HANDLING FUNCTIONS

%% TODO: Add writeUser to this call
handle_call({getLock, Node, Purpose}, _From,
	    ConfigHandle) ->
     %% Purpose is the unique key
     Resp = util_data:get_data(ConfigHandle, Purpose),
     case Resp of
     	[] ->
		NewLock = #emxlock{ node = Node, purpose = Purpose },
		erlang:monitor_node(Node, true),
		util_data:put_data(ConfigHandle, NewLock),
		%%?LOG(debug, "Got lock ~p ~p", [ Node, Purpose ]),
		{ reply, ok, ConfigHandle};
	[Val | _ ] ->
		case Val#emxlock.node of
			Node ->
				{reply, ok, ConfigHandle };
			_ ->
				{reply, no, ConfigHandle }
		end;
	_ ->
		{reply, no, ConfigHandle}
     end;
     
handle_call({releaseLock, Node, Purpose}, _From, ConfigHandle) -> 
    %% ?LOG(debug, "Release lock ~p ~p", [ Node, Purpose ]),
    util_data:delete_data(ConfigHandle, Purpose),
    erlang:monitor_node(Node, false),
    {reply, ok, ConfigHandle}.

handle_cast(Msg, N) ->
    ?LOG(debug, "Unexpected cast message ~p", [Msg]),
    {noreply, N}.

handle_info({nodedown, Node}, ConfigHandle) ->
    ?LOG(debug, "Node ~p is down, cleaning", [Node]),
    Records = util_data:foldl(fun(Record, AccIn) -> 
    	case Record#emxlock.node of
		Node -> AccIn ++ [Record];
		_ -> nothing
	end
	end, [], ConfigHandle),
    lists:foreach(fun(Record) ->
    	util_data:delete_data(ConfigHandle, Record)
	end, Records),
    ?LOG(debug, "Node ~p is down - done", [Node]),
    {noreply, ConfigHandle};
    
handle_info(Info, N) ->
    ?LOG(debug, "Unexpected info message ~p", [Info]),
    {noreply, N}.

run_coordinated(Module, Function, Args, Lock, Timeout) when Timeout < 0 ->
    {timeout, 0};
    
run_coordinated(Module, Function, Args, Lock, Timeout) ->
    %% Obtain a lock to run, if we cannot get the lock, wait a little bit and retry
    %% Once we have the lock (or we have a timeout), run the function and return {ok, Res} or {timeout, Res}
    %%{ok, Coordinator } = application:get_env(coordinator),
    Coordinator = 'emx1@localhost',
    MyNode = node(),
    case Coordinator of
    	MyNode -> LockRes = emx_lock:get_lock(node(), Lock);
	_ -> LockRes = 
			rpc:call(Coordinator, emx_lock, get_lock, [node(), Lock])
    end,
    case LockRes of
    	ok -> 
		Resp = apply(Module, Function, Args),
		rpc:call(Coordinator, emx_lock, release_lock, [node(), Lock]),
		{ ok, Resp };
	no ->
		%% We did not get the lock, wait and try again until we hit a timeout
		?LOG(debug, "Lock fail, sleeping", []),
		timer:sleep(100),
		run_coordinated(Module, Function, Args, Lock, Timeout - 100)
    end.
    