-module(emx_restserver).

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-include("../../yaws-1.77/include/yaws.hrl").

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1, start_link/1]).

-export([code_change/3, set_conf/1, terminate/2]).

start_link(Args) ->
    util_flogger:logMsg(self(), ?MODULE, debug,
			"~p starting with ~p", [?MODULE, Args]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args,
			  []).

getAppArgs(List) ->
     lists:map(fun(Param) ->
     			{ ok, Ret} = application:get_env(Param),
			Ret
			end, List).
init(_Args) ->
    [ Host, Port, LogDir] = getAppArgs([webhost, webport, weblogdir]),
    
    util_flogger:logMsg(self(), ?MODULE, debug,
			"Init emx_restserver"),
    process_flag(trap_exit, true),
    case application:start(yaws) of
      ok -> set_conf([Host, Port, LogDir]);
      Error -> {stop, Error}
    end.

set_conf([Host, Port, LogDir]) ->
    util_flogger:logMsg(self(), ?MODULE, debug,
			"Setting configuration for yaws"),
    GC = #gconf{trace = false,
		logdir = LogDir,
		yaws = "EMX 1.0",
		yaws_dir = "../../yaws-1.77"},
    SC = #sconf{port = Port, servername = atom_to_list(Host),
		listen = {0, 0, 0, 0}, docroot = "../www",
		appmods =
		    [{"/emx/put", rest_emxput},
		     {"/emx/get", rest_emxget},
		     {"/emx/getkeys", rest_emxgetkeys}]},
    case catch yaws_api:setconf(GC, [[SC]]) of
      ok -> {ok, started};
      Error -> {stop, Error}
    end.

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
