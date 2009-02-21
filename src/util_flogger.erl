%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore
%% @doc A logging utility class, formatting to a Flog standard
%%      files will be rotated depending their size, defined in the
%%      config.

-module(util_flogger).

-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-behaviour(gen_server).

-export([doLog/7, logMsg/4, logMsg/5, start_link/1]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-define(GD1, {local, ?MODULE}).

-define(GD2, ?MODULE).

-include_lib("kernel/include/file.hrl").

start_link(_Arg) ->
    gen_server:start_link(?GD1, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, FilePattern} = application:get_env(logdir),
    {ok, LogSize} = application:get_env(logsize),
    {ok, LogTags} = application:get_env(logtags),
    util_flogger:logMsg(self(), ?MODULE, debug,
			"File pattern is ~p", [FilePattern]),
    FileName = util_string:format(atom_to_list(FilePattern),
					   [node()]),
    util_flogger:logMsg(self(), ?MODULE, debug,
			"~p starting", [?MODULE]),
    {ok, {{FileName, FileName ++ ".log", LogSize, LogTags}, true}}.

terminate(_Reason, _N) ->
    util_flogger:logMsg(self(), ?MODULE, debug,
			"~p stopping", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

%% These get performed as a cast for performance reasons so clients don't have to
%% wait for a response.

logMsg(Pid, Module, Priority, Msg) ->
    gen_server:cast(?GD2,
		    {logmsg, Pid, Module, Priority, calendar:local_time(), Msg, []}).

logMsg(Pid, Module, Priority, Msg, Params) ->
    gen_server:cast(?GD2,
		    {logmsg, Pid, Module, Priority, calendar:local_time(), Msg, Params}).

handle_call(_Message, _From, N) -> {reply, ok, N}.

handle_cast({logmsg, Pid, Module, Priority, When, Msg,
	     Params},
	    {N, LogOn}) ->
    {message_queue_len, Len } = erlang:process_info(self(), message_queue_len),
    case {LogOn, Len > 100} of
    	{true, true} ->
		NewState = { N, false},
		doLog(self(), ?MODULE, debug, calendar:local_time(), "LOGGING SUSPENDED", [], N);
	{true, false} ->
		NewState = { N, LogOn},
		doLog(Pid, Module, Priority, When, Msg, Params, N);
	{false, false} ->
		NewState = { N, true},
		doLog(self(), ?MODULE, debug, calendar:local_time(), "LOGGING RESUMED", [], N),
		doLog(Pid, Module, Priority, When, Msg, Params, N);
	{false, true} ->
		NewState = { N, LogOn}
    end,
    {noreply, NewState}.

doLog(Pid, Module, Priority, When, Msg, Params, N) ->
    {FileRoot, LogFile, LogSize, LogTags} = N,
    case lists:any(fun (X) -> X == Priority end, LogTags) of
      true ->
	  {{Year, Month, Day}, {Hour, Minute, Second}} =
	      When,
	  case file:read_file_info(LogFile) of
	    {ok, FileInfo} ->
		if FileInfo#file_info.size > LogSize ->
		       NewFileName = FileRoot ++
				       util_string:format("~2..0B~2..0B~4..0B_~2..0B~2..0B~2..0B.log",
								   [Day, Month,
								    Year, Hour,
								    Minute,
								    Second]),
		       file:rename(LogFile, NewFileName);
		   true -> null
		end;
	    _ -> null
	  end,
	  case file:open(LogFile, [append]) of
	    {ok, IODevice} ->
		io:fwrite(IODevice,
			  "~n~2..0B ~2..0B ~4..0B, ~2..0B:~2..0B:~2..0B: "
			  "~-8s : ~-20s : ~12w : ",
			  [Day, Month, Year, Hour, Minute, Second, Priority,
			   Module, Pid]),
		io:fwrite(IODevice, Msg, Params),
		io:fwrite(IODevice, "~c", [13]),
		file:close(IODevice);
	    _ -> 1
	  end;
      false -> 1
    end.

handle_info(_Info, N) -> {noreply, N}.
