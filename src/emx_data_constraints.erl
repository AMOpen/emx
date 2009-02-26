%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data_constraints).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-export([run_constraints/2, run_constraint/3]).

-include_lib("emx.hrl").

current_time() ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time()).
	
should_bail(StartTime) ->
	%% Current time
	%%util_flogger:logMsg(self(), ?MODULE, debug, "Should bail check, StartTime ~p, CurrentTime ~p", [ StartTime, current_time()]),
	(StartTime + 2 - current_time()) < 0.
	
should_not_bail(StartTime) ->
	%% Current time
	%%util_flogger:logMsg(self(), ?MODULE, debug, "Should not bail check, StartTime ~p, CurrentTime ~p", [ StartTime, current_time()]),
	(StartTime + 2 - current_time()) > 0.		
	
%% The following functions are to handle the removal of data from a cache to keep it within certain constraints
%% Constraints are by memory use, number of records or age of documents.

run_constraints(TableConfig, Constraints) ->
	StartTime = current_time(),
	run_constraints2(TableConfig, Constraints, StartTime).
	
run_constraints2(_TableConfig, [], StartTime) ->
	ok;
run_constraints2(TableConfig, [ H | T], StartTime) ->
	%% We need to bail out if this is taking too long
	case should_bail(StartTime) of
		true ->
			util_flogger:logMsg(self(),?MODULE,debug, "bail out housekeep"),
			ok;
		false ->
			run_constraint(TableConfig, H, StartTime)
	end,
	run_constraints2(TableConfig, T, StartTime).

run_constraint(_TableConfig, { records, infinity }, _StartTime) ->
	nothingtodo;
	
run_constraint(TableConfig, { records, MaxCount }, StartTime) ->
	%% The table should have no more than MaxCount records. If there are more than that,
	%% remove them in age order until the number of records is below MaxCount
	{ RecordCount, _, _} = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	case RecordCount > MaxCount of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p to keep number of records in limit", [ Record#emxcontent.displayname]),
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				util_zip:cleanup_record(Record),
				{ NewRecordCount, _ , _} = util_data:get_size(TableConfig#emxstoreconfig.tableid),
				NewRecordCount > MaxCount andalso should_not_bail(StartTime) 
				end, SortedList),
			%% Run garbage collection after removing the data to ensure that future tests on memory use the correct figures
			erlang:garbage_collect();				
		false ->
			nothingtodo
	end;

run_constraint(_TableConfig, { age, infinity}, StartTime) ->
	nothingtodo;
	
run_constraint(TableConfig, { age, MaxAge }, StartTime) ->
	%% Remove all records older than MaxAge
	TestTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - MaxAge,	
	util_data:foldl(fun(Record, AccIn) ->
		WriteTime = calendar:datetime_to_gregorian_seconds(Record#emxcontent.writetime),
		case WriteTime < TestTime of
			true ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p as it is old", [ Record#emxcontent.displayname]),
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				util_zip:cleanup_record(Record);
			false ->
				ok
		end,
		erlang:garbage_collect(),
		AccIn end, [], TableConfig#emxstoreconfig.tableid);

run_constraint(_TableConfig, { size, infinity }, StartTime) ->
	nothingtodo;
		
run_constraint(TableConfig, { size, MaxSize }, StartTime) ->
	%% The table should be smaller than MaxSize. If it is greater, remove records
	%% in age order until the size is reduced below MaxSize
	{ _, Memory, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	%%io:format("Memory use is ~p~n", [ Memory]),
	case Memory > MaxSize of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p to free up memory", [ Record#emxcontent.displayname]),
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				util_zip:cleanup_record(Record),
				%% Run garbage collect after deletion or the get_size method below will not return the correct
				%% and up to date value. It makes the whole loop slower though
				erlang:garbage_collect(),
				{ _, NewMemory, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
				NewMemory > MaxSize andalso should_not_bail(StartTime)
				end, SortedList);
		false ->
			nothingtodo
	end;
	
run_constraint(TableConfig, { archive, ArchiveAge}, StartTime) ->
	%% Any records older than ArchiveAge that are not already extracted to file should be extracted to file
	TestTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - ArchiveAge,	
	util_data:foldl(fun(Record, AccIn) ->
		WriteTime = calendar:datetime_to_gregorian_seconds(Record#emxcontent.writetime),
		IsArchived = getArchiveStatus(Record#emxcontent.content),
		case {WriteTime < TestTime, IsArchived, should_not_bail(StartTime)} of
			{true, false, true} ->
				NewRecord = util_zip:archive_record(Record),
				case NewRecord of 
					Record -> nothing;
					_ ->
						util_flogger:logMsg(self(), ?MODULE, debug, "Archived ~p as it is old", [ Record#emxcontent.displayname]),
						util_data:put_data(TableConfig#emxstoreconfig.tableid, NewRecord)
				end;
			_ ->
				ok
		end,
		erlang:garbage_collect(),
		AccIn end, [], TableConfig#emxstoreconfig.tableid).

getArchiveStatus({archived, _FileName}) ->
	true;
getArchiveStatus({compressed, _Data}) ->
	false;
getArchiveStatus(_Data) ->
	false.
	
%% Get data from a table and sort it by age

get_all_sorted(TableConfig) ->
	UnsortedList = util_data:foldl(fun(Record, AccIn) -> AccIn ++ [ Record ] end, [], TableConfig#emxstoreconfig.tableid),
	lists:sort(fun(Elem1, Elem2) -> 
					First = calendar:datetime_to_gregorian_seconds(Elem1#emxcontent.writetime),
					Second = calendar:datetime_to_gregorian_seconds(Elem2#emxcontent.writetime),
					Second > First end, UnsortedList). 
