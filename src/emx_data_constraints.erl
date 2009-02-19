%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

-module(emx_data_constraints).

%% Access to dets/ets tables for the store - low level

-author('Alan Moore <amkimian@mac.com>').

-export([run_constraints/2, run_constraint/2]).

-include_lib("records.hrl").

%% The following functions are to handle the removal of data from a cache to keep it within certain constraints
%% Constraints are by memory use, number of records or age of documents.

run_constraints(_TableConfig, []) ->
	ok;
run_constraints(TableConfig, [ H | T]) ->
	case TableConfig#emxstoreconfig.location of
		system -> null;
		_ ->
			case lists:any(fun(Node) -> Node == node() end, TableConfig#emxstoreconfig.location) of
				true -> run_constraint(TableConfig, H);
				false -> null
			end
	end,
	run_constraints(TableConfig, T).

run_constraint(_TableConfig, { records, infinity }) ->
	nothingtodo;
	
run_constraint(TableConfig, { records, MaxCount }) ->
	%% The table should have no more than MaxCount records. If there are more than that,
	%% remove them in age order until the number of records is below MaxCount
	{ RecordCount, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	case RecordCount > MaxCount of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p to keep number of records in limit", [ Record#emxcontent.displayname]),
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				{ NewRecordCount, _ } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
				NewRecordCount > MaxCount
				end, SortedList),
			%% Run garbage collection after removing the data to ensure that future tests on memory use the correct figures
			erlang:garbage_collect();				
		false ->
			nothingtodo
	end;

run_constraint(_TableConfig, { age, infinity}) ->
	nothingtodo;
	
run_constraint(TableConfig, { age, MaxAge }) ->
	%% Remove all records older than MaxAge
	TestTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - MaxAge,	
	util_data:foldl(fun(Record, AccIn) ->
		WriteTime = calendar:datetime_to_gregorian_seconds(Record#emxcontent.writetime),
		case WriteTime < TestTime of
			true ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p as it is old", [ Record#emxcontent.displayname]),
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname);
			false ->
				ok
		end,
		erlang:garbage_collect(),
		AccIn end, [], TableConfig#emxstoreconfig.tableid);

run_constraint(_TableConfig, { size, infinity }) ->
	nothingtodo;
		
run_constraint(TableConfig, { size, MaxSize }) ->
	%% The table should be smaller than MaxSize. If it is greater, remove records
	%% in age order until the size is reduced below MaxSize
	{ _, Memory } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
	%%io:format("Memory use is ~p~n", [ Memory]),
	case Memory > MaxSize of
		true ->
			%% need to remove
			SortedList = get_all_sorted(TableConfig),
			lists:takewhile(fun(Record) ->
				util_flogger:logMsg(self(), ?MODULE, debug, "Removing ~p to free up memory", [ Record#emxcontent.displayname]),
				util_data:delete_data(TableConfig#emxstoreconfig.tableid, Record#emxcontent.displayname),
				%% Run garbage collect after deletion or the get_size method below will not return the correct
				%% and up to date value. It makes the whole loop slower though
				erlang:garbage_collect(),
				{ _, NewMemory } = util_data:get_size(TableConfig#emxstoreconfig.tableid),
				NewMemory > MaxSize
				end, SortedList);
		false ->
			nothingtodo
	end.

%% Get data from a table and sort it by age

get_all_sorted(TableConfig) ->
	UnsortedList = util_data:foldl(fun(Record, AccIn) -> AccIn ++ [ Record ] end, [], TableConfig#emxstoreconfig.tableid),
	lists:sort(fun(Elem1, Elem2) -> 
					First = calendar:datetime_to_gregorian_seconds(Elem1#emxcontent.writetime),
					Second = calendar:datetime_to_gregorian_seconds(Elem2#emxcontent.writetime),
					Second > First end, UnsortedList). 
