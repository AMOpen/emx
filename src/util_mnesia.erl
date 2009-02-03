%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-9 Alan Moore

-module(util_mnesia).

-author('Alan Moore <amkimian@mac.com>').

-export([do/1, doLimit/2, forAll/2, forAll/3,
	 forAllMax/3, forAllRemove/4, getAllData/2, getData/2,
	 transformAndGet/5, transformData/4,
	 transformDataAll/4]).

do(Q) ->
    F = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

doLimit(Q, Max) ->
    F = fun () ->
		C = qlc:cursor(Q),
		R = qlc:next_answers(C, Max),
		ok = qlc:delete_cursor(C),
		R
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

forAll(Q, Fun) ->
    AccF = fun (Record, AccIn) -> Fun(Record), AccIn end,
    F = fun () -> qlc:fold(AccF, [], Q, unique_all) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

forAllMax(Q, Fun, Max) ->
    F = fun () ->
		C = qlc:cursor(Q),
		R = qlc:next_answers(C, Max),
		ok = qlc:delete_cursor(C),
		lists:foreach(Fun, R)
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

forAllRemove(Table, MatchHead, Max, DeleteFun) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       Max, read),
		case Result of
		  {Records, _Cont} ->
		      lists:foreach(fun (Record) -> DeleteFun(Record, Table)
				    end,
				    Records);
		  '$end_of_table' -> 1;
		  true -> 1
		end
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

forAll(Table, MatchHead, Fun) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       500, read),
		case Result of
		  {Records, _Cont} -> lists:foreach(Fun, Records);
		  '$end_of_table' -> 1;
		  true -> 1
		end
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

getData(Table, MatchHead) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       1, read),
		case Result of
		  {Records, _Cont} -> Records;
		  '$end_of_table' -> [];
		  true -> []
		end
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

getAllData(Table, MatchHead) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       1000, read),
		case Result of
		  {Records, _Cont} -> Records;
		  '$end_of_table' -> [];
		  true -> []
		end
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

transformAndGet(Table, MatchHead, Transformer, Param,
		MaxRecords) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       MaxRecords, read),
		case Result of
		  {Records, _Cont} ->
		      RetRecords = [Transformer(Record, Param)
				    || Record <- Records],
		      lists:foreach(fun (Record) ->
					    mnesia:write(Table, Record, write)
				    end,
				    RetRecords);
		  '$end_of_table' -> RetRecords = [];
		  _ -> RetRecords = []
		end,
		RetRecords
	end,
    TransRes = mnesia:transaction(F),
    case TransRes of
      {atomic, Val} -> Val;
      _ -> TransRes
    end.

transformData(Table, MatchHead, Transformer, Param) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       1, read),
		case Result of
		  {Records, _Cont} ->
		      [Record | _] = Records, State = recordexist;
		  '$end_of_table' -> Record = null, State = recordnoexist;
		  _ -> Record = null, State = recordnoexist
		end,
		NewRecord = Transformer(Record, Param),
		case NewRecord of
		  null -> {fail, norecord};
		  _ -> {mnesia:write(Table, NewRecord, write), State}
		end
	end,
    TransRes = mnesia:transaction(F),
    case TransRes of
      {atomic, Val} -> Val;
      _ -> TransRes
    end.

transformDataAll(Table, MatchHead, Transformer,
		 Param) ->
    F = fun () ->
		Result = mnesia:select(Table, [{MatchHead, [], ['$_']}],
				       1000, read),
		case Result of
		  {Records, _Cont} ->
		      %%io:format("All records are ~p~n", [ Records]),
		      Records;
		  '$end_of_table' -> Records = [];
		  _ -> Records = []
		end,
		lists:foreach(fun (Record) ->
				      %%io:format("Running for ~p~n", [ Record]),		
				      NewRecord = Transformer(Record, Param),
				      case NewRecord of
					null -> {fail, norecord};
					_ ->
					    mnesia:write(Table, NewRecord,
							 write)
				      end
			      end,
			      Records)
	end,
    TransRes = mnesia:transaction(F),
    case TransRes of
      {atomic, Val} -> Val;
      _ -> TransRes
    end.
