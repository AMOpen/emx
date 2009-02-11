%% @author Alan Moore amkimian@mac.com
%% @copyright 2008-2009 Alan Moore

%% @doc Conversion utilities

-module(util_conv).
-include_lib("eunit/include/eunit.hrl").

-author('Alan Moore <amkimian@mac.com>').

-export([getFloatValue/1, getStringValue/1, getIntValue/1]).

getIntValue(Value) when is_list(Value) ->
	try
		list_to_integer(Value)
	catch
		error:badarg ->
			try
				list_to_float(Value)
			catch
				error:badarg ->
					0
			end
	end;
getIntValue(Value) when is_integer(Value) ->
	Value;
getIntValue(Value) when is_float(Value) ->
	Value.
	
getFloatValue(Value) when is_list(Value) ->
	try
		list_to_float(Value)
	catch
		error:badarg ->
			try
				list_to_integer(Value)
			catch
				error:badarg ->
					0
			end
	end;
getFloatValue(Value) when is_integer(Value) ->
	Value;
getFloatValue(Value) when is_float(Value) ->
	Value.
	
getStringValue(Value) when is_integer(Value) ->
	integer_to_list(Value);
getStringValue(Value) when is_float(Value) ->
	float_to_list(Value);
getStringValue(Value) ->
	Value.
	
convert_test_() ->
	[
	?_assert(getIntValue(5) =:= 5),
	?_assert(getIntValue(5.5) =:= 5.5),
	?_assert(getIntValue("5") =:= 5),
	?_assert(getIntValue("12345") =:= 12345),
	?_assert(getFloatValue(5) =:= 5),
	?_assert(getFloatValue(5.5) =:= 5.5),
	?_assert(getFloatValue(5.5) =:= 5.5),
	?_assert(getFloatValue("5.5") =:= 5.5),
	?_assert(getStringValue(5.5) =:= "5.50000000000000000000e+00"),
	?_assert(getStringValue(5) =:= "5")	
	].
