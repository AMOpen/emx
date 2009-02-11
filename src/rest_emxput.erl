-module(rest_emxput).

-export([handle_tokenRequest/3, out/1]).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("records.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include_lib("stdlib/include/qlc.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = util_yaws:get_path(Req),
    %% util_flogger:logMsg(self(), ?MODULE, debug,"~p - ~p", [Req#http_request.method, ReqPath]),
    PathTokens = string:tokens(ReqPath, "/"),
    handle_tokenRequest(Req#http_request.method, PathTokens,
			Arg).

handle_tokenRequest('POST',
		    ["emx", "put"], Arg) ->
	  Vars = yaws_api:parse_post(Arg),
	  RealQueryData = convertData(Vars),
	  DisplayName = proplists:get_value(displayName, RealQueryData),
	  util_flogger:logMsg(self(), ?MODULE, debug, "Store ~p", [ DisplayName]),
	  Resp = emx_admin:put_data(DisplayName, proplists:get_value(xml, RealQueryData)),
	  util_yaws:make_response(200,
			    util_string:format("<response>~p</response>",
							[Resp]));
handle_tokenRequest(_A, _B, _C) ->
    util_flogger:logMsg(self(), ?MODULE, debug, "Invalid request", []),
    util_yaws:make_response(200, "<error/>").

handle_tokenRequest_1(Record, AtomList) ->
    lists:any(fun (AtomId) ->
		      element(1, Record) == list_to_atom(AtomId)
	      end,
	      AtomList).

convertText(Data) ->
    util_flogger:logMsg(self(), ?MODULE, debug, "~p",
			[Data]),
    lists:foldl(fun ({Name, Value}, AccIn) ->
			util_flogger:logMsg(self(), ?MODULE, debug,
					    "Name ~p, Value ~p", [Name, Value]),
			AccIn ++
			  util_string:format("<record><name>~p</name><value>~p</value></rec"
						      "ord>",
						      [Name, Value])
		end,
		"", Data).

convertData(RequestArray) ->
    %% Request Array will be an array of { key, value} tuples, key will be a string, we want it to be an atom
    %% the value will also be a string, we want to attempt to convert that depending on the prefix of the atom
    [convertData_1(V1) || V1 <- RequestArray].

convertData_1({"xmlencode", Value}) when is_list(Value) ->
    String = base64:mime_decode_to_string(replaceSpace(Value)),
    %%io:format("Decode is ~p~n", [ String ]),
    {xml, String};    
       
convertData_1({Key, Value}) ->
    {list_to_atom(Key), Value}.

replaceSpace(S) ->
	replaceSpace(S, []).
	
replaceSpace([], Acc) ->
	lists:reverse(Acc);
replaceSpace([$\s | Rest], Acc) ->
	replaceSpace(Rest, [$+ | Acc]);
replaceSpace([C | Rest], Acc) ->
	replaceSpace(Rest, [ C | Acc ]).