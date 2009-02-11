-module(rest_emxgetkeys).

-export([out/1]).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("records.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include_lib("stdlib/include/qlc.hrl").

-include_lib("eunit/include/eunit.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = util_yaws:get_path(Req),
    {KeyPrefix, EpochNumber} = get_vals(ReqPath),
    
    {datainfo, {MaxEpoch, Keys}} = emx_admin:get_datakeys(KeyPrefix, EpochNumber),
    MainContent = lists:foldl(fun(Key, AccIn) ->
    					ContentKey = util_string:format("<key>~s</key>", [ Key#emxcontent.displayname]),					
    					AccIn ++ ContentKey
					end, [], Keys),				
    util_yaws:make_response(200, util_string:format("<Keys MaxEpoch='~p'>~s</Keys>", [ MaxEpoch, MainContent])).
    
get_vals(ReqPath) ->
     ["emx", "getkeys" | Remainder] = string:tokens(ReqPath, "/"),
     parse_vals(Remainder).
     
parse_vals([Path1, Path2]) ->
	parse_vals([Path1, Path2, 0]);
	
parse_vals([Path1, Path2, Epoch]) when is_list(Epoch) ->
	parse_vals([Path1, Path2, util_conv:getIntValue(Epoch)]);
	
parse_vals([Path1, Path2, Epoch]) when is_integer(Epoch) ->
	{string:join([Path1, Path2], "/"), Epoch}.
	
get_vals_test() ->
	[
		?_assert(get_vals("emx/getkeys/official/system.config/123") =:= { "official/system.config", 123}),
		?_assert(get_vals("emx/getkeys/official/system.config") =:= { "official/system.config", 0})
	].
	
