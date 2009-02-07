-module(rest_emxgetkeys).

-export([out/1]).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("records.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include_lib("stdlib/include/qlc.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = util_yaws:get_path(Req),
    %%io:format("Path is ~p~n", [ ReqPath]),
    KeyPrefix = string:join(lists:nthtail(2, string:tokens(ReqPath, "/")), "/"),
    {datainfo, Keys} = emx_admin:get_datakeys(KeyPrefix),
    MainContent = lists:foldl(fun(Key, AccIn) ->
    					ContentKey = util_string:format("<key>~s</key>", [ Key]),					
    					AccIn ++ ContentKey
					end, [], Keys),				
    util_yaws:make_response(200, util_string:format("<Keys>~s</Keys>", [ MainContent])).
