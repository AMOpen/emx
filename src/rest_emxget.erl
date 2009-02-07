-module(rest_emxget).

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

    DisplayName = string:join(lists:nthtail(2, string:tokens(ReqPath, "/")), "/"),
    %%io:format("Display name is ~p~n", [ DisplayName]),
    {datainfo, BackRecord} = emx_admin:get_data(DisplayName),
    util_yaws:make_response(200, BackRecord#emxcontent.content).
