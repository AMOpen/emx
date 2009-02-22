-module(rest_emxgetkeys).

-export([out/1]).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("emx.hrl").

-include_lib("eunit/include/eunit.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = util_yaws:get_path(Req),
    PathTokens = string:tokens(ReqPath, "/"),
    processPath(PathTokens).

processPath(["emx", "gettables"]) ->
    Prefixes = emx_admin:get_prefixes(),
    MainContent = lists:foldl(fun (Prefix, AccIn) ->
				      case Prefix of
					"default" -> AccIn;
					_ ->
					    ContentKey =
						util_string:format("<Table>~s</Table>",
								   [Prefix]),
					    AccIn ++ ContentKey
				      end
			      end,
			      [], Prefixes),
    util_yaws:make_response(200,
			    util_string:format("<Tables>~s</Tables>",
					       [MainContent]));
processPath(["emx", "getkeys" | Remainder]) ->
    {KeyPrefix, EpochNumber} = parse_vals(Remainder),
    {datainfo, {MaxEpoch, Keys}} =
	emx_admin:get_datakeys(KeyPrefix, EpochNumber),
    MainContent = lists:foldl(fun (Key, AccIn) ->
				      ContentKey =
					  util_string:format("<Key>~s</Key>",
							     [Key#emxcontent.displayname]),
				      AccIn ++ ContentKey
			      end,
			      [], Keys),
    util_yaws:make_response(200,
			    util_string:format("<Keys MaxEpoch='~p'>~s</Keys>",
					       [MaxEpoch, MainContent]));
processPath(["emx", "getdata" | Remainder]) ->
    {KeyPrefix, EpochNumber} = parse_vals(Remainder),
    {datainfo, {MaxEpoch, Keys}} =
	emx_admin:get_datakeys(KeyPrefix, EpochNumber),
    MainContent = lists:foldl(fun (Key, AccIn) ->
				      ContentKey =
					  util_string:format("<Data><Key>~s</Key><Content>~s</Content></Data>",
							     [Key#emxcontent.displayname,
							      Key#emxcontent.content]),
				      AccIn ++ ContentKey
			      end,
			      [], Keys),
    util_yaws:make_response(200,
			    util_string:format("<DataSet MaxEpoch='~p'>~s</DataSet>",
					       [MaxEpoch, MainContent])).

parse_vals([Path1, Path2]) ->
    parse_vals([Path1, Path2, 0]);
parse_vals([Path1, Path2, Epoch]) when is_list(Epoch) ->
    parse_vals([Path1, Path2,
		util_conv:getIntValue(Epoch)]);
parse_vals([Path1, Path2, Epoch])
    when is_integer(Epoch) ->
    {string:join([Path1, Path2], "/"), Epoch}.
