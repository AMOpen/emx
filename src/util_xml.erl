%% @author Alan Moore amkimian@mac.com
%% @copyright 2008 Alan Moore

-module(util_xml).
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("records.hrl").

-author('Alan Moore <amkimian@mac.com>').
-compile(export_all).

goParsed(ParsedXml, Xpath) ->
	try
		handleMatch(xmerl_xpath:string(Xpath, ParsedXml))
	catch
		_ ->
			null
	end.

go(Xml, Xpath) ->
	try
		{ParsedXml, _} = xmerl_scan:string(Xml),
		handleMatch(xmerl_xpath:string(Xpath, ParsedXml))
	catch
		_ ->
			null
	end.

handleMatch([{xmlText, _, _, _, Value, text} | _]) ->
	Value;

handleMatch([{xmlAttribute, _Attr, _, _, _, _, _, _, Value, false} | _]) ->
	Value;

handleMatch(_) ->
	null.
	
content_test_() ->
	Doc = "<fred><hello name='one'>1</hello></fred>",
	[
	?_assert(go(Doc, "/fred/hello/text()") =:= "1"),
	?_assert(go(Doc, "/fred/hello") =:= null),
	?_assert(go(Doc, "/fred/hello/@name") =:= "one")
	].
	
