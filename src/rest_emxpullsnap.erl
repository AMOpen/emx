-module(rest_emxpullsnap).

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


processPath(["emx", "pullsnap", CouchDBDatabase]) ->
% Need to load the documents from couch, then persist them into a table
% in emx
    { ok, { obj, [ _, _, { "rows", Rows}] }} = ecouch:doc_get_all(CouchDBDatabase, []),

    lists:foreach(fun({obj, Data}) ->
    		    % Data will be [ { "key", Value } ]
    		    % Find the record with the value of "key"
    		    % Write the value out as that
    		    [{"id", ID} | _] = lists:filter(fun({K,V}) -> K=="id" end, Data),
    		    io:format("ID is ~p~n", [ID]),
    		    {ok, Doc} = ecouch:doc_get(CouchDBDatabase, binary_to_list(ID)),
		    emx_admin:put_data(lists:append(["Couch", "/", CouchDBDatabase,"/",binary_to_list(ID)]), rfc4627:encode(Doc))
    		end, Rows),

    %% For each Row (which is {obj, [ {key, value} ]}, write a doc into emx
    util_yaws:make_response(200,"<ok/>").
