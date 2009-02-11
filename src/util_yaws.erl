-module(util_yaws).

-export([get_path/1, make_ehtml_response/2,
	 make_response/2]).

-export([make_all_response/4, make_response/3]).

-include("../../yaws-1.77/include/yaws.hrl").

-include("../../yaws-1.77/include/yaws_api.hrl").

-include_lib("records.hrl").


get_path(Req) ->
    {_, Path} = Req#http_request.path, Path.

make_ehtml_response(Status, Message) ->
    make_all_response(Status, make_header("text/html"),
		      Message, ehtml).

make_response(Status, Message) ->
    make_response(Status, "application/xml", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message,
		      html).

make_header(Type) ->
    [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message, Type) ->
    [{status, Status}, {allheaders, Headers},
     {Type, Message}].

