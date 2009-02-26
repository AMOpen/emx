-module(emx_http).

-export([get_data/3, get_data_keys/4, put_data/4]).

post(Url, Username, Password, Params)
    when is_binary(Username) ->
    post(Url, binary_to_list(Username), Password, Params);
post(Url, Username, Password, Params)
    when is_binary(Password) ->
    post(Url, Username, binary_to_list(Password), Params);
post(Url, Username, Password, Params) ->
    Encoded = binary_to_list(base64:encode(Username ++
					     ":" ++ Password)),
    Headers = [],
    %% [{"Authorization", "Basic " ++ Encoded}],
    ContentType = "application/x-www-form-urlencoded",
    Body = util_string:join([[Field, $=,
			      yaws_api:url_encode(Val)]
			     || {Field, Val} <- Params],
			    "&"),
    http:request(post,
		 {Url, Headers, ContentType, iolist_to_binary(Body)}, [],
		 []).

put_data(Server, Port, Key, Data) ->
    Params = [{"displayName", Key},
	      {"xmlencode", base64:encode_to_string(Data)}],
    Url = util_string:format("http://~s:~p/emx/put",
			     [Server, Port]),
    post(Url, "alan", "fred", Params).

get_data(Server, Port, Key) -> nothing.

get_data_keys(Server, Port, KeyPrefix, Epoch) ->
    nothing.
