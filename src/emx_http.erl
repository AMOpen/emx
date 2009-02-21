-module(emx_http).


post(Url, Username, Password, Params) when is_binary(Username) ->
    post(Url, binary_to_list(Username), Password, Params);
 
post(Url, Username, Password, Params) when is_binary(Password) ->
    post(Url, Username, binary_to_list(Password), Params);
 
post(Url, Username, Password, Params) ->
    Encoded = binary_to_list(
    base64:encode(Username ++ ":" ++ Password)),
    Headers =
  [{"Authorization", "Basic " ++ Encoded}],
    ContentType = "application/x-www-form-urlencoded",
    Body = twoorl_util:join([[Field,$=,yaws_api:url_encode(Val)] ||
        {Field,Val} <- Params], "&"),
    http:request(
      post,
      {Url, Headers, ContentType, iolist_to_binary(Body)}, [], []).
      
put_data(Server, Port, Key, Data)->
     nothing.
     
get_data(Server, Port, Key) ->
     nothing.
     
get_data_keys(Server, Port, KeyPrefix, Epoch) ->
    nothing.
    
