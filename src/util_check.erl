-module(util_check).

-export([check_message_queue/0]).

-include("emx.hrl").

check_message_queue() ->
    none.
    
check_message_queue_old() ->
    {message_queue_len, Len} = erlang:process_info(self(),
						   message_queue_len),
    case Len > 100 of
      true ->
	  ?LOG(debug, "Message queue length is ~p", [Len]);
      false -> none
    end.
