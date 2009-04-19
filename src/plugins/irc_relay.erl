-module(irc_relay).
-author("orbitz@ortdotlove.net").

-include("db.hrl").

-export([relay/3, add_relay/2]).

add_relay(From, To) ->
    p1_db:insert_row(#relay_client{from=From, to=To}).

relay(From, To, Message) ->
    {ok, FromNick, _, _} = irc_lib:decode_mask(From),
    broadcast_msg(FromNick, get_destinations(irc_bot:get_botname(self()), To), Message).

broadcast_msg(From, [{Botname, Dest} | Rest], Message) ->
    irc_bot:say(irc_bot:get_botpid(Botname), Dest, io_lib:format("(~s@~s) ~s", [From, Botname, Message])),
    broadcast_msg(From, Rest, Message);
broadcast_msg(_, [], _) ->
    ok.
    

% ----------------------------------------------------------------
% Helper funcs
get_destinations(Botname, From) ->
    Query = #relay_client{from={Botname, From}, to='$1', _='_'},
    {atomic, List} = p1_db:select(relay_client, [{Query, [], ['$1']}]),
    List.
