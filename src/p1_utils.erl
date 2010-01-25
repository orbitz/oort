-module(p1_utils).

-export([bot_trigger/2, should_handle/4]).

% Determines if the bot has been triggered, these are hardcoded for now because i don't know
% of a really nice way to make it a config option withotu doing too much work
% Th enick is apssed to this because generally nicks are used to trigger bots
bot_trigger(Nick, Message) ->
    % Normalize the message and nick to lowercase
    NormNick = string:to_lower(Nick),
    NormMsg = string:to_lower(Message),
    case string:equal(NormNick, string:substr(NormMsg, 1, length(NormNick))) of
        true ->
            remove_delim(string:substr(Message, length(NormNick) + 1));
        false ->
            false
    end.

should_handle(From, _Channel, Message, Bot) ->
    {ok, Nick, _Ident, _Hostname} = irc_lib:decode_mask(From),
    case lists:member(string:to_lower(Nick), ["candide", "geordi", "nolyc", "bioperl-bot", "r"]) of
        true ->
            false;
        false ->
            bot_trigger(irc_bot:get_nick(Bot), Message)
    end.


remove_delim([$, | Msg]) ->
    string:strip(Msg, left);
remove_delim([$. | Msg]) ->
    string:strip(Msg, left);
remove_delim([$: | Msg]) ->
    string:strip(Msg, left);
remove_delim([$\s | Msg]) ->
    string:strip(Msg, left);
remove_delim(_) ->
    false.
