-module(factoid_plugin).

-include("db.hrl").

-export([start/0, init/0, deps/0, loop/0]).

-export([delete_factoid/3, change_factoid/3, literal_factoid/3]).

start() ->
    spawn(factoid_plugin, init, []).

init() ->
    msg_dispatch:add([{bot_plugin, "PRIVMSG"}]),
    loop().

deps() ->
    [].


loop() ->
    receive
        {{bot_plugin, "PRIVMSG"}, {From, [$# | _Channel] = To, Message}, Bot} ->
            case should_handle(From, To, Message, Bot) of
                false ->
                    ok;
                Msg ->
                    privmsg(From, To, Msg, Bot)
            end;
        {{bot_plugin, "PRIVMSG"}, {_From, _To, _Message}, _Bot} ->
            ok
    end,
    factoid_plugin:loop().

%% Helper functions
privmsg(From, To, Message, Bot) ->
    {ok, Name} = bot_manager:fetch_name(Bot),
    factoid_result(To, factoid:process(From, {To, Name}, Message), Bot).
    
factoid_result(To, {set, {ok, #factoid_data{name=Name}}}, Bot) ->
    irc_bot:say(Bot, To, io_lib:format("~s set.", [Name])),
    ok;
factoid_result(To, {get, {ok, #factoid_data{data={string, Data}}}, _}, Bot) ->
    irc_bot:say(Bot, To, Data),
    ok;
factoid_result(To, {get_who, Who, {ok, #factoid_data{data={string, Data}}}, _}, Bot) ->
    irc_bot:say(Bot, To, Who ++ ": " ++ Data);
factoid_result(To, {get, {ok, #factoid_data{data={function, Func}}}, Leftover}, Bot) ->
    case catch(Func(To, Bot, Leftover)) of
        Res when is_list(Res) ->
            irc_bot:say(Bot, To, Res);
         _ ->
            irc_bot:say(Bot, To, "That caused an exception, don't do that")
    end;
factoid_result(_, _, _) ->
    ok.
                      

delete_factoid(To, Bot, Factoid) ->
    {ok, Name} = bot_manager:fetch_name(Bot),
    case factoid:delete(string:to_lower(string:strip(Factoid)), {To, Name}) of
        ok ->
            Factoid ++ " deleted";
        {error, Reason} ->
            {error, Reason}
    end.

change_factoid(To, Bot, String) ->
    {ok, Name} = bot_manager:fetch_name(Bot),
    {Factoid, Regexp} = irc_lib:split_once(String, $\s),
    ["s", Old, New] = split_string_slash(Regexp),
    {ok, RE} = regexp:parse(Old),
    {ok, #factoid_data{data={string, Data}}} = factoid:factoid_get(Factoid, {To, Name}),
    %% The usage of flatten here is odd and I don't like it
    {ok, NewString, _} = regexp:sub(lists:flatten(Data), RE, New),
    factoid:factoid_set_value(Factoid, {To, Name}, {string, lists:flatten(NewString)}),
    Factoid ++ " updated".
    
literal_factoid(_To, _Bot, String) ->
    String.


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
    case lists:member(string:to_lower(Nick), ["candide", "geordi", "nolyc", "bioperl-bot"]) of
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

split_string_slash(String) ->
    lists:reverse(split_string_slash(String, [], [])).

split_string_slash([], [], Acc) ->
    Acc;
split_string_slash([], Word, Acc) ->
    split_string_slash([], [], [lists:reverse(Word) | Acc]);
split_string_slash([$\\, $/ | Rest], Word, Acc) ->
    split_string_slash(Rest, [$/ | Word], Acc);
split_string_slash([$/ | Rest], Word, Acc) ->
    split_string_slash(Rest, [], [lists:reverse(Word) | Acc]);
split_string_slash([C | Rest], Word, Acc) ->
    split_string_slash(Rest, [C | Word], Acc).
