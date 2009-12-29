-module(memo_plugin).

-include("db.hrl").

-export([start/0, init/0, deps/0, loop/0, create_tables/0,
        add_memo/3, read_memos/1, delete_memos/1, delete_memo/1,
        parse_message/1, handle_message/4]).

%%
% This record represents a memo.
% to is in the from of nick@channel@botname
% from is of the same form
% timestamp is the result of erlang:localtime
% message is the content of the message
-record(memos, {to, from, timestamp, message}).

create_tables() ->
    p1_db:create_table(memos, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, memos)}]).
    

start() ->
    spawn(memo_plugin, init, []).

init() ->
    msg_dispatch:add([{bot_plugin, "PRIVMSG"}]),
    loop().

deps() ->
    [].


loop() ->
    receive
        {{bot_plugin, "PRIVMSG"}, {From, To, Message}, Bot} ->
            {ok, Name} = bot_manager:fetch_name(Bot),
            BotStr = atom_to_list(Name),
            {ok, Nick, _, _} = irc_lib:decode_mask(From),
            FromValue = string:to_lower(Nick) ++ "@" ++ BotStr,
            case p1_utils:should_handle(From, To, Message, Bot) of
                false ->
                    case check_for_memos(FromValue) of
                        true ->
                            irc_bot:say(Bot, Nick, "You have memos awaiting you, type say \"oort memo read\" in order to read them");
                        false ->
                            ok
                    end;
                Msg ->
                    SayList = case handle_message(FromValue, To, BotStr, Msg) of
                                  {ok, Say} ->
                                      Say;
                                  {error, unknown_command} ->
                                      ["Unknown command"];
                                  {error, missing_params} ->
                                      ["You are missing some parameters"];
                                  {error, bad_param} ->
                                      ["You have given me a bad paramter, jackass"];
                                  {error, _} ->
                                      ["I quite honestly do not know what happened here"];
                                  {ignore, _} ->
                                      []
                              end,
                    lists:foreach(fun(OutMsg) -> irc_bot:say(Bot, Nick, OutMsg) end, SayList)
            end
    end,
    memo_plugin:loop().


%%
% Adds a memo, this creates the timestamp on its own
add_memo(To, From, Message) ->
    {atomic, Ok} = p1_db:insert_row(#memos{to=To,
                                           from=From,
                                           timestamp=erlang:localtime(),
                                           message=Message}),
    Ok.

%%
% Reads all memos for a particular recipient
read_memos(To) ->
    {atomic, Messages} = p1_db:read(memos, To),
    Messages.

%%
% This delets all memos for a particular person
delete_memos(To) ->
    {atomic, ok} = p1_db:delete(memos, To),
    ok.
    

%%
% Deletes a specific memo
delete_memo(Memo) ->
    {atomic, ok} = p1_db:delete_record(Memo),
    ok.

%%
% Checks to see if any memos exist for a user
% Returns true if there are memos to be processed
check_for_memos(To) ->
    length(read_memos(To)) /= 0.


%%
% This handles a message that comes in from a channel
%
% From is the source, To is a channel or a the bot if it is over pm
% Bot is which bot this happens on.
% This returns:
% {ok, [String]} -- List of strings to write out as seperate messages
% {error, Error} -- Error why this failed
% {ignore, Ignore} -- Ignore this message
handle_message(From, To, BotName, Message) ->
    case parse_message(Message) of
        {ok, Ret} ->
            handle_parsed_message(From, To, BotName, Ret);
        Other ->
            Other
    end.

handle_parsed_message(From, _To, BotName, {add_memo, ToNick, Msg}) ->
    ToValue = ToNick ++ "@" ++ BotName,
    ok = add_memo(ToValue, From, Msg),
    {ok, ["Successfully added memo"]};
            
%%
% Read a message and delete it
handle_parsed_message(From, _To, _BotName, {read_memo, Value}) ->
    case read_memos(From) of
        Memos when Value > 0 andalso length(Memos) =< Value andalso length(Memos) /= 0 ->
            Memo = lists:nth(Value, Memos),
            delete_memo(Memo),
            {ok, [format_memo(Memo)]};
        _ ->
            {ok, "Invalid index cheif"}
    end;

%%
% Sends them all of the memos they have and deletes them
handle_parsed_message(From, _To, _BotName, {read_memo_all}) ->
    Memos = read_memos(From),
    lists:foreach(fun(M) -> delete_memo(M) end, Memos),
    {ok, [format_memo(M) || M <- Memos]};

%%
% Gives user a help menu
handle_parsed_message(_From, _To, _BotName, {help_memo}) ->
    {ok, ["Memo help:",
          "oort memo for <who> <text> -- Give someone a memo",
          "oort memo read -- Read all your memos",
          "oort memo read # -- Read a particular memo",
          "oort memo help -- This",
          "Note, all memos are deleted upon reading"]};
%%
% Just delete them
handle_parsed_message(_From, _To, _BotName, {delete_memo, _Value}) ->
    {ok, ["This does nothing currently"]}.


format_memo(Memo) ->
    [Nick | _Rest] = string:tokens(Memo#memos.from, "@"),
    Nick ++ " says: " ++ Memo#memos.message.

parse_message(Message) ->
    case Message of
        "memo for " ++ Rest ->
            parse_memo_for(Rest);
        "memo to " ++ Rest ->
            parse_memo_for(Rest);
        "memo read " ++ Rest ->
            parse_memo_read(Rest);
        "memo read" ->
            parse_memo_read_all();
        "memo delete " ++ Rest ->
            parse_memo_delete(Rest);
        "memo del " ++ Rest ->
            parse_memo_delete(Rest);
        "memo help" ->
            parse_memo_help();
        "memo " ++ _ ->
            {error, unknown_command};
        _ ->
            % Not a memo command
            {ignore, ok}
    end.

parse_memo_for(Message) ->
    case lists:splitwith(fun(A) -> A /= $\s end, string:strip(Message)) of
        {[], []} ->
            {error, missing_params};
        {_Name, []} ->
            {error, missing_params};
        {Name, Msg} ->
            {ok, {add_memo, string:to_lower(Name), string:strip(Msg, left)}}
    end.

parse_memo_read(Message) ->
    try
        list_to_integer(Message)
    of
        Int ->
            {ok, {read_memo, Int}}
    catch
        error:badarg ->
            {error, bad_param}
    end.

parse_memo_read_all() ->
    {ok, {read_memo_all}}.

parse_memo_delete(Message) ->
    {ok, {read_memo, Int}} = parse_memo_read(Message),
    {ok, {delete_memo, Int}}.

parse_memo_help() ->
    {ok, {help_memo}}.
