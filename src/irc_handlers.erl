-module(irc_handlers).

-export([handle_whois/4, handle_connect/4, handle_join/4]).

-include("irc.hrl").

%% Whois
handle_whois(_Sock, _Client, State, {_, "RPL_WHOISUSER", Args}) ->
    State#irc_command{state={whois, [{user, Args}]}};
handle_whois(_Sock, _Client, #irc_command{state={whois, Info}} = State, {_, "RPL_WHOISSERVER", Args}) ->
    State#irc_command{state={whois, [{server, Args} | Info]}};
handle_whois(_Sock, _Client, #irc_command{state={whois, Info}} = State, {_, "320", _}) ->
    % Some servers suppor this (like freenode)
    %{whois, [{identified, Args} | Info]};  % I dont' think we need teh ARgs stuff
    State#irc_command{state={whois, [identified | Info]}};
handle_whois(_Sock, _Client, #irc_command{state={whois, Info}} = State, {_, "RPL_WHOISIDLE", Args}) ->
    State#irc_command{state={whois, [{idle, Args} | Info]}};
handle_whois(_Sock, _Client, #irc_command{state={whois, Info}} = State, {_, "RPL_WHOISCHANNELS", Args}) ->
    State#irc_command{state={whois, [{channels, Args} | Info]}};
handle_whois(_Sock, _Client, #irc_command{state={whois, Info}} = State, {_, "RPL_WHOISOPERATOR", Args}) ->
    State#irc_command{state={whois, [{oper, Args} | Info]}};
handle_whois(_Sock, _Client, #irc_command{state={whois, Info}, func=Fun}, {_, "RPL_ENDOFWHOIS", _}) ->
    Fun({ok, Info}),
    ok.



%% Connecting
handle_connect(Sock, Client, #irc_command{state=connecting} = State, _) ->
    case dict:fetch(password, Client) of
        undefined ->
            ok;
        Pass ->
            irc_lib:send_command(Sock, [{"PASS", [Pass]}])
    end,
    irc_lib:send_command(Sock, [{"NICK", [dict:fetch(nick, Client)]}, {"USER", ["1", "2", "3", ":" ++ dict:fetch(realname, Client)]}]),
    State#irc_command{state=nick_verify};
handle_connect(_Sock, _Client, #irc_command{state=nick_verify, func=Fun},
               {_, Msg, _}) when Msg == "ERR_NICKCOLLISION" orelse
                                 Msg == "ERR_NICKNAMEINUSE" ->
    Fun({error, Msg}),
    %% Everythign is *NOT* ok, but the surounding code doesn't care here unless we
    %% Explicilty want to kill everything
    ok;
handle_connect(_Sock, _Client, #irc_command{state=nick_verify, func=Fun}, {_, "RPL_WELCOME", _}) ->
    Fun(ok),
    ok;

handle_connect(_Sock, _Client, _State, {_, "PING", _}) ->
    dispatch_msg;
%% For this, we want to ignore anything else until we are ready
handle_connect(_Sock, _Client, State, _Msg) ->
    State.


%% Joining
handle_join(_Sock, _Client, #irc_command{state={join, Info}} = State, {_, "RPL_TOPIC", [_, _, Topic]}) ->
    State#irc_command{state={join, [{topic, Topic} | Info]}};
handle_join(_Sock, _Client, #irc_command{state={join, Info}} = State, {_, "333", [_, _, Author, _]}) ->
    State#irc_command{state={join, [{topic_author, Author} | Info]}};
handle_join(_Sock, _Client, #irc_command{state={join, Info}} = State, {_, "RPL_NAMREPLY", [_, _, _, Names]}) ->
    State#irc_command{state={join, [{users, string:tokens(Names, " ")} | Info]}};
handle_join(_Sock, _Client, #irc_command{state={join, Info}, func=Fun}, {_, "RPL_ENDOFNAMES", _}) ->
    Fun({ok, Info}),
    ok;
handle_join(_Sock, _Client, #irc_command{state={join, _Data}, func=Fun},
            {_, Msg, _}) when Msg == "ERR_BANNEDFROMCHAN" orelse
                              Msg == "ERR_TOOMANYCHANNELS" orelse
                              Msg == "ERR_INVITEONLY" orelse
                              Msg == "ERR_NOSUCHCHANNEL" orelse
                              Msg == "ERR_CHANNELISFULL" orelse
                              Msg == "ERR_BADCHANNELKEY" ->
    Fun({error, Msg}),
    ok;
handle_join(_Sock, _Client, _State, _Msg) ->
    dispatch_msg.
