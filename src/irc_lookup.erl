-module(irc_lookup).
-author("orbitz@ortdotlove.net").

-export([start/0, translate_command/1, loop/2, shutdown/0, test/0]).

start() ->
    case whereis(irc_lookup_server) of
        undefined ->
            Symbols = dict:from_list([
                                      {"RPL_UNAWAY", "305"},
                                      {"ERR_TOOMANYTARGETS", "407"},
                                      {"ERR_KEYSET", "467"},
                                      {"RPL_INFO", "371"},
                                      {"RPL_TRACECLASS", "209"},
                                      {"RPL_AWAY", "301"},
                                      {"ERR_RESTRICTED", "484"},
                                      {"RPL_NOWAWAY", "306"},
                                      {"RPL_ADMINME", "256"},
                                      {"RPL_TRACESERVER", "206"},
                                      {"ERR_NOPERMFORHOST", "463"},
                                      {"ERR_NOTREGISTERED", "451"},
                                      {"RPL_BANLIST", "367"},
                                      {"RPL_ENDOFINFO", "374"},
                                      {"RPL_TRACENEWTYPE", "208"},
                                      {"RPL_LUSEROP", "252"},
                                      {"RPL_WHOWASUSER", "314"},
                                      {"RPL_LUSERCHANNELS", "254"},
                                      {"RPL_LUSERUNKNOWN", "253"},
                                      {"ERR_NOLOGIN", "444"},
                                      {"ERR_NOTEXTTOSEND", "412"},
                                      {"RPL_MYINFO", "004"},
                                      {"ERR_INVITEONLYCHAN", "473"},
                                      {"ERR_UNKNOWNMODE", "472"},
                                      {"RPL_WHOISCHANNELS", "319"},
                                      {"RPL_ENDOFUSERS", "394"},
                                      {"RPL_TOPIC", "332"},
                                      {"RPL_ADMINEMAIL", "259"},
                                      {"RPL_STATSCOMMANDS", "212"},
                                      {"RPL_ENDOFMOTD", "376"},
                                      {"ERR_NOSUCHNICK", "401"},
                                      {"ERR_TOOMANYCHANNELS", "405"},
                                      {"RPL_TRACEUSER", "205"},
                                      {"ERR_NOTONCHANNEL", "442"},
                                      {"ERR_BADCHANMASK", "476"},
                                      {"ERR_CANTKILLSERVER", "483"},
                                      {"RPL_TRYAGAIN", "263"},
                                      {"ERR_NORECIPIENT", "411"},
                                      {"RPL_NOUSERS", "395"},
                                      {"RPL_ENDOFLINKS", "365"},
                                      {"ERR_BANLISTFULL", "478"},
                                      {"RPL_SERVLIST", "234"},
                                      {"RPL_STATSOLINE", "243"},
                                      {"ERR_NOSUCHSERVER", "402"},
                                      {"ERR_UNAVAILRESOURCE", "437"},
                                      {"RPL_EXCEPTLIST", "348"},
                                      {"ERR_NICKCOLLISION", "436"},
                                      {"RPL_LIST", "322"},
                                      {"ERR_ERRONEUSNICKNAME", "432"},
                                      {"ERR_NOSUCHSERVICE", "408"},
                                      {"RPL_TRACERECONNECT", "210"},
                                      {"RPL_TRACEOPERATOR", "204"},
                                      {"RPL_USERS", "393"},
                                      {"RPL_ENDOFWHOIS", "318"},
                                      {"ERR_USERSDISABLED", "446"},
                                      {"ERR_ALREADYREGISTRED", "462"},
                                      {"RPL_ENDOFNAMES", "366"},
                                      {"RPL_WELCOME", "001"},
                                      {"RPL_LUSERME", "255"},
                                      {"ERR_USERONCHANNEL", "443"},
                                      {"ERR_NOMOTD", "422"},
                                      {"ERR_NOPRIVILEGES", "481"},
                                      {"RPL_TRACELOG", "261"},
                                      {"RPL_INVITELIST", "346"},
                                      {"ERR_UNIQOPPRIVSNEEDED", "485"},
                                      {"ERR_USERSDONTMATCH", "502"},
                                      {"RPL_ENDOFINVITELIST", "347"},
                                      {"RPL_CHANNELMODEIS", "324"},
                                      {"ERR_NICKNAMEINUSE", "433"},
                                      {"RPL_VERSION", "351"},
                                      {"RPL_ENDOFEXCEPTLIST", "349"},
                                      {"ERR_NOADMININFO", "423"},
                                      {"RPL_USERHOST", "302"},
                                      {"RPL_LUSERCLIENT", "251"},
                                      {"RPL_LINKS", "364"},
                                      {"ERR_PASSWDMISMATCH", "464"},
                                      {"RPL_TIME", "391"},
                                      {"ERR_YOUWILLBEBANNED", "466"},
                                      {"RPL_TRACELINK", "200"},
                                      {"ERR_BADCHANNELKEY", "475"},
                                      {"RPL_ENDOFSTATS", "219"},
                                      {"RPL_ADMINLOC", "258"},
                                      {"RPL_NAMREPLY", "353"},
                                      {"RPL_WHOISOPERATOR", "313"},
                                      {"RPL_CREATED", "003"},
                                      {"RPL_REHASHING", "382"},
                                      {"ERR_NOCHANMODES", "477"},
                                      {"ERR_NOSERVICEHOST", "492"},
                                      {"RPL_TRACEEND", "262"},
                                      {"ERR_NOTOPLEVEL", "413"},
                                      {"ERR_NEEDMOREPARAMS", "461"},
                                      {"ERR_CHANNELISFULL", "471"},
                                      {"RPL_LISTSTART", "321"},
                                      {"ERR_NONICKNAMEGIVEN", "431"},
                                      {"ERR_NOOPERHOST", "491"},
                                      {"ERR_WASNOSUCHNICK", "406"},
                                      {"RPL_TRACEHANDSHAKE", "202"},
                                      {"RPL_UNIQOPIS", "325"},
                                      {"ERR_CANNOTSENDTOCHAN", "404"},
                                      {"RPL_TRACESERVICE", "207"},
                                      {"RPL_TRACEUNKNOWN", "203"},
                                      {"RPL_WHOISSERVER", "312"},
                                      {"RPL_USERSSTART", "392"},
                                      {"RPL_LISTEND", "323"},
                                      {"RPL_ENDOFWHOWAS", "369"},
                                      {"RPL_ENDOFWHO", "315"},
                                      {"RPL_ISON", "303"},
                                      {"RPL_INVITING", "341"},
                                      {"ERR_CHANOPRIVSNEEDED", "482"},
                                      {"ERR_UNKNOWNCOMMAND", "421"},
                                      {"ERR_UMODEUNKNOWNFLAG", "501"},
                                      {"RPL_NOTOPIC", "331"},
                                      {"ERR_SUMMONDISABLED", "445"},
                                      {"ERR_NOORIGIN", "409"},
                                      {"ERR_FILEERROR", "424"},
                                      {"RPL_MOTDSTART", "375"},
                                      {"RPL_WHOISUSER", "311"},
                                      {"RPL_SUMMONING", "342"},
                                      {"RPL_YOURHOST", "002"},
                                      {"RPL_YOUREOPER", "381"},
                                      {"RPL_ENDOFBANLIST", "368"},
                                      {"RPL_MOTD", "372"},
                                      {"ERR_USERNOTINCHANNEL", "441"},
                                      {"ERR_BANNEDFROMCHAN", "474"},
                                      {"ERR_NOSUCHCHANNEL", "403"},
                                      {"RPL_STATSUPTIME", "242"},
                                      {"RPL_WHOREPLY", "352"},
                                      {"ERR_WILDTOPLEVEL", "414"},
                                      {"ERR_YOUREBANNEDCREEP", "465"},
                                      {"RPL_WHOISIDLE", "317"},
                                      {"RPL_BOUNCE", "005"},
                                      {"RPL_SERVLISTEND", "235"},
                                      {"RPL_TRACECONNECTING", "201"},
                                      {"RPL_UMODEIS", "221"},
                                      {"ERR_BADMASK", "415"},
                                      {"RPL_STATSLINKINFO", "211"},
                                      {"RPL_YOURESERVICE", "383"}]),
            Codes = dict:from_list(lists:map(fun({K, V}) ->
                                                     {V, K}
                                             end, dict:to_list(Symbols))),
            register(irc_lookup_server, spawn_link(?MODULE, loop, [Symbols, Codes]));
        _ ->
            ok
    end.

loop(Symbols, Codes) ->
    receive
        {get_symbol, Pid, Code} ->
            case dict:find(Code, Codes) of
                {ok, Value} ->
                    Pid ! {symbol, Value};
                error ->
                    Pid ! {irc_lookup_error, not_found}
            end,
            loop(Symbols, Codes);
        {get_code, Pid, Symbol} ->
            case dict:find(Symbol, Symbols) of
                {ok, Value} ->
                    Pid ! {code, Value};
                error ->
                    Pid ! {irc_lookup_error, not_found}
            end,
            loop(Symbols, Codes);            
        stop ->
            unregister(irc_lookup_server)
    end.

            
translate_command(Command) ->                    
    irc_lookup_server ! {get_symbol, self(), Command},
    receive
        {symbol, Value} ->
            Value;
        {irc_lookup_error, _} ->
            Command
    end.
            
shutdown() ->
    irc_lookup_server ! stop.

% Unit Tests
test() ->
    start(),
    "ERR_CHANOPRIVSNEEDED" = translate_command("482"),
    "PLOP" = translate_command("PLOP"),
    shutdown().
