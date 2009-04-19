-module(rec_convert).
-author("orbitz@ortdotlove.net").

-include("db.hrl").
-include("irc.hrl").

-export([convert/3]).

convert(irc_bot_db, irc_bot, Rec) ->
    #irc_bot{botname=Rec#irc_bot_db.botname,
             nick=Rec#irc_bot_db.nick,
             realname=Rec#irc_bot_db.realname,
             servers=Rec#irc_bot_db.servers,
             channels=Rec#irc_bot_db.channels,
             password=Rec#irc_bot_db.password}.
