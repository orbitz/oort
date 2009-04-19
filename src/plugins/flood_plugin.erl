%%%-------------------------------------------------------------------
%%% File    : flood_monitor.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : 
%%%
%%% Created : 10 Oct 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(flood_plugin).

-export([start/0, init/0, deps/0, loop/0]).


start() ->
    spawn(flood_plugin, init, []).

init() ->
    msg_dispatch:add([{bot_plugin, "PRIVMSG"}]),
    flood_policy:start_link(),
    flood_policy:add(privmsg, 15, 30),
    loop().

deps() ->
    [].

loop() ->
    receive
       {{bot_plugin, "PRIVMSG"}, {From, To, _Message}, Bot} ->
            case flood_policy:update(privmsg, {From, To, Bot}) of
                1 ->
                    irc_bot:say(Bot, "orbitz", lists:flatten(io_lib:format("Flood ~s ~s", [From, To])));
                _ ->
                    ok
            end
    end,
    flood_plugin:loop().
