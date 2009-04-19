%%%-------------------------------------------------------------------
%%% File    : bot_supervisor.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : 
%%%
%%% Created : 23 Mar 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(bot_supervisor).
-include("db.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_bot/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    {atomic, ChildSpecs} = create_childspec(),
    {ok,{{one_for_one,3,60}, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
create_childspec() ->
    F = fun(Bot, Acc) ->
                [{Bot#irc_bot_db.botname, {irc_bot, start_link, [rec_convert:convert(irc_bot_db, irc_bot, Bot)]},
                  transient, 2000, worker, [irc_bot]} | Acc]
        end,
    p1_db:foldl(F, [], irc_bot_db).

add_bot(Name) ->
    {atomic, [Bot]} = p1_db:read(irc_bot_db, Name),
    supervisor:start_child(?MODULE, {Name, {irc_bot, start_link, [rec_convert:convert(irc_bot_db, irc_bot, Bot)]},
                                              transient, 2000, worker, [irc_bot]}).
