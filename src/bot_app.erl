%%%-------------------------------------------------------------------
%%% File    : bot_app.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : 
%%%
%%% Created : 23 Mar 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(bot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-record(state, {plugins}).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    p1_db:start(),
    irc_lookup:start(),
    bot_manager:start_link(),
    msg_dispatch:start_link(),
    p1_db:wait_for_tables(),
    Plugins = plugin_manager:load(),
    case bot_supervisor:start_link() of
        {ok, Pid} ->
            {ok, Pid, #state{plugins=Plugins}};
        Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(#state{plugins=Plugins} = _State) ->
    plugin_manager:unload(Plugins),
    irc_lookup:stop(),
    bot_manager:stop(),
    msg_dispatch:stop(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
