%%%-------------------------------------------------------------------
%%% File    : plugin_manager.erl
%%% Author  : orbitz <orbitz@osx.myhome.westell.com>
%%% Description : 
%%%
%%% Created : 28 Feb 2007 by orbitz <orbitz@osx.myhome.westell.com>
%%%-------------------------------------------------------------------
-module(plugin_manager).

-include("db.hrl").

%% API
-export([load/0, unload/1, add/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: load
%% Description: load's all of the plugins in the table
%%--------------------------------------------------------------------
load() ->
    lists:foldl(fun load_pl/2, [], [factoid_plugin]).


%%--------------------------------------------------------------------
%% Function: unload
%% Description: unload the plugins
%%--------------------------------------------------------------------
unload(Plugins) ->
    lists:map(fun(X) ->
                       X:stop()
              end, Plugins).

%%--------------------------------------------------------------------
%% Function: load_pl
%% Description: loads a specific plugin
%%--------------------------------------------------------------------
load_pl(Elm, Plugins) ->
    case lists:member(Elm, Plugins) of
        true ->
            Plugins;
        false ->
            p1_db:foldl(fun load_pl/2, Plugins, Elm:deps()),
            Elm:start(),
            [Elm | Plugins]
    end.
    
%%--------------------------------------------------------------------
%% Function: add
%% Description: adds a plugin to the list
%%--------------------------------------------------------------------
add(Module) ->
    p1_db:insert_row({plugin_record, Module}).

%%====================================================================
%% Internal functions
%%====================================================================
