%%%-------------------------------------------------------------------
%%% File    : irc_bot2.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : Irc bot gen server
%%%
%%% Created : 23 Mar 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(irc_bot).

-include("db.hrl").
-include("irc.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API exports
-export([add_bot/1, say/3, stop/2]).
-export([get_irclib/1, get_nick/1]).

-export([test/0, test1/0]).

-record(state, {nick, dict, state, irclib, pong_timeout=undefined, connection_timeout}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Client]) ->
    {ok, Ref} = timer:send_interval(connection_timeout(), timed_ping),
    msg_dispatch:add(["PRIVMSG", "PING", "PING"]),
    ok = bot_manager:store(Client#irc_bot.botname, self()),
    {ok, Pid} = irc_lib:start_link(
                  #irc_client_info{nick=Client#irc_bot.nick,
                                   realname=Client#irc_bot.realname,
                                   servers=Client#irc_bot.servers,
                                   password=Client#irc_bot.password}),
    ok = irc_lib:connect(Pid, fun_wrapper(fun connected/2)),
    {ok, #state{nick=Client#irc_bot.nick,
                irclib=Pid,
                dict=dict:from_list([{join_on_connect, Client#irc_bot.channels}]),
                connection_timeout=Ref,
                state=connecting}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_nick, _From, #state{nick=Nick} = State) ->
    {reply, {ok, Nick}, State};
handle_call(get_irclib, _From, #state{irclib=Irclib} = State) ->
    {reply, {ok, Irclib}, State};
handle_call({say, Where, What}, _From, #state{irclib=Irclib} = State) ->
    irc_lib:say(Irclib, Where, What),
    {reply, ok, State};
handle_call({stop, Message}, _From, #state{irclib=Irclib} = State) ->
    irc_lib:quit(Irclib, Message),
    irc_lib:stop(Irclib),
    {stop, stop, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(irc_connected, #state{dict=Dict, irclib=Irclib} = State) ->
    % Do connect stuff
    join_channels(Irclib, dict:fetch(join_on_connect, Dict)),
    {noreply, State#state{state=idle}};
handle_cast({stop, _}, #state{state=connecting} = State) ->
    {stop, stop, State};
handle_cast({irc_message, {_, "PONG", _}}, #state{state=pong, pong_timeout=Ref} = State) ->
    {ok, cancel} = timer:cancel(Ref),
    {noreply, State#state{state=idle, pong_timeout=undefined}};
handle_cast(irc_closed, #state{irclib=Irclib} = State) ->
    irc_lib:connect(Irclib),
    {noreply, State#state{state=connecting}};
handle_cast({irc_message, {_, "PING", [Server]}}, #state{irclib=Irclib} = State) ->
    irc_lib:pong(Irclib, Server),
    {noreply, State};
handle_cast({say, Where, What}, #state{irclib=Irclib} = State) ->
    irc_lib:say(Irclib, Where, What),
    {noreply, State};
% Catch all
handle_cast({irc_message, _}, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({"PRIVMSG", Msg, Bot}, #state{irclib=Irclib} = State) ->
    %%io:format("~s ~s ~s~n", Msg),
    case Bot of
        Irclib ->
            msg_dispatch:dispatch({bot_plugin, "PRIVMSG"}, Msg, self());
        _ ->
            ok
    end,
    {noreply, State};
handle_info({"PING", [Server], Bot}, #state{irclib=Irclib} = State) ->
    case Bot of
        Irclib -> irc_lib:pong(Irclib, Server);
        _ -> ok
    end,
    {noreply, State};
handle_info({"PONG", _Any, Bot}, #state{irclib=Irclib, state=pong, pong_timeout=Ref} = State) ->
    case Bot of
        Irclib -> {ok, cancel} = timer:cancel(Ref);
        _ -> ok
    end,
    {noreply, State#state{state=idle, pong_timeout=undefined}};

handle_info(timedout_ping, #state{state=pong} = State) ->
    %%irc_lib:disconnect(Irclib),
    %%irc_lib:connect(Irclib, fun_wrapper(fun connected/2)),
    {noreply, State};
handle_info(timed_ping, #state{irclib=Irclib} = State) ->
    irc_lib:ping(Irclib),
    {ok, Ref} = timer:send_after(pong_timeout(), timedout_ping),
    {noreply, State#state{state=pong, pong_timeout=Ref}}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{connection_timeout=Ref}) ->
    timer:cancel(Ref),
    bot_manager:erase(self()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

join_channels(Bot, [Channel | Rest]) ->
    irc_lib:join(Bot, Channel, fun_wrapper(fun joined_channel/2)),
    join_channels(Bot, Rest);
join_channels(_, []) ->
    ok.

connection_timeout() ->
    % 10 minutes
    600000.

pong_timeout() ->
    % 10 Seconds
    10000.


% -------------------------------------------------------------
% Functions used to interact with the bot
say(Bot, Where, What) ->
    gen_server:cast(Bot, {say, Where, What}).

stop(Bot, Message) ->
    gen_server:call(Bot, {stop, Message}).

get_irclib(Name) ->
    {ok, Pid} = bot_manager:fetch_pid(Name),
    gen_server:call(Pid, get_irclib).

get_nick(Bot) when is_pid(Bot) ->
    {ok, Nick} = gen_server:call(Bot, get_nick),
    Nick.

% -------------------------------------------------------------
% Functions for manipulating the bot database
add_bot({Botname, Nick, Realname, Servers, Channels, Password}) ->
    p1_db:insert_row(#irc_bot_db{botname=Botname, nick=Nick, realname=Realname, servers=Servers, channels=Channels, password=Password}).


%% Callbacks
connected(Self, ok) ->
    gen_server:cast(Self, irc_connected);
connected(_Self, _) ->
    throw(bad_match).

joined_channel(_Self, {ok, _}) ->
    ok;
joined_channel(_Self, _) ->
    throw(bad_match).


fun_wrapper(Fun) ->
    Me = self(),
    fun (Res) ->
            Fun(Me, Res)
    end.


test() ->
    p1_db:start(),
    irc_lookup:start(),
    bot_manager:start_link(),
    msg_dispatch:start_link(),
    p1_db:wait_for_tables(),
    _Plugins = plugin_manager:load(),
    {atomic, [Bot]} = p1_db:read(irc_bot_db, opn),
    B1 = rec_convert:convert(irc_bot_db, irc_bot, Bot),
    {ok, _Pid} = irc_bot:start_link(B1),
    receive
        _ ->
             ok
    after
        100000 ->
            ok
    end.

test1() ->
    p1_db:start(),
    irc_lookup:start(),
    bot_manager:start_link(),
    msg_dispatch:start_link(),
    p1_db:wait_for_tables(),
    _Plugins = plugin_manager:load(),
    try
        bot_supervisor:start_link()
    catch
        A:B ->
            io:format("exception~n~w:~w~n", [A, B]),
            io:format("~w~n", [erlang:get_stacktrace()])
    end.
            
