%%%-------------------------------------------------------------------
%%% File    : irc_lib.erl
%%% Author  : orbitz <orbitz@osx.local>
%%% Description : Handles irc lib functions
%%%
%%% Created :  1 Jun 2008 by orbitz <orbitz@osx.local>
%%%-------------------------------------------------------------------
-module(irc_lib).

-behaviour(gen_server).

-include("irc.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% API Functions
-export([whois/3, join/3, stop/1, quit/1, quit/2, say/3, pong/2, connect/2, disconnect/1, ping/1]).
-export([decode_mask/1]).

%% This should only be used by someone operating inside the bots process (handlers need to do this)
-export([send_command/2]).

%% Random useful function
%% These should be moved into another lib really
-export([split_once/2]).

-record(state, {sock,       % The socket
                client,     % Dictionary containing client data
                state=idle, % The status of the process
                cmd_queue,  % Queue of commands to run
                cmd_curr    % Current command being run (same as the head of cmd_queue)
               }).


                      
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Client) ->
    irc_lookup:start(),
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
    Dict = dict:from_list([
                           {nick, Client#irc_client_info.nick},
                           {realname, Client#irc_client_info.realname},
                           {servers, Client#irc_client_info.servers},
                           {password, Client#irc_client_info.password}]),
    {ok, #state{client=Dict, sock=undefined, state=disconn, cmd_queue=queue:new(), cmd_curr=undefined}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({irc_connect, Command}, _From, #state{client=Client, state=disconn, cmd_queue=Queue} = State) ->
    {ok, Sock} = connect_to_server(dict:fetch(servers, Client)),
    {reply, ok, State#state{sock=Sock,
                            state=connecting,
                            cmd_queue=queue:snoc(Queue, Command),
                            cmd_curr=Command}};
handle_call({irc_send_command, #irc_command{command={"PING", []}}}, _From, #state{sock=Sock, client=Client} = State) ->
    send_command(Sock, {"PING", [dict:fetch(nick, Client)]}),
    {reply, ok, State};
handle_call({irc_send_command, #irc_command{handler=undefined, command=Command}}, _From, #state{sock=Sock} = State) ->
    send_command(Sock, Command),
    {reply, ok, State};
handle_call({irc_send_command, Command}, _From, State) ->
    {reply, ok, queue_command(Command, State)};
handle_call(irc_disconnect, _From, #state{sock=Sock} = State) ->
    ok = gen_tcp:close(Sock),
    {reply, ok, State#state{sock=undefined, state=disconn}}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, State) ->
    %%io:format("~s~n", [Data]),
    inet:setopts(Sock, [{active, once}]),
    {Prefix, Command, Args} = scan_string(Data),
    {noreply, handle_data(State, {Prefix, irc_lookup:translate_command(Command), Args})};
handle_info({tcp_closed, Sock}, State) ->
    inet:setopts(Sock, [{active, once}]),
    {stop, {tcp_closed, Sock}, State#state{state=disconn}};
handle_info({tcp_error, Sock, Reason}, State) ->
    inet:setopts(Sock, [{active, once}]),
    {stop, {tcp_error, Sock, Reason}, State#state{state=disconn}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



handle_data(#state{cmd_curr=undefined} = State, Msg) ->
    dispatch_msg(Msg),
    State;
handle_data(#state{cmd_curr=Cmd, client=Client, sock=Sock, cmd_queue=Queue} = State, Msg) ->
    #irc_command{handler=Handler} = Cmd,
    case Handler(Sock, Client, Cmd, Msg) of
        ok ->
            {_Cmd, Q} = queue:out(Queue),
            send_next_queue_command(State#state{cmd_queue=Q, cmd_curr=undefined});
        dispatch_msg ->
            %% The handler wants this message to be dispatched as well as continue
            %% getting the messages
            dispatch_msg(Msg),
            State;
        ok_dispatch_msg ->
            %% This situation is for when a handler isn't sure when it will end
            %% until it gets a message that it doesn't recognize
            %% So this lets us know that it is done but to dispatch the message as well
            dispatch_msg(Msg),
            {_Cmd, Q} = queue:out(Queue),
            send_next_queue_command(State#state{cmd_queue=Q, cmd_curr=undefined});
        NewState ->
            State#state{cmd_curr=NewState}
    end.
    



dispatch_msg({From, "PRIVMSG", [To, Msg]}) ->
    msg_dispatch:dispatch("PRIVMSG", {From, To, Msg}, self());
dispatch_msg({From, "MODE", [Who | Opts]}) ->
    msg_dispatch:dispatch("MODE", {From, Who, Opts}, self());
dispatch_msg({From, "KICK", [Channel, User, Comment]}) ->
    msg_dispatch:dispatch("KICK", {From, Channel, User, Comment}, self());
dispatch_msg({From, "KICK", [Channel, User]}) ->
    msg_dispatch:dispatch("KICK", {From, Channel, User}, self());
dispatch_msg({_, "PING", Who}) ->
    msg_dispatch:dispatch("PING", Who, self());
dispatch_msg({_, "PONG", Any}) ->
    msg_dispatch:dispatch("PONG", Any, self());
dispatch_msg({_From, _Val, _Args}) ->
    %% io:format("Unknwon message to handle ~s~n", [Val]).
    ok.


% This will attempt to connect to each of the IRC servers on the list until one of them succeeds, if all of them fail it
% will fail out.

% Create a connection to an IRC server and return the socket
connect_server_port(Server, Port) ->
    gen_tcp:connect(Server, Port, [list, {active, once}, {packet, line}]).

connect_to_server([]) ->
    fail;
connect_to_server([{Server, Port} | Serverlist]) ->
    % This is horribly broken and should be done in handle_info or handle_call whatever
    case connect_server_port(Server, Port) of
        {ok, Sock} ->
            {ok, Sock};
        _ ->
            connect_to_server(Serverlist)
    end.



send_command(Sock, {Command, Params})  ->
    ok = gen_tcp:send(Sock, lists:foldl(
                              fun(Elm, Acc) ->
                                      Acc ++ " " ++ Elm
                              end, "", [Command | Params]) ++ "\r\n");
send_command(Sock, {Command}) ->
    ok = gen_tcp:send(Sock, Command ++ "\r\n");
send_command(Sock, [Command | Rest]) ->
    send_command(Sock, Command),
    send_command(Sock, Rest);
send_command(_, []) ->
    ok.



% Queue stuff
%% This queue's a command, and if it is the only command in there sends it to be processed
queue_command(Command, #state{cmd_queue=Queue} = State) ->
    case queue:is_empty(Queue) of
        true ->
            send_next_queue_command(State#state{cmd_queue=queue:snoc(Queue, Command)});
        false ->
            State#state{cmd_queue=queue:snoc(Queue, Command)}
    end.

send_next_queue_command(#state{sock=Sock, cmd_queue=Queue} = State) ->
    % Commands should be of the form {Type, [Args]}
    case queue:is_empty(Queue) of
        true ->
            State;
        false ->
            #irc_command{command=Command} = queue:head(Queue),
            send_command(Sock, Command),
            State#state{cmd_curr=queue:head(Queue)}
    end.



% Parsing functions
split_once(String, Char) ->
    case string:chr(String, Char) of
        0 ->
            {"", String};
        Idx ->
            {string:substr(String, 1, Idx - 1), string:substr(String, Idx + 1)}
    end.

parse_args(String) ->
    parse_args(String, string:str(String, " :")).

parse_args(String, 0) ->
    string:tokens(String, " ");
parse_args(String, Idx) ->
    {S, Trail} = {string:substr(String, 1, Idx - 1), string:substr(String, Idx + 2)},
    Args = string:tokens(S, " "),
    Args ++ [Trail].

strip_lineend(Data) ->
    strip_lineend_(lists:reverse(Data)).

strip_lineend_([$\n | Data]) ->
    strip_lineend_(Data);
strip_lineend_([$\r | Data]) ->
    strip_lineend_(Data);
strip_lineend_(Data) ->
    lists:reverse(Data).

% Parse a line up
scan_string([$: | Data]) ->
    {Prefix, Temp} = split_once(Data, $\s),
    {_, Command,  Args} = scan_string(Temp),
    {Prefix, Command, Args};
scan_string(Data) ->
    [Command | Args] = parse_args(strip_lineend(Data)),
    {"", Command, Args}.
   




%% ----------------------------------
%% Public interface
%% ---------------------------------
send_client_command(Irclib, Command) ->
    gen_server:call(Irclib, {irc_send_command, Command}).


% Functions used to send various command sto the client
pong(Irclib, Server) ->
    send_client_command(Irclib, #irc_command{state=undefined,
                                             func=undefined,
                                             command={"PONG", [Server]},
                                             handler=undefined}).


ping(Irclib) ->
    send_client_command(Irclib, #irc_command{state=undefined,
                                             func=undefined,
                                             command={"PING", []},
                                             handler=undefined}).

whois(Irclib, Who, Fun) ->
    send_client_command(Irclib, #irc_command{state={whois, []},
                                             func=Fun,
                                             command={"WHOIS", [Who]},
                                             handler=fun irc_handler:handle_whois/4}).


join(Irclib, {Channel, Pass}, Fun) ->
    send_client_command(Irclib, #irc_command{state={join, []},
                                             func=Fun,
                                             command={"JOIN", [Channel, Pass]},
                                             handler=fun irc_handlers:handle_join/4});
join(Irclib, Channel, Fun) when is_list(Channel) ->
    send_client_command(Irclib, #irc_command{state={join, []},
                                             func=Fun,
                                             command={"JOIN", [Channel]},
                                             handler=fun irc_handlers:handle_join/4}).


quit(Irclib) ->
    send_client_command(Irclib, #irc_command{state=undefined,
                                             func=undefined,
                                             command={"QUIT", []},
                                             handler=undefined}).

quit(Irclib, Message) ->
    send_client_command(Irclib, #irc_command{state=undefined,
                                             func=undefined,
                                             command={"QUIT", [":" ++ Message]},
                                             handler=undefined}).

stop(Irclib) ->
    gen_server:cast(Irclib, irc_stop).

say(Irclib, Where, What) ->
    send_client_command(Irclib, #irc_command{state=undefined,
                                             func=undefined,
                                             command={"PRIVMSG", [Where, ":" ++ What]},
                                             handler=undefined}).

connect(Irclib, Fun) ->
    %% The 5 second timeout is a bit too short for us
    gen_server:call(Irclib, {irc_connect, #irc_command{state=connecting,
                                                       func=Fun,
                                                       command=irc_connect,
                                                       handler=fun irc_handlers:handle_connect/4}},
                   30000).

disconnect(Irclib) ->
    gen_server:call(Irclib, irc_disconnect).

% ----------------------------------------------------------------------------------------------------
% Parse out the various information from a mask
% ----------------------------------------------------------------------------------------------------
decode_mask(Mask) ->
    decode_mask(nick, Mask, {"", "", ""}).

decode_mask(nick, [$! | Rest], Info) ->
    decode_mask(ident, Rest, Info);
decode_mask(nick, [C | Rest], {Nick, Ident, Host}) ->
    decode_mask(nick, Rest, {[ C | Nick], Ident, Host});
decode_mask(nick, [], _) ->
    {error, bad_mask};
decode_mask(ident, [$@ | Rest], Info) ->
    decode_mask(host, Rest, Info);
decode_mask(ident, [C | Rest], {Nick, Ident, Host}) ->
    decode_mask(ident, Rest, {Nick, [C | Ident], Host});
decode_mask(ident, [], _) ->
    {error, bad_mask};
decode_mask(host, [C | Rest], {Nick, Ident, Host}) ->
    decode_mask(host, Rest, {Nick, Ident, [C | Host]});
decode_mask(host, [], {Nick, Ident, Host}) ->
    {ok, lists:reverse(Nick), lists:reverse(Ident), lists:reverse(Host)}.
