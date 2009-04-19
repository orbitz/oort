%%%-------------------------------------------------------------------
%%% File    : bot_manager.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : 
%%%
%%% Created : 23 Mar 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(bot_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([store/2, erase/1, fetch_name/1, fetch_pid/1, stop/0]).

-record(state, {names, pids}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{names=dict:new(), pids=dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, Name, Pid}, _From, #state{names=Names, pids=Pids}) ->
    {reply, ok, #state{names=dict:store(Name, Pid, Names), pids=dict:store(Pid, Name, Pids)}};
handle_call({unregister, Pid}, _From, #state{names=Names, pids=Pids}) when is_pid(Pid) ->
    Name = dict:fetch(Pid, Pids),
    {reply, ok, #state{names=dict:erase(Name, Names), pids=dict:erase(Pid, Pids)}};
handle_call({unregister, Name}, _From, #state{names=Names} = State) ->
    Pid = dict:fetch(Name, Names),
    handle_call({unregister, Pid}, _From, State);
handle_call({find_name, Pid}, _From, #state{pids=Pids} = State) ->
    {reply, dict:find(Pid, Pids), State};
handle_call({find_pid, Name}, _From, #state{names=Names} = State) ->
    {reply, dict:find(Name, Names), State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
store(Name, Pid) ->
    ok = gen_server:call(?MODULE, {register, Name, Pid}).

erase(Pid) when is_pid(Pid) ->
    ok = gen_server:call(?MODULE, {unregister, Pid});
erase(Name) ->
    ok = gen_server:call(?MODULE, {unregister, Name}).

fetch_name(Pid) ->
    gen_server:call(?MODULE, {find_name, Pid}).

fetch_pid(Name) ->
    gen_server:call(?MODULE, {find_pid, Name}).

stop() ->
    gen_server:cast(?MODULE, stop).
