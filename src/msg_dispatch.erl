%%%-------------------------------------------------------------------
%%% File    : msg_dispatch.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : Handles dispatching messages to select receivers
%%%
%%% Created : 14 May 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(msg_dispatch).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add/1, remove/0, remove/1, dispatch/3, stop/0]).
-export([test/0]).

-record(state, {dispatch_map}).

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
    {ok, #state{dispatch_map=dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, Pid, Messages}, _From, #state{dispatch_map=Map} = State) when is_pid(Pid), is_list(Messages) ->
    F = fun(Msg, Dict) ->
                dict:append(Msg, Pid, Dict)
        end,
    {reply, ok, State#state{dispatch_map=lists:foldl(F, Map, Messages)}};
handle_call({remove, Pid, Messages}, _From, #state{dispatch_map=Map} = State) when is_pid(Pid), is_list(Messages) ->
    F = fun(Msg, Dict) ->
                case dict:find(Msg, Dict) of
                    {ok, Value} ->
                        dict:store(Msg, lists:filter(fun(Elm) -> Elm /= Pid end, Value), Dict);
                    _ ->
                        Dict
                end
        end,
    {reply, ok, State#state{dispatch_map=lists:foldl(F, Map, Messages)}};
handle_call({remove, Pid}, _From, State) ->
    handle_call({remove, Pid, dict:fetch_keys(State#state.dispatch_map)}, _From, State).


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({Message, Params, Extra}, #state{dispatch_map=Map} = State) ->
    case dict:find(Message, Map) of
        {ok, Pids} ->
            lists:foreach(fun(P) ->
                                  P ! {Message, Params, Extra} end,
                          Pids);
        _ ->
            ok
    end,
    {noreply, State}.

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
%%% Exported functions
%%--------------------------------------------------------------------

% Add messages for yourself to receive
add(Messages) when is_list(Messages) ->
    ok = gen_server:call(?MODULE, {add, self(), Messages}).

% Remove self from some mesages
remove(Messages) when is_list(Messages) ->
    ok = gen_server:call(?MODULE, {remove, self(), Messages}).

remove() ->
    ok = gen_server:call(?MODULE, {remove, self()}).

dispatch(Message, Params, Extra) ->
    ok = gen_server:cast(?MODULE, {Message, Params, Extra}).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Unit test
test() ->
    start_link(),
    add([boo]),
    dispatch(boo, [test, test1], []),
    receive
        Anything ->
            Anything = {boo, [test, test1], []}
    end,
    dispatch(bar, [], []),
    remove([boo]),
    dispatch(boo, [], []),
    receive
        Anything1 ->
            Anything1 = {boo, [], []}
    after
        10000 ->
            ok
    end,
    stop().
