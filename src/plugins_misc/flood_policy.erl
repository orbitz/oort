%%%-------------------------------------------------------------------
%%% File    : flood_policy.erl
%%% Author  : ortitz <orbitz@blong.orbitz>
%%% Description : 
%%%
%%% Created :  7 Oct 2006 by ortitz <orbitz@blong.orbitz>
%%%-------------------------------------------------------------------
-module(flood_policy).

-behaviour(gen_server).

%% API
-export([start_link/0, add/3, remove/1, update/2, stop/0, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {monitors}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #state{monitors=dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};
handle_call({add, Name, Number, Time}, _From, #state{monitors=Mon} = State) ->
    {reply, ok, State#state{monitors=dict:store(Name, {Number, Time, dict:new()}, Mon)}};
handle_call({remove, Name}, _From, #state{monitors=Mon} = State) ->
    {reply, ok, State#state{monitors=dict:erase(Name, Mon)}};
handle_call({update, Name, Who}, _From, #state{monitors=Mon} = State) ->
    {Number, Time, DName} = dict:fetch(Name, Mon),
    {NDName, Val} = update_user(Who, Time, DName),
    {reply, {ok, compare(Val, {Number, Time})},
             State#state{monitors=dict:store(Name, {Number, Time, NDName}, Mon)}}.
    

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {stop, invalid_call, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {stop, invalid_call, State}.

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
compare({S1, M1}, {S2, M2}) when S1/M1 > S2/M2 -> 1;
compare({S1, M1}, {S2, M2}) when S1/M1 < S2/M2 -> -1;
compare(_, _) -> 0.

update_user(Who, Time, Dict) ->
    Info = store_time(get_or_create_info(Who, Dict), Time),
    {dict:store(Who, Info, Dict), {length(Info), Time}}.

store_time(List, Time) when is_list(List) ->
    T = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    lists:filter(fun(Elm) ->
                         (Elm + Time) > T
                 end,
                 [T | List]).
    
get_or_create_any(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default()
    end.

get_or_create_info(User, Dict) ->
    get_or_create_any(User, Dict, fun() ->
                                          []
                                  end).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
add(Name, Number, Time) ->
    ok = gen_server:call(?MODULE, {add, Name, Number, Time}).

remove(Name) ->
    ok = gen_server:call(?MODULE, {remove, Name}).

%%--------------------------------------------------------------------
%% Func: update(Name, Who) -> Score
%% Description: Updates and scores a user.  If score is 1 then they are flooding
%%--------------------------------------------------------------------
update(Name, Who) ->
    {ok, Score} = gen_server:call(?MODULE, {update, Name, Who}),
    Score.

stop() ->
    ok = gen_server:call(?MODULE, stop).



%%--------------------------------------------------------------------
%% Func: test()
%% Description: Test cases
%%--------------------------------------------------------------------
test() ->
    start_link(),
    add(test, 2, 3),
    io:format("Score: ~w~n", [update(test, foo)]),
    stop().

    
    
