-module(factoid).
-author("orbitz@ortdotlove.net").

% -export([test/0, test/1]).
-export([process/3,
         set/4,
         set_value/3,
         get/2,
         delete/2,
         search/2,
         replace_args/2]).

%%
% Records needed for factoids and inheritence
-record(factoid_data, {name, id, ro, revision, data}).

%%
% Factoids are an integral part of any IRC bot. This module provides an API for managing the factoid database
% as well as a function to handle it through a text interface.  The text interface looks like:
% Set: factoid is string
% Set: factoid is reply string
% Get: factoid [arg1 arg2 .. argn]
% Get-and-forward: who ! factoid [arg1 arg2 .. argn]
% Delete is handled through an actual factoid
%
% Set can also be done through a factoid in its 'raw' form:
% Set: set factoid string
% This corresponds to 'factoid is reply string'
%
% Factoids can be passed arguments, if the factoids is a function then the arguments are passed without being parsed to the function.
% For example: rot13 hello this is a test
% If 'rot13' is defined as a function, it will recieve the entire string "hello this is a test" as a parameter.
%
% On the other hand, arguments can be used to replace values in string factoids.
% For example: literal hello this is a test
% If 'literal' is defined as a string factoid that looks like "${1-}" it will evaluate to "hello this is a test". If it were defined as
% "${2-}" it would evaluate to "this is a test".
%
% A wiki factoid could be defined as:
% "http://en.wikipedia.org/wiki/${1}"
%
% In which case:
% wiki foo
% Would evaluate to:
% http://en.wikipedia.org/wiki/foo
%
% In the future, functions could be accessed through ${..} notation.  For example to get a random number we could do ${rand}, this functionality
% does not exist yet though.
%
% A note on arguments.  If a factoid requests arguments, for example like the wikipedia example, and an argument is not given, an error is raised.
% However, if arguments are given but the factoid does not require any, it is silently ignored.
%
% For example, if we define the factoid:
% oort is a great bot
%
% and access it as "oort what are you"  the result will be "oort is a great bot".  However, if we define oort as:
% oort is reply oort doesn't know what you mean by ${1-}
%
% and then simply access it as "oort" we will get an error saying not enough parameters were provided.
%
% This is probably confusing at first.
%
%
% Each factoid is both version controlled and broken into a heirarchy.
% When setting or getting a factoid a 'where' needs to also be provided, in most cases this will be a channel name.
% The first time a factoid is set the 'where' variable is not used and the factoid is defined as global (anywhere gets the same value).
% If the factoid is set to a new value, the 'where' value is then used and only when it is accessed in that 'where' will it get the new value.
%
% For example, if the factoid "oort" is defined as "oort is great" at first, then in #oort it is defiend as "oort is not great", whenever the
% "oort" factoid is accessed inside #oort, it will evaluate to "oort is not great", however when "oort" is accessed outside of #oort, it will be
% evaluate to "oort is great".  Everytime "oort" is redefined its revision number increases as well.  Currently there is no way to access
% different revision values, they exist for future use.

%%
% PUBLIC API FUNCTIONS

%%
% This takes a message as described above and returns a tuple
% {say, String} => Say the string back to whoever sent it
% ok => Do nothing
process(_Who, Where, Message) ->
    Tokens = tokenize_string(Message),
    case tokenized_to_action(Tokens) of
        {get, Factoid, []} ->
            {get, get(Factoid, Where), []};
        {get, Factoid, Args} ->
            {get, get(Factoid, Where), Args};
        {set, Factoid, Data} ->
            {set, set_value(Factoid, Where, {string, Data})};
        {get_who, Who, Factoid, []} ->
            {get_who, Who, get(Factoid, Where), []};
        {get_who, Who, Factoid, Args} ->
            {get_who, Who, get(Factoid, Where), Args};
        {error, Anything} ->
            {error, Anything}
    end.


%%
% This function sets data for a factoid.  This takes into acount
% a new revision number and if the 'where' value should be taken into
% account.  It also allows one to specify if the factoid is read only or not
set(FactoidRaw, Where, Readonly, Data) ->
    Factoid = string:to_lower(FactoidRaw),
    {atomic, Namelist} = p1_db:read(factoid_data, Factoid),
    {ok, Record} = create_record_from_data(Factoid, Where, Data, Readonly, Namelist),
    {ok, _} = insert_factoid(Record),
    {ok, Record}.

%%
% Same as set, but assumes Readonly to be false
set_value(FactoidRaw, Where, Data) ->
    set(FactoidRaw, Where, false, Data).

%%
% REturns {ok, Factoid} on success
% {error, factoid_not_found} if the factoid could not be found
get(FactoidRaw, Where) ->
    case p1_db:read(factoid_data, string:to_lower(FactoidRaw)) of
        {atomic, []} ->
            {error, factoid_not_found};
        {atomic, FactoidList} ->
            {ok, find_most_derived(FactoidList, create_heirarchy(Where))}
    end.

%%
% Returns ok on success
% {error, already_deleted} if the factoid has already been dleted
% {error, factoid_not_found} if it could not be found
delete(FactoidRaw, Where) ->
    case get(FactoidRaw, Where) of
        {ok, #factoid_data{data=deleted}} ->
            {error, already_deleted};
        {ok, #factoid_data{ro=false}} ->
            set_value(FactoidRaw, Where, deleted),
            ok;
        _ ->
            {error, factoid_not_found}
    end.

%%
% Takes a search string (currently just a string, will be regex in future)
% and returns a list of all those factoids active in the current Where
% that are not deleted that have the SearchString in the factoid name
% Returns a List.  All errors will happen throuhg a badmatch (such as mnesia dieing)
search(SearchStringRaw, Where) ->
    SearchString = string:to_lower(SearchStringRaw),
    {atomic, Keys} = p1_db:get_all_keys(factoid_data),
    %%
    % Get all the keys in the table and filter out only those where the search string
    % exists in it
    FKeys = lists:filter(fun(S) -> string:str(S, SearchString) /= 0 end, Keys),
    %%
    % Take all of the keys that match out search string, get the latest version from the Db
    % and remove all those factoids that are deleted
    Records = lists:filter(fun(R) -> R#factoid_data.data /= deleted end,
                           lists:map(fun(K) -> {ok, Record} = get(K, Where), Record end, FKeys)),
    %%
    % Just get the names from what is left over
    lists:map(fun(R) -> R#factoid_data.name end, Records).
                      


%%
% This takes a string and replaces arguments in it based on Args
% Args is a string as well, words can be access through ${#} where
% # is the word to replace.  This returns
% {ok, NewString} on success and
% {error, bad_index} => if a bad index is given
% {error, malformed_idx} => the index was nor properly formed
replace_args(String, Args) ->
    ArgIdx = lists:usort(extract_args(String)),
    catch(lists:foldl(fun(Elm, Acc) -> replace_single_arg(Elm, Acc, Args) end, String, ArgIdx)).

to_integer(String) ->
    case string:to_integer(String) of
        {error, no_integer} ->
            throw({error, malformed_idx});
        {Int, []} ->
            Int
    end.

replace_single_arg(Idx, String, Args) ->
    case string:sub_word(Args, to_integer(Idx)) of
        [] ->
            throw({error, bad_index});
        Word ->
            re:replace(String, "\\$\\{" ++ Idx ++ "\\}", Word, [{return, list}, global])
    end.

%%
% INTERNAL FUNCTIONS

insert_factoid(Record) ->
    p1_db:write(Record),
    {ok, Record}.


extract_args(String) ->
    {ok, RE} = regexp:parse("\\$\\{[^\\}]+\\}"),
    {match, Matches} = regexp:matches(String, RE),
    lists:map(fun({Start, Length}) ->
                      string:substr(String, Start + 2, Length - 3)
              end, Matches).

%%
% This takes a factoid, its origin, the value of it, if its read only, and
% most importantly a list of the other revisions of the factoids in the
% the database.
%
% If there are no factoids, a base revision is made and its id is all nil.
% If there are other factoids, the location in the heirarchy the factoid
% should live in is found and its revision number is incremented by 1
create_record_from_data(Name, _, Data, Readonly, []) ->
    {ok, #factoid_data{name=Name, revision=0, id={nil, nil}, data=Data, ro=Readonly}};
create_record_from_data(_, Where, Data, Readonly, CurrentList) ->
    Heir = create_heirarchy(Where),
    Last = find_most_derived(CurrentList, Heir),
    case Last#factoid_data.ro of
        false ->
            {ok, Last#factoid_data{revision=Last#factoid_data.revision + 1,
                                   id=create_new_id(Where, Last#factoid_data.id, Heir),
                                   ro=Readonly,
                                   data=Data}};
        true ->
            {error, factoid_ro, "Factoid is readonly"}
    end.

%%
% This finds the most derived factoid given the heirarchy
% as well as the latest revision
find_most_derived(FactoidList, Heirarchy) ->
    %%
    % We want to extract all those factoids that are in the heirarchy
    % and then sort them by revision, the last to the first
    [Last|_] = lists:sort(fun(X, Y) -> not (X#factoid_data.revision =< Y#factoid_data.revision) end,
                          lists:filter(fun(X) -> find_idx(X, Heirarchy) /= error end,
                                       FactoidList)),
    Last.

%%
% This takes the current id, the last id that is in the factoid list
% and the heirarchy to create the id and returns the next one in the heirarchy
create_new_id(Now, Last, Heirarchy) ->
    Pos = find_idx(Last, Heirarchy),
    %%
    % try to find the next in the heirarchy, if we are already
    % at the last element we will get an exception though
    % so return our current location
    try lists:nth(Pos + 1, Heirarchy) of
        Newid ->
            Newid
    catch
        _:_ ->
            Now
    end.

%%
% This compares a two entries in a heirarchy and
% returns eq, lt if H1 < H2, gt if H1 > H2
% It returns error otherwise
%% heirarchy_cmp(Heir, H1, H2) ->
%%     case {find_idx(H1, Heir), find_idx(H2, Heir)} of
%%         {P, Q} when is_integer(P), is_integer(Q), P < Q ->
%%             lt;
%%         {P, Q} when is_integer(P), is_integer(Q), P > Q ->
%%             gt;
%%         {P, Q} when is_integer(P), is_integer(Q), P == Q ->
%%             eq;
%%         _ ->
%%             error
%%     end.

%%
% Finds the index of an element in a list
% returns not_found if the elemnt cannot be found
find_idx(E, L) ->
    find_idx(E, L, 0).

find_idx(_E, [], _Idx) ->
    not_found;
find_idx(E, [L|_R], Idx) when E == L ->
    Idx;
find_idx(E, [_|R], Idx) ->
    find_idx(E, R, Idx + 1).
    

%%
% Given a channel and network this gives a list of the heirarchies
% available from the most general to the most specific
create_heirarchy({Chan, Net}) ->
    [{nil, nil}, {Chan, nil}, {Chan, Net}].


%%
% These are internal functions for parsing a message.
tokenized_to_action([{string, Factoid}]) ->
    {get, Factoid, []};
tokenized_to_action([{string, Factoid}, {string, Args}]) ->
    {get, Factoid, Args};
tokenized_to_action([{string, Factoid}, {op, 'is'}, {string, Rest}]) ->
    {set, Factoid, Factoid ++ " is " ++ Rest};
tokenized_to_action([{string, Factoid}, {op, 'is'}, {op, 'reply'}, {string, Rest}]) ->
    {set, Factoid, Rest};
tokenized_to_action([{string, Who}, {op, '!'}, {string, Factoid}]) ->
    {get_who, Who, Factoid, []};
tokenized_to_action([{string, Who}, {op, '!'}, {string, Factoid}, {string, Args}]) ->
    {get_who, Who, Factoid, Args};
tokenized_to_action(Anything) ->
    {error, Anything}.

%%
% tokenize_string takes a string and turns it into:
% factoid is string => [{string, "factoid"}, {op, 'is'}, {string, "string"}]
% Set: factoid is reply string => [{string, "factoid"}, {op, 'is'}, {op, 'reply'}, {string, "string"}]
% Get: factoid [arg1 arg2 .. argn] => [{string, "factoid"}, {string, "arg1 arg2 .. argn"}]
% Get-and-forward: who ! factoid [arg1 arg2 .. argn] => [{string, "who"}, {op, '!'}, {string, "factoid"}, {string, "arg1 arg2 .. argn"}]
tokenize_string(String) ->
    first_word(String).

first_word(String) ->
    case pop_first_word(String) of
        {Word, []} ->
            [{string, Word}];
        {Word, Rest} ->
            [{string, Word} | next_op(Rest)]
    end.

next_op(String) ->
    case pop_first_word(String) of
        {"is", []} ->
            [{op, 'is'}];
        {"is", Rest} ->
            [{op, 'is'} | next_op_or_word(Rest)];
        {"!", []} ->
            [{op, '!'}];
        {"!", Rest} ->
            case pop_first_word(Rest) of
                {Word, []} ->
                    [{op, '!'}, {string, Word}];
                {Word, LeftOver} ->
                    [{op, '!'}, {string, Word}, {string, LeftOver}]
            end;
        {_Word, _Rest} ->
            %%
            % It looks liek factoid argn ..
            [{string, String}]
    end.

next_op_or_word(String) ->
    case pop_first_word(String) of
        {"reply", []} ->
            [{op, 'reply'}];
        {"reply", Rest} ->
            [{op, 'reply'}, {string, Rest}];
        {_Word, _Rest} ->
            %%
            % It looks like factoid is string
            [{string, String}]
    end.


%% TODO
%% Fix this to use split_once
pop_first_word(String) ->
    case string:chr(String, $\s) of
        0 ->
            {string:strip(String, both), []};
        Idx ->
            {string:strip(string:substr(String, 1, Idx - 1), both), string:strip(string:substr(String, Idx + 1), left)}
    end.
