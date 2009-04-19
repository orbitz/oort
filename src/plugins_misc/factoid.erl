-module(factoid).
-author("orbitz@ortdotlove.net").

-include("db.hrl").

-export([test/0, test/1]).
-export([process/3, factoid_set/4, factoid_set_value/3, factoid_get/2, delete/2]).

process(Who, Where, Message) ->
    {ok, Scanned, _} = scan_string(Message),
    {ok, Parsed} = factoid_parser:parse(Scanned),
    handle_factoid(Who, Where, Parsed).

handle_factoid(_, Where, {factoid_read, {factoid, _, Factoid}, {string, _, Leftover}}) ->
    {get, factoid_get(string:to_lower(Factoid), Where), Leftover};
handle_factoid(_, Where, {factoid_read_who, {factoid, _, Who}, {factoid, _, Factoid}, {string, _, Leftover}}) ->
    {get_who, Who, factoid_get(http_util:to_lower(Factoid), Where), Leftover};
handle_factoid(_, Where, {factoid_set,
                            {factoid, _, Factoid},
                            'is',  % This is for arguments which we are currently ignoring
                            {string, _, String}}) ->
    factoid_set_value(Factoid, Where, {string, io_lib:format("~s is ~s", [Factoid, String])});
handle_factoid(_, Where, {factoid_set,
                          {factoid, _, Factoid},
                          'reply',
                          {string, _, String}}) ->
    factoid_set_value(Factoid, Where, {string, String}).


factoid_set_value(Factoid, Where, Value) ->
    {set, factoid_set(string:to_lower(Factoid), Where, false, Value)}.


factoid_set(Name, Where, Readonly, Data) ->
    {atomic, Namelist} = p1_db:read(factoid_data, Name),
    {ok, Record} = create_record_from_data(Name, Where, Data, Readonly, Namelist),
    insert_factoid(Record).

factoid_get(Name, Where) ->
    {atomic, Namelist} = p1_db:read(factoid_data, Name),
    get_best_match(Namelist, Where).

delete(Factoid, Where) ->
    case factoid_get(Factoid, Where) of
        {ok, #factoid_data{data=deleted}} ->
            {error, {factoid_deleted, Factoid}};
        {ok, #factoid_data{ro=false}} ->
            factoid_set_value(Factoid, Where, deleted),
            ok;
        _ ->
            {error, {factoid_not_found, Factoid}}
    end.
  
get_best_match([], _) ->
    {error, factoid_not_found};
get_best_match(Flist, Where) ->
    Heir = create_heirarchy(Where),
    {ok, First, Rest} = find_first_in_heirarchy(Flist, Heir),
    {ok, Derived} = iterate_factoids_derive(Rest, [First], Heir),
    {ok, get_last_revision(Derived)}.
    
    
create_record_from_data(Name, _, Data, Readonly, []) ->
    {ok, #factoid_data{name=Name, revision=0, id={nil, nil}, data=Data, ro=Readonly}};
create_record_from_data(_, Where, Data, Readonly, Currentlist) ->
    Heir = create_heirarchy(Where),
    {ok, First, Rest} = find_first_in_heirarchy(Currentlist, Heir),
    {ok, Derived} = iterate_factoids_derive(Rest, [First], Heir),
    Last = get_last_revision(Derived),
    case Last#factoid_data.ro of
        false ->
            {ok, Last#factoid_data{revision=(Last#factoid_data.revision + 1),
                                   id=find_new_id(Where, Last#factoid_data.id, Heir),
                                   ro=Readonly,
                                   data=Data}};
        true ->
            {error, factoid_ro, "Factoid is readonly"}
    end.

insert_factoid(Record) ->
    p1_db:write(Record),
    {ok, Record}.

% Helper functions
get_last_revision([H | Rest]) ->
    get_last_revision(Rest, H).

get_last_revision([H | Rest], Latest) when Latest#factoid_data.revision > H#factoid_data.revision ->
    get_last_revision(Rest, Latest);
get_last_revision([H | Rest], Latest) when Latest#factoid_data.revision < H#factoid_data.revision ->
    get_last_revision(Rest, H);
get_last_revision([], Latest) ->
    Latest.

find_new_id(Now, Curr, Heir) ->
    Pos = find(Curr, Heir),
    try lists:nth(Pos + 1, Heir) of
        Newid ->
            Newid
    catch
        % This means taht nth failed so we are out of range
        _:_ ->
            Now
    end.
    
create_heirarchy({Chan, Net}) ->
    [{nil, nil}, {Chan, nil}, {Chan, Net}].

% Find the first element int he list which corresponds to an element in teh heirarchy
find_first_in_heirarchy([H | Rest], Heir) ->
    try find(H#factoid_data.id, Heir) of
        _ ->
            {ok, H, Rest}
    catch
        {elm_not_found, _} ->
            find_first_in_heirarchy(Rest, Heir)
    end;
find_first_in_heirarchy([], _) ->
    {error, not_found, "No tree found in heirarchy"}.

% Find the most derived id in the list according to the passed heirarchy
iterate_factoids_derive([H | Rest], Best, Heir) ->
    B = hd(Best),
    try id_compare(B#factoid_data.id, H#factoid_data.id, Heir) of
        -1 ->
            iterate_factoids_derive(Rest, [H], Heir);
        0 ->
            iterate_factoids_derive(Rest, [H | Best], Heir);
        _ ->
            iterate_factoids_derive(Rest, Best, Heir)
    catch
        {elm_not_found, _} ->
            iterate_factoids_derive(Rest, Best, Heir)
    end;
iterate_factoids_derive([], Best, _) ->
    {ok, Best}.

% This is the comparator we'll use to find the most derived. the heirarchy looks like:
% {nil, nil} < {Channel, nil} < {Channel, Network}
% This compares Id1 and Id2, returns 0 if equal, 1 if Id1 > Id2, -1 if Id1 < Id2
% id_compare assumes that the Channel names and network, if present, or the same, or they are nil.
% This will be changed to use the heirarchy table eventually
% Heirarchy is the means to compare the two
id_compare(Id1, Id2, Heirarchy) ->
    case find(Id1, Heirarchy) - find(Id2, Heirarchy) of
        Diff when Diff < 0 ->
            -1;
        Diff when Diff > 0 ->
            1;
        _ ->
            0
    end.


find(Elm, List) ->
    find(Elm, List, 1).

find(Elm, [H | Rest], Pos) ->
    case H of
        Elm ->
            Pos;
        _ ->
            find(Elm, Rest, Pos + 1)
    end;
find(_, [], _) ->
    throw({elm_not_found, "Id is not part of the heiarchy"}).
    

% Factoid syntax
% To add a factoid: factoid [on id] is data
% To delete: remove [all] factoid
% to get a factoid: factoid
scan_string(String) ->
    {Factoid, Newstring} = pop_first_word(String),
    {Word, Nstr} = pop_first_word(Newstring),
    scan_string(Word, Nstr, [{factoid, 1, Factoid}]).

scan_string("is", String, Accum) ->
    {NAccum, NStr} = case pop_first_word(String) of
                         {"reply", NString} ->
                             {[{'reply', 1} | [{'is', 1} | Accum]], NString};
                         _ ->
                             {[{'is', 1} | Accum], String}
                     end,
    {ok, lists:reverse([{string, 1, NStr} | NAccum]), 1};
%% scan_string("on", String, Accum) ->
%%     {Id, Newstring} = pop_first_word(String),
%%     {Nextword, Finalstring} = pop_first_word(Newstring),
%%     scan_string(Nextword, Finalstring, [{string, 1, Id} | [{'on', 1} | Accum]]);
scan_string("!", String, Accum) ->
    scan_string(">", String, Accum);
scan_string(">", String, Accum) ->
    {Fact, Nstr} = pop_first_word(String),
    {ok, Accum ++ [{'>', 1}, {factoid, 1, Fact}, {string, 1, Nstr}], 1};
scan_string(Word, String, Accum) ->
    {ok, lists:reverse([{string, 1, string:strip(Word ++ " " ++ String)} | Accum]), 1}.

%% TODO
%% Fix this to use split_once
pop_first_word(String) ->
    case string:chr(String, $\s) of
        0 ->
            {String, []};
        Idx ->
            {string:substr(String, 1, Idx - 1), string:substr(String, Idx + 1)}
    end.

% Unit tests
test() ->
    {scan_string("blah is good"), scan_string("is good"), scan_string("is"),
     scan_string("blah good"), scan_string("blah all foo"), scan_string("blah on ##c++ is foo"),
    scan_string("blah on ##c++ is good on ##c++"), scan_string("blah is reply good"),
    scan_string("blah > foo bar")}.

test(parser) ->
    {factoid_parser:parse(element(2, scan_string("blah is good"))),
     factoid_parser:parse(element(2, scan_string("blah is reply good"))),
     factoid_parser:parse(element(2, scan_string("blah"))),
     factoid_parser:parse(element(2, scan_string("blah good bad"))),
     factoid_parser:parse(element(2, scan_string("blah > foo")))
    };
test(scan_string) ->
    scan_string("test is TEST");
test(compare) ->
    H = [{nil, nil}, {foo, nil}, {foo, bar}],
    {0, -1, 1} = {id_compare({nil, nil}, {nil, nil}, H), id_compare({nil, nil}, {foo, nil}, H), id_compare({foo, bar}, {foo, nil}, H)}.
