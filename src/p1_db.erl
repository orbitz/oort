-module(p1_db).
-author("orbitz@ortdotlove.net").

-include("db.hrl").

-export([create_tables/0, start/0, stop/0, insert_row/1, select/2, foldl/3, write/1, wait_for_tables/0, create_database/0, read/2, transaction/1]).

start() ->
    ok = mnesia:start().

create_database() ->
    mnesia:create_schema([node()]),
    start(),
    create_tables().

create_tables() ->
    mnesia:create_table(irc_bot_db, [{disc_copies, [node()]}, {attributes, record_info(fields, irc_bot_db)}]),
    mnesia:create_table(relay_client, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, relay_client)}]),
    mnesia:create_table(factoid_tree, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, factoid_tree)}]),
    mnesia:create_table(plugin_record, [{disc_copies, [node()]}, {record_name, plugin_record}, {type, set}]),
    mnesia:create_table(factoid_data, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, factoid_data)}]).


insert_row(Row) ->
    F = fun() -> 
                mnesia:write(Row)
        end,
    transaction(F).

select(Tag, Matchspec) ->
    F = fun() ->
                mnesia:select(Tag, Matchspec)
        end,
    transaction(F).

foldl(Func, Acc0, Tab) ->
    F = fun() ->
                mnesia:foldl(Func, Acc0, Tab)
        end,
    transaction(F).

write(Data) ->
    F = fun() ->
                mnesia:write(Data)
        end,
    transaction(F).

read(Tab, Key) ->
    F = fun() ->
                mnesia:read(Tab, Key, read)
        end,
    transaction(F).

transaction(Fun) ->
    mnesia:transaction(Fun).

wait_for_tables() ->
    mnesia:wait_for_tables([irc_bot_db], infinity).

stop() ->
    mnesia:stop().

