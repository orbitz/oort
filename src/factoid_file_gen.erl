-module(factoid_file_gen).

-include("db.hrl").

-export([write_file/2, write_text_file/1, write_html_file/1]).

%% Need to export these so they work proppa
-export([write_text_factoid/3]).
-export([write_html_header/1, write_html_footer/2, write_html_factoid/3]).

%% Tests
-export([test/0]).


factoid_dict() ->
    F = fun(#factoid_data{name=Name} = Factoid, Dict) ->
                dict:append(Name, Factoid, Dict)
        end,
    p1_db:foldl(F, dict:new(), factoid_data).

write_text_file(Filename) ->
    write_file(Filename, {fun(Io) -> io:format(Io, "~20s ~3s ~s~n", ["Name", "Rev", "Description"]), {ok, undefined} end,
                          fun write_text_factoid/3,
                          fun(_Io, _Acc) -> ok end}).

write_html_file(Filename) ->
    write_file(Filename, {fun write_html_header/1,
                          fun write_html_factoid/3,
                          fun write_html_footer/2}).


write_file(Filename, {Header, Body, Footer}) ->
    {ok, Io} = file:open(Filename, [write]),
    {atomic, Data} = factoid_dict(),
    {ok, Acc} = Header(Io),
    {ok, NAcc} = write_data(Io, dict:to_list(Data), Body, Acc),
    Footer(Io, NAcc),
    file:close(Io),
    ok.


write_data(_Io, [], _Func, Acc) ->
    {ok, Acc};
write_data(Io, [{_Key, D} | Data], Func, Acc) ->
    {ok, NAcc} = Func(Io, D, Acc),
    write_data(Io, Data, Func, NAcc).

write_text_factoid(_Io, [], _Acc) ->
    {ok, undefined};
write_text_factoid(Io, [#factoid_data{name=Name, data={string, Str}, revision=Rev} | L], _Acc) ->
    io:format(Io, "~20s ~3B ~s~n", [Name, Rev, Str]),
    write_text_factoid(Io, L, undefined);
write_text_factoid(Io, [#factoid_data{name=Name, data=Data, revision=Rev} | L], _Acc) ->
    io:format(Io, "~20s ~3B ~w~n", [Name, Rev, Data]),
    write_text_factoid(Io, L, undefined);
write_text_factoid(_Io, Unknown, _Acc) ->
    throw({unknown_factoid, Unknown}).

%% Let's write an HTML file!!!
write_html_header(Io) ->
    io:put_chars(Io, "<html><head><title>Factoids</title></head><body><table><tr bgcolor='#0000FF'><th align='left'>Name</th><th align='left'>Revision</th><th align='left'>Description</th></tr>"),
    {ok, white}.

write_html_footer(Io, _Acc) ->
    io:put_chars(Io, "</table></body></html>").

write_html_factoid(_Io, [], Acc) ->
    {ok, Acc};
write_html_factoid(Io, [D | L], white) ->
    write_html_row(Io, D, "#FFFFFF"),
    write_html_factoid(Io, L, blue);
write_html_factoid(Io, [D | L], blue) ->
    write_html_row(Io, D, "#00FFFF"),
    write_html_factoid(Io, L, white);
write_html_factoid(_Io, Unknown, _Acc) ->
    throw({unknown_factoid, Unknown}).

write_html_row(Io, #factoid_data{name=Name, data={string, String}, revision=Rev}, Color) ->
    io:format(Io, "<tr bgcolor='~s'><td align='left'>~s</td><td align='left'>~B</td><td align='left'>~s</td></tr>~n", [Color, Name, Rev, String]);
write_html_row(Io, #factoid_data{name=Name, data=Data, revision=Rev}, Color) ->
    io:format(Io, "<tr bgcolor='~s'><td align='left'>~s</td><td align='left'>~B</td><td bgcolor='#FF0000' align='left'>~w</td></tr>~n", [Color, Name, Rev, Data]).


%% Tests
test() ->
    factoid_dict().
