#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin/

load_trie(FilePath) ->
    {ok, File} = file:open(FilePath, [read]),
    io:format("Loading words...~n"),
    Trie = fill_trie(File, 1, ternary_trie:new()),
    io:format("Done.~n"),
    file:close(File),
    Trie.

fill_trie(File, N, Trie) ->
    case file:read_line(File) of
        {ok, LineNL} ->
            Line = string:strip(LineNL, right, $\n),
            fill_trie(File, N + 1, ternary_trie:insert(Line, N, Trie));
        eof ->
            Trie
    end.

loop(Trie) ->
    case io:get_line("> ") of
        eof ->
            ok;
        LineNL ->
            Prefix = string:strip(LineNL, right, $\n),
            Ans = ternary_trie:prefix(Prefix, Trie),
            io:format("Prefix = ~p~nlength(Ans) = ~p~nAns = ~p~n",
                      [Prefix, length(Ans), Ans]),
            loop(Trie)
    end.

main([WordsPath]) ->
    loop(load_trie(WordsPath)).
