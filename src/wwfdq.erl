%%
%% wwfdq --
%%      words with friends done quick
%%
%% ----------- 
%%
%% Copyright (c) 2011 Daniel Carlsson
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(wwfdq).
-export([is_word/1, valid_words/1, 
	 perms/1, perms2/1, perms3/1]).


-spec is_word(Word::string()) -> list().
is_word(Word) ->
    Words = read_word_list(),
    [X || X <- Words, X =:= Word].

-spec valid_words(Chars::list()) -> list().
valid_words(Chars) ->
    Words = read_word_list(),
    Perms = perms3(Chars),
    [Y || X <- Words, Y <- Perms, Y =:= X].
    

%%
%% Internal functions
%%

read_word_list() ->
    {ok, File} = file:read_file("../enable1.txt"),
    string:tokens(binary_to_list(File), "\r\n").
    

perms([]) -> [[]];
perms(L)  -> 
     %io:format("L1: ~p~n", [L]),
    [[H|T] || H <- L, T <- perms(L--[H])].

perms2([]) -> [[]];
perms2(L) ->
    %io:format("L2: ~p~n", [L]),
    [[H|T] || H <- L, T <- perms2(L--[H])] ++ 
	[lists:concat(perms2(L--[X])) 
	 || X <- L, L--[X] =/= [], length(L--[X]) =:= 1].% ++
	%[perms2(L--[X]) || X <- L, L--[X] =/= [], length(L--[X]) =/= 1].

perms3(L) -> 
    lists:usort([lists:concat(X) || X <- perms2(L)]).

%remove_empty_list(List) ->
%    remove_empty_list(List, []).
%remove_empty_list([], Res) ->
%    lists:reverse(Res);
%remove_empty_list([H|T], Res) ->
%    case H =:= [] of
%	true -> remove_empty_list(T, Res);
%	_ -> remove_empty_list(T, [H|Res])
%    end.


