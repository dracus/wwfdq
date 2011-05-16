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
-export([is_word/1,
	 valid_words/1,
	 valid_words_prefix/2,
	 valid_words_suffix/2,
	 valid_words_prefix_and_suffix/3,
	 words_score/1,
	 words_score_prefix/2,
	 words_score_suffix/2,
	 words_score_prefix_and_suffix/3]).


-spec is_word(Word::string()) -> list().
is_word(Word) ->
    Words = read_word_list(),
    [X || X <- Words, X =:= Word].

-spec valid_words(Chars::string()) -> list(string()).
valid_words(Chars) ->
    Words = read_word_list(),
    Perms = all_perms(Chars),
    [Y || X <- Words,
	  Y <- Perms,
	  Y =:= X].

-spec valid_words_prefix(Prefix::string(), Chars::string()) ->
				list(string()).
valid_words_prefix(Prefix, Chars) ->
    Words = read_word_list(),
    Perms = all_perms(Chars),
    [X || X <- Words,
	  Y <- Perms,
	  Prefix ++ Y =:= X].

-spec valid_words_suffix(Suffix::string(), Chars::string()) ->
				list(string()).
valid_words_suffix(Suffix, Chars) ->
    Words = read_word_list(),
    Perms = all_perms(Chars),
    [X || X <- Words,
	  Y <- Perms,
	  Y ++ Suffix =:= X].

-spec valid_words_prefix_and_suffix(
	Prefix::string(), Chars::string(), Suffix::string()) ->
				 list(string()).
valid_words_prefix_and_suffix(Prefix, Chars, Suffix) ->
    Words = read_word_list(),
    Perms = all_perms(Chars),
    [X || X <- Words,
	  Y <- Perms,
	  Prefix ++ Y ++ Suffix =:= X].

-spec words_score(Chars::string()) -> list({string(), integer()}).
words_score(Chars) ->    
    ValidWords = valid_words(Chars),
    sort_words_score(ValidWords).

-spec words_score_prefix(Prefix::string(), Chars::string()) ->
				list({string(), integer()}).
words_score_prefix(Prefix, Chars) ->
    ValidWords = valid_words_prefix(Prefix, Chars),
    sort_words_score(ValidWords).

-spec words_score_suffix(Suffix::string(), Chars::string()) ->
				list({string(), integer()}).
words_score_suffix(Suffix, Chars) ->
    ValidWords = valid_words_suffix(Suffix, Chars),
    sort_words_score(ValidWords).

-spec words_score_prefix_and_suffix(
	Prefix::string(), Chars::string(), Suffix::string()) ->
					   list({string(), integer()}).
words_score_prefix_and_suffix(Prefix, Chars, Suffix) ->
    ValidWords = valid_words_prefix_and_suffix(Prefix, Chars, Suffix),
    sort_words_score(ValidWords).

%%
%% Internal functions
%%

read_word_list() ->
    {ok, File} = file:read_file("../enable1.txt"),
    string:tokens(binary_to_list(File), "\r\n").
    

perms([]) -> [[]];
perms(L)  -> 
    [[H|T] || H <- L, T <- perms(L--[H])].

perms2([]) -> [];
perms2(L) ->
    perms(L) ++ lists:concat([perms2(L--[X]) || X <- L]).

all_perms(L) -> 
    lists:usort(perms2(L)).


word_score(Word) ->
    lists:sum([value(X) || X <- Word]).

sort_words_score(Words) ->
    lists:sort(fun({_, A}, {_, B}) -> A >= B end,
	       [{X, word_score(X)} || X <- Words]).

value(97) -> 1; %a
value(98) -> 4; %b
value(99) -> 3; %c
value(100) -> 2; %d
value(101) -> 1; %e
value(102) -> 4; %f
value(103) -> 3; %g
value(104) -> 4; %h
value(105) -> 1; %i
value(106) -> 8; %j
value(107) -> 5; %k
value(108) -> 1; %l
value(109) -> 4; %m
value(110) -> 1; %n
value(111) -> 1; %o
value(112) -> 4; %p
value(113) -> 10; %q
value(114) -> 1; %r
value(115) -> 1; %s
value(116) -> 1; %t
value(117) -> 2; %u
value(118) -> 5; %v
value(119) -> 4; %w
value(120) -> 8; %x
value(121) -> 4; %y
value(122) -> 10; %z
value(32) -> 0; %blank
value(_) -> error. 

    
