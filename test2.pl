%% -------------------------------------------------------- %%
%% File:   test1.pl                                         %%
%% Author: Jens Kallup - <kallup.jens@web.de> - paule32     %%
%% Lizenz: all rights reserved.								%%
%%                                                          %%
%% only for non-profit usage !!!                            %%
%% -------------------------------------------------------- %%

:- set_prolog_flag(verbose, silent).
:- initialization(main).

:- dynamic(noun_sm/1).

%% ----------------------------- %%
%% print a banner on screen :-)  %%
%% ----------------------------- %%
?- writeln('SWIPL-Prolog Beispiel-Script (c) 2020 by paule32'), nl.

%% female nouns ...
human_female(greta).
human_female(gislinde).
human_female(jessy).
human_female(antje).
human_female(sonja).
human_female(doreen).
human_female(silvia).
human_female(sandra).
human_female(katrin).

%% male nouns ...
human_male(jan).
human_male(janosh).
human_male(jens).
human_male(gabriel).
human_male(gerd).
human_male(heinz).
human_male(hans).
human_male(franz).
human_male(paul).
human_male(fritz).

human(X,Y) :-
		(human_male(X)  , Y -> true, writeln(X))
	;	(human_female(X), Y -> true, writeln(X))
	;	(	strcat([X,' is not found!'],Z),
			writeln(Z)
		).
human(X) :-
	(Y   = member(X,human_male(X))   -> true, writeln(Y)) ;
	(Y   = member(X,human_female(X)) -> true, writeln(Y)) ;
	(Y \== member(X,human_male(X))   -> false, writeln(Y)).

have_eyes(X) :-
		X is 2 -> true, writeln('has 2 eyes... ok')
	;	writeln('eyes wrong')
	;	fail.

%% ----------------------------------------------- %%
%% predicate to "join/add" string's in a list ...  %%
%% ----------------------------------------------- %%
strcat(StringList, Result) :-
	maplist(atom_chars, StringList, Lists),
	append(Lists, List),
	atom_chars(Result, List).

?- human(jens,have_eyes(2)).
?- halt(0).

wort(paul,human).

wort(rot, verb).
wort(dank, verb).
wort(geb, verb).

w_wort( wer   , subjekt_frage ).
w_wort( wen   , objekt_frage  ).
w_wort( warum , kausal_frage  ).


read_file(Stream,[]) :-
	at_end_of_stream(Stream).
	
read_file(Stream,[X|L]) :-
	\+ at_end_of_stream(Stream),
	read(Stream,X),
	read_file(Stream,L).

%% ---------------------------------------------- %%
%% Datenbank-Liste der männlichen nouns ...       %%
%% ---------------------------------------------- %%
setup_sm_nouns :-
    retractall(noun_sm(_)),                             %% delete all prev. nouns
    catch(open('sm_nouns.txt', read, FileHandle),       %% open database file, catch error
		error(_Err,_Context),                           %% error handler/context
		(	writeln('Error: could not open file: "sm_nouns.txt".'),
			writeln('exit program.'),
			halt(1)                                     %% halt (exit code = 1)
		)),
	writeln('read up file ...'),
	repeat,                                             %% read line per line ...
		read(FileHandle, Term),
		(	Term = end_of_file
		->	true                                        %% if EOF -> true ?
		;	assert(noun_sm(Term)),                      %% add one line to "noun_sm"
			fail
		),
	close(FileHandle).                                  %% close file stream/file handle

?- setup_sm_nouns.

?- assert(nouner([mann])).
?- assert(nouner([frau])).

checker(X) :-
	catch(nouner(X),
		error(_Err,_Context),
		(	writeln('Error: word not on list.'),
			halt(2)
		)),
	writeln('ok word in list.').

?- checker([manns]).



test1(X,Y) :-
	Y is (X+3) * 2.
	
?- Term = [noun_sm],
	write_canonical(Term), nl.

%?- functor(Term,noun_sm,2).

%?- member(noun_sm([mann]),[H|T]).


%noun_sm([mann]).
%noun_pm([maenner]).
noun_masculin(Y, X) :-
	( Y = singular, noun_sm(X)
	; Y = plural  , noun_pm(X)
	).

noun_femin([frau]).


%% ------------------------------ %%
%% Relativ-Sätze: der/die/das ... %%
%% ------------------------------ %%
relativ_singular_nominativ_masculin([der]).
relativ_singular_nominativ_masculin([welcher]).
%
relativ_singular_nominativ_femin([die]).
relativ_singular_nominativ_femin([welche]).
%
relativ_singular_nominativ_neutrum([das]).
relativ_singular_nominativ_neutrum([welches]).
%
relativ_singular_akkusativ_masculin([den]).
relativ_singular_akkusativ_masculin([welchen]).
%
relativ_singular_akkusativ_femin([die]).
relativ_singular_akkusativ_femin([welche]).
%
relativ_singular_akkusativ_neutrum([das]).
relativ_singular_akkusativ_neutrum([welches]).
%
relativ_singular_dativ_masculin([dem]).
relativ_singular_dativ_masculin([welchem]).
%
relativ_singular_dativ_femin([der]).
relativ_singular_dativ_femin([welcher]).
%
relativ_singular_dativ_neutrum([dem]).
relativ_singular_dativ_neutrum([welchem]).
%
relativ_singular_genitiv_masculin([dessen]).
relativ_singular_genitiv_femin([deren]).
relativ_singular_genitiv_femin([derer]).
relativ_singular_genitiv_neutrum([dessen]).
%
relativ_plural_nominativ([die]).
relativ_plural_nominativ([welche]).
%
relativ_plural_akkusativ([die]).
relativ_plural_akkusativ([welche]).
%
relativ_plural_dativ([denen]).
relativ_plural_dativ([welchen]).
%
relativ_plural_genitiv([deren]).
relativ_plural_genitiv([derer]).

%% ----------------------------------------------------------------- %%
%%
%%	satz(X) :-
%%	(
%%		append(Y, Z, X), relativ_singular_nominativ_masculin(Y), noun_masculin(singular,Z)
%%	;	append(Y, Z, X), relativ_plural_nominativ(Y)           , noun_masculin(plural  ,Z)
%%	;	append(Y, Z, X), relativ_singular_nominativ_femin(Y)   , noun_femin(Z)
%%	).

%%v([sieht]).

%%check_s(X) :- satz(X)
%%	-> format("Satz ist richtig"), nl
%%	;  format("Satz ist nicht richtig"), nl.

%%?- check_s([die,maenner]). %%,sieht,die,frau]).


animal-watter(fish).
animal-watter(lachs).
animal-watter(crimps).

animal-air(tiger).
animal-air(cow).
animal-air(puma).

eats(cats, X) :- human(X), !, fail.
eats(cats, X) :- animal-air(X), !, fail.
eats(cats, X) :- animal-watter(X).

eat(X, Y) :-
	once(eats(X, Y))
	-> writef('%t eats %t\n', [X, Y])
	;  writef('%t does not eat %t\n', [X, Y]).

main :-
	eat(cats, crimps),
	halt(0).








%<ski> paule32 : if you have a list `Words' (that could come from read_file/2), then you could use `forall(( member(Word,Words) ),( assertz(noun_sm(Word)) ))' in place of the repeat/0 - fail/0 failure-driven loop, to assert all the words to individual