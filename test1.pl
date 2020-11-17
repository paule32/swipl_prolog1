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

human(X) :-
		human_male(X)   -> writeStr([X, ' is found as male...     ok'])
	;	human_female(X) -> writeStr([X, ' is found as female...   ok'])
	;					   writeStr([X, ' is not found!']).
	
have_heads(X,Y)   :- Y is 1  -> writeStr([X, ' have '      , Y, ' head...            ok']) ; writeStr(['query: heads   wrong']), halt(1).
have_eyes(X,Y)    :- Y is 2  -> writeStr([X, ' have '      , Y, ' eyes...            ok']) ; writeStr(['query: eyes    wrong']), halt(1).
have_necks(X,Y)   :- Y is 1  -> writeStr([X, ' have '      , Y, ' necks...           ok']) ; writeStr(['query: necks   wrong']), halt(1).
have_arms(X,Y)    :- Y is 2  -> writeStr([X, ' have '      , Y, ' arms...            ok']) ; writeStr(['query: arms    wrong']), halt(1).
have_hands(X,Y)   :- Y is 2  -> writeStr([X, ' have '      , Y, ' hands...           ok']) ; writeStr(['query: hands   wrong']), halt(1).
have_fingers(X,Y) :- Y is 10 -> writeStr([X, ' hands have ', Y, ' fingers...         ok']) ; writeStr(['query: fingers wrong']), halt(1).
have_legs(X,Y)    :- Y is 2  -> writeStr([X, ' have '      , Y, ' legs...            ok']) ; writeStr(['query: legs    wrong']), halt(1).
have_foots(X,Y)   :- Y is 2  -> writeStr([X, ' have '      , Y, ' foots...           ok']) ; writeStr(['query: foots   wrong']), halt(1).
have_teeths(X,Y)  :- Y is 10 -> writeStr([X, ' have '      , Y, ' teeths...          ok']) ; writeStr(['query: teeth   wrong']), halt(1).

person(X) :-
		human(X),
		(	have_heads(X,1),
			have_eyes(X,2),
			have_necks(X,1),
		(	have_arms(X,2),
				have_hands(X,2),
				have_fingers(X,10)
		),
			have_legs(X,2),	
		(	have_foots(X,2),
				have_teeths(X,10)
		)	)
	;	writeStr(['query is wrong!']).

%% ----------------------------------------------- %%
%% predicate to "join/add" string's in a list ...  %%
%% ----------------------------------------------- %%
strcat(StringList, Result) :-
	maplist(atom_chars, StringList, Lists),
	append(Lists, List),
	atom_chars(Result, List).

%% ----------------------------------------------- %%
%% concat string list, then write list ...         %%
%% ----------------------------------------------- %%
writeStr(StringList) :-
	strcat(StringList, Z),
	writeln(Z).

?- format('+~`-t~78|+ ~n',[]).
?- format('| Name~t~20|| Type~t~40|| Count~t~60|| Extra~t~78||~n',[]).
?- format('+~`-t~78|+ ~n',[]).


?- human(greta).
?- human(paul).
?- writef('\n').
?- person(sonja).
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