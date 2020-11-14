%% --------------------------------------------------------

%% File:   test1.pl

%% Author: Jens Kallup - <kallup.jens@web.de> - paule32

%% Lizenz: all rights reserved.

%%

%%         only for non-profit usage !!!

%% --------------------------------------------------------

:- set_prolog_flag(verbose, silent).

:- initialization(main).



human(jan).

human(janosh).

human(jessy).

human(jens).

human(gabriel).

human(greta).

human(gislinde).

human(gerd).

human(heinz).

human(hans).

human(franz).

human(paul).

human(fritz).


human(nomen).


wort(paul,human).



wort(rot, verb).

wort(dank, verb).

wort(geb, verb).



w_wort( wer   , subjekt_frage ).

w_wort( wen   , objekt_frage  ).

w_wort( warum , kausal_frage  ).



satzanfang(WORT, SATZTYP) :-

	w_wort(WORT, SATZTYP) ,
	!.



%% Syntaktische Regeln %%

satz(X) :- append( Y, Z, X), np(Y) , vp(Z).

np(X)   :- append( Y, Z, X), det(Y), n(Z).

vp(X)   :- append( Y, Z, X), v(Y)  , np(Z).

vp(X)   :- v(X).



%% Lexikalische Regeln %%

det([der]).

det([die]).



n([mann]).

n([frau]).



v([sieht]).



check_s(X) :- satz(X)

	-> format("Satz ist richtig"), nl

	;  format("Satz ist nicht richtig"), nl.



?- check_s([der,mann,sieht,frau]).



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

	format('SWIPL-Prolog Beispiel-Script (c) 2020 by paule32'), nl,

	eat(cats, crimps),

	halt(0).
