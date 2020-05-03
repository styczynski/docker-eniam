%
% Predykat obsługujący wysycanie wymagań czasownikowych 
% w gramatyce Świdzińskiego.
%
%
% Copyright © 1997-2007,2010 Marcin Woliński
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License version 3 as
% published by the Free Software Foundation.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
% MA 02110-1301, USA 
%
% In addition, as a special exception, the copyright holder gives
% permission to link the code of this program with the Morfeusz library
% (see http://www.nlp.ipipan.waw.pl/~wolinski/morfeusz), and distribute
% linked combinations including the two. You must obey the GNU General
% Public License in all respects for all of the code used other than
% Morfeusz. If you modify this file, you may extend this exception to
% your version of the file, but you are not obligated to do so. If you
% do not wish to do so, delete this exception statement from your
% version.


wymagania( [I0,TrId/NT|PP], PN, I0, In, IW, WW, OW, NT, FW ) :-
	goal( NT, I0, I1, TrId ),
	wyjmijl( IW, WW, WWW),
	wymagane( PP, PN, I1, In, WWW, OW, FW ).

% wymagania( [I0,TrId/F|PP], PN, I0, In, IW, WW, OW, NT, [W/F | FF] ) :-
% 	goal( F, I0, I1, TrId ),
% 	wymagania( PP, PN, I1, In, [W|IW], WW, OW, NT, FF ).

wymagania( [I0,TrId/F|PP], PN, I0, In, IW, WW, OW, NT, W0/F0/{ Cond0 } ) :-
	copy_term(W0/F0/{ Cond0 }, W/F/{ Cond }),
	F0=fw(_, K, A, C, Rl, O, Neg, I, _),
	F=fw(_, K, A, C, Rl, O, Neg, I, _),
	goal( F, I0, I1, TrId ), call(Cond),
	wymagania( PP, PN, I1, In, [W|IW], WW, OW, NT, W0/F0/{ Cond0 } ).



% wymagane( [I0,TrId/F | PP], PN, I0, IN, WW, OW, [W/F | FF] ) :-
% 	goal( F, I0, I1, TrId ),
% 	wyjmijwymaganie( W, WW, NW ),
% 	wymagane( PP, PN, I1, IN, NW, OW, FF ).

wymagane( [I0,TrId/F | PP], PN, I0, IN, WW, OW, W0/F0/{ Cond0 } ) :-
	copy_term(W0/F0/{ Cond0 }, W/F/{ Cond }),
	F0=fw(_, K, A, C, Rl, O, Neg, I, _),
	F=fw(_, K, A, C, Rl, O, Neg, I, _),
	goal( F, I0, I1, TrId ), call(Cond),
	wyjmijwymaganie( W, WW, NW ),
	wymagane( PP, PN, I1, IN, NW, OW, W0/F0/{ Cond0 } ).

wymagane( P, P, I, I, wym([],OW), OW, _).



wyjmijwymaganie(E, wym([E|S],W), wym(S,NW)) :-
	wyjmijwymaganie2(E, W, NW),
	NW = [_|_].
wyjmijwymaganie(E, wym([E|S],koord(W)), wym(S,koord(NW))) :-
	wyjmij_skoordynowane_wymaganie(E, W, NW).


wyjmijwymaganie2(_, [], []).
wyjmijwymaganie2(E, [W|WW], [NW|NWW]) :-
	wyjmij(E, W, NW), !,
	wyjmijwymaganie2(E, WW, NWW).
wyjmijwymaganie2(E, [_|WW], NW) :-
	wyjmijwymaganie2(E,WW,NW).


% Wersja dla list wymagań fraz skoordynowanych
% Wymaganie E musi dać się wyjąć z każdego ze skoordynowanych członów:
wyjmij_skoordynowane_wymaganie(_, [], []).
wyjmij_skoordynowane_wymaganie(E, [W|WW], [NW|NWW]) :-
	wyjmijwymaganie2(E, W, NW),
	NW = [_|_],
	wyjmij_skoordynowane_wymaganie(E, WW, NWW).

% Sprawdza, czy E jest jednym z elementów listy i go wyjmuje.
% Uwaga: zdeterminizowany za pomocą czerwonego odcięcia!
wyjmij( E, [E|L], L ) :- !.
wyjmij( E, [F|L], [F|K] ) :- wyjmij( E, L, K ).

% Wyjmuje wszystkie elementy pierwszego argumentu z drugiego
wyjmijl( [], L, L ) :- !.
wyjmijl( [E|EE], L, K ) :- wyjmijwymaganie(E, L, LL), wyjmijl(EE,LL,K).

% TUTAJ się dopuszcza elipsę:
%resztawym(_).

% wersja z uwolnionym ‘się’: nie ma prawa zostać niewykorzystane ‘sie’
resztawym(W) :-
%	format(user_error,"~NReszta wymagań: ~p~n",[W]),
	odfiltrujsie(W,BEZSIE),
%	format(user_error,"~NReszta bez się: ~p~n",[BEZSIE]),
	BEZSIE \= [].

sprawdz_reszte(wym([],koord(OWW))) :- !, sprawdz_wszystkie_reszty(OWW).
sprawdz_reszte(wym([],OW)) :- !, resztawym(OW).

sprawdz_wszystkie_reszty([]).
sprawdz_wszystkie_reszty([R|RR]) :- resztawym(R), sprawdz_wszystkie_reszty(RR).

odfiltrujsie([],[]).
odfiltrujsie([R|RR], W) :-
	(%member(sie,R),
%konieczne, jeśli pozycja słownika wymagań może zawierać _:
	var_member(sie,R,_)
	-> W = WW
	; W=[R|WW]),
	odfiltrujsie(RR,WW).


% Poniższy predykat jest używany do podmiany wybranych argumentów w
% ramkach.  Potrzebny np. przy derywowaniu ramki imiesłowowej z
% czasownikowej:
% zamień_argument(subj,prepnp(przez,bier), W, NW)

zamień_argument(_, _, [], []) :- !.
zamień_argument(A1, A2, [W|WW], [NW|NWW]) :-
	zamień_argument_pom(A1, A2, W, NW),
	zamień_argument(A1, A2, WW, NWW).

zamień_argument_pom(_, _, [], []) :- !.
zamień_argument_pom(A1, A2, [A1|W], [A2|NW]) :- !,
	zamień_argument_pom(A1, A2, W, NW).
zamień_argument_pom(A1, A2, [A|W], [A|NW]) :-
	zamień_argument_pom(A1, A2, W, NW).

% Następny predykat jest używany do usuwania wybranych argumentów z
% ramek.  Potrzebny np. przy derywowaniu ramki dla imiesłowu biernego
% z czasownikowej:
% usuń_argument(np(bier), W, NW)

usuń_argument(A, koord(W), koord(NW)) :- !,
	usuń_argument_ze_wszystkich(A, W, NW).
usuń_argument(_, [], []) :- !.
usuń_argument(A1, [W|WW], [NW|NWW]) :-
	usuń_argument_pom(A1, W, NW),
	usuń_argument(A1, WW, NWW).

usuń_argument_pom(_, [], []) :- !.
usuń_argument_pom(A1, [A1|W], NW) :- !,
	usuń_argument_pom(A1, W, NW).
usuń_argument_pom(A1, [A|W], [A|NW]) :-
	usuń_argument_pom(A1, W, NW).

usuń_argument_ze_wszystkich(_, [], []) :- !.
usuń_argument_ze_wszystkich(A, [W|WW], [NW|NWW]) :-
	usuń_argument(A,W,NW),
	usuń_argument_ze_wszystkich(A,WW,NWW).

% (Powyższe predykaty można by zapisać wspólnie za pomocą
% metawywołania, ale nie wiem, czy to by nie wpłynęło źle na
% efektywność, więc niech się lepiej kompilują.)

% To jest wersja specjalna dla argumentu subj przy gerundiach.
% Zakodowana na żywca, dla uproszczenia.  Dla gerundiów subj musi
% zamienić się w alternatywę np(dop) i prepnp(przez,bier).  Korzystamy
% bezczelnie z tego, że subj, jeśli jest, to jest pierwszym elementem
% w ramce.

zamień_subj_dla_ger([], []) :- !.
zamień_subj_dla_ger([[subj|R]|WW], [[np(dop)|R],[prepnp(przez,bier)|R]|NWW]) :-
	!,
	zamień_subj_dla_ger(WW, NWW).
zamień_subj_dla_ger([R|WW], [R|NWW]) :-
	!,
	zamień_subj_dla_ger(WW, NWW).


% Pobieranie wymagań czasownikowych ze słownika.  Tutaj, bo
% dostosowuje format wymagań do wersji procedury obsługującej wymagania

rekcja_finitywna(H, wym(_,
% domyślna ramka używana, gdy nie mamy czasownika w słowniku:
	       [[subj,np(bier),np(cel)],
		[subj,np(bier),np(narz)],
		[subj,np(bier),np(dop)],
		[subj,np(bier),advp],
		[subj,np(bier),sentp(_)],
		[subj,np(bier),infp(_)],
		[subj,np(bier),prepnp(bez,dop)],
		[subj,np(bier),prepnp(dla,dop)],
		[subj,np(bier),prepnp(do,dop)],
		[subj,np(bier),prepnp(na,bier)],
		[subj,np(bier),prepnp(na,miej)],
		[subj,np(bier),prepnp(o,bier)],
		[subj,np(bier),prepnp(o,miej)],
		[subj,np(bier),prepnp(od,dop)],
		[subj,np(bier),prepnp(po,bier)],
		[subj,np(bier),prepnp(po,miej)],
		[subj,np(bier),prepnp(przed,narz)],
		[subj,np(bier),prepnp(przez,bier)],
		[subj,np(bier),prepnp(przy,miej)],
		[subj,np(bier),prepnp(u,dop)],
		[subj,np(bier),prepnp(w,bier)],
		[subj,np(bier),prepnp(w,miej)],
		[subj,np(bier),prepnp(wobec,dop)],
		[subj,np(bier),prepnp(z,dop)],
		[subj,np(bier),prepnp(z,narz)],
		[subj,np(bier),prepnp(za,bier)],
		[subj,np(bier),prepnp(za,narz)],
		[subj,np(cel),infp(_)],
		[subj,np(cel),sentp(_)],
% musimy dopuszczać możliwość, że czasownik jest sięiczny:
		[subj,sie,np(narz)],
		[subj,sie,np(dop)],
		[subj,sie,advp],
		[subj,sie,prepnp(bez,dop)],
		[subj,sie,prepnp(dla,dop)],
		[subj,sie,prepnp(do,dop)],
		[subj,sie,prepnp(na,bier)],
		[subj,sie,prepnp(na,miej)],
		[subj,sie,prepnp(o,bier)],
		[subj,sie,prepnp(o,miej)],
		[subj,sie,prepnp(od,dop)],
		[subj,sie,prepnp(po,bier)],
		[subj,sie,prepnp(po,miej)],
		[subj,sie,prepnp(przed,narz)],
		[subj,sie,prepnp(przez,bier)],
		[subj,sie,prepnp(przy,miej)],
		[subj,sie,prepnp(u,dop)],
		[subj,sie,prepnp(w,bier)],
		[subj,sie,prepnp(w,miej)],
		[subj,sie,prepnp(wobec,dop)],
		[subj,sie,prepnp(z,dop)],
		[subj,sie,prepnp(z,narz)],
		[subj,sie,prepnp(za,bier)],
		[subj,sie,prepnp(za,narz)],
		[subj,sie,np(cel),infp(_)],
		[subj,sie,np(cel),sentp(_)]
	       ])) :-
	\+ slowczas(H,n,_), !.
%rekcja(H,_,_) :-
%	\+ slowczas(H,_,_),
%	!, throw(morf(wymagania, H)).
rekcja_finitywna(H,wym(_SwWym,Wym)) :-
	slowczas(H,n,Wym).

rekcja_niefinitywna(H, wym(_,NWym)) :-
	rekcja_finitywna(H, wym(_,Wym)),
	usuń_argument(subj, Wym, NWym).

%% Wymagania gerundiów:

rekcja_gerundialna(H, wym(_SwWym, Wym)) :-
	slowczas(H, _S, WymC), !,
	%%stare: zamień_argument(subj,prepnp(przez,bier),WymC,WymC1),
	zamień_subj_dla_ger(WymC,WymC1),
	zamień_argument(np(bier),np(dop), WymC1, Wym).
rekcja_gerundialna(_H, wym(_SwWym,[[prepnp(przez,bier),np(dop)]])).

%% Wymagania rzeczowników:

rekcja_nominalna(H, wym(_SwWym, Wym)) :-
	s_rzecz(H, Wym), !.
rekcja_nominalna(_H, wym(_SwWym,[[]])).

%% Wymagania przymiotników:

rekcja_przymiotnikowa(H, wym(_SwWym, Wym)) :-
	s_przym(H, Wym), !.
rekcja_przymiotnikowa(_H, wym(_SwWym,[[]])).

%% Wymagania wykrzykników:

rekcja_wykrzyknikowa(H, wym(_SwWym, Wym)) :-
	s_wykrz(H, Wym), !.
rekcja_wykrzyknikowa(_H, wym(_SwWym,[[]])).

%% Wymagania imiesłowów:

rekcja_ppas(H, wym(_SwWym, Wym)) :-
	slowczas(H, _S, WymC), !,
	zamień_argument(subj,prepnp(przez,bier), WymC, WymC1),
	usuń_argument(np(bier), WymC1, Wym).
rekcja_ppas(_H, wym(_SwWym,[[prepnp(przez,bier)]])).

rekcja_pact(H, wym(_SwWym, Wym)) :-
	slowczas(H, _S, WymC), !,
	usuń_argument(subj, WymC, Wym).
rekcja_pact(_H, wym(_SwWym,[[]])).



% predykat używany w GFJP1:

rekcja(H,_S,W) :- rekcja_finitywna(H,W).

% predykat używany w GFJP1 w definicji niefinitywnych form
% czasownikowych do niedopuszczania wymagania podmiotu (w GFJP2 jest
% zamiast tego rekcja_niefinitywna):

wykluczpodmiot(wym(SwW,W), wym(SwW,NW)) :-
	usuń_argument(subj, W, NW).


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
