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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REPREZENTACJA WYMAGAŃ:
%
% Atrybut Wym ma postać wym(SwW, Uż, Ramki) gdzie
%
% • SwW jest listą wysyconych wymagań do pokazania w drzewie.
%
% • Uż reprezentuje dziewiczość: użyte oznacza, że wymagania zostały
%   częściowo wysycone, czyste(_) — że nie.  Argumentem tego termu
%   jest ‹proste›, jeśli to pojedynczy czasownik, oraz oznaczenie typu
%   koordynacji poziomu o jeden niżej wpp.
%
% • Ramki albo jest listą schematów, albo ma postać koord([Ramki…]) —
% ta ostatnia postać potrzebna gdy mamy frazę werbalną ze
% skoordynowanymi kilkoma czasownikami.  Pojedynczy schemat jest listą
% pozycji, które są listami (typów) argumentów (które mogą się
% koordynować na tej pozycji).  Przy realizacji argumentu A znajduje
% się wszystkie ramki, które zawierają pozycje, które go zawierają;
% wynikiem staje się lista tych ramek, ale z pozycją wypełnioną przez
% A wyjętą.  W wypadku koord(…) A musi się dać wyjąć co najmniej z
% jednego schematu na każdej liście schematów (czyli musi się dać
% zrealizować przy każdym z koordynowanych czasowników).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

wymagane( P, P, I, I, wym([],_,OW), OW, _).



wyjmijwymaganie(E, wym([E|S],U,W), wym(S,U,NW)) :-
	wyjmijwymaganie2(E, W, NW),
	NW = [_|_].
wyjmijwymaganie(E, wym([E|S],U,koord(W)), wym(S,U,koord(NW))) :-
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
wyjmij( subj(E), [subj(EW)|L], L ) :- sprawdź_wymaganie(E,EW), !.
wyjmij( E, [EW|L], L ) :- sprawdź_wymaganie(E,EW), !.
wyjmij( E, [F|L], [F|K] ) :- wyjmij( E, L, K ).

sprawdź_wymaganie(E,EW) :- E = [_|_], !,
			   sprawdź_wymaganie_koord(E,EW).
sprawdź_wymaganie(E,EW) :- member(E,EW).

sprawdź_wymaganie_koord([E|EE], EW) :-
    member(E,EW),
    sprawdź_wymaganie_koord(EE, EW).
sprawdź_wymaganie_koord([], _).

% Wyjmuje wszystkie elementy pierwszego argumentu z drugiego
wyjmijl( [], L, L ) :- !.
wyjmijl( [E|EE], L, K ) :- wyjmijwymaganie(E, L, LL), wyjmijl(EE,LL,K).


% Warunek iterowany wysycający kolejne wymaganie E:
oblwym_iter(wym([E|S],U,koord(W)), E, wym(S,U,koord(NW))) :- !,
	wyjmij_skoordynowane_wymaganie(E, W, NW).
oblwym_iter(wym([E|S],U,W), E, wym(S,U,NW)) :- !,
	wyjmijwymaganie2(E, W, NW),
	NW = [_|_].


% OBLICZENIE WYMAGAŃ FRAZY SKOORDYNOWANEJ:
% Teraz mi się wydaje, że z koordynacji zawsze powinno wychodzić
% ‹czyste(_)›. Zobaczmy, jak to będzie działać:
oblwym_koord(wym(SwW,czyste(Oz),koord(Dowyp)), Oz, WymL) :-
	oblwym_koord(SwW,Dowyp,Oz,WymL).

oblwym_koord(_SwW, [], _, []) :- !.
oblwym_koord(SwW, Wym, Oz, [wym(SwW, _U1, koord(Wym1)) | WymL]) :- !,
	append(Wym1, Dowyp, Wym),
	oblwym_koord(SwW, Dowyp, Oz, WymL).
oblwym_koord(SwW, [Wym1 | Dowyp], Oz, [wym(SwW, _U1, Wym1) | WymL]) :-
	is_list(Wym1),
	oblwym_koord(SwW, Dowyp, Oz, WymL).

% oblwym_koord(_SwW, czyste(_), [], []).
% oblwym_koord(SwW, U, Wym, [wym(SwW, U1, koord(Wym1)) | WymL]) :- !,
% 	append(Wym1, Dowyp, Wym),
% 	oblwym_koord(SwW, U2, Dowyp, WymL),
% 	obl_użytość(U,U1,U2).
% oblwym_koord(SwW, U, [Wym1 | Dowyp], [wym(SwW, U1, Wym1) | WymL]) :-
% 	is_list(Wym1),
% 	oblwym_koord(SwW, U2, Dowyp, WymL),
% 	obl_użytość(U,U1,U2).

% obl_użytość(użyte,użyte,_) :- !.
% obl_użytość(użyte,_,użyte) :- !.
% obl_użytość(czyste(_),_,_).


% Predykat skutkuje, o ile jego drugi argument reprezentuje dziewicze,
% niezrealizowane wymagania (tylko takie wymagania mogą być we frazie
% finitywnej):
% Dodatkowo ujawnia oznaczenie typu czystości:

wymagania_czyste(Oz,wym(_,czyste(Oz),_)).

% Oznacza wymagania jako częściowo wysycone:
wymagania_oznacz_użyte(wym(SwW,_,W),wym(SwW,użyte,W)).

% Predykat skutkuje, o ile jego argument reprezentuje wymagania
% skoordynowane:

wymagania_skoordynowane(wym(_,_,koord(_))).

wyklucz_przecinkowość_w_szeregu([]) :- !.
wyklucz_przecinkowość_w_szeregu([_]) :- !.
wyklucz_przecinkowość_w_szeregu([Wym1 | WymL]) :-
	Wym1 \= wym(_,czyste(sz:przec),_),
	wyklucz_przecinkowość_w_szeregu(WymL).


% wersja z uwolnionym ‘się’: nie ma prawa zostać niewykorzystane ‘sie’
resztawym(W) :-
%	format(user_error,"~NReszta wymagań: ~p~n",[W]),
	odfiltrujsie(W,BEZSIE),
%	format(user_error,"~NReszta bez się: ~p~n",[BEZSIE]),
	BEZSIE \= [].

% Sygnalizuje zakończenie procesu wysycania danego zestawu wymagań i
% sprawdza, czy nie zostały niewysycone elementy obowiązkowe (na razie
% ‹się›, ale dodamy fixy z Walentego):
wymagania_zamknij(wym([],_U,bw)) :- !. % dodane na okoliczność domykania fno w w7
wymagania_zamknij(wym([],_U,koord(OWW))) :- !, sprawdz_wszystkie_reszty(OWW).
wymagania_zamknij(wym([],_U,OW)) :- !, resztawym(OW).

% To jest tymczasowe rozwiązanie dla problemu gerundiów zwrotnych,
% przy których puszczamy „się” na żywioł.  Trzeba zrobić porządniej,
% jak wprowadzimy lexy z Walentego.
wymagania_zamknij_nieuważnie(wym([],_U,_)).

% Na razie wprowadzam to na okoliczność objęcia frazy w cudzysłowy:
% ‹atakowany „nurt realistyczny”› — normalnie fno powinno przyjąć
% podrzędniki w jednym rzucie, ale jak ujmiemy w cudzysłów, to możliwe
% przyłączenie kolejnych.  Na dobrą sprawę należałoby zapewnić
% faktyczne wysycanie wymagań po odemknięciu, ale to by wymagało
% reorganizacji ich reprezentacji, więc na razie dopuszczam luźne.
wymagania_odemknij(wym(SwW,U,bw), wym(SwW,U,[[]])) :- !.
wymagania_odemknij(wym(SwW,U,W), wym(SwW,U,W)).

% Analogiczna operacja na potrzeby fwe, gdzie usuwamy znacznik użycia:
wymagania_oznacz_czyste(wym(SwW,użyte,W), wym(SwW,czyste(proste),W)) :- !.
wymagania_oznacz_czyste(wym(SwW,U,W), wym(SwW,U,W)).
%%%% Piotr „czyta książkę”.


sprawdz_wszystkie_reszty([]).
sprawdz_wszystkie_reszty([R|RR]) :- resztawym(R), sprawdz_wszystkie_reszty(RR).

odfiltrujsie([],[]).
odfiltrujsie([R|RR], W) :-
	(%member(sie,R),
	 wyjmij(sie,R,_)
	-> W = WW
	; W=[R|WW]),
	odfiltrujsie(RR,WW).


% Poniższy predykat jest używany do podmiany wybranych argumentów w
% ramkach.  A1 jest argumentem, a A2 — listą argumentów, w
% szczególności pustą.

zamień_argument(_, _, koord(A), _) :- throw(koord_in_zamień_argument(A)).
zamień_argument(_, _, [], []) :- !.
zamień_argument(A1, A2, [W|WW], [NW|NWW]) :-
	zamień_argument_pom(A1, A2, W, NW),
	zamień_argument(A1, A2, WW, NWW).

zamień_argument_pom(_, _, [], []) :- !.
%zamień_argument_pom(A1, A2, [subj(P)|W], NW) :- 
zamień_argument_pom(A1, A2, [P|W], NW) :- 
    select(A1, P, P1), !,
    append(A2,P1,P2),
    (P2 = [] *-> NW=WW; NW = [P2|WW]),
    zamień_argument_pom(A1,A2, W, WW).
zamień_argument_pom(A1, A2, [A|W], [A|NW]) :-
	zamień_argument_pom(A1, A2, W, NW).

% Poniższe zamienia zawartość pozycji subj(…) na listę pozycji Poz
% (np. [[np(gen)]]).  Lista może być pusta — oznacza to usunięcie
% pozycji podmiotowej.

zamień_subj(Poz, koord([Sch|SS]), koord([SchBezSubj|SSBS])) :-
    !,
    zamień_subj(Poz, Sch, SchBezSubj),
    zamień_subj(Poz, koord(SS), koord(SSBS)).
zamień_subj(_Poz, koord([]), koord([])) :- !.
zamień_subj(_Poz, [], []) :- !.
zamień_subj(Poz, [Sch|SS], [SchBezSubj|SSBS]) :-
    select(subj(_), Sch, Sch2), !,
    append(Sch2, Poz, SchBezSubj),
    zamień_subj(Poz, SS, SSBS).
zamień_subj(Poz, [S|SS], [S|NSS]) :-
    zamień_subj(Poz, SS, NSS).


% Pobieranie wymagań czasownikowych ze słownika.  Tutaj, bo
% dostosowuje format wymagań do wersji procedury obsługującej wymagania

rekcja_finitywna(H, wym(_,czyste(proste),
% domyślna ramka używana, gdy nie mamy czasownika w słowniku:
	       [
		[subj([np(mian)]),[np(bier)],[np(cel)]],
		[subj([np(mian)]),[np(bier)],[np(narz)]],
		[subj([np(mian)]),[np(bier)],[np(dop)]],
		[subj([np(mian)]),[np(bier)],[advp]],
		[subj([np(mian)]),[np(bier)],[sentp(_)]],
		[subj([np(mian)]),[np(bier)],[infp(_)]],
		[subj([np(mian)]),[np(bier)],[prepnp(bez,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(dla,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(do,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(na,bier)]],
		[subj([np(mian)]),[np(bier)],[prepnp(na,miej)]],
		[subj([np(mian)]),[np(bier)],[prepnp(o,bier)]],
		[subj([np(mian)]),[np(bier)],[prepnp(o,miej)]],
		[subj([np(mian)]),[np(bier)],[prepnp(od,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(po,bier)]],
		[subj([np(mian)]),[np(bier)],[prepnp(po,miej)]],
		[subj([np(mian)]),[np(bier)],[prepnp(przed,narz)]],
		[subj([np(mian)]),[np(bier)],[prepnp(przez,bier)]],
		[subj([np(mian)]),[np(bier)],[prepnp(przy,miej)]],
		[subj([np(mian)]),[np(bier)],[prepnp(u,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(w,bier)]],
		[subj([np(mian)]),[np(bier)],[prepnp(w,miej)]],
		[subj([np(mian)]),[np(bier)],[prepnp(wobec,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(z,dop)]],
		[subj([np(mian)]),[np(bier)],[prepnp(z,narz)]],
		[subj([np(mian)]),[np(bier)],[prepnp(za,bier)]],
		[subj([np(mian)]),[np(bier)],[prepnp(za,narz)]],
		[subj([np(mian)]),[np(cel)],[infp(_)]],
		[subj([np(mian)]),[np(cel)],[sentp(_)]],
% musimy dopuszczać możliwość, że czasownik jest sięiczny:
		[subj([np(mian)]),[sie],[np(narz)]],
		[subj([np(mian)]),[sie],[np(dop)]],
		[subj([np(mian)]),[sie],[advp]],
		[subj([np(mian)]),[sie],[prepnp(bez,dop)]],
		[subj([np(mian)]),[sie],[prepnp(dla,dop)]],
		[subj([np(mian)]),[sie],[prepnp(do,dop)]],
		[subj([np(mian)]),[sie],[prepnp(na,bier)]],
		[subj([np(mian)]),[sie],[prepnp(na,miej)]],
		[subj([np(mian)]),[sie],[prepnp(o,bier)]],
		[subj([np(mian)]),[sie],[prepnp(o,miej)]],
		[subj([np(mian)]),[sie],[prepnp(od,dop)]],
		[subj([np(mian)]),[sie],[prepnp(po,bier)]],
		[subj([np(mian)]),[sie],[prepnp(po,miej)]],
		[subj([np(mian)]),[sie],[prepnp(przed,narz)]],
		[subj([np(mian)]),[sie],[prepnp(przez,bier)]],
		[subj([np(mian)]),[sie],[prepnp(przy,miej)]],
		[subj([np(mian)]),[sie],[prepnp(u,dop)]],
		[subj([np(mian)]),[sie],[prepnp(w,bier)]],
		[subj([np(mian)]),[sie],[prepnp(w,miej)]],
		[subj([np(mian)]),[sie],[prepnp(wobec,dop)]],
		[subj([np(mian)]),[sie],[prepnp(z,dop)]],
		[subj([np(mian)]),[sie],[prepnp(z,narz)]],
		[subj([np(mian)]),[sie],[prepnp(za,bier)]],
		[subj([np(mian)]),[sie],[prepnp(za,narz)]],
		[subj([np(mian)]),[sie],[np(cel)],[infp(_)]],
		[subj([np(mian)]),[sie],[np(cel)],[sentp(_)]]
	       ])) :-
	\+ slowczas(H,_), !.
%rekcja(H,_,_) :-
%	\+ slowczas(H,_),
%	!, throw(morf(wymagania, H)).
rekcja_finitywna(H,wym(_SwWym,czyste(proste),Wym)) :-
	slowczas(H,Wym).

rekcja_niefinitywna(H, wym(_,U,NWym)) :-
	rekcja_finitywna(H, wym(_,U,Wym)),
	zamień_subj([], Wym, NWym).

%% Wymagania gerundiów:

rekcja_gerundialna(H, wym(_SwWym, czyste(proste), Wym)) :-
	slowczas(H, WymC), !,
	zamień_subj([[np(dop),prepnp(przez,bier)]], WymC,WymC1),
	zamień_argument(np(bier), [np(dop)], WymC1, WymC2),
	zamień_argument(sentp(bier,Sp), [sentp(dop,Sp)], WymC2, WymC3),
	zamień_argument(np(part), [np(dop)], WymC3, Wym).
rekcja_gerundialna(_H, wym(_SwWym,czyste(proste),[[[prepnp(przez,bier),np(dop)]]])).

%% Wymagania rzeczowników:

rekcja_nominalna(H, wym(_SwWym,czyste(proste), Wym)) :-
	s_rzecz(H, Wym), !.
rekcja_nominalna(_H, wym(_SwWym,czyste(proste),[[]])).

%% Wymagania przymiotników:

rekcja_przymiotnikowa(H, wym(_SwWym,czyste(proste), Wym)) :-
	s_przym(H, Wym), !.
rekcja_przymiotnikowa(_H, wym(_SwWym,czyste(proste),[[]])).

%% Wymagania wykrzykników:

rekcja_wykrzyknikowa(H, wym(_SwWym,czyste(proste), Wym)) :-
	s_wykrz(H, Wym), !.
rekcja_wykrzyknikowa(_H, wym(_SwWym,czyste(proste),[[]])).

%% Wymagania imiesłowów:

rekcja_ppas(H, wym(_SwWym,czyste(proste), Wym)) :-
	slowczas(H, WymC), !,
	zamień_subj([[prepnp(przez,bier)]], WymC, WymC1),
	zamień_argument(np(bier), [], WymC1, WymC2),
	zamień_argument(np(part), [], WymC2, Wym).
rekcja_ppas(_H, wym(_SwWym,czyste(proste),[[[prepnp(przez,bier)]]])).

rekcja_pact(H, wym(_SwWym,czyste(proste), Wym)) :-
	slowczas(H, WymC), !,
	zamień_subj([], WymC, Wym).
rekcja_pact(_H, wym(_SwWym,czyste(proste),[[]])).

rekcja_zdanioidowa(wym(_,czyste,
		       [
			   [[adjp(_)]],
			   [subj([np(mian)]),[adjp(mian)]], % „Wstęp wzbroniony.”
			   [subj([np(mian)]),[advp]], % „Pod drzwiami pikieta.”
			   [subj([np(mian)]),[np(bier)],[np(cel)]],
			   [subj([np(mian)]),[np(bier)],[np(narz)]],
			   [subj([np(mian)]),[np(bier)],[np(dop)]],
			   [subj([np(mian)]),[np(bier)],[advp]],
			   [subj([np(mian)]),[np(bier)],[prepnp(bez,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(dla,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(do,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(na,bier)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(na,miej)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(o,bier)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(o,miej)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(od,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(po,bier)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(po,miej)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(przed,narz)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(przez,bier)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(przy,miej)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(u,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(w,bier)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(w,miej)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(wobec,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(z,dop)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(z,narz)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(za,bier)]],
			   [subj([np(mian)]),[np(bier)],[prepnp(za,narz)]]
		  ])).



%% Wymagania puste (używane do inicjowania atrybutu Wym gdy nie
%% pobieramy ze słownika):

rekcja_pusta(wym(_SwW, czyste(proste), [[]])).

%% Wymagania blokujące wejście w wysycanie wymagań:

rekcja_zablokowana(wym(_SwW, czyste(proste), bw)).

% predykat używany w GFJP1:

rekcja(H,_S,W) :- rekcja_finitywna(H,W).


wykluczpodmiot(wym(SwW,U,W), wym(SwW,U,NW)) :-
    zamień_subj([], W, NW).

% Poniższy predykat jest używany w zdaniu elementarnym do wykluczenia
% realizacji podmiotu, jeżeli wyróżnik fleksyjny ma wartość
% bezokolicznika.  Odbywa się to po zgromadzeniu wszystkich
% argumentów, ponieważ posiłk przekształcający bok w os może wystąpić
% zarówno przed jak i za ff.  Tak więc najpierw wymagania są
% realizowane cokolwiek optymistycznie dopuszczając podmiot, ale jeśli
% końcową wartością Wf pozostanie bok, to taka realizacja z podmiotem
% jest odrzucana.
wyklucz_podmiot_bezokolicznika(Wf,_Wym) :- Wf \= bok, !.
wyklucz_podmiot_bezokolicznika(bok,wym(SwW,_Wym,_)) :-
	\+ member(subj(_), SwW).


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
