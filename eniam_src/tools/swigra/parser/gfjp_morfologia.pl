% Predykaty związane z analizą morfologiczną.
% 
% Copyright (C) 1997-2007 Marcin Woliński
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

% definicja predykatu morfeusz/4:
:-[morfeusz].

% wyjątki słownikowe (dostosowanie wyników Morfeusza do potrzeb GFJP):
:-[gfjp_slowwyj].

% analiza poprzyimkowości:
:-[gfjp_poprzyimkowosc].

% dodawanie pustych przecinków:
:-[gfjp_dodajprzec].

% Ten predykat będzie reprezentował graf fleksyjny:
:-dynamic(input/5).

% analiza morfologiczna, przygotowanie grafu fleksyjnego:
morfologia(Tekst,Od,Do) :-
	morfeusz(Tekst, input, Od, Do),
	sprawdzmorf,
	zmienpraep(Do, Add),
	zweryfikuj_brevpun,
%	Do=Add,
	dodajprzec(Od, Do, Add).


% za pomocą tego predykatu analizator pobiera kolejny element wejścia:
getinput( W0, W1, morf(Slowo,OHaslo,NTag), _ ) :-
	input(W0, W1, Slowo, Haslo, Tagi),
	oddwukropkuj_lemat(Haslo,OHaslo),
 	pałkowa(Tagi,Tag), slowwyj(OHaslo, Tag, NTag).

oddwukropkuj_lemat(H:_,H) :- !.
oddwukropkuj_lemat(H,H).


% Czy Morfeusz znał wszystkie słowa?  Jeśli nie, zgłaszamy wyjątek:
% Morfeusz 1 zwracał nil, który zamieniał się w []:
sprawdzmorf :-
	input(_,_,S,_,[]),
	statprint(morph_ok,ign),
	throw(morf('nieznane słowo', S)),
	fail.
% Morfeusz 2 zwraca jawny ign:
sprawdzmorf :-
    bagof(S,A^B^C^input(A,B,S,C,ign),IGN),
    statprint(morph_ok,ign),
    throw(morf('nieznane', IGN)),
    fail.


% sprawdzmorf :-
% 	input(_,_,S,_,num:_),
% 	statprint(morph_ok,num),
% 	throw(morf('liczebnik', S)),
% 	fail.

sprawdzmorf :-
%	input(_,_,_,_,T:_),
%	jestczasownikiem(T),
	!,
	statprint(morph_ok,tak).

sprawdzmorf :-
	statprint(morph_ok,noverb),
	throw(morf('brak czasowników')),
	fail.

zweryfikuj_brevpun :-
	input(Od,Do,F,H,brev:pun),
	\+ input(Do, _Do1, _, '.', interp),
	retract(input(Od,Do,F,H,brev:pun)),
	fail.

zweryfikuj_brevpun.

jestczasownikiem(fin).
jestczasownikiem(bedzie).
jestczasownikiem(inf).
jestczasownikiem(praet).
jestczasownikiem(aglt).
jestczasownikiem(impt).
jestczasownikiem(imps).
%jestczasownikiem(pant).
%jestczasownikiem(pcon).
jestczasownikiem(pred).
jestczasownikiem(winien).

% Tłumaczenie znaczników Morfeusza na notację GFJP

%liczba(X,poj) :- var(X), throw(nieustalona_liczba).
liczba(sg.pl,poj).
liczba(sg.pl,mno) :-!.
liczba(sg,poj).
liczba(pl,mno).

kropkowa([C|_],C).
kropkowa([_|CC],C) :- !, kropkowa(CC,C).
kropkowa(C,C).

przypadki(_:_,_) :- !, fail.
przypadki(C,P) :- kropkowa(C,C1), przypadek(C1,P).
przypadek(nom,mian).
przypadek(gen,dop).
przypadek(dat,cel).
przypadek(acc,bier).
przypadek(inst,narz).
przypadek(loc,miej).
przypadek(voc,wol).

rodzaj(Gend, r(_,_) ) :- var(Gend), !.
rodzaj(m1, r(mn(m(zy)),mo) ) :- !.
rodzaj(m1:depr, r(mn(m(zy)),mo) ) :- !. % do poprawki w Morfeuszu!
rodzaj(m1:ndepr, r(mn(m(zy)),mo) ) :- !.
rodzaj(m2, r(mn(m(zy)),nmo(m)) ) :- !.
rodzaj(m3, r(mn(m(nzy)),nmo(m)) ) :- !.
rodzaj(n1, r(mn(n),nmo(np(n))) ) :- !.
rodzaj(n2, r(mn(n),nmo(np(n))) ) :- !.
rodzaj(n1.n2, r(mn(n),nmo(np(n))) ) :- !.
rodzaj(f, r(nmn,nmo(z)) ) :- !.
rodzaj(m1.f, r(mn(m(zy)),mo) ).
rodzaj(m1.f, r(nmn,nmo(z)) ) :- !.
rodzaj(m3.f, r(mn(m(nzy)),nmo(m)) ).
rodzaj(m3.f, r(nmn,nmo(z)) ) :- !.
rodzaj(f.n2, r(nmn,nmo(z)) ).
rodzaj(f.n2, r(mn(n),nmo(np(n))) ) :- !.
rodzaj(p1,  r(mn(m(zy)),mo) ) :- !.
rodzaj(p2,  r(nmn,nmo(np(p))) ) :- !.
rodzaj(p3,  r(nmn,nmo(np(p))) ) :- !.
rodzaj(p2.p3,  r(nmn,nmo(np(p))) ) :- !.
rodzaj(p1.p2.p3,  r(mn(m(zy)),mo) ).
rodzaj(p1.p2.p3,  r(nmn,nmo(np(p))) ) :- !.
rodzaj(m1.m2.m3.n1.n2, r(mn(_),_)) :- !.
rodzaj(m1.m2.m3.n2.f, r(mn(_),_)).
rodzaj(m1.m2.m3.n2.f, r(nmn,nmo(z))) :- !.
rodzaj(m1.p1, r(_,mo)) :- !.
rodzaj(m2.m3.n1.n2.f.p2.p3, r(_,nmo(_)) ) :- !.
rodzaj(m2.m3.f.n1.n2.p2.p3, r(_,nmo(_)) ) :- !.
rodzaj(m2.m3.f.n1.n2.p1.p2, r(_,nmo(_)) ) :- !. % to jest błąd dla «ile»: p1???
rodzaj(n1.n2.p2.p3,  r(_,nmo(np(_))) ) :- !. % p1??
rodzaj(m1.m2.m3,   r(mn(m(_)),_) ) :- !.
rodzaj(m2.m3,r(mn(m(_)),nmo(m)) ) :- !.
rodzaj(m1.m2,r(mn(m(zy)),_) ) :- !.
rodzaj(m1.m3, r(mn(m(zy)),mo) ). % bez odcięcia!
rodzaj(m1.m3, r(mn(m(nzy)),nmo(m)) ) :- !.
rodzaj(m1.m2.m3.f.n1.n2,r(_,_)) :- !.
rodzaj(m1.m2.m3.f.n1.n2.p1.p2.p3,r(_,_)) :- !.
% dla liczebników:
rodzaj(m2.m3.n2,r(mn(_),nmo(_))) :- !. % m-1n
rodzaj(m1.m2.m3.n2,r(mn(_),_)) :- !. % mn
rodzaj(m2.m3.f.n2,r(mn(_),nmo(_))). % m-1n lub f:
rodzaj(m2.m3.f.n2,r(nmn,nmo(z))) :- !.
rodzaj(m1.m2.m3.f.n2,r(mn(_),_)). % mn lub f:
rodzaj(m1.m2.m3.f.n2,r(nmn,nmo(z))) :- !.
rodzaj(m1.m2.m3.f.n1.n2.p1.p2,r(_,_)) :- !.
rodzaj(n1.p1.p2,r(_,nmo(np(_)))) :- !.
% „pas” ma m3/n2
rodzaj(m3.n2, r(mn(m(nzy)),nmo(m)) ).
rodzaj(m3.n2, r(mn(n),nmo(np(n))) ) :- !.
%
rodzaj(mie,mie) :- !.
rodzaj(Gend,_) :- throw(morf(rodzaj,Gend)).

stopien(pos,row).
stopien(com,wyz).
stopien(sup,naj).

asagl(As:Agl,As,Agl) :- !.
asagl(As,As,_).

aspekt(_:_,_) :- !, fail.
aspekt(imperf,nd).
aspekt(perf,dk).
aspekt(imperf.perf,nd).
aspekt(imperf.perf,dk).

czas(fin,nd,ter).
czas(fin,dk,przy).
%czas(fin,imperf.perf,ter).
%czas(fin,imperf.perf,przy).

osoba(pri,1).
osoba(sec,2).
osoba(ter,3).

akomodacyjnosc(congr,uzg).
akomodacyjnosc(rec,nuzg).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
