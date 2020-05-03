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

% uwaga! w mymember/2 zakładamy, że lista jest ustalona.
mymember( E, [E|_] ) :- !.
mymember( E, [_|L] ) :- mymember( E, L ).

rowne( X, Y ) :- var(Y), !, X = Y. % dodane 091225
rowne( X, [X|_] ).
rowne( X, [_|Y] ) :-  rowne( X, Y ).
rowne( X, X ) :- atomic(X), X \== [].

rozne( E, [H | T] ) :- !,
        E \== H, rozne( E, T ).
rozne( _E, [] ) :- !.
rozne( E, T ) :- atomic(T), E \== T.

% Obliczenie niepytajności
% oblnp( Z, Z1 ), gdzie Z ustalona lista wartości
% warunek filtruje z listy tylko wartości np, npx, npxx zamieniając 
% je w p, px, pxx odpowiednio

oblnp( [N | Z], [P | Z1] ) :- npp(N,P), !, oblnp1( Z, Z1 ).
oblnp( [_ | Z], Z1 ) :- !, oblnp( Z, Z1 ).
oblnp1( [], [] ) :- !.  % tymczasowe odcięcie do wyeliminowania nieustalonych Z
oblnp1( [N | Z], [P | Z1] ) :- npp(N,P), !, oblnp1( Z, Z1 ).
oblnp1( [_ | Z], Z1 ) :- oblnp1( Z, Z1 ).

npp('np','p').
npp('npx','px').
npp('npxx','pxx').

% oblpnp( Z2, Z ) zastępuje następujący zestaw warunków oryginału:
% { rowne(Z, ['p','px','pxx']), 
%   rowne(Z2, [Z,Z1]), 
%   oblnp(Z1, Z) }

oblpnp( Z, NZ ) :- oblpnp1(Z,Z1), makeset(Z1,NZ), NZ \= [].
oblpnp1( [], [] ) :- !.  % tymczasowe odcięcie j.w.
oblpnp1( [N | Z], [P | Z1] ) :- pnpp(N,P), !, oblpnp1( Z, Z1 ).
oblpnp1( [_ | Z], Z1 ) :- oblpnp1( Z, Z1 ).

pnpp('np','p').
pnpp('npx','px').
pnpp('npxx','pxx').
pnpp('p','p').
pnpp('px','px').
pnpp('pxx','pxx').


% Obliczenie negacji

%oblneg(_Oz,_Neg0,nie(ani),nie(ani)) :- !.
%oblneg(Oz, Neg0, _Neg, _Neg1) :- \+ Oz=ani.

oblneg(_,_,Neg1, Neg2) :- (var(Neg1); var(Neg2)), 
	format(user_error,"~NNieustalona wartość w oblneg!~n", []), fail.
oblneg(Oz, tak, tak, _) :- !, Oz \== ani.
oblneg(Oz, tak, _, tak) :- !, Oz \== ani.
oblneg(ani, nie(ani), nie(ani), nie(ani)) :- !.
oblneg(Oz, nie(X), nie(X), nie(X)) :- var(X), !, Oz \== ani.
oblneg(Oz, nie(nie), nie(_), nie(_)) :- Oz \== ani.


% obliczenie zależności
% tabela nr 2 na s. 172.
% oblzal (Z1, Z, Ow)
% Z1 - zal. podrzędnika, Ow - ogr. wewnętrzne ---> Z - zal. nadrzędnika

oblzal( Z, Z, Ow) :- var(Ow), !.
oblzal( Z, _, _ ) :- var(Z), !,
	format(user_error,"~NNieustalona wartość w oblzal!~n", []), fail.
oblzal( Z, Z, br).
oblzal( Z1, Z, Ow) :- rowne(Ow, ['choćby','gdy','gdyby']), !,
	mapzalx(Z1,Z), Z\=[].
oblzal( Z1, Z, 'dopóki') :- !, mapzalxx(Z1, Z), Z \= [].
oblzal( Z1, Z, 'więc') :- mapzalt(Z1,Z), Z\=[].

mapzalx([],[]).
mapzalx([Z1|ZZ1],Z) :- xzal(Z1,Z-ZZ), !, mapzalx(ZZ1,ZZ).
mapzalx([_|ZZ1],Z) :- mapzalx(ZZ1,Z).

mapzalxx([],[]).
mapzalxx([Z1|ZZ1],Z) :- xxzal(Z1,Z-ZZ), !, mapzalxx(ZZ1,ZZ).
mapzalxx([_|ZZ1],Z) :- mapzalxx(ZZ1,Z).

mapzalt([],[]).
mapzalt([Z1|ZZ1],Z) :- tzal(Z1,Z-ZZ), !, mapzalt(ZZ1,ZZ).
mapzalt([_|ZZ1],Z) :- mapzalt(ZZ1,Z).

xzal(px, [p,px|Z]-Z).
xzal(pxx, [pxx|Z]-Z).
xzal(npx, [np,npx|Z]-Z).
xzal(npxx, [npxx|Z]-Z).
xzal(npxt, [npt,npxt|Z]-Z).
xzal(npxxt, [npxxt|Z]-Z).
xzal(box, [bo,box,bowiem,'chociaż',czy,jak,'jeśli',
	podczas,'ponieważ','że'|Z]-Z).
xzal(Z,[Z|ZZ]-ZZ) :- rowne(Z, ['aż1','aż2',gdy,zanim,'aż1xx',
	'boxx','byxx','dopóki',
	'gdyxx','zanimxx','choćby','czyżby','gdyby','jakby','jakoby','żeby']).

xxzal(pxx,[p,px,pxx|Z]-Z).
xxzal(npxx,[np,npx,npxx,npt,npxt|Z]-Z).
xxzal(npxxt,[npxxt|Z]-Z).
xxzal(boxx, [bo,boxx,bowiem,'chociaż',czy,jak,'jeśli',
	podczas,'ponieważ','że'|Z]-Z).
xxzal('aż1xx', ['aż1','aż1xx'|Z]-Z).
xxzal(gdyxx, ['aż2',box,gdy,gdyxx|Z]-Z).
xxzal(zanimxx, [zanim,zanimxx|Z]-Z).
xxzal(byxx, [byxx,'choćby','czyżby','gdyby','jakby','jakoby','żeby'|Z]-Z).
xxzal('dopóki',['dopóki'|ZZ]-ZZ).

tzal(npt, [np,npt|Z]-Z).
tzal(npxt, [npx,npxt|Z]-Z).
tzal(npxxt, [npxx,npxxt|Z]-Z).
tzal(Z,[Z|ZZ]-ZZ) :- rowne(Z, [bo,bowiem,'chociaż',czy,jak,'jeśli',podczas,
	'ponieważ','że','aż1','aż2',gdy,zanim,
	'aż1xx','boxx','byxx','dopóki',	'gdyxx','zanimxx',
	'choćby','czyżby','gdyby','jakby','jakoby','żeby']).

% obliczenie klasy
% definicja na podstawie opisu na s. 333
% trzeba będzie sprawdzić, o co na prawdę chodzi.
% można podejrzewać, że drugi i trzeci parametr są wejściowe,
% a pierwszy wyjściowy.

oblkl( tk, tk, _) :- !.
oblkl( tk, _, tk) :- !.
oblkl( Kl, _, Kl) :-
	Kl \== tk.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% warunki typowane
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Zależność

% $Z = X \cap Y$ i $Z \neq \emptyset$
zrowne( L, Z, Z ) :- var(L), !.%,
%	format(user_error, "~N Warning: Nie ustalone Z w zrowne~n", []).
zrowne( X, Y, Z ) :- przetnij( X, Y, Z ).

% $Z=X\setminus Y$ i $Z \neq \emptyset$
zrozne( L, Y, L ) :- var(L), !,
	format(user_error, "~N Warning: Nie ustalone Z w zrozne(Z,~p).~n", [Y]).
zrozne( X, Y, Z ) :- usunliste( Y, X, Z ), Z \= [].

% zplubnp( Z1, Z2, SwZ1, SwZ2 )
% zastępuje warunki
% { rowne(p, [Z1, Z2]), rowne(Z1, ['np','p']), rowne(Z2, ['np','p'])}
% czyli: Z1 i Z2 pytajne lub niepytaje, ale przynajmniej jeden pytajny.
% Ostatnie dwa argumenty przekazują obliczone świdzińskizacje odpowiednich
% zbiorów zależności.

zplubnp( Z1, Z2, SwZ1, SwZ2 ) :-
	zrowne( Z1, [p], SwZ1 ), !,
	zrowne( Z2, [np,p], SwZ2 ).
zplubnp( Z1, Z2, SwZ1, SwZ2 ) :-
    	zrowne( Z1, [np], SwZ1 ),
	zrowne( Z2, [p], SwZ2 ).

% Pomocnicze:

przetnij( [E|XX], YY, [E|ZZ] ) :-
	mymember( E, YY ), !,
	przetnij1( XX, YY, ZZ ).
przetnij( [_|XX], YY, ZZ ) :-
	przetnij( XX, YY, ZZ ).
przetnij1( [], _, [] ).
przetnij1( [E|XX], YY, [E|ZZ] ) :-
	mymember( E, YY ), !,
	przetnij1( XX, YY, ZZ ).
przetnij1( [_|XX], YY, ZZ ) :-
	przetnij1( XX, YY, ZZ ).

usunliste( [], X, X ).
usunliste( [E|XX], Y, Z ) :-
	usun( E, Y, Z1 ),
	usunliste( XX, Z1, Z ).
usun( _, [], [] ).
usun( E, [E|Y], Y ) :- !.
usun( E, [X|Y], [X|Z] ) :- usun(E, Y, Z).

makeset( [], [] ).
makeset( [E|X], Y ) :- mymember(E,X), !, makeset( X, Y ).
makeset( [E|X], [E|Y]) :- makeset(X,Y).


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
