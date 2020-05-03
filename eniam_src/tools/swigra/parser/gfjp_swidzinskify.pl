%
% Świdzińskizacja wyników analizy.
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

% Rodzaje:
% zbiory:
swirodzaj('_', r('$VAR'(_),'$VAR'(_)) ) :- !.
swirodzaj(mn,  r(mn('$VAR'(_)),'$VAR'(_)) ) :- !.
swirodzaj(nmo, r('$VAR'(_),nmo('$VAR'(_))) ) :- !.
swirodzaj(np,  r('$VAR'(_),nmo(np('$VAR'(_)))) ) :- !.
swirodzaj(m,   r(mn(m('$VAR'(_))),'$VAR'(_)) ) :- !.
swirodzaj(mnos,r(mn(m('$VAR'(_))),nmo(m)) ) :- !.
swirodzaj('mżyw',r(mn(m(zy)),'$VAR'(_)) ) :- !.
% nie wiem, skąd takie, ale czasem przychodzi:
swirodzaj(mos, r('$VAR'(_),mo) ) :- !.
% elementarne:
swirodzaj(mos, r(mn(m(zy)),mo) ) :- !.
swirodzaj(mzw, r(mn(m(zy)),nmo(m)) ) :- !.
swirodzaj('mnż', r(mn(m(nzy)),nmo(m)) ) :- !.
swirodzaj(nij, r(mn(n),nmo(np(n))) ) :- !.
swirodzaj('żeń', r(nmn,nmo(z)) ) :- !.
swirodzaj(pt,  r(nmn,nmo(np(p))) ) :- !.

% Zależność:

zaleznosc( Z, _, Z ) :- var(Z), !.
zaleznosc( [Z], _, Z1 ) :- zaleznosc( Z, _, Z1 ).
zaleznosc( npp([Z]), _, Z1 ) :- npp( Z1, Z ).
zaleznosc( pnpp([Z]), Z1, NZ ) :- swipnp(Z,Z1,Z2), zaleznosc(Z2,_,NZ).
zaleznosc( ow(Z,Ow), _, NZ ) :- zaleznosc(Z,_,Z1), swioblzal(Z1,Ow,NZ).
zaleznosc( Z, _, Z ).

% Świdzińskizacja warunku oblpnp:

swipnp(Z,Z1,Z2) :- setof(NZ, (pnpp(NZ,Z), mymember(NZ,Z1)), Z2).

% Świdzińskizacja wyników oblzal:

swioblzal(Z, Ow, Z) :- var(Ow).
swioblzal(Z, '$VAR'(Ow), Z) :- var(Ow).
swioblzal(Z, br, Z).
%swioblzal(np, 'dopóki', npxx).
swioblzal(Z, Ow, Z1) :- rowne(Ow, ['choćby','gdy','gdyby']), !,
	xzal(Z1,ZZ-[]), mymember(Z,ZZ).
swioblzal(Z, 'dopóki', Z1) :- !,
	xxzal(Z1,ZZ-[]), mymember(Z,ZZ).
swioblzal(Z, 'więc', Z1) :- 
	tzal(Z1,ZZ-[]), mymember(Z,ZZ).

% Portray:

portray(z(SwZ,Z)) :- var(SwZ), !, print(Z).
portray(z(SwZ,Z)) :- zaleznosc(SwZ,Z,PrZ), print(PrZ).
portray(r(A,B)) :- swirodzaj( PrR, r(A,B)), print(PrR).

:-op(100, fy, @).
%portray(@ _) :- write('_').
portray(@ 0) :- write(1).
portray(@ @ 0) :- write(2).
portray(@ @ @ 0) :- write(3).
portray(@ @ @ @ 0) :- write(4).
portray(@ @ @ @ @ 0) :- write(5).
portray(@ @ @ @ @ @ 0) :- write(6).
portray(@ @ @ @ @ @ @ 0) :- write(7).

% negacja:
portray(nie('$VAR'(_))) :- write(nie).
portray(nie(nie)) :- write(nie).
portray(nie(ani)) :- write(ani).
portray(tak) :- write(tak). % to dla req(tak), żeby nie zawodziło
portray(req(Neg)) :- portray(Neg).

% wymagania:
portray(wym(SwW,_)) :- write_term(SwW,[portray(true),quoted(true)]).
portray(wym(SwW,_,_)) :- write_term(SwW,[portray(true),quoted(true)]).

portray('$VAR'(X)) :- format("_", [X]).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
