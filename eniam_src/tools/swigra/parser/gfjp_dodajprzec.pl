%
% Moduł dodający fałszywe puste przecinki.
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

dodajprzec(Od, Do, Add) :-
	findall(input(Od,Do1,F,H,T), input(Od,Do1,F,H,T), Inp),
	dodajpierw(Inp, Add, Add1),
	findall(input(O2,D2,F2,H2,T2), przecspowalny(O2,D2,F2,H2,T2), Inp2),
	dodajprzecsp(Inp2,Add1,Add2),
	dodajprzec1(Od, Do, Add2).

dodajpierw([], Add, Add).
dodajpierw([input(Od,Do1,F,H,T)|Ogon], Add, AddN) :-
	dodajjeden(Od, Do1, F, H, T, Add, Add, Add1),
	dodajpierw(Ogon, Add1, AddN).

dodajprzec1(Od, Od, _) :- !.
dodajprzec1(Od, Do, Add) :-
	input(Od, Do1, F, H, T), !, % idziemy tylko jedną ścieżką!
	(przecinkowalny(T)
    ->
	dodajjeden(Od, Do1, F, H, T, Add, Add, Add1)
    ;
	Add=Add1
    ),
	dodajprzec1(Do1, Do, Add1).
	    
dodajjeden(_Od, _Do1, _F, _H, _T, Add0, Add, Add) :-
	Roz is Add - Add0,
	Roz >= 2, !. % ta stała rządzi liczbą dodanych przecinków

dodajjeden(Od, Do1, F, H, T, Add0, Add, AddN) :-
	Add1 is Add + 1,
	assertz(input(Od, Add1, '', ',', interp)),
	assertz(input(Add1, Do1, F, H, T)),
	dodajjeden(Add1, Do1, F, H, T, Add0, Add1, AddN).

przecinkowalny(interp).


% dodawanie przecsp:

% Dodanie "falbanek" przy jednym łuku grafu morfologicznego:

dodajjedenprzecsp(_Od, _Do1, _F, _H, _T, Add0, Add, Add) :-
	Roz is Add - Add0,
	Roz >= 2, !. % ta stała rządzi liczbą dodanych przecinków

dodajjedenprzecsp(Od, Do1, F, H, T, Add0, Add, AddN) :-
	Add1 is Add + 1,
	assertz(input(Od, Add1, F, H, T)),
	assertz(input(Add1, Do1, '', '', przecsp)),
	dodajjedenprzecsp(Od, Add1, F, H, T, Add0, Add1, AddN).


% kandydatem do dodania przecsp jest łuk spójnikowy, po którym stoi
% nieinterpunkcja (przynajmniej jedna z rzeczy dalej nie jest interp): 

przecspowalny(Od,Do,F,H,conj) :-
	input(Od,Do,F,H,conj),
	przecspo(H),
	input(Do,_,_,_,T), T \= interp.

przecspo(albowiem).
przecspo(aż).
przecspo(bo).
przecspo(chociaż).
przecspo(choć).
przecspo(czy).
przecspo(dopóki).
przecspo(dopóty).
przecspo(gdy).
przecspo(gdyż).
przecspo(iż).
przecspo(jak).
przecspo(jeśli).
przecspo(jeżeli).
przecspo(kiedy).
przecspo(nim).
przecspo(ponieważ).
przecspo(póki).
przecspo(póty).
przecspo(skoro).
przecspo(wówczas).
przecspo(wtedy).
przecspo(zanim).
przecspo(że).

% dodaje przecspy do wszystkich łuków z pierwszego argumentu:

dodajprzecsp([], Add, Add).
dodajprzecsp([input(Od,Do1,F,H,T)|Ogon], Add, AddN) :-
	dodajjedenprzecsp(Od, Do1, F, H, T, Add, Add, Add1),
	dodajprzecsp(Ogon, Add1, AddN).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
