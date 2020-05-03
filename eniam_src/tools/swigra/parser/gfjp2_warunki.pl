%
% Copyright (C) 2010 Marcin Woliński
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

równe( X, Y ) :- var(Y), !, X = Y. % dodane 091225
równe( X, [X|_] ).
równe( X, Y ) :- \+ Y = [_|_], !, X = Y.
równe( X, [_|Y] ) :- \+ Y = [], równe( X, Y ).
%równe( X, X ) :- atomic(X), X \== [].

różne( _, Y ) :- var(Y), !, fail. % dodane 100202
różne( E, [H | T] ) :- !,
        E \= H, różne( E, T ).
różne( _E, [] ) :- !.
różne( E, T ) :- T \= [_|_], !, E \= T.



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
