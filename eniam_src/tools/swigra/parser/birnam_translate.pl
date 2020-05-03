%
% Predykat translaterule/3 tłumaczący pojedynczą regułę gramatyczną na 
% klauzulę prologową. 
%
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


translaterule( LHSin, RHSin, LHSout, RHSout, First ) :-
	conjtolist( RHSin, RHSlist, [] ),
	firstrhs( RHSlist, First, NrR, RHSlist2 ),
	addargs( First,  Wpocz, W0, FId, LHSout ),
	addargs( LHSin,  Wpocz, Wn1, TrId, Head ),
	transrhs( RHSlist2, PP, W0, Wn1, RHSoutlist,
	          [ register(LHSin, Wpocz, Wn1, NrR, [FId/First | PP], TrId), 
		    Head] ),
%	listtoconj( [first(LHSin,Goal)|RHSoutlist], RHSout ).
	listtoconj( RHSoutlist, RHSout ).


% conjtolist/3:
% Poniższy predykat obchodzi drzewo przecinkowe w głąb i generuje 
% listę jego liści. 

conjtolist( X, [X|LL], LL ) :- var(X), !.
conjtolist( (X,Y), L, LL ) :- !,
	conjtolist( X, L, L1 ),
	conjtolist( Y, L1, LL ).
conjtolist( X, [X|LL], LL ).

% listtoconj/3:
% zamiana listy na term przecinkowy.

% listtoconj( [X], X ).
% listtoconj( [X|XX], (X,Y) ) :- listtoconj( XX, Y ).


% addargs/8:
% dodaje argumenty administracyjne do nieterminala NT

addargs(NT, In, Out, TrId, NTArg) :-
	NT =.. [Head | Args],
	NTArg =.. [Head, In, Out, TrId | Args].

% firstrhs:
% ujmuje z prawej strony reguły pierwszy nieterminal i ewentualnie numer
% reguły.

% !!! metawywołania!

firstrhs( [s(NrR) | R], First, NrR, RR) :- !,
	firstrhs( R, First, NrR, RR).
firstrhs( [{X} | R], F, NrR, RR ) :- !,
	format(user_error,
	       "~NCannot start rule with a condition {~p}.  Ignoring.~n", [X]),
	firstrhs( R, F, NrR, RR ).
firstrhs( [[T] | RR], terminal(T), NrR, RR ) :- !,
	(nonvar(NrR) ; NrR='UNKNOWN').
firstrhs( [First | RR], First, NrR, RR) :-
	nonvar(NrR) ; NrR='UNKNOWN'.
firstrhs( [], _, _, _ ) :- 
	format(user_error, '~NDetected epsilon rule !~n', []), nl,
	fail.

% transrhs:
% tłumaczenie prawej strony reguły.

transrhs( [s(NrR) | RR], PP, W0, Wn, O, OO ) :- !,
	format(user_error, "~NMisplaced rule number (~p). Ignoring.~n", [NrR]),
	transrhs( RR, PP, W0, Wn, O, OO).
%transrhs( [przec | RR], [quasicomma|PP], W0, Wn, [comma(W0) | O], OO ) :-
%	transrhs( RR, PP, W0, Wn, O, OO ).
transrhs( [{X} | RR], PP, W0, Wn, O, OOO ) :- !,
	conjtolist( X, O, OO ),
	transrhs( RR, PP, W0, Wn, OO, OOO ).
transrhs( [[X] | RR], [W0,TrId/terminal(X)|PP], W0, Wn,
	  [goal(terminal(X), W0, W1, TrId) | O], OO) :- !,
	transrhs( RR, PP, W1, Wn, O, OO ).
transrhs( [wymagania( IW, W, OW, FF, FW ) | RR ], P, W0, Wn,
	  [wymagania( P, PP, W0, W1, IW, W, OW, FF, FW ) | O], OO) :- !,
	transrhs( RR, PP, W1, Wn, O, OO ).
transrhs( [wymagane( W, OW, FW ) | RR ], P, W0, Wn,
	  [wymagane( P, PP, W0, W1, W, OW, FW ) | O], OO) :- !,
	transrhs( RR, PP, W1, Wn, O, OO ).
transrhs( [sequence_of( Elems ) | RR ], P, W0, Wn,
	  [do_sequence_of( P, PP, W0, W1, ElemsCode ) | O], OO) :- !,
	przygotuj_sekwencję(Elems, ElemsCode),
	transrhs( RR, PP, W1, Wn, O, OO ).
transrhs( [optional( NT, TC, FC ) | RR ], P, W0, Wn,
	  [optional( P, PP, W0, W1, NT, TC, FC ) | O], OO) :- !,
	transrhs( RR, PP, W1, Wn, O, OO ).
transrhs( [ NT | RR], [W0,TrId/NT|PP], W0, Wn,  
	  [goal(NT, W0, W1, TrId) | O], OO ) :-
	transrhs( RR, PP, W1, Wn, O, OO).
transrhs( [], [], W, W, O, O ).



save_rulehead( NT ) :-
	functor( NT, Name, Arity ),
	Arity4 is Arity + 3,
	functor( Head, Name, Arity4 ),
	(rulehead( Head ), !
    ;
	assertz( rulehead(Head) )).


% gen_rule( Head ) :-
% 	Head =.. [NT, Goal, P, P, S, S | Args],
% 	Goal =.. [NT | Args],
% 	portray_clause( Head ), nl,
% 	fail.
gen_rule( LHS ) :-
	(
	  rule( LHS, RHS ) 
	*->
	portray_clause( (LHS:-RHS) ), nl
      ;
	portray_clause( (LHS:-fail) ), nl
      ),
	fail.


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
