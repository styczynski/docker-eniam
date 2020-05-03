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

zmienpraep(Add, Add1) :-
	setof(input(Od,Do,F,H,TT),
	  once((input(Od,Do,F,H,TT),
	    pałkowa(TT,ppron3:_:_:_:_:_:Praep), Praep==praep)),
	  Inp), !,
	zmienpraep1(Inp,Add, Add1).
zmienpraep(Add, Add).

zmienpraep1([], Add, Add).
zmienpraep1([P|PP], Add, AddN) :-
	retract(P),
	Add1 is Add + 1,
	P = input(Od, Do, F, H, T),
	bagof(input(O1, Add1, F1, H1, prep:T1),
	  input(O1, Od, F1, H1, prep:T1), Poprzedniki), !,
	asserta(input(Add1, Do, F, H, T)),
	zmienpoprzedniki(Poprzedniki),
	zmienpraep1(PP, Add1, AddN).
zmienpraep1([_|PP], Add, AddN) :-
	zmienpraep1(PP, Add, AddN).

zmienpoprzedniki([]).
zmienpoprzedniki([P|PP]) :-
	asserta(P),
	zmienpoprzedniki(PP).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
