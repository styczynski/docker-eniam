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

:-[gettreetex].
:-[gfjp_warunki].

dumpterm(T) :-
	write(':-'),
	writeq(T),
	write('.'),nl.

process_info :-
	info(X,Y),
	format("\\info{~a}{~p}\n",[X,Y]),
	fail.
process_info.

process_forest(NT,Od,Do) :-
	flag(trees,_,0),
	format(user_error,"Generating tree signatures ",[]),
	gettreesgnt(NT,Od,Do,_TrId,Sgnt),
	flag(trees,N,N+1), N1 is N+1,
	assertz(treesgnt(Sgnt,N1)),
 	(0 is N1 mod 100 -> format(user_error,"+",[])),
	fail.
process_forest(NT,Od,Do) :-
	flag(trees,_,0),
%	listing(treesgnt),
	format(user_error,"~nGenerating trees ",[]),
	gettree(NT,Od,Do,_TrId,Drzewo),
	flag(trees,N,N+1), N1 is N+1,
	format("% ~p~n", [N1]),
	numbervars(Drzewo,0,_),
	treeprint(Drzewo,N1),
 	(0 is N1 mod 100 -> format(user_error,"+",[])),
	fail.
process_forest(_,_,_) :-
	format(user_error,"~n",[]).


% wypisujemy informacje:
:- process_info.

% wypisujemy drzewa:
:-
	info(nonterminal,NT),
	info(startnode,Od),
	info(endnode,Do),
	process_forest(NT,Od,Do).

% podsumowanie:
:-
	sukces,
	format("\\koniecprzykladu\n", [])
	;
	porazka,
	format("\\porazka\n", []).

:-
	format("\n%%% \x4Cocal Variables:\n", []),
	format("%%% coding: utf-8\n",[]),
	format("%%% mode: latex\n",[]),
	format("%%% End: \n",[]).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
