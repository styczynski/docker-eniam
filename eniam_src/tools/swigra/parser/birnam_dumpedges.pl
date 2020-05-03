%
% Predykat wypisujący tylko użyteczne łuki lasu wynikowego w postaci
% nadającej się do ponownego wczytania.  Zmienne zostają ukonkretnione
% do takiego stanu, jaki faktycznie pojawi się w drzewach.  Wymaga to
% zduplikowania niektórych krawędzi.  Upraszcza się struktura danych —
% krawędź jest reprezentowana jako:
%
%   edge(Id, NT, Od, Do, [NReg/[EId1, Przez1, EId2, …], …], NumTrees)
%
%
% Copyright (C) 1997–2009 Marcin Woliński
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

dumpterm(T) :-
	copy_term(T,TC),
	numbervars(TC,0,_),
	write_term(TC,[portray(true),quoted(true)]),
	write('.'),
	nl.

nb_addval(Var, Current, Increment) :-
	nb_getval(Var, Current),
	New is Current + Increment,
	nb_setval(Var, New).

% Pozyskiwanie drzew analizy:

process_parses(NT,Od,Do) :-
	write(':-style_check(-singleton).'), nl,
	statprint(nonterminal,NT),
	statprint(startnode,Od),
	statprint(endnode,Do),
	nb_setval(useful_edge_number,0),
	nb_setval(number_of_trees,0),
	getforest(NT,Od,Do, _Children,TrId),
	counttrees(NT,Od,Do,TrId,NumTrees,_EId),
	nb_addval(number_of_trees,_,NumTrees),
	fail.

process_parses(_NT,_Od,_Do) :-
	nb_getval(number_of_trees,NumTrees),
	statprint(trees,NumTrees),
	nb_getval(useful_edge_number,N),
	statprint(useful_edges,N).

statprint(X,Y) :-
	dumpterm(info(X,Y)).

koniec :-
	dumpterm(sukces),
	nb_setval(parsing_result,accepted).

porazka :-
	dumpterm(porazka),
	nb_setval(parsing_result,rejected).

:- thread_local known_identifier/4.

% counttrees(terminal(_),_,_,_,1) :- 
%     !.
counttrees(_NT,_Od,_Do,TrId,_NumTrees,_EId) :-
	var(TrId), !, throw('Variable ID in a tree?!!').
counttrees(NT,_Od,_Do,TrId,NumTrees,EId) :-
	nonvar(TrId), known_identifier(TrId,NT1,NumTrees,EId), NT=@=NT1, !.
counttrees(NT,Od,Do,TrId,NumTrees,EId) :-
	nb_addval(useful_edge_number,EId,1),
	getforest(NT,Od,Do, Children,TrId),
	sumchildren(Children, NewChildren, Od, Do, NumTrees),
	assertz(known_identifier(TrId,NT,NumTrees,EId)),
	dumpterm(edge(EId,Od,Do,NT,NewChildren,NumTrees)).

sumchildren([_NReg/[]], [], _Od, _Do, 1) :- !.  % to był terminal
sumchildren([_NReg/xinfo(XI)], xinfo(XI), _Od, _Do, 1) :- !.  % to był terminal
sumchildren([NReg/Children | CC], [NReg/NewChildren|NCC], Od, Do, NumTrees) :-
	countchildren(Children,NewChildren,Od,Do,ChildTrees),
	sumchildren(CC, NCC, Od, Do, OtherTrees),
	NumTrees is ChildTrees + OtherTrees.
sumchildren([], [], _Od, _Do, 0).


countchildren([],[],_,_,1) :-!.
countchildren([TrId/NT],[EId],Od,Do,NumT) :- !,
	counttrees(NT,Od,Do,TrId,NumT,EId).
countchildren([TrId/NT,Przez | CC], [EId,Przez | NCC], Od, Do, NumTrees) :- !,
	counttrees(NT,Od,Przez,TrId,NumT,EId),
	countchildren(CC, NCC, Przez, Do, NumTT),
	NumTrees is NumT * NumTT.
countchildren(C,_,Od,Do,0) :-
    format(user_error,"~NInvalid children (~p-~p): ~p~n",[Od,Do,C]), fail.

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
