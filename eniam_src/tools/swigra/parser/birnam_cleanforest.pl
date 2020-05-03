%
% Predykat wypisujący wyszukujący użyteczne łuki w lesie wynikowym,
% zliczający drzewa i wyrzucający łuki nieużyteczne.  Po jego
% wykonaniu forest/5 zawiera tylko użyteczne łuki a pod każdym
% identyfikatorem łuku jest zarecordowana liczba poddrzew z tym łukiem
% jako korzeniem.
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

dumpterm(_T).

% Pozyskiwanie drzew analizy:

process_parses(NT,Od,Do) :-
	flag(useful_edge_number,_,0),
	flag(number_of_trees,_,0),
	forest(NT,Od,Do, _Children,TrId),
	counttrees(NT,Od,Do,TrId,NumTrees),
	flag(number_of_trees,N,N+NumTrees),
	fail.

process_parses(_NT,_Od,_Do) :-
	forest(NT,Od,Do, Children,TrId),
	\+ recorded(TrId,_),
	retract(forest(NT,Od,Do, Children,TrId)),
	fail.

process_parses(_NT,_Od,_Do) :-
	flag(number_of_trees,NumTrees,NumTrees),
	statprint(trees,NumTrees),
	flag(useful_edge_number,N,N),
	statprint(useful_edges,N).

statprint(_,_).

koniec.

porazka.


% zliczanie drzew i znakowanie użytecznych łuków:

% counttrees(terminal(_),_,_,_,1) :- 
%     !.
counttrees(_NT,_Od,_Do,TrId,_NumTrees) :-
	var(TrId), !, throw('Variable ID in a tree?!!').
counttrees(_NT,_Od,_Do,TrId,NumTrees) :-
	nonvar(TrId), recorded(TrId,NumTrees), !.
counttrees(_IgnNT,Od,Do,TrId,NumTrees) :-
	forest(_NT,Od,Do, Children,TrId),
	sumchildren(Children, _NReg, Od, Do, NumTrees),
	recorda(TrId,NumTrees),
%	dumpterm(forest(NT,Od,Do, Children,TrId)),
%	dumpterm(numsubtrees(TrId,NumTrees)),
	flag(useful_edge_number,N,N+1).

sumchildren([NReg/[]], NReg, _Od, _Do, 1) :- !.  % to był terminal
sumchildren([NReg/Children | CC], NReg, Od, Do, NumTrees) :-
	countchildren(Children,Od,Do,ChildTrees),
	sumchildren(CC, _NReg1, Od, Do, OtherTrees),
	NumTrees is ChildTrees + OtherTrees.
sumchildren([], _NReg, _Od, _Do, 0).


countchildren([],_,_,1) :-!.
countchildren([TrId/NT],Od,Do,NumT) :- !,
	counttrees(NT,Od,Do,TrId,NumT).
countchildren([TrId/NT,Przez | CC], Od, Do, NumTrees) :- !,
	counttrees(NT,Od,Przez,TrId,NumT),
	countchildren(CC, Przez, Do, NumTT),
	NumTrees is NumT * NumTT.
countchildren(C,Od,Do,0) :-
    format(user_error,"~NInvalid children (~p-~p): ~p~n",[Od,Do,C]), fail.

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
