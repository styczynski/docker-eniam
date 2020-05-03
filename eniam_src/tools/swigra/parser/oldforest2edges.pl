%
% wariant birnam_dumpedges konwertujący z forest() na edge()
% trudnostka: w pewnym momencie zmieniliśmy sposób zapisywania terminali, tutaj stary.
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
%	write(':-'),
	numbervars(T,0,_),
%	writeq(T),
	write_term(T,[portray(true),quoted(true)]),
%	print(T),
	write('.'),
	nl.

% Pozyskiwanie drzew analizy:

process_parses(NT,Od,Do) :-
	write(':-style_check(-singleton).'), nl,
	statprint(nonterminal,NT),
	statprint(startnode,Od),
	statprint(endnode,Do),
	flag(useful_edge_number,_,0),
	flag(number_of_trees,_,0),
	getforest(NT,Od,Do, _Children,TrId),
	counttrees(NT,Od,Do,TrId,NumTrees,_EId),
	flag(number_of_trees,N,N+NumTrees),
	fail.

process_parses(_NT,_Od,_Do) :-
	flag(number_of_trees,NumTrees,NumTrees),
	statprint(trees,NumTrees),
	flag(useful_edge_number,N,N),
	statprint(useful_edges,N).

statprint(X,Y) :-
	dumpterm(info(X,Y)).

koniec :-
	dumpterm(sukces).

porazka :-
	dumpterm(porazka).


counttrees(_NT,_Od,_Do,TrId,_NumTrees,_EId) :-
	var(TrId), !, throw('Variable ID in a tree?!!').
counttrees(NT,_Od,_Do,TrId,NumTrees,EId) :-
	nonvar(TrId), recorded(TrId,t(NT1,NumTrees,EId)), NT=@=NT1, !.
counttrees(terminal(T),Od,Do,TrId,1,EId) :- 
    !,
    flag(useful_edge_number,EId,EId+1),
    recordz(TrId,t(terminal(T),1,EId)),
    dumpterm(edge(EId,Od,Do,terminal(T),[],1)).
counttrees(NT,Od,Do,TrId,NumTrees,EId) :-
	flag(useful_edge_number,EId,EId+1),
	getforest(NT,Od,Do, Children,TrId),
	sumchildren(Children, NewChildren, Od, Do, NumTrees),
	recordz(TrId,t(NT,NumTrees,EId)),
	dumpterm(edge(EId,Od,Do,NT,NewChildren,NumTrees)).

%sumchildren([_NReg/[]], [], _Od, _Do, 1) :- !.  % to był terminal
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

run(SentId) :-
	info(tekst, Tekst),
	info(nonterminal, NT),
	info(startnode, Od),
	info(endnode, Do),
%	info(trees, Trees),
%	info(useful_edges, Edges),
	info(parse_inferences, Infer),
	info(parse_cputime, CPU),
	statprint(sent_id, SentId),
	statprint(tekst, Tekst),
%	statprint(trees, Trees),
%	statprint(useful_edges, Edges),
	statprint(parse_inferences, Infer),
	statprint(parse_cputime, CPU),
	process_parses(NT,Od,Do),
	write('sukces.'), nl.


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
