% Zapis lasu w formie XML
%
% Copyright (C) 2009 Marcin Woliński
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

nonterminal(wypowiedzenie,[]).
nonterminal(zdanie, [wyróżnik,aspekt,czas,tryb,rodzaj/liczba,osoba,neg,dest,ink,ign,ctr]).
nonterminal(zdanioid, [neg,dest,ink,ign,ctr]).
nonterminal(ff,[wyróżnik,aspekt,czas,tryb,rodzaj/liczba,osoba,rekcja,neg,dest,ink,ign,ctr]).
nonterminal(fw,[tfw, aspekt, czas, rodzaj/liczba, osoba,poz,neg,dest,ink,ign,ctr]).
nonterminal(fl,[aspekt, czas, rodzaj/liczba, osoba, neg, dest, ink,ign,ctr]).
nonterminal(fwe,[wyróżnik,aspekt,czas,tryb,rodzaj/liczba,osoba,rekcja,neg,dest,ink,ign,ctr]).
nonterminal(fno,[przypadek, rodzaj/liczba, osoba, rekcja, klasa, zap, poz, neg, dest, ink, ign, ctr]).
nonterminal(formarzecz,[przypadek, rodzaj/liczba, rekcja, ctr]).
nonterminal(zaimos,[przypadek, rodzaj/liczba, osoba, ctr]).
nonterminal(zaimrzecz,[klasa, przypadek, rodzaj/liczba, ctr]).
nonterminal(zaimprzym,[klasa, przypadek, rodzaj/liczba, ctr]).
nonterminal(flicz,[przypadek, rodzaj/liczba, akom, zap, dest, ink, ign, ctr]).
nonterminal(formalicz,[przypadek,rodzaj/liczba,akom, klasa, ctr]).
nonterminal(fzd,[tfz, kor, aspekt, czas, tryb, neg, ink, ign, ctr]).
nonterminal(fpt,[przypadek, rodzaj/liczba, stopień, rekcja, klasa, zap, neg, dest, ink, ign, ctr]).
nonterminal(modjaki,[przypadek, rodzaj/liczba, zap, dest, ink, ign, ctr]).
nonterminal(fps,[stopień, klasa, neg, dest, ink, ign, ctr]).
nonterminal(formaprzys,[stopień, klasa, neg, dest, ctr]).
nonterminal(modpart, [klasa, neg, dest, ink, ign, ctr]).
nonterminal(formaprzym,[przypadek, rodzaj/liczba, stopień, rekcja, ctr]).
nonterminal(formaczas,[wyróżnik, aspekt, czas, tryb, rodzaj/liczba, osoba, rekcja, neg, ctr]).
nonterminal(formaczas1,[ign,wyróżnik, aspekt, czas, tryb, rodzaj/liczba, osoba, rekcja, ctr]).
nonterminal(przyzlo,[ign, rodzaj/liczba, rekcja, ctr]).
nonterminal(condaglt,[liczba, osoba,ctr]).
nonterminal(posiłk,[klasa,rodzaj/liczba,dest,ink,ign,ctr]).
nonterminal(fpm,[przyim, przypadek, klasa, zap, neg, dest, ink, ign, ctr]).
nonterminal(fpmpt,[przyim, przypadek, klasa, zap, neg, dest, ink, ign, ctr]).
nonterminal(przyimek,[przyim, przypadek,ctr]).
nonterminal(formawykrz,[rekcja,ctr]).
nonterminal(znakkonca,[dest,ctr]).
nonterminal(korelat,[typ,ink,ctr]).
nonterminal(spójnik,[typ,ozn,ink,ctr]).
nonterminal(spoj1,[typ,ozn]).
%nonterminal(spójnik,[ozn]).
nonterminal(partykuła,[ozn,ctr]).
nonterminal(przec,[ign,ctr]).
nonterminal(cudz,[typc,ctr]).
nonterminal(nawias,[typn,kszt,ctr]).
nonterminal(pauza,[ctr]).
nonterminal(dwukropek,[ctr]).

:-[library(sgml_write)].

portray('$VAR'(X)) :- format("_", [X]).


edges2xml(N,N,[]) :- !.
edges2xml(N,NN, [E|EE]) :-
	edge2xml(N,E),
	N1 is N+1,
	edges2xml(N1,NN,EE).

edge2xml(Id,element(node,
		    [nid=Id,from=Od,to=Do,subtrees=NumTrees|NodeAttr],
		    [LabelElem|Rules])) :-
	edge(Id, Od, Do, NT, Children, NumTrees),
	label2xml(NT, Children, NodeAttr, LabelElem, Ctr),
	(nonvar(Ctr) -> get_head(Id, Ctr) ; get_head(Id,false)),
	rules2xml(Children,Od,Do,Rules).

label2xml(terminal(T), xinfo([NKJP, TId, InterpId, Space]),
	[], element(terminal,[token_id=TId, interp_id=InterpId,
			         disamb=NKJPAttr | SpAttr],TermElems),
				 true) :- 
	nonvar(NKJP), !, % sprawdzamy, czy faktycznie są atrybuty xinfo
	sprawdzspację(Space, SpAttr),
	sprawdzNKJP(NKJP, NKJPAttr),
	numbervars(T,0,_),
	morf2xml(T,TermElems).

% to na okoliczność plików, które nie są wynikiem parsowania NKJP i
% nie mają xinfo:
label2xml(terminal(T), _,
	[], element(terminal,[],TermElems), true) :- !,
	numbervars(T,0,_),
	morf2xml(T,TermElems).

label2xml(NT, _Children, [], element(nonterminal,[],
	                       [element(category,[],[Cat])|Features]), Ctr) :-
	numbervars(NT,0,_),
	NT =.. [Cat | Args],
	(nonterminal(Cat,ArgSpec) -> true; throw(unknown_nonterminal(NT))),
	length(Args,L1), length(ArgSpec,L2),
	(L1 is L2 -> true; throw(spec_mismatch(Cat,L1,L2,Args,ArgSpec))),
	args2xml(ArgSpec, Args, Features, [], Ctr).


morf2xml(morf(F,H,T),[element(orth,[],[F]),
	              element(base,[],[H]),
		      element(f,[type=tag],[StrT])]) :- !,
	format(atom(StrT),'~p',[T]).
morf2xml(T,[]) :-
	throw(dziwny_terminal(T)).

sprawdzspację(sp, []).
sprawdzspację(nps, [nps=true]).

sprawdzNKJP(nkjp:tak, true).
sprawdzNKJP(nkjp:nie, false).

args2xml([], [], Features, Features, _) :- !.
% ten argument ma być ignorowany:
args2xml([ign | ArgSpec], [_A|Args], F0, F1, Ctr) :- !,
	args2xml(ArgSpec, Args, F0, F1, Ctr).
% ten argument określa bycie nad/podrzędnikiem:
args2xml([ctr | ArgSpec], [A|Args], F0, F1, Ctr) :- !,
	ctr2xml(A,Ctr),
	args2xml(ArgSpec, Args, F0, F1, _Ctr).
args2xml([Spec|ArgSpec], [Arg|Args], 
	 [element(f,[type=Spec],[StrArg]) | FF], Features, Ctr) :-
	atom(Spec), !,
	numbervars(Arg,0,_),
	format(atom(StrArg),'~p',[Arg]),
	args2xml(ArgSpec, Args, FF, Features, Ctr).
% Spec jest termem złożonym (np. R/L) a Arg było zmienną:
args2xml([Spec|ArgSpec], ['$VAR'(_)|Args], F0, Features, Ctr) :- !,
	Spec =.. [_SpHd|SpArgs],
	args2xml(SpArgs, _AArgs, F0, FF, Ctr), % uwaga: _AArgs może należałoby ukonkretnić?
	args2xml(ArgSpec, Args, FF, Features, Ctr).
% Spec jest termem złożonym (np. R/L):
args2xml([Spec|ArgSpec], [Arg|Args], F0, Features, Ctr) :-
	Spec =.. [SpHd|SpArgs],
	Arg =.. [AHd|AArgs],
	(SpHd=AHd -> true; throw(spec_pred_mismatch(SpHd,AHd))),
	length(SpArgs,L1), length(AArgs,L2),
	(L1 is L2 -> true; throw(spec_len_mismatch(SpHd,L1,L2))),
	args2xml(SpArgs, AArgs, F0, FF, Ctr),
	args2xml(ArgSpec, Args, FF, Features, Ctr).

ctr2xml(na,true) :- !.
ctr2xml(po,false) :- !.
ctr2xml(_,false).

% unifikuje Val z N-tą komórką globalnej tablicy głów:
get_head(N, Val) :-
	N1 is N + 1,
	b_getval(heads,H), arg(N1, H, Val).

rules2xml([],_Od,_Do,[]) :- !.
rules2xml(xinfo(_),_Od,_Do,[]) :- !.
rules2xml([NReg/Children],Od,Do,[element(children,[rule=SNReg],R)]) :-
	format(atom(SNReg),'~p',[NReg]),
	rule2xml(Children,Od,Do,R).
rules2xml([NReg/Children|CC],Od,Do,[element(children,[rule=SNReg],R)|RR]) :-
	format(atom(SNReg),'~p',[NReg]),
	rule2xml(Children,Od,Do,R),
	rules2xml(CC,Od,Do,RR).

rule2xml([],_Od,_Do,[]) :-!.
rule2xml([TrId],Od,Do,[element(child,[nid=TrId,from=Od,to=Do,head=H],[])]) :- 
	!, get_head(TrId,H).
rule2xml([TrId,Przez | CC],Od,Do,[element(child,[nid=TrId,from=Od,to=Przez,head=H],[])
			    |RR]) :- !, 
    get_head(TrId,H),
    rule2xml(CC,Przez,Do,RR).


forest2xml(Stream) :- 
	info(tekst, Tekst),
	info(sent_id,SentId),
	info(grammar_no, GramNo),
	(info(trees, Trees) ->
	    info(parse_inferences, Infer),
	    info(parse_cputime, CPU) 
	;
	    Trees = 0,
	    Infer = inf,
	    CPU = inf
	),
	( Trees = 0 ->
	  xml_write(Stream,
		    [element(forest,[sent_id=SentId, grammar_no=GramNo],
			     [element(text,[],[Tekst]),
			      element(stats,[trees=Trees,
					     inferences=Infer,
					     cputime=CPU],[])
			     ])
		    ], [])
	;
	  info(nonterminal, NT),
	  info(startnode, Od),
	  info(endnode, Do),
	  info(useful_edges, Edges),
	  % to będzie tablica głów fraz:
	  NH is Edges + 1, functor(Heads,h,NH), b_setval(heads,Heads),
	  %
	  edges2xml(0,Edges,Nodes),
	  xml_write(Stream,
		    [element(forest,[sent_id=SentId, grammar_no=GramNo],
			     [element(text,[],[Tekst]),
			      element(startnode,[from=Od,to=Do],[NT]),
			      element(stats,[trees=Trees,
					     nodes=Edges,
					     inferences=Infer,
					     cputime=CPU],[])
			     |
			     Nodes
			     ])
		    ], [])
	).

f2x :- forest2xml(user_output).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
