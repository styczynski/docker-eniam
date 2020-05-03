% Wydobywanie drzew z lasu (edge/6) z zapisem w postaci TeXowej.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nazwijmy wierzchołkiem alternatywy łuk lasu odpowiadający jednostce
% nieterminalnej, która może być zrealizowana za pomocą więcej niż
% jednej reguły.  Chcemy udekorować manifestację każdego wierzchołka
% alternatywy w drzewie analizy etykietką z hiperlinkami do
% poprzedniej i następnej możliwej realizacji tego wierchołka (a więc
% do drzew, które różnią się tylko poddrzewami tego wierzchołka; chyba
% sensownie będzie odesłać do pierwszego w kolejności drzewa
% stosującego odpowiednią alternatywę dla rozpatrywanego wierzchołka).

% Sygnaturą drzewa analizy nazwiemy drzewo, którego wierzchołkami są
% wierzchołki alternatywy występujące w danym drzewie analizy.  Przy
% czym relacja dominacji ma być ,,taka sama'' jak w drzewie
% wyjściowym.  Czyli potomkami wierzchołka są wierchołki alternatywy
% występujące w jego poddrzewach w drzewie analizy.  Wierzchołki
% sygnatury będą miały postać

%           s(IdŁuku, NrAlt, LiczbaAlt, ListaKropkowaDzieci).

% Identyfikatory łuków właściwie nie są konieczne dla jednoznaczności
% sygnatur, ale pomogą nam w wybiórczym ich kopiowaniu.

% Zacznijmy od procedury produkującej sygnaturę pewnego drzewa
% zawartego w lesie.  

gettreesgnt(NT,Od,Do,TrId,Signt) :-
    edge(TrId,Od,Do, NT, Children, _NumTrees),
    NT \= terminal(_), !,
    length(Children,NC),
    explodechildsgnt(Children, Od, Do, TrId, 1, NC, Signt).
gettreesgnt(terminal(T),Od,Do,TrId,[]) :- 
    edge(TrId,Od,Do, terminal(T), _Children, _Subtr),
    !.

explodechildsgnt([_NReg/Children], Od, Do, _TrId, 1, 1, Signt) :- !,
    getchildsgnt(Children,Od,Do,Signt).
explodechildsgnt([_NReg/Children | _], Od, Do, TrId, NrAlt, NC,
		 s(TrId,NrAlt,NC, Trees)) :-
    getchildsgnt(Children,Od,Do,Trees).
explodechildsgnt([_ | C], Od, Do, TrId, NrAlt, NC, Trees) :-
    NrAlt1 is NrAlt + 1,
    explodechildsgnt(C, Od, Do, TrId, NrAlt1, NC, Trees).

getchildsgnt([],_,_,[]) :-!.
getchildsgnt([TrId],Od,Do,T) :- !,
    gettreesgnt(_NT,Od,Do,TrId,T).
getchildsgnt([TrId,Przez | CC], Od, Do, TTT) :- !,
    gettreesgnt(_NT,Od,Przez,TrId,T),
    getchildsgnt(CC, Przez, Do, TT),
    ( T == [] -> TTT = TT ; TTT = [T | TT]).
getchildsgnt(C,Od,Do,_) :-
    format(user_error,"~NInvalid children (~p-~p): ~p~n",[Od,Do,C]), fail.


% Następna procedura służy do wybiórczego kopiowania sygnatur.
% Mianowicie sgnt_copy(Sgnt,NSgnt,Id,Alt,Subst) jest spełnione jeżeli
% NSgnt jest głęboką kopią Sgnt, ale zamiast dzieci wierzchołka o
% identyfikatorze Id podstawione jest Subst, a zamiast numeru
% alternatywy Alt.  Zakładamy, że Sgnt jest w pełni ukonkretnione.

sgnt_copy(s(Id,_,LA,_),s(Id,Alt,LA,Subst),Id,Alt,Subst) :- !. % czerwone
sgnt_copy([],[],_,_,_) :- !. % zielone
sgnt_copy(s(Id2,NA,LA,C),s(Id2,NA,LA,CC),Id,Alt,Subst) :- !, % zielone
	sgnt_copy(C,CC,Id,Alt,Subst).
sgnt_copy([X|Y],[XX|YY],Id,Alt,Subst) :-
	sgnt_copy(X,XX,Id,Alt,Subst),
	sgnt_copy(Y,YY,Id,Alt,Subst).

% Procedura sprawdza, czy sygnatura będąca jej argumentem opisuje
% pierwsze z drzew mających te same wierzchołki, czyli czy wszystkie
% numery alternatywy w jej obrębie są równe 1.

first_sgnt([]) :- !.
first_sgnt(s(_,1,_,C)) :- !, % zielone
	first_sgnt(C).
first_sgnt([S1|S2]) :-
	first_sgnt(S1),
	first_sgnt(S2).

% Ogólny plan jest taki, żeby najpierw wygenerować sygnatury dla
% wszystkich drzew i powiązać je z numerami drzew.  Będziemy je
% przechowywać w postaci klauzul treesgnt(Signt,TreeNum).  Następnie
% przy generowaniu drzew analizy w każdym wierzchołku alternatywy
% weźmiemy sobie sygnaturę dla bieżącego drzewa, przekształcimy ją na
% szkielet sygnatury dla drzewa, do którego chcemy mieć hiperlink (tu
% będzie trzeba sprawdzić, że odpowiednie poddrzewo jest "jedynkowe",
% wreszcie pobierzemy numer drzewa dla tej nowej sygnatury.

% Procedura find_nextprev znajduje numer następnego i poprzedniego
% drzewa dla danego węzła alternatywy na podstawie zgromadzonych
% wcześniej sygnatur.

find_nextprev(_,_,1,[],[]) :- !. % czerwone
find_nextprev(TrId,NC,NC,Prev,[]) :- !, %czerwone
	flag(trees,N,N),
	treesgnt(Sgnt,N),
	find_prev(TrId,NC,Prev,Sgnt).
find_nextprev(TrId,Altern,_NC,Prev,Next) :-
	flag(trees,N,N),
	treesgnt(Sgnt,N),
	AlternNext is Altern + 1,
	sgnt_copy(Sgnt,SgntNext,TrId,AlternNext,T),
	treesgnt(SgntNext,Next),
	first_sgnt(T),
	find_prev(TrId,Altern,Prev,Sgnt).

find_prev(_,1,[],_) :- !. % czerwone
find_prev(TrId,Altern,Prev,Sgnt) :-
	AlternPrev is Altern - 1,
	sgnt_copy(Sgnt,SgntPrev,TrId,AlternPrev,T),
	treesgnt(SgntPrev,Prev),
	first_sgnt(T).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gettree(NT,Od,Do,TrId,ptnode(NT,NReg,Trees,Altern/NC/TrId/Subtr)) :-
    edge(TrId,Od,Do, NT, Children, Subtr),
    NT \= terminal(_), !,
    length(Children,NC),
    explodechildren(Children, NReg, Od, Do, Trees,Altern).
gettree(terminal(morf(F,H,T)),Od,Do,TrId,ptterminal(F,H,T)) :- 
    edge(TrId,Od,Do, terminal(morf(F,H,T)), _Children, _Subtr),
    !.

% pierwsza klauzula, żeby nie przechowywał informacji o nawrocie do
% explodechildren([],...)
explodechildren([NReg/Children], NReg, Od, Do, Trees, 1) :- !,
    getchildren(Children,Od,Do,Trees).
explodechildren([NReg/Children | _], NReg, Od, Do, Trees, 1) :-
    getchildren(Children,Od,Do,Trees).
explodechildren([_ | C], NReg, Od, Do, Trees, NrAlt) :-
    explodechildren(C, NReg, Od, Do, Trees, NrAltP),
    NrAlt is NrAltP + 1.

getchildren([],_,_,[]) :-!.
getchildren([TrId],Od,Do,[T]) :- !,
    gettree(_NT,Od,Do,TrId,T).
getchildren([TrId,Przez | CC], Od, Do, [T | TT]) :- !,
    gettree(_NT,Od,Przez,TrId,T),
    getchildren(CC, Przez, Do, TT).
getchildren(C,Od,Do,_) :-
    format(user_error,"~NInvalid children (~p-~p): ~p~n",[Od,Do,C]), fail.




treeprint(T,N) :-
	format('\\begin{tree}{~a}\n', [N]),
	tree1(T,l,n), !,  % nie pamiętam dlaczego odcięcie. na pewno czerwone
	write('\n\\end{tree}\n').

tree1(X,_,_) :- var(X), write('\\TreeLeaf{DZIWNE BARDZO}'), nl.
tree1([],_,_).
tree1(ptnode(H, N, T,AltN/AltC/TrId/Subtr), L, C) :-                       % nonterminal with children
	( T = [X], X\=ptterminal(_,_,_) ->
	    ( (C=c, AltC=1) -> write('\\TreeC{') ; write('\\Tree{') ),
	    C1 = c
	;   write('\\Tree{'),
	    C1 = n),
	find_nextprev(TrId,AltN,AltC,Prev,Next),
	alttype(AltN/AltC,Prev,Next,Subtr,AltCmd),
	format("~p}{\\nonterminal{~p}{~p}{~a}}{\n",[L,H,N,AltCmd]),
	tree2(T,C1),
	write('}').
tree1(ptterminal(F,H,T), _L, _C) :-
	format("\\TreeLeaf{\\terminal{~a}{~a}{~p}}\n", [F,H,T]).
%tree1(i(I),_) :- print(I), nl.
tree1(X,_,_L,_C) :-
	format("\\TreeLeaf{\\nonterminal{~p}{}}\n", [X]).

alttype(_/1,_,_,_,'') :- !.
alttype(1/AltC,_,Next,Subtr,AltCmd) :- !,
	sformat(AltCmd,'\\altfirst{~p}{~p}{~p}',[AltC,Next,Subtr]).
alttype(AltC/AltC,Prev,_Next,Subtr,AltCmd) :- !, 
	sformat(AltCmd,'\\altlast{~p}{~p}{~p}',[AltC,Prev,Subtr]).
alttype(AltN/AltC,Prev,Next,Subtr,AltCmd) :-
	sformat(AltCmd,'\\altmid{~p}{~p}{~p}{~p}{~p}',
		[AltN,AltC,Prev,Next,Subtr]).

tree2('INCOMPLETE',_).
tree2([H],C) :- tree1(H,l,C).
tree2([H|T],C) :- tree1(H,n,C), tree2(T,C).

portray('$VAR'(X)) :- format("_", [X]).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
