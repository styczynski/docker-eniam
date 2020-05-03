%
% Obsługa metanieterminala sequence_of
%
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


% Predykat przekształca definicję warunków iterowanych towarzyszącą
% nieterminalom w zasięgu sequence_of.
% Definicje są rozdzielane na następujące listy (kolejne argumenty):
% zmienne inicjalizatorów iteracji, wyników iteracji, 
% zmienne przed krokiem, kroku, po kroku,
% warunki do wykonania.

% zapis ^V znaczy, że V ma być „lokalna” dla jednego elementu sekwencji:
% (pierwsza klauzula dla zabezpieczenia końca „listy”.
warunki_iterowane(V, [], [], [], [V], [], []) :- var(V), !.
warunki_iterowane(V^ZZ, InitVs, StopVs, PreVs, [V | StepVs], PostVs, Preds) :- 
	var(V), !,
	warunki_iterowane(ZZ, InitVs, StopVs, PreVs, StepVs, PostVs, Preds).

% to samo znaczy ^[V]:
warunki_iterowane([V]^ZZ, InitVs, StopVs, PreVs, [V | StepVs], PostVs, Preds) :- 
	var(V), !,
	warunki_iterowane(ZZ, InitVs, StopVs, PreVs, StepVs, PostVs, Preds).

% zapis ^[Func, Var0, VarI, VarN] wprowadza warunek iterowany
% Func(N0,VarI,N1):
warunki_iterowane([Func, Var0, VarI, VarN]^ZZ,
	[Var0|InitVs], [VarN|StopVs], [N0|PreVs], [VarI|StepVs], [N1|PostVs],
	[Pred|Preds]) :- !,
	Pred =.. [Func, N0, VarI, N1],
	warunki_iterowane(ZZ, InitVs, StopVs, PreVs, StepVs, PostVs, Preds).

% zapis ^[Func, V] wprowadza warunek Func(V) sprawdzany osobno dla
% każdego elementu sekwencji:
warunki_iterowane([Func, V]^ZZ,
	InitVs, StopVs, PreVs, [V | StepVs], PostVs, [Pred|Preds]) :- !,
	Pred =.. [Func, V],
	warunki_iterowane(ZZ, InitVs, StopVs, PreVs, StepVs, PostVs, Preds).

% nieco paskudny sposób poradzenia sobie z końcem „listy potęgowej” (X^Y^Z^T):
warunki_iterowane([], [], [], [], [], [], []) :- !.
warunki_iterowane(ZZ, InitVs, StopVs, PreVs, StepVs, PostVs, Preds) :-
	warunki_iterowane(ZZ^[], InitVs, StopVs, PreVs, StepVs, PostVs, Preds).

oddziel_zmienne(VV,[],VV) :- !.
oddziel_zmienne([],_,[]) :- !.
oddziel_zmienne([V|VV], NBV, BV) :-
	var_member(V, NBV, NBVV) 
    ->
	oddziel_zmienne(VV, NBVV, BV) 
    ; 
	BV = [V|BVV],
	oddziel_zmienne(VV, NBV, BVV).

%var_member(_V1, [], []).
var_member(V1, [V2 | VV], VV) :- V1 == V2, !.
var_member(V1, [V2 | VV], [V2 | VVV]) :- V1 \== V2,
	var_member(V1, VV, VVV).

% przygotuj_nieterminal
%
% 1 — warunki wspólne w iteracji
% 2 — nieterminal z warunkami własnymi
% 3 — wynik

przygotuj_nieterminal(CPreVs/CStepVs/CPostVs/CPreds,
	              NT^War, 
		      NT/PredsC/BoundVs/
		      WInitVs/WStopVs/
		      (CPreVs/WPreVs)/(CPostVs/WPostVs)) :- !,
	warunki_iterowane(War, WInitVs, WStopVs, WPreVs, WStepVs, WPostVs, WPreds),
	append(CStepVs,WStepVs,StepVs),
	append(CPreds,WPreds,Preds),
	listtoconj(Preds, PredsC),
	term_variables(NT,NTVars),
	term_variables(StepVs,StepVsVars),
	oddziel_zmienne(NTVars,StepVsVars,BoundVs).

przygotuj_nieterminal(CPreVs/CStepVs/CPostVs/CPreds,
                      NT,
		      NT/PredsC/BoundVs/[]/[]/(CPreVs/[])/(CPostVs/[])) :-
	listtoconj(CPreds, PredsC),
	term_variables(NT,NTVars),
	term_variables(CStepVs,CStepVsVars),
	oddziel_zmienne(NTVars,CStepVsVars,BoundVs).


% przygotuj_sekwencję iteruje przygotuj_nieterminal po liście
% nieterminali będących argumentem sequence_of:

% W pierwszym kroku sprawdzamy, czy są warunki wspólne i przetwarzamy
% je.  Zmienne inicjujące i kończące warunków wspólnych przechowujemy
% osobno od innych, przy całej liście przetworzonych warunków
% iterowanych.  Pozostałe elementy warunków wspólnych są doczepiane do
% poszczególnych nieterminali.
przygotuj_sekwencję(NT^CommonWar,NTprzyg/CInitVs/CStopVs) :- !,
	warunki_iterowane(CommonWar,
	  CInitVs, CStopVs, CPreVs, CStepVs, CPostVs, CPreds),
	przygotuj_sekwencję(CPreVs/CStepVs/CPostVs/CPreds, NT,NTprzyg).
przygotuj_sekwencję(NT,NTprzyg/[]/[]) :-
	przygotuj_sekwencję([]/[]/[]/[], NT,NTprzyg).

% przygotuj_sekwencję/3 dostaje przetworzone warunki wspólne:
przygotuj_sekwencję(CommonWar, [NT | Other],[NTprzyg|OtherPrzyg]) :- !,
	przygotuj_nieterminal(CommonWar, NT, NTprzyg),
	przygotuj_sekwencję(CommonWar, Other,OtherPrzyg).
przygotuj_sekwencję(_,[],[]).


% żeby sekwencja została uznana za zamkniętą, trzeba ostateczne wyniki
% w InitVs przekazać do StopVs:
zakończ_sekwencję([_NT/_Preds/_BoundVs/InitVs/InitVs/_PreVs/_PostVs | Other]) :-
	zakończ_sekwencję(Other).
zakończ_sekwencję([]).


sequence_of( P, PP, W0, W1, Elems ) :-
	przygotuj_sekwencję(Elems, ElemsCode),
	do_sequence_of(P, PP, W0, W1, ElemsCode).

do_sequence_of( P0, PN, I0, IN, ElemsCode/CInitVs/CStopVs ) :-
	select( NT/Preds/BoundVs/InitVs/StopVs/PreVs/PostVs, ElemsCode, RestElemsCode),
	copy_term(NT/Preds/BoundVs/PreVs/PostVs,
		  NTCopy/PredsCopy/BoundVs/(CInitVs/InitVs)/(CPostVsCopy/PostVsCopy)),
% Teraz w NTCopy i PredsCopy zmienne z BoundVars są dzielone z
% oryginałami (czyli z całą regułą), kopie PreVs są zainicjowane
% wartościami InitVs, kopie PostVs są dostępne jako PostVsCopy.	
        call_goals(NTCopy, P0, PP, I0, I1),		  
	call(PredsCopy),
% W następnym kroku w miejsce InitVs zostaną użyte PostVsCopy:
	do_sequence_of( PP, PN, I1, IN,
			[NT/Preds/BoundVs/PostVsCopy/StopVs/PreVs/PostVs | RestElemsCode]
		        /CPostVsCopy/CStopVs).

do_sequence_of( P, P, I, I, ElemsCode/CVs/CVs ) :-
	zakończ_sekwencję(ElemsCode).

% pierwszym argumentem może być jeden nieterminal lub ich lista (przy
% sekwencji sekwencji):
call_goals([optional(NT,TC,FC)], PP, PN, I0, IN) :- !,
	optional(PP, PN, I0, IN, NT, TC, FC).
call_goals([NT], [I0,TrId/NT|PN], PN, I0, IN) :- !,
	goal(NT,I0,IN,TrId).
call_goals([optional(NT,TC,FC)|NTT], PP, PN, I0, IN) :- !,
	optional(PP, P1, I0, I1, NT, TC, FC),
	call_goals(NTT, P1,PN, I1,IN).
call_goals([NT|NTT], [I0,TrId/NT|P1], PN, I0, IN) :- !,
	goal(NT,I0,I1,TrId),
	call_goals(NTT, P1,PN, I1,IN).
call_goals(NT, [I0,TrId/NT|P1], P1, I0, I1) :-
	goal(NT, I0, I1, TrId).

%%%% OPTIONAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% opcjonalny element reguły:

optional( [W0,TrId/NT|PP], PP, W0, W1, NT, TC, _FC ) :-
	goal( NT, W0, W1, TrId ),
	call_conds(TC).
optional( PP, PP, W0, W0, _NT, _TC, FC ) :-
	call_conds(FC).

call_conds({}).
call_conds({C}) :-
	call(C).

%%%% POMOCNICZE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% listtoconj/3:
% zamiana listy na term przecinkowy.
listtoconj( [], true ) :- !.
listtoconj( [X], X ) :- !.
listtoconj( [X|XX], (X,Y) ) :- listtoconj( XX, Y ).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
