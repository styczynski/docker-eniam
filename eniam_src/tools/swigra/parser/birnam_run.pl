%
% Runtime parsera bottom-up generującego upakowany las drzew. 
%
%
% Copyright © 1997–2013 Marcin Woliński
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

:- dynamic(forest/5).
:- dynamic(tested/1).
:- dynamic(variant/3).
%:- set_prolog_flag(unknown,fail).

parse(NT,Od,Do) :-
	goal(NT,Od,Do,_TrId).
%	format(user_error,"~Nfound: ~p~n", [goal(NT,Od,Do,TrId)]).
%	tell(zsyp),listing(forest),told,
%	trace,
%	gettree(NT,Od,Do,TrId,Drzewo).


% Predykat goal jest wywoływany, gdy reguła szuka następnego
% potrzebnego jej nieterminala.
%
% Goal jest szkieletyzowany, bo jeśli forest zawiera już jakiś łuk dla
% tego nieterminala, to znaczy, że goal było już wywoływane dla
% tego nieterminala w tym miejscu i już znamy odpowiedź.  Na tej
% zasadzie również Wn jest dopasowywane dopiero po sprawdzeniu, czy
% forest zawiera łuk dla danego nieterminala zaczynający się w danym
% miejscu.  Próba nowej analizy (parsegoal) jest podejmowana tylko w
% przeciwnym wypadku.

goal( Goal, W0, Wn, TrId ) :-
    tested(W0) ->
        forest(Goal, W0, Wn, _, TrId)
    ;   assertz(tested(W0)),
        parsegoal(Goal, W0, Wn, TrId).

% Parsegoal będzie wywołany dokładnie jeden raz dla każdej wartości W0.
% Usunięte wewnętrzne sprawdzanie jednokrotności.

parsegoal( _Goal, W0, _Wn, TrId ) :-
    getinput(W0, W1, I, xinfo(XInfo)),
    new_edge_number(TrId),
    assertz(forest(terminal(I),W0,W1,[],TrId) ),
    register_variant(TrId, dummy/terminal(I)/xinfo(XInfo)),
    terminal(W0, W1, TrId, I).

parsegoal( Goal, W0, Wn, TrId ) :-
	forest( Goal, W0, Wn, _, TrId).

%checkforest( NT, Od, Do, Drzewa ) :-
%	skeletify(NT,NTSkel),
%	forest(NTSkel, Od, Do, Drzewa, ?), 
%	NTSkel =@= NT.

register( NT, Od, Do, NReg, Analiza, _ ) :-
	skeletify(NT,NTSkel),
	forest(NTSkel, Od, Do, _, TrId), NTSkel =@= NT, 
	!,
	compute_variant_hash(TrId,NReg/NT/Analiza,VarHash),
	(variant(VarHash,_,_) %eqmember(NReg/Analiza, Drzewa) 
      -> 
%	print(NReg/Analiza), nl, 
	fail
      ;
%	retract(forest(NT,Od,Do,Drzewa,TrId)),
%	assert(forest(NT,Od,Do,[NReg/Analiza | Drzewa],TrId)),
	assert(variant(VarHash,TrId,NReg/NT/Analiza)),
	!, fail).
register( NT, Od, Do, NReg, Analiza, TrId ) :-
	new_edge_number(TrId),
	assert(forest(NT,Od,Do,[],TrId)),
	register_variant(TrId,NReg/NT/Analiza).

initforest :- reset_edge_numbers.

dropforest :-
	retractall( forest(_,_,_,_,_) ),
	retractall( tested(_) ),
	retractall( variant(_,_,_)).

% Ten predykat pobiera jedną krawędź z forestu.  W tej wersji jest
% trywialny, ale w innej reprezentacja Drzew będzie wymagała ich
% przetwarzania przy pobieraniu.
getforest(NT,Od,Do,Drzewa,TrId) :-
	forest(NT,Od,Do,_,TrId),
	findall(Tree,variant(_,TrId,Tree),Drzewa1),
	unifikuj_z_głową(NT,Drzewa1,Drzewa).

% Ten predykat jest potrzebny do dość subtelnego przetworzenia:
% zmienne powtarzające się w NT i w Analiza muszą się ze sobą
% zunifikować, bo może się zdarzyć, że one się ukonkretniły na innym
% poziomie analizy.  Bez tego drzewa wyjęte z lasu mogłyby mieć nie w
% pełni ukonkretnione zmienne.
unifikuj_z_głową(_NT, [], []) :- !.
unifikuj_z_głową(NT, [NReg/NT/Analiza | Drzewa1], [NReg/Analiza | Drzewa]) :-
	unifikuj_z_głową(NT, Drzewa1, Drzewa).

dumpforest.
% dumpforest :-
% 	listing(forest),
% 	listing(fail_goal).

skeletify(T,S) :-
	functor(T, N, A),
	functor(S, N, A).

% lista Pałkowa:
pałkowa(X|_,X).
pałkowa(_|XX,X) :- !, pałkowa(XX,X).
pałkowa(X,X).

% member nieunifikujący

eqmember(E,[F|_]) :- E=@=F, !.
eqmember(E,[_|L]) :- eqmember(E,L).

% Warianty rozpisania danego nieterminalna będą przechowywane w
% postaci predykatu variant/3.  Chodzi o to, żeby zapewnić czas O(1)
% dostępu do poszczególnych wariantów.  Dlatego dany wariant, czyli
% para NReg/Analiza (ciąg nieterminali i pozycji) zostaje wypisany na
% atom, który stanie się pierwszym hashowanym argumentem predykatu
% variant/3.  Warianty są wiązane z daną instancją nieterminala przez
% jego identyfikator (TrId).

compute_variant_hash(VarId,Variant,VarHash) :-
    variant_sha1(VarId/Variant,VarHash).

register_variant(TrId, Variant) :-
	compute_variant_hash(TrId,Variant,VarHash),
	assert(variant(VarHash,TrId,Variant)).


% generator unikatowych identyfikatorów dla łuków lasu:

reset_edge_numbers :- 
	nb_setval(swigra_edge_number,0).
new_edge_number(N) :-
	nb_getval(swigra_edge_number, N),
	N1 is N+1,
	nb_setval(swigra_edge_number, N1).
get_edge_number(N) :-
	nb_getval(swigra_edge_number, N).


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
