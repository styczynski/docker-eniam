%
% Interfejs graficzny parsera Birnam.
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

:- use_module(library(pce)).

:-[birnam_gettree, birnam_xfonts].

% Przeglądarka drzew:

:- pce_global(@ptbrowser, new(parse_tree_browser)).


:- pce_begin_class(parse_tree_browser, picture).

variable(forest, vector, both).
variable(thetext, text_item, both).
variable(nonterminalmenu, menu, both).
variable(numentry, text_item, both).
variable(oflabel, label, both).

initialise(P) :->
	send_super(P, initialise('Analizator składniowy Świgra')),
	send(P, slot, forest, new(vector)),
% 	send(P, slot, thetext,
% 	     new(TL, text_item(nowa,' ', %font(helvetica,bold,14)
% 	     font(verdana,roman,16)
% 	     ))),
	send(P, slot, thetext,
	     new(TL, text_item(@default,'',
		  message(P, analyse_sentence, @receiver?selection)))),
	send(TL, show_label, false),
	send(TL, value_font, font(verdana,roman,16)),
	send(TL, value_width, 600),
%	send(TL, hor_stretch, 100),
	send(P, slot, nonterminalmenu,
	     new(MNT, menu(@default,cycle))),
%		      message(P, set_nonterminal, @receiver?selection)))),
	send(MNT, show_label, false),
	findall(NT,nt_arity(NT,_),Items),
	send_list(MNT, append, Items),
	send(P, slot, numentry, new(I, int_item(tree, 1, 
	             message(P, display_tree, @receiver?selection)))),
	send(I, range, 1, 1000),
	send(P, slot, oflabel, new(L, label('of 0'))), % '
	new(TD, dialog),
	send(TD, gap, size(5,5)),
	send(P, below, TD),
	send(TD, append, TL),
	send(TD, append, MNT, next_row),
	send(TD, append, button(analizuj, message(P, analyse)), right),
	send(TD, append, I, right),
	send(TD, append, L, right),
	send(I, alignment, right),
	send(L, alignment, right),
% 	new(D, dialog),
% %	send(D, gap, size(1,3)),
% 	send(D, below, P),
% 	send(D, append, button(close, message(P, destroy)), right),
	send(P, open),
	send(P?frame, size, size(600,500)).

add_tree(P, T:tree) :->
	send(P?forest, append, T),
	get(P?forest, size, Size),
	send(P?numentry, range, 1, Size),
	sformat(Tmp, 'of ~p', [Size]), string_to_atom(Tmp,Text),
	send(P?oflabel, selection, Text),
	(1 is Size -> send(P, display, T); true).

display_tree(P, N) :->
	send(P, clear),
	get(P?forest, element, N, T),
	send(P, display, T).

analyse_sentence(P, T) :->
	get(P, nonterminalmenu, M),
	get(M, selection, NT),
	format("Do analizy: ~p as ~p~n", [T,NT]),
	analiza(T,NT).

analyse(P) :->
	get(P?thetext, selection, T),
	get(P, nonterminalmenu, M),
	get(M, selection, NT),
	format("Do analizy2: ~p as ~p~n", [T,NT]),
	analiza(T,NT).


new_parse(P, T) :->
	send(P,clear),
	send(P?thetext, selection, T),
	send(P, slot, forest, new(vector)),
	send(P?numentry, range, 1, 1),
	send(P?numentry, selection, 1),
	send(P?oflabel, selection, 'of 0'). %'


:- pce_end_class.


% Pozyskiwanie drzew analizy:

process_parses(NT,Od,Do) :-
	flag(trees,_,0),
	gettree(NT,Od,Do,_TrId,Drzewo),
	treeprint(Drzewo),
	flag(trees,N,N+1),
 	(0 is (N+1) mod 100 -> format(user_error,"+",[]),	fail).
process_parses(_,_,_) :-
	flag(trees,N,0),
	statprint(trees,N).


% to żeby można było pokazać drzewo zapisane w pliku:
drzewo(D,_) :-
	treeprint(D).

treeprint(ptnode(PTH, PTN, PTT)) :-
	sformat(S, '~p : ~p', [PTH, PTN]), string_to_atom(S,Text),
	new(T, tree(new(RootNode, node(text(Text, center, font(verdana,roman,10)))))),
	send(T, direction, list),
	send(T, level_gap, 17),
	send(T, neighbour_gap, 1),
	convert_children(PTT, RootNode),
	send(@ptbrowser, add_tree, T),
	!.

convert_node(ptterminal(F,H,_), C) :-
	sformat(S, '~p (~p)', [F, H]),
	string_to_atom(S, Text),
	send(C, son, node(text(Text, center, font(verdana,bold,10)))).

convert_node(ptnode(H, N, T), C) :-
	sformat(S, '~p : ~p', [H, N]),
	string_to_atom(S, Text),
	send(C, son, new(Son, node(text(Text,center,font(verdana,roman,10))))),
	convert_children(T, Son).

convert_children([], _).
convert_children([H|T],C) :-
	convert_node(H, C),
	convert_children(T,C).


statprint(tekst, T) :- !,
	send(@ptbrowser, new_parse, T).
statprint(plik,F) :- !,
    format("[~a]\n",[F]).

statprint(X,Y) :-
    format("~a: ~a\n",[X,Y]).

koniec :-
    format("**********************************************************************\n\n", []).

porazka :-
    format("Porażka\n", []),
    koniec.

run :-
	send(@ptbrowser, new_parse, '').


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
