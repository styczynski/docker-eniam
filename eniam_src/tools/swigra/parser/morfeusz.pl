% Prolog interface to the morphological analyzer Morfeusz
%
% Author: Marcin Woliński
% This file is in the public domain.

% this defines morfeusz_analyse/2:
:-prolog_load_context(directory,Dir), % where are we?
	absolute_file_name('morfeusz-swi',
			   [relative_to(Dir),file_type(executable)],
			   MorfeuszPath), % find the absolute path
%	format("~Nścieżka Morfeusza: ~p~n",[MorfeuszPath]),
%	initialization(load_foreign_library(MorfeuszPath)).
% zmiana dla SWI 5.8+:
	use_foreign_library(MorfeuszPath).

:-op(550, xfy, '.').

% morfeusz(+String, +Predicate, Start, Stop)
% redefines predicate Predicate/5: Predicate(from,to, form,base,tag)

morfeusz(String, _, _, _) :- var(String), !, fail.
morfeusz(_, Predicate, _, _) :- not(atom(Predicate)), !, fail.
morfeusz(String, Predicate, Start, Stop) :-
	morfeusz_analyse(String, MO),
	abolish(Predicate/5),
	budujgrafmorf(Predicate, Start, Stop, MO).

budujgrafmorf(_, 0, 0, []).
budujgrafmorf(Pred, Start, Stop, [i(Start, Stop, F, H, I)]) :- !,
	T =.. [Pred, Start, Stop, F, H, I],
	assertz(T).
budujgrafmorf(Pred, Start, Stop, [i(Start, K, F, H, I) | MOO]) :-
	T =.. [Pred, Start, K, F, H, I],
	assertz(T),
	budujgrafmorf(Pred, _, Stop, MOO).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
