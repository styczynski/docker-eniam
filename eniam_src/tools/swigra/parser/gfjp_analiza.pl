% Predykat służący do wywołania analizy zdania.
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

analiza(T) :- analiza1(T, wypowiedzenie).
analiza(T, N/A) :- !, functor(NT,N,A), analiza1(T, NT).
analiza(Tekst,NT) :- atom(NT), !, nt_arity(NT,A), analiza(Tekst, NT, A).
analiza(T, N, A) :- functor(NT,N,A), analiza1(T, NT).

analiza1(Tekst,NT) :-
	write(':-discontiguous info/2, forest/5, numsubtrees/2.\n'),
	copy_term(NT,NTP),
%	format(user_error, "~NAnaliza: ~a ", [Tekst]),
	statprint(tekst,Tekst),
	grammar_no(GramNo),
	statprint(grammar_no,GramNo),
	morfologia(Tekst,Od,Do),
	initforest,
	start_statistics(inferences), start_statistics(cputime),
	parse(NTP,Od,Do),
	stop_statistics(parse_inferences,inferences),
	stop_statistics(parse_cputime,cputime),
	get_edge_number(Edg), statprint(edges,Edg),
	start_statistics(inferences), start_statistics(cputime),
	process_parses(NT,Od,Do),
	stop_statistics(process_inferences,inferences),
	stop_statistics(process_cputime,cputime),
%	flush_output(user_error),
	koniec,
	flush,
%	format(user_error,"+~n",[]),
	dropforest,
	!.

analiza1(_,_) :-
% wejście tutaj znaczy, że parse/4 zawiodło:
	stop_statistics(parse_inferences,inferences),
	stop_statistics(parse_cputime,cputime),
	get_edge_number(Edg), statprint(edges,Edg),
	statprint(trees,0),
	dumpforest,
	porazka,
%	format(user_error,"-~n",[]),
	dropforest.


% gromadzenie statystyk wykonania:
start_statistics(Type) :-
	statistics(Type,Val),
	nb_setval(Type,Val).

stop_statistics(Name,Type) :-
	statistics(Type,NVal),
	nb_getval(Type, OVal),
	Val is NVal-OVal,
	statprint(Name, Val).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
