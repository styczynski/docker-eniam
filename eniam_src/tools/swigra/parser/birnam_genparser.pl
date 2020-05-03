%
% Program tłumaczący reguły gramatyczne na postać wymaganą przez
% parser bottom-up. 
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


:- dynamic(rulehead/1).

translate(Infile, Outfile)  :-
        see(Infile),  tell(Outfile), 
	write(':-style_check(-singleton).'), nl,
	get_time(TimeStamp), format_time(atom(Sec4Epoch),'%s',TimeStamp),
	format('grammar_no(~a).~n',Sec4Epoch),
	repeat,
	read(Rule),
	process_rule(Rule),
	!,
        seen,
	dump_rules,
	told.

process_rule(end_of_file) :- !.
process_rule((H-->T)) :-
	translaterule( H, T, LHS, RHS, _F ),
	assertz( rule(LHS,RHS) ),
	save_rulehead( H ),
	save_rulehead2( LHS ),
	!, fail.
process_rule((:-Goal)) :-
	call(Goal),
	portray_clause((:-Goal)), nl,
	!, fail.
process_rule(Clause) :-
	portray_clause(Clause), nl,
	fail.


save_rulehead2( NT ) :-
	functor(NT, Name, Arity),
	functor(Head, Name, Arity),
	(rulehead( Head ), !
    ;
	assertz( rulehead(Head) )).

dump_rules :-
	rulehead( H ),
	gen_rule( H ),
	fail.
dump_rules.


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
