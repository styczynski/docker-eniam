% This is a demo of using Birnam for your own grammar.
%
% Author: Marcin Woliński
% This file is in the public domain.

:-[
% Runtime definitions for the parser:
   birnam_run,
%
% Either use the following for interactive display of trees using XPCE
% extension:
% (On Linux xpce is a separate version of the interpreter!)
   birnam_xpce,
% or that one for just dumping of the usefull part of the forest:
%  birnam_dumpforest,
%
% A simplistic demo grammar.  The file demo_gramm.pl needed below can
% be generated with the following system call from demo_gram.dcg
% (which see):
%
%     ./birnam_dcg2pl demo_gramm.dcg
%
   demo_gramm
].

% Birnam expects to be able to get input with the getinput/3 predicate.
% Here is a version using difference lists:
getinput([C|CC],CC,C).


% Try the following (ask for all answers):
%
% ?- parse(expr(X),"11+3*2",[]).
%
% ?- dropforest.
%
% ?- parse(expr(X),"11+3*2",Z).
%
% ?- dropforest.
%

test(NT,From) :-
	initforest,
	parse(NT,From,[]),
	process_parses(NT,From,[]),
	dropforest.

% The following predicate provides arities of the nonterminals of the
% demo grammar.  This info is needed to populate the combobox in XPCE
% GUI for Birnam.  You can generate this predicate with the program
% getarities.pl .
nt_arity(expr,1).
nt_arity(term,1).
nt_arity(factor,1).
nt_arity(number,1).
nt_arity(digit,1).

% ?- test(expr(_),"11+3*2").


%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
