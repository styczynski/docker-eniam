%
% Author: Marcin Woliński
% This file is in the public domain.
%
% parser wypluwający drzewa texowe:

:-[
   birnam_run,
%   birnam_dumpforest,
%   birnam_gettree,
   gettreetex,
   gfjp_morfologia,
   slowczas,
   gfjp_analiza,
   gfjp_wymagania,
   gfjp_warunki,
   gfjp,
   gfjp_arnoscint
  ].

statprint(X,Y) :-
    format("%~a: ~a\n",[X,Y]).

process_parses(NT,Od,Do) :-
	flag(trees,_,0),
	gettree(NT,Od,Do,_TrId,Drzewo),
	treeprint(Drzewo),
	flag(trees,N,N+1),
 	(0 is (N+1) mod 100 -> format(user_error,"+",[]),	fail).
process_parses(_,_,_) :-
	flag(trees,N,0),
	statprint(trees,N).



:-tell(zsyp).
:-analiza('czytam.').

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 

