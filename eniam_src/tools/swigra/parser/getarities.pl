% Program znajdujący arność wszystkich nieterminali GFJP.
%
% Author: Marcin Woliński
% This file is in the public domain.


:-dynamic nt_arity/2.

term_expansion( (H-->_), Res ) :-
	functor(H, NT, A),
	(nt_arity(NT,A) -> Res = (:-true)
    ; Res = nt_arity(NT,A) ).


%:-['gfjp-tmp.dcg'].
:-['gfjp2.dcg'].

:-listing(nt_arity).

:-halt.

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
