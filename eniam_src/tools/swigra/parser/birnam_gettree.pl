%
% Predykat wydobywający poszczególne drzewa z lasu.
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

gettree(terminal(morf(F,H,T)),_,_,_,ptterminal(F,H,T)) :- 
    !.
gettree(NT,Od,Do,TrId,ptnode(NT,NReg,Trees)) :-
    getforest(NT,Od,Do, Children,TrId),
    explodechildren(Children, NReg, Od, Do, Trees).

% pierwsza klauzula, żeby nie przechowywał informacji o nawrocie do
% explodechildren([],...)
explodechildren([NReg/Children], NReg, Od, Do, Trees) :- !,
    getchildren(Children,Od,Do,Trees).
explodechildren([NReg/Children | _], NReg, Od, Do, Trees) :-
    getchildren(Children,Od,Do,Trees).
explodechildren([_ | C], NReg, Od, Do, Trees) :-
    explodechildren(C, NReg, Od, Do, Trees).

getchildren([],_,_,[]) :-!.
getchildren([TrId/NT],Od,Do,[T]) :- !,
    gettree(NT,Od,Do,TrId,T).
getchildren([TrId/NT,Przez | CC], Od, Do, [T | TT]) :- !,
    gettree(NT,Od,Przez,TrId,T),
    getchildren(CC, Przez, Do, TT).
getchildren(C,Od,Do,_) :-
    format(user_error,"~NInvalid children (~p-~p): ~p~n",[Od,Do,C]), fail.

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
