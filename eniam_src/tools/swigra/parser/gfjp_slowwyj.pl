% słownik wyjątków
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

% zamieniamy tagi dawane przez Morfeusza na inne

% Uwaga: Musi być odcięcie po przetworzeniu każdego z leksemów!


%% 101210 źle: slowwyj(chyba, qub, adv:pos) :- !.
% to jest patch na co sklejone z CO ndm:
slowwyj(co, subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:n2, psubst:sg:nom.acc:n2) :- !.
slowwyj(co, subst:T, psubst:T) :- !.
slowwyj(coś, subst:T, psubst:T) :- !.
slowwyj(cokolwiek, subst:T, psubst:T) :- !.
slowwyj(czyj, adj:T, padj:T) :- !.
slowwyj(czemu, adv, padv) :- !.
slowwyj(dlaczego, qub, padv) :- !.
slowwyj(dokąd, qub, padv) :- !.
slowwyj(dziś, qub, adv:pos) :- !.
slowwyj(dzisiaj, qub, adv:pos) :- !.
%slowwyj(gdzie, qub, padv) :- !.
%slowwyj(gdzieś, qub, adv:pos) :- !.
% Porządkujemy dane NKJP i będziemy się słuchać w sprawie ‹i›:
% slowwyj(i, conj, conj).
% slowwyj(i, conj, qub) :- !.
% slowwyj(jak, conj, conj). % dodana interpretacja
% slowwyj(jak, conj, padv) :- !.
slowwyj(jaki, adj:T, padj:T) :- !.
slowwyj(jakiś, adj:T, padj:T) :- !.
%slowwyj(jakby, qub, conj) :- !.
%slowwyj(jakoby, qub, conj) :- !.
%slowwyj(jednak, qub, conj) :- !.
%%%chyba błąd: slowwyj(już, qub, padv) :- !.
%slowwyj(kiedy, qub, conj). % dodana interpretacja
slowwyj(kiedy, qub, padv) :- !.
slowwyj(kiedyś, qub, adv:pos) :- !.
slowwyj(kto, subst:T, psubst:T) :- !.
slowwyj(ktoś, subst:T, psubst:T) :- !.
slowwyj(ktokolwiek, subst:T, psubst:T) :- !.
%slowwyj(który, adj:sg:acc:_, zsyp) :- !.
slowwyj(który, adj:T, padj:T) :- !.
slowwyj(naprawdę, qub, adv:pos) :- !.
slowwyj(miło, adv:pos, adv:pos).
slowwyj(miło, adv:pos, pred) :- !.  % predykatywny przysłówek (proteza)
slowwyj(nic, subst:T, psubst:T) :- !.
slowwyj(nigdy, qub, padv) :- !. % SIaT
slowwyj(nikt, subst:T, psubst:T) :- !.
slowwyj(odkąd, qub, padv) :- !.
slowwyj(potem, qub, adv:pos) :- !.
slowwyj(przedtem, qub, adv:pos) :- !.
slowwyj(przykro, adv:pos, adv:pos).
slowwyj(przykro, adv:pos, pred) :- !.  % predykatywny przysłówek (proteza)
slowwyj(się, subst:T, zsyp:T) :- !.
slowwyj(skąd, qub, padv) :- !.
slowwyj(tak, qub, padv) :- !.
slowwyj(taki, adj:T, padj:T) :- !.
slowwyj(taki, adv, padv) :- !.
slowwyj(ten, adj:T, padj:T).  % „ten” ma interpretację jako przymiotnik + coś specjalnego
slowwyj(ten, adj:T, adj:T) :- !.
%slowwyj(to, conj, qub) :- !.
slowwyj(to, subst:T, psubst:T) :- !.
slowwyj(trudno, adv:pos, adv:pos).
slowwyj(trudno, adv:pos, pred) :- !.  % predykatywny przysłówek (proteza)
slowwyj(tutaj, qub, adv:pos) :- !.
slowwyj(wczoraj, qub, adv:pos) :- !.
slowwyj(wkrótce, qub, adv:pos) :- !.
slowwyj(właśnie, qub, adv:pos) :- !.
slowwyj(wreszcie, qub, adv:pos) :- !.
slowwyj(wówczas, qub, adv:pos) :- !.
slowwyj(wtedy, qub, adv:pos) :- !.
%% slowwyj(zaraz, qub, adv:pos) :- !.
%% slowwyj(znów, qub, adv:pos) :- !.

% dodane dla prototypowego Morfeusza SGJP:
slowwyj(dlaczego, adv, padv) :- !.
slowwyj(dokąd, adv, padv) :- !.
slowwyj(gdzie, adv, padv) :- !.
slowwyj(kiedy, adv, padv) :- !.
slowwyj(którędy, adv, padv) :- !.
slowwyj(nigdy, adv, padv) :- !.
slowwyj(odkąd, adv, padv) :- !.
slowwyj(skąd, adv, padv) :- !.
slowwyj(tak, adv, padv) :- !.
slowwyj(tak, adv:pos, padv) :- !.
slowwyj(jak, adv, padv) :- !.
slowwyj(jak, adv:pos, padv) :- !.
slowwyj(gdy, adv, comp) :- !.

% Przeciwkorby na anotację NKJP:
slowwyj(jednak, conj, qub) :- !.
slowwyj(dobra, qub, interj) :- !.
slowwyj(ewentualnie, qub, adv:pos) :- !.

% Pseudotager współczesny:
slowwyj(a, prep:nom, zsyp) :- !.
%slowwyj(akurat, qub, adv:pos) :- !.
slowwyj('Al', depr:pl:nom.voc:m2, zsyp) :- !.  % ale
slowwyj('Ala', subst:pl:nom.acc.voc:f, zsyp) :- !.  % ale
slowwyj('Ali', depr:pl:nom.voc:m2, zsyp) :- !.  % ale
slowwyj('Alo', depr:pl:nom.voc:m2, zsyp) :- !.  % ale
slowwyj('Ania', subst:sg:gen.dat.loc:f, zsyp) :- !.  % ani
% wskakuje jako interpretacja spójnika „lub”!!!
slowwyj('Luba', subst:pl:gen:f, zsyp) :- !.
slowwyj(niża, subst:_, zsyp) :- !.
% ignorujemy formy przymiotnika „on” (byłoby lepiej tylko te
% mieszające się z zaimikem, ale nie mam jak tego tu zapisać):
slowwyj(on, adj:_, zsyp) :- !.


% domyślna klauzula łapiąca pozostałe leksemy
slowwyj(_, T, T).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
