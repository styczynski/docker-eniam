% This file has been generated with genslow.  Do not modify.  
% Modifications should be applied to the file s_wykrz instead.

%
% A somewhat ad-hoc dictionary of valence frames for interjections
%
% Version 0
%
% Copyright © 2013 by Marcin Woliński
%
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
%
%ach	np(wol)
%ach	np(mian) % Ach prawda, zapomniałem!
%o	np(wol)
%oj	np(wol)
s_wykrz(biada, [[[np(cel)]]]).
s_wykrz(dobranoc, [[[np(cel)]]]).
s_wykrz(frr, [[[np(mian)]]]).
s_wykrz(nuże, [[[prepnp('do',dop)]]]).
s_wykrz(oczywiście, [[[sentp(że)]]]).
s_wykrz(precz, [[[prepnp('z',narz)]], [[np(cel)], [advp]], [[np(cel)], [prepnp('z',dop)]]]).
s_wykrz(wiwat, [[[np(mian)]]]).
%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
