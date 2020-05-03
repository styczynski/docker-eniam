% Słownik rozwinięć skrótów
%
% W obecnej wersji Morfeusz przypisuje skrótom znacznik brev: i
% rozwinięcie.  Nie daje znaczników morfosyntaktycznych.  Potrzebujemy
% ich do analizy składniowej.  Dodatkowo podajemy jeszcze informację,
% czy dany skrót może wystąpić na końcu zdania (a więc jak się
% zachowuje w kwestii pochłaniania kropki):
% nkon — niekońcowy, zawsze pochłania kropkę,
% kon — końcowy, kropka musi zostać jako kończąca zdanie,
% dow — może wystąpić i tu i tu, więc i z kropką może być różnie.
% Uwaga: wszystkie skróty bezkropkowe (npun) powinny mieć kon (nie
% pochłaniają kropki).
%
% Skrótom rozwijającym się do jednej wielkiej litery dajemy nowy
% znacznik: ini jak inicjał.
%
% Copyright © 2010 Marcin Woliński
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

slowskr('rok', 'r', pun, dow, subst:sg:nom.gen.acc.loc:m3). % 175
slowskr('złoty', 'zł', npun, kon, subst:sg:nom.gen.dat.acc.inst.loc:m2). % 131
slowskr('złoty', 'zł', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m2). % 131
slowskr('procent', 'proc', pun, dow, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 91
slowskr('procent', 'proc', pun, dow, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 91
slowskr('ustęp', 'ust', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 86
slowskr('tysiąc', 'tys', pun, dow, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 81
slowskr('ulica', 'ul', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 80
slowskr('artykuł', 'art', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 73
%slowskr('między innymi', 'm.in', pun, nkon, subst:pl:inst:m1). % 66
%slowskr('na przykład', 'np', pun, nkon, subst:sg:nom.acc:m3). % 65
slowskr('wiek', 'w', pun, dow, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 53
slowskr('tak zwany', 'tzw', pun, nkon, adj:nom.gen.dat.acc.inst.loc:_:_:pos). % 52
slowskr('milion', 'mln', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 51
%slowskr('numer', 'nr', npun, kon, subst:sg:nom.acc:m3). % 49 % hmm, „gimnazjum nr 6” — jaka struktura?
%slowskr('numer', 'nr', pun, nkon, subst:sg:nom.acc:m3). % 7
%slowskr('numer', 'Nr', npun, kon, subst:sg:nom.acc:m3). % 5
slowskr('około', 'ok', pun, nkon, qub). % 45
slowskr('około', 'ok', pun, nkon, prep:gen). % 45
slowskr('kilometr', 'km', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 42
slowskr('godzina', 'godz', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 39
slowskr('metr', 'm', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 38
slowskr('święty', 'św', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 29
%slowskr('święta', 'św', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 29
slowskr('święty', 'św', pun, nkon, adj:sg:nom.gen.dat.acc.inst.loc:f:pos). % 29
slowskr('święty', 'św', pun, nkon, adj:sg:nom.gen.dat.acc.inst.loc:m1:pos). % 29
%slowskr('święty', 'Św', pun, nkon, subst:sg:nom.voc:m1). % 2
slowskr('profesor', 'prof', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 29
%slowskr('profesor', 'Prof', pun, nkon, subst:sg:nom:m1). % 4
slowskr('lata', 'l', pun, dow, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 27
slowskr('miliard', 'mld', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 26
%slowskr('i tym podobne', 'itp', pun, nkon, adj:sg:nom.acc.voc:n1.n2:pos|adj:pl:nom.acc.voc:m2.m3.f.n1.n2.p2.p3:pos). % 22
slowskr('punkt', 'pkt', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 18
slowskr('punkt', 'pkt', pun, nkon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 6 % to błąd, ale występuje
slowskr('województwo', 'woj', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:n2). % 16
%slowskr('imienia', 'im', pun, nkon, subst:sg:gen:n1.n2). % 15 może to prep:gen?
%slowskr('i tak dalej', 'itd', pun, nkon, adv:com). % 14
slowskr('bieżący rok', 'br', pun, dow, subst:sg:gen.acc.inst.loc:m3). % 13
slowskr('bieżącego roku', 'br', pun, dow, subst:sg:gen:m3). % 2 % błąd anotatorów
slowskr('bieżącego/bieżącym roku', 'br', pun, nkon, subst:sg:gen.loc:m3). % 2 % błąd anotatorów
slowskr('ksiądz', 'ks', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 12
slowskr('kilogram', 'kg', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 12
slowskr('hektar', 'ha', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 12
%slowskr('południowy', 'pd', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 11
slowskr('centymetr', 'cm', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 11
slowskr('generał', 'gen', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 10
%slowskr('do spraw', 'ds', pun, nkon, impt:sg:sec:perf). % 10
slowskr('połowa', 'poł', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 9
slowskr('jezioro', 'jez', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:n2). % 9
slowskr('zachód', 'zach', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 8
slowskr('rysunek', 'rys', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 8
%slowskr('północny', 'pn', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 8
slowskr('minuta', 'min', pun, dow, subst:sg:nom.gen.dat.acc.inst.loc:f). % 8
slowskr('minuta', 'min', npun, kon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 3
slowskr('artykuł', 'art', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 8
slowskr('inżynier', 'inż', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 7
%slowskr('były', 'b', pun, nkon, praet:pl:m2.m3.f.n1.n2.p2.p3:imperf). % 7
%slowskr('zobacz', 'zob', pun, nkon, impt:sg:sec:perf). % 6
slowskr('wschód', 'wsch', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 6
slowskr('według', 'wg', npun, kon, prep:gen). % 6
%slowskr('według', 'Wg', npun, kon, prep:gen). % 2
%slowskr('urodzony', 'ur', pun, nkon, subst:sg:nom.voc:m1). % 6
slowskr('kwintal', 'q', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 6
slowskr('pozycja', 'poz', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 6
slowskr('początek', 'pocz', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 6
%slowskr('kilometr', 'km', pun, nkon, subst:sg:nom.acc:m3). % 6
slowskr('ubiegły rok', 'ub.r', pun, dow, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 5
slowskr('strona', 's', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 5
%slowskr('redakcja', 'red', pun, nkon, subst:sg:nom:f). % 5 % poza tekstem ciągłym
%slowskr('pod tytułem', 'pt', pun, nkon, subst:sg:inst:m3). % 5
slowskr('pułkownik', 'płk', npun, kon, subst:sg:nom.gen.dat.acc.inst.loc:m1). % 5
slowskr('miligram', 'mg', npun, kon, subst:sg:nom.gen.dat.acc.inst.loc:m3). % 5
%slowskr('fotografia', 'fot', pun, nkon, subst:sg:nom:f). % 5 % poza tekstem ciągłym
%slowskr('Celsjusz', 'C', npun, kon, subst:sg:nom:m1). % 5
slowskr('ustawa', 'U', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 4
slowskr('ustawa', 'ust', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 4
%slowskr('telefon', 'tel', pun, nkon, subst:sg:nom.acc:m3). % 4
slowskr('SA', 'SA', npun, kon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 4
slowskr('rzeka', 'rz', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 4
%slowskr('postscriptum', 'PS', npun, kon, subst:sg:nom.gen.dat.acc.inst.loc.voc:n2). % 4
%slowskr('powierzchnia', 'pow', pun, nkon, subst:sg:nom:f). % 4
%slowskr('nr', 'nr', npun, kon, '???'). % 4
slowskr('milimetr', 'mm', npun, kon, subst:pl:nom.gen.dat.acc.inst.loc:m3). % 4
%slowskr('miasto', 'm', pun, nkon, subst:sg:nom.acc.voc:n2). % 4
%slowskr('godzina', 'h', npun, kon, subst:sg:nom:f). % 4
slowskr('gdański', 'Gd', pun, nkon, adj:sg:nom.gen.dat.acc.inst.loc:m3:pos). % 4
%slowskr('dziennik', 'Dz', pun, nkon, subst:sg:nom.acc:m3). % 4
%slowskr('doktor', 'dr', pun, nkon, subst:sg:nom:m1.f|subst:sg:gen.dat.acc.inst.loc.voc:f|subst:pl:nom.gen.dat.acc.inst.loc.voc:f). % 4
%slowskr('doktor', 'Dr', npun, kon, subst:sg:nom:m1.f|subst:sg:gen.dat.acc.inst.loc.voc:f|subst:pl:nom.gen.dat.acc.inst.loc.voc:f). % 4
%slowskr('doktor', 'dr', npun, kon, subst:sg:nom:m1.f|subst:sg:gen.dat.acc.inst.loc.voc:f|subst:pl:nom.gen.dat.acc.inst.loc.voc:f). % 4
%slowskr('dolina', 'Dol', pun, nkon, subst:sg:nom:f). % 4
%slowskr('znak', 'zn', pun, nkon, subst:sg:nom.acc:m3). % 3
%slowskr('wrzesień', 'wrz', npun, kon, subst:sg:nom.acc:m3). % 3
slowskr('ubiegły', 'ub', pun, nkon, adj:sg:nom.gen.dat.acc.inst.loc:_:pos). % 3
%slowskr('sztuka', 'szt', pun, nkon, subst:sg:nom:f). % 3
%slowskr('stopień', 'st', pun, nkon, subst:sg:nom.acc:m3). % 3
%slowskr('siostra', 's', pun, nkon, subst:sg:nom:f). % 3
%slowskr('spółka akcyjna', 'SA', npun, kon, adj:sg:nom.voc:f:pos). % 3
%slowskr('pani', 'p', pun, nkon, subst:sg:nom.gen.dat.loc.voc:f). % 3
%slowskr('pan', 'p', pun, nkon, subst:sg:nom:m1). % 3
%slowskr('północ', 'pn', pun, nkon, subst:sg:nom.acc:f). % 3
%slowskr('ograniczona odpowiedzialność', 'o.o', pun, nkon, subst:sg:nom.acc:f). % 3
%slowskr('mieszkaniec', 'mk', pun, nkon, subst:sg:nom:m1). % 3
%slowskr('mieszkaniec', 'mieszk', pun, nkon, subst:sg:nom:m1). % 3
%slowskr('megaherc', 'MHz', npun, kon, subst:sg:nom.acc:m3). % 3
%slowskr('metr kwadratowy', 'm2', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 3
%slowskr('litera', 'lit', pun, nkon, subst:sg:nom:f). % 3
%slowskr('kwadratowy', 'kw', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 3
%slowskr('koło', 'k', pun, nkon, subst:sg:nom.acc.voc:n2). % 3
%slowskr('jezioro', 'Jez', pun, nkon, subst:sg:nom.acc.voc:n2). % 3
%slowskr('głębokość', 'głęb', pun, nkon, subst:sg:nom.acc:f). % 3
%slowskr('dyrektor', 'dyr', pun, nkon, subst:sg:nom:m1). % 3
%slowskr('długość', 'dł', pun, nkon, subst:sg:nom.acc:f). % 3
%slowskr('dawny', 'd', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 3
%slowskr('bieżącego miesiąca', 'bm', pun, nkon, subst:sg:gen:m3). % 3
%slowskr('architekt', 'arch', pun, nkon, subst:sg:nom:m1). % 3
%slowskr('aleja', 'Al', pun, nkon, subst:sg:nom:f). % 3
%slowskr('aleja', 'al', pun, nkon, subst:sg:nom:f). % 3
%slowskr('zmarł', 'zm', pun, nkon, praet:sg:m1.m2.m3:perf). % 2
%slowskr('zł', 'zł', npun, kon, brev:npun). % 2
%slowskr('zamieszkały', 'zam', pun, nkon, praet:pl:m2.m3.f.n1.n2.p2.p3:perf). % 2
%slowskr('październik', 'X', npun, kon, subst:sg:nom.acc:m3). % 2
%slowskr('wyżej wymieniony', 'ww', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 2
%slowskr('wolt', 'V', npun, kon, subst:pl:gen:f). % 2
%slowskr('to znaczy', 'tzn', pun, nkon, fin:sg:ter:imperf). % 2
%slowskr('towarzysz', 'tow', pun, nkon, impt:sg:sec:imperf). % 2
%slowskr('starszy', 'st', pun, nkon, adj:sg:nom.voc:m1.m2.m3:com|adj:sg:acc:m3:com). % 2
%slowskr('strona', 'str', pun, nkon, subst:sg:nom:f). % 2
%slowskr('Solidarność', 'S', npun, kon, subst:sg:nom.acc:f). % 2
%slowskr('spółka', 'Sp', pun, nkon, subst:sg:nom:f). % 2
%slowskr('spółka akcyjna', 'S.A', pun, nkon, adj:sg:nom.voc:f:pos). % 2
%slowskr('redaktor', 'red', pun, nkon, subst:sg:nom:m1). % 2
%slowskr('pod wezwaniem', 'pw', pun, nkon, ger:sg:inst:n2:perf:aff). % 2
%slowskr('post scriptum', 'PS', pun, nkon, '???'). % 2
%slowskr('post scriptum', 'P.S', pun, nkon, '???'). % 2
%slowskr('postscriptum', 'P.S', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc.voc:n2). % 2
%slowskr('przypisek', 'przyp', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('projekt', 'proj', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('podpułkownik', 'ppłk', npun, kon, subst:sg:nom:m1). % 2
%slowskr('powiat', 'pow', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('porównaj', 'por', pun, nkon, impt:sg:sec:perf). % 2
%slowskr('położony', 'położ', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 2
%slowskr('północny', 'Płn', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 2
%slowskr('pułkownik', 'płk', pun, nkon, subst:sg:nom:m1). % 2
%slowskr('plac', 'pl', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('osiedle', 'os', pun, nkon, subst:sg:nom.acc.voc:n2). % 2
%slowskr('około', 'Ok', pun, nkon, qub). % 2
%slowskr('odległość', 'odl', pun, nkon, subst:sg:nom.acc:f). % 2
%slowskr('odbudowany', 'odbud', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 2
%slowskr('obwód', 'obw', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('numer', 'Nr', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('na przykład', 'Np', pun, nkon, subst:sg:nom.acc:m3). % 2
%slowskr('metr kwadratowy', 'mkw', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 2
%slowskr('magister', 'mgr', npun, kon, subst:sg:nom:m1). % 2
%slowskr('megabajt', 'MB', npun, kon, subst:sg:nom.acc:m3). % 2
%slowskr('metr sześcienny', 'm3', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 2
%slowskr('książę', 'ks', pun, nkon, subst:sg:nom.voc:m1). % 2
%slowskr('kodeks postępowania cywilnego', 'k.p.c', pun, nkon, adj:sg:gen:m1.m2.m3.n1.n2:pos|adj:sg:acc:m1.m2:pos). % 2
%slowskr('kilometr kwadratowy', 'km²', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 2
%slowskr('kaszubski', 'kaszb', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 2
%slowskr('Jezioro', 'Jez', pun, nkon, subst:sg:nom.acc.voc:n2). % 2
%slowskr('inspektor', 'insp', pun, nkon, subst:sg:nom:m1). % 2
%slowskr('IMHO', 'IMHO', npun, kon, '???'). % 2
slowskr('gmina', 'gm', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc:f). % 2
%slowskr('editor', 'ed', pun, nkon, '???'). % 2
%slowskr('do spraw', 'd/s', npun, kon, impt:sg:sec:perf). % 2
%slowskr('doktor', 'dra', npun, kon, subst:sg:nom:m1.f|subst:sg:gen.dat.acc.inst.loc.voc:f|subst:pl:nom.gen.dat.acc.inst.loc.voc:f). % 2
%slowskr('dzień', 'd', pun, nkon, subst:sg:nom.acc:m3.f). % 2
%slowskr('CS', 'CS', npun, kon, '???'). % 2
%slowskr('związek', 'Zw', pun, nkon, subst:pl:gen:f). % 1
%slowskr('zmiana', 'zm', pun, nkon, subst:sg:nom:f). % 1
%slowskr('zdjęcie', 'zdj', pun, nkon, subst:sg:nom.acc.voc:n2). % 1
%slowskr('zastępca', 'z-ca', npun, kon, subst:sg:nom:m1). % 1
%slowskr('zawodowy', 'Zaw', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('założony', 'założ', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 1
%slowskr('względnie', 'wzgl', pun, nkon, adv:pos). % 1
%slowskr('wyżej wymieniony', 'w.w', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 1
%slowskr('Wielki', 'W', pun, nkon, subst:sg:nom.voc:m1). % 1
%slowskr('wschód', '-wsch', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('wschodni', 'wsch', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos|adj:pl:nom.voc:m1.p1:pos). % 1
%slowskr('wchód', 'wsch', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('winien', 'Wn', npun, kon, adjc). % 1
slowskr('Wielkopolski', 'Wlkp', pun, nkon, adj:sg:nom.gen.dat.acc.inst.loc:m3:pos). % 1
%slowskr('WG', 'WG', npun, kon, brev:npun). % 1
%slowskr('water closet', 'w.c', pun, nkon, '???'). % 1
%slowskr('wagowy', 'wag', pun, nkon, subst:sg:nom.voc:m1). % 1
%slowskr('tak zwane', 'tzw', pun, nkon, ppas:sg:nom.acc:n1.n2:imperf:aff|ppas:pl:nom.acc:m2.m3.f.n1.n2.p2.p3:imperf:aff). % 1
%slowskr('tom', 't', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('towarzystwo', 'Tow', pun, nkon, subst:sg:nom.acc.voc:n2). % 1
%slowskr('TK', 'TK', npun, kon, subst:sg.pl:nom.gen.dat.acc.inst.loc.voc:m3). % 1
%slowskr('to jest', 'tj', pun, nkon, fin:sg:ter:imperf). % 1
%slowskr('tak zwany', 'tgz', pun, nkon, ppas:sg:nom:m1.m2.m3:imperf:aff|ppas:sg:acc:m3:imperf:aff). % 1
%slowskr('świętej pamięci', 'śp', pun, nkon, subst:sg:gen.dat.loc.voc:f|subst:pl:nom.gen.acc.voc:f). % 1
%slowskr('Sz', 'Sz', pun, nkon, '???'). % 1
%slowskr('sześcienny', 'sześć', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('***', 'sz', npun, kon, interp). % 1
%slowskr('styczeń', 'sty', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('stratus', 'St', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('stołeczny', 'st', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('Stanisław', 'St', pun, nkon, subst:pl:gen:f). % 1
%slowskr('siostra', 'ss', pun, nkon, subst:sg:nom:f). % 1
%slowskr('solidarność', 'S', npun, kon, subst:sg:nom.acc:f). % 1
%slowskr('sen', 'S', pun, nkon, brev:pun). % 1
%slowskr('spółka', 'sp', pun, nkon, subst:sg:nom:f). % 1
%slowskr('sierpień', 'sie', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('senator', 'sen', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('sekunda', 'sek', pun, nkon, subst:sg:nom:f). % 1
%slowskr('stratocumulus', 'Sc', npun, kon, subst:sg:nom:m2.m3|subst:sg:acc:m3). % 1
%slowskr('RZ', 'RZ', npun, kon, '???'). % 1
%slowskr('rycina', 'ryc', pun, nkon, subst:sg:nom:f). % 1
%slowskr('rentgenowski', 'RTG', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('rozbudowany', 'rozbud', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 1
%slowskr('redaktor', 'Red', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('***', 'R', npun, kon, interp). % 1
%slowskr('Księga Powtórzonego Prawa', 'Pwt', npun, kon, adj:sg:nom.voc:f:pos). % 1
%slowskr('piątek', 'pt', pun, nkon, subst:pl:gen:f). % 1
%slowskr('pseudonim', 'ps', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('post scriptum', 'PS', npun, kon, '???'). % 1
%slowskr('postscriptum', 'PS', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc.voc:n2). % 1
%slowskr('przebudowany', 'przebud', pun, nkon, ppas:sg:nom:m1.m2.m3:perf:aff|ppas:sg:acc:m3:perf:aff). % 1
%slowskr('przebudowa', 'przebud', pun, nkon, subst:sg:nom:f). % 1
%slowskr('późniejszy', 'późn', pun, nkon, adj:sg:nom.voc:m1.m2.m3:com|adj:sg:acc:m3:com). % 1
%slowskr('poniedziałek', 'pon', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('podkomisarz', 'podkom', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('punkt', 'pnkt', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('przed naszą erą', 'pne', pun, nkon, subst:sg:inst:f). % 1
%slowskr('północny', 'płn', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('pkt', 'pkt', pun, nkon, '???'). % 1
%slowskr('południe', 'pd', pun, nkon, subst:sg:nom.acc.voc:n2). % 1
%slowskr('papier', 'pap', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('stopień', 'o', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('ojciec', 'o', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('odpowiedzialność', 'odp', pun, nkon, subst:sg:nom.acc:f). % 1
%slowskr('na temat', 'nt', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('nie ta grupa', 'NTG', npun, kon, subst:sg:nom:f). % 1
%slowskr('nad poziomem morza', 'n.p.m', pun, nkon, subst:sg:gen:n2|subst:pl:nom.acc.voc:n2). % 1
%slowskr('nasza', 'n', pun, nkon, fin:sg:ter:imperf). % 1
%slowskr('nadkomisarz', 'nadkom', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('megawat', 'MW', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('Mr', 'mr', npun, kon, '???'). % 1
%slowskr('mikrometr', 'µm', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('młodszy', 'Mł', pun, nkon, adj:sg:nom.voc:m1.m2.m3:com|adj:sg:acc:m3:com). % 1
%slowskr('major', 'mjr', npun, kon, subst:sg:nom:m1.m3|subst:sg:gen.dat.acc.inst.loc.voc:m3|subst:pl:nom.gen.dat.acc.inst.loc.voc:m3). % 1
%slowskr('ministerstwo', 'Min', pun, nkon, subst:sg:nom.acc.voc:n2). % 1
%slowskr('minimum', 'min', pun, nkon, subst:sg:nom.gen.dat.acc.inst.loc.voc:n2). % 1
%slowskr('między innymi', 'M.in', pun, nkon, subst:pl:inst:m1). % 1
%slowskr('między innnymi', 'm.in', pun, nkon, '???'). % 1
%slowskr('megabajt na sekundę', 'Mbps', npun, kon, subst:sg:acc:f). % 1
%slowskr('MB', 'MB', npun, kon, subst:sg:nom.acc:m3.n2|subst:sg:gen.dat.inst.loc.voc:n2|subst:pl:nom.gen.dat.acc.inst.loc.voc:n2). % 1
%slowskr('Mateusz', 'Mat', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('marzec', 'mar', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('łaciński', 'łac', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('luty', 'lut', npun, kon, subst:sg:nom.acc.voc:m3). % 1
%slowskr('limited company', 'Ltd', pun, nkon, '???'). % 1
%slowskr('kodeks wykroczeń', 'k.w', pun, nkon, subst:pl:gen:n2). % 1
%slowskr('ksiądz', 'Ks', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('kapitan', 'kpt', pun, nkon, subst:pl:gen:f). % 1
%slowskr('koń mechaniczny', 'KM', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('km2', 'km2', npun, kon, '???'). % 1
%slowskr('klasa', 'kl', pun, nkon, subst:sg:nom:f). % 1
%slowskr('Kościół Katolicki', 'KK', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('karta', 'k', pun, nkon, subst:sg:nom:f). % 1
%slowskr('kilobajt na sekundę', 'Kbps', npun, kon, subst:sg:acc:f). % 1
%slowskr('katolicki', 'kat', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('kategoria', 'kat', pun, nkon, subst:sg:nom:f). % 1
%slowskr('kszubski', 'kaszb', pun, nkon, '???'). % 1
%slowskr('kardynał', 'kard', pun, nkon, subst:sg:nom:m1.m2). % 1
%slowskr('jezioro', 'J', pun, nkon, subst:sg:nom.acc.voc:n2). % 1
%slowskr('i tym podobnie', 'itp', pun, nkon, adv:pos). % 1
%slowskr('itp', 'itp', pun, nkon, '???'). % 1
%slowskr('harcmistrz', 'hm', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('habilitowany', 'hab', pun, nkon, ppas:sg:nom:m1.m2.m3:imperf.perf:aff|ppas:sg:acc:m3:imperf.perf:aff). % 1
%slowskr('grudzień', 'gru', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('grosz', 'gr', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('godzina', 'Godz', pun, nkon, subst:sg:nom:f). % 1
%slowskr('godz', 'godz', pun, nkon, brev:pun). % 1
%slowskr('gigadżul', 'GJ', npun, kon, '???'). % 1
%slowskr('generał', 'Gen', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('Gdański', 'GD', pun, nkon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('fundacja', 'fund', pun, nkon, subst:sg:nom:f). % 1
%slowskr('fotograf', 'fot', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('era', 'e', pun, nkon, subst:sg:nom:f). % 1
%slowskr('dotyczący', 'dot', pun, nkon, pact:sg:nom:m1.m2.m3:imperf:aff|pact:sg:acc:m3:imperf:aff|pact:pl:nom:m1.p1:imperf:aff). % 1
%slowskr('dolina', 'dol', pun, nkon, subst:sg:nom:f). % 1
%slowskr('dnia', 'dn', pun, nkon, subst:sg:gen:m3). % 1
%slowskr('decymetr sześcienny', 'dm3', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('druh', 'dh', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('Dominikańskie Centrum Informacji o Sektach', 'DCIoS', npun, kon, subst:pl:loc:f). % 1
%slowskr('Ćwiczenia Duchowe', 'ĆD', npun, kon, adj:sg:nom.acc.voc:n1.n2:pos|adj:pl:nom.acc.voc:m2.m3.f.n1.n2.p2.p3:pos). % 1
%slowskr('czytaj', 'czyt', pun, nkon, impt:sg:sec:imperf). % 1
%slowskr('cytat', 'cyt', pun, nkon, subst:pl:gen:f). % 1
%slowskr('ciąg dalszy', 'Cd', pun, nkon, adj:sg:nom.voc:m1.m2.m3:com|adj:sg:acc:m3:com). % 1
%slowskr('centymetr sześcienny', 'ccm', npun, kon, adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos). % 1
%slowskr('by the way', 'BTW', npun, kon, '???'). % 1
%slowskr('BTW', 'BTW', npun, kon, '???'). % 1
%slowskr('bieżący rok', 'br', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('biskup', 'bp', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('biskup', 'bp', npun, kon, subst:sg:nom:m1). % 1
%slowskr('bieżący miesiąc', 'bm', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('bieżący miesiąc', 'bm', pun, nkon, subst:sg:nom.acc:m3). % 1
%slowskr('bilion', 'bln', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('bardzo', 'b', pun, nkon, adv:pos). % 1
%slowskr('atrykuł', 'art', pun, nkon, '???'). % 1
%slowskr('artytuł', 'art', pun, nkon, '???'). % 1
%slowskr('artysta', 'art', pun, nkon, subst:sg:nom:m1). % 1
%slowskr('artykuł', 'art', npun, kon, subst:sg:nom.acc:m3). % 1
%slowskr('angielski', 'ang', pun, nkon, subst:sg:nom.acc.voc:m3). % 1
%slowskr('agielski', 'ang', pun, nkon, '???'). % 1
%slowskr('AE', 'AE', npun, kon, '???'). % 1
%slowskr('Anno Domini', 'A.D', pun, nkon, '???'). % 1
%slowskr('7-procentowy', '7-proc', pun, nkon, '???'). % 1
%slowskr('50-procentowy', '50-proc', pun, nkon, '???'). % 1
%slowskr('10-tysięczny', '10-tys', pun, nkon, '???'). % 1

slowskr('A', 'A', npun, kon, ini). % 3
slowskr('A', 'A', pun, nkon, ini). % 20
slowskr('B', 'B', pun, nkon, ini). % 12
slowskr('C', 'C', pun, nkon, ini). % 4
slowskr('Ch', 'Ch', pun, nkon, ini). % 4
slowskr('D', 'D', pun, nkon, ini). % 14
slowskr('E', 'E', pun, nkon, ini). % 8
slowskr('F', 'F', pun, nkon, ini). % 3
slowskr('G', 'G', pun, nkon, ini). % 14
slowskr('H', 'H', pun, nkon, ini). % 7
slowskr('I', 'I', pun, nkon, ini). % 2
slowskr('J', 'J', pun, nkon, ini). % 18
slowskr('K', 'K', pun, nkon, ini). % 29
slowskr('L', 'L', pun, nkon, ini). % 5
slowskr('M', 'M', pun, nkon, ini). % 26
slowskr('N', 'N', pun, nkon, ini). % 10
slowskr('O', 'O', pun, nkon, ini). % 4
slowskr('P', 'P', pun, nkon, ini). % 18
slowskr('R', 'R', pun, nkon, ini). % 8
slowskr('S', 'S', pun, nkon, ini). % 27
slowskr('T', 'T', npun, kon, ini). % 1
slowskr('T', 'T', pun, nkon, ini). % 6
slowskr('V', 'V', pun, nkon, ini). % 8
slowskr('W', 'W', npun, kon, ini). % 3
slowskr('W', 'W', pun, nkon, ini). % 26
slowskr('Z', 'Z', pun, nkon, ini). % 6
slowskr('Ż', 'Ż', pun, nkon, ini). % 1

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
