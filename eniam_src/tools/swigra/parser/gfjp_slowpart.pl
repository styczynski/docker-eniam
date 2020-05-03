% Słownik partykuł
%
% Przyporządkowuje partykule listę typów fraz, których może ona być
% podrzędnikiem (zob. komentarz w sgfp2.dcg).
%
% Copyright © 2010 Marcin Woliński & Marek Świdziński
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


% slowpart(Leksem,Klasy). % SGJP, Uwagi MŚ, fw

slowpart(akurat, [fno,flicz,fpt,fpm,fin]). % ,,fw
%(koszmar) slowpart(ale, [fin]). % „Ale się kotłuje!”
slowpart(ależ, [fin]). % „Ale się kotłuje!”
slowpart(ani, [flicz,fno,fin]).
slowpart(ano, [fin]). % „Ano miały.”
slowpart(aż, [fno,flicz,fpt,fpm,fin]).
slowpart(boć, [fin]). % lemizm
slowpart(blisko,[flicz]).
slowpart(bodaj, [fno,flicz,fpt,fpm,fin]). % +op.tr.,,
slowpart(byle, [fno]). % Op.tr, sp.p., op.ads.,byle co, kto, kiedy, który, skąd,
slowpart(całkiem, [fpt,fpm]). % ,,fw
slowpart(chociaż, [fno,flicz,fpt,fpm,fin]).
slowpart(chociażby, [fno,flicz,fpt,fpm]).
slowpart(choć, [fno,flicz,fpt,fpm,fin]).
slowpart(choćby, [fno,flicz,fpt,fpm,fin]).
slowpart(chyba, [fno,flicz,fpt,fpm,fin]).
slowpart(coraz, [fpt]). % Przys.ze st. w.,,
%slowpart(cóż, []). % Rzecz.!!!,Wykrz.,
%slowpart(czyżby, [fin]). % pytajne
slowpart(dalibóg, [fin]). % „dalibóg nie wiem”
slowpart(dopiero, [fno,flicz,fpt,fpm,fin]).
slowpart(doprawdy, [fpt]). % Dopow.,,
slowpart(dosyć, [fpt,fin]). % ,,fw  % „wiedział dosyć.” — przysł.???
slowpart(dość, [fpt,fin]). % ,,fw % jw.
%slowpart(ewentualnie, [fno,flicz]). % można by dodać ze względu na „prócz ewentualnie ludzi pijanych”
slowpart(głównie, [fno,fpm]). % +MW jest też przysłówek
% zablokowałem, bo to powoduje koszmarne koszmary w uczciwych koordynacjach:
slowpart(i, [fno]). % „Zdarzali się i niekomuniści.” +MW
slowpart(istotnie, [fin]).
slowpart(jakby, [fno,flicz,fpt,fpm,fin]).
slowpart(jakoby, [fin]). % +MW
slowpart(jednak, [fno,flicz,fpt,fpm,fin]). % +MW
slowpart(jedynie, [fno,flicz,fpt,fpm,fin]).
slowpart(jeszcze, [fno,flicz,fpt,fpm,fin]). % ,,fw
slowpart(już, [fno,flicz,fpt,fpm,fin]). % ,,fw
%slowpart(mimochodem, [fin]). % ,Przysl.,fw
slowpart(może, [fno,flicz,fpt,fpm,fin]). % +dopow.,,
slowpart(nader, [fpt]). % Przys.,,
slowpart(najwyraźniej, [fno,flicz,fpm,fin]).
slowpart(naprawdę, [fno,flicz,fpt,fpm,fin]). % ,,fw?
%slowpart(natychmiast, []). % ,Przysl.,fw
slowpart(nawet, [fno,flicz,fpt,fpm,fin]).
%slowpart(nawzajem, []). % Dopow., przys.,Przysl.,fw
slowpart(nareszcie, [fin]). % 
slowpart(nazbyt, [fpt]). % Przys.???,Nie: part.,
slowpart(niby, [fno,flicz,fpt,fpm,fin]).
slowpart(nie, [fpm]). % historyjka nie z tej ziemi
slowpart(nieco, [fpt]). % Zaim.ps.,Nie: part.,fw
slowpart(niemal, [fno,flicz,fpt,fpm,fin]). % Przys.adnum.,, %+MW
slowpart(niemalże, [fno,flicz,fpt,fpm,fin]). % Przys.adnum.,, %+MW
slowpart(nieomal, [flicz,fpt,fpm,fin]).
slowpart(niespełna, [fno,flicz,fpt]). % Przys.adnum.,,
slowpart(niestety, [fin]). % Part.,Wykrz.? Też dopow.,
slowpart(niezbyt, [fpt]).
slowpart(no, [fin]).  % modyfikator deklaratywności, partykuła wzmacniająca +SGJP
slowpart(oczywiście, [fno,fin]). %+MW ?
slowpart(około, [flicz]).
slowpart(omal, [flicz,fpt,fpm,fin]). % ,Omal nie???,
%slowpart(ot, []). % Part.???,Wykrz.,
slowpart(oto, [fno,flicz,fpt,fpm,fin]).
%slowpart(otóż, []). % ,Co to jest???,
slowpart(owszem, [fin]).
slowpart(pewnie, [fno,flicz,fpt,fpm,fin]). % +MW
slowpart(podobno, [fno,flicz,fpt,fpm,fin]). % +MW
slowpart(ponad, [flicz,fno]).
slowpart(ponadto, [fno,flicz,fpt,fpm,fin]).
slowpart(poniekąd, [fno,flicz,fpt,fpm,fin]). % ,???,
slowpart(ponoć, [fno,flicz,fpt,fpm,fin]).
slowpart(prawdopodobnie, [fno,flicz,fpt,fpm,fin]).
slowpart(prawie, [fno,flicz,fpt,fpm,fin]).
slowpart(przecie, [fno,flicz,fpt,fpm,fin]). % ,???,
slowpart(przecież, [fno,flicz,fpt,fpm,fin]). % ,???,
slowpart(przeszło, [flicz,fno]). % „na przeszło trójkę”
slowpart(przynajmniej, [fno,flicz,fpt,fpm,fin]).
slowpart(raczej, [fno,flicz,fpt,fpm,fin]).
slowpart(raptem, [fno,flicz,fpt,fpm,fin]). % Przysl.!!!,Też part.,fw
%slowpart(razem, []). % Przysł.,Przysl.,fw
slowpart(równie, [fpt]). % Przysł.???,Part.,
slowpart(również, [fno,flicz,fpt,fpm,fin]).
slowpart(rzekomo, [fno,flicz,fpt,fpm,fin]).
%%slowpart(sobie, [fin]). % ,???,
slowpart(także, [fno,fpt,fpm,fin]). % +MW
%slowpart(tam, []). % ,,fw
%slowpart(tedy, []). % ,???Dysk.!!!,
slowpart(też, [fno,fpt,fpm,fin]).
slowpart(toż, [fin]).
slowpart(trochę, [fpt,fpm]). % ,Przysl.,fw
slowpart(troszkę, [fpt,fpm]). % ,Przysl.,fw
%slowpart(tu, []). % ,Przysl.,fw
slowpart(tuż, [fpm]). % Tuż przed ósmą / po dziewiątej / koło domu.
slowpart(tylko, [fno,flicz,fpt,fpm,fin]).
%slowpart(tymczasem, []). % ,Przysl.,fw
slowpart(wcale, [fpt,fin]). % ,fwe: neg?,
slowpart(wciąż, [fpt,fin]). % ,,fw
%slowpart(więc, []). % ,???Dysk!!!,
slowpart(widocznie,[fin]).
slowpart(właśnie, [fno,fin]). % „właśnie ta połowa była zagrożona”
slowpart(właściwie,[fin]).
%slowpart(wnet, []). % ,,fw
%slowpart(wpierw, []). % ,Przysl.,fw
slowpart(wprost, [fpm,fin]). % Przysl., op.ads., ,Fpn: frazeol.?,fw
slowpart(wreszcie, [fno,fpt,fpm,fin]). % ,Przysl.,fw % +MW!?  SGJP part.
slowpart(wręcz, [fno,fpt,fpm,fin]). % 
slowpart(wszak, [fno,flicz,fpt,fpm,fin]). % ,???,
slowpart(wszakże, [fno,flicz,fpt,fpm,fin]). % ,???,
slowpart(wszelako, [fno,flicz,fpt,fpm,fin]). % ,???,
%slowpart(z, [fno,flicz]).
slowpart(za, [fpt]).
%slowpart(zaiste, []). % ,???,
slowpart(zaledwie, [fno,flicz,fpt,fpm,fin]). % MW: +fpt „wykształcenie zaledwie zawodowe”, +fpm „placek posypany malinami zaledwie na środku”
slowpart(zapewne, [fno,flicz,fpt,fpm,fin]).
slowpart(zaraz, [fps,fpm]).
slowpart(zarazem, [fin,nfin]).	 % ,???,
%slowpart(zatem, []). % ,???,
slowpart(zbyt, [fpt,fpm]). % MW: +fpm: „zbyt na wierzchu”
slowpart(zgoła, [fno, fpt,fin]). % ,???,
slowpart(znowu, [fpt,fin]). % ,Przysl.,fw
slowpart(znów, [fpt,fin]). % ,Przysl.,fw
slowpart(zresztą, [fno,flicz,fpt,fin]).
slowpart(zwłaszcza, [fno,flicz,fpt,fpm,fin]).
slowpart(że, [fno,flicz,fpt,fpm,fin]). % na okoliczność „leksemu wzmacniajacego «że»”, np. «czym|że»

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
