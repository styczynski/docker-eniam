%
% Author: Marcin Woliński
% This file is in the public domain.
%
% Ten plik definiuje zestaw modułów potrzebny do wsadowej analizy
% zdań z użyciem Morfeusza.
%
% przykład wywołania:
% swipl -g "[swigra2_batch,'Mysz.doa']" -t halt >Mysz.forest
% 
% Plik .doa powinien zawierać wywołanie predykatu analiza/1 lub analiza/2.

% To ze względu na Windows, gdzie UTF-8 nie wydaje się być domyślny:
:-set_prolog_flag(encoding,utf8).

% Test nowych fraz nominalnych:
:-[
   birnam_run,
   birnam_dumpedges,
   gfjp_morfologia,
   gfjp_slowskr,
   gfjp_slowpart,
   slowczas, slowrzecz, slowprzym, slowwykrz,
   gfjp_analiza,
   gfjp2_wymagania,
   birnam_sequences,
   gfjp2_warunki,
   gfjp2,
   gfjp2_arnoscint,
   gfjp_swidzinskify
].

% zaszłość ze Świgry 1, której już nie potrzebujemy:
:-abolish(dodajprzec/3).
dodajprzec(_Od, _Do, _Add).

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
