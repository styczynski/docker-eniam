%
% Author: Marcin Woliński
% This file is in the public domain.
%

% To ze względu na Windows, gdzie UTF-8 nie wydaje się być domyślny:
:-set_prolog_flag(encoding,utf8).

% Potrzebne moduły Birnama i Świgry:
:-[
   birnam_run,
   birnam_xpce,
   gfjp_morfologia,
   gfjp_slowskr,
   gfjp_slowpart,
   slowczas, slowrzecz, slowprzym, slowwykrz,
   gfjp_analiza,
   gfjp2_wymagania,
   birnam_sequences,
   gfjp2_warunki,
   gfjp2,
   gfjp2_arnoscint% ,
   % gfjp_swidzinskify
].

:-abolish(dodajprzec/3).
dodajprzec(_Od, _Do, _Add).

:-run.

%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
