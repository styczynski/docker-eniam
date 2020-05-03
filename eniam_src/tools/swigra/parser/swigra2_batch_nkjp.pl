%
% Author: Marcin Woliński
% This file is in the public domain.
%
% Ten plik definiuje zestaw modułów potrzebny do wsadowej analizy
% zdań, w których dane fleksyjne pochodzą z NKJP.  Odpowiednie pliki
% danych do analizy można wyekstrahować z NKJP za pomocą skryptu
% prepdataNKJP.

% To ze względu na Windows, gdzie UTF-8 nie wydaje się być domyślny:
:-initialization(set_prolog_flag(encoding,utf8)).
:-set_prolog_flag(encoding,utf8).

% To jest sygnał dostarczany, gdy limit czasu zostanie wyczerpany.
% Coś tryka przy jego przechwytywaniu do throw, więc niech się odczepi:
:-initialization(on_signal(xcpu,_,default)).

% Test nowych fraz nominalnych:
:-[
   birnam_run,
%   birnam_dumpforest,
   birnam_dumpedges,
   gfjp_morfologia_nkjp,
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




%%% Local Variables: 
%%% coding: utf-8
%%% mode: prolog
%%% End: 
