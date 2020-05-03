Before using Świgra it might be necessary to compile the Prolog interface to the morphological analyzer Morfeusz.  
First the package libmorfeusz2-dev must be downloaded from sgjp.pl/morfeusz/dopobrania.html and installed.  
(WARNING: do NOT install the package morfeusz2 as it conflicts with the required package)
Then the interface can be compiled by executing

    swipl-ld -v -shared -o morfeusz2-swi.so morfeusz2-glueswi.cc -lmorfeusz2  
in the parser directory.

For an example usage of Świgra's server mode (in OCaml) see parser/swigra_test.ml  
The demo assumes you are using swipl which you can install by executing:

    sudo apt-get install swi-prolog  
It also makes use of curl which can be installed with:

    sudo apt-get install curl

Below are the contents of the original README file.

========================================================================
Analizator składniowy Świgra

========================================================================

Paczka zawiera analizator składniowy Świgra w wersji 1 (2007) i w wersji
rozwojowej kandydującej do nazwy Świgra 2 (2016).

Wersja 1 stanowi komputerową realizację gramatyki przedstawionej przez
prof. Marka Świdzińskiego w jego książce „Gramatyka formalna języka
polskiego” (1992).  Wersja 1 jest częścią wyników pracy doktorskiej
Marcina Wolińskiego opracowanej i obronionej w Instytucie Podstaw
Informatyki PAN w Warszawie w maju 2005. Inicjatorem i promotorem
pracy był Janusz S. Bień, aktualnie z Katedry Lingwistyki Formalnej
Uniwersytetu Warszawskiego.

Wersja 2 reguł została napisana od nowa przez Marcina Wolińskiego przy
współpracy z prof. Świdzińskim.

Runtime analizatora nazywamy roboczo Birnam (z hasłem reklamowym „We
bring forests to your doors”).  Stanowi on interpreter gramatyk DCG
stosujący strategię wstępującą z zapamiętywaniem wyników pośrednich.
Zawiera też pewne rozszerzenia specyficzne dla implementowanej
gramatyki.

Nie wszystkie pliki składające się na analizator zostały udostępnione
na tej samej licencji.  Warunki licencji można sprawdzić w nagłówkach
poszczególnych plików.  Większość plików składających się na te
programy zostaje udostępniona na licencji GNU General Public License 3
(por. plik gpl-3.0.txt).  Pliki umożliwiające skład drzew analizy w
LaTeXu zostały udostępnione na licencji LaTeX Project Public License
wersja 1.2, ponieważ licencja GPL nie jest zgodna ze standardowymi
modułami LaTeXa.  Niektóre pliki pomocnicze zostały dla uproszczenia
uznane za dobro publiczne — autor zrzekł się praw autorskich do nich.
Dotyczy to przykładu użycia platformy Birnam z inną gramatyką oraz
kodu w języku C pozwalającego na wywołanie analizatora Morfeusz z
programu prologowego.

Udostępnienie wersji 1 analizatora na licencji GPL było możliwe dzięki
uprzejmej zgodzie następujących osób i instytucji:

1. prof. Marka Świdzińskiego jako autora reguł gramatycznych,

2. dr. hab. Janusza S. Bienia, prof. UW jako kierownika zrealizowanego
   w Instytucie Informatyki Uniwersytetu Warszawskiego projektu KBN
   8T11C 00213 "Zestaw testów do weryfikacji i oceny analizatorów
   języka polskiego", w ramach którego w latach 1997-1999 powstał
   prototyp analizatora Świgra (pod nazwą AS),

3. dyrekcji Instytutu Podstaw Informatyki PAN - mojego pracodawcy.

Rozwój wersji 2 był finansowany w ramach projektów:
• MNiSW N N104 224735 (2008–2011),
• Nekst http://www.ipipan.waw.pl/nekst/ (2012–2013),
• CLARIN-PL http://clarin-pl.eu/ (2014–2016).

Właścicielem majątkowych praw autorskich do wersji 2 jest Instytut
Podstaw Informatyki Polskiej Akademii Nauk.

========================================================================
Użytkowanie w systemie Ubuntu
========================================================================

Pliki analizatora i dezambiguatora muszą znaleźć się w systemie plików
w takich podkatalogach, jak w paczce dystrybucyjnej.  Analizator (z
wczepionym dezambiguatorem) uruchamia się za pomocą skryptów zawartych
w podkatalogu parser/.

Najwygodniejszym sposobem interaktywnego użycia Świgry wydaje się
interfejs webowy, który można wywołać poleceniem
	  ./swigra -w
Alternatywnie można użyć interfejsu okienkowego SWI Prologu:
	  ./swigra
Domyślnie używana jest Świgra 2, aby wywołać archiwalną wersję Świgry
jeden w wywołaniu skryptu musi znaleźć się argument -1.

Świgra używa analizatora morfologicznego Morfeusz 2
(http://sgjp.pl/morfeusz), który musi być zainstalowany w systemie.
Dodatkową komplikację stanowi fragment kodu łączącego Morfeusza 2 z
SWI Prologiem (morfeusz2-glueswi.cc), który musi zostać skompilowany
do biblioteki dynamicznej zgodnie z opisem na początku wymienionego
pliku źródłowego.  Dystrybucja zawiera kilka wariantów kompilatu
(morfeusz2-*so), w szczególności morfeusz2-swi7-64.so dla Ubuntu 16.04
64-bit.  W razie niezgodności należy przekompilować kod.

W trybie wsadowym Świgra może używać Morfeusza lub pracować na
interpretacjach dostarczonych w formacie NKJP.

Pakiety Ubuntu konieczne dla działania całej wsadowej ścieżki
przetwarzania (skrypty genparser i swigra_batch):

swi-prolog
libbsd-resource-perl
libfile-slurp-perl
libxml-libxml-perl



========================================================================
Marcin Woliński                                 <wolinski@ipipan.waw.pl>
Zespół Inżynierii Lingwistycznej
Instytut Podstaw Informatki PAN

%%% Local Variables: 
%%% coding: utf-8
%%% mode: text
%%% End: 
