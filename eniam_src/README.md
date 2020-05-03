Kategorialny Parser Składniowo-Semantyczny „ENIAM” jest narzędziem generującym formy logiczne dla zdań w języku polskim. Parser pracuje na niepreparowanych danych, realizuje kolejne etapy przetwarzania tekstu: tokenizację, lematyzację, rozpoznawanie związków składniowych, anotację sensami słów oraz rolami tematycznymi, częściową dezambiguację oraz tworzenie reprezentacji semantycznej. Integruje informację zawartą w zasobach wytworzonych w ramach Clarin-pl m. in. w Słowosieci i Walentym.

Kolejne etapy przetwarzania tekstu realizowane są w ramach rozmytego potoku przetwarzania. Parser nie dezambiguuje na bieżąco niejednoznaczności powstającej po każdym kroku przetwarzania tekstu. Zamiast tego tworzy zwartą reprezentację niejednoznacznego wyniku, którą przekazuję do następnego etapu, dezambiguację wykonuje pod koniec potoku przetwarzania.

Początkowe etapy przetwarzania tekstu (poprzedzające określenie struktury zależnościowej) będziemy określać mianem preprocesingu. Etapy te realizowane są przez program subsyntax stanowiący wolnostojący serwer sieciowy, z którym komunikuje się parser eniam.

Podczas preprocesingu następuje integracja zasobów. Przetwarzane zdanie anotowane jest m. in. informacjami pochodzącymi następujących zasobów: SGJP, Polimorf, Słowosieć, Walenty. Kolejne etapy preprocesingu to tokenizacja, lematyzacja, wykrywanie nazw własnych, określanie sensów słów, określanie walencji. Na tym etapie wykonywane jest też rozwijanie skrótów oraz rozpoznawanie wyrażeń wielosłownych.

Zaanowowany podczas preprocesingu tekst przetwarzeny jest następnie przez parser działający w oparciu o gramatykę kategorialną (Type Logical Categorial Grammar). Mają tu miejsce następujące działania: generowane wpisu w leksykonie, określanie struktury zależnościowej, nadawanie walencji semantycznej, dezambiguacja, tworzenie reprezentacji semantycznej.

Parsowanie gramatykach kategorialnych jest dowodzeniem twierdzeń w niekomutatywnej intuicjonistycznej logice liniowej. Reguły gramatyczne to uniwersalne, niezależne od przetwarzanego języka reguły dowodzenia w logice. Powoduje to, że gramatyka jest w pełni zleksykalizowana, tj. cała wiedza o przetwarzanym języku znajduje się w leksykonie. Spójniki LCG wyrażają podstawowe zjawiska występujące w języku takie jak konkatenacja (tworzenie wektorów cech), niejednoznaczność, wymaganie argumentu, polimorficzne argumenty, sumowanie typów pusty argument, wielokrotny argument, parametryzowana niejednoznaczność, tworzenie listy.

Leksykon generowany jest dynamicznie dla poszczególnych zdań na podstawie informacji morfosyntaktycznej formy oraz schematu walencyjnego leksemu. Stanowi bezpośrednie tłumaczenie informacji dostarczanej przez lematyzację i zawartej w Walentym na język spójników LCG.

Gramatyka uzupełniona jest o konstrukcje mowy niezależnej oraz zleksykalizowany opis określeń czasu.

Należy tutaj zaznaczyć, że ENIAM nie korzysta z gramatyki języka polskiego w tradycyjnym rozumieniu tego słowa. Więzy gramatyczne pomiędzy słowami zadane są przez słownik walencyjny uzupełniony o informację o możliwych modyfikatorach dla danego typu leksemu. Z racji tego, że gramatyka kategorialna jest w pełni zleksykalizowana nie było potrzeby tworzyć ogólnych reguł mówiących np. o tym, że rzeczownik uzgadnia się z przymiotnikiem pod względem przypadku, liczy i rodzaju. Informacja ta zawarta jest częściowo w walencji mówiącej, że rzeczownik może być modyfikowany przez uzgadniający się z nim przymiotnik, a częściowo w procedurach tłumaczących ramy walencyjne na leksykon gramatyki kategorialnej. 

