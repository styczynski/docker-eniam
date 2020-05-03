(* Generowanie pliku prepositions_skladnica.txt na podstawie krzaków *)
let _ =
  Prepositions.process_conll_corpus_for_prepositions "../../NLP resources/skladnica_zaleznosciowa.conll"

(* Generowanie pliku nouns_skladnica.txt na podstawie krzaków *)
let _ =
  Nouns.process_conll_corpus_for_nouns "../../NLP resources/skladnica_zaleznosciowa.conll"

(* Generowanie pliku connections_skladnica.txt na podstawie krzaków *)
let _ =
  Connections.process_conll_corpus_for_connections "../../NLP resources/skladnica_zaleznosciowa.conll"
