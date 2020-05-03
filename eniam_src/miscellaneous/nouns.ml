open Xstd
open PreTypes

let wynik = ref []

let get_paths = function
    DepSentence paths -> Array.to_list paths
  | _ -> failwith "get_paths"

let if_cat cats = function
    Lemma(_,cat,_) -> List.exists (fun x -> x = cat) cats
  | _ -> false

let if_interps interps token =
  let interp = match token with
      Lemma(_,_,i) -> i
    | _ -> [[[]]] in
  let if_interp nr value =
    List.exists (fun x ->
      try
        List.exists (fun y ->
          y = value) (List.nth x nr)
      with _ -> false
      ) interp in
  Xlist.fold interps true (fun acc (nr,value) -> acc && (if_interp nr value))

let lemma_string = function
    Lemma(l,c,_) -> l ^ " " ^ c
  | _ -> failwith "lemma_string"

let get_noun_part paths tokens i id super label =
  List.iteri (fun i2 (id2,super2,label2) ->
    if super2 = i &&
       if_cat ["subst"; "ppron3"; "depr"; "ger"] (ExtArray.get tokens id2).token &&
       if_interps [1,"gen"] (ExtArray.get tokens id2).token
    then wynik := ((lemma_string (ExtArray.get tokens id).token) ^ " " ^
        (ExtArray.get tokens id2).orth ^ " " ^
        (lemma_string (ExtArray.get tokens id2).token) ^ "\n")
        :: !wynik
    ) paths

let parse_for_nouns paths tokens =
  List.iteri (fun i (id,super,label) ->
    if if_cat ["subst"; "ppron3"; "depr"; "ger"] (ExtArray.get tokens id).token
    then (get_noun_part paths tokens i id super label)) paths

let process_conll_corpus_for_nouns filename =
  let oc = open_out "../miscellaneous/nouns_skladnica.txt" in
  let corpus = File.file_in filename (fun file -> CONLL.load_corpus file) in
  Xlist.iter corpus (fun (p_record, tokens) -> parse_for_nouns (get_paths p_record.psentence) tokens);
  Xlist.iter (List.sort compare !wynik) (output_string oc); flush oc
