open Xstd
open PreTypes

let wynik = ref []

let get_paths = function
    DepSentence paths -> Array.to_list paths
  | _ -> failwith "get_paths"

let lemma_string = function
    Lemma(l,c,i) -> l ^ " " ^ c ^ " " ^ (if i = [[]]
                 then "_"
                 else String.concat "][" @@ Xlist.map i (fun x ->
                        String.concat "|" @@ Xlist.map x ( fun y ->
                          String.concat "." y)))
  | Interp t -> t
  | _ -> failwith "lemma_string"

let get_connection_part paths tokens i id super label =
  if super > 0
  then
    let id2,super2,label2 = List.nth paths super in
    wynik := ((lemma_string (ExtArray.get tokens id2).token) ^ " " ^
        (lemma_string (ExtArray.get tokens id).token) ^ " " ^ label ^ "\n")
        :: !wynik

let parse_for_connections paths tokens =
  List.iteri (fun i (id,super,label) -> get_connection_part paths tokens i id super label) paths

let process_conll_corpus_for_connections filename =
  let oc = open_out "../miscellaneous/connections_skladnica.txt" in
  let corpus = File.file_in filename (fun file -> CONLL.load_corpus file) in
  Xlist.iter corpus (fun (p_record, tokens) -> parse_for_connections (get_paths p_record.psentence) tokens);
  Xlist.iter (List.sort compare !wynik) (output_string oc); flush oc
