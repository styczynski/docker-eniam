open Xstd
open PreTypes

let wynik = ref []

let get_paths = function
    DepSentence paths -> Array.to_list paths
  | _ -> failwith "get_paths"

let get_super paths super = match (List.nth paths super) with
    (_, s, _) -> s

let if_cat cats = function
    Lemma(_,cat,_) -> List.exists (fun x -> x = cat) cats
  | _ -> false

let rec isChildOf i paths (id_x,super_x,label_x) =
  (* print_endline (string_of_int i ^ " " ^ (string_of_int id_x) ^ " " ^ (string_of_int super_x)); *)
  id_x <> 0 &&
  (super_x = i || isChildOf i paths (List.nth paths super_x))

let rec get_ancestor paths tokens id =
  if id = 0
  then "0"
  else if if_cat ["conj"; "interp"] (ExtArray.get tokens id).token
    then get_ancestor paths tokens (get_super paths id)
    else (ExtArray.get tokens id).orth

let print_preposition_part paths tokens i id super label =
  wynik :=
    (String.uncapitalize_ascii (ExtArray.get tokens id).orth ^ " " ^
    (String.concat " " @@ Xlist.map (List.filter (isChildOf i paths) paths)
      (fun (id_x,super_x,label_x) -> (ExtArray.get tokens id_x).orth)), get_ancestor paths tokens super) :: !wynik


let parse_for_prepositions paths tokens =
  List.iteri (fun i (id,super,label) ->
    if if_cat ["prep"] (ExtArray.get tokens id).token
    then (print_preposition_part paths tokens i id super label)) paths

let process_conll_corpus_for_prepositions filename =
  let oc = open_out "../miscellaneous/prepositions_skladnica.txt" in
  let corpus = File.file_in filename (fun file -> CONLL.load_corpus file) in
  Xlist.iter corpus (fun (p_record, tokens) -> parse_for_prepositions (get_paths p_record.psentence) tokens);
  Xlist.iter (List.sort compare !wynik) (fun (a,b) -> output_string oc (b ^ " " ^ a ^ "\n");
    flush oc)
