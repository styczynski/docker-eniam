open SkladnicaTypes

let get_filenames path =
  let readdir path =
    if Xstring.check_sufix ".DS_Store" path
    then [||]
    else Sys.readdir path in
  List.filter (fun s -> not @@ Xstring.check_sufix ".DS_Store" s) @@
  List.fold_left
      (fun files_list folder1 ->
          List.fold_left
          (fun acc folder2 ->
              List.fold_left
              (fun fl_list file ->
                (path ^ "/" ^ folder1 ^ "/" ^ folder2 ^ "/" ^file) :: fl_list)
              acc
              (Array.to_list (readdir (path ^ "/" ^ folder1 ^ "/" ^ folder2))))
          files_list
          (Array.to_list (readdir (path ^ "/" ^ folder1))))
      []
      (Array.to_list (readdir path))

let is_below_head head checked =
  let get_from = function
    Node(from,_,_,_,_) -> from
  | Leaf(from,_,_,_,_,_) -> from
  | TreeNotFound -> failwith "get_from" in
  get_from checked < get_from head

let rec get_head_id = function
    Node(_,_,_,[head],_) -> get_head_id head
  | Leaf(_,sto,_,_,_,_) -> sto
  | _ -> failwith "get_head_id"

let rec tree_to_conll super = function
  Node(_,_,_,[head],children) ->
    let left, right = List.partition (is_below_head head) children in
    let head_id = get_head_id head in
    String.concat "\n"
      ((List.map (tree_to_conll head_id) left)
      @ (tree_to_conll super head
      :: (List.map (tree_to_conll head_id) right)))
| Leaf(from,sto,terminal_attrs,orth,base,f) ->
    let rec change_syntax = (function
     | Str.Text txt :: tail -> txt ^ (change_syntax tail)
     | Str.Delim "[" :: Str.Text txt :: Str.Delim "]" :: tail -> (String.concat "." (Xstring.split "|" txt)) ^ (change_syntax tail); 
     | [] -> ""
     | _ -> failwith ("tree_to_conll: " ^ f.text)) in
    let splitted = Str.full_split (Str.regexp "\\]\\|\\[") f.text in
    let t = change_syntax splitted in
    let str_fs = Xstring.split ":" t in
        String.concat "\t" [string_of_int sto;
         orth; base; List.hd str_fs; List.hd str_fs; String.concat "|" (List.tl str_fs);
         string_of_int super; "_"; "_"; "_"]
| TreeNotFound -> raise NoForest
| _ -> failwith "tree_to_conll"

let parse xml =
  print_endline "parseXmlToConll";
  let forest = SkladnicaXmlToOcaml.to_ocaml_forest xml in
  (* print_endline (SkladnicaTreeFinder.text_of_forest forest); *)
  let tree = SkladnicaTreeFinder.get_tree "" forest in
  tree_to_conll 0 tree

(* let _ =
  let path =
    try
      Sys.argv.(1)
    with
    | _ -> failwith ("Usage: " ^ Sys.argv.(0) ^ " corpus_name\n") in
  List.iteri (fun i filename ->
    (* print_string (string_of_int i ^ " "); flush stdout; *)
    (* let forest = SkladnicaXmlToOcaml.to_ocaml_forest (Xml.parse_file filename) in
    (* print_endline (SkladnicaTreeFinder.text_of_forest forest); *)
    let tree = SkladnicaTreeFinder.get_tree filename forest in
    let str_tree = tree_to_conll 0 tree in *)
    let str_tree = parse (Xml.parse_file filename) in
    if str_tree <> "empty_tree"
    then print_endline (str_tree ^ "\n");
  ) (get_filenames path)
  (* StringSet.iter print_endline !m *) *)
