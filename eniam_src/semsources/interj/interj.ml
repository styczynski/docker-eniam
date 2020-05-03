open Xstd

let interj_filename = "interj_raw.tab"

let interj = File.load_tab interj_filename (function
    [lemma; cat] -> lemma,cat
  | _ -> failwith "interj")

let translate_cat_type = function
  | "interj" -> []
  | "interj-NKJP1M" -> []
  | "interj-wiki" -> []
  | "interj-mwe" -> []
  | "interj-prepnp" -> []
  | "interj-emo" -> ["emo"]
  | "interj-apel" -> ["apel"]
  | "interj-onom" -> ["onom"]
  | "interj-emo2" -> ["emo"]
  | "interj-onom2" -> ["onom"]
  | "interj-wola2" -> ["apel"]
  | "interj-inne2" -> ["inne"]
  | "interj-emo3" -> ["emo"]
  | "interj-wola3" -> ["apel"]
  | "interj-onom3" -> ["onom"]
  | "interj-apel3" -> ["apel"]
  | "interj-wiki4" -> []
  | s -> failwith ("translate_cat: " ^ s)

let translate_cat_source = function
  | "interj" -> ["SGJP"]
  | "interj-NKJP1M" -> ["NKJP1M"]
  | "interj-wiki" -> ["wiki"]
  | "interj-mwe" -> ["wiki"]
  | "interj-prepnp" -> ["wiki"]
  | "interj-emo" -> ["wiki"]
  | "interj-apel" -> ["wiki"]
  | "interj-onom" -> ["wiki"]
  | "interj-emo2" -> ["wiki"]
  | "interj-onom2" -> ["wiki"]
  | "interj-wola2" -> ["wiki"]
  | "interj-inne2" -> ["wiki"]
  | "interj-emo3" -> ["wiki"]
  | "interj-wola3" -> ["wiki"]
  | "interj-onom3" -> ["wiki"]
  | "interj-apel3" -> ["wiki"]
  | "interj-wiki4" -> ["wiki"]
  | s -> failwith ("translate_cat: " ^ s)

let is_mwe lemma =
  String.contains lemma ' ' || String.contains lemma '-'

let make_interj_map interj =
  Xlist.fold interj StringMap.empty (fun map (lemma,cat) ->
    let cats = translate_cat_source cat in
    let cats = cats @ translate_cat_type cat in
    let cats = if is_mwe lemma then "mwe" :: cats else cats in
    StringMap.add_inc map lemma (StringSet.of_list cats) (fun set -> Xlist.fold cats set StringSet.add))

let make_rev_map interj =
  StringMap.fold interj StringMap.empty (fun map lemma cats ->
    let s = String.concat " " (Xlist.sort (StringSet.to_list cats) compare) in
    StringMap.add_inc map s [lemma] (fun l -> lemma :: l))

let print_string_map map =
  StringMap.iter map (fun k l ->
    Printf.printf "%s\t%s\n" k (String.concat " " l))

let make_tab interj =
  List.rev (StringMap.fold interj [] (fun tab lemma cats ->
    let sources,mwe,types = StringSet.fold cats ([],false,[]) (fun (sources,mwe,types) -> function
        "mwe" -> sources,true,types
      | "emo" -> sources,mwe,"emo" :: types
      | "apel" -> sources,mwe,"apel" :: types
      | "onom" -> sources,mwe,"onom" :: types
      | "inne" -> sources,mwe,"inne" :: types
      | "SGJP" -> "SGJP" :: sources,mwe,types
      | "NKJP1M" -> "NKJP1M" :: sources,mwe,types
      | "wiki" -> "wiki" :: sources,mwe,types
      | s -> failwith "make_tab") in
    let sources = String.concat " " (Xlist.sort sources compare) in
    let types = if types = [] then [""] else types in
    let mwe = if mwe then "mwe" else "" in
    Xlist.fold types tab (fun tab t ->
      (lemma,"interj",t,mwe,sources) :: tab)))

let print_tab filename tab =
  File.file_out filename (fun file ->
    Xlist.iter tab (fun (lemma,cat,t,mwe,sources) ->
      Printf.fprintf file "%s\t%s\t%s\t%s\t%s\n" lemma cat t mwe sources))

(*let _ =
  let map = make_interj_map interj in
  let map = make_rev_map map in
  print_string_map map;
  ()*)

let _ =
  let map = make_interj_map interj in
  let tab = make_tab map in
  print_tab "interj_processed.tab" tab;
  ()
