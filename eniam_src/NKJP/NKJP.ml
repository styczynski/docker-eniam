(*TYPE DEFINITIONS*)
type segm = {
  id_seg: string; (* -; [segm_]1.1-seg; [morph_]1.1-seg; [senses_]1.1-seg *)
  pos: int;
  length: int option;
  orth: string option;
  disamb: string option;
  sense: string option}

type sentence = {
  id_s: string; (* -; [segm_]1.57-s; [morph_]1.57-s; [senses_]1.57-s *)
  segments: segm list}

type abs = {
  id_ab: string; (* [txt_]1.1-ab; -; -; - *)
  contents: string option;
  sentences: sentence list}

type text = {
  id_source: string;
  id_p: string; (* [txt_]1[-div]; [segm_]1[-p]; [morph_]1[-p]; [senses_]1[-p] *)
  abs: abs list}

type 'a fold = string -> source:string list -> channel:string list -> 'a -> ('a -> text -> 'a) -> 'a

(*STRING CONVERSION*)
let string_of_some_int = function
  | None -> ""
  | Some x -> string_of_int x

let string_of_some = function
  | None -> ""
  | Some x -> x

let string_of_segm seg =
  seg.id_seg ^ "; " ^
  (string_of_int seg.pos) ^ "; " ^
  (string_of_some_int seg.length) ^ "; " ^
  string_of_some seg.orth ^ "; " ^
  string_of_some seg.disamb ^ "; " ^
  string_of_some seg.sense ^ "; "

let string_of_sentence s =
  List.fold_left (fun x y -> x ^ "\t \t \t" ^ (string_of_segm y) ^ "\n") (s.id_s ^ ";\n") s.segments

let string_of_abs a =
  List.fold_left (fun x y -> x ^ "\t \t" ^ (string_of_sentence y) ^ "\n") (a.id_ab ^ ";\n" ^ (string_of_some a.contents) ^ ";\n") a.sentences

let string_of_text t =
  List.fold_left (fun x y -> x ^ "\t" ^ (string_of_abs y) ^ "\n") (t.id_source ^ ";\n" ^ t.id_p ^ ";\n") t.abs

(*COMPARISONS*)
let compare_dotted a b =
  let fst id = int_of_string (NKJPxmlbasics.before_split id "\\.") in
  let snd id = int_of_string (NKJPxmlbasics.after_split (NKJPxmlbasics.before_split id "-") "\\.") in
  if compare (fst a) (fst b) <> 0 then compare (fst a) (fst b)
  else compare (snd a) (snd b)

let compare_string a b =
  if a = b then 0 else
  if a < b then -1 else 1

let compare_segm id_ab1 id_ab2 segm1 segm2 =
  if compare_dotted id_ab1 id_ab2 <> 0 then compare_dotted id_ab1 id_ab2
  else compare segm1.pos segm2.pos

let compare_sentence id_ab1 id_ab2 sentence1 sentence2 = compare_segm id_ab1 id_ab2 (List.hd sentence1.segments) (List.hd sentence2.segments)

let compare_flat_abs abs1 abs2 =
  match abs1.sentences, abs2.sentences with
  | {segments = hd1 :: []; _}::[], {segments = hd2 :: []; _}::[] ->
    compare_segm abs1.id_ab abs2.id_ab hd1 hd2
  | _, _ -> failwith "compare_flat_abs"

let compare_abs abs1 abs2 =
  let num x = int_of_string (NKJPxmlbasics.after_split (NKJPxmlbasics.before_split x "-") "\\.") in
  let num1 = num abs1.id_ab in
  let num2 = num abs2.id_ab in
  if num1 = num2 then 0
  else if num1 < num2 then -1 else 1

let compare_text text1 text2 =
  if text1.id_source = text2.id_source then compare (int_of_string text1.id_p) (int_of_string text2.id_p)
  else if text1.id_source < text2.id_source then -1 else 1

(*given node of xml file it compares its possible ids from xml:id and corresp and returns
  the id when values mathes else prints to stdout where values are incorrect*)
let compare_id_corresp file source =
  let id = NKJPxmlbasics.find_attribute source "xml:id" in
  let corresp = NKJPxmlbasics.find_attribute source "corresp" in
  let id_trun = NKJPxmlbasics.before_split (NKJPxmlbasics.after_split id "_") "-" in
  let corresp_trun = NKJPxmlbasics.before_split (NKJPxmlbasics.after_split corresp "_") "-" in
  if id_trun = corresp_trun then Some id_trun
  else begin
    Printf.printf "compare_id_corresp, invalid input at %s %s %s %s %s \n" file id corresp id_trun corresp_trun;
    None
  end

(*MERGES*)
let merge_list compare_el merge_el list1 list2 =
  let sort_list1 = List.sort compare_el list1 in
  let sort_list2 = List.sort compare_el list2
  in
    let rec submerge acc = function
      | _, [] -> acc
      | [], temp -> temp@acc
      | hd1::tl1, hd2::tl2 ->
        let c = compare_el hd1 hd2 in
        if c = 0 then submerge ((merge_el hd1 hd2)::acc) (hd1::tl1, tl2)
        else if c < 0 then submerge acc (tl1, hd2::tl2)
        else submerge (hd2::acc) (hd1::tl1, tl2)
    in
      List.sort compare_el (submerge [] (sort_list1, sort_list2))

let implication a = function
  | None -> a
  | Some x ->
    match a with
    | Some y -> Some x
    | None -> Some x

let merge_segm segm1 segm2 =
  assert(segm1.id_seg = segm2.id_seg);
  {
    id_seg = segm1.id_seg;
    pos = segm1.pos;
    length = implication segm1.length segm2.length;
    orth = implication segm1.orth segm2.orth;
    disamb = implication segm1.disamb segm2.disamb;
    sense = implication segm1.sense segm2.sense;
  }

let merge_sentence id_ab1 id_ab2 sentence1 sentence2 =
  assert(sentence1.id_s = sentence2.id_s);
  {
    id_s = sentence1.id_s;
    segments = merge_list (compare_segm id_ab1 id_ab2) merge_segm sentence1.segments sentence2.segments
  }

let merge_abs abs1 abs2 =
  assert(abs1.id_ab = abs2.id_ab);
  {
    id_ab = abs1.id_ab;
    contents = implication abs1.contents abs2.contents;
    sentences = merge_list (compare_sentence abs1.id_ab abs2.id_ab) (merge_sentence abs1.id_ab abs2.id_ab) abs1.sentences abs2.sentences
  }

let merge_text ~flat text1 text2 =
  assert (text1.id_source = text2.id_source);
  assert (text1.id_p = text2.id_p);
  let abs_comparison = if flat then compare_flat_abs else compare_abs in
  let abs = merge_list abs_comparison merge_abs text1.abs text2.abs in
  {
    id_source = text1.id_source;
    id_p = text1.id_p;
    abs
  }

let merge_flat_text text1 text2 =
  assert (text1.id_source = text2.id_source);
  assert (text1.id_p = text2.id_p);
  let abs = merge_list compare_flat_abs merge_abs text1.abs text2.abs in
  {
    id_source = text1.id_source;
    id_p = text1.id_p;
    abs
  }

let merge_file = merge_list compare_text (merge_text ~flat:false)

let merge_flat_file = merge_list compare_text (merge_text ~flat:true)

let unflatten s =
  match List.rev (List.sort compare_flat_abs s) with
  | [] -> []
  | hd::tl ->
    let exporter x y =
      match x with
      | [] -> assert false
      | hd::tl ->
        if hd.id_ab = y.id_ab then
          let sentences = match hd.sentences, y.sentences with
            | [], _
            | _, [] -> assert false
            | hd1::tl1, hd2::[] ->
              if hd1.id_s = hd2.id_s
              then {id_s = hd1.id_s; segments = hd2.segments @ hd1.segments}::tl1
              else hd2::hd1::tl1
            | _, _ -> assert false
          in
          {id_ab = hd.id_ab; contents = implication hd.contents y.contents; sentences} :: tl
        else y::hd::tl
    in
    List.fold_left exporter [hd] tl

let unflatten_file file =
  let exporter = function {id_source; id_p; abs = flat_abs} -> {id_source; id_p; abs = unflatten flat_abs}
  in
  List.map exporter file

let flatten = function
  | [] -> assert false
  | l ->
    let exporter x y =
      (List.map (fun z -> {id_ab = x.id_ab; contents=None; sentences=[z]}) x.sentences) @ y
    in
    List.fold_right exporter l []

(*SYSTEM UTILITIES*)
module StringMap = Map.Make(String);;

let create_map l =
  List.fold_left (fun x1 x2 -> (StringMap.add x2 true x1)) StringMap.empty l

let validate path name ~source_validate ~channel_validate =
  try
    let header = Xml.parse_file (path ^ name ^ "/header.xml") in
    let info_list = NKJPxmlbasics.go_to header ["teiHeader";"profileDesc";"textClass";"catRef"] in
    let source_predicate x = (NKJPxmlbasics.find_attribute x "scheme") = "#taxonomy-NKJP-type" in
    let channel_predicate x = (NKJPxmlbasics.find_attribute x "scheme") = "#taxonomy-NKJP-channel" in
    let source_info = List.hd (List.filter source_predicate info_list) in
    let channel_info = List.hd (List.filter channel_predicate info_list) in
    let source_res = NKJPxmlbasics.after_split (NKJPxmlbasics.find_attribute source_info "target") "_" in
    let channel_res = NKJPxmlbasics.after_split (NKJPxmlbasics.find_attribute channel_info "target") "_" in
    source_validate source_res && channel_validate channel_res
  with
  | Xml.Error err -> begin
    print_endline name;
    print_endline (Xml.error err);
    false
  end

let get_dirnames path ~source ~channel =
  let source_map = create_map source in
  let channel_map = create_map channel in
  let source_check x =
    if source = [] then true
    else StringMap.mem x source_map
  in
  let channel_check x =
    if channel = [] then true
    else StringMap.mem x channel_map
  in
  List.fold_left
      (fun folder_list folder ->
        if
          Sys.is_directory (path ^ folder) &&
          validate path folder ~source_validate:source_check ~channel_validate:channel_check
        then folder :: folder_list
        else folder_list)
      []
      (Array.to_list (Sys.readdir path))

(*FOLDS*)
let fold_folder_left path s f ending ~source ~channel=
  let folder_list = get_dirnames path ~source ~channel in
  List.fold_left
    (fun x y ->
      f x (y, ( Xml.parse_file (path ^ y ^ ending)))
    ) s folder_list

let fold_folder_right path f s ending ~source ~channel =
  let folder_list = get_dirnames path ~source:source ~channel:channel in
  List.fold_right
    (fun x y ->
      f (x, ( Xml.parse_file (path ^ x ^ ending))) y
    ) folder_list s

let fold_export path ~source ~channel s f export_function ending =
  let exporter =
    fun x1 y1 ->
      List.fold_left f x1 (export_function path y1)
  in
    fold_folder_left path s exporter ending ~source ~channel

(*UNIVERSAL EXPORTS*)
let notRejected seg = List.for_all (fun x -> x <> ("nkjp:rejected", "true")) (Xml.attribs seg)

let rec filterRejected = function
  | Xml.Element (tag_name, _, _) as seg when tag_name="seg" -> if notRejected seg then [seg] else []
  | Xml.Element (tag_name, _, children) when tag_name="s" || tag_name="choice" || tag_name="nkjp:paren" ->
    List.fold_left (fun x y -> x @ (filterRejected y)) [] children
  | _ -> failwith "filterRejected: given node is not seg s choice nor nkjp:paren"

let export_type_text name source export_type_abslist =
  match source with
  | Xml.Element (tag_name, _, children) when tag_name="p" ->
    (match compare_id_corresp name source with
    | None -> None
    | Some id_p ->
      let t = List.sort compare_abs (export_type_abslist children) in
      Some { id_source = name; id_p ; abs = t})
  | _ -> failwith "export_segm_p: given node is not p"

let export_ab export_seg = function
  | Xml.Element (tag_name, _, children) as source when tag_name="s" ->
    (let id_s = compare_id_corresp "not_aviable" source in
    match id_s with
    | None -> failwith "export_ab: missing id"
    | Some id ->
      let exporter x =
        let id_ab = Xml.attrib x "ab" in
        let sentences = [{ id_s = id; segments = [export_seg x]}] in
        {id_ab; contents = None; sentences}
      in
      List.map exporter (filterRejected source))
  | _ -> failwith "export_ab: given node is not s"

let export_seg_fs export_fs = function
  | Xml.Element (tag_name, _, children) as source when tag_name="seg" ->
    (match compare_id_corresp "not_aviable" source with
    | Some id_seg ->
      let id_seg = id_seg ^ "-seg" in
      let pos = int_of_string (Xml.attrib source "pos") in
      (match children with
      | [] ->
        { id_seg; pos; length = None;
        orth = None; disamb = None; sense = None}
      | hd::[] -> export_fs pos id_seg hd
      | _ -> failwith "too much fs")
    | None -> failwith "export_seg")
  | _ -> failwith "given node is not seg"

(*TEXT*)
let export_text_ab source =
  match source with
  | Xml.Element(tag_name, _, children) when tag_name="ab" -> {
    id_ab = NKJPxmlbasics.after_split (NKJPxmlbasics.find_attribute source "xml:id") "_";
    contents = NKJPxmlbasics.find_text source;
    sentences = []}
  | _ -> failwith "export_text_ab: given node is not ab"

let export_text_div name parsed_text =
  match parsed_text with
  | Xml.Element (tag_name, _, children) when tag_name="div" ->
    let id_div = NKJPxmlbasics.before_split (NKJPxmlbasics.after_split (NKJPxmlbasics.find_attribute parsed_text "xml:id") "_") "-" in
    let ab_list = List.map export_text_ab children
    in {
      id_source = name;
      id_p = id_div;
      abs = ab_list}
  | _ -> failwith "export_text_div: given node is not div"

let export_text_file path (name, parsed_text) =
  let div_list = NKJPxmlbasics.go_to parsed_text ["teiCorpus";"TEI";"text";"body";"div"]
  in
    List.fold_right (fun x y -> (export_text_div name x)::y) div_list []

let fold_text path ~source ~channel s f =
  (fold_export path ~source ~channel s f export_text_file "/text.xml")

(*SEGM*)
let rec extract_ab_id = function
  | Xml.Element (tag_name, _, children) as source when tag_name="seg" ->
    let corresp = NKJPxmlbasics.find_attribute source "corresp" in
    NKJPxmlbasics.after_split( NKJPxmlbasics.before_split (NKJPxmlbasics.after_split corresp "(") ",") "_"
  | _ ->  failwith "extract_ab_id"

let export_segm_seg ~id_s = function
  | Xml.Element (tag_name, _, children) as source when tag_name="seg" ->
    let corresp = NKJPxmlbasics.find_attribute source "corresp" in
    let id_seg = NKJPxmlbasics.after_split (NKJPxmlbasics.find_attribute source "xml:id") "_" in
    let pos = NKJPxmlbasics.before_split (NKJPxmlbasics.after_split corresp "ab,") "," in
    let len = NKJPxmlbasics.before_split (NKJPxmlbasics.after_split corresp ",") ")" in
    let id_ab = extract_ab_id source in
    let segment = { id_seg; pos = int_of_string pos; length = Some (int_of_string len);
      orth = None; disamb = None; sense = None; }
    in
      [{
        id_ab;
        contents = None;
        sentences = [{ id_s; segments = [ segment ] }]
      }]
  | _ -> failwith "export_seg: given node is not seg"

let export_segm_s_ab = function
  | Xml.Element (tag_name, _, children) as source when tag_name="s" ->
    let temp_id_s = NKJPxmlbasics.find_attribute source "xml:id" in
    let id_s = NKJPxmlbasics.before_split (NKJPxmlbasics.after_split temp_id_s "_") "-" in
    List.flatten (List.map (export_segm_seg ~id_s) (filterRejected source))
  | _ -> failwith "export_s: given node is not s"

let export_segm_slist ~flat sourcelist =
  (if flat then flatten else unflatten) (List.flatten (List.map export_segm_s_ab sourcelist))

let export_segm_file ~flat path (name, parsed_text) =
    let p_list = NKJPxmlbasics.go_to parsed_text ["teiCorpus";"TEI";"text";"body";"p"] in
    (*exports every p to some text and if it's not none value adds it to a list*)
    let exporter x y =
      match export_type_text name x (export_segm_slist ~flat) with
      | None -> y
      | Some v -> v::y
    in
    let segm = List.fold_right exporter p_list [] in
    let parsed_text = Xml.parse_file (path^name^"/text.xml") in
    let text = export_text_file path (name, parsed_text)
    in
      List.sort compare_text (merge_file text segm)

let fold_segm path ~source ~channel s f =
  (fold_export path ~source ~channel s f (export_segm_file ~flat:false) "/ann_segmentation.xml")

(*MORPH*)
let grandchildren = function
  | Xml.Element (_, _, children) ->
    let temp x y = match x with
      | Xml.Element (_, _, children) -> children @ y
      | _ -> y
    in
    List.fold_right temp children []
  | _ -> failwith "grandchildren"

let export_morph_f ({id_seg; pos; length; orth; disamb; sense} as acc) = function
  | Xml.Element (tag_name, _, children) as source when tag_name="f" ->
    (match NKJPxmlbasics.find_attribute source "name" with
    | "orth" ->
      if (List.tl children = []) then
      { id_seg; pos; length; orth = implication (NKJPxmlbasics.find_text (List.hd children)) orth; disamb; sense }
      else assert false
    | "disamb" ->
      (match List.filter (fun x -> NKJPxmlbasics.find_attribute x "name" = "interpretation") (grandchildren source) with
      | [] -> assert false
      | hd::tl ->
        if tl = [] then
        { id_seg; pos; length; orth; disamb = implication (NKJPxmlbasics.find_text hd) disamb; sense }
        else assert false)
    | _ -> acc )
  | _ -> failwith "export_morph_f"

let export_morph_fs pos id_seg = function
  | Xml.Element (tag_name, _, children) when tag_name="fs" ->
    let empty = { id_seg; pos; length = None; orth = None; disamb = None; sense = None } in
    List.fold_left export_morph_f empty children
  | _ -> failwith "export_morph_fs"

let export_morph_seg = export_seg_fs export_morph_fs

let export_morph_ab = export_ab export_morph_seg

let export_morph_p id_source = function
  | Xml.Element (tag_name, _, children) as source when tag_name="p" ->
    let abs = List.fold_right (fun x y -> export_morph_ab x @ y) children [] in
    (match compare_id_corresp "not_aviable" source with
    | Some id_p -> { id_source; id_p; abs }
    | None -> failwith "export_morph_p")
  | _ -> failwith "export_morph_p"

let export_morph_file ~flat path (name, parsed_text) =
  let parsed_segm = Xml.parse_file (path^name^"/ann_segmentation.xml") in
  let segm = export_segm_file ~flat:true path (name, parsed_segm) in
  let p_list = NKJPxmlbasics.go_to parsed_text ["teiCorpus";"TEI";"text";"body";"p"] in
  let morph = List.map (fun x -> (export_morph_p name x)) p_list in
  if flat then merge_flat_file segm morph else unflatten_file (merge_flat_file segm morph)

let fold_morph path ~source ~channel s f =
  (fold_export path ~source ~channel s f (export_morph_file ~flat:false) "/ann_morphosyntax.xml")

(*SENSE*)
let export_sense_seg = function
  | Xml.Element (tag_name, _, _) as source when tag_name = "seg" ->
    (match compare_id_corresp "not_aviable" source with
    | Some id_seg ->
      (let id_seg = id_seg ^ "-seg" in
      let sense =
        (match grandchildren source with
        | [] -> None
        | hd::[] -> Some (NKJPxmlbasics.after_split (NKJPxmlbasics.find_attribute hd "fVal") "#")
        | _ -> failwith "export_sense_seg: too much fs") in
      let pos = int_of_string (Xml.attrib source "pos") in
      {id_seg; pos; length = None; orth = None; disamb = None; sense})
    | None -> failwith "export_sense_seg")
  | _ -> failwith "export_sense_seg"

let export_sense_ab = export_ab export_sense_seg

let export_sense_p id_source = function
  | Xml.Element (tag_name, _, children) as source when tag_name="p" ->
    let abs = List.fold_right (fun x y -> export_sense_ab x @ y) children []in
    (match compare_id_corresp "not_aviable" source with
    | Some id_p -> { id_source; id_p; abs }
    | None -> failwith "export_sense_p")
  | _ -> failwith "export_sense_p"

let export_sense_file path (name, parsed_text) =
  let parsed_morph = Xml.parse_file (path^name^"/ann_morphosyntax.xml") in
  let morph = export_morph_file ~flat:true path (name, parsed_morph) in
  let p_list = NKJPxmlbasics.go_to parsed_text ["teiCorpus";"TEI";"text";"body";"p"] in
  let sense = List.map (fun x -> (export_sense_p name x)) p_list in
  unflatten_file (merge_flat_file morph sense)

let fold_sense path ~source ~channel s f =
  (fold_export path ~source ~channel s f export_sense_file "/ann_senses.xml")
