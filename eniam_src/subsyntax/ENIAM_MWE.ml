(*
 *  ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This library is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Xstd
open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes

type sel = V of string | S of string | G

type t =
    L of string * string * sel list
  | O of string
  | D of string
  | I of string
  | SL

let create_fixed_dict path filename dict =
  let valence = ENIAMtokenizer.extract_valence_lemmata path filename StringMap.empty in
  StringMap.fold valence dict (fun dict lemma map ->
(*     print_endline ("create_fixed_dict 1: " ^ lemma); *)
    if StringMap.mem map "fixed" then
      try
        let orths = List.flatten (List.rev (Xlist.rev_map (ENIAMtokenizer.parse_internal lemma) ENIAMtokens.get_orth_list)) in
(*       print_endline ("create_fixed_dict 2: " ^ String.concat " " orths); *)
	    let s = List.hd orths in
        let orths = Xlist.map orths (fun s -> O s) in
        StringMap.add_inc dict s [orths,lemma,"fixed",[]] (fun l -> (orths,lemma,"fixed",[]) :: l)
	  with Failure e -> failwith (e ^ ": " ^ lemma)
    else dict)
  
let process_interp lemma interp =
  match Xstring.split ":" interp with
    cat :: interp -> L(lemma,cat,Xlist.map interp (function
        "$c" -> S "c"
      | "$n" -> S "n"
      | "$g" -> S "g"
      | "$d" -> S "d"
      | "$C" -> S "C"
      | "_" -> G
      | s -> if String.get s 0 = '$' then failwith ("process_interp: " ^ s) else V s))
  | _ -> failwith "process_interp"

let load_mwe_dict filename dict =
  File.fold_tab filename dict (fun dict -> function
      [orths; lemma; interp] ->
        let orths = Xstring.split " " orths in
        if orths = [] then failwith "load_mwe_dict" else
        let s = List.hd orths in
        let orths = Xlist.map orths (fun s -> O s) in
        let lemma,cat,interp = match process_interp lemma interp with
            L(lemma,cat,interp) -> lemma,cat,interp
          | _ -> failwith "load_mwe_dict2" in
        StringMap.add_inc dict s [orths,lemma,cat,interp] (fun l -> (orths,lemma,cat,interp) :: l)
    | l -> failwith ("load_mwe_dict '" ^ String.concat "\t" l ^ "'"))

(*<<<<<<< HEAD
let load_mwe_dict () =
  let dict = load_dict StringMap.empty brev_filename in
  let dict = try load_dict dict fixed_filename with _ -> (prerr_endline ("ENIAMsubsyntax file " ^ fixed_filename ^ " not found"); dict) in
(*    let dict = load_dict dict complete_entries_filename in*)
  let dict = load_dict dict mwe_filename in
  dict

let mwe_dict = ref (StringMap.empty : (string * string * string) list StringMap.t)

let preselect_dict orths dict =
  StringSet.fold orths [] (fun rules orth ->
    try
      let l = StringMap.find dict orth in
      Xlist.fold l rules (fun rules (orth,lemma,interp) ->
               (* print_endline ("preselect_dict: " ^ orth); *)
               let match_list = Str.split (Str.regexp " ") orth in
               let b = Xlist.fold match_list true (fun b s ->
                   (* if not (StringSet.mem orths s) then print_endline s; *)
                   StringSet.mem orths s && b) in
               if b then (match_list,lemma,interp) :: rules else rules)
    with Not_found -> rules)*)

let process_orth = function
    [Lexer.T lemma; Lexer.B("(",")",[Lexer.T interp])] -> process_interp lemma interp
  | [Lexer.T orth] -> O orth
  | [Lexer.B("{","}",l); Lexer.B("(",")",[Lexer.T interp])] -> process_interp (Lexer.string_of_token_list l) interp
  | [Lexer.B("{","}",l)] -> O(Lexer.string_of_token_list l)
  | tokens -> failwith ("process_orth: " ^ Lexer.string_of_token_list tokens)

let load_mwe_dict2 filename (dict,dict2) =
  File.fold_tab filename (dict,dict2) (fun (dict,dict2) -> function
      [orths; lemma] ->
        (* print_endline (orths ^ "\t" ^ lemma); *)
        let tokens = Lexer.split "(\\|)\\|{\\|}\\| " orths in
        (* print_endline ("load_dict2 1: " ^ Lexer.string_of_token_list tokens); *)
        let tokens = Lexer.find_brackets ["{","}";"(",")"] [] tokens in
        (* print_endline ("load_dict2 2: " ^ Lexer.string_of_token_list tokens); *)
        let orths = List.rev (Xlist.rev_map (Lexer.split_symbol (Lexer.T " ") [] tokens) process_orth) in
        let tokens = Lexer.split "(\\|)\\|{\\|}" lemma in
        (* print_endline ("load_dict2 3: " ^ Lexer.string_of_token_list tokens); *)
        let tokens = Lexer.find_brackets ["{","}";"(",")"] [] tokens in
        (* print_endline ("load_dict2 4: " ^ Lexer.string_of_token_list tokens); *)
        let lemma,cat,interp = match process_orth tokens with
            L(lemma,cat,interp) -> lemma,cat,interp
          | _ -> failwith "load_mwe_dict2" in
        if orths = [] then failwith "load_mwe_dict2" else
        (match List.hd orths with
            L(s,_,_) -> dict, StringMap.add_inc dict2 s [orths,lemma,cat,interp] (fun l -> (orths,lemma,cat,interp) :: l)
          | O s -> StringMap.add_inc dict s [orths,lemma,cat,interp] (fun l -> (orths,lemma,cat,interp) :: l), dict2
          | _ -> failwith "load_mwe_dict2")
    | l -> failwith ("load_mwe_dict2 '" ^ String.concat "\t" l ^ "'"))

let add_known_orths_and_lemmata dict =
  let a = {number=""; gender=""; no_sgjp=false; poss_ndm=false; exact_case=false; ont_cat="MWEcomponent"} in
  let orths,lemmata = StringMap.fold dict (!known_orths,!known_lemmata) (fun (orth_set,lemma_map) _ l ->
    Xlist.fold l (orth_set,lemma_map) (fun (orth_set,lemma_map) (orths,lemma,cat,interp) ->
      Xlist.fold orths (orth_set,lemma_map) (fun (orth_set,lemma_map) -> function
          O s -> StringSet.add orth_set s, lemma_map
        | L(lemma,pos,_) -> orth_set, 
            let map2 = try StringMap.find lemma_map lemma with Not_found -> StringMap.empty in
            let map2 = StringMap.add_inc map2 (ENIAMtagset.simplify_pos pos) (OntSet.singleton a) (fun set -> OntSet.add set a) in
            StringMap.add lemma_map lemma map2
        | _ -> failwith "add_known_orths_and_lemmata"))) in
  known_orths := orths;
  known_lemmata := lemmata

    
let load_mwe_dicts () =
  let dict = File.catch_no_file (load_mwe_dict brev_filename) StringMap.empty in
  let dict = File.catch_no_file (load_mwe_dict fixed_filename) dict in
  let dict = File.catch_no_file (load_mwe_dict mwe_filename) dict in
  let dict =
    Xlist.fold !theories_paths dict (fun dict path ->
      File.catch_no_file (load_mwe_dict (path ^ "/mwe.tab")) dict) in
  let dict,dict2 = (*File.catch_no_file (load_mwe_dict2 sejf_filename)*) (dict,StringMap.empty) in
(*  let dict,dict2 = File.catch_no_file (load_mwe_dict2 sejfek_filename) (dict,dict2) in
  (* let dict,dict2 = File.catch_no_file (load_mwe_dict2 sawa_filename) (dict,dict2) in *)
  let dict,dict2 = File.catch_no_file (load_mwe_dict2 sawa_sort_filename) (dict,dict2) in
  let dict,dict2 = File.catch_no_file (load_mwe_dict2 sawa_ulice_filename) (dict,dict2) in
  let dict,dict2 = File.catch_no_file (load_mwe_dict2 sawa_dzielnice_filename) (dict,dict2) in*)
  let dict,dict2 = File.catch_no_file (load_mwe_dict2 mwe2_filename) (dict,dict2) in
  let dict,dict2 =
    Xlist.fold !ENIAMtokenizerTypes.theories_paths (dict,dict2) (fun (dict,dict2) path ->
      File.catch_no_file (load_mwe_dict2 (path ^ "/mwe2.tab")) (dict,dict2)) in
  let dict = File.catch_no_file (create_fixed_dict data_path "/valence.dic") dict in
  let dict =
    Xlist.fold !ENIAMtokenizerTypes.theories_paths dict (fun dict path ->
      File.catch_no_file (create_fixed_dict path "/valence.dic") dict) in
  add_known_orths_and_lemmata dict;
  add_known_orths_and_lemmata dict2;
  dict,dict2

let mwe_dict = ref (StringMap.empty : (t list * string * string * sel list) list StringMap.t)
let mwe_dict2 = ref (StringMap.empty : (t list * string * string * sel list) list StringMap.t)

let get_orths paths =
  IntMap.fold paths StringSet.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        Xlist.fold (ENIAMtokens.get_orths t.token) orths StringSet.add)))

let get_lemmas paths =
  IntMap.fold paths StringSet.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        StringSet.add orths (ENIAMtokens.get_lemma t.token))))

let get_intnum_orths paths =
  IntMap.fold paths StringMap.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        match t.token with
          Dig(lemma,"intnum") -> StringMap.add_inc orths (ENIAMtokens.get_orth t.token) (StringSet.singleton lemma) (fun set -> StringSet.add set lemma)
        | _ -> orths)))

let get_intnum_orths paths =
  IntMap.fold paths StringMap.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        match t.token with
          Dig(lemma,"intnum") -> StringMap.add_inc orths (ENIAMtokens.get_orth t.token) (StringSet.singleton lemma) (fun set -> StringSet.add set lemma)
        | _ -> orths)))

let get_year_orths paths =
  IntMap.fold paths StringSet.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        match t.token with
          Dig(lemma,"year") -> StringSet.add orths lemma
        | _ -> orths)))

let get_single_letter_orths paths =
  IntMap.fold paths StringSet.empty (fun orths _ map ->
    IntMap.fold map orths (fun orths _ l ->
      TokenEnvSet.fold l orths (fun orths t ->
        match t.token with
          SmallLetter(_,lemma) -> (*if lemma <> "g" then*) StringSet.add orths lemma (*else orths*) (* FIXME: !!!! *)
        | CapLetter(lemma,_) -> StringSet.add orths lemma
        | _ -> orths)))

let preselect orths lemmas rules l =
  Xlist.fold l rules (fun rules (match_list,lemma,cat,interp) ->
    (* print_endline ("preselect: " ^ lemma); *)
    let b = Xlist.fold match_list true (fun b -> function
        O s -> StringSet.mem orths s && b
      | L(s,_,_) -> StringSet.mem lemmas s && b
      | _ -> failwith "preselect") in
    if b then (Xlist.size match_list > 1,match_list,lemma,cat,interp) :: rules else rules)

let preselect_dict orths lemmas dict rules =
  StringSet.fold orths rules (fun rules orth ->
    try
      (* print_endline ("preselect_dict: " ^ orth); *)
      preselect orths lemmas rules (StringMap.find dict orth)
    with Not_found -> rules)

let preselect_dict2 orths lemmas dict2 rules =
  StringSet.fold lemmas rules (fun rules lemma ->
    try
      preselect orths lemmas rules (StringMap.find dict2 lemma)
    with Not_found -> rules)

(* let add_ordnum_rules orths rules =
  StringMap.fold orths rules (fun rules orth lemmas ->
    StringSet.fold lemmas rules (fun rules lemma ->
      (* Printf.printf "%s %s\n%!" orth lemma; *)
      (false,[D(orth,"intnum");O "."],lemma,"ordnum",[]) :: rules)) *)

let add_ordnum_rules rules =
  (*(false,[D "intnum" ;S "."],"<concat>","ordnum",[]) ::*)
  (*(false,[C "day-month" ;S "."],"<first>","ordnum",[]) ::*) rules (* FIXME: dokończyć implementację *)

let add_quot_rule rules =
  (false,[I "„x";I "<sentence>"; I "<clause>"],"„","Interp",[]) :: rules

(* let add_building_number_rules dig_orths letter_orths rules =
  StringSet.fold dig_orths rules (fun rules dig1 ->
    let rules = StringSet.fold letter_orths rules (fun rules letter1 ->
      (true,[D(dig1,"year");O letter1],dig1^letter1,"building-number",[]) :: rules) in
    StringSet.fold dig_orths rules (fun rules dig2 ->
      let rules = (true,[D(dig1,"year");O "/";D(dig2,"year")],dig1^"/"^dig2,"building-number",[]) :: rules in
      let rules = StringSet.fold letter_orths rules (fun rules letter1 ->
        (true,[D(dig1,"year");O letter1;O "/";D(dig2,"year")],dig1^letter1^"/"^dig2,"building-number",[]) ::
        (true,[D(dig1,"year");O "/";D(dig2,"year");O letter1],dig1^"/"^dig2^letter1,"building-number",[]) :: rules) in
      StringSet.fold dig_orths rules (fun rules dig3 ->
        let rules = (true,[D(dig1,"year");O "/";D(dig2,"year");O "/";D(dig3,"year")],dig1^"/"^dig2^"/"^dig3,"building-number",[]) :: rules in
        let rules = StringSet.fold letter_orths rules (fun rules letter1 ->
          (true,[D(dig1,"year");O letter1;O "/";D(dig2,"year");O "/";D(dig3,"year")],dig1^letter1^"/"^dig2^"/"^dig3,"building-number",[]) ::
          (true,[D(dig1,"year");O "/";D(dig2,"year");O letter1;O "/";D(dig3,"year")],dig1^"/"^dig2^letter1^"/"^dig3,"building-number",[]) :: rules) in
        rules))) *)

let add_building_number_rules rules =
  [true,[D "year";SL],"<concat>","building-number",[(*V "Proper"*)];
   true,[D "year";O "/";D "year"],"<concat>","building-number",[(*V "Proper"*)];
   true,[D "year";SL;O "/";D "year"],"<concat>","building-number",[(*V "Proper"*)];
   true,[D "year";O "/";D "year";SL],"<concat>","building-number",[(*V "Proper"*)];
   true,[D "year";O "/";D "year";O "/";D "year"],"<concat>","building-number",[(*V "Proper"*)];
   true,[D "year";SL;O "/";D "year";O "/";D "year"],"<concat>","building-number",[(*V "Proper"*)];
   true,[D "year";O "/";D "year";SL;O "/";D "year"],"<concat>","building-number",[(*V "Proper"*)];
  ] @ rules

let select_rules paths mwe_dict mwe_dict2 =
  let orths = get_orths paths in
  (* print_endline ("ENIAM_MWE.select_rules 1 orths=[" ^ String.concat ";" (StringSet.to_list orths) ^ "]"); *)
  let lemmas = get_lemmas paths in
  (* let intnum_orths = get_intnum_orths paths in *)
  (* let year_orths = get_year_orths paths in *)
  (* let letter_orths = get_single_letter_orths paths in *)
  let rules = preselect_dict orths lemmas mwe_dict [] in
  (* print_endline ("ENIAM_MWE.select_rules 1 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  (* Xlist.iter rules (fun (is_mwe,match_list,lemma,cat,interp) -> print_endline lemma); *)
  let rules = preselect_dict2 orths lemmas mwe_dict2 rules in
  (* print_endline ("ENIAM_MWE.select_rules 2 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  (* let rules = add_ordnum_rules intnum_orths rules in *)
  let rules = add_ordnum_rules rules in
  (* print_endline ("ENIAM_MWE.select_rules 3 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let rules = add_quot_rule rules in
  (* print_endline ("ENIAM_MWE.select_rules 4 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  (* let rules = add_building_number_rules year_orths letter_orths rules in *)
  let rules = add_building_number_rules rules in
  (* print_endline ("ENIAM_MWE.select_rules 5 |rules|=" ^ string_of_int (Xlist.size rules) ^ " |year_orths|=" ^ string_of_int (StringSet.size year_orths) ^ " |letter_orths|=" ^ string_of_int (StringSet.size letter_orths)); *)
  rules

let rec check_interp sels = function
    [],[] -> true
  | s :: interp, ["_"] :: interp2 -> check_interp sels (interp,interp2)
  | V s :: interp, l2 :: interp2 -> if Xlist.mem l2 s then check_interp sels (interp,interp2) else false
  | S s :: interp, l2 :: interp2 ->
      (try
        let l = Xlist.assoc sels s in
        let b = Xlist.fold l false (fun b s -> Xlist.mem l2 s || b) in
        if b then check_interp sels (interp,interp2) else false
      with Not_found -> check_interp sels (interp,interp2))
  | G :: interp, l2 :: interp2 -> check_interp sels (interp,interp2)
  | _ -> failwith "check_interp"

let rec get_sels sels = function
    [],[] -> sels
  | s :: interp, ["_"] :: interp2 -> get_sels sels (interp,interp2)
  | V s :: interp, l2 :: interp2 -> get_sels sels (interp,interp2)
  | S s :: interp, l2 :: interp2 ->
      (try
        let l = Xlist.assoc sels s in
        let sels = List.remove_assoc s sels in
        let l = Xlist.fold l [] (fun l s -> if Xlist.mem l2 s then s :: l else l) in
        get_sels ((s,l) :: sels) (interp,interp2)
      with Not_found -> get_sels ((s,l2) :: sels) (interp,interp2))
  | G :: interp, l2 :: interp2 -> get_sels sels (interp,interp2)
  | _ -> failwith "get_sels"

let rec match_path_rec map found (t:token_env) sels rev = function
    [] -> (t :: rev, sels) :: found
  | s :: l ->
     let map2 = try IntMap.find map t.next with Not_found -> IntMap.empty in
     let found2 = IntMap.fold map2 [] (fun found2 _ l ->
       TokenEnvSet.fold l found2 (fun found2 new_t ->
           match s,new_t.token with
             O s, token -> if Xlist.mem (ENIAMtokens.get_orths token) s then (new_t,sels) :: found2 else found2
           | L(s,cat,interp), Lemma(s2,cat2,interps2) ->
               Xlist.fold interps2 found2 (fun found2 interp2 ->
                 if s=s2 && cat=cat2 && check_interp sels (interp,interp2) then
                   (new_t,get_sels sels (interp,interp2)) :: found2 else found2)
           (* | D(s,cat), Dig(s2,cat2) -> if s=s2 && cat=cat2 then (new_t,sels) :: found2 else found2 *)
           | D cat, Dig(s2,cat2) -> if cat=cat2 then (new_t,sels) :: found2 else found2
           | I s, Interp s2 -> if s=s2 then (new_t,sels) :: found2 else found2
           | SL, SmallLetter _ -> (new_t,sels) :: found2
           | SL, CapLetter _ -> (new_t,sels) :: found2
           | _ -> found2)) in
     Xlist.fold found2 found (fun found (new_t,sels) -> match_path_rec map found new_t sels (t :: rev) l)

let match_path map = function
    [] -> failwith "match_path"
  | s :: l ->
     let found = IntMap.fold map [] (fun found i map2 ->
       IntMap.fold map2 found (fun found j l ->
         TokenEnvSet.fold l found (fun found t ->
           match s,t.token with
             O s, token -> if Xlist.mem (ENIAMtokens.get_orths token) s then (t,[]) :: found else found
           | L(s,cat,interp), Lemma(s2,cat2,interps2) ->
               Xlist.fold interps2 found (fun found interp2 ->
                 if s=s2 && cat=cat2 && check_interp [] (interp,interp2) then
                   (t,get_sels [] (interp,interp2)) :: found else found)
           (* | D(s,cat), Dig(s2,cat2) -> if s=s2 && cat=cat2 then (t,[]) :: found else found *)
           | D cat , Dig(s2,cat2) -> if cat=cat2 then (t,[]) :: found else found
           | I s, Interp s2 -> if s=s2 then (t,[]) :: found else found
           | SL, SmallLetter _ -> (t,[]) :: found
           | SL, CapLetter _ -> (t,[]) :: found
           | _ -> found))) in
     Xlist.fold found [] (fun found (t,sels) -> match_path_rec map found t sels [] l)

let concat_orths l =
  let s = String.concat "" (Xlist.map l (fun t -> t.orth ^ (if t.beg+t.len=t.next then "" else " "))) in
  let n = Xstring.size s in
  if String.get s (n-1) = ' ' then String.sub s 0 (n-1) else s

let create_token is_mwe (matching:token_env list) sels lemma pos interp = (* FIXME: problem z nazwami własnymi *)
(*   let interp,proper_flag = if interp = [V "Proper"] then [],true else interp,false in *)
  let tags = ENIAMtagset.validate lemma pos [Xlist.map interp (function
          S s -> (try Xlist.assoc sels s with Not_found -> ["_"])
        | V s -> Xstring.split "\\." s
        | G -> ["_"])] in
  let l = List.rev matching in
  let beg = (List.hd l).beg in
  let t = List.hd matching in
  let lemma = if lemma = "<concat>" then concat_orths l else lemma in
  let len = t.beg + t.len - beg in
  List.flatten (Xlist.map tags (fun tags ->
  Xlist.map (ENIAMtokenizer.get_ontological_category lemma pos tags) (fun (is_in_lexicon,has_no_sgjp_tag,has_poss_ndm_tag,has_exact_case_tag,cat,tags) ->
   {empty_token_env with
    orth=concat_orths l;
    beg=beg;
    len=len;
    next=t.next;
    token=
      if pos = "Interp" then Interp lemma else
(*       if proper_flag && !recognize_proper_names then Proper(lemma,pos,[[]],[pos]) else *)
      Lemma(lemma,pos,[tags]);
    cat=cat;
    weight=0.; (* FIXME: dodać wagi do konkretnych reguł i uwzględnić wagi maczowanych tokenów *)
    attrs=(if is_mwe then [MWE] else []) @ ENIAMtokens.merge_attrs l})))

let is_lemma t =
  match t.token with Lemma _ -> true | _ -> false
    
let add_token2 paths t =
 let map = try IntMap.find paths t.beg with Not_found -> IntMap.empty in
  let map = IntMap.add_inc map t.next (TokenEnvSet.singleton t) (fun set -> TokenEnvSet.add set t) in
  IntMap.add paths t.beg map

let add_token (paths,l) t =
  if is_lemma t && t.cat <> "MWEcomponent" then paths, t :: l else
  add_token2 paths t, l

let apply_rule paths (is_mwe,match_list,lemma,cat,interp) =
  (* print_endline ("apply_rule: " ^ lemma); *)
  let matchings_found = match_path paths match_list in
  Xlist.fold matchings_found paths (fun paths (matching,sels) ->
    try
      Xlist.fold (create_token is_mwe matching sels lemma cat interp) paths add_token2
    with Not_found -> paths)

let count_path_size paths =
  IntMap.fold paths 0 (fun n _ map2 ->
    IntMap.fold map2 n (fun n _ set ->
      TokenEnvSet.size set + n))

let process (paths,last) =
  (* print_endline ("ENIAM_MWE.process 1 |paths|=" ^ string_of_int (Xlist.size paths)); *)
  let paths,rest = Xlist.fold paths (IntMap.empty,[]) add_token in
  (* print_endline ("ENIAM_MWE.process 2 |paths|=" ^ string_of_int (count_path_size paths)); *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline ("ENIAM_MWE.process 3 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline ("ENIAM_MWE.process 4 |paths|=" ^ string_of_int (count_path_size paths)); *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline ("ENIAM_MWE.process 5 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline ("ENIAM_MWE.process 6 |paths|=" ^ string_of_int (count_path_size paths)); *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline ("ENIAM_MWE.process 7 |rules|=" ^ string_of_int (Xlist.size rules)); *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline "ENIAM_MWE.process 8"; *)
  let rules = select_rules paths !mwe_dict !mwe_dict2 in
  (* print_endline "ENIAM_MWE.process 9"; *)
  let paths = Xlist.fold rules paths apply_rule in
  (* print_endline "ENIAM_MWE.process 10"; *)
  let paths = IntMap.fold paths rest (fun paths _ map ->
    IntMap.fold map paths (fun paths _ l ->
      TokenEnvSet.fold l paths (fun paths t ->
        t :: paths))) in
  (* print_endline "ENIAM_MWE.process 11"; *)
  ENIAMpaths.sort (paths,last)
