(*
 *  ENIAMwalenty, a converter for Polish Valence Dictionary "Walenty".
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
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

open ENIAMwalTypes
open Xstd

let controll_string_of_morf = function
    Phrase(NP _) as p -> ENIAMwalStringOf.morf p
  | Phrase(AdjP _) as p -> ENIAMwalStringOf.morf p
  | Phrase(PrepNP("do",Case _)) -> "prepnp"
  | Phrase(PrepNP("na",Case _)) -> "prepnp"
  | Phrase(PrepNP("za",Case _)) -> "prepnp"
  | Phrase(PrepNP("z",Case _)) -> "prepnp"
  | Phrase(PrepNP("o",Case _)) -> "prepnp"
  | Phrase(PrepNP("przy",Case _)) -> "prepnp"
  | Phrase(PrepNP("w",Case _)) -> "prepnp"
  | Phrase(PrepNP("jak",_)) as p -> ENIAMwalStringOf.morf p
  | Phrase(PrepNP("jako",_)) as p -> ENIAMwalStringOf.morf p
  | Phrase(PrepAdjP("do",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("na",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("za",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("z",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("o",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("przy",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("w",Case _)) -> "prepadjp"
  | Phrase(PrepAdjP("jak",_)) as p -> ENIAMwalStringOf.morf p
  | Phrase(PrepAdjP("jako",_)) as p -> ENIAMwalStringOf.morf p
  | Phrase(ComprepNP _) -> "prepnp"
  | Phrase(ComparP _) as p -> ENIAMwalStringOf.morf p
  | Phrase(InfP _) -> "infp"
  | Phrase(Or) -> "or"
  | Phrase(FixedP "za hetkę pętelkę") -> "prepnp"
  | Phrase(FixedP "za hetkę-pętelkę") -> "prepnp"
  | PhraseComp(Cp,_) -> "cp"
  | PhraseComp(Ncp case,_) -> ENIAMwalStringOf.morf (Phrase(NP case))
  | PhraseComp(Prepncp("o",Case _),_) -> "prepnp"
  | PhraseComp(Prepncp("na",Case _),_) -> "prepnp"
  | LexPhrase((SUBST(_,case),_) :: _,_) -> ENIAMwalStringOf.morf (Phrase(NP case))
  | LexPhrase((ADJ(_,case,_,_),_) :: _,_) -> ENIAMwalStringOf.morf (Phrase(AdjP case))
  | LexPhrase((INF _,_) :: _,_) -> "infp"
  | LexPhrase((COMPAR,Lexeme prep) :: _,_) -> ENIAMwalStringOf.morf (Phrase(ComparP prep))
  | LexPhrase((PREP(Case _),Lexeme "za") :: (SUBST _,_) :: _,_) -> "prepnp"
  | E Null -> "E"
  | m -> print_endline ("controll_string_of_morf: " ^ ENIAMwalStringOf.morf m); ENIAMwalStringOf.morf m

let controll_string_of_schema schema =
  String.concat "+" (Xlist.map schema (fun s ->
    let morfs = StringSet.to_list (Xlist.fold s.morfs StringSet.empty (fun set morf ->
      StringSet.add set (controll_string_of_morf morf))) in
    String.concat "," (
      (if s.gf = ARG then [] else [ENIAMwalStringOf.gf s.gf])@
      (ENIAMwalStringOf.controllers s.cr)@(ENIAMwalStringOf.controllees s.ce)) ^ "{" ^  String.concat ";" morfs ^ "}"))

let controll_weight p =
  match p.cr, p.ce with
    ["1"],[] -> 1
  | [],["1"] -> 2
  | ["2"],[] -> 3
  | [],["2"] -> 4
  | _ -> 5

let controll_compare p1 p2 =
  compare (controll_weight p1) (controll_weight p2)

let correct_walenty entry =
  if entry.form_orth = "podobać" then
    {entry with schemata=Xlist.map entry.schemata (fun s ->
         {s with positions=Xlist.map s.positions (fun p ->
              if p.gf=SUBJ then {p with morfs=List.flatten (Xlist.map p.morfs (function
                    MorfId 126 -> []
                  | m -> [m]))}
              else p)})}
  else entry

let walenty_filename,expands_filename =
  if Array.length Sys.argv < 3 then failwith "missing argument" else Sys.argv.(1), Sys.argv.(2)
  (* "/home/yacheu/Dokumenty/NLP resources/Walenty/walenty_20170311.xml",
    "/home/yacheu/Dokumenty/NLP resources/Walenty/phrase_types_expand_20170311.xml" *)

(*let _ =
  let walenty,phrases = ENIAMwalTEI.load_walenty walenty_filename in
  let walenty = Xlist.rev_map walenty correct_walenty in
  let expands = ENIAMwalTEI.load_expands expands_filename in
  let meanings =
    Xlist.fold walenty IntMap.empty (fun meanings entry ->
      Xlist.fold entry.meanings meanings (fun meanings meaning ->
        IntMap.add meanings meaning.mng_id meaning)) in
  let connected_walenty =
    Xlist.fold walenty Entries.empty (fun connected_walenty e ->
        let entries = ENIAMwalConnect.connect e in
        Entries.add_inc_list connected_walenty e.form_pos e.form_orth entries) in
  let schemata_walenty =
    Xlist.fold walenty Entries.empty (fun schemata_walenty e ->
        let entries = ENIAMwalConnect.schemata e in
        Entries.add_inc_list schemata_walenty e.form_pos e.form_orth entries) in
  let expands,compreps,subtypes,equivs,adv_types =
    ENIAMwalRealizations.load_realizations (expands,ENIAMwalTEI.subtypes,ENIAMwalTEI.equivs) in
  (* Test wczytywania Walentego TEI *)
  let n = Entries.fold connected_walenty 0 (fun n pos lemma entry -> n + 1) in
  let m = Entries.fold schemata_walenty 0 (fun n pos lemma entry -> n + 1) in
  Printf.printf "%d connected\n%d schemata\n|phrases|=%d\n" n m (IntMap.size phrases);
  let cmap = Entries.fold schemata_walenty StringMap.empty (fun cmap pos lemma (opinion,(n,p,a),schema) ->
    let schema = Xlist.fold schema [] (fun schema p ->
      if p.ce = [] && p.cr = [] then schema else p :: schema) in
    if schema = [] then cmap else
    let schema = Xlist.map schema (fun p ->
      {p with morfs=Xlist.map (Xlist.sort p.morfs compare) (function MorfId id -> IntMap.find phrases id)}) in
    let s = pos ^ "\t" ^ controll_string_of_schema (Xlist.sort schema controll_compare) in
    StringMap.add_inc cmap s [lemma] (fun l -> lemma :: l)) in
  File.file_out "results/controll.tab" (fun file ->
    StringMap.iter cmap (fun s l  ->
      Printf.fprintf file "%d\t%s\t%s\n" (Xlist.size l) s (String.concat " " l)));
  ()*)

(* Test unikalności indeksów sensów *)
(* let _ =
  let walenty,phrases = ENIAMwalTEI.load_walenty "/home/yacheu/Dokumenty/NLP resources/Walenty/walenty_20170311.xml" in
  Xlist.fold walenty IntMap.empty (fun map e ->
    Xlist.fold e.meanings map (fun map m ->
      IntMap.add_inc map m.mng_id m (fun m1 -> if m1 = m then m else failwith "meaning"))) *)

(*
(* let insert_phrases phrases = function
    Frame(atrs,s) -> Frame(atrs,Xlist.map s (fun p ->
      {p with morfs=Xlist.map p.morfs (function
             MorfId id -> (try IntMap.find phrases id with Not_found -> failwith "insert_phrases")
           | _ -> failwith "insert_phrases")}))
  | _ -> failwith "insert_phrases: ni"

let print_entry pos_map pos orth =
  let orth_map = try StringMap.find pos_map pos with Not_found -> StringMap.empty in
  let frames = try StringMap.find orth_map orth with Not_found -> [] in
  Xlist.iter frames (fun frame ->
      let frame = insert_phrases ENIAMwalTEI.phrases frame in
      print_endline (ENIAMwalStringOf.frame orth frame)) *)

(* Wypisanie hasła *)
(* let _ =
   print_entry connected_walenty "verb" "brudzić";
   () *)

(* let has_nontrivial_lex = function
    Frame(atrs,s) -> Xlist.fold s false (fun b p ->
      if p.role = "Lemma" && p.role_attr = "" then b else
        Xlist.fold p.morfs b (fun b -> function
              MorfId id -> failwith "has_nontrivial_lex"
            | LexPhrase _ -> true
            (* | LexRPhrase _ -> true
               | LexPhraseMode _ -> true *)
            | _ -> b))
  | _ -> failwith "has_nontrivial_lex: ni" *)

(* Leksykalizacje nie wchodzące do lematu *)
(* let _ =
   StringMap.iter connected_walenty (fun _ orth_map ->
      StringMap.iter orth_map (fun orth frames ->
          Xlist.iter frames (fun frame ->
              let frame = insert_phrases ENIAMwalTEI.phrases frame in
              if has_nontrivial_lex frame then
                print_endline (ENIAMwalStringOf.frame orth frame)))) *)

let simplify_frame_verb = function
    Phrase(NP(Case "dat")) -> []
  | Phrase(NP(Case "inst")) -> []
  | Phrase(PrepNP _) -> []
  | Phrase(ComprepNP _) -> []
  | Phrase(AdvP) -> []
  | t -> [t]

let simplify_frame_noun = function
    Phrase(NP(Case "gen")) -> []
  | Phrase(NP(Case "nom")) -> []
  | Phrase(NP(CaseAgr)) -> []
  | Phrase(PrepNP _) -> []
  | Phrase(ComprepNP _) -> []
  | Phrase(AdjP CaseAgr) -> []
  | PhraseComp(Ncp(Case "gen"),_)
  | PhraseComp(Prepncp(_,_),_) -> []
  | PhraseAbbr(Possp,[]) -> []
  | t -> [t]

let simplify_frame_adj = function
  | t -> [t]

let simplify_frame_adv = function
  | t -> [t]


(* let simplify_frame pos = function
    Frame(atrs,s) ->
    let schema = Xlist.fold s [] (fun schema p ->
        let morfs = Xlist.fold p.morfs [] (fun morfs morf ->
            match pos with
              "verb" -> simplify_frame_verb morf @ morfs
            | "noun" -> simplify_frame_noun morf @ morfs
            | "adj" -> simplify_frame_adj morf @ morfs
            | "adv" -> simplify_frame_adv morf @ morfs
            | _ -> failwith "simplify_frame") in
        if morfs = [] then schema else
          {p with ce=[]; cr=[]; morfs=morfs} :: schema) in
    if schema = [] then [] else [Frame(atrs,schema)]
  | _ -> failwith "simplify_frame: ni" *)


(* Uproszczone schematy *)
(* let _ =
   StringMap.iter schemata_walenty (fun pos orth_map ->
      if pos = "noun" then
      StringMap.iter orth_map (fun orth frames ->
          Xlist.iter frames (fun frame ->
              let frame = insert_phrases ENIAMwalTEI.phrases frame in
              let frames = simplify_frame pos frame in
              Xlist.iter frames (fun frame -> print_endline (ENIAMwalStringOf.frame orth frame))))) *)

(* let has_mode_coordination = function
    Frame(atrs,s) -> Xlist.fold s false (fun b p ->
      let n = Xlist.fold p.morfs 0 (fun n -> function
            MorfId id -> failwith "has_nontrivial_lex"
          | PhraseAbbr(Advp _,_) -> n+1
          | PhraseAbbr(Xp _,_) -> n+1
          (* | LexPhraseMode _ -> n+1 FIXME*)
          | _ -> n) in
      if n>1 then true else b)
  | _ -> failwith "has_nontrivial_lex: ni" *)

(* Koordynacja z mode *)
(* let _ =
   StringMap.iter schemata_walenty(*connected_walenty*) (fun _ orth_map ->
      StringMap.iter orth_map (fun orth frames ->
          Xlist.iter frames (fun frame ->
              let frame = insert_phrases ENIAMwalTEI.phrases frame in
              if has_mode_coordination frame then
                print_endline (ENIAMwalStringOf.frame orth frame)))) *)


(* let get_entry orth pos *)
     (*
let load_walenty2 () =
  let walenty = load_walenty walenty_filename in
  Xlist.fold walenty StringMap.empty (fun walenty entry ->
    if entry.frames = [] then Xlist.fold (connect2 entry) walenty (fun walenty (lemma,pos,frame) ->
      let map = try StringMap.find walenty pos with Not_found -> StringMap.empty in
      let map = StringMap.add_inc map lemma [frame] (fun l -> frame :: l) in
      StringMap.add walenty pos map)
    else Xlist.fold (connect entry) walenty (fun walenty (lemma,pos,frame) ->
      let map = try StringMap.find walenty pos with Not_found -> StringMap.empty in
      let map = StringMap.add_inc map lemma [frame] (fun l -> frame :: l) in
      StringMap.add walenty pos map))


let print_stringqmap filename qmap =
  let l = StringQMap.fold qmap [] (fun l k v -> (v,k) :: l) in
  File.file_out filename (fun file ->
    Xlist.iter (Xlist.sort l compare) (fun (v,k) ->
      Printf.fprintf file "%5d %s\n" v k))

let sel_prefs_quantities walenty =
  Xlist.fold walenty StringQMap.empty (fun quant e ->
    Xlist.fold e.frames quant (fun quant f ->
      Xlist.fold f.arguments quant (fun quant a ->
        Xlist.fold a.sel_prefs quant (fun quant l ->
          Xlist.fold l quant (fun quant -> function
              Numeric s ->
                let name = try ENIAMplWordnet.synset_name s with Not_found -> "unknown" in
                StringQMap.add quant ("N " ^ s ^ " " ^ name)
            | Symbol s -> StringQMap.add quant ("S " ^ s)
            | Relation(s,t) -> StringQMap.add quant ("R " ^ s ^ " | " ^ t))))))
*)
(*let _ =
  let walenty = load_walenty walenty_filename in
  let quant = sel_prefs_quantities walenty in
  print_stringqmap "results/quant_sel_prefs.txt" quant*)

(*let _ =
  let walenty = load_walenty2 () in
  let frames_sem = try StringMap.find (StringMap.find walenty "verb") "bębnić" with Not_found -> failwith "walTEI" in
  Xlist.iter frames_sem (fun frame ->
    print_endline (WalStringOf.frame "bębnić" frame))*)


(* Wypisanie realizacji *)
(* let _ =
   Xlist.iter ENIAMwalTEI.expands (fun (id,morf,l) ->
      Printf.printf "%d %s:\n" id (ENIAMwalStringOf.morf morf);
      Xlist.iter l (fun morf -> Printf.printf "    %s\n" (ENIAMwalStringOf.morf morf))) *)

(* Wypisanie realizacji po przetworzeniu *)
(* let _ =
   AbbrMap.iter expands (fun morf l ->
      Printf.printf "%s:\n" (ENIAMwalStringOf.phrase_abbr morf);
      Xlist.iter l (fun morf -> Printf.printf "    %s\n" (ENIAMwalStringOf.morf morf))) *)

let has_realization = function
    PhraseAbbr _ -> true
  | PhraseComp _ -> true
  | _ -> false

(* Wypisanie fraz, które podlegają rozwijaniu *)
(*let _ =
  IntMap.iter ENIAMwalTEI.phrases (fun i morf ->
      if has_realization morf then
      Printf.printf "%4d %s\n" i (ENIAMwalStringOf.morf morf)) *)

(* Wypisanie fraz, które podlegają rozwijaniu *)
(* let _ =
   IntMap.iter phrases (fun i morf ->
      if has_realization morf then
      Printf.printf "%4d %s\n" i (ENIAMwalStringOf.morf morf)) *)

(* let test_phrases = [17088; 17133; 1642]
   let _ =
   Xlist.iter test_phrases (fun i ->
      let m1 = IntMap.find ENIAMwalTEI.phrases i in
      let m2 = IntMap.find phrases i in
      Printf.printf "%4d %s\n" i (ENIAMwalStringOf.morf m1);
      Printf.printf "%4d %s\n" i (ENIAMwalStringOf.morf m2)) *)

(* let print_entries entries =
  StringMap.iter entries (fun pos entries2 ->
      StringMap.iter entries2 (fun lemma entries3 ->
          EntrySet.iter entries3 (fun entry ->
              Printf.printf "%s: %s: %s\n" pos lemma (ENIAMwalStringOf.entry entry)))) *)

(* let _ = print_entries entries *)
*)

let selected_phrases =
  File.fold_tab "results/phrases_cp.tab" IntSet.empty (fun set -> function
    [id;_] -> IntSet.add set (int_of_string id)
  | _ -> failwith "selected_phrases")

let print_phrases filename phrases =
  File.file_out filename (fun file ->
      IntMap.iter phrases (fun id morf ->
        Printf.fprintf file "%d\t%s\n" id (ENIAMwalStringOf.morf morf)))

let rec connected_schema schema =
  String.concat "+" (Xlist.map schema (fun s ->
      String.concat "," (
        (if s.gf = ARG then [] else [ENIAMwalStringOf.gf s.gf])@
        s.mode@(ENIAMwalStringOf.controllers s.cr)@(ENIAMwalStringOf.controllees s.ce)) ^
      "{" ^  String.concat ";" (Xlist.map s.morfs ENIAMwalStringOf.morf) ^ "}:" ^ ENIAMwalStringOf.sem_frame s))

let print_connected filename connected =
  File.file_out filename (fun file ->
      Entries.iter connected (fun pos lemma c(*sopinion,fopinion,meanings,(n,p,a),schema,examples*) ->
          Printf.fprintf file "\n\t%d\t%d\t%s: %s: %s: %s: %s: %s: %s: %s:\t%s\n"
            c.sch_id c.frm_id pos lemma
            (ENIAMwalStringOf.opinion c.sopinion)
            (ENIAMwalStringOf.opinion c.fopinion)
            (String.concat "," (Xlist.map c.meanings (fun m ->
              if m.name="" then string_of_int m.mng_id else m.name ^ "-" ^ m.variant)))
            (ENIAMwalStringOf.negation c.negativity)
            (ENIAMwalStringOf.pred c.predicativity)
            (ENIAMwalStringOf.aspect c.aspect)
            (connected_schema c.schema);
          Xlist.iter c.examples (fun (opinion,exm) ->
            Printf.fprintf file "#%s: %s\n" (ENIAMwalStringOf.opinion opinion) exm)))

let expand_morf phrases = function
  | MorfId id ->
      (try IntMap.find phrases id
      with Not_found -> Printf.printf "expand_morf: %d\n" id; MorfId id)
  | _ -> failwith "expand_morf"

let expand_sel_prefs meanings = function
    SynsetId id ->
      (try
        let m = IntMap.find meanings id in
        Predef (m.name ^ "-" ^ m.variant)
      with Not_found -> (*Printf.printf "expand_sel_prefs: %d\n" id;*) SynsetId id)
  | s -> s

let expand_schema phrases meanings_map c =
  let schema = Xlist.map c.schema (fun (s : position) ->
    {s with
      morfs = Xlist.map s.morfs (expand_morf phrases);
      sel_prefs = Xlist.map s.sel_prefs (expand_sel_prefs meanings_map)}) in
  (* let meanings = Xlist.map c.meanings (fun id -> try IntMap.find meanings_map id with Not_found -> {empty_meaning with name=string_of_int id}) in *)
  {c with (*meanings2=meanings;*) schema=schema}

let assign_examples examples c =
  let p_set = Xlist.fold c.schema IntSet.empty (fun p_set p ->
    Xlist.fold p.morfs p_set (fun p_set -> function
        MorfId id -> IntSet.add p_set id
      | _ -> p_set)) in
  let m_set = Xlist.fold c.meanings IntSet.empty (fun m_set m -> IntSet.add m_set m.mng_id) in
  let examples = Xlist.fold examples [] (fun examples (e : example) ->
    let b = Xlist.fold e.phrases false (fun b (sch_id,_,morf_id) ->
      if c.sch_id = sch_id && IntSet.mem p_set morf_id then true else b) in
    if IntSet.mem m_set e.meaning && b then e :: examples else examples) in
  let examples = Xlist.rev_map examples (fun e -> e.opinion,e.sentence) in
  {c with examples=examples}

let select_morfs morfs =
  List.rev (Xlist.fold morfs [] (fun morfs -> function
      MorfId id -> if IntSet.mem selected_phrases id then (MorfId id) :: morfs else morfs
    | _ -> failwith "select_morfs"))

let select_positions schema =
  List.rev (Xlist.fold schema [] (fun schema p ->
    let morfs = select_morfs p.morfs in
    if morfs = [] then schema else
    {p with morfs = morfs} :: schema))

let select_entries entries =
  Xlist.fold entries [] (fun entries c ->
    let schema = select_positions c.schema in
    if schema = [] then entries else c :: entries)

(* Wypisanie podrzędników zdaniowych *)
let _ =
  let walenty,phrases = ENIAMwalTEI.load_walenty walenty_filename in
  print_phrases "results/phrases.tab" phrases;
  let meanings =
    Xlist.fold walenty IntMap.empty (fun meanings entry ->
      Xlist.fold entry.meanings meanings (fun meanings meaning ->
        IntMap.add meanings meaning.mng_id meaning)) in
  let connected_walenty =
    Xlist.fold walenty Entries.empty (fun connected_walenty e ->
        (* print_endline "1"; *)
        let entries = ENIAMwalConnect.connect e in
        (* print_endline "2"; *)
        let entries = select_entries entries in
        (* print_endline "3"; *)
        let entries = Xlist.rev_map entries (assign_examples e.examples) in
        (* print_endline "4"; *)
        let entries = Xlist.rev_map entries (expand_schema phrases meanings) in
        (* print_endline "5"; *)
        Entries.add_inc_list connected_walenty e.form_pos e.form_orth entries) in
  print_connected "results/connected.tab" connected_walenty;
  ()
