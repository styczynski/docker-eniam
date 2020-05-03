(*
 *  ENIAMlexSemantics is a library that assigns tokens with lexicosemantic information.
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

open ENIAMtokenizerTypes
open ENIAMsubsyntaxTypes
open ENIAMlexSemanticsTypes
open ENIAMwalTypes
open Xstd

(*let snode_values = ENIAM_LCGlexiconTypes.SelectorMap.find ENIAMcategoriesPL.selector_values ENIAM_LCGlexiconTypes.SNode*)

let find_sense m =
  try
    ENIAMplWordnet.find_sense m.plwnluid
  with Not_found ->
    m.name ^ "-" ^ m.variant, [], unknown_sense_weight

let find_prep_sense lemma hipero =
  let hipero = match hipero with
      [Predef hipero] -> hipero
    | _ -> failwith "find_prep_sense" in
  if hipero = "ALL" then lemma, [hipero,0], unknown_sense_weight else
  let syn_id = StringMap.find !ENIAMplWordnet.predef hipero in
  let hipero = IntMap.fold (ENIAMplWordnet.get_hipero syn_id) [] (fun hipero syn_id cost -> (ENIAMplWordnet.synset_name syn_id, cost) :: hipero) in
  lemma, hipero, unknown_sense_weight

let lex_sie = LCG (ENIAMwalRenderer.render_morf (SimpleLexArg("się",QUB)))

let rec has_lemma_sie = function
    [] -> false
  | p :: l -> if p.mode = ["lemma"] && p.morfs = [lex_sie] then true else has_lemma_sie l

(* FIXME: naiwnie wierzymy, że jeśli leksem jest opisany semantycznie w walentym to zawiera ramy dla wszystkich sensów *)
let find_senses t s =
  (*let set = Xlist.fold s.frames StringSet.empty (fun set frame ->
    Xlist.fold frame.senses set (fun set (name,hipero,weight) ->
      StringSet.add set name)) in*)
  let senses = match t.token with
      Lemma(lemma,pos,_) -> ENIAMplWordnet.find_senses lemma pos
(*     | Proper(_,_,_,senses) -> ENIAMplWordnet.find_proper_senses senses *)
    | _ -> [] in
  (* let senses =
    Xlist.fold senses [] (fun senses (name,hipero,weight) ->
      if StringSet.mem set name then senses else (name,hipero,weight) :: senses) in *)
  let senses_sie = match t.token with
      Lemma(lemma,pos,_) -> ENIAMplWordnet.find_senses (lemma ^ " się") pos
(*     | Proper(_,_,_,senses) -> [] *)
    | _ -> [] in
(*  let senses_sie = Xlist.fold senses_sie [] (fun senses_sie (name,hipero,weight) ->
    if StringSet.mem set name then senses_sie else (name,hipero,weight) :: senses_sie) in
  let frames = if senses = [] then s.frames else {empty_frame with senses=senses} :: s.frames in
  let frames = if senses_sie = [] then frames else {empty_frame with senses=senses_sie;
    positions=[{empty_position with role="Lemma"; mode=["lemma"]; morfs=[lex_sie]; is_necessary=Req}]} :: frames in*) (* FIXME: czy to nie usuwa elementów z ramy? *)
  let frames = Xlist.rev_map s.frames (fun f ->
    if f.senses <> [] then f else
    if has_lemma_sie f.positions then
      if senses_sie = [] then {f with senses=[ENIAMtokens.get_lemma t.token ^ " się", [], unknown_sense_weight]} else {f with senses=senses_sie}
    else
      if senses = [] then {f with senses=[ENIAMtokens.get_lemma t.token, [], unknown_sense_weight]} else {f with senses=senses}) in
  {s with frames=frames}

let find_selprefs schema = (* FIXME: RelationRole *)
  Xlist.map schema (fun p ->
      let l = Xlist.fold p.sel_prefs [] (fun l -> function
          SynsetId id -> (try ENIAMplWordnet.synset_name id :: l with ENIAMplWordnet.SynsetNotFound -> l)
        | Predef s -> s :: l
        | SynsetName _ -> failwith "find_selprefs"
        | RelationRole _ -> l) in
      let l = if l = [] then ["ALL"] else l in
      {p with sel_prefs=Xlist.map l (fun s -> SynsetName s)})

let rec find a l i =
  if a.(i) = max_int then (
    a.(i) <- i;
    i) else
  if a.(i) = i then (
    Xlist.iter l (fun j -> a.(j) <- i);
    i) else
  find a (i :: l) a.(i)

let union a i j =
  if i = j then i else
  let x = min i j in
  let y = max i j in
  a.(y) <- x;
  x

let rec split_tokens_into_groups_sentence a = function
    RawSentence s -> ()
  | StructSentence([],_) -> ()
  | StructSentence((id,_,_) :: paths,_) ->
      ignore (Xlist.fold paths (find a [] id) (fun m (id,_,_) ->
        union a m (find a [] id)))
  | DepSentence paths_list  ->
      Xlist.iter paths_list (fun paths ->
        if Array.length paths = 0 then () else
        let id,_,_ = paths.(0) in
        (* Printf.printf "id=%s i=%d\n%!" id 0; *)
        ignore (Int.fold 1 (Array.length paths - 1) (find a [] id) (fun m i ->
          let id,_,_ = paths.(i) in
          (* Printf.printf "id=%s i=%d\n%!" id i; *)
          union a m (find a [] id))))
  | QuotedSentences sentences ->
      Xlist.iter sentences (fun p ->
        split_tokens_into_groups_sentence a p.sentence)
  | AltSentence l -> Xlist.iter l (fun (mode,sentence) ->
        split_tokens_into_groups_sentence a sentence)
  | ErrorSentence s -> ()

let rec split_tokens_into_groups_paragraph a = function
    RawParagraph s -> ()
  | StructParagraph sentences ->
      Xlist.iter sentences (fun p -> split_tokens_into_groups_sentence a p.sentence)
  | AltParagraph l -> Xlist.iter l (fun (mode,paragraph) ->
      split_tokens_into_groups_paragraph a paragraph)
  | ErrorParagraph s -> ()

let rec split_tokens_into_groups_text a = function
    RawText s -> ()
  | StructText paragraphs ->
      Xlist.iter paragraphs (split_tokens_into_groups_paragraph a)
  | AltText l -> Xlist.iter l (fun (mode,text) ->
      split_tokens_into_groups_text a text)
  | ErrorText s -> ()

let split_tokens_into_groups size text =
  let a = Array.make size max_int in
  split_tokens_into_groups_text a text;
  Int.iter 0 (Array.length a - 1) (fun i ->
    if a.(i) <> max_int then a.(i) <- a.(a.(i)));
  let map = Int.fold 0 (Array.length a - 1) IntMap.empty (fun map i ->
    if a.(i) = max_int then map else
    IntMap.add_inc map a.(i) [i] (fun l -> i :: l)) in
  IntMap.fold map [] (fun l _ v -> v :: l)

let get_preps tokens group = (* FIXME: To nie zadziała przy kilku wystąpieniach tego samego przyimka *)
  let preps,compars = Xlist.fold group (StringMap.empty,StringSet.empty) (fun (preps,compars) id ->
    let t = ExtArray.get tokens id in
    match t.token with
      Lemma(lemma,"prep",interp) ->
        let preps = if lemma = "po" then StringMap.add_inc preps "po" (StringSet.singleton "postp") (fun cases -> StringSet.add cases "postp") else preps in (* FIXME: to należałoby dodawać w morfology *)
        let preps = if lemma = "per" then StringMap.add_inc preps "per" (StringSet.singleton "voc") (fun cases -> StringSet.add cases "voc") else preps in (* FIXME: to należałoby dodawać w morfology *)
        if StringSet.mem ENIAMvalence.compars lemma then preps,StringSet.add compars lemma else
        Xlist.fold interp preps (fun map -> function
                [cases] -> Xlist.fold cases map (fun map case -> StringMap.add_inc map lemma (StringSet.singleton case) (fun cases -> StringSet.add cases case))
              | [cases;_] -> Xlist.fold cases map (fun map case -> StringMap.add_inc map lemma (StringSet.singleton case) (fun cases -> StringSet.add cases case))
              | _ -> map),compars
      | _ -> preps,compars) in
  StringMap.fold preps [] (fun l prep v -> (prep, StringSet.to_list v) :: l), StringSet.to_list compars

let make_unique schemata =
  let map = Xlist.fold schemata StringMap.empty (fun map (selectors,schema) ->
    let s = "[" ^ ENIAMcategoriesPL.string_of_selectors selectors ^ "] {" ^ ENIAMwalStringOf.schema schema ^ "}" in
    StringMap.add map s (selectors,schema)) in
  StringMap.fold map [] (fun l _ (selectors,schema) -> (selectors,schema) :: l)

let semantize lemma pos (selectors,schema) =
  let schema = Xlist.rev_map schema (fun p ->
    {p with role="Arg"; sel_prefs=[Predef "X"]}) in (* FIXME: zaślepka, żeby preferować znane argumenty *)
  Xlist.rev_map (ENIAMvalence.get_aroles schema lemma pos) (fun (sel,arole,arole_attr,arev) ->
    {empty_frame with selectors=sel @ selectors; positions=schema;
     arole=arole; arole_attr=arole_attr; arev=arev})

let load_num_sem filename (num_sem,num_sem_args) =
  File.fold_tab filename (num_sem,num_sem_args) (fun (num_sem,num_sem_args) -> function
      [lemma;_;nsems;sense;sem_args] ->
        let sem_args = Xstring.split "," sem_args in
        Xlist.fold (Xstring.split "," nsems) num_sem (fun num_sem nsem ->
          StringMap.add_inc num_sem lemma [nsem,sense] (fun l -> (nsem,sense) ::l)),
        StringMap.add_inc num_sem_args lemma sem_args  (fun _ -> failwith "load_num_sem")
    | _ -> failwith "load_num_sem")

let num_sem = ref (StringMap.empty : (string * string) list StringMap.t)
let num_sem_args = ref (StringMap.empty : string list StringMap.t)

let add_sem_args lemma pos frame =
  {frame with sem_args =
      match pos with
        "subst" | "depr" -> (try StringMap.find ENIAMlexSemanticsData.noun_sem_args lemma with Not_found -> [])
      | "adj" | "adjc" | "adjp" -> (try StringMap.find ENIAMlexSemanticsData.adj_sem_args lemma with Not_found -> [])
      | "adv" -> (try StringMap.find ENIAMlexSemanticsData.adv_sem_args lemma with Not_found -> [])
      | "qub" ->  (try StringMap.find ENIAMlexSemanticsData.qub_sem_args lemma with Not_found -> [])
      | "ppron12" | "ppron3" | "siebie" -> (try StringMap.find ENIAMlexSemanticsData.pron_sem_args lemma with Not_found -> [])
      | "num" -> (try StringMap.find !num_sem_args lemma with Not_found -> [])
      | "ordnum" -> ["order"]
      | _ -> []}

let mark_reversed_hipero lemma pos frame =
  let set = try StringMap.find ENIAMlexSemanticsData.reversed_hipero pos with Not_found -> StringSet.empty in
  {frame with rev_hipero=StringSet.mem set lemma}

let mark_nosem frame =
  {frame with positions = List.rev (Xlist.rev_map frame.positions (fun p ->
    if p.mode=["lemma"] || p.role="Lemma" then
      if p.gf <> ARG then failwith "mark_nosem" else
      {p with gf=NOSEM}
    else p))}

let assign_prep_semantics lemma =
  if StringSet.mem ENIAMcategoriesPL.compar_lexemes lemma then
    [{empty_frame with
      senses = [find_prep_sense lemma [Predef "ALL"]];
      positions= [{empty_position with
        dir=Forward_; gf=CORE;
        morfs=ENIAMwalRenderer.compar_morfs; is_necessary=Req}];
      agf="arg"};
    {empty_frame with
      senses = [find_prep_sense lemma [Predef "ALL"]];
      positions= [{empty_position with
        sel_prefs=[SynsetName "ALL"]; dir=Forward_; gf=CORE;
        morfs=ENIAMwalRenderer.compar_morfs; is_necessary=Req}];
      arole="Arg"; arole_attr=""; arev=false; agf="adjunct"}]
  else
  let roles = try StringMap.find ENIAMlexSemanticsData.prep_roles lemma with Not_found -> [] in
  (* Printf.printf "assign_prep_semantics: |roles|=%d\n%!" (Xlist.size roles); *)
  {empty_frame with
    senses = [find_prep_sense lemma [Predef "ALL"]];
    positions= [{empty_position with
      dir=if lemma="temu" then Backward_ else Forward_; gf=CORE;
      morfs=ENIAMwalRenderer.prep_morfs; is_necessary=Req}];
    agf="arg"} ::
  (if roles = [] then (* FIXME: zaślepka do usunięcia po stworzeniu listy przyimków *)
  [{empty_frame with
    senses = [find_prep_sense lemma [Predef "ALL"]];
    positions= [{empty_position with
      sel_prefs=[SynsetName "ALL"]; dir=if lemma="temu" then Backward_ else Forward_; gf=CORE;
      morfs=ENIAMwalRenderer.prep_morfs; is_necessary=Req}];
    arole="Arg"; arole_attr=""; arev=false; agf="adjunct"}]
  else
  Xlist.map roles (function (case,arole,arole_attr,hipero,sel_prefs) ->
    (* Printf.printf "assign_prep_semantics: case=%s arole=%s arole_attr=%s\n%!" case arole arole_attr; *)
    let sense = find_prep_sense lemma hipero in (* FIXME: zaślepka dla sense i weight *)
    (* print_endline "assign_prep_semantics 1"; *)
    let positions = [{empty_position with
      sel_prefs=sel_prefs; dir=if lemma="temu" then Backward_ else Forward_; gf=CORE;
      morfs=ENIAMwalRenderer.prep_morfs(*ENIAMwalRenderer.assing_prep_morfs (lemma,case)*); is_necessary=Req}] in
    (* print_endline "assign_prep_semantics 2"; *)
    {empty_frame with selectors=[ENIAM_LCGlexiconTypes.Case,ENIAM_LCGlexiconTypes.Eq,[case]]; senses=[sense]; positions=find_selprefs positions;
     arole=arole; arole_attr=arole_attr; arev=false; agf="adjunct"}))

let assign_num_semantics lemma =
  let sems = try StringMap.find !num_sem lemma with Not_found -> [] in
  Xlist.map sems (fun (nsem,sense) ->
    let sense,arole_attr =
      if sense = "" then (lemma, [], unknown_sense_weight),"Approximate"
      else (sense, [], unknown_sense_weight),"Exact" in
    let arole = match nsem with
          "count" -> "Count"
        | "mass" -> "Measure"
        | _ -> failwith "assign_num_semantics" in
    {empty_frame with
      selectors=[ENIAM_LCGlexiconTypes.Nsem,ENIAM_LCGlexiconTypes.Eq,[nsem]];
      senses=[sense]; arole=arole; arole_attr=arole_attr; arev=false})

let assign_symb_num_semantics lemma pos =
  let arole_attr = match pos with
          "intnum" -> "Exact"
        | "realnum" -> "Exact"
        | "intnum-interval" -> "Approximate"
        | "realnum-interval" -> "Approximate"
        | _ -> failwith "assign_symb_num_semantics" in
  [{empty_frame with
      selectors=[ENIAM_LCGlexiconTypes.Nsem,ENIAM_LCGlexiconTypes.Eq,["count"]];
      senses=[lemma, [], unknown_sense_weight]; arole="Count"; arole_attr=arole_attr; arev=false}]

(*let set_context lemma pos frame =
  if pos = "fin" || pos = "praet" || pos = "winien" || pos = "inf" || pos = "pred" || pos = "impt" || pos = "imps" || pos = "ger" || pos = "pcon" || pos = "pant" then
    [{frame with has_context=true}] else
  if pos = "subst" then
    if frame.senses = [] then failwith "set_context" else
    let Xlist.fold frame.senses (fun -> ) in
  else [{frame with has_context=true}](*wydarzenie 1 czynność 1*)  (*czynności 1 czyn 1*)*)

let assign_valence tokens lex_sems group =
  let lexemes = Xlist.fold group StringSet.empty (fun lexemes id ->
      let lemma = ENIAMtokens.get_lemma (ExtArray.get tokens id).token in
      StringSet.add lexemes lemma) in
  let preps,compars = get_preps tokens group in
  let compreps = ENIAMwalReduce.select_comprep_adjuncts lexemes in
  let entries,schemata,connected = ENIAMwalReduce.select_entries lexemes in
  Xlist.iter group (fun id ->
      let lemma = ENIAMtokens.get_lemma (ExtArray.get tokens id).token in
      let pos = ENIAMtokens.get_pos (ExtArray.get tokens id).token in
      let pos2 = ENIAMtagset.simplify_pos pos in
      let schemata = Entries.find schemata pos2 lemma in
      let schemata = if schemata = [] then ENIAMvalence.get_default_valence pos2 else schemata in
      (* Printf.printf "A %s %s %s |schemata|=%d\n" lemma pos pos2 (Xlist.size schemata); *)
      let entries = Entries.find entries pos lemma in
      let connected = Entries.find connected pos2 lemma in
      let schemata1 = List.flatten (Xlist.map schemata (fun (opinion,neg,pred,aspect,schema) ->
          ENIAMvalence.transform_entry pos lemma neg pred aspect schema)) in (* gubię opinię *)
      (* Printf.printf "B %s |schemata|=%d\n" lemma (Xlist.size schemata); *)
      let schemata = ENIAMadjuncts.simplify_schemata lexemes pos pos2 lemma schemata1 in
      (* Printf.printf "C %s |schemata|=%d\n" lemma (Xlist.size schemata); *)
      let schemata = Xlist.rev_map schemata (fun (selectors,schema) ->
          selectors,["X",["X"]],(*snode_values,*)[],ENIAMwalRenderer.render_simple_schema schema,[]) in
      let schemata = List.flatten (Xlist.rev_map schemata (ENIAMadjuncts.add_adjuncts preps compreps compars pos2)) in
      let schemata = if schemata = [] then [[],["X",["X"]],(*snode_values,*)[],[],[]] else schemata in
      (* Printf.printf "D %s |schemata|=%d\n" lemma (Xlist.size schemata); *)
      let entries = List.flatten (Xlist.rev_map entries (ENIAMvalence.transform_lex_entry pos lemma)) in
      let entries = Xlist.map entries (fun (selectors,entry) ->
          selectors,ENIAMwalRenderer.render_lex_entry entry) in
      let connected = List.flatten (Xlist.map connected (fun (sopinion,fopinion,senses,neg,pred,aspect,schema1) ->
          List.flatten (Xlist.rev_map (ENIAMvalence.transform_entry pos lemma neg pred aspect schema1) (fun (selectors,schema) ->
              Xlist.rev_map (ENIAMvalence.get_aroles schema1 lemma pos) (fun (sel,arole,arole_attr,arev) ->
                  {empty_frame with selectors=sel @ selectors; senses=Xlist.map senses find_sense; positions=schema;
                   arole=arole; arole_attr=arole_attr; arev=arev; agf=""; rev_hipero=false; sem_args=[]; sopinion=sopinion; fopinion=fopinion}))))) in
      (* Printf.printf "E %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = if connected = [] then List.flatten (Xlist.rev_map (make_unique schemata1) (semantize lemma pos)) else connected in
      (* Printf.printf "F %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = Xlist.fold connected [] (fun connected frame ->
          if ENIAMadjuncts.check_selector_lex_constraints lexemes pos frame.selectors then frame :: connected else connected) in
      (* Printf.printf "G %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = Xlist.rev_map connected (fun frame ->
          {frame with
            positions = find_selprefs (ENIAMwalRenderer.render_connected_schema (ENIAMwalReduce.set_necessary pos frame.positions))}) in
      (* Printf.printf "H %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = List.flatten (Xlist.rev_map connected (ENIAMadjuncts.add_connected_adjuncts preps compreps compars pos2)) in
      (* Printf.printf "I %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = if pos = "prep" then
        if connected <> [] then failwith "assign_valence" else
        assign_prep_semantics lemma else connected in
      let connected = if pos = "num" then
        if connected <> [] then failwith "assign_valence" else
        assign_num_semantics lemma else connected in
      let connected = if pos = "intnum" || pos = "realnum" || pos = "intnum-interval" || pos = "realnum-interval" then
        if connected <> [] then failwith "assign_valence" else
        assign_symb_num_semantics lemma pos else connected in
      (* Printf.printf "J %s |connected|=%d\n" lemma (Xlist.size connected); *)
      let connected = if connected = [] then
        Xlist.rev_map (ENIAMvalence.get_aroles [] lemma pos) (fun (sel,arole,arole_attr,arev) ->
          {empty_frame with selectors=sel; arole=arole; arole_attr=arole_attr; arev=arev}) else connected in
      let connected = Xlist.rev_map connected (add_sem_args lemma pos) in
      let connected = Xlist.rev_map connected (mark_reversed_hipero lemma pos) in
      let connected = Xlist.rev_map connected mark_nosem in
      let connected = if connected = [] then semantize lemma pos ([],[]) else connected in
      let connected = Xlist.rev_map connected (fun f ->
        if f.senses = [] then {f with senses=[lemma, ["X",1], unknown_sense_weight]} else f) in
      (* let connected = List.flatten (Xlist.rev_map connected (set_context lemma pos)) in *)
      (* Printf.printf "K %s |connected|=%d\n" lemma (Xlist.size connected); *)
      ExtArray.set lex_sems id {(*(ExtArray.get lex_sems id) with*)
                                schemata=schemata; lex_entries=entries; frames=connected})

(* TODO:
   slashe
   zgranie z LCGlexicon
   usuwanie lex_entries gdy nie spełnione są selektory i gdy nie ma pasującego id wśród innych tokenów
   possp jako adjunct dla noun
   - uwzględnienie cech morfoskładniowych - np usunięcie schematów wymagających negacji, gdy nie ma "nie"
   - leksykalizacje bez schema
   - scalanie frames
*)

(*
let assign_valence tokens lex_sems group =
  let lexemes = Xlist.fold group StringMap.empty (fun lexemes id ->
    match (ExtArray.get tokens id).token with
      Lemma(lemma,pos,_) ->
        StringMap.add_inc lexemes lemma (StringSet.singleton pos) (fun set -> StringSet.add set pos)
    | Proper(lemma,pos,_,_) ->
        let pos = match pos with
          "subst" -> "psubst"
        | "depr" -> "pdepr"
        | _ -> pos (*failwith ("assign_valence: Proper " ^ pos ^ " " ^ lemma)*) in
        StringMap.add_inc lexemes lemma (StringSet.singleton pos) (fun set -> StringSet.add set pos) (* nazwy własne mają przypisywaną domyślną walencję rzeczowników *)
    | _ -> lexemes) in
  let valence = ENIAMwalenty.find_frames lexemes in
  Xlist.iter group (fun id ->
    match (ExtArray.get tokens id).token with
      Lemma(lemma,pos,_) ->
        ExtArray.set lex_sems id {(ExtArray.get lex_sems id) with
          valence=try Xlist.rev_map (StringMap.find (StringMap.find valence lemma) pos) (fun frame -> 0,frame) with Not_found -> []}
    | Proper(lemma,pos,interp,_) ->
        ExtArray.set lex_sems id {(ExtArray.get lex_sems id) with
          valence=(try Xlist.rev_map (StringMap.find (StringMap.find valence lemma)
            (if pos = "subst" || pos = "depr" then "p" ^ pos else pos)) (fun frame -> 0,frame) with Not_found -> [](*failwith ("assign_valence: Proper(" ^ lemma ^ "," ^ pos ^ ")")*))};
        ExtArray.set tokens id {(ExtArray.get tokens id) with token=Lemma(lemma,pos,interp)}
    | _ -> ())
*)


let disambiguate_senses lex_sems group =
  let prefs = Xlist.fold group (StringSet.singleton "ALL") (fun prefs id ->
    Xlist.fold (ExtArray.get lex_sems id).frames prefs (fun prefs frame ->
      Xlist.fold frame.positions prefs (fun prefs t ->
        Xlist.fold t.sel_prefs prefs (fun prefs -> function
          SynsetName s -> StringSet.add prefs s
        | t -> failwith ("disambiguate_senses: " ^ ENIAMwalStringOf.sel_prefs t))))) in
  (*let hipero = Xlist.fold group (StringSet.singleton "ALL") (fun hipero id ->
    Xlist.fold (ExtArray.get lex_sems id).senses hipero (fun hipero (_,l,_) ->
      Xlist.fold l hipero StringSet.add)) in
  let senses = StringSet.intersection prefs hipero in
  let is_zero = StringSet.mem hipero "0" in
  let senses = if is_zero then StringSet.add senses "0" else senses in*)
  Xlist.iter group (fun id ->
    let t = ExtArray.get lex_sems id in
    ExtArray.set lex_sems id {t with frames=Xlist.map t.frames (fun frame ->
      let senses = Xlist.map frame.senses (fun (name,hipero,weight) ->
        let hipero = Xlist.fold hipero ["ALL",0] (fun hipero (name,cost) ->
          if StringSet.mem prefs name then (name,cost) :: hipero else hipero) in
        name,hipero,weight) in
      {frame with senses=senses})})

let remove_unused_tokens tokens groups =
  let set = Xlist.fold groups IntSet.empty (fun set group ->
    Xlist.fold group set IntSet.add) in
  Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
    if IntSet.mem set i then () else
    ExtArray.set tokens i ENIAMtokenizerTypes.empty_token_env)

let assign tokens text =
  let lex_sems = ExtArray.make (ExtArray.size tokens) empty_lex_sem in
  let _ = ExtArray.add lex_sems empty_lex_sem in
  Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
    ignore (ExtArray.add lex_sems empty_lex_sem));
  let groups = split_tokens_into_groups (ExtArray.size tokens) text in
  (* Xlist.iter groups (fun group -> print_endline (String.concat " " (Xlist.map group string_of_int))); *)
  remove_unused_tokens tokens groups;
  Xlist.iter groups (fun group -> assign_valence tokens lex_sems group);
  Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
    let token = ExtArray.get tokens i in
    let lex_sem = ExtArray.get lex_sems i in
    let lex_sem = find_senses token lex_sem in
    ExtArray.set lex_sems i lex_sem);
  Xlist.iter groups (fun group -> disambiguate_senses lex_sems group);
  (*Xlist.iter groups (fun group -> ENIAMlexSemanticsData.assign_semantics tokens lex_sems group); *)
  lex_sems

let catch_assign tokens text =
  try
    assign tokens text,""
  with e ->
    ExtArray.make 0 empty_lex_sem,
    Printexc.to_string e

let initialize () =
  ENIAMsubsyntax.initialize ();
  ENIAMwalParser.initialize ();
  ENIAMwalReduce.initialize ();
  ENIAMplWordnet.initialize ();
  ENIAMcategoriesPL.initialize ();
  let a,b = File.catch_no_file (load_num_sem ENIAM_LCGlexiconTypes.num_nsems_filename) (StringMap.empty,StringMap.empty) in
  num_sem := a;
  num_sem_args := b;
  ()

open ENIAM_LCGtypes

let rec create_tokens_for_artificial_nodes_rec tokens lex_sems = function
    Node t ->
        let t = if t.id = 0 then (
          let id = ExtArray.add tokens empty_token_env in
          let lex_sem = {empty_lex_sem with frames=[{empty_frame with senses=[t.lemma, [t.lemma,0], unknown_sense_weight]}]} in
          let id2 = ExtArray.add lex_sems lex_sem in
          if id <> id2 then failwith "create_tokens_for_artificial_nodes_rec: tokens inconsistent with lex_sems" else
          let t = if t.symbol = Dot then
            {t with symbol = match t.pos with
                "<root>" -> Tuple[Val "<root>"]
              | "<merge>" -> Tuple[Val "<merge>"]
              | "<raw>" -> Tuple[Val "<raw>"]
              | "pro" -> Tuple[Val "pro"]
              | s -> failwith ("create_tokens_for_artificial_nodes_rec: " ^ s)} else t in
          {t with id=id}) else t in
        Node{t with args = create_tokens_for_artificial_nodes_rec tokens lex_sems t.args}
  | Tuple l ->
      Tuple(List.rev (Xlist.rev_map l (create_tokens_for_artificial_nodes_rec tokens lex_sems)))
  | Variant(e,l) ->
      Variant(e,List.rev (Xlist.rev_map l (fun (i,t) ->
        i, create_tokens_for_artificial_nodes_rec tokens lex_sems t)))
  | Dot -> Dot
  | Ref i -> Ref i
  | t -> failwith ("create_tokens_for_artificial_nodes_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let create_tokens_for_artificial_nodes tokens lex_sems dependency_tree =
  (* print_endline "create_tokens_for_artificial_nodes"; *)
  Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
    dependency_tree.(i) <- create_tokens_for_artificial_nodes_rec tokens lex_sems dependency_tree.(i))
