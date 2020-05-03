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

open ENIAMwalTypes
open Xstd

let simplify_position_verb mode l = function (* FIXME: dodać czyszczenie E Pro *)
    NP(Case "dat") -> l
  | NP(Case "inst") -> l
  | NCP(Case "dat",_,_) -> l
  | NCP(Case "inst",_,_) -> l
  | CP _ -> l
  | PrepNP _ -> l
  | PrepAdjP _ -> l
  | ComprepNP _ -> l
  | ComparP _ -> l
  | PrepNCP _ -> l
  | AdvP _ -> l
  | Or -> l
  | SimpleLexArg("się",QUB) -> l
  | E Or -> l
  | E (CP(CompTypeUndef,CompUndef)) -> l
  | E (PrepNP(prep,Case case)) -> l
  | E (PrepNCP(prep,Case case,CompTypeUndef,CompUndef)) -> l
  | NP(Case "gen") as t -> if mode = "temp" then l else t :: l
  | NP(Case "acc") as t -> if mode = "dur" then l else t :: l
  | t -> t :: l

let simplify_position_noun mode l = function
    NP(Case "gen") -> l
  | NP(Case "nom") -> l
  | NCP(Case "gen",_,_) -> l
  | NP(CaseAgr) -> l
  | AdjP AllAgr -> l
  | PrepNP _ -> l
  | ComprepNP _ -> l
  | ComparP _ -> l
  | PrepNCP _ -> l
  | t -> t :: l

let simplify_position_adj mode l = function
    AdvP _ -> l
  | ComparP _ -> l
  | t -> t :: l

let simplify_position_adv mode l = function
    AdvP _ -> l (* FIXME: czy na pewno zostawić swobodę modyfikowania przysłówka? *)
  | t -> t :: l

(*
let simplify_position pos l s =
  let morfs = match pos with
      "verb" -> List.rev (Xlist.fold s.morfs [] simplify_position_verb)
    | "noun" -> List.rev (Xlist.fold s.morfs [] simplify_position_noun)
    | "adj" -> List.rev (Xlist.fold s.morfs [] simplify_position_adj)
    | "adv" -> List.rev (Xlist.fold s.morfs [] simplify_position_adv)
    | _ -> s.morfs in
  match morfs with
    [] -> l
  | [Phrase Null] -> l
  | _ -> {s with morfs=morfs} :: l

let simplify_schemata pos schemata =
  let schemata = Xlist.fold schemata StringMap.empty (fun schemata (schema,frame) ->
      let schema = List.sort compare (Xlist.fold schema [] (fun l s ->
          let s = {s with role=""; role_attr=""; sel_prefs=[]; cr=[]; ce=[]; morfs=List.sort compare s.morfs} in
          if s.gf <> ARG && s.gf <> ADJUNCT then s :: l else
            (*       if s.cr <> [] || s.ce <> [] then s :: l else  *)
            simplify_position pos l s)) in
      StringMap.add_inc schemata (ENIAMwalStringOf.schema schema) (schema,[frame]) (fun (_,frames) -> schema, frame :: frames)) in
  StringMap.fold schemata [] (fun l _ s -> s :: l)

let simplify_schemata2 pos schemata =
  let simplify_position_fun = match pos with
      "verb" -> simplify_position_verb2
    | "noun" -> simplify_position_noun
    | "adj" -> simplify_position_adj
    | "adv" -> simplify_position_adv
    | _ -> (fun l x -> x :: l) in
  let morfs = Xlist.fold schemata [] (fun morfs schema ->
      Xlist.fold schema morfs (fun morfs s ->
          Xlist.fold s.morfs morfs simplify_position_fun)) in
  let morfs = Xlist.fold morfs StringMap.empty (fun map s ->
      StringMap.add map (ENIAMwalStringOf.morf s) s) in
  let schema = StringMap.fold morfs [] (fun schema _ morf ->
      {gf=ARG; role=""; role_attr=""; sel_prefs=[]; cr=[]; ce=[];
       dir=Both; morfs=[Phrase Null;morf]} :: schema) in
  schema*)

(*let rec classify_phrase = function
    NP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | PrepNP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | AdjP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | PrepAdjP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | ComprepNP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | ComparP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | CP _ -> "cp"
  | NCP(case,_,_) -> ENIAMwalStringOf.phrase (NP case)
  | PrepNCP(prep,case,_,_) -> ENIAMwalStringOf.phrase (PrepNP(prep,case))
  | InfP _ -> "infp"
  | AdvP _ -> "advp"
  | FixedP _ as phrase -> ENIAMwalStringOf.phrase phrase
  | Or -> "or"
  | E Or -> "or"
  | E phrase -> classify_phrase phrase
  | SimpleLexArg("się",QUB) -> "się"
  (* | SimpleLexArg _ -> "lex" *)
(* | LexArg _ -> "lex" *)
  | SimpleLexArg _ as phrase -> ENIAMwalStringOf.phrase phrase
  | LexArg _ as phrase -> ENIAMwalStringOf.phrase phrase
  | phrase -> print_endline ("classify_phrase: " ^ ENIAMwalStringOf.phrase phrase); "other"

let classify_position pos p =
  let l = (*StringSet.to_list*) (Xlist.fold p.morfs StringSet.empty (fun set morf ->
      StringSet.add set ((*classify_phrase*)ENIAMwalStringOf.phrase morf))) in
  (* match l with
    [] -> "empty"
  | [c] -> c
  (* | ["np(gen)"; "np(acc)"] -> "np(str)"
  | ["np(gen)"; "infp"] -> "np(gen)-infp"
  | ["np(acc)"; "infp"] -> "np(acc)-infp" *)
     | _ -> let c = String.concat " " l in if pos="adv" then print_endline c; c *)
  l*)

module OrderedPhrase = struct
  type  t = phrase
  let compare = compare
end

module PhraseSet = Xset.Make(OrderedPhrase)


let remove_adjuncts_schema pos lemma schema =
  let simplify_position_fun = match pos with
      "verb" -> simplify_position_verb
    | "noun" -> simplify_position_noun
    | "adj" -> simplify_position_adj
    | "adv" -> simplify_position_adv
    | _ -> (fun _ l x -> x :: l) in
  List.flatten (Xlist.map schema (fun p ->
      let morfs = Xlist.fold p.morfs [] (simplify_position_fun (String.concat " " p.mode)) in
      if morfs = [] then [] else [PhraseSet.of_list morfs]))
  (* let schema2 = List.flatten (Xlist.map schema1 (fun p ->
      let p = {p with morfs = Xlist.fold p.morfs [] (simplify_position_fun (String.concat " " p.mode))} in
      let c = classify_position pos p in
      if StringSet.is_empty c (*"empty"*) then [] else [c,[p]])) in
  (* let sum = Xlist.fold schema2 StringSet.empty (fun set (c,p) -> StringSet.union set c) in
  let n = Xlist.fold schema2 0 (fun n (c,p) -> n + StringSet.size c) in
  if StringSet.size sum <> n (*&& pos = "noun"*) then (*Printf.printf "%s %s %s\n" pos lemma (ENIAMwalStringOf.schema schema1);*)
    Printf.printf "%s %s %s\n" pos lemma (String.concat "+" (Xlist.map schema2 (fun (c,p) -> String.concat "," p.mode ^ "{" ^ String.concat ";" (StringSet.to_list c) ^ "}"))); *)
(*  let set = Xlist.fold schema2 StringSet.empty (fun set (c,_) ->
      StringSet.add set c) in
  (* if StringSet.mem set "np(acc)" && StringSet.mem set "infp" then
      Printf.printf "%s %s %s\n" pos lemma (ENIAMwalStringOf.schema schema1); *)
  let schema2 = if StringSet.mem set "np(gen)" && StringSet.mem set "np(acc)" then schema2 else
      Xlist.map schema2 (function
            "np(gen)",p -> "np(str)",p
          | "np(acc)",p -> "np(str)",p
          | c,p -> c,p) in*)
     schema2 *)


let is_disjunctive schema =
  let sum = Xlist.fold schema PhraseSet.empty (fun set morfs -> PhraseSet.union set morfs) in
  let n = Xlist.fold schema 0 (fun n morfs -> n + PhraseSet.size morfs) in
  PhraseSet.size sum = n

let rec find_overlapping morfs rev = function
    morfs2 :: schema ->
    if PhraseSet.is_empty (PhraseSet.intersection morfs morfs2) then find_overlapping morfs (morfs2 :: rev) schema
    else morfs2, List.rev rev @ schema
  | [] -> raise Not_found

let rec merge_schemata_rec cont = function
    [] -> cont
  | [schema] -> schema :: cont
  | schema1 :: schema2 :: schemata ->
    let sum_schema,diff_schema = Xlist.fold schema1 ([],schema2) (fun (sum_schema,diff_schema) morfs ->
        try
          let morfs2,diff_schema = find_overlapping morfs [] diff_schema in
          (PhraseSet.union morfs morfs2) :: sum_schema,diff_schema
        with Not_found -> morfs :: sum_schema,diff_schema) in
    let schema = sum_schema @ diff_schema in
    if is_disjunctive schema then ((*print_endline "A";*) merge_schemata_rec cont (schema :: schemata))
    else ((*print_endline "B";*) merge_schemata_rec (schema :: cont) schemata)

let rec merge_schemata schemata =
  let cont,schemata = Xlist.fold schemata ([],[]) (fun (cont,schemata) schema ->
      if is_disjunctive schema then cont, schema :: schemata else ((*print_endline "C";*) schema :: cont, schemata)) in
  merge_schemata_rec cont schemata

open ENIAM_LCGlexiconTypes

let nie_vebs = StringSet.of_list ["fin";"bedzie";"praet";"winien";"impt";
                                  "imps";"pred";"inf";"pcon";"pant"]

let imp_aux = StringSet.of_list ["niech";"niechaj";"niechże";"niechajże"]

let rec check_selector_lex_constraints lexemes pos = function
    [] -> true
  | (Negation,Eq,["neg"]) :: selectors ->
    if not (StringSet.mem lexemes "nie") && (StringSet.mem nie_vebs pos) then false
    else check_selector_lex_constraints lexemes pos selectors
  | (Mood,Eq,["conditional"]) :: selectors ->
    if not (StringSet.mem lexemes "by") && (pos = "praet" || pos = "winien") then false
    else check_selector_lex_constraints lexemes pos selectors
  | (Mood,Eq,["imperative"]) :: selectors ->
    if StringSet.is_empty (StringSet.intersection lexemes imp_aux) && pos = "fin" then false
    else check_selector_lex_constraints lexemes pos selectors
  | _  :: selectors -> check_selector_lex_constraints lexemes pos selectors


module OrderedSelector = struct
   type  t = (selector * selector_relation * string list) list
   let compare = compare
end

module SelectorMap = Xmap.Make(OrderedSelector)

let simplify_schemata lexemes pos pos2 lemma schemata =
      (* Xlist.iter schemata (fun (selectors,schema) -> if pos2="verb" then Printf.printf "A %s %s [%s] %s\n" pos lemma (ENIAMcategoriesPL.string_of_selectors selectors) (ENIAMwalStringOf.schema schema)); *)
      let map = Xlist.fold schemata SelectorMap.empty (fun map (selectors,schema) ->
      SelectorMap.add_inc map selectors [schema] (fun l -> schema :: l)) in
      let map = SelectorMap.fold map SelectorMap.empty (fun map selectors schemata ->
          if check_selector_lex_constraints lexemes pos selectors then SelectorMap.add map selectors schemata else map) in
      let schemata = SelectorMap.fold map [] (fun new_schemata selectors schemata ->
          (selectors,Xlist.map schemata (remove_adjuncts_schema pos2 lemma)) :: new_schemata) in
      (* Xlist.iter schemata (fun (_,schemata) ->
          Xlist.iter schemata (fun schema -> if pos2="verb" then Printf.printf "%s %s %s\n" pos lemma (ENIAMwalStringOf.schema schema))); *)
      (* Xlist.iter schemata (fun (selectors,schemata) -> Xlist.iter schemata (fun schema ->
          if pos2="verb" then Printf.printf "B %s %s [%s] %s\n" pos lemma (ENIAMcategoriesPL.string_of_selectors selectors) (String.concat "+" (Xlist.map schema (fun morfs ->
              "{" ^ String.concat ";" (PhraseSet.fold morfs [] (fun l m -> ENIAMwalStringOf.phrase m :: l)) ^ "}"))))); *)
      let schemata = List.flatten (Xlist.map schemata (fun (selectors,schemata) ->
          Xlist.map (merge_schemata schemata) (fun schema -> selectors,Xlist.map schema PhraseSet.to_list))) in
      (* Xlist.iter schemata (fun (selectors,schema) ->
          if pos2="verb" then Printf.printf "C %s %s [%s] %s\n" pos lemma (ENIAMcategoriesPL.string_of_selectors selectors) (String.concat "+" (Xlist.map schema (fun morfs ->
              "{" ^ String.concat ";" (PhraseSet.fold morfs [] (fun l m -> ENIAMwalStringOf.phrase m :: l)) ^ "}")))); *)
      schemata

let add_adjuncts preps compreps compars pos2 (selectors,cat,(*has_context,*)local_schema,schema,distant_schema) =
  let compreps = Xlist.rev_map compreps ENIAMwalRenderer.render_comprep in
  let prepnps = Xlist.rev_map preps (fun (prep,cases) -> ENIAMwalRenderer.render_prepnp prep cases) in
  let prepadjps = Xlist.rev_map preps (fun (prep,cases) -> ENIAMwalRenderer.render_prepadjp prep cases) in
  let compars = Xlist.rev_map compars ENIAMwalRenderer.render_compar in
  match pos2 with
    "verb" -> [selectors,cat,(*has_context,*)local_schema,schema @ ENIAMwalRenderer.verb_adjuncts_simp @ prepnps @ prepadjps @ compreps @ compars,distant_schema]
  | "noun" -> [
      [Nsyn,Eq,["proper"]] @ selectors,cat,(*has_context,*)local_schema,ENIAMwalRenderer.proper_noun_adjuncts_simp @ prepnps @ compreps @ compars,distant_schema;
      [Nsyn,Eq,["common"];Nsem,Eq,["measure"]] @ selectors,cat,(*has_context,*)local_schema,ENIAMwalRenderer.measure_noun_adjuncts_simp @ prepnps @ compreps @ compars,distant_schema;
      [Nsyn,Eq,["common"];Nsem,Neq,["measure"]] @ selectors,cat,(*has_context,*)local_schema,ENIAMwalRenderer.common_noun_adjuncts_simp @ prepnps @ compreps @ compars,distant_schema]
  | "adj" -> [selectors,cat,(*has_context,*)local_schema,schema @ ENIAMwalRenderer.adj_adjuncts_simp @ compars,distant_schema]
  | "adv" -> [selectors,cat,(*has_context,*)local_schema,schema @ ENIAMwalRenderer.adv_adjuncts_simp @ compars,distant_schema]
  | _ -> []

open ENIAMlexSemanticsTypes

let add_subj_cr cr positions =
  Xlist.map positions (fun p ->
    if p.gf = SUBJ then {p with cr=cr :: p.cr} else p)

let add_connected_adjuncts preps compreps compars pos2 frame =
  let compreps = Xlist.rev_map compreps ENIAMwalRenderer.render_connected_comprep in
  let prepnps = Xlist.rev_map preps (fun (prep,cases) -> ENIAMwalRenderer.render_connected_prepnp prep cases) in
  let prepadjps = Xlist.rev_map preps (fun (prep,cases) -> ENIAMwalRenderer.render_connected_prepadjp prep cases) in
  let compars = Xlist.rev_map compars ENIAMwalRenderer.render_connected_compar in
  match pos2 with
    "verb" -> [{frame with positions=(add_subj_cr "3" frame.positions) @ ENIAMwalRenderer.verb_connected_adjuncts_simp @ prepnps @ prepadjps @ compreps @ compars}]
  | "noun" -> [
      {frame with selectors=[Nsyn,Eq,["proper"]] @ frame.selectors; positions=ENIAMwalRenderer.proper_noun_connected_adjuncts_simp @ prepnps @ compreps @ compars};
      {frame with selectors=[Nsyn,Eq,["common"];Nsem,Eq,["measure"]] @ frame.selectors; positions=ENIAMwalRenderer.measure_noun_connected_adjuncts_simp @ prepnps @ compreps @ compars};
      {frame with selectors=[Nsyn,Eq,["common"];Nsem,Neq,["measure"]] @ frame.selectors; positions=frame.positions @ ENIAMwalRenderer.common_noun_connected_adjuncts_simp @ prepnps @ compreps @ compars}]
  | "adj" -> [{frame with positions=frame.positions @ ENIAMwalRenderer.adj_connected_adjuncts_simp @ compars}]
  | "adv" -> [{frame with positions=frame.positions @ ENIAMwalRenderer.adv_connected_adjuncts_simp @ compars}]
  | _ -> []

(* let _ =
  let schemata,entries = ENIAMvalence.prepare_all_valence ENIAMwalParser.phrases ENIAMwalParser.schemata ENIAMwalParser.entries in
  let _ = Entries.map2 schemata (fun pos lemma schemata -> simplify_schemata pos (ENIAMvalence.simplify_pos pos) lemma schemata) in
  () *)


(*
let default_frames = Xlist.fold [ (* FIXME: poprawić domyślne ramki po ustaleniu adjunctów *)
    "verb",(ReflEmpty,Domyslny,NegationUndef,PredNA,AspectUndef,"subj{np(str)}+obj{np(str)}"); (* FIXME: dodać ramkę z refl *)
    "noun",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"{possp}+{adjp(agr)}");
    "adj",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"");
    "adv",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"");
    "empty",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"");
    "date",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"{null;lex(np(gen),sg,'rok',natr)}");
    "date2",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"{null;lex(np(gen),sg,'rok',atr1({adjp(agr)}))}"); (* FIXME: wskazać możliwe podrzędniki *)
    "day",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,""
    (*"{lex(np(gen),sg,XOR('styczeń','luty','marzec','kwiecień','maj','czerwiec','lipiec','sierpień','wrzesień','październik','litopad','grudzień'),atr1({np(gen)}))}"*)); (* FIXME: wskazać możliwe podrzędniki *)
    "hour",(ReflEmpty,Domyslny,NegationNA,PredNA,AspectNA,"{null;lex(advp(temp),pos,'rano',natr)}");
  ] StringMap.empty (fun map (k,(refl,opinion,negation,pred,aspect,schema)) ->
    StringMap.add map k (Frame(DefaultAtrs([],refl,opinion,negation,pred,aspect),prepare_schema expands subtypes equivs schema)))

*)
