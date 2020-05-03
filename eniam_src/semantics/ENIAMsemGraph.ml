(*
 *  ENIAMsemantics implements semantic processing for ENIAM
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

open ENIAMsemTypes
open Xstd
open Printf

(**let empty_concept =
  {c_sense=Dot;c_gsense=Dot;c_orth=Dot;c_name=Dot;(* c_variable: string; c_visible_var: bool;*) c_quant=Dot; c_local_quant=true; (*c_modalities: (string * type_term) list;
   c_left_input_pos: int; c_right_input_pos: int;*) c_relations=Dot; c_variable="",""; c_pos=(-1); c_cat=Dot; c_label=""; c_def_label=""}

let empty_context = {cx_sense=Dot; cx_contents=Dot; cx_relations=Dot; cx_variable="",""; cx_pos=(-1); cx_cat=Dot; cx_label=""; cx_def_label=""}*)

let make_tuple = function
    [] -> Dot
  | [t] -> t
  | l -> Tuple l

(*let rec make_args_list = function
    Tuple l -> List.flatten (Xlist.map l make_args_list)
  | t -> [t]

let symbols = StringSet.of_list [
  "symbol"; "date"; "date-interval"; "hour-minute"; "hour"; "hour-minute-interval"; "hour-interval";
  "year"; "year-interval"; "day"; "day-interval"; "day-month"; "day-month-interval"; "month-interval"; "roman"; "roman-interval";
  "match-result"; "url"; "email"; "phone-number"; "obj-id"; "building-number";
  "month-lex"; "day-lex"]

let rec get_person = function
   ("PERS", Val s) :: _ -> s
 | ("PERS", _) :: _-> failwith "get_person"
 | _ :: l -> get_person l
 | [] -> ""

let make_relation t c =
  match t.gf with
    "subj" | "obj" | "arg" ->
      Relation(t.role,t.role_attr,c)
  | "adjunct" ->
      if t.arev then RevRelation(t.arole,t.arole_attr,c) else
      Relation(t.arole,t.arole_attr,c)
  | "core" -> Relation("CORE","",c)
  | s -> failwith ("make_relation: " ^ s)

(*let make_make_triple_relation t c =
  match t.gf with
    "subj" | "obj" | "arg" ->
      MakeTripleRelation(t.role,t.role_attr,c)
  | "adjunct" -> MakeTripleRelation(t.arole,t.arole_attr,c)
  | s -> failwith ("make_make_triple_relation: " ^ s)*)

(* let add_coerced coerced c =
  if coerced = Dot then Concept c else
  Concept{empty_concept with c_cat=coerced; c_relations=Tuple[Relation("Has","",Concept{c with c_relations=Dot});c.c_relations]} *)
let add_coerced coerced c =
  if coerced = Dot then Concept c else
  Concept{empty_concept with c_cat=coerced; c_relations=Relation("Has","",Concept c)} (* FIXME: trzeba dodać concept do tokenów *)
(* let add_coerced coerced c =
  if coerced = Dot then Concept c else
  let coerced_rels,c_rels = split_relations c.c_relations in
  Concept{empty_concept with c_cat=coerced; c_relations=Tuple[Relation("Has","",Concept{c with c_relations=c_rels});coerced_rels]} *)
let add_coerced2 coerced c =
  if coerced = Dot then c else
  Concept{empty_concept with c_cat=coerced; c_relations=Relation("Has","",c)} (* FIXME: trzeba dodać concept do tokenów *)


let create_normal_concept tokens lex_sems t cat coerced =
  (*if t.agf = ENIAMwalTypes.NOSEM then t.args else*)
  let cat,coerced = if !user_ontology_flag then cat,coerced else Dot,Dot in
  let coerced = if coerced = cat then Dot else coerced in
  let c = {empty_concept with
    c_sense =  (*if !user_ontology_flag then Val t.lemma else*) (*if t.lemma = "<root>" then Dot else*) t.sense;
    c_relations=t.args;
    c_quant=if t.label = "" then t.sem_args else Dot; (* FIXME: zakładam że t.label <> "" występuje tylko dla pro *)
    c_variable=string_of_int t.id,"";
    c_pos=(ExtArray.get tokens t.id).ENIAMtokenizerTypes.beg;
    c_local_quant=true;
    c_cat=cat;
    c_label=t.label;
    c_def_label=t.def_label} in
  if t.pos = "subst" || t.pos = "depr" || t.pos = "ger" || t.pos = "unk" || StringSet.mem symbols t.pos then (* FIXME: wykrywanie plurale tantum *)
    let c = {c with c_local_quant=false} in
    let c,measure,cx_flag = Xlist.fold t.attrs (c,false,false) (fun (c,measure,cx_flag) -> function
        "NSYN",Val "common" -> c,measure,cx_flag
      | "NSYN",Val "proper" -> {c with c_name=Val t.lemma; c_sense=Dot(*t.sense*)(*c_sense=if Val t.pred=c.c_sense then Dot else c.c_sense*)},measure,cx_flag; (* FIXME: zaślepka na potrzeby gramatyk semantycznych *)  (* Rozpoznawanie propoer names nieznanego typu - ryzykowne ale proste *)
      | "NSYN",Val "pronoun" -> c(*{c with c_quant=Tuple[c.c_quant;Val "indexical"]}*),measure,cx_flag
      | "NSEM",Val "count" -> c(*{c with c_quant=Tuple[c.c_quant;Val "count"]}*),measure,cx_flag
      | "NSEM",Val "mass" -> {c with c_quant=Tuple[c.c_quant;Val "mass"]},measure,cx_flag
      | "NSEM",Variant(e,[a,Val "mass";b,Val "count"]) -> {c with c_quant=Tuple[c.c_quant;Variant(e,[a,Val "mass";b,Val "count"])]},measure,cx_flag (* FIXME: tu by należało podzielić to na dwa pudełka *)
      | "NSEM",Variant(e,[a,Val "count";b,Val "mass"]) -> {c with c_quant=Tuple[c.c_quant;Variant(e,[a,Val "count";b,Val "mass"])]},measure,cx_flag
      | "NSEM",Val "measure" -> c,true,cx_flag
      | "NSEM",Val "time" -> c,measure,cx_flag(*failwith "create_normal_concept: time"*)
      | "NUM",t -> {c with c_quant=Tuple[c.c_quant;t]},measure,cx_flag
      | "CASE",_ -> c,measure,cx_flag
      | "GEND",_ -> c,measure,cx_flag
      | "PERS",Val "ter" -> c,measure,cx_flag
      | "PERS",Val "sec" -> {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "impt")]},measure,true
      | "ASPECT",_ -> c,measure,cx_flag
      | "NEGATION",Val "aff" -> c,measure,cx_flag
      | "NEGATION",Val "neg" -> {c with c_quant=Tuple[c.c_quant;Val "nie"]},measure,cx_flag
      | "controller",_ -> c,measure,cx_flag
      (* | "INCLUSION",_ -> c,measure ,cx_flag
      | "QUOT",Val "+" -> {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "quot")]},measure,cx_flag
      | "LEX",_ -> c,measure,cx_flag (* FIXME *) *)
(*       | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]},measure *)
      (* | "TYPE",_ -> c,measure,cx_flag (* FIXME *) *)
      | e,t -> failwith ("create_normal_concept noun: " ^ e ^ ": " ^ ENIAMsemStringOf.linear_term 0 t)) in
    (* let c = if t.pos = "depr" then {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "depr")]} else c in *)
    if cx_flag then
      let id = ExtArray.add tokens ENIAMtokenizerTypes.empty_token_env in
      let _ = ExtArray.add lex_sems ENIAMlexSemanticsTypes.empty_lex_sem in
      make_relation t (Context{empty_context with cx_contents=add_coerced coerced c; cx_variable=string_of_int id,""; cx_pos=c.c_pos})
    else
      make_relation t (add_coerced coerced c) else
  if t.pos = "fin" || t.pos = "bedzie" || t.pos = "praet" || t.pos = "winien" || t.pos = "impt" || t.pos = "imps" || t.pos = "pred" || t.lemma = "pro-komunikować" then
    let c = {c with c_local_quant=false} in
    let c = Xlist.fold t.attrs c (fun c -> function
(*         "SENSE",t -> {c with c_sense=Tuple[c.c_sense;t]} *)
      | "NUM",t -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "ASPECT",_ -> c
      (* | "CTYPE",_ -> c (* FIXME *) *)
      | "TENSE",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation t]}
      | "MOOD",Val "indicative" -> c
      | "MOOD",Val "conditional" -> {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "cond")]} (* FIXME *)
      | "MOOD",Val "imperative" -> {c with c_relations=Tuple[c.c_relations;SingleRelation(Val "impt")]} (* FIXME *)
      | "NEGATION",Val "aff" -> c
      | "NEGATION",Val "neg" -> {c with c_quant=Tuple[c.c_quant;Val "nie"]}
      | e,t -> failwith ("create_normal_concept verb: " ^ e)) in
    let c = if t.lemma = "pro-komunikować" then {c with c_relations=Relation("Theme","",c.c_relations)} else c in (* FIXME: to by trzeba przesunąć na wcześniej *)
    let id = ExtArray.add tokens ENIAMtokenizerTypes.empty_token_env in
    let _ = ExtArray.add lex_sems ENIAMlexSemanticsTypes.empty_lex_sem in
    let cx = {empty_context with cx_contents=add_coerced coerced c; cx_variable=string_of_int id,""; cx_pos=c.c_pos; cx_cat=Val "Situation"} in
    (* if t.role <> "" || t.role_attr <> "" then failwith "create_normal_concept: verb" else *)
    make_relation t (Context cx) else
  if t.pos = "inf" then
    let c = {c with c_local_quant=false} in
    let c = Xlist.fold t.attrs c (fun c -> function
      | "ASPECT",_ -> c
      | "TENSE",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation t]}
      | "NEGATION",Val "aff" -> c
      | "NEGATION",Val "neg" -> {c with c_quant=Tuple[c.c_quant;Val "nie"]}
      | e,t -> failwith ("create_normal_concept verb: " ^ e)) in
    let id = ExtArray.add tokens ENIAMtokenizerTypes.empty_token_env in
    let _ = ExtArray.add lex_sems ENIAMlexSemanticsTypes.empty_lex_sem in
    let cx = {empty_context with cx_contents=add_coerced coerced c; cx_variable=string_of_int id,""; cx_pos=c.c_pos; cx_cat=Val "Situation"} in
    make_relation t (Context cx) else
  if t.pos = "adj" || t.pos = "adjc" || t.pos = "adjp" || t.pos = "adja" || t.pos = "pact" || t.pos = "ppas" || t.pos = "apron" || t.pos = "ordnum" || t.pos = "roman-adj" then
    let c = if t.pos = "pact" || t.pos = "ppas" then {c with c_local_quant=false} else c in
    let c = Xlist.fold t.attrs c (fun c -> function
(*         "SENSE",t -> {c with c_sense=Tuple[c.c_sense;t]} *)
      | "SYN",Val "common" -> c
      | "SYN",Val "pronoun" -> c(*{c with c_quant=Tuple[c.c_quant;Val "indexical"]}*)
      | "SYN",Val "proper" -> if t.pos = "roman-adj" then c else failwith "create_normal_concept adj: SYN=proper"
      | "NSEM",Val "count" -> if t.pos = "roman-adj" then c else failwith "create_normal_concept adj: NSEM=count"
      | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "GRAD",Val "pos" -> c
      | "GRAD",Val "com" -> {c with c_relations=Tuple[c.c_relations;SingleRelation (Val "com")]}
      | "GRAD",Val "sup" -> {c with c_relations=Tuple[c.c_relations;SingleRelation (Val "sup")]}
      | "ASPECT",_ -> c
      | "CTYPE",_ -> c (* FIXME1: trzeba zaznaczyć pytajność w grafie, CTYPE pojawia się w dwu węzłach *)
(*       | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]} *)
      | "TYPE",_ -> c (* FIXME *)
      | "PERS",_ -> c
      | "NEGATION",Val "aff" -> c
      | "NEGATION",Val "neg" -> {c with c_quant=Tuple[c.c_quant;Val "nie"]}
      | "LEX",_ -> c (* FIXME *)
      | e,t -> failwith ("create_normal_concept adj: " ^ e)) in
    make_relation t (Concept c) else
  if t.pos = "adv" || t.pos = "pcon" || t.pos = "pant" then
    let c = if t.pos = "pcon" || t.pos = "pant" then {c with c_local_quant=false} else c in
    let c = Xlist.fold t.attrs c (fun c -> function
(*         "SENSE",t -> {c with c_sense=Tuple[c.c_sense;t]} *)
      | "GRAD",Val "pos" -> c
      | "GRAD",Val "com" -> {c with c_relations=Tuple[c.c_relations;SingleRelation (Val "com")]}
      | "GRAD",Val "sup" -> {c with c_relations=Tuple[c.c_relations;SingleRelation (Val "sup")]}
      | "ASPECT",_ -> c
(*       | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]} *)
      | "TYPE",_ -> c
      | "MODE",_ -> c
      | "NEGATION",Val "aff" -> c
      | "NEGATION",Val "neg" -> {c with c_quant=Tuple[c.c_quant;Val "nie"]}
      | e,t -> failwith ("create_normal_concept adv: " ^ e)) in
    make_relation t (add_coerced coerced c) else
  if t.pos = "prep" then
    (* if t.arole = "NOSEM" then make_relation t (t.args) else *)
    let c,is_sem = Xlist.fold t.attrs (c,false) (fun (c,is_sem) -> function
      | "CASE",_ -> c,is_sem
      | "PSEM",Val "sem" -> c,true
      | "PSEM",Val "nosem" -> c,false
      | e,t -> failwith ("create_normal_concept prep: " ^ e)) in
    (* make_make_triple_relation t (Concept c) else *)
    if is_sem then make_relation t (add_coerced2 coerced (CreateContext({empty_context with cx_sense=c.c_sense; cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat},c.c_relations)))
    else make_relation t (RemoveRelation("CORE","",c.c_relations)) else
  if coerced <> Dot then failwith ("create_normal_concept coerced: " ^ t.lemma ^ ":" ^ t.pos) else
  if t.pos = "pro" || t.pos = "ppron12" || t.pos = "ppron3" || t.pos = "siebie" then (* FIXME: indexicalność *)
    let c = {c with c_local_quant=false} in
    let c = Xlist.fold t.attrs c (fun c -> function
        (* "NUM",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation t]}
      | "GEND",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation t]}
      | "PERS",t2 -> if t.pos = "siebie" then c else {c with c_relations=Tuple[c.c_relations;SingleRelation t2]} *)
        "NUM",t -> {c with c_quant=Tuple[c.c_quant;t]}
      | "GEND",t -> {c with c_quant=Tuple[c.c_quant;t]}
      | "PERS",t2 -> if t.pos = "siebie" then c else {c with c_quant=Tuple[c.c_quant;t2]}
      | "CASE",_ -> c
      | "SYN",_ -> c
      | "NSEM",_ -> c
      | "controller",_ -> c
      | "controllee",_ -> c
      (* | "coref",t -> {c with c_relations=Tuple[c.c_relations;SingleRelation (Val "coref")]} (* FIXME: zaślepka do poprawienia przy implementacji kontroli *) *)
      | e,t -> failwith ("create_normal_concept pron: " ^ e)) in
    make_relation t (Concept c) else
  if t.pos = "num" || t.pos = "intnum" || t.pos = "realnum" || t.pos = "intnum-interval" || t.pos = "realnum-interval" then
    let c = Xlist.fold t.attrs c (fun c -> function
(*         "SENSE",t -> {c with c_sense=Tuple[c.c_sense;t]} *)
      | "ACM",_ -> c
      | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "NSYN",_ -> c
      | "NSEM",_ -> c
      | e,t -> failwith ("create_normal_concept num: " ^ e)) in
    make_relation t ((*Quantifier*)(Concept c)) else
  if t.pos = "part" && t.lemma="się" then
    (*let c = {c with c_quant=Tuple[c.c_quant;Val "coreferential"]} in*)
    make_relation t ((*Quantifier*)(Concept c)) else
  if t.pos = "part" && (t.lemma="czy" || t.lemma="gdyby") then
    make_relation t (SetContextName(c.c_sense,RemoveRelation("CORE","",c.c_relations))) else
  if t.pos = "qub" then
    let c = Xlist.fold t.attrs c (fun c -> function
(*      | "TYPE",Val "int" -> {c with c_quant=Tuple[c.c_quant;Val "interrogative"]}
      | "TYPE",_ -> c*)
      | e,t -> failwith ("create_normal_concept qub: " ^ e)) in
    make_relation t (Concept c) else
  if t.pos = "comp" then
    make_relation t (SetContextName(c.c_sense,RemoveRelation("CORE","",c.c_relations))) else
  if t.pos = "conj" then
    let c = {empty_context with cx_sense=t.sense; cx_contents=t.args; cx_variable=c.c_variable; cx_pos=c.c_pos; cx_cat=c.c_cat; cx_def_label=c.c_def_label; cx_label=c.c_label} in
    let c = Xlist.fold t.attrs c (fun c -> function
      | "NUM",_ -> c
      | "CASE",_ -> c
      | "GEND",_ -> c
      | "PERS",_ -> c
      | "ASPECT",_ -> c
      | "controller",_ -> c
      | "controllee",_ -> c
      | e,t -> failwith ("create_normal_concept conj: " ^ e)) in
    ManageCoordination({t with attrs=[]; args=Dot},Context c) else
  (* if t.pos = "interj" then
    let c = Xlist.fold t.attrs c (fun c -> function
      | e,t -> failwith ("create_normal_concept interj: " ^ e)) in
    make_relation t (Concept c) else *)
  if t.pos = "sinterj" || t.pos = "interj" then
    let c = Xlist.fold t.attrs c (fun c -> function
      | e,t -> failwith ("create_normal_concept sinterj: " ^ e)) in
    let id = ExtArray.add tokens ENIAMtokenizerTypes.empty_token_env in
    let _ = ExtArray.add lex_sems ENIAMlexSemanticsTypes.empty_lex_sem in
    let cx = {empty_context with cx_contents=add_coerced coerced c; cx_variable=string_of_int id,""; cx_pos=c.c_pos; cx_cat=Val "Situation"} in
    make_relation t (Context cx) else
  if t.lemma = "<root>" then t.args else
  if t.lemma = "<merge>" then RemoveRelation("null","",t.args) else
  (* if t.pos = "interp" && t.lemma = "?" && t.args = Dot then SingleRelation(Val "int") else *)
  if t.pos = "interp" && t.lemma = "?" then
    make_relation t (AddSingleRelation(Val "int",RemoveRelation("CORE","",t.args))) else (* FIXME1: to powinno tworzyć kontekst i zaznaczać ze jest interrogative *)
  if t.pos = "interp" && t.lemma = ":" then
    make_relation t (RemoveRelation("CORE","",t.args)) else
  if t.pos = "interp" && t.lemma = "</sentence>" then
    let l = (*List.rev*) (make_args_list t.args) in
    Xlist.fold (List.tl l) (RemoveRelation("null","",List.hd l)) (fun t s -> AddRelation(t,"Next","Clause",RemoveRelation("null","",s))) else
  if t.pos = "interp" && t.lemma = "<sentence>" then t.args else
  if t.pos = "interp" && t.lemma = "</query>" then
    let l = (*List.rev*) (make_args_list t.args) in
    Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(t,"Next","Sentence",s)) else
  if t.pos = "interp" && t.lemma = "<query>" then t.args else
(*  if t.pos = "interp" && t.lemma = "”s" then
    let l = List.rev (make_args_list t.args) in
    let x = Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) in
    Relation(t.arole,t.arole_attr,x) else (* FIXME: czy na pewno tu i w następnych arole a nie position.role? *)
  if t.pos = "interp" && t.lemma = "<or>" then
    Relation(t.arole,t.arole_attr,t.args) else
  if t.pos = "interp" && t.lemma = "<speaker>" then
    Relation(t.arole,t.arole_attr,RemoveRelation t.args) else
  if t.pos = "interp" && t.lemma = "</query>" then
    let l = List.rev (make_args_list t.args) in
    let x = Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) in
    if t.gf = "obj" then Relation(t.arole,t.arole_attr,x) else x else
  if t.pos = "interp" && t.lemma = "<query1>" then t.args else
  if t.pos = "interp" && t.lemma = "<query2>" then t.args else
  if t.pos = "interp" && t.lemma = "<query4>" then t.args else
  if t.pos = "interp" && t.lemma = "<query5>" then
    let l = List.rev (make_args_list t.args) in
    Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) else
  if t.pos = "interp" && t.lemma = "<query6>" then
    let l = List.rev (make_args_list t.args) in
    Xlist.fold (List.tl l) (List.hd l) (fun t s -> AddRelation(RemoveRelation t,"Next","Sentence",RemoveRelation s)) else
  if t.pos = "interp" && t.lemma = "?" then SingleRelation(Val "int") else
  if t.pos = "interp" && t.lemma = "„" then
    make_relation t (RemoveRelation t.args) else
  if t.pos = "interp" || t.lemma = "</or-sentence>" then make_relation t (t.args) else*) (
  if t.pos = "interp" then Node t else
  (*if t.pos = "" then make_relation t (t.args) else*)
  (* print_endline t.lemma; *)
  Node t)

let rec translate_node tokens lex_sems t =
  let attrs = Xlist.map t.ENIAM_LCGtypes.attrs (fun (k,t) -> k, create_concepts tokens lex_sems t) in
  let t = {
    orth=t.ENIAM_LCGtypes.orth; lemma=t.ENIAM_LCGtypes.lemma; pos=t.ENIAM_LCGtypes.pos; weight=t.ENIAM_LCGtypes.weight;
    id=t.ENIAM_LCGtypes.id; symbol=create_concepts tokens lex_sems t.ENIAM_LCGtypes.symbol; arg_symbol=create_concepts tokens lex_sems t.ENIAM_LCGtypes.arg_symbol;
    arg_dir=t.ENIAM_LCGtypes.arg_dir;
    attrs=[]; label=""; def_label=""; snode="";
    args=create_concepts tokens lex_sems t.ENIAM_LCGtypes.args;
    gf=""; role=""; role_attr=""; coord_arg=""; selprefs=Dot; sense=Dot; arole=""; arole_attr=""; arev=false; sem_args=Dot;
    (*cat=Dot;coerced=Dot*)} in
  let t,attrs,cat,coerced = Xlist.fold attrs (t,[],Dot,Dot) (fun (t,attrs,cat,coerced) -> function
      "gf",Val s -> {t with gf=s},attrs,cat,coerced
    | "role",Val s -> {t with role=s},attrs,cat,coerced
    | "role-attr",Val s -> {t with role_attr=s},attrs,cat,coerced
    | "selprefs",s -> {t with selprefs=s},attrs,cat,coerced
    | "sense",s -> {t with sense=s},attrs,cat,coerced
    | "hipero",_ -> t,attrs,cat,coerced
    | "arole",Val s -> {t with arole=s},attrs,cat,coerced
    | "arole-attr",Val s -> {t with arole_attr=s},attrs,cat,coerced
    | "arev",Val "-" -> {t with arev=false},attrs,cat,coerced
    | "arev",Val "+" -> {t with arev=true},attrs,cat,coerced
    | "agf",Val s -> t,attrs,cat,coerced
    | "sem-args",s -> {t with sem_args=s},attrs,cat,coerced
    | "rev-hipero",_ -> t,attrs,cat,coerced
    | "fopinion",_ -> t,attrs,cat,coerced
    | "sopinion",_ -> t,attrs,cat,coerced
    | "ACM",s -> t,("ACM",s) :: attrs,cat,coerced
    | "ASPECT",s -> t,("ASPECT",s) :: attrs,cat,coerced
    | "NEGATION",s -> t,("NEGATION",s) :: attrs,cat,coerced
    | "MOOD",s -> t,("MOOD",s) :: attrs,cat,coerced
    | "TENSE",s -> t,("TENSE",s) :: attrs,cat,coerced
    | "CTYPE",s -> t,("CTYPE",s) :: attrs,cat,coerced
    | "controller",s -> t,("controller",s) :: attrs,cat,coerced
    | "controllee",s -> t,("controllee",s) :: attrs,cat,coerced
    | "coref",s -> t,attrs,cat,coerced
    | "label",Val s -> {t with label=s},attrs,cat,coerced
    | "def-label",Val s -> {t with def_label=s},attrs,cat,coerced
    | "CAT",s -> t,attrs,s,coerced
    | "COERCED",s -> t,attrs,cat,s
    | "NUM",s -> t,("NUM",s) :: attrs,cat,coerced
    | "CASE",s -> t,("CASE",s) :: attrs,cat,coerced
    | "GEND",s -> t,("GEND",s) :: attrs,cat,coerced
    | "PERS",s -> t,("PERS",s) :: attrs,cat,coerced
    | "NSYN",s -> t,("NSYN",s) :: attrs,cat,coerced
    | "NSEM",s -> t,("NSEM",s) :: attrs,cat,coerced
    | "MODE",s -> t,("MODE",s) :: attrs,cat,coerced
    | "GRAD",s -> t,("GRAD",s) :: attrs,cat,coerced
    | "PSEM",s -> t,("PSEM",s) :: attrs,cat,coerced
    (* | k,v -> printf "translate_node: %s %s\n%!" k (ENIAMsemStringOf.linear_term 0 v); t, (k,v) :: attrs,cat,coerced) in *)
    | k,v -> failwith (sprintf "translate_node: %s %s\n%!" k (ENIAMsemStringOf.linear_term 0 v))) in
  {t with attrs=attrs},cat,coerced

and create_concepts tokens lex_sems = function
    ENIAM_LCGtypes.Node t ->
      let t,cat,coerced = translate_node tokens lex_sems t in
      create_normal_concept tokens lex_sems t cat coerced
  | ENIAM_LCGtypes.Tuple l -> Tuple(Xlist.map l (create_concepts tokens lex_sems))
  | ENIAM_LCGtypes.Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, create_concepts tokens lex_sems t))
  | ENIAM_LCGtypes.Dot -> Dot
  | ENIAM_LCGtypes.Val s -> Val s
  | ENIAM_LCGtypes.Ref i -> Ref i
  (* | Choice choices -> Choice(StringMap.map choices (create_concepts tokens lex_sems)) *)
  | t -> failwith ("create_concepts: " ^ ENIAM_LCGstringOf.linear_term 0 t)


let translate tokens lex_sems term =
  let sem = Array.make (Array.length term) Dot in
  Int.iter 0 (Array.length sem - 1) (fun i ->
    sem.(i) <- create_concepts tokens lex_sems term.(i));
  sem**)
let translate tokens lex_sems term = failwith "translate: ni"

let rec make_tree_rec references = function
    Node t -> Node{t with args=make_tree_rec references t.args}
  | Concept c -> Concept{c with relations=make_tree_rec references c.relations; contents=make_tree_rec references c.contents}
(*   | Context c -> Context{c with cx_contents=make_tree_rec references c.cx_contents; cx_relations=make_tree_rec references c.cx_relations} *)
  | Relation(r,a,t) -> Relation(r,a,make_tree_rec references t)
  | RevRelation(r,a,t) -> RevRelation(r,a,make_tree_rec references t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,make_tree_rec references s,make_tree_rec references t) *)
  | AddRelation(t,r,a,s) -> AddRelation(make_tree_rec references t,r,a,make_tree_rec references s)
  | AddParentRelation(t,s) -> AddParentRelation(make_tree_rec references t,make_tree_rec references s)
  | AddSingleRelation(r,s) -> AddSingleRelation(r,make_tree_rec references s)
  | RemoveRelation(r,a,t) -> RemoveRelation(r,a,make_tree_rec references t)
  | SetContextName(s,t) -> SetContextName(s,make_tree_rec references t)
  | CreateContext(s,t) -> CreateContext(s,make_tree_rec references t)
  (* | MakeTripleRelation(r,a,t) -> MakeTripleRelation(r,a,make_tree_rec references t) *)
  | ManageCoordination(n,t) -> ManageCoordination(n,make_tree_rec references t)
  | Tuple l -> Tuple(Xlist.map l (make_tree_rec references))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, make_tree_rec references t))
  | Dot -> Dot
  | Val s -> Val s
  | Ref i -> make_tree_rec references references.(i)
  (* | t -> failwith ("make_tree_rec: " ^ LCGstringOf.linear_term 0 t) *)

let make_tree references =
  (*RemoveRelation*)(make_tree_rec references references.(0))

let rec validate_translation r = function
    Node t ->
      r := ("validate_translation: " ^ ENIAMsemStringOf.linear_term 0 (Node{t with args=Dot})) :: !r;
      validate_translation r t.args
  | Concept c -> validate_translation r c.relations; validate_translation r c.contents
(*   | Context c -> validate_translation r c.cx_contents; validate_translation r c.cx_relations *)
  | Relation(_,_,t) -> validate_translation r t
  | RevRelation(_,_,t) -> validate_translation r t
  | SingleRelation _  -> ()
  (* | TripleRelation(_,_,s,t) -> validate_translation r s; validate_translation r t *)
  | AddRelation(t,_,_,s) -> validate_translation r t; validate_translation r s
  | AddParentRelation(t,s) -> validate_translation r t; validate_translation r s
  | AddSingleRelation(_,s) -> validate_translation r s
  | RemoveRelation(_,_,t) -> validate_translation r t
  | SetContextName(s,t) -> validate_translation r t
  | CreateContext(s,t) -> validate_translation r t
  (* | MakeTripleRelation(_,_,t) -> validate_translation r t *)
  | ManageCoordination(_,t) -> validate_translation r t
  | Tuple l -> Xlist.iter l (validate_translation r)
  | Variant(e,l) ->
      if e = "" then r := "validate_translation: empty variant label" :: !r;
      Xlist.iter l (fun (i,t) -> validate_translation r t)
  | Dot -> ()
  | t -> failwith ("validate_translation: " ^ ENIAMsemStringOf.linear_term 0 t)

(***************************************************************************************)

let rec simplify_tree_add_relation r a s = function
    Concept c -> Concept{c with relations=Tuple[Relation(r,a,s);c.relations]}
(*   | Context c -> Context{c with cx_relations=Tuple[Relation(r,a,s);c.cx_relations]} *)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, simplify_tree_add_relation r a s t))
  | t -> AddRelation(t,r,a,s)

let rec transpose_tuple_variant e ll =
  match List.hd ll with
    _,[] -> []
  | _ ->
     let hd,tl = Xlist.fold ll ([],[]) (fun (hd,tl) (i,l) ->
       (i,List.hd l) :: hd, (i,List.tl l) :: tl) in
     (Variant (e,List.rev hd)) :: (transpose_tuple_variant e (List.rev tl))

(* FIXME TODO:
Bryka chmara wieczorów: problem z wyborem relacji
uzgadnianie preferencji i role tematyczne przy num, measure i prep:nosem
Witold bryka.: dezambiguacja
Niearanżowany szpak bryka.: lematyzacja 'Niearanżowany'

dobre:
Bryka na chmarze strusi.
Pięć strusi bryka.
*)

let rec is_core_variant = function
    Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> is_core_variant t && b)
  | Relation("CORE","",_) -> true
  | Relation _ -> false
  | RevRelation _ -> false
  | SingleRelation _ -> false
  (* | TripleRelation("CORE","",_,_) -> true *)
  | Dot -> false
  | t -> failwith ("is_core_variant: " ^ ENIAMsemStringOf.linear_term 0 t)

let get_core_tuple = function
    Tuple l ->
      let core,nocore = Xlist.fold l ([],[]) (fun (core,nocore) t ->
        if is_core_variant t then t :: core,nocore else core,t :: nocore) in
      (match core with
        [t] -> t
      | _ -> failwith "get_core_tuple"),
      (match nocore with
        [] -> Dot
      | [t] -> t
      | l -> Tuple l)
  | t -> if is_core_variant t then t,Dot else failwith ("get_core_tuple: " ^ ENIAMsemStringOf.linear_term 0 t)

(* let get_core c =
  let core,l = get_core_tuple c.c_relations in
  core,{c with c_relations=l} *)

let set_aroles t r a b =
  if t.arole="" then {t with arole=r; arole_attr=a; arev=b} else
  if t.arole=r && t.arole_attr=a && t.arev=b then t else
  failwith ("set_aroles: t.arole=" ^ t.arole ^ " r=" ^ r)

let rec extract_aroles t = function
    Relation(r,a,s) -> set_aroles t r a false, s
  | RevRelation(r,a,s) -> set_aroles t r a true, s
  | Tuple l ->
      let t,l = Xlist.fold l (t,[]) (fun (t,l) s ->
        let t,s = extract_aroles t s in t, s :: l) in
      t,Tuple(List.rev l)
  | Variant(e,l) ->
      let t,l = Xlist.fold l (t,[]) (fun (t,l) (i,s) ->
        let t,s = extract_aroles t s in t, (i,s) :: l) in
      t,Variant(e,List.rev l)
  | Dot -> t,Dot
  | s -> failwith ("extract_aroles: " ^ ENIAMsemStringOf.linear_term 0 s)

let rec reduce_tree = function
    Concept c -> Concept{c with relations=reduce_tree c.relations; contents=reduce_tree c.contents}
(*   | Context c -> Context{c with cx_contents=reduce_tree c.cx_contents; cx_relations=reduce_tree c.cx_relations} *)
  | Relation(r,a,t) ->
      (match reduce_tree t with
        AddParentRelation(x,Dot) -> x
      | AddParentRelation(x,y) -> Tuple[Relation(r,a,y);x]
      | t -> Relation(r,a,reduce_tree t))
  | RevRelation(r,a,t) -> RevRelation(r,a,reduce_tree t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,reduce_tree s,reduce_tree t) *)
(*  | AddRelation(Concept c,r,a,s) -> reduce_tree (Concept{c with c_relations=Tuple[Relation(Val r,Val a,s);c.c_relations]})
  | AddRelation(Context c,r,a,s) -> reduce_tree (Context{c with cx_relations=Tuple[Relation(Val r,Val a,s);c.cx_relations]})*)
  | AddSingleRelation(r,t) ->
      (match reduce_tree t with
        Concept t -> Concept{t with relations=Tuple[t.relations;SingleRelation r]}
(*      | Context({cx_sense=Val "czy"} as t) -> Context t
      | Context({cx_sense=Val "jaki"} as t) -> Context t
      | Context({cx_sense=Dot} as t) -> Context{t with cx_sense=Val "czy"}*)
      | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, reduce_tree (AddSingleRelation(r,t))))
      | t -> AddSingleRelation(r,t))
  | AddRelation(t,r,a,s) -> simplify_tree_add_relation r a (reduce_tree s) (reduce_tree t)
(*      let t = reduce_tree t in
      let s = reduce_tree s in
      (match t with
        Concept c -> Concept{c with c_relations=Tuple[Relation(Val r,Val a,s);c.c_relations]}
      | Context c -> Context{c with cx_relations=Tuple[Relation(Val r,Val a,s);c.cx_relations]}
      | _ -> AddRelation(t,r,a,s))*)
  | AddParentRelation(t,s) -> AddParentRelation(reduce_tree t,reduce_tree s)
  | RemoveRelation(r0,a0,t) ->
      (match reduce_tree t with
        Relation(r,a,t) ->
            if (r = r0 && a = a0) || r0 = "" then t else
            Concept{empty_concept with cat="Situation"; contents=
              Concept{empty_concept with relations=Relation(r,a,t)}; (*cx_variable=string_of_int id,""; cx_pos=c.c_pos*)}
      (* | TripleRelation(r,a,s,t) ->
            Context{empty_context with cx_contents=
              Concept{empty_concept with c_relations=TripleRelation(r,a,s,t)}; (*cx_variable=string_of_int id,""; cx_pos=c.c_pos*)} *)
      | Dot -> Dot
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i,RemoveRelation(r0,a0,t))))
      | Tuple l -> reduce_tree (Tuple(Xlist.map l (fun t -> RemoveRelation(r0,a0,t))))
      | Concept c -> Concept c (* FIXME: to jest obejście błędu *)
      (* | Context t -> Context t
      | Concept t -> Concept t *)
      | t -> RemoveRelation(r0,a0,t))
  | SetContextName(s,t) ->
      (match reduce_tree t with
        Concept({sense=""} as t) -> Concept{t with sense=s}
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i,SetContextName(s,t))))
      | t ->  SetContextName(s,t))
  | CreateContext(c,t) ->
      (match reduce_tree t with
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i, CreateContext(c,t))))
      | t ->
          let core,t = get_core_tuple t in
          Concept{c with relations=t; contents=reduce_tree (RemoveRelation("CORE","",core))})
(*  | MakeTripleRelation(r,a,t) ->
      (match reduce_tree t with
        Concept t ->
          let core,t = get_core t in
          TripleRelation(r,a,Concept t,reduce_tree (RemoveRelation("CORE","",core)))
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,t) -> i, MakeTripleRelation(r,a,t))))
      | t -> MakeTripleRelation(r,a,t))*)
  | ManageCoordination(t,c) ->
      (match reduce_tree c with
        Concept c ->
           let t,args = extract_aroles {t with arole=""} c.contents in
           (*make_relation t (Context {c with cx_contents=args})*) (* FIXME: to trzeba poprawić tak by działało w obu wersjach parserów *)
           Relation(t.role,"",Concept {c with contents=args})
      | Variant(e,l) -> reduce_tree (Variant(e,Xlist.map l (fun (i,c) -> i,ManageCoordination(t,c))))
      | c -> ManageCoordination(t,c))
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l reduce_tree))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, reduce_tree t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("reduce_tree: " ^ ENIAMsemStringOf.linear_term 0 t)

let rec validate_reduction r = function
    Concept c -> validate_reduction r c.relations; validate_reduction r c.contents
(*   | Context c -> validate_reduction r c.cx_contents; validate_reduction r c.cx_relations *)
  | Relation(_,_,t) -> validate_reduction r t
  | RevRelation(_,_,t) -> validate_reduction r t
  | SingleRelation _  -> ()
  (* | TripleRelation(_,_,s,t) -> validate_reduction r s; validate_reduction r t *)
  | Tuple l -> Xlist.iter l (validate_reduction r)
  | Variant(e,l) ->
      if e = "" then r := "validate_reduction: empty variant label" :: !r;
      Xlist.iter l (fun (i,t) -> validate_reduction r t)
  | Dot -> ()
  | t -> r := ("validate_reduction: " ^ ENIAMsemStringOf.linear_term 0 t) :: !r

(***************************************************************************************)

let rec count_variant_labels map = function
    Concept c -> Xlist.fold (c.relations :: [c.contents]) map count_variant_labels
(*    Concept c -> Xlist.fold [c.c_sense; c.c_name; c.c_quant; c.c_cat; c.c_relations] map count_variant_labels
  | Context c -> Xlist.fold [c.cx_sense; c.cx_contents; c.cx_cat; c.cx_relations] map count_variant_labels*)
  | Relation(_,_,t) -> count_variant_labels map t
  | RevRelation(_,_,t) -> count_variant_labels map t
  | SingleRelation t  -> count_variant_labels map t
  | Tuple l -> Xlist.fold l map count_variant_labels
  | Variant(e,l) ->
      let map = StringQMap.add map e in
      Xlist.fold l map (fun map (i,t) -> count_variant_labels map t)
  | Dot -> map
  | Val s -> map
  | t -> failwith ("count_variant_labels: " ^ ENIAMsemStringOf.linear_term 0 t)

let rec remove_variant_labels map = function
    Concept c -> Concept{c with
(*      c_sense=remove_variant_labels map c.c_sense;
      c_name=remove_variant_labels map c.c_name;
      c_quant=remove_variant_labels map c.c_quant;
      c_cat=remove_variant_labels map c.c_cat;*)
      contents=remove_variant_labels map c.contents;
      relations=remove_variant_labels map c.relations}
(*  | Context c -> Context{c with
      cx_sense=remove_variant_labels map c.cx_sense;
      cx_contents=remove_variant_labels map c.cx_contents;
      cx_cat=remove_variant_labels map c.cx_cat;
      cx_relations=remove_variant_labels map c.cx_relations}*)
  | Relation(r,a,t) -> Relation(r,a,remove_variant_labels map t)
  | RevRelation(r,a,t) -> RevRelation(r,a,remove_variant_labels map t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (remove_variant_labels map)))
  | Variant(e,l) ->
      let e = if StringQMap.find map e = 1 then "" else e in
      let l = Xlist.rev_map l (fun (i,t) -> i, remove_variant_labels map t) in
      Variant(e,Xlist.sort l (fun x y -> compare (fst x) (fst y)))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("remove_variant_labels: " ^ ENIAMsemStringOf.linear_term 0 t)

let rec set_variant_labels map = function
    Concept c -> Concept{c with
(*      c_sense=set_variant_labels map c.c_sense;
      c_name=set_variant_labels map c.c_name;
      c_quant=set_variant_labels map c.c_quant;
      c_cat=set_variant_labels map c.c_cat;*)
      contents=set_variant_labels map c.contents;
      relations=set_variant_labels map c.relations}
(*  | Context c -> Context{c with
      cx_sense=set_variant_labels map c.cx_sense;
      cx_contents=set_variant_labels map c.cx_contents;
      cx_cat=set_variant_labels map c.cx_cat;
      cx_relations=set_variant_labels map c.cx_relations}*)
  | Relation(r,a,t) -> Relation(r,a,set_variant_labels map t)
  | RevRelation(r,a,t) -> RevRelation(r,a,set_variant_labels map t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(List.rev (Xlist.rev_map l (set_variant_labels map)))
  | Variant(e,l) ->
      let e = try StringMap.find map e with Not_found -> ENIAM_LCGreductions.get_variant_label () in
      let l = Xlist.rev_map l (fun (i,t) -> i, set_variant_labels map t) in
      Variant(e,List.rev l)
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("set_variant_labels: " ^ ENIAMsemStringOf.linear_term 0 t)

let manage_variant_labels t =
  ENIAM_LCGreductions.reset_variant_label ();
  let qmap = count_variant_labels StringQMap.empty t in
  let map = StringQMap.fold qmap StringMap.empty (fun map k _ ->
    if k = "" then map else
    StringMap.add map k (ENIAM_LCGreductions.get_variant_label ())) in
  set_variant_labels map t

let rec simplify_tree = function
    Concept c -> Concept{c with
(*      c_sense=simplify_tree c.c_sense;
      c_name=simplify_tree c.c_name;
      c_quant=simplify_tree c.c_quant;
      c_cat=simplify_tree c.c_cat;*)
      contents=simplify_tree c.contents;
      relations=simplify_tree c.relations}
(*  | Context c -> Context{c with
      cx_sense=simplify_tree c.cx_sense;
      cx_contents=simplify_tree c.cx_contents;
      cx_cat=simplify_tree c.cx_cat;
      cx_relations=simplify_tree c.cx_relations}*)
  | Relation(r,a,t) -> Relation(r,a,simplify_tree t)
  | RevRelation(r,a,t) -> RevRelation(r,a,simplify_tree t)
  | SingleRelation r  -> SingleRelation r
  (* | TripleRelation(r,a,s,t) -> TripleRelation(r,a,simplify_tree s,simplify_tree t) *)
  | Tuple l ->
      let l = Xlist.fold l [] (fun l t ->
        match simplify_tree t with
          Dot -> l
        | Tuple l2 -> l2 @ l
        | t -> t :: l) in
      make_tuple (List.rev l)
  | Variant(_,[_,t]) -> simplify_tree t
  | Variant(e,l) ->
      let l = Xlist.map l (fun (i,t) -> i, simplify_tree t) in
      let set = Xlist.fold l TermSet.empty (fun set (_,t) -> TermSet.add set t) in
      if TermSet.size set = 1 then TermSet.max_elt set else
      let l = List.rev (fst (TermSet.fold set ([],1) (fun (l,i) t -> (string_of_int i,t) :: l, i+1))) in
      let _,t = List.hd l in
      let b = Xlist.fold (List.tl l) true (fun b (_,s) -> if s = t then b else false) in
      if b then t else
      (try
        (match t with
           Concept c ->
             let lt = Xlist.fold l [] (fun lt -> function
                 i,Concept c2 ->
                    if c.sense = c2.sense && c.cat = c2.cat && c.label = c2.label &&
                      c.def_label = c2.def_label && c2.contents = Dot then (i,c2.relations) :: lt else raise Not_found
               | _ -> raise Not_found) in
             let e = if e = "" then ENIAM_LCGreductions.get_variant_label () else e in
             Concept{c with
               relations = simplify_tree (Variant(e,lt))}
(*           Concept c ->
             let lt1,lt2,lt3 = Xlist.fold l ([],[],[]) (fun (lt1,lt2,lt3) -> function
                 i,Concept c2 ->
                    if c.c_sense = c2.c_sense && c.c_name = c2.c_name &&
                      c.c_local_quant = c2.c_local_quant && c.c_label = c2.c_label &&
                      c.c_def_label = c2.c_def_label then (i,c2.c_quant) :: lt1, (i,c2.c_relations) :: lt2, (i,c2.c_cat) :: lt3 else raise Not_found
               | _ -> raise Not_found) in
             let e = if e = "" then ENIAM_LCGreductions.get_variant_label () else e in
             Concept{c with
               c_quant = simplify_tree (Variant(e,lt1));
               c_relations = simplify_tree (Variant(e,lt2));
               c_cat = simplify_tree (Variant(e,lt3))}
         | Context c ->
             let lt1,lt2,lt3 = Xlist.fold l ([],[],[]) (fun (lt1,lt2,lt3) -> function
                 i,Context c2 -> if c.cx_sense = c2.cx_sense && c.cx_label = c2.cx_label &&
                      c.cx_def_label = c2.cx_def_label then (i,c2.cx_contents) :: lt1, (i,c2.cx_relations) :: lt2, (i,c2.cx_cat) :: lt3 else raise Not_found
               | _ -> raise Not_found) in
             let e = if e = "" then ENIAM_LCGreductions.get_variant_label () else e in
             Context{c with
               cx_contents= simplify_tree (Variant(e,lt1));
               cx_relations = simplify_tree (Variant(e,lt2));
               cx_cat = simplify_tree (Variant(e,lt3))}*)
        | Relation(r,a,t) ->
             let lt = Xlist.fold l [] (fun lt -> function
                 i,Relation(r2,a2,t2) -> if r = r2 && a = a2 then (i,t2) :: lt else raise Not_found
               | _ -> raise Not_found) in
             simplify_tree (Relation(r,a,Variant(e,lt)))
        (* | TripleRelation(r,a,s,t) ->
             let ls,lt = Xlist.fold l ([],[]) (fun (ls,lt) -> function
                 i,TripleRelation(r2,a2,s2,t2) -> if r = r2 && a = a2 then (i,s2) :: ls, (i,t2) :: lt else raise Not_found
               | _ -> raise Not_found) in
             simplify_tree (TripleRelation(r,a,Variant(e,ls),Variant(e,lt))) *)
        | Tuple tl ->
(*             print_endline ("V3: " ^ LCGstringOf.linear_term 0 (Variant l));  *)
            let n = Xlist.size tl in
            let lt = Xlist.fold l [] (fun lt -> function
              i,Tuple tl -> if n = Xlist.size tl then (i,tl) :: lt else raise Not_found
            | _ -> raise Not_found) in
            let e = if e = "" then ENIAM_LCGreductions.get_variant_label () else e in
            let t = Tuple(transpose_tuple_variant e lt) in
(*             print_endline ("V4: " ^ LCGstringOf.linear_term 0 t); *)
            simplify_tree t
         | Dot -> if Xlist.fold l true (fun b -> function
              _,Dot -> b
            | _ -> false) then Dot else raise Not_found
         | _ -> raise Not_found)
      with Not_found -> Variant(e,l))
(*   Variant(e,Xlist.map l (fun (i,t) -> i, simplify_tree t)) *)
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("simplify_tree: " ^ ENIAMsemStringOf.linear_term 0 t)

let greater_simplify tree =
  let map = count_variant_labels StringQMap.empty tree in
  let tree = remove_variant_labels map tree in
  let tree = simplify_tree tree in
  let map = count_variant_labels StringQMap.empty tree in
  let tree = remove_variant_labels map tree in
  tree

(*let rec manage_quantification2 (quants,quant) = function
    Tuple l -> Xlist.fold l (quants,quant) manage_quantification2
  | Dot -> quants,quant
  | Val s -> quants,Tuple[Val s;quant]
  | t -> (Relation("Quantifier","",t)) :: quants,quant

let rec manage_quantification = function
    Node t -> Node{t with args=manage_quantification t.args}
  | Concept c ->
       let quants,quant = manage_quantification2 ([],Dot) c.c_quant in
       Concept{c with c_quant=quant; c_relations=manage_quantification (Tuple(c.c_relations :: quants))}
  | Context c -> Context{c with cx_contents=manage_quantification c.cx_contents; cx_relations=manage_quantification c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,manage_quantification t)
  | RevRelation(r,a,t) -> RevRelation(r,a,manage_quantification t)
  | SingleRelation r  -> SingleRelation r
  | AddRelation(t,r,a,s) -> AddRelation(manage_quantification t,r,a,manage_quantification s)
  (* | RemoveRelation t -> RemoveRelation(manage_quantification t) *)
  | Tuple l -> Tuple(Xlist.map l manage_quantification)
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, manage_quantification t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("manage_quantification: " ^ ENIAMsemStringOf.linear_term 0 t)*)

let simplify_gender2 = function
    Variant("",l) ->
      let l2 = List.sort compare (Xlist.rev_map l (function (_,Val s) -> s | _ -> raise Not_found)) in
      (match l2 with
          ["f"; "m1"; "m2"; "m3"; "n"] -> Dot
        | ["m1"; "m2"; "m3"] -> Val "m"
        | ["f"; "m2"; "m3"; "n"] -> Val "nmo"
        | ["pl"; "sg"] -> Dot
        | _ -> raise Not_found)
  | _ -> raise Not_found

let rec simplify_gender = function
    Concept c -> Concept{c with relations=simplify_gender c.relations; contents=simplify_gender c.contents(*c_quant=simplify_gender c.c_quant*)}
(*   | Context c -> Context{c with cx_contents=simplify_gender c.cx_contents; cx_relations=simplify_gender c.cx_relations} *)
  | Relation(r,a,t) -> Relation(r,a,simplify_gender t)
  | RevRelation(r,a,t) -> RevRelation(r,a,simplify_gender t)
  | SingleRelation r  -> SingleRelation(simplify_gender r)
  | Tuple l -> Tuple(Xlist.map l simplify_gender)
  | Variant(e,l) ->
      (try simplify_gender2 (Variant(e,l)) with Not_found ->
        Variant(e,Xlist.map l (fun (i,t) -> i, simplify_gender t)))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("simplify_gender: " ^ ENIAMsemStringOf.linear_term 0 t)

(***************************************************************************************)
(*
let rec validate_semantics_quant = function
    Val _ -> true
  | Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> b && validate_semantics_quant t)
  | Tuple l -> Xlist.fold l true (fun b t -> b && validate_semantics_quant t)
  | Dot -> true
  | t -> (*print_endline ("validate_semantics_quant: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

let rec validate_semantics_sense = function
    Val _ -> true
  | Dot -> true
  | t -> (*print_endline ("validate_semantics_sense: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

let rec validate_semantics_rel_name = function
    Val _ -> true
  | t -> (*print_endline ("validate_semantics_rel_name: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

let rec validate_semantics = function
    Context c -> validate_semantics_sense c.cx_sense && validate_semantics_contents c.cx_contents && validate_semantics_relations c.cx_relations
  | Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> b && validate_semantics t)
  | t -> (*print_endline ("validate_semantics: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

and validate_semantics_relations = function
    SingleRelation r -> validate_semantics_rel_name r
  | Relation(r,a,t) -> validate_semantics_rel_name r && validate_semantics_rel_name a && validate_semantics_concept t
  | RevRelation(r,a,t) -> validate_semantics_rel_name r && validate_semantics_rel_name a && validate_semantics_concept t
  | Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> b && validate_semantics_relations t)
  | Tuple l -> Xlist.fold l true (fun b t -> b && validate_semantics_relations t)
  | Dot -> true
  | t -> (*print_endline ("validate_semantics_relations: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

and validate_semantics_concept = function
    Concept c -> validate_semantics_sense c.c_sense && validate_semantics_sense c.c_name && validate_semantics_quant c.c_quant && validate_semantics_relations c.c_relations
  | Context c -> validate_semantics_sense c.cx_sense && validate_semantics_contents c.cx_contents && validate_semantics_relations c.cx_relations
  | Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> b && validate_semantics_concept t)
  | t -> (*print_endline ("validate_semantics_concept: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

and validate_semantics_contents = function
    Concept c -> validate_semantics_concept (Concept c)
  | Context c -> validate_semantics_concept (Context c)
  | Variant(e,l) -> Xlist.fold l true (fun b (_,t) -> b && validate_semantics_contents t)
  | Tuple l -> Xlist.fold l true (fun b t -> b && validate_semantics_contents t)
  | t -> (*print_endline ("validate_semantics_contents: " ^ ENIAMsemStringOf.linear_term 0 t);*) false

(***************************************************************************************)

let rec find_multiple_variants v m = function
    Concept c ->
      let v,m = find_multiple_variants v m c.c_quant in
      let v,m = find_multiple_variants v m c.c_relations in
      v,m
  | Context c ->
      let v,m = find_multiple_variants v m c.cx_contents in
      let v,m = find_multiple_variants v m c.cx_relations in
      v,m
  | Relation(r,a,t) -> find_multiple_variants v m t
  | RevRelation(r,a,t) -> find_multiple_variants v m t
  | SingleRelation r  -> v,m
  | Tuple l ->
      Xlist.fold l (v,m) (fun (v,m) t ->
        find_multiple_variants v m t)
  | Variant(e,l) ->
      let m = if StringSet.mem v e then StringMap.add m e (Xlist.map l fst) else m in
      let v = StringSet.add v e in
      let vl,m = Xlist.fold l ([],m) (fun (vl,m) (i,t) ->
        let v2,m = find_multiple_variants v m t in
        v2 :: vl,m) in
      Xlist.fold vl v StringSet.union, m
  | Dot -> v,m
  | Val s -> v,m
  | t -> failwith ("find_multiple_variants: " ^ ENIAMsemStringOf.linear_term 0 t)

type variant_structure =
    C of variant_structure * variant_structure
  | E
  | T of variant_structure list
  | V of string * int * (string * int * variant_structure) list

let rec string_of_variant_structure = function
    C(s,t) -> sprintf "C(%s,%s)" (string_of_variant_structure s) (string_of_variant_structure t)
  | E -> "E"
  | T l -> sprintf "T[%s]" (String.concat ";" (Xlist.map l string_of_variant_structure))
  | V(e,n,l) ->
      sprintf "V(%s,%d,[%s])" e n (String.concat ";" (Xlist.map l (fun (i,n,t) ->
        sprintf "%s,%d,%s" i n (string_of_variant_structure t))))

let rec create_variant_structure = function
    Concept c -> (*create_variant_structure c.c_relations*)
      let n,s = create_variant_structure c.c_quant in
      let m,t = create_variant_structure c.c_relations in
      m*n,C(s,t)
  | Context c ->
      let n,s = create_variant_structure c.cx_contents in
      let m,t = create_variant_structure c.cx_relations in
      m*n,C(s,t)
  | Relation(r,a,t) -> create_variant_structure t
  | RevRelation(r,a,t) -> create_variant_structure t
  | SingleRelation r  -> 1,E
  | Tuple l ->
      let n,l = Xlist.fold l (1,[]) (fun (n,l) t ->
        let m,v = create_variant_structure t in
        n*m,v :: l) in
      n,T(List.rev l)
  | Variant(e,l) ->
      let n,l = Xlist.fold l (0,[]) (fun (n,l) (i,t) ->
        let m,v = create_variant_structure t in
        n+m,(i,m,v) :: l) in
      n,V(e,n,List.rev l)
  | Dot -> 1,E
  | Val s -> 1,E
  | t -> failwith ("create_variant_structure: " ^ ENIAMsemStringOf.linear_term 0 t)

let rec get_all_variants = function
    Concept c ->
(*      let l = get_all_variants c.c_relations in
      Xlist.map l (fun t -> Concept{c with c_relations=t})*)
      let lq = get_all_variants c.c_quant in
      let lr = get_all_variants c.c_relations in
      List.flatten (Xlist.map lq (fun q ->
        Xlist.map lr (fun r ->
          Concept{c with c_relations=r; c_quant=q})))
  | Context cx ->
      let lc = get_all_variants cx.cx_contents in
      let lr = get_all_variants cx.cx_relations in
      List.flatten (Xlist.map lc (fun c ->
        Xlist.map lr (fun r ->
          Context{cx with cx_contents=c; cx_relations=r})))
  | Relation(r,a,t) ->
      let l = get_all_variants t in
      Xlist.map l (fun t -> Relation(r,a,t))
  | RevRelation(r,a,t) ->
      let l = get_all_variants t in
      Xlist.map l (fun t -> RevRelation(r,a,t))
  | SingleRelation r  -> [SingleRelation r]
  | Tuple l ->
      let ll = Xlist.multiply_list (Xlist.map l get_all_variants) in
      Xlist.map ll (fun l -> Tuple l)
  | Variant(e,l) ->
      List.rev (Xlist.fold l [] (fun l (_,t) -> get_all_variants t @ l))
  | Dot -> [Dot]
  | Val s -> [Val s]
  | t -> failwith ("get_all_variants: " ^ ENIAMsemStringOf.linear_term 0 t)

let _ = Random.self_init ()

let rec draw_variant2 k = function
    (i2,m,v) :: lv, (i,t) :: l ->
      if i2 <> i then failwith "draw_variant2" else
      if k < m then v,t else
      draw_variant2 (k - m) (lv,l)
  | _ -> failwith "draw_variant2"

let rec draw_variant = function
(*     s,Concept c -> Concept{c with c_relations=draw_variant (s,c.c_relations)} *)
    C(s,t),Concept c -> Concept{c with c_quant=draw_variant (s,c.c_quant); c_relations=draw_variant (t,c.c_relations)}
  | C(s,t),Context c -> Context{c with cx_contents=draw_variant (s,c.cx_contents); cx_relations=draw_variant (t,c.cx_relations)}
  | s,Relation(r,a,t) -> Relation(r,a,draw_variant (s,t))
  | s,RevRelation(r,a,t) -> RevRelation(r,a,draw_variant (s,t))
  | E,SingleRelation r  -> SingleRelation r
  | T lv,Tuple l -> Tuple(List.rev (Xlist.fold2 lv l [] (fun l s t -> (draw_variant (s,t)) :: l)))
  | V(e2,n,lv),Variant(e,l) ->
      if e <> e2 then failwith "draw_variant" else
      let k = Random.int n in
      let s,t = draw_variant2 k (lv,l) in
      draw_variant (s,t)
  | E,Dot -> Dot
  | E,Val s -> Val s
  | s,t -> (*print_endline ("draw_variant: " ^ ENIAMsemStringOf.linear_term 0 t);*) failwith ("draw_variant: " ^ string_of_variant_structure s)

let rec get_some_variants chosen = function
    Concept c -> (* FIXME: czy pozostałe atrybuty można pominąć? *)
      let q = get_some_variants chosen c.c_quant in
      let r = get_some_variants chosen c.c_relations in
      Concept{c with c_relations=r; c_quant=q}
  | Context cx ->
      let c = get_some_variants chosen cx.cx_contents in
      let r = get_some_variants chosen cx.cx_relations in
      Context{cx with cx_contents=c; cx_relations=r}
  | Relation(r,a,t) -> Relation(r,a,get_some_variants chosen t)
  | RevRelation(r,a,t) -> RevRelation(r,a,get_some_variants chosen t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(Xlist.map l (get_some_variants chosen))
  | Variant(e,l) ->
      if StringMap.mem chosen e then
        let t = try Xlist.assoc l (StringMap.find chosen e) with Not_found -> failwith "get_some_variants" in
        get_some_variants chosen t
      else Variant(e,Xlist.map l (fun (i,t) -> i,get_some_variants chosen t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("get_some_variants: " ^ ENIAMsemStringOf.linear_term 0 t)

let get_all_multiple_variants t mv =
  let ll = StringMap.fold mv [] (fun ll e l ->
    (Xlist.map l (fun i -> e,i)) :: ll) in
  if ll = [] then [t] else
  Xlist.fold (Xlist.multiply_list ll) [] (fun variants l ->
    let chosen = Xlist.fold l StringMap.empty (fun chosen (e,i) -> StringMap.add chosen e i) in
    get_some_variants chosen t :: variants)

(*let rec merge_multiple_variant l = function
    [] -> l
  | x :: rev -> merge_multiple_variant (x :: l) rev

let rec select_multiple_variant rev k = function
    [] -> failwith "select_multiple_variant"
  | x :: l -> if k=0 then x, merge_multiple_variant rev l else select_multiple_variant (x :: rev) (k-1) l*)

let rec select_multiple_variant k = function
    [] -> failwith "select_multiple_variant"
  | x :: l -> if k=0 then x else select_multiple_variant (k-1) l

let draw_multiple_variant k t mv =
  let ll = StringMap.fold mv [] (fun ll e l ->
    (Xlist.map l (fun i -> e,i)) :: ll) in
  let mv = Int.fold 1 k [] (fun mv _ ->
    let variants = Xlist.fold ll [] (fun variants l ->
      let k = Random.int (Xlist.size l) in
      select_multiple_variant k l :: variants) in
    variants :: mv) in
  Xlist.fold mv [] (fun variants l ->
    let chosen = Xlist.fold l StringMap.empty (fun chosen (e,i) -> StringMap.add chosen e i) in
    get_some_variants chosen t :: variants)

let rec draw_multiple_variant2_rec k = function
    [] -> failwith "draw_multiple_variant2_rec"
  | (n,s,t) :: l -> if k < n then s,t else draw_multiple_variant2_rec (k-n) l

let draw_multiple_variant2 sum_n mv =
  let k = Random.int sum_n in
  draw_multiple_variant2_rec k mv

let draw_trees max_n t =
  let _,multiple_variants = find_multiple_variants StringSet.empty StringMap.empty t in
  let mo = StringMap.fold multiple_variants 1 (fun mo _ l -> mo * Xlist.size l) in
(*   printf "|multiple_variants|=%d |mo|=%d\n%!" (StringMap.size multiple_variants) mo; *)
  let multiple_variants =
    if mo <= 100 then get_all_multiple_variants t multiple_variants else
      draw_multiple_variant 100 t multiple_variants in
(*   printf "|multiple_variants|=%d |mo|=%d\n%!" (Xlist.size multiple_variants) mo; *)
  let multiple_variants = Xlist.map multiple_variants (fun t ->
    let n,s = create_variant_structure t in
    n,s,t) in
  let sum_n = Xlist.fold multiple_variants 0 (fun sum_n (n,_,_) -> sum_n + n) in
(*  print_endline (ENIAMsemStringOf.linear_term 0 t);
  print_endline (string_of_variant_structure s);*)
  if sum_n <= max_n then
    List.flatten (Xlist.rev_map multiple_variants (fun (n,s,t) ->
      get_all_variants t)) else
  Int.fold 1 max_n [] (fun l _ ->
    let s,t = draw_multiple_variant2 sum_n multiple_variants in
    (draw_variant (s,t)) :: l)

(* FIXME!: założenie o jednokrotnym występowaniu wagi nie jest prawdziwe np. dla zdania: "Łódź wyprzedza statek." *)
*)
