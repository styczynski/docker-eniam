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

let create_phrase_reqs s (reqs,noreqs) = function
  | PrepNP(prep,_) -> StringMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | PrepAdjP(prep,_) -> StringMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | PrepNCP(prep,_,_,_) -> StringMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | ComparP(prep,_) -> StringMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | FixedP(prep) -> StringMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | SimpleLexArg(lex,_) -> StringMap.add_inc reqs s (StringSet.singleton lex) (fun set -> StringSet.add set lex), noreqs
  | LexArg(_,lex,_) -> StringMap.add_inc reqs s (StringSet.singleton lex) (fun set -> StringSet.add set lex), noreqs
  | MorfId _ -> failwith "create_phrase_reqs"
  | _ -> reqs, StringSet.add noreqs s

let create_phrase_reqs2 s (reqs,noreqs) = function
  | PrepNP(prep,_) -> IntMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | PrepAdjP(prep,_) -> IntMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | PrepNCP(prep,_,_,_) -> IntMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | ComparP(prep,_) -> IntMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | FixedP(prep) -> IntMap.add_inc reqs s (StringSet.singleton prep) (fun set -> StringSet.add set prep), noreqs
  | SimpleLexArg(lex,_) -> IntMap.add_inc reqs s (StringSet.singleton lex) (fun set -> StringSet.add set lex), noreqs
  | LexArg(_,lex,_) -> IntMap.add_inc reqs s (StringSet.singleton lex) (fun set -> StringSet.add set lex), noreqs
  | MorfId _ -> failwith "create_phrase_reqs2"
  | _ -> reqs, IntSet.add noreqs s

let create_comprep_reqs entries =
  let reqs,noreqs,reqs2 = Entries.fold entries (StringMap.empty,StringSet.empty,StringMap.empty) (fun (reqs,noreqs,reqs2) _ lemma -> function
        ComprepNPEntry(s,NoRestr,[p]) ->
        let reqs,noreqs = Xlist.fold p.morfs (reqs,noreqs) (create_phrase_reqs s) in
        reqs,noreqs,StringMap.add_inc reqs2 s (StringSet.singleton lemma) (fun set -> StringSet.add set lemma)
      | ComprepNPEntry(s,NoRestr,_) -> reqs, StringSet.add noreqs s, reqs2
      | ComprepNPEntry _ -> failwith "create_comprep_reqs"
      | _ -> reqs,noreqs,reqs2) in
  StringMap.fold reqs StringMap.empty (fun reqs s l ->
      if StringSet.mem noreqs s then reqs else StringMap.add reqs s l),reqs2

let create_lexarg_reqs entries =
  let reqs,noreqs = Entries.fold entries (IntMap.empty,IntSet.empty) (fun (reqs,noreqs) _ _ -> function
        LexEntry(id,_,_,NoRestr,[p]) -> Xlist.fold p.morfs (reqs,noreqs) (create_phrase_reqs2 id)
      | LexEntry(id,_,_,NoRestr,_) -> reqs, IntSet.add noreqs id
      | _ -> reqs,noreqs) in
  IntMap.fold reqs IntMap.empty (fun reqs s l ->
      if IntSet.mem noreqs s then reqs else IntMap.add reqs s l)

let create_comprep_adjuncts comprep_reqs comprep_reqs2 =
  let map = StringMap.fold comprep_reqs2 StringMap.empty (fun map s set ->
      StringSet.fold set map (fun map lemma ->
          StringMap.add_inc map lemma [s] (fun l -> s :: l))) in
  StringMap.map map (fun l ->
      Xlist.map l (fun s -> s, try StringMap.find comprep_reqs s with Not_found -> StringSet.empty))

let comprep_reqs = ref StringMap.empty
let comprep_reqs2 = ref StringMap.empty
let lexarg_reqs = ref IntMap.empty
let comprep_adjuncts = ref StringMap.empty

let initialize () =
  let a,b = create_comprep_reqs !ENIAMwalParser.entries in
  comprep_reqs := a;
  comprep_reqs2 := b;
  lexarg_reqs := create_lexarg_reqs !ENIAMwalParser.entries;
  comprep_adjuncts := create_comprep_adjuncts !comprep_reqs !comprep_reqs2;
  ()

let select_comprep_adjuncts lexemes =
  StringSet.fold lexemes [] (fun l lemma ->
      try
        Xlist.fold (StringMap.find !comprep_adjuncts lemma) l (fun l (s,reqs) ->
            (* Printf.printf "%s: %s: %s\n" lemma s (String.concat " " (StringSet.to_list reqs)); *)
          if StringSet.is_empty reqs ||
             not (StringSet.is_empty (StringSet.intersection reqs lexemes)) then s :: l else l)
      with Not_found -> l)

(* FIXME: trzeba zanalizować interację tej procedury z Pro w schemacie w wersji z walentym i z semantyką dziedzinową *)
let set_necessary pos schema =
  Xlist.map schema (fun p ->
    let nec =
      if p.gf = ADJUNCT then Opt else
      if Xlist.fold p.morfs false (fun b -> function
          SimpleLexArg _ -> true
        | LexArg _ -> true
        | FixedP _ -> true
        | _ -> b) then Req else
      if p.gf <> SUBJ && p.cr = [] (*&& p.ce = []*) then Opt else
      if p.gf = SUBJ && pos = "impt" then ProNG else
      if p.gf = SUBJ && pos = "pact" then Opt else
      if p.gf = OBJ && pos = "ppas" then Opt else
      if Xlist.fold p.morfs false (fun b -> function
          NP NomAgr -> true
        | NCP(NomAgr,_,_) -> true
        | E (NCP(NomAgr,CompTypeUndef,CompUndef)) -> true
        | E (NP(NomAgr)) -> true
        | Pro -> true
        | ProNG -> true (* czy to jest potrzebne? *)
        | _ -> b) then ProNG else Pro in
    {p with is_necessary=nec})

exception ImpossibleSchema

let rec reduce_comp test_lexemes = function
    Comp s -> if test_lexemes s then Comp s else raise Not_found
  | Zeby -> if test_lexemes "żeby" || test_lexemes "że" then Zeby else raise Not_found
  | Gdy -> if test_lexemes "gdy" || test_lexemes "gdyby" then Gdy else raise Not_found
  | CompUndef -> failwith "reduce_comp"

let reduce_phrase (test_comprep_reqs,test_comprep_reqs2,test_lexarg_reqs,test_lexemes) = function
  | PrepNP(prep,case) as phrase -> if test_lexemes prep then phrase else raise Not_found
  | PrepAdjP(prep,case) as phrase -> if test_lexemes prep then phrase else raise Not_found
  | ComprepNP(prep) as phrase  -> if test_comprep_reqs prep && test_comprep_reqs2 prep then phrase else raise Not_found
  | ComparP(prep,case) as phrase  -> if test_lexemes prep then phrase else raise Not_found
  | CP(ctype,comp) -> CP(ctype,reduce_comp test_lexemes comp)
  | NCP(case,ctype,comp) -> if test_lexemes "to" then NCP(case,ctype,reduce_comp test_lexemes comp) else raise Not_found
  | PrepNCP(prep,case,ctype,comp) -> if test_lexemes prep && test_lexemes "to" then PrepNCP(prep,case,ctype,reduce_comp test_lexemes comp) else raise Not_found
  | SimpleLexArg(lemma,_) as phrase  -> if test_lexemes lemma then phrase else raise Not_found
  | LexArg(id,lemma,_) as phrase  -> if test_lexemes lemma && test_lexarg_reqs id then phrase else raise Not_found
  | FixedP lemma as phrase  -> if test_lexemes lemma then phrase else raise Not_found
  | phrase -> phrase

let rec reduce_morfs tests = function
    [] -> []
  | morf :: l -> (try [reduce_phrase tests morf]
                  with Not_found -> []) @ reduce_morfs tests l

let rec reduce_schema2 tests = function
    [] -> []
  | s :: l ->
    let morfs = reduce_morfs tests s.morfs in
    if morfs = [] then reduce_schema2 tests l else
      {s with morfs=morfs} :: reduce_schema2 tests l

let rec reduce_schema tests = function
    [] -> []
  | s :: l ->
    let morfs = reduce_morfs tests s.morfs in
    if morfs = [] then raise ImpossibleSchema else
      {s with morfs=morfs} :: reduce_schema tests l

let reduce_entries lexemes entries =
  let lexemes = StringSet.add lexemes "" in
  StringMap.map entries (fun entries ->
      StringSet.fold lexemes StringMap.empty (fun reduced lemma ->
          try StringMap.add reduced lemma (StringMap.find entries lemma)
          with Not_found -> reduced))

let merge_schema phrases schema =
  Xlist.map schema (fun p ->
      let morfs = List.flatten (Xlist.map p.morfs (function
            MorfId id -> (try IntMap.find phrases id with Not_found -> failwith "merge_schema")
          | _ -> failwith "merge_schema")) in
      {p with morfs=morfs})

let merge_entries phrases entries =
  Entries.map entries (fun _ _ (opinion,neg,pred,aspect,schema) ->
      opinion,neg,pred,aspect,merge_schema phrases schema)

let merge_entries_conn phrases senses entries =
  Entries.map entries (fun _ _ (sopinion,fopinion,sense_ids,neg,pred,aspect,schema) ->
      let senses = Xlist.map sense_ids (fun id ->
          try IntMap.find senses id with Not_found -> failwith "merge_entries_conn") in
      sopinion,fopinion,senses,neg,pred,aspect,merge_schema phrases schema)

let create_tests comprep_reqs comprep_reqs2 lexarg_reqs lexemes =
  let lexemes = StringSet.add (StringSet.add lexemes "_") "" in
  (fun s ->
     if StringMap.mem comprep_reqs s then
       not (StringSet.is_empty (StringSet.intersection (StringMap.find comprep_reqs s) lexemes))
     else true),
  (fun s ->
     if StringMap.mem comprep_reqs2 s then
       not (StringSet.is_empty (StringSet.intersection (StringMap.find comprep_reqs2 s) lexemes))
     else failwith "create_tests"),
  (fun s ->
     if IntMap.mem lexarg_reqs s then
       not (StringSet.is_empty (StringSet.intersection (IntMap.find lexarg_reqs s) lexemes))
     else true),
  StringSet.mem lexemes


let select_entries_full phrases entries schemata connected senses comprep_reqs comprep_reqs2 lexarg_reqs lexemes =
  let tests = create_tests comprep_reqs comprep_reqs2 lexarg_reqs lexemes in
  let entries = reduce_entries lexemes entries in
  let schemata = reduce_entries lexemes schemata in
  let connected = reduce_entries lexemes connected in
  let schemata = merge_entries phrases schemata in
  let entries = Entries.flatten_map entries (fun _ _ entry ->
    try (match entry with
          | LexEntry(id,lemma,pos,NoRestr,schema) -> [LexEntry(id,lemma,pos,NoRestr,reduce_schema tests schema)]
          | ComprepNPEntry(s,NoRestr,schema) -> [ComprepNPEntry(s,NoRestr,reduce_schema tests schema)]
          |  _ -> [entry])
    with ImpossibleSchema -> []) in
  let schemata = Entries.map schemata (fun _ _ (opinion,neg,pred,aspect,schema) ->
      opinion,neg,pred,aspect,reduce_schema2 tests schema) in
  let connected = merge_entries_conn phrases senses connected in
  let connected = Entries.map connected (fun _ _ (sopinion,fopinion,sense_ids,neg,pred,aspect,schema) ->
      sopinion,fopinion,sense_ids,neg,pred,aspect,reduce_schema2 tests schema) in
  entries,schemata,connected

let select_all_entries phrases entries schemata connected senses =
  let schemata = merge_entries phrases schemata in
  let connected = merge_entries_conn phrases senses connected in
  entries,schemata,connected

let select_entries lexemes =
  select_entries_full !ENIAMwalParser.phrases !ENIAMwalParser.entries !ENIAMwalParser.schemata
    !ENIAMwalParser.connected !ENIAMwalParser.senses !comprep_reqs !comprep_reqs2 !lexarg_reqs lexemes

(* let entries,schemata,connected =
  (* let lexemes = StringSet.of_list ["Ala"; "ma"; "kot"] in *)
  let lexemes = StringSet.of_list ["dorastać"; "dorobić"; "po"; "bok"; "na"] in
  select_entries ENIAMwalParser.phrases ENIAMwalParser.entries ENIAMwalParser.schemata
    ENIAMwalParser.connected ENIAMwalParser.senses comprep_reqs comprep_reqs2 lexarg_reqs lexemes *)

(* let _ =
  StringMap.iter comprep_reqs (fun s set ->
      Printf.printf "%s: %s\n" s (String.concat " " (StringSet.to_list set))) *)

(* let _ =
  StringMap.iter comprep_reqs2 (fun s set ->
      Printf.printf "%s: %s\n" s (String.concat " " (StringSet.to_list set))) *)

(* let _ =
  IntMap.iter lexarg_reqs (fun s set ->
      Printf.printf "%d: %s\n" s (String.concat " " (StringSet.to_list set))) *)

(* let _ =
  Entries.iter entries (fun pos lemma entry ->
      Printf.printf "%s\t%s\t%s\n" pos lemma (ENIAMwalStringOf.lex_entry entry));
  Entries.iter schemata (fun pos lemma (_,_,_,_,schema) ->
      Printf.printf "%s\t%s\t%s\n" pos lemma (ENIAMwalStringOf.schema schema));
  Xlist.iter (Entries.find ENIAMwalParser.schemata "verb" "dorobić") (fun (_,_,_,_,schema) ->
      let schema = merge_schema ENIAMwalParser.phrases schema in
      Printf.printf "%s\n" (ENIAMwalStringOf.schema schema));
  Xlist.iter (Entries.find ENIAMwalParser.schemata "verb" "dorastać") (fun (_,_,_,_,schema) ->
      let schema = merge_schema ENIAMwalParser.phrases schema in
      Printf.printf "%s\n" (ENIAMwalStringOf.schema schema));
  () *)
