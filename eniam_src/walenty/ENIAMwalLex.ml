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

let position morfs = {empty_position with morfs=morfs}

let rec split_elexeme = function
    Lexeme s -> [],[Lexeme s]
  | XOR l ->
    let genders,l = Xlist.fold l ([],[]) (fun (genders,lexs) lex ->
        let gender,lex = split_elexeme lex in
        gender @ genders, lex @ lexs) in
    genders,[XOR(List.rev l)]
  | ORconcat l ->
    let genders,l = Xlist.fold l ([],[]) (fun (genders,lexs) lex ->
        let gender,lex = split_elexeme lex in
        gender @ genders, lex @ lexs) in
    genders,[ORconcat(List.rev l)]
  | ORcoord l ->
    let genders,l = Xlist.fold l ([],[]) (fun (genders,lexs) lex ->
        let gender,lex = split_elexeme lex in
        gender @ genders, lex @ lexs) in
    genders,[ORcoord(List.rev l)]
  | Elexeme gender -> [gender],[]

let rec get_lexemes = function
    Lexeme s -> [s]
  | ORconcat l -> List.flatten (Xlist.map l get_lexemes)
  | ORcoord l -> List.flatten (Xlist.map l get_lexemes)
  | XOR l -> List.flatten (Xlist.map l get_lexemes)
  | Elexeme gender -> failwith "get_lexemes"

let rec remove_list set = function
    [] -> []
  | s :: l -> if Xlist.mem set s then remove_list set l else s :: (remove_list set l)

let rec check_lexemes_morfs l = function
    LexPhrase(lexs,(_,schema)) ->
              let l = Xlist.fold lexs l (fun l (_,lex) ->
                  remove_list (get_lexemes lex) l) in
              check_lexemes_schema l schema
  | _ -> l

and check_lexemes_schema l schema =
  Xlist.fold schema l (fun l s ->
      Xlist.fold s.morfs l check_lexemes_morfs)

let add_refl_restr (restr,schema) =
    (match restr with
      Natr -> Ratr
    | Atr1 -> Atr
    | Atr -> Atr
    | Ratr1 -> Ratr
    | Ratr -> Ratr
    | Ratrs -> Ratrs
    | NoRestr -> failwith "add_refl_restr"),
    position [LexPhrase([QUB,Lexeme "się"],(Natr,[]))] :: schema

let rec expand_lexicalizations_schema schema =
  Xlist.map schema (fun s ->
      {s with morfs=expand_lexicalizations_morfs s.morfs})

and expand_lexicalizations_morfs morfs = (* uproszczenie polegające na zezwoleniu na koordynację przy zwiększaniu ilości LexPhrase *)
  List.flatten (Xlist.map morfs (fun morf ->
      let morf = match morf with
          LexPhrase(pos_lex,(restr,schema)) -> LexPhrase(pos_lex,(restr,expand_lexicalizations_schema schema))
        | morf -> morf in
      match morf with
(* | Phrase(PrepNumP(prep,case)) -> [LexPhrase([PREP case,Lexeme prep],(Ratrs,[position(*2*) [Phrase(NumP(case))]]))] *)
      | Phrase(PrepNumP(prep,case)) -> [Phrase(PrepNP(prep,case))] (* FIXME: celowe uproszczenie *)
      | LexPhrase([PREP pcase,plex;SUBST(n,c),slex],(Atr1,[{morfs=[LexPhrase([QUB,_],_)]} as s])) ->
        (*            print_endline (ENIAMwalStringOf.morf morf);  *)
        [LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([SUBST(n,c),slex],(Natr,[]))]]));
         LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([SUBST(n,c),slex],(Natr,[]))];s(*{s with dir=Backward}*)]))]
      | LexPhrase([PREP(pcase),plex;SUBST(n,c),slex],(Atr1,[{morfs=[LexPhrase([ADV _,_],_)]} as s])) ->
        (*            print_endline (ENIAMwalStringOf.morf morf);  *)
        [LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([SUBST(n,c),slex],(Natr,[]))]]));
         LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([SUBST(n,c),slex],(Natr,[]))];s(*{s with dir=Backward}*)]))]
      | LexPhrase([PREP pcase,plex;SUBST(n,c),slex],(Ratr1,[{morfs=[LexPhrase([ADV _,_],_)]} as s])) ->
        (*            print_endline (ENIAMwalStringOf.morf morf);  *)
        [LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([SUBST(n,c),slex],(Natr,[]))];s(*{s with dir=Backward}*)]))]
      | LexPhrase([PREP pcase,plex;pos,lex],restr) ->
        [LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([pos,lex],restr)]]))]
      | LexPhrase([PREP pcase,plex;NUM(c,g),nlex;pos,lex],restr) ->
        let genders,lexs = split_elexeme lex in
        Xlist.map genders (fun gender ->
            LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([NUM(c,gender),nlex],(Ratrs,[(*num*)position [Phrase Null(*Pro*)]]))]]))) @ (*FIXME*)
        Xlist.map lexs (fun lex ->
            LexPhrase([PREP pcase,plex],(Ratrs,[position [LexPhrase([NUM(c,g),nlex],(Ratrs,[(*num*)position [LexPhrase([pos,lex],restr)]]))]])))
      | LexPhrase([NUM(c,g),nlex;pos,lex],restr) ->
        let genders,lexs = split_elexeme lex in
        Xlist.map genders (fun gender ->
            LexPhrase([NUM(c,gender),nlex],(Ratrs,[(*num*)position [Phrase Null(*Pro*)]]))) @
        Xlist.map lexs (fun lex ->
            LexPhrase([NUM(c,g),nlex],(Ratrs,[(*num*)position [LexPhrase([pos,lex],restr)]])))
      | LexPhrase([INF(a,n),lex;QUB,Lexeme "się"],restr) -> [LexPhrase([INF(a,n),lex],add_refl_restr restr)]
      | LexPhrase([COMP ctype,clex;pos,lex;QUB,Lexeme "się"],restr) ->
        if Xlist.size (check_lexemes_schema (get_lexemes clex) (snd restr)) = 0 then
          [LexPhrase([pos,lex],add_refl_restr restr)]
        else [LexPhrase([COMP ctype,clex],(Ratrs,[(*std*)position (*Forward*) [LexPhrase([pos,lex],add_refl_restr restr)]]))]
      | LexPhrase([COMP ctype,clex;pos,lex],restr) ->
        if Xlist.size (check_lexemes_schema (get_lexemes clex) (snd restr)) = 0 then
          [LexPhrase([pos,lex],restr)]
        else [LexPhrase([COMP ctype,clex],(Ratrs,[(*std*)position (*Forward*) [LexPhrase([pos,lex],restr)]]))]
      | LexPhrase(_::_::_,_) -> failwith ("expand_lexicalizations_morfs: " ^ ENIAMwalStringOf.morf morf)
      | morf -> [morf]))

let winien = StringSet.of_list ["winien"; "rad"; "powinien"; "nierad"; "niekontent"; "kontent"; "gotów"]
let pred = StringSet.of_list ["żal"; "śmiech"; "znać"; "wstyd"; "wolno"; "widać"; "wiadomo";
"warto"; "trzeba"; "trza"; "słychać"; "szkoda"; "strach"; "stać"; "sposób"; "potrzeba"; "pora";
"podobna"; "niewiada"; "niepodobno"; "niepodobna"; "można"; "lża"; "lza"; "dziw"; "dość"; "dosyć";
"czuć"; "czas"; "brak"]

let get_pos lex = function
    SUBST _ ->
    (match lex with
       "ja" -> ["ppron12"]
     | "my" -> ["ppron12"]
     | "ty" -> ["ppron12"]
     | "wy" -> ["ppron12"]
     | "on" -> ["ppron3"]
     | "siebie" -> ["siebie"]
     | "się" -> ["qub"]
     | _ -> ["subst"])
  | PREP _ -> ["prep"]
  | NUM _ ->
    (try
       let _ = int_of_string lex in
       ["intnum"]
     with _ -> ["num"])
  | ADV _ -> ["adv"]
  | ADJ _ -> ["adj"]
  | GER _ -> ["ger"]
  | PPAS _ -> ["ppas"]
  | PACT _ -> ["pact"]
  | PERS _ -> if lex = "być" then ["fin";"praet";"bedzie"] else
      if StringSet.mem winien lex then ["winien"] else
      if StringSet.mem pred lex then ["pred"] else
      ["fin";"praet"](*;"impt";"imps"*)
  | INF _ -> ["inf"]
  | QUB -> ["qub"]
  | COMPAR -> ["compar"]
  | COMP _ -> ["comp"]
  | FIXED -> ["fixed"]
  | _ -> failwith "get_pos"

let map_pos lemma = function
    SUBST(number,case) ->
    (match lemma with
       "ja" -> PPRON12(number,case)
     | "my" -> PPRON12(number,case)
     | "ty" -> PPRON12(number,case)
     | "wy" -> PPRON12(number,case)
     | "on" -> PPRON3(number,case)
     | "siebie" -> SIEBIE case
     | "się" -> QUB
     | _ -> SUBST(number,case))
  | p -> p

let lex_id_counter = ref 0

let get_lex_id () =
  incr lex_id_counter;
  !lex_id_counter

(* FIXME: to trzeba będzie poprawić przy unlike coordination *)
(* FIXME: słownik pos wywołuje redundancję *)
(* FIXME: parametr refl z typu pos można przenieść do schematu *)
let rec extract_lex_entries (morfs,entries) = function
    LexPhrase([pos,lex],(Natr,[])) ->
    let lexemes = get_lexemes lex in
    let entries = Xlist.fold lexemes entries (fun entries lemma ->
        Xlist.fold (get_pos lemma pos) entries (fun entries pos2 ->
            (pos2,lemma,SimpleLexEntry(lemma,pos2)) :: entries)) in
            (* let entries = Xlist.fold lexemes entries (fun entries lemma ->
        Xlist.fold (get_pos lemma pos) entries (fun entries pos2 ->
            let entries2 = try StringMap.find entries pos2 with Not_found -> StringMap.empty in
            let entry = SimpleLexEntry(lemma,pos2) in
            let entries2 = StringMap.add_inc entries2 lemma (EntrySet.singleton entry) (fun set -> EntrySet.add set entry) in
            StringMap.add entries pos2 entries2)) in *)
    let morfs = Xlist.fold lexemes morfs (fun morfs lemma -> SimpleLexArg(lemma,map_pos lemma pos) :: morfs) in
    morfs,entries
  | LexPhrase([pos,lex],(restr,schema)) ->
    let id = get_lex_id () in
    let lexemes = get_lexemes lex in
    let schema,entries = extract_lex_entries_schema entries schema in
    let entries = Xlist.fold lexemes entries (fun entries lemma ->
        Xlist.fold (get_pos lemma pos) entries (fun entries pos2 ->
            (pos2,lemma,LexEntry(id,lemma,pos2,restr,schema)) :: entries)) in
    (* let entries = Xlist.fold lexemes entries (fun entries lemma ->
        Xlist.fold (get_pos lemma pos) entries (fun entries pos2 ->
            let entries2 = try StringMap.find entries pos2 with Not_found -> StringMap.empty in
            let entry = LexEntry(id,lemma,pos2,restr,schema) in
            let entries2 = StringMap.add_inc entries2 lemma (EntrySet.singleton entry) (fun set -> EntrySet.add set entry) in
            StringMap.add entries pos2 entries2)) in *)
    let morfs = Xlist.fold lexemes morfs (fun morfs lemma -> LexArg(id,lemma,map_pos lemma pos) :: morfs) in
    morfs,entries
  | LexPhrase _ as morf -> failwith ("extract_lex_entries: " ^ ENIAMwalStringOf.morf morf)
  | morf -> morf :: morfs, entries

and extract_lex_entries_schema entries schema =
  let schema,entries = Xlist.fold schema ([],entries) (fun (schema,entries) p ->
      let morfs,entries = Xlist.fold p.morfs ([],entries) extract_lex_entries in
      {p with morfs=List.rev morfs} :: schema, entries) in
  List.rev schema, entries

let extract_lex_entries_comprepnp entries compreps =
  Xlist.fold compreps entries (fun entries (clemma,morfs) ->
      Xlist.fold morfs entries (fun entries -> function
            LexPhrase([pos,lex],(Natr,[])) -> failwith "extract_lex_entries_comprepnp"
          | LexPhrase([pos,lex],(restr,schema)) ->
            let lexemes = get_lexemes lex in
            let schema,entries = extract_lex_entries_schema entries schema in
            Xlist.fold lexemes entries (fun entries lemma ->
                Xlist.fold (get_pos lemma pos) entries (fun entries pos2 ->
                    (pos2,lemma,ComprepNPEntry(clemma,restr,schema)) :: entries))
            (* Xlist.fold lexemes entries (fun entries lemma ->
                Xlist.fold (get_pos lemma pos) entries (fun entries pos2 ->
                    let entries2 = try StringMap.find entries pos2 with Not_found -> StringMap.empty in
                    let entry = ComprepNPEntry(clemma,restr,schema) in
                    let entries2 = StringMap.add_inc entries2 lemma (EntrySet.singleton entry) (fun set -> EntrySet.add set entry) in
                    StringMap.add entries pos2 entries2)) *)
          | _ -> failwith "extract_lex_entries_comprepnp"))

let rec expand_restr valence lexeme pos = function
    SimpleLexEntry(lemma,pos2) -> [SimpleLexEntry(lemma,pos2)]
  (* | LexEntry(id,lemma,pos2,Natr,[]) -> [LexEntry(id,lemma,pos2,NoRestr,[])] *)
  | LexEntry(id,lemma,pos2,Natr,_) -> failwith "expand_restr"
  | LexEntry(id,lemma,pos2,restr,[]) ->
    (* print_endline (lexeme ^ " " ^ pos); *)
    [LexEntry(id,lemma,pos2,restr,[])] (* FIXME *)
(*    (*       print_endline "expand_restr"; *)
    let frames = try StringMap.find (StringMap.find valence lexeme) pos
      with Not_found -> failwith ("expand_restr:" ^ lexeme ^ " " ^ pos) in
    (*      Printf.printf "%s %s %d\n" lexeme pos (Xlist.size frames);
            Xlist.iter frames (fun frame -> print_endline (ENIAMwalStringOf.frame lexeme frame));
            print_endline "";*)
    (if restr = Atr || restr = Atr1 then [LexEntry(id,lemma,pos2,NoRestr,[])] else []) @
    (Xlist.fold frames [] (fun frames -> function
           Frame(_,schema) ->
           let schema = remove_pro_args schema in
           if schema = [] then frames else
             (expand_restr valence lexeme pos (LexEntry(id,lemma,pos2,restr,schema))) @ frames
         | _ -> frames))*)
  | LexEntry(id,lemma,pos2,Atr,schema) ->
    let schema = Xlist.map schema (fun p -> {p with morfs=Phrase Null :: p.morfs}) in
    [LexEntry(id,lemma,pos2,NoRestr,schema)]
  | LexEntry(id,lemma,pos2,Atr1,schema) ->
    LexEntry(id,lemma,pos2,NoRestr,[]) :: (Xlist.map schema (fun x -> LexEntry(id,lemma,pos2,NoRestr,[x])))
  | LexEntry(id,lemma,pos2,Ratr,schema) ->
    let schemas = Xlist.map (Xlist.multiply_list (Xlist.map schema (fun x -> [[x];[]]))) List.flatten in
    Xlist.fold schemas [] (fun schemas schema ->
        if schema = [] then schemas else LexEntry(id,lemma,pos2,NoRestr,schema) :: schemas)
  | LexEntry(id,lemma,pos2,Ratr1,schema) ->
    Xlist.map schema (fun x -> LexEntry(id,lemma,pos2,NoRestr,[x]))
  | LexEntry(id,lemma,pos2,Ratrs,schema) -> [LexEntry(id,lemma,pos2,NoRestr,schema)]
  | LexEntry(id,lemma,pos2,NoRestr,_) -> failwith "expand_restr"
  (* | ComprepNPEntry(lemma,Natr,[]) -> [ComprepNPEntry(lemma,NoRestr,[])] *)
  | ComprepNPEntry(lemma,Natr,_) -> failwith "expand_restr"
  | ComprepNPEntry(lemma,restr,[]) as entry -> failwith ("expand_restr: " ^ ENIAMwalStringOf.lex_entry entry)
  | ComprepNPEntry(lemma,Atr,schema) ->
    let schema = Xlist.map schema (fun p -> {p with morfs=Phrase Null :: p.morfs}) in
    [ComprepNPEntry(lemma,NoRestr,schema)]
  | ComprepNPEntry(lemma,Atr1,schema) ->
    ComprepNPEntry(lemma,NoRestr,[]) :: (Xlist.map schema (fun x -> ComprepNPEntry(lemma,NoRestr,[x])))
  | ComprepNPEntry(lemma,Ratr,schema) ->
    let schemas = Xlist.map (Xlist.multiply_list (Xlist.map schema (fun x -> [[x];[]]))) List.flatten in
    Xlist.fold schemas [] (fun schemas schema ->
        if schema = [] then schemas else ComprepNPEntry(lemma,NoRestr,schema) :: schemas)
  | ComprepNPEntry(lemma,Ratr1,schema) ->
    Xlist.map schema (fun x -> ComprepNPEntry(lemma,NoRestr,[x]))
  | ComprepNPEntry(lemma,Ratrs,schema) -> [ComprepNPEntry(lemma,NoRestr,schema)]
  | ComprepNPEntry(lemma,NoRestr,_) -> failwith "expand_restr"
  (* | _ -> failwith "expand_restr" *)
