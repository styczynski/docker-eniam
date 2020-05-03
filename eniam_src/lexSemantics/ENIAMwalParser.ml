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

type token =
    Text of string
  | Paren of token list
  | Bracet of token list
  | SqBra of token list
  | LParen | RParen | LBracet | RBracet | LSqBra | RSqBra
  | Semic | Plus | Comma | Quot | Colon

let rec find_brackets = function
    LParen :: l ->
        let found,l = find_rbracket RParen [] l in
        (Paren found) :: (find_brackets l)
  | LBracet :: l ->
        let found,l = find_rbracket RBracet [] l in
        (Bracet found) :: (find_brackets l)
  | LSqBra :: l ->
        let found,l = find_rbracket RSqBra [] l in
        (SqBra found) :: (find_brackets l)
  | s :: l -> s :: (find_brackets l)
  | [] -> []

and find_rbracket bracket rev = function
    LParen :: l ->
        let found,l = find_rbracket RParen [] l in
        find_rbracket bracket (Paren found :: rev) l
  | LBracet :: l ->
        let found,l = find_rbracket RBracet [] l in
        find_rbracket bracket (Bracet found :: rev) l
  | LSqBra :: l ->
        let found,l = find_rbracket RSqBra [] l in
        find_rbracket bracket (SqBra found :: rev) l
  | RParen :: l -> if bracket = RParen then List.rev rev, l else failwith "find_rbracket"
  | RBracet :: l -> if bracket = RBracet then List.rev rev, l else failwith "find_rbracket"
  | RSqBra :: l -> if bracket = RSqBra then List.rev rev, l else failwith "find_rbracket"
  | s :: l -> find_rbracket bracket (s :: rev) l
  | [] -> failwith "find_rbracket"

let split_text schema =
  find_brackets (Xlist.map (Str.full_split (Str.regexp "\\]\\|\\+\\|{\\|}\\|(\\|)\\|,\\|;\\|:\\|'\\|\\[") schema) (function
      Str.Text s -> Text s
    | Str.Delim "(" -> LParen
    | Str.Delim ")" -> RParen
    | Str.Delim "{" -> LBracet
    | Str.Delim "}" -> RBracet
    | Str.Delim "[" -> LSqBra
    | Str.Delim "]" -> RSqBra
    | Str.Delim ";" -> Semic
    | Str.Delim ":" -> Colon
    | Str.Delim "+" -> Plus
    | Str.Delim "," -> Comma
    | Str.Delim "'" -> Quot
    | _ -> failwith "parse_text"))

let rec string_of_token = function
    Text s -> s
  | Paren l -> "(" ^ String.concat "" (Xlist.map l string_of_token) ^ ")"
  | Bracet l -> "{" ^ String.concat "" (Xlist.map l string_of_token) ^ "}"
  | SqBra l -> "[" ^ String.concat "" (Xlist.map l string_of_token) ^ "]"
  | LParen -> "("
  | RParen -> ")"
  | LBracet -> "{"
  | RBracet -> "}"
  | LSqBra -> "["
  | RSqBra -> "]"
  | Semic -> ";"
  | Colon -> ":"
  | Plus -> "+"
  | Comma -> ","
  | Quot  -> "'"

let string_of_token_list l =
  String.concat "" (Xlist.map l string_of_token)

let rec split_symbol symb rev = function
    [] -> [List.rev rev](*failwith "split_symbol"*)
  | s :: l ->
      if s = symb then
        if l = [] then (*[List.rev rev]*)failwith ("split_symbol: " ^ string_of_token symb)
        else (List.rev rev) :: (split_symbol symb [] l)
      else split_symbol symb (s :: rev) l

let parse_case = function
      [Text "nom"] -> Case "nom"
    | [Text "gen"] -> Case "gen"
    | [Text "dat"] -> Case "dat"
    | [Text "acc"] -> Case "acc"
    | [Text "inst"] -> Case "inst"
    | [Text "loc"] -> Case "loc"
    | [Text "str"] -> Str
    | [Text "pred"] -> Case "pred"
    | [Text "part"] -> Part
    | [Text "postp"] -> Case "postp"
    | [Text "agr"] -> CaseAgr
    | [Text "_"] -> CaseUndef
    | l -> failwith ("parse_case: " ^ string_of_token_list l)

let parse_number = function
      [Text "sg"] -> Number "sg"
    | [Text "pl"] -> Number "pl"
    | [Text "agr"] -> NumberAgr
    | [Text "_"] -> NumberUndef
    | l -> failwith ("parse_number: " ^ string_of_token_list l)

let parse_gender = function
      [Text "m1"] -> Gender "m1"
    | [Text "m3"] -> Gender "m3"
    | [Text "n"] -> Gender "n"(*Genders["n1";"n2"]*)
    | [Text "f"] -> Gender "f"
    | [Text "m1.n"] -> Genders["m1";"n"(*"n1";"n2"*)]
    | [Text "m1.n1.n2"] -> Genders["m1";"n"(*"n1";"n2"*)]
    | [Text "n1.n2"] -> Gender "n"(*Genders["n1";"n2"]*)
    | [Text "_"] -> GenderUndef
    | [Text "agr"] -> GenderAgr
    | l -> failwith ("parse_gender: " ^ string_of_token_list l)

let parse_grad = function
      [Text "pos"] -> Grad "pos"
    | [Text "com"] -> Grad "com"
    | [Text "sup"] -> Grad "sup"
    | [Text "_"] -> GradUndef
    | l -> failwith ("parse_grad: " ^ string_of_token_list l)

let parse_aspect = function
      [Text "perf"] -> Aspect "perf"
    | [Text "imperf"] -> Aspect "imperf"
    | [Text "_"] -> AspectUndef
    (* | [Text ""] -> AspectNA *)
    | l -> failwith ("parse_aspect: " ^ string_of_token_list l)

let parse_negation = function
    [Text "_"] -> NegationUndef
  | [Text "neg"] -> Negation
  | [Text "aff"] -> Aff
  (* | [Text ""] -> NegationNA *)
  | l -> failwith ("parse_negation: " ^ string_of_token_list l)

(* let parse_refl = function
      (* [] -> ReflEmpty
    | [Text "się"] -> ReflSie
    | [Text ""] -> ReflEmpty
    | [Text "false"] -> ReflEmpty
    | [Text "true"] -> ReflSie *)
  | [Text "nosię"] -> ReflFalse
  | [Text "się"] -> ReflTrue
  | l -> failwith ("parse_refl: " ^ string_of_token_list l) *)

let parse_ctype = function
    [Text "int"] -> Int
  | [Text "rel"] -> Rel
  | [Text "_"] -> CompTypeUndef
  | l -> failwith ("parse_ctype: " ^ string_of_token_list l)

(* let parse_acm = function
    (* [Text "int"] -> Int
  | [Text "rel"] -> Rel *)
  | [Text "_"] -> AcmUndef
  | l -> failwith ("parse_acm: " ^ string_of_token_list l) *)

let parse_comp = function
    | [Text "co"] -> Comp "co" (* subst qub prep comp *)
    | [Text "kto"] -> Comp "kto" (* subst *)
    | [Text "ile"] -> Comp "ile" (* num adv *)
    | [Text "jaki"] -> Comp "jaki" (* adj *)
    | [Text "który"] -> Comp "który" (* adj *)
    | [Text "czyj"] -> Comp "czyj" (* adj *)
    | [Text "jak"] -> Comp "jak" (* prep conj adv *)
    | [Text "kiedy"] -> Comp "kiedy" (* comp adv *)
    | [Text "gdzie"] -> Comp "gdzie" (* qub adv *)
    | [Text "odkąd"] -> Comp "odkąd" (* adv *)
    | [Text "skąd"] -> Comp "skąd" (* adv *)
    | [Text "dokąd"] -> Comp "dokąd" (* adv *)
    | [Text "którędy"] -> Comp "którędy" (* adv *)
    | [Text "dlaczego"] -> Comp "dlaczego" (* adv *)
    | [Text "czemu"] -> Comp "czemu" (* adv *)
    | [Text "czy"] -> Comp "czy" (* qub conj *)
    | [Text "jakby"] -> Comp "jakby" (* qub comp *)
    | [Text "jakoby"] -> Comp "jakoby" (* qub comp *)
    | [Text "gdy"] -> Gdy (* adv; gdyby: qub comp *)
    | [Text "dopóki"] -> Comp "dopóki" (* comp *)
    | [Text "zanim"] -> Comp "zanim" (* comp *)
    | [Text "jeśli"] -> Comp "jeśli" (* comp *)
    | [Text "żeby2"] -> Zeby
    | [Text "żeby"] -> Comp "żeby" (* qub comp *)
    | [Text "że"] -> Comp "że" (* qub comp *)
    | [Text "aż"] -> Comp "aż" (* qub comp *)
    | [Text "bo"] -> Comp "bo" (* qub comp *)
    | [Text "niczym"] -> Comp "niczym"
    | [Text "_"] -> CompUndef
    | l -> failwith ("parse_comp: " ^ string_of_token_list l)

let parse_opinion = function
    "cer" -> Pewny
  | "col" -> Potoczny
  | "unc" -> Watpliwy
  | "dat" -> Archaiczny
  | "bad" -> Zly
  | "vul" -> Wulgarny
  | "unk" -> Nieokreslony
  | "met" -> Metaforyczny
  | "dom" -> Dziedzinowy
  | "rar" -> Sporadyczny
  | x -> failwith ("parse_opinion: " ^ x)

let parse_pred = function
    "pred" -> PredTrue
  | "nopred" -> PredFalse
  | s -> failwith ("parse_pred: " ^ s)

let parse_pos = function
    "SUBST",[number;case] -> SUBST(parse_number number,parse_case case)
  | "PPRON12",[number;case] -> PPRON12(parse_number number,parse_case case)
  | "PPRON3",[number;case] -> PPRON3(parse_number number,parse_case case)
  | "SIEBIE",[case] -> SIEBIE(parse_case case)
  | "PREP",[case] -> PREP(parse_case case)
  | "NUM",[case;gender] -> NUM(parse_case case,parse_gender gender)
  | "ADJ",[number;case;gender;grad] -> ADJ(parse_number number,parse_case case,parse_gender gender,parse_grad grad)
  | "ADV",[grad] -> ADV(parse_grad grad)
  | "GER",[number;case;gender;aspect;negation] -> GER(parse_number number,parse_case case,parse_gender gender,parse_aspect aspect,parse_negation negation)
  | "PPAS",[number;case;gender;aspect;negation] -> PPAS(parse_number number,parse_case case,parse_gender gender,parse_aspect aspect,parse_negation negation)
  | "PACT",[number;case;gender;aspect;negation] -> PACT(parse_number number,parse_case case,parse_gender gender,parse_aspect aspect,parse_negation negation)
  | "INF",[aspect;negation] -> INF(parse_aspect aspect,parse_negation negation)
  | "QUB",[] -> QUB
  | "COMPAR",[] -> COMPAR Str
  | "COMP",[ctype] -> COMP(parse_ctype ctype)
  | "PERS",[negation] -> PERS(parse_negation negation)
  | s,ll -> failwith ("parse_pos: " ^ s ^ "(" ^ String.concat "," (Xlist.map ll string_of_token_list) ^ ")")

let rec parse_phrase = function
    "np",[case] -> NP(parse_case case)
  | "prepnp",[[Text prep]; case] -> PrepNP((*Psem,*)prep,parse_case case)
  | "adjp",[case] -> AdjP(parse_case case)
  | "prepadjp",[[Text prep]; case] -> PrepAdjP(prep,parse_case case)
  | "comprepnp",[[Text prep]] -> ComprepNP prep
  | "comparp",[[Text prep]] -> ComparP((*Psem,*)prep,Str)
  | "cp",[ctype;comp] -> CP(parse_ctype ctype,parse_comp comp)
  | "ncp",[case;ctype;comp] -> NCP(parse_case case,parse_ctype ctype,parse_comp comp)
  | "prepncp",[[Text prep];case;ctype;comp] -> PrepNCP((*Psem,*)prep,parse_case case,parse_ctype ctype,parse_comp comp)
  | "infp",[aspect] -> InfP(parse_aspect aspect)
  | "fixed",[[Text lemma]] -> FixedP lemma
  | "fixed",[[Text lemma1];[Text lemma2]] -> FixedP (lemma1 ^ "," ^ lemma2)
  | "or",[] -> Or
  (* | "refl",[] -> Refl
  | "recip",[] -> Recip *)
  | "E",[morf] -> E(parse_morf morf)
  | "advp",[[Text mode]] -> AdvP mode
  | "null",[] -> Null
  | "lex",[[Text lemma];[Text pos; Paren p]] -> SimpleLexArg(lemma,parse_pos (pos, split_symbol Comma [] p))
  | "lex",[[Text lemma];[Text pos]] -> SimpleLexArg(lemma,parse_pos (pos, []))
  | "lex",[[Text id];[Text lemma];[Text pos; Paren p]] -> LexArg(int_of_string id,lemma,parse_pos (pos, split_symbol Comma [] p))
  | "lex",[[Text id];[Text lemma];[Text pos]] -> LexArg(int_of_string id,lemma,parse_pos (pos, []))
  | s,ll -> failwith ("parse_phrase: " ^ s ^ "(" ^ String.concat "," (Xlist.map ll string_of_token_list) ^ ")")

and parse_morf = function
    [Text a; Paren p] -> parse_phrase (a, split_symbol Comma [] p)
  | [Text a] -> parse_phrase (a, [])
  | l -> failwith ("parse_morf: " ^ string_of_token_list l)

let parse_roles l =
  let r,cr,ce,m = Xlist.fold l ([],[],[],[]) (fun (r,controller,controllee,m) -> function
      "subj" -> SUBJ :: r, controller, controllee, m
    | "obj" -> OBJ :: r, controller, controllee, m
    | "controller" -> r, "1" :: controller, controllee, m
    | "controllee" -> r, controller, "1" :: controllee, m
    | "controller2" -> r, "2" :: controller, controllee, m
    | "controllee2" -> r, controller, "2" :: controllee, m
    | "misc" -> r, controller, controllee, "misc" :: m
    | "locat" -> r, controller, controllee, "locat" :: m
    | "abl" -> r, controller, controllee, "abl" :: m
    | "adl" -> r, controller, controllee, "adl" :: m
    | "caus" -> r, controller, controllee, "caus" :: m
    | "mod" -> r, controller, controllee, "mod" :: m
    | "temp" -> r, controller, controllee, "temp" :: m
    | "dur" -> r, controller, controllee, "dur" :: m
    | "possp" -> r, controller, controllee, "possp" :: m
    | "perl" -> r, controller, controllee, "perl" :: m
    | "instr" -> r, controller, controllee, "instr" :: m
    | "dest" -> r, controller, controllee, "dest" :: m
    | "distrp" -> r, controller, controllee, "distrp" :: m
    | "lemma" -> r, controller, controllee, "lemma" :: m
    | "refl" -> r, controller, controllee, "refl" :: m
    | "recip" -> r, controller, controllee, "recip" :: m
    | "nonch" -> r, controller, controllee, "nonch" :: m
    | "pron" -> r, controller, controllee, "pron" :: m
    | "" -> r, controller, controllee, m
    | x -> failwith ("parse_roles: " ^ x)) in
  (match r with
    [] -> ARG
  | [x] -> x
  | _ -> failwith "parse_roles"),cr,ce,m

let parse_schema = function
    [] -> NoRestr,[]
  | [Text "atr"] -> Atr,[]
  | [Text "ratr"] -> Ratr,[]
  | [Text "atr1"] -> Atr1,[]
  | [Text "ratr1"] -> Ratr1,[]
  | l -> NoRestr,Xlist.map (split_symbol Plus [] l) (function
        [Bracet l] -> {empty_position with morfs=Xlist.map (split_symbol Semic [] l) parse_morf}
      | [Text s; Bracet l] ->
        let gf,cr,ce,m = parse_roles [s] in
        {empty_position with gf=gf; cr=cr; ce=ce; mode=m; morfs=Xlist.map (split_symbol Semic [] l) parse_morf}
      | l -> failwith ("parse_schema: " ^ string_of_token_list l))

let parse_simple_morf = function
    [Text id] -> MorfId (int_of_string id)
  | l -> failwith ("parse_simple_morf: " ^ string_of_token_list l)

let parse_position = function
    [Bracet l] ->
    let morfs = Xlist.map (split_symbol Semic [] l) parse_simple_morf in
    {empty_position with morfs=morfs}
  | [Text s; Bracet l] ->
    let gf,cr,ce,m = parse_roles [s] in
    let morfs = Xlist.map (split_symbol Semic [] l) parse_simple_morf in
    {empty_position with gf=gf; cr=cr; ce=ce; mode=m; morfs=morfs}
  | [Text s1; Comma; Text s2; Bracet l] ->
    let gf,cr,ce,m = parse_roles [s1;s2] in
    let morfs = Xlist.map (split_symbol Semic [] l) parse_simple_morf in
    {empty_position with gf=gf; cr=cr; ce=ce; mode=m; morfs=morfs}
  | [Text s1; Comma; Text s2; Comma; Text s3; Bracet l] ->
    let gf,cr,ce,m = parse_roles [s1;s2;s3] in
    let morfs = Xlist.map (split_symbol Semic [] l) parse_simple_morf in
    {empty_position with gf=gf; cr=cr; ce=ce; mode=m; morfs=morfs}
  | [Text s1; Comma; Text s2; Comma; Text s3; Comma; Text s4; Bracet l] ->
    let gf,cr,ce,m = parse_roles [s1;s2;s3;s4] in
    let morfs = Xlist.map (split_symbol Semic [] l) parse_simple_morf in
    {empty_position with gf=gf; cr=cr; ce=ce; mode=m; morfs=morfs}
  | l -> failwith ("parse_simple_schema: " ^ string_of_token_list l)

let parse_sel_pref = function
    [Text "synset"; Paren[Text id]] -> SynsetId(int_of_string id)
  | [Text "relation"; Paren[Text name; Comma; Text role]] -> RelationRole(name,role,"")
  | [Text "relation"; Paren[Text name; Comma; Text role; Comma; Text role_attr]] -> RelationRole(name,role,role_attr)
  | [Text "relation"; Paren[Text name; Paren[Text name2]; Comma; Text role;]] ->
    RelationRole(name ^ "(" ^ name2 ^ ")",role,"")
  | [Text "relation"; Paren[Text name; Paren[Text name2]; Comma; Text role; Comma; Text role_attr]] ->
    RelationRole(name ^ "(" ^ name2 ^ ")",role,role_attr)
  | [Text s] -> Predef s
  | l -> failwith ("parse_sel_pref: " ^ string_of_token_list l)

let parse_sem position = function
    [Text role] -> {position with role=role}
  | [Text role; Comma; Text role_attr] ->
    {position with role=role; role_attr=role_attr}
  | [Text role; SqBra l] ->
    let sel_prefs = Xlist.map (split_symbol Semic [] l) parse_sel_pref in
    {position with role=role; sel_prefs=sel_prefs}
  | [Text role; Comma; Text role_attr; SqBra l] ->
    let sel_prefs = Xlist.map (split_symbol Semic [] l) parse_sel_pref in
    {position with role=role; role_attr=role_attr; sel_prefs=sel_prefs}
  | l -> failwith ("parse_sem: " ^ string_of_token_list l)

let parse_simple_schema l =
  if l = [] then [] else
  Xlist.map (split_symbol Plus [] l) parse_position

let parse_connected_schema l =
  if l = [] then [] else
    Xlist.map (split_symbol Plus [] l) (fun pos ->
        match split_symbol Colon [] pos with
          [syntax;sem] -> parse_sem (parse_position syntax) sem
        | _ -> failwith "parse_connected_schema")

let parse_entry (restr,schema) = function
    [Text "lex"; Paren[Text lemma;Comma;Text pos]] -> SimpleLexEntry(lemma,pos)
  | [Text "lex"; Paren[Text id;Comma;Text lemma;Comma;Text pos]] -> LexEntry(int_of_string id,lemma,pos,restr,schema)
  | [Text "comprepnp"; Paren[Text lemma]] -> ComprepNPEntry(lemma,restr,schema)
  | l -> failwith ("parse_entry: " ^ string_of_token_list l)

let load_entries filename =
  let l = File.load_tab filename (function
        [pos; lemma; entry; schema] -> pos, lemma, entry, schema
      | [pos; lemma; entry] -> pos, lemma, entry, ""
      | _ -> failwith "load_entries") in
  Xlist.fold l Entries.empty (fun entries (pos,lemma,entry,schema) ->
      let schema = parse_schema (split_text schema) in
      let entry = parse_entry schema (split_text entry) in
      Entries.add_inc entries pos lemma entry)

let load_phrases filename =
  let l = File.load_tab filename (function
        id :: morfs -> int_of_string id, morfs
      | _ -> failwith "load_phrases") in
  Xlist.fold l IntMap.empty (fun phrases (id,morfs) ->
      (* print_endline (string_of_int id); *)
      let morfs = Xlist.map morfs (fun morf -> parse_morf (split_text morf)) in
      IntMap.add phrases id morfs)

let load_schemata filename =
  let l = File.load_tab filename (function
        [pos; lemma; opinion; neg; pred; aspect; schema] -> pos, lemma, opinion, neg, pred, aspect, schema
      | _ -> failwith "load_schemata") in
  Xlist.fold l Entries.empty (fun entries (pos,lemma,opinion,neg,pred,aspect,schema) ->
      let opinion = parse_opinion opinion in
      let neg = parse_negation [Text neg] in
      let pred = parse_pred pred in
      let aspect = parse_aspect [Text aspect] in
      let schema = parse_simple_schema (split_text schema) in
      let entry = opinion,neg,pred,aspect,schema in
      Entries.add_inc entries pos lemma entry)

let load_connected filename =
  let l = File.load_tab filename (function
        [pos; lemma; sopinion; fopinion; senses; neg; pred; aspect; schema] ->
          pos, lemma, sopinion, fopinion, senses, neg, pred, aspect, schema
      | _ -> failwith "load_schemata") in
  Xlist.fold l Entries.empty (fun entries (pos,lemma,sopinion,fopinion,senses,neg,pred,aspect,schema) ->
      let sopinion = parse_opinion sopinion in
      let fopinion = parse_opinion fopinion in
      let senses = Xlist.map (Xstring.split "," senses) int_of_string in
      let neg = parse_negation [Text neg] in
      let pred = parse_pred pred in
      let aspect = parse_aspect [Text aspect] in
      let schema = parse_connected_schema (split_text schema) in
      let entry = sopinion,fopinion,senses,neg,pred,aspect,schema in
      Entries.add_inc entries pos lemma entry)

let load_senses filename =
  let l = File.load_tab filename (function
        [id; name; variant; plwnluid; gloss] -> {mng_id=int_of_string id;
                                                 name=name;
                                                 variant=variant;
                                                 plwnluid=int_of_string plwnluid;
                                                 gloss=gloss}
      | _ -> failwith "load_sense") in
  Xlist.fold l IntMap.empty (fun senses m ->
      IntMap.add senses m.mng_id m)

let phrases = ref IntMap.empty
let entries = ref StringMap.empty
let schemata = ref StringMap.empty
let connected = ref StringMap.empty
let senses = ref IntMap.empty

let initialize () =
  phrases := load_phrases phrases_filename;
  entries := load_entries entries_filename;
  schemata := load_schemata schemata_filename;
  connected := load_connected connected_filename;
  senses := load_senses senses_filename;
  ()


(*
let print_subjs () =
(*   let expands,compreps,comprep_reqs,subtypes,equivs = load_realizations () in *)
  let subjs = Xlist.fold Paths.walenty_filenames StringQMap.empty (fun subjs filename ->
(*     print_endline filename; *)
    let frames = load_frames (Paths.walenty_path ^ filename) in
    StringMap.fold frames subjs (fun subjs _ l ->
      Xlist.fold l subjs (fun subjs (refl,opinion,negation,pred,aspect,schema) ->
        Xlist.fold (parse_schema schema) subjs (fun subjs s ->
          if s.gf = SUBJ then StringQMap.add subjs (ENIAMwalStringOf.schema [s]) else subjs)))) in
  StringQMap.iter subjs (fun s v ->
      Printf.printf "%5d %s\n" v s)

(* let _ = print_subjs () *)

let print_ctrls () =
(*   let expands,compreps,comprep_reqs,subtypes,equivs = load_realizations () in *)
  let ctrls = Xlist.fold Paths.walenty_filenames StringQMap.empty (fun ctrls filename ->
(*     print_endline filename; *)
    let frames = load_frames (Paths.walenty_path ^ filename) in
    StringMap.fold frames ctrls (fun ctrls _ l ->
      Xlist.fold l ctrls (fun ctrls (refl,opinion,negation,pred,aspect,schema) ->
        let schema = List.rev (Xlist.fold (parse_schema schema) [] (fun l s ->
          if s.cr = [] && s.ce = [] then l else s :: l)) in
        StringQMap.add ctrls (ENIAMwalStringOf.schema schema)))) in
  StringQMap.iter ctrls (fun s v ->
      Printf.printf "%5d %s\n" v s)

(* let _ = print_ctrls () *)
*)
