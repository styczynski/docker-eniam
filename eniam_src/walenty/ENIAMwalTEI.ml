(*
 *  ENIAMwalenty, a converter for Polish Valence Dictionary "Walenty".
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Maciej Hołubowicz
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

type id = {hash: bool; suffix: string; numbers: int list}

let empty_id = {hash = false; suffix = ""; numbers = []}

let parse_id s =
  if String.length s = 0 then empty_id else
  if String.length s < 6 then failwith "za krótkie id"  else
    let hash,s = if (String.get s 0) = '#' then true, String.sub s 1 (String.length s - 1) else false, s in
    if String.sub s 0 4 <> "wal_" then failwith "id nie ma wal" else
      let s = String.sub s 4 (String.length s - 4) in
      let s,suf = match Str.split (Str.regexp "-") s with
          [s;suf] -> s,suf
        | _ -> failwith ("parse_id: zła ilość '-' " ^ s) in
      let id = {hash = hash; suffix = suf; numbers = try Xlist.map (Xstring.split "\\." s) int_of_string with _ -> failwith ("parse_id: " ^ s)} in
      id

let string_of_id id =
  (if id.hash then "#" else "") ^ "wal_" ^ (String.concat "." (Xlist.map id.numbers string_of_int)) ^ "-" ^ id.suffix

type tei =
    Symbol of string
  | TEIstring of string
  | Binary of bool
  | Numeric of int
  | F of string * tei
  | Fset of string * tei list
  | Fs of string * tei list
  | Id of id
  | SameAs of id * string

let rec tei_to_string = function
    Symbol s -> Printf.sprintf "Symbol %s" s
  | TEIstring s -> Printf.sprintf "String %s" s
  | Binary b -> Printf.sprintf "Binary %s" (string_of_bool b)
  | Numeric n -> Printf.sprintf "Numeric %d" n
  | F(s,t) -> Printf.sprintf "F(%s,%s)" s (tei_to_string t)
  | Fset(s,l) -> Printf.sprintf "Fset(%s,[%s])" s (String.concat ";" (Xlist.map l tei_to_string))
  | Fs(s,l) -> Printf.sprintf "Fs(%s,[%s])" s (String.concat ";" (Xlist.map l tei_to_string))
  | Id id -> Printf.sprintf "Id(%s)" (string_of_id id)
  (* | SameAs(id,s) -> Printf.sprintf "F(Id,%s)" s *)
  | SameAs(id,s) -> Printf.sprintf "SameAs(%s,%s)" (string_of_id id) s

let rec parse_tei = function
    Xml.Element("f",["name",name],[Xml.Element("vColl",["org","set"],set)]) ->
    Fset(name,List.rev (Xlist.map set parse_tei))
  | Xml.Element("f", ["name",name],[]) -> Fset(name,[])
  | Xml.Element("f", ["name",name],[tei]) -> F(name,parse_tei tei)
  | Xml.Element("f", ["name",name],set) -> Fset(name,List.rev (Xlist.map set parse_tei))
  | Xml.Element("fs", ["type",name], l) -> Fs(name,List.rev (Xlist.rev_map l parse_tei))
  | Xml.Element("fs", ["xml:id",id;"type",name], l) -> Fs(name,Id(parse_id id) :: List.rev (Xlist.rev_map l parse_tei))
  | Xml.Element("symbol",["value",value],[]) -> Symbol value
  | Xml.Element("string",[], [Xml.PCData s]) -> TEIstring s
  | Xml.Element("string",[], []) -> TEIstring ""
  | Xml.Element("binary",["value",value],[]) -> Binary(try bool_of_string value with _ -> failwith "parse_tei")
  | Xml.Element("numeric",["value",value],[]) -> Numeric(try int_of_string value with _ -> failwith "parse_tei")
  | Xml.Element("fs", ["sameAs", same_as; "type",name], []) -> SameAs(parse_id same_as,name)
  | Xml.Element("fs", ["sameAs", same_as], []) -> SameAs(parse_id same_as,"")
  | xml -> failwith ("parse_tei: " ^ Xml.to_string_fmt xml)

let parse_gf = function
    "subj" -> SUBJ
  | "obj" -> OBJ
  | "head" -> HEAD
  | s -> failwith ("parse_gf: " ^ s)

let parse_control arg = function
    "controller" -> {arg with cr="1" :: arg.cr}
  | "controllee" -> {arg with ce="1" :: arg.ce}
  | "controller2" -> {arg with cr="2" :: arg.cr}
  | "controllee2" -> {arg with ce="2" :: arg.ce}
  | s -> failwith ("parse_control: " ^ s)

let parse_case = function
    "nom" -> Case "nom"
  | "gen" -> Case "gen"
  | "dat" -> Case "dat"
  | "acc" -> Case "acc"
  | "inst" -> Case "inst"
  | "loc" -> Case "loc"
  | "str" -> Str
  | "pred" -> Case "pred"
  | "part" -> Part
  | "postp" -> Case "postp"
  | "agr" -> CaseAgr
  | s -> failwith ("parse_case: " ^ s)

let parse_aspect = function
    "perf" -> Aspect "perf"
  | "imperf" -> Aspect "imperf"
  | "_" -> AspectUndef
  | "" -> AspectNA
  | s -> failwith ("parse_aspect: " ^ s)

let parse_negation = function
    "_" -> NegationUndef
  | "neg" -> Negation
  | "aff" -> Aff
  | "" -> NegationNA
  | s -> failwith ("parse_negation: " ^ s)

let parse_number = function
    "sg" -> Number "sg"
  | "pl" -> Number "pl"
  | "agr" -> NumberAgr
  | "_" -> NumberUndef
  | s -> failwith ("parse_number: " ^ s)

let parse_gender = function
    "m1" -> Gender "m1"
  | "m2" -> Gender "m2"
  | "m3" -> Gender "m3"
  | "n" -> Gender "n"(*Genders["n1";"n2"]*)
  | "f" -> Gender "f"
  | "m1.n" -> Genders["m1";"n"(*"n1";"n2"*)]
  | "_" -> GenderUndef
  | "agr" -> GenderAgr
  | s -> failwith ("parse_gender: " ^ s)

let parse_genders = function
    [Symbol "agr"] -> GenderAgr
  | genders ->
      let genders = Xlist.map genders (function
           Symbol "m1" -> "m1"
         | Symbol "m2" -> "m2"
         | Symbol "m3" -> "m3"
         | Symbol "n" -> "n"
         | Symbol "f" -> "f"
         | s -> failwith ("parse_genders: " ^ tei_to_string s)) in
      (match genders with
        [g] -> Gender g
      | [] -> failwith "parse_genders: empty"
      | _ -> Genders genders)

let parse_grad = function
    "pos" -> Grad "pos"
  | "com" -> Grad "com"
  | "sup" -> Grad "sup"
  | "_" -> GradUndef
  | s -> failwith ("parse_grad: " ^ s)

let rec parse_restr = function
    "natr" -> Natr
  | "atr" -> Atr
  | "ratr" -> Ratr
  | "atr1" -> Atr1
  | "ratr1" -> Ratr1
  | s -> failwith ("parse_restr: " ^ s)


let parse_comp = function
    "int" -> Int,[]
  | "rel" -> Rel,[]
  | "co" -> CompTypeUndef,[Comp "co"] (* subst qub prep comp *)
  | "kto" -> CompTypeUndef,[Comp "kto"] (* subst *)
  | "ile" -> CompTypeUndef,[Comp "ile"] (* num adv *)
  | "jaki" -> CompTypeUndef,[Comp "jaki"] (* adj *)
  | "który" -> CompTypeUndef,[Comp "który"] (* adj *)
  | "czyj" -> CompTypeUndef,[Comp "czyj"] (* adj *)
  | "jak" -> CompTypeUndef,[Comp "jak"] (* prep conj adv *)
  | "kiedy" -> CompTypeUndef,[Comp "kiedy"] (* comp adv *)
  | "gdzie" -> CompTypeUndef,[Comp "gdzie"] (* qub adv *)
  | "odkąd" -> CompTypeUndef,[Comp "odkąd"] (* adv *)
  | "skąd" -> CompTypeUndef,[Comp "skąd"] (* adv *)
  | "dokąd" -> CompTypeUndef,[Comp "dokąd"] (* adv *)
  | "którędy" -> CompTypeUndef,[Comp "którędy"] (* adv *)
  | "dlaczego" -> CompTypeUndef,[Comp "dlaczego"] (* adv *)
  | "czemu" -> CompTypeUndef,[Comp "czemu"] (* adv *)
  | "czy" -> CompTypeUndef,[Comp "czy"] (* qub conj *)
  | "jakby" -> CompTypeUndef,[Comp "jakby"] (* qub comp *)
  | "jakoby" -> CompTypeUndef,[Comp "jakoby"] (* qub comp *)
  | "gdy" -> CompTypeUndef,[Gdy] (* adv; gdyby: qub comp *)
  | "dopóki" -> CompTypeUndef,[Comp "dopóki"] (* comp *)
  | "zanim" -> CompTypeUndef,[Comp "zanim"] (* comp *)
  | "jeśli" -> CompTypeUndef,[Comp "jeśli"] (* comp *)
  | "żeby2" -> CompTypeUndef,[Zeby]
  | "żeby" -> CompTypeUndef,[Comp "żeby"] (* qub comp *)
  | "że" -> CompTypeUndef,[Comp "że"] (* qub comp *)
  | "aż" -> CompTypeUndef,[Comp "aż"] (* qub comp *)
  | "bo" -> CompTypeUndef,[Comp "bo"] (* qub comp *)
  | s -> failwith ("parse_comp: " ^ s)

let load_type_constrains = function
  | Symbol value ->
    (match parse_comp value with
       CompTypeUndef,[c] -> c
     | _ -> failwith "load_type_constrains")
  | xml -> failwith ("load_type_constrains:\n " ^ tei_to_string xml)

let load_ctype = function
  | F("type",Fs("type_def", x)) ->
    (match x with
     | [F("conjunction",Symbol value)] -> parse_comp value
     | [F("conjunction",Symbol value);Fset("constraints",set)] ->
       (match parse_comp value with
          CompTypeUndef, _ -> failwith "load_ctype"
        | ctype,[] -> ctype, List.rev (Xlist.rev_map set load_type_constrains)
        | _ -> failwith "load_ctype")
     | l -> failwith ("load_ctype 2:\n " ^ String.concat "\n" (Xlist.map l tei_to_string)))
  | xml -> failwith ("load_ctype:\n " ^ tei_to_string xml)

let load_lemmas_set = function
  | TEIstring mstring -> mstring
  | xml -> failwith ("load_lemmas_set:\n " ^ tei_to_string xml)

let check_lemma s =
  match Str.full_split (Str.regexp "(\\|)") s with
    [Str.Text s] -> Lexeme s
  | [Str.Text "E"; Str.Delim "("; Str.Text g; Str.Delim ")"] -> Elexeme(parse_gender g)
  | _ -> failwith "check_lemma"

let make_lemma = function
  | _,_,[lemma] -> check_lemma lemma
  | "XOR","concat",lemmas -> XOR(Xlist.map lemmas check_lemma)
  | "OR","coord",lemmas -> ORcoord(Xlist.map lemmas check_lemma)
  | "OR","concat",lemmas -> ORconcat(Xlist.map lemmas check_lemma)
  | _ -> failwith "make_lemma"

let process_lex_phrase lemma = function
    NP(case),number,GenderUndef,GradUndef,NegationUndef,ReflUndef -> [SUBST(number,case),lemma]
  | PrepNP(prep,case),number,GenderUndef,GradUndef,NegationUndef,ReflUndef -> [PREP case,Lexeme prep;SUBST(number,case),lemma]
  | AdjP(case),number,gender,grad,NegationUndef,ReflUndef -> [ADJ(number,case,gender,grad),lemma]
  | PrepAdjP(prep,case),number,gender,grad,NegationUndef,ReflUndef -> [PREP case,Lexeme prep;ADJ(number,case,gender,grad),lemma]
  | InfP(aspect),NumberUndef,GenderUndef,GradUndef,negation,ReflTrue -> [INF(aspect,negation),lemma;QUB,Lexeme "się"]
  | InfP(aspect),NumberUndef,GenderUndef,GradUndef,negation,refl -> [INF(aspect,negation),lemma]
  | PpasP(case),number,gender,GradUndef,negation,ReflUndef -> [PPAS(number,case,gender,AspectUndef,negation),lemma]
  | PrepPpasP(prep,case),number,gender,GradUndef,negation,ReflUndef -> [PREP case,Lexeme prep;PPAS(number,case,gender,AspectUndef,negation),lemma]
  | PactP(case),number,gender,GradUndef,negation,ReflTrue -> [PACT(number,case,gender,AspectUndef,negation),lemma;QUB,Lexeme "się"]
  | PactP(case),number,gender,GradUndef,negation,refl -> [PACT(number,case,gender,AspectUndef,negation),lemma]
  | PrepGerP(prep,case),number,GenderUndef,GradUndef,negation,ReflTrue -> [PREP case,Lexeme prep;GER(number,case,GenderUndef,AspectUndef,negation),lemma;QUB,Lexeme "się"]
  | PrepGerP(prep,case),number,GenderUndef,GradUndef,negation,refl -> [PREP case,Lexeme prep;GER(number,case,GenderUndef,AspectUndef,negation),lemma]
  | Qub,NumberUndef,GenderUndef,GradUndef,NegationUndef,ReflUndef -> [QUB,lemma]
  | AdvP(mode),NumberUndef,GenderUndef,grad,NegationUndef,ReflUndef -> [ADV grad,lemma]
  | phrase,number,gender,grad,negation,reflex ->
    Printf.printf "%s %s %s %s %s %s\n" (ENIAMwalStringOf.phrase phrase) (ENIAMwalStringOf.number number)
      (ENIAMwalStringOf.gender gender) (ENIAMwalStringOf.grad grad) (ENIAMwalStringOf.negation negation) (ENIAMwalStringOf.refl reflex); []

let new_schema r cr ce morfs =
  {psn_id=(-1); gf=r; role=""; role_attr=""; mode=[]; sel_prefs=[]; cr=cr; ce=ce; morfs=morfs}

let rec process_lex lex = function
  | Phrase(ComparP prep),arguments,Lexeme "",Lexeme "" ->
    LexPhrase([COMPAR,Lexeme prep],(Ratrs,Xlist.map arguments (fun morf -> new_schema ARG [] [] [morf])))
  | PhraseAbbr(Xp mode,[argument]),_,_,_ ->
    let lex = {lex with lex_argument=argument; lex_mode=mode :: lex.lex_mode} in
    process_lex lex (lex.lex_argument,lex.lex_arguments,lex.lex_lemma,lex.lex_numeral_lemma)
  (* | PhraseAbbr(Advp mode,[]),[],lemma,Lexeme ""  ->
    let poss = process_lex_phrase lemma (AdvP,lex.lex_number,lex.lex_gender,lex.lex_degree,lex.lex_negation,lex.lex_reflex) in
    LexPhrase(poss,lex.lex_modification) *)
  | Phrase (NumP(case)),[],lemma,num_lemma -> LexPhrase([NUM(case,GenderUndef),num_lemma;SUBST(NumberUndef,CaseUndef),lemma],lex.lex_modification)
  | Phrase (PrepNumP(prep,case)),[],lemma,num_lemma  -> LexPhrase([PREP case,Lexeme prep;NUM(case,GenderUndef),num_lemma;SUBST(NumberUndef,CaseUndef),lemma],lex.lex_modification)
  | PhraseComp(Cp,(ctype,[Comp comp])),[],lemma,Lexeme "" ->
    if lex.lex_reflex = ReflTrue then LexPhrase([COMP ctype,Lexeme comp;PERS(lex.lex_negation),lemma;QUB,Lexeme "się"],lex.lex_modification)
        else LexPhrase([COMP ctype,Lexeme comp;PERS(lex.lex_negation),lemma],lex.lex_modification)
  | PhraseComp(Cp,(ctype,[Comp comp1;Comp comp2])),[],lemma,Lexeme "" ->
    if lex.lex_reflex = ReflTrue then LexPhrase([COMP ctype,XOR[Lexeme comp1;Lexeme comp2];PERS(lex.lex_negation),lemma;QUB,Lexeme "się"],lex.lex_modification)
        else LexPhrase([COMP ctype,XOR[Lexeme comp1;Lexeme comp2];PERS(lex.lex_negation),lemma],lex.lex_modification)
  | Phrase phrase,[],lemma,Lexeme ""  ->
    let poss = process_lex_phrase lemma (phrase,lex.lex_number,lex.lex_gender,lex.lex_degree,lex.lex_negation,lex.lex_reflex) in
    LexPhrase(poss,lex.lex_modification)
  | (argument,arguments,lemma,numeral_lemma) ->
    let s = Printf.sprintf "%s [%s] %s %s\n" (ENIAMwalStringOf.morf argument)
      (String.concat ";" (Xlist.map arguments ENIAMwalStringOf.morf))
      (ENIAMwalStringOf.lex lemma) (ENIAMwalStringOf.lex numeral_lemma) in
    failwith ("process_lex: " ^ s)

(* UWAGA: refl_id może się zmienić wraz z wersją Walentego *)
let refl_id = 25
let refl_position = {empty_position with role="Lemma"; mode=["lemma"]; morfs=[MorfId refl_id]}

let rec load_category = function
  | F("category",Fs("category_def",x)) ->
    (match x with
     | [F("name",Symbol value)] -> value, []
     | [F("name",Symbol value);Fset("constraints",set)] ->
       value, List.rev (Xlist.rev_map set (load_phrase (ref [])))
     | l -> failwith ("load_category 2:\n " ^ String.concat "\n" (Xlist.map l tei_to_string)))
  | xml -> failwith ("load_category:\n " ^ tei_to_string xml)

and load_modification_def = function (*pomocnicza do load_lex *)
  | [F("type",Symbol value)] -> parse_restr value, []
  | [F("type",Symbol value); Fset("positions",set)] ->
    parse_restr value, List.rev (Xlist.rev_map set (load_position (-1) (-1) (ref IntMap.empty)))
  | x -> Printf.printf "%s\n" (tei_to_string (List.hd x));
    failwith "load_modification_def:\n"

and load_lex arg xml = match xml with
  | F("argument",set) ->
    let mode = ref [] in
    let a = load_phrase mode set in
    {arg with lex_argument = a; lex_mode = !mode}
  | Fset("arguments",set) ->
    {arg with lex_arguments=List.rev (Xlist.rev_map set (load_phrase (ref [])))}
  | F("modification",Fs("modification_def",x)) -> {arg with lex_modification = load_modification_def x}
  | F("lemma",Fs("lemma_def",[F("selection_mode",Symbol value1);
                              F("cooccurrence",Symbol value2);
                              Fset("lemmas",lemmas)])) ->
    {arg with lex_lemma = make_lemma (value1, value2, List.rev (Xlist.rev_map lemmas load_lemmas_set))}
  |  F("numeral_lemma",Fs("numeral_lemma_def",[F("selection_mode",Symbol value1);
                                               F("cooccurrence",Symbol value2);
                                               Fset("lemmas",lemmas)])) ->
    {arg with lex_numeral_lemma = make_lemma (value1, value2, List.rev (Xlist.rev_map lemmas load_lemmas_set))}
  | F("negation",Symbol value) -> {arg with lex_negation = parse_negation value}
  | F("degree",Symbol value) -> {arg with lex_degree = parse_grad value}
  | F("number",Symbol value) -> {arg with lex_number = parse_number value}
  | F("reflex",Binary true) -> {arg with lex_reflex = ReflTrue}
  | F("reflex",Binary false) -> {arg with lex_reflex = ReflFalse}
  | Fset("reflex",[]) -> {arg with lex_reflex = ReflEmpty}
  | Fset("gender",genders) -> {arg with lex_gender = parse_genders genders}
  | xml ->
    Printf.printf "%s\n" (tei_to_string xml);
    failwith "load_lex:\n "

and load_phrase mode = function
  | Fs("np",[F("case",Symbol a)]) -> Phrase (NP(parse_case a));
  | Fs("prepnp", [F("preposition",Symbol a);F("case",Symbol b)]) -> Phrase (PrepNP(a, parse_case b))
  | Fs("adjp", [F("case",Symbol a)]) -> Phrase (AdjP(parse_case a))
  | Fs("prepadjp", [F("preposition",Symbol a);F("case",Symbol b)]) -> Phrase (PrepAdjP(a, parse_case b))
  | Fs("comprepnp", [e;F("complex_preposition",TEIstring a)]) -> Phrase (ComprepNP(a))
  | Fs("comprepnp", [F("complex_preposition",TEIstring a)]) -> Phrase (ComprepNP(a))
  | Fs("cp", [a]) -> PhraseComp(Cp,load_ctype a)
  | Fs("ncp", [F("case",Symbol a);b]) -> PhraseComp(Ncp(parse_case a),load_ctype b)
  | Fs("prepncp", [F("preposition",Symbol a);F("case",Symbol b);c]) -> PhraseComp(Prepncp(a, parse_case b),load_ctype c)
  | Fs("infp", [F("aspect",Symbol a)]) -> Phrase (InfP(parse_aspect a))
  | Fs("xp", [a]) -> let x,y = load_category a in mode:=x :: !mode; PhraseAbbr(Xp x,y)
  | Fs("xp", [e;a]) -> let x,y = load_category a in mode:=x :: !mode; PhraseAbbr(Xp x,y)
  | Fs("advp", [F("category",Symbol a)]) -> mode:=a :: !mode; Phrase(AdvP(a))
  | Fs("advp", [e;F("category",Symbol a)]) -> mode:=a :: !mode; Phrase(AdvP(a))
  | Fs("nonch", []) -> mode:="nonch" :: !mode; PhraseAbbr(Nonch,[])
  | Fs("or", []) -> Phrase Or
  | Fs("refl", []) -> mode:="refl" :: !mode; LexPhrase([QUB,Lexeme "się"],(Natr,[]))
  | Fs("E", []) -> E Null
  | Fs("lex", x) ->
    let lex = Xlist.fold x empty_lex load_lex in
    mode := lex.lex_mode @ !mode;
    process_lex lex (lex.lex_argument,lex.lex_arguments,lex.lex_lemma,lex.lex_numeral_lemma)
  | Fs("fixed", [F("argument",a);F("string",TEIstring b)]) -> Phrase (FixedP((*snd (load_phrase a),*)b))
  | Fs("possp", [e]) -> mode:="possp" :: !mode; PhraseAbbr(Possp,[])
  | Fs("possp", []) -> mode:="possp" :: !mode; PhraseAbbr(Possp,[])
  | Fs("recip", []) -> mode:="recip" :: !mode; LexPhrase([QUB,Lexeme "się"],(Natr,[]))
  | Fs("distrp", [e]) -> mode:="distrp" :: !mode; PhraseAbbr(Distrp,[])
  | Fs("distrp", []) -> mode:="distrp" :: !mode; PhraseAbbr(Distrp,[])
  | Fs("compar", [F("compar_category",Symbol value)]) -> Phrase(ComparP value)
  | Fs("gerp", [F("case",Symbol a)]) -> Phrase (GerP(parse_case a))
  | Fs("prepgerp", [F("preposition",Symbol a);F("case",Symbol b)]) -> Phrase (PrepGerP(a, parse_case b))
  | Fs("nump", [F("case",Symbol a)]) -> Phrase (NumP(parse_case a))
  | Fs("prepnump", [F("preposition",Symbol a);F("case",Symbol b)]) -> Phrase (PrepNumP(a, parse_case b))
  | Fs("ppasp", [F("case",Symbol a)]) -> Phrase (PpasP(parse_case a))
  | Fs("prepppasp", [F("preposition",Symbol a);F("case",Symbol b)]) -> Phrase (PrepPpasP(a, parse_case b))
  | Fs("qub", []) -> Phrase Qub
  | Fs("pactp", [F("case",Symbol a)]) -> Phrase (PactP(parse_case a))
  | Fs("adverb",[F("adverb",Symbol s)]) -> LexPhrase([ADV (Grad "pos"),Lexeme s],(Natr,[]))
  | xml -> failwith ("load_phrase match:\n " ^ tei_to_string xml)

and load_phrase_id ent sch psn phrases mode = function
    | Fs(morf,Id{hash=false; numbers=[ent_id;sch_id;psn_id;id]; suffix="phr"} :: l) ->
      if ent_id = ent && sch_id = sch && psn_id = psn then
       let morf = load_phrase mode (Fs(morf, l)) in
       phrases := IntMap.add_inc (!phrases) id morf (fun morf2 -> if morf = morf2 then morf else failwith "load_phrase_id");
       MorfId id
     else failwith (Printf.sprintf "load_phrase %d %d" ent ent_id)
    | Fs(morf, l) -> load_phrase mode (Fs(morf, l))
    | _ -> failwith "load_phrase_id"


and load_control arg = function
  | Symbol  value -> parse_control arg value
  | xml -> failwith ("load_control:\n " ^ tei_to_string xml)

and load_position_info ent sch phrases arg = function
  | F("function",Symbol  value) -> {arg with gf = parse_gf value}
  | Fset("phrases",phrases_set) ->
    let mode = ref [] in
    let morfs = List.rev (Xlist.rev_map phrases_set (load_phrase_id ent sch arg.psn_id phrases mode)) in
    {arg with morfs = morfs; mode = StringSet.to_list (StringSet.of_list (!mode))}
  | Fset("control",control_set) -> Xlist.fold control_set arg load_control
  | Id{hash=false; numbers=[ent_id;sch_id;id]; suffix="psn"} ->
     if ent_id = ent && sch_id = sch then {arg with psn_id = id}
     else failwith (Printf.sprintf "load_position_info %d %d" ent ent_id)
  | xml -> failwith ("load_position_info:\n " ^ tei_to_string xml)

and load_position ent sch phrases = function
  | Fs("position", listt) ->
    Xlist.fold listt empty_position (load_position_info ent sch phrases)
  | xml -> failwith ("load_position:\n " ^ tei_to_string xml)

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
  | "wątpliwy" -> Watpliwy
  | "dobry" -> Pewny
  | "zły" -> Zly
  | x -> failwith ("parse_opinion: " ^ x)

let load_schema_info ent phrases (arg:schema) = function
  | F("opinion",Symbol opinion_value) -> {arg with opinion = parse_opinion opinion_value}
  | F("inherent_sie",Binary b) -> {arg with reflexiveMark = b}
  | F("aspect",Symbol aspect_value) -> {arg with aspect = parse_aspect aspect_value}
  | Fset("aspect", []) -> arg
  | F("negativity",Symbol negativity_value) -> {arg with negativity = parse_negation negativity_value}
  | Fset("negativity",[]) -> arg
  | F("predicativity",Binary true) -> {arg with predicativity = PredTrue}
  | F("predicativity",Binary false) -> {arg with predicativity = PredFalse}
  | Fset("positions", positions) ->
    {arg with positions = List.rev (Xlist.rev_map positions (load_position ent arg.sch_id phrases))}
  | F("text_rep",TEIstring text_rep) -> {arg with text_rep = text_rep}
  | Id{hash=false; numbers=[ent_id;id]; suffix="sch"} -> if ent_id = ent then {arg with sch_id = id} else failwith (Printf.sprintf "load_schema_info %d %d" ent ent_id)
  | xml -> failwith ("load_schema_info\n " ^ tei_to_string xml)

let load_schema ent phrases = function
    Fs("schema", schema) ->
    let result = {sch_id = (-1); opinion = OpinionUndef; reflexiveMark = false; aspect = AspectUndef;
                  negativity = NegationUndef; predicativity = PredUndef; positions = []; text_rep=""} in
    let result = Xlist.fold schema result (load_schema_info ent phrases) in
    result
  | xml -> failwith ("load_schema:\n " ^ tei_to_string xml)

let load_phrases_set ent = function
  | SameAs({hash=true; numbers=[ent_id;sch_id;psn_id;phr_id]; suffix="phr"},"phrase") ->
      if ent_id <> ent then failwith (Printf.sprintf "load_phrases_set %d %d" ent ent_id) else
      sch_id,psn_id,phr_id
  | xml -> failwith ("load_phrases_set :\n " ^ tei_to_string xml)

let load_example_info ent arg = function
  | F("meaning",SameAs({hash=true; numbers=[ent_id;id]; suffix="mng"},"lexical_unit")) ->
      if ent_id = ent then {arg with meaning = id} else failwith (Printf.sprintf "load_example_info %d %d" ent ent_id)
  | F("meaning",SameAs({hash=true; numbers=[id]; suffix="mng"},"lexical_unit")) ->
      {arg with meaning = id}
  | Fset("phrases",phrases_set) ->
    {arg with phrases = List.rev (Xlist.rev_map phrases_set (load_phrases_set ent))}
  | F("sentence",TEIstring sentence_string) -> {arg with sentence = sentence_string}
  | F("source",Symbol source_value) -> {arg with source = source_value}
  | F("opinion",Symbol opinion_value) -> {arg with opinion = parse_opinion opinion_value}
  | F("note",TEIstring note_string) -> {arg with note = note_string}
  | Id{hash=false; numbers=[ent_id;id]; suffix="exm"} -> if ent_id = ent then {arg with exm_id = id} else failwith (Printf.sprintf "load_example_info %d %d" ent ent_id)
  | xml -> failwith ("load_example_info: \n " ^ tei_to_string xml)

let load_example ent = function
  | Fs("example",example_elements) ->
    let result = {exm_id = (-1); meaning = (-1); phrases = []; sentence = "";
                  source = ""; opinion = OpinionUndef; note = "";} in
    let result = Xlist.fold example_elements result (load_example_info ent) in
    result
  | xml -> failwith ("load_example: \n " ^ tei_to_string xml)

let load_self_prefs_sets name ent frm = function
  | Numeric value -> if name = "synsets" then SynsetId value else failwith "load_self_prefs_sets"
  | Symbol value -> if name = "predefs" then Predef value else failwith "load_self_prefs_sets"
  | Fs("relation",[F("type",Symbol value);F("to",SameAs({hash=true; numbers=[(*ent_id;*)frm_id;arg_id]; suffix="arg"}, "argument"))]) ->
    if (*ent_id <> ent ||*) frm_id <> frm || name <> "relations" then failwith (Printf.sprintf "load_self_prefs_sets %d" ent (*ent_id*))
    else RelationArgId(value,arg_id)
  | xml -> failwith ("load_self_prefs_sets: \n " ^ tei_to_string xml)

let load_argument_self_prefs ent frm = function
  | Fset(name,self_prefs_set) ->
    List.rev (Xlist.rev_map self_prefs_set (load_self_prefs_sets name ent frm))
  | xml -> failwith ("load_argument_self_prefs: \n " ^ tei_to_string xml)

let load_argument_info ent frm arg = function
  | F("role",Symbol value) -> {arg with role = value}
  | F("role_attribute",Symbol value) -> {arg with role_attribute = value}
  | F("sel_prefs",Fs("sel_prefs_groups", self_prefs)) ->
    {arg with sel_prefs = List.flatten (List.rev (Xlist.rev_map self_prefs (load_argument_self_prefs ent frm)))}
  (* | Id id -> {arg with arg_id = id} *)
  | Id{hash=false; numbers=[(*ent_id;*)frm_id;id]; suffix="arg"} ->
     if (*ent_id = ent &&*) frm_id = frm then {arg with arg_id = id}
     else failwith (Printf.sprintf "load_argument_info %d" ent (*ent_id*))
  | xml -> failwith ("load_argument_info :\n " ^ tei_to_string xml)

let load_arguments_set ent frm = function
  | Fs("argument", info) ->
    let result = {arg_id = (-1); role = ""; role_attribute = ""; sel_prefs = []} in
    let result = Xlist.fold info result (load_argument_info ent frm) in
    result
  | xml -> failwith ("load_arguments_set :\n " ^ tei_to_string xml)

let load_meanings_set ent = function
  | SameAs({hash=true; numbers=[(*ent_id;*)id]; suffix="mng"},"lexical_unit") ->
      (*if ent_id = ent then*) id (*else failwith (Printf.sprintf "load_meanings_set %d %d" ent ent_id)*)
  | xml -> failwith ("load_meanings_set :\n " ^ tei_to_string xml)

let load_frame ent = function
  | Fs("frame",[
      Id{hash=false; numbers=[(*ent_id;*)id]; suffix="frm"};
      F("opinion",Symbol opinion);
      Fset("meanings",meanings_set);
      Fset("arguments",arguments_set)]) ->
    (*if ent_id <> ent then failwith (Printf.sprintf "load_frame %d %d" ent ent_id) else*)
    (* Printf.printf "Frame IN %d\n" id; *)
    {frm_id = id;
     opinion = parse_opinion opinion;
     meanings = List.rev (Xlist.rev_map meanings_set (load_meanings_set ent));
     arguments = List.rev (Xlist.rev_map arguments_set (load_arguments_set ent id))}
  | SameAs({hash=true; numbers=[id]; suffix="frm"},frame) -> (* FIXME !! *)
      (* (try IntMap.find frames id with Not_found -> failwith ("load_frame: ^ " ^ string_of_int id)) *)
      (* Printf.printf "Frame OUT %d\n" id; *)
      {frm_id=(-id); opinion=Nieokreslony; meanings=[]; arguments=[]}
  | xml -> failwith ("load_frame :\n " ^ tei_to_string xml)

let load_meaning_info ent arg = function
  | F("name",TEIstring name_string) -> {arg with name = name_string}
  | F("variant",TEIstring variant_string) -> {arg with variant = variant_string}
  | F("plwnluid",Numeric value) -> {arg with plwnluid = value}
  | F("gloss",TEIstring gloss_string) -> {arg with gloss = gloss_string}
  (* | Id{hash=false; numbers=[ent_id;id]; suffix="mng"} -> if ent_id = ent then {arg with mng_id = id} else failwith (Printf.sprintf "load_meaning_info %d %d" ent ent_id) *)
  | Id{hash=false; numbers=[id]; suffix="mng"} -> {arg with mng_id = id}
  | xml -> failwith ("load_meaning_info:\n " ^ tei_to_string xml)


let load_meaning ent = function
  | Fs("lexical_unit", meaning_info) ->
    Xlist.fold meaning_info empty_meaning (load_meaning_info ent)
  | xml -> failwith ("load_meaning:\n " ^ tei_to_string xml)

let load_alter_connection ent = function
  | Fs("connection", [
      F("argument",SameAs({hash=true; numbers=[(*ent_id;*)frm_id;arg_id]; suffix="arg"},"argument"));
      Fset("phrases",phrases)]) ->
   (* if ent_id <> ent then failwith (Printf.sprintf "load_alter_connection %d %d" ent ent_id) else *)
     let phrases,sch_set = Xlist.fold phrases (IntMap.empty,IntSet.empty) (fun (phrases,sch_set) phrase ->
       let sch_id,psn_id,phr_id = load_phrases_set ent phrase in
       IntMap.add_inc phrases psn_id [phr_id] (fun l -> phr_id :: l),
       IntSet.add sch_set sch_id) in
     if IntSet.size sch_set <> 1 then failwith (Printf.sprintf "load_alter_connection: |sch_set|=%d" (IntSet.size sch_set)) else
     IntSet.min_elt sch_set, frm_id,
     {argument = arg_id; phrases = IntMap.fold phrases [] (fun l psn phrs -> (psn,phrs) :: l)}
  | xml -> failwith ("load_alter_connection: \n " ^ tei_to_string xml)

let load_alternations ent = function
  | Fs("alternation",[Fset("connections",connections_set)]) ->
      let conns,sch_set,frm_set = Xlist.fold connections_set ([],IntSet.empty,IntSet.empty) (fun (conns,sch_set,frm_set) conn ->
        let sch_id,frm_id,conn = load_alter_connection ent conn in
        conn :: conns, IntSet.add sch_set sch_id, IntSet.add frm_set frm_id) in
      if IntSet.size sch_set <> 1 then failwith (Printf.sprintf "load_alternations: |sch_set|=%d" (IntSet.size sch_set)) else
      if IntSet.size frm_set <> 1 then failwith (Printf.sprintf "load_alternations: |frm_set|=%d" (IntSet.size sch_set)) else
      {schema=IntSet.min_elt sch_set; frame=IntSet.min_elt frm_set; connections=List.rev conns}
  | xml -> failwith ("load_alternations: \n " ^ tei_to_string xml)

let load_entry phrases = function
  | Xml.Element("entry",["xml:id",id], l) ->
    (* print_endline id; *)
    let id = match parse_id id with
        {hash=false; numbers=[id]; suffix="ent"} -> id
      | _ -> failwith "process_meanings" in
    let entry = {empty_entry with ent_id = id} in
    Xlist.fold l entry (fun e -> function
          Xml.Element("form", [], [
            Xml.Element("orth",[],[Xml.PCData orth]);
            Xml.Element("pos",[],[Xml.PCData pos])]) -> (*print_endline orth;*) {e with form_orth=orth; form_pos=pos}
        | xml -> (match parse_tei xml with
            | Fs("syntactic_layer", [Fset("schemata",schemata_set)]) -> {e with schemata = List.rev (Xlist.rev_map schemata_set (load_schema id phrases))}
            | Fs("examples_layer", [Fset("examples",examples_set)]) -> {e with examples = List.rev (Xlist.rev_map examples_set (load_example id))}
            | Fs("semantic_layer", [Fset("frames",frame_set)]) -> {e with frames = List.rev (Xlist.rev_map frame_set (load_frame id))}
            | Fs("meanings_layer", [Fset("meanings",meanings_set)]) -> {e with meanings = List.rev (Xlist.rev_map meanings_set (load_meaning id))}
            | Fs("connections_layer",[Fset("alternations",alternations)]) -> {e with alternations = List.rev (Xlist.rev_map alternations (load_alternations id))}
            | Fs("general_info",[F("status",TEIstring status)]) -> {e with status=status}
            | xml -> failwith ("load_entry: \n" ^ tei_to_string xml)))
   | xml -> failwith ("load_entry: \n" ^ Xml.to_string_fmt xml)

let add_known_frames known_frames e =
  Xlist.fold e.frames known_frames (fun known_frames f ->
    if f.frm_id < 0 then known_frames else IntMap.add known_frames f.frm_id f)

let expand_frames known_frames e =
  {e with frames =
    List.rev (Xlist.rev_map e.frames (fun f ->
      if f.frm_id < 0 then
        try IntMap.find known_frames (-f.frm_id) with Not_found -> failwith "expand_frames"
      else f))}

let load_walenty filename =
  begin
    match Xml.parse_file filename with
      Xml.Element("TEI", _,
                  [Xml.Element("teiHeader",_,_) ;
                   Xml.Element("text",[],[Xml.Element("body",[],entries)])]) ->
        let phrases = ref IntMap.empty in
        let walenty = Xlist.rev_map entries (load_entry phrases) in
        let known_frames = Xlist.fold walenty IntMap.empty add_known_frames in
        let walenty = Xlist.rev_map walenty (expand_frames known_frames) in
        walenty, !phrases
    | _ -> failwith "load_walenty"
  end

let correct_expansion = function
    [{gf=ARG; cr=[]; ce=[]; morfs=[Phrase(FixedP s)]};p] -> [LexPhrase([FIXED,Lexeme s],(Ratr,[p]))]
  | [{gf=ARG; cr=[]; ce=[]; morfs=[LexPhrase([pos,Lexeme "własny"],(Natr,[]))]};{morfs=[a;b]} as p] ->
    [a;b;LexPhrase([pos,Lexeme "własny"],(Atr,[p]))]
  | _ -> failwith "correct_expansion"

let load_expansion = function
    Fs("expansion",[F("opinion",Symbol opinion);Fset("phrases",[p])]) -> [load_phrase (ref []) p]
  | Fs("expansion",[F("opinion",Symbol opinion);Fset("positions",set)]) -> correct_expansion (List.rev (Xlist.rev_map set (load_position (-1) (-1) (ref IntMap.empty))))
  | tei -> failwith ("load_expansion: \n" ^ tei_to_string tei)

let load_rentry = function
  | Xml.Element("entry",["xml:id",id], [phrase;exp]) ->
    let id = match parse_id id with
        {hash=false; numbers=[id]; suffix="exp"} -> id
      | _ -> failwith "process_meanings" in
    let morf = load_phrase (ref []) (parse_tei phrase) in
    let expansions = match parse_tei exp with
        | Fs("phrase_type_expansions", [Fset("expansions",expansions)]) -> List.flatten (List.rev (Xlist.rev_map expansions load_expansion))
        | Fs("phrase_type_expansions", [F("expansions",expansion)]) -> load_expansion expansion
        | tei -> failwith ("load_entry: \n" ^ tei_to_string tei) in
    id,morf,expansions
  | xml -> failwith ("load_entry: \n" ^ Xml.to_string_fmt xml)

let expands_supplement = [
  (-2), PhraseAbbr(Nonch,[]), [
    LexPhrase([SUBST(NumberUndef,Str),Lexeme "co"],(Natr,[]));
    LexPhrase([SUBST(NumberUndef,Str),Lexeme "coś"],(Natr,[]));
    LexPhrase([SUBST(NumberUndef,Str),Lexeme "nic"],(Natr,[]));
    LexPhrase([SUBST(NumberUndef,Str),Lexeme "to"],(Natr,[]));
    ];
  (-3), Phrase (AdvP "pron"), [
    LexPhrase([ADV (Grad "pos"),Lexeme "tak"],(Natr,[]));
    LexPhrase([ADV (Grad "pos"),Lexeme "jak"],(Natr,[]))
    ]]

let load_expands filename =
  begin
    match Xml.parse_file filename with
      Xml.Element("TEI", _,
                  [Xml.Element("teiHeader",_,_) ;
                   Xml.Element("text",[],[Xml.Element("body",[],entries)])]) ->
      expands_supplement @ List.rev (Xlist.rev_map entries load_rentry)
    | _ -> failwith "load_walenty"
  end

let subtypes = [
  "int",[
    "co"; "czemu"; "czy"; "czyj"; "dlaczego"; "dokąd"; "gdzie"; "ile"; "jak";
    "jaki"; "kiedy"; "kto"; "którędy"; "który"; "odkąd"; "skąd"; "jakoby"];
  "rel",[
    "co"; "dokąd"; "gdzie"; "jak"; "jakby"; "jaki"; "jakoby"; "kiedy"; "kto";
    "którędy"; "który"; "odkąd"; "skąd"]]

let equivs = ["jak",["niczym"]; "przeciw",["przeciwko"]]
