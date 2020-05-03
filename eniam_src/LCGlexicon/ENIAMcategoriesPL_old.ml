(*
 *  ENIAM_LCGlexicon is a library that provides LCG lexicon form Polish
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

open ENIAM_LCGlexiconTypes
open Xstd

let all_numbers = ["sg";"pl"]
let all_cases = ["nom";"gen";"dat";"acc";"inst";"loc";"voc"]
(* let all_genders = ["m1";"m2";"m3";"f";"n1";"n2";"p1";"p2";"p3"] *)
let all_genders = ["m1";"m2";"m3";"f";"n"]
let all_persons = ["pri";"sec";"ter"]
(* FIXME: zamiast wszystkich możliwych wartości można używać Zero gdy nie ma uzgodnienia *)

let selector_values = Xlist.fold [
    Lemma, [];
    Pos, ["subst";"depr";"ppron12";"ppron3";"siebie";"prep";"fixed";"num";"numcomp";"intnum";
          "realnum";"intnum-interval";"realnum-interval";"symbol";"ordnum";
          "date";"date-interval";"hour-minute";"hour";"hour-minute-interval";
          "hour-interval";"year";"year-interval";"day";"day-interval";"day-month";
          "day-month-interval";"month-interval";"roman";"roman-interval";"roman-ordnum";
          "match-result";"url";"email";"phone-number";"postal-code";"obj-id";"building-number";"list-item";"adj";"adjc";"adjp";"adja";
          "adv";"ger";"pact";"ppas";"fin";"bedzie";"praet";"winien";"impt";
          "imps";"pred";"aglt";"inf";"pcon";"pant";"qub";"part";"comp";"conj";"interj";
          "sinterj";"burk";"interp";"xxx";"unk";"html-tag";"apron";"compar"];
    Pos2, [];
    Cat, [];
    Coerced, [];
    Number, all_numbers;
    Case, "postp" :: "pred" :: all_cases;
    Gender, all_genders;
    Person, all_persons;
    Grad, ["pos";"com";"sup"];
    Praep, ["praep";"npraep";"praep-npraep"];
    Acm, ["congr";"rec"];
    Ctype, ["int";"rel";"sub";"coord"];
    Mode, ["abl";"adl";"locat";"perl";"dur";"temp";"mod"];
    Aspect, ["perf";"imperf"];
    Negation, ["neg";"aff"];
    Mood, ["indicative";"imperative";"conditional"];
    Tense, ["past";"pres";"fut"];
    Nsyn, ["proper";"pronoun";"common"];
    Nsem, ["count";"time";"mass";"measure"];
    Psem, ["sem";"nosem"];
    Ucase, all_cases;
] SelectorMap.empty (fun map (selector,vals) -> SelectorMap.add map selector vals)


let expand_numbers numbers =
  if Xlist.mem numbers "_" then all_numbers else numbers

let expand_genders genders  =
  if Xlist.mem genders "_" then all_genders else genders

let expand_cases cases  =
  if Xlist.mem cases "_" || Xlist.mem cases "$C" then all_cases else cases

let expand_akcs akcs  =
  if Xlist.mem akcs "_" then ["akc";"nakc"] else akcs

let split_voc cases =
  Xlist.fold cases ([],[]) (fun (cases,voc) -> function
        "voc" -> cases, "voc" :: voc
      | s -> s :: cases, voc)

let load_subst_data filename _ =
  StringSet.of_list (File.load_lines filename)

let subst_uncountable_lexemes = ref StringSet.empty
let subst_uncountable_lexemes2 = ref StringSet.empty
let subst_container_lexemes = ref StringSet.empty
let subst_numeral_lexemes = ref StringSet.empty
let subst_time_lexemes = ref StringSet.empty

let subst_pronoun_lexemes = StringSet.of_list ["co"; "kto"; "cokolwiek"; "ktokolwiek"; "nic"; "nikt"; "coś"; "ktoś"; "to"]
let adj_pronoun_lexemes = StringSet.of_list ["czyj"; "jaki"; "który"; "jakiś"; "ten"; "taki"]
let compar_lexemes = StringSet.of_list ["jak"; "jako"; "niż"; "niczym"; "niby"; "co"; "zamiast"]

(* let adj_quant_lexemes = StringSet.of_list ["każdy"; "wszelki"; "wszystek"; "żaden"; "jakiś"; "pewien"; "niektóry"; "jedyny"; "sam"] *)

let load_adv_modes filename adv_modes =
  File.fold_tab filename adv_modes (fun adv_modes -> function
      [adv;mode] -> StringMap.add_inc adv_modes adv [mode] (fun l -> mode :: l)
    | _ -> failwith "load_adv_modes")

let load_num_nsems filename num_nsems =
  File.fold_tab filename num_nsems (fun num_nsems -> function
      lemma :: _ :: nsems :: _ ->
        Xlist.fold (Xstring.split "," nsems) num_nsems (fun num_nsems nsem ->
          StringMap.add_inc num_nsems lemma [nsem] (fun l -> nsem :: l))
    | _ -> failwith "load_num_nsems")

let adv_modes = ref (StringMap.empty : string list StringMap.t)
let num_nsems = ref (StringMap.empty : string list StringMap.t)

let initialize () =
  subst_uncountable_lexemes := File.catch_no_file (load_subst_data subst_uncountable_lexemes_filename) StringSet.empty;
  subst_uncountable_lexemes2 := File.catch_no_file (load_subst_data subst_uncountable_lexemes_filename2) StringSet.empty;
  subst_container_lexemes := File.catch_no_file (load_subst_data subst_container_lexemes_filename) StringSet.empty;
  subst_numeral_lexemes := File.catch_no_file (load_subst_data subst_numeral_lexemes_filename) StringSet.empty;
  subst_time_lexemes := File.catch_no_file (load_subst_data subst_time_lexemes_filename) StringSet.empty;
  adv_modes := File.catch_no_file (load_adv_modes adv_modes_filename) StringMap.empty;
  num_nsems := File.catch_no_file (load_num_nsems num_nsems_filename) StringMap.empty;
  ()

let noun_type proper lemma pos =
  let nsyn =
    if proper then "proper" else
    if pos = "ppron12" || pos = "ppron3" || pos = "siebie" then "pronoun" else
    if pos = "symbol" || pos = "date" || pos = "date-interval" || pos = "hour" || pos = "hour-minute" || pos = "hour-interval" || pos = "hour-minute-interval" ||
       pos = "year" || pos = "year-interval" || pos = "day" || pos = "day-interval" || pos = "day-month" || pos = "day-month-interval" ||
       pos = "match-result" || pos = "month-interval" || pos = "roman" || pos = "roman-interval" || pos = "url" || pos = "email" || pos = "phone-number" || pos = "postal-code" || pos = "obj-id" || pos = "building-number" || pos = "date" then "proper" else
    if StringSet.mem subst_pronoun_lexemes lemma then "pronoun" else
    "common" in
  let nsem =
    if pos = "ppron12" || pos = "ppron3" || pos = "siebie" then ["count"] else
    if StringSet.mem !subst_time_lexemes lemma then ["time"] else
    let l = ["count"] in
    let l = if StringSet.mem !subst_uncountable_lexemes lemma || StringSet.mem !subst_uncountable_lexemes2 lemma then "mass" :: l else l in
    if StringSet.mem !subst_container_lexemes lemma then "measure" :: l else l in
  [nsyn],nsem

let adv_mode lemma =
  try
    StringMap.find !adv_modes lemma
  with Not_found -> ["mod"]

let num_nsem lemma =
  try
    StringMap.find !num_nsems lemma
  with Not_found -> (*try
    StringMap.find !num_nsems (String.lowercase lemma)
  with Not_found ->*) failwith ("num_nsem: " ^ lemma)


let part_set = StringSet.of_list ["się"; "nie"; "by"; "niech"; "niechaj"; "niechże"; "niechajże"; "czy"; "gdyby"]

let clarify_categories proper cat coerced = function
    lemma,"subst",[numbers;cases;genders] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let cases,voc = split_voc cases in
      let nsyn,nsem = noun_type proper lemma "subst" in
      (if cases = [] then [] else
         [{empty_cats with lemma=lemma; pos="subst"; pos2="noun"; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; nsyn=nsyn; nsem=nsem}]) @
      (if voc = [] then [] else
         [{empty_cats with lemma=lemma; pos="subst"; pos2="noun"; cat=cat; coerced=coerced; numbers=numbers; cases=voc; genders=genders; persons=["sec"]; nsyn=nsyn; nsem=nsem}])
  | lemma,"subst",[numbers;cases;genders;_] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let cases,voc = split_voc cases in
      let nsyn,nsem = noun_type proper lemma "subst" in
      (if cases = [] then [] else
         [{empty_cats with lemma=lemma; pos="subst"; pos2="noun"; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; nsyn=nsyn; nsem=nsem}]) @
      (if voc = [] then [] else
         [{empty_cats with lemma=lemma; pos="subst"; pos2="noun"; cat=cat; coerced=coerced; numbers=numbers; cases=voc; genders=genders; persons=["sec"]; nsyn=nsyn; nsem=nsem}])
  | lemma,"depr",[numbers;cases;genders] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let cases,voc = split_voc cases in
      let nsyn,nsem = noun_type proper lemma "depr" in
      (if cases = [] then [] else
         [{empty_cats with lemma=lemma; pos="subst"; pos2="noun"; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; nsyn=nsyn; nsem=nsem}]) @
      (if voc = [] then [] else
         [{empty_cats with lemma=lemma; pos="subst"; pos2="noun"; cat=cat; coerced=coerced; numbers=numbers; cases=voc; genders=genders; persons=["sec"]; nsyn=nsyn; nsem=nsem}])
  | lemma,"ppron12",[numbers;cases;genders;persons] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="ppron12"; pos2="pron"; numbers=numbers; cases=cases; genders=genders; persons=persons}]
  | lemma,"ppron12",[numbers;cases;genders;persons;akcs] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="ppron12"; pos2="pron"; numbers=numbers; cases=cases; genders=genders; persons=persons}]
  | lemma,"ppron3",[numbers;cases;genders;persons] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="ppron3"; pos2="pron"; numbers=numbers; cases=cases; genders=genders; persons=persons; praeps=["praep-npraep"]}]
  | lemma,"ppron3",[numbers;cases;genders;persons;akcs] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="ppron3"; pos2="pron"; numbers=numbers; cases=cases; genders=genders; persons=persons; praeps=["praep-npraep"]}]
  | lemma,"ppron3",[numbers;cases;genders;persons;akcs;praep] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let praep = match praep with
        ["praep";"npraep"] -> ["praep-npraep"]
      | ["npraep";"praep"] -> ["praep-npraep"]
      | _ -> praep in
      [{empty_cats with lemma=lemma; pos="ppron3"; pos2="pron"; numbers=numbers; cases=cases; genders=genders; persons=persons; praeps=praep}]
  | lemma,"siebie",[cases] -> (* FIXME: czy tu określać numbers genders persons? *)
      let cases = expand_cases cases in
      [{empty_cats with lemma=lemma; pos="siebie"; pos2="pron"; numbers=all_numbers; cases=cases; genders=all_genders; persons=["ter"]}]
  | lemma,"prep",[cases;woks] ->
      if StringSet.mem compar_lexemes lemma then
        [{empty_cats with lemma=lemma; pos="compar"; pos2="prep"}] else
      let cases = expand_cases cases in
      [{empty_cats with lemma=lemma; pos="prep"; pos2="prep"; cases=cases; psem=["sem";"nosem"]}]
  | lemma,"prep",[cases] ->
      if StringSet.mem compar_lexemes lemma then
        [{empty_cats with lemma=lemma; pos="compar"; pos2="prep"}] else
      let cases = expand_cases cases in
      [{empty_cats with lemma=lemma; pos="prep"; pos2="prep"; cases=cases; psem=["sem";"nosem"]}]
  | lemma,"num",[numbers;cases;genders;acms] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let nsem = num_nsem lemma in
      [{empty_cats with lemma=lemma; pos="num"; pos2="num"; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; acms=acms; nsem=nsem}]
  | lemma,"num",[numbers;cases;genders;acms;_] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      let nsem = num_nsem lemma in
      [{empty_cats with lemma=lemma; pos="num"; pos2="num"; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; acms=acms; nsem=nsem}]
  | lemma,"numcomp",[] -> [{empty_cats with lemma=lemma; pos="numcomp"; pos2="numcomp"}]
  | lemma,"intnum",[] ->
      let numbers,acms =
        if lemma = "1" || lemma = "-1" then ["sg"],["congr"] else
        let s = String.get lemma (String.length lemma - 1) in
        ["pl"],if s = '2' || s = '3' || s = '4' then ["rec";"congr"] else ["rec"] in
      [{empty_cats with lemma=lemma; pos="intnum"; pos2="num"; numbers=numbers; cases=all_cases; genders=all_genders; persons=["ter"]; acms=acms; nsem=["count"]}]
  | lemma,"realnum",[] ->
      [{empty_cats with lemma=lemma; pos="realnum"; pos2="num"; numbers=["sg"]; cases=all_cases; genders=all_genders; persons=["ter"]; acms=["rec"]; nsem=["count"]}]
  | lemma,"intnum-interval",[] ->
      [{empty_cats with lemma=lemma; pos="intnum-interval"; pos2="num"; numbers=["pl"]; cases=all_cases; genders=all_genders; persons=["ter"]; acms=["rec";"congr"]; nsem=["count"]}]
  | lemma,"realnum-interval",[] ->
      [{empty_cats with lemma=lemma; pos="realnum-interval"; pos2="num"; numbers=["sg"]; cases=all_cases; genders=all_genders; persons=["ter"]; acms=["rec"]; nsem=["count"]}]
  | lemma,"symbol",[] ->
      [{empty_cats with lemma=lemma; pos="symbol"; pos2="noun"; numbers=["sg"]; cases=all_cases; genders=all_genders; persons=["ter"]}]
  | lemma,"ordnum",[] ->
      [{empty_cats with lemma=lemma; pos="ordnum"; pos2="adj"; numbers=all_numbers; cases=all_cases; genders=all_genders; grads=["pos"]}] (* FIXME: czy dać możliwość więcej niż jednego stopnia *)
  | lemma,"date",[] ->
      let nsyn,nsem = noun_type proper lemma "date" in
      [{empty_cats with lemma=lemma; pos="date"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"date-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "date-interval" in
      [{empty_cats with lemma=lemma; pos="date-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"hour-minute",[] ->
      let nsyn,nsem = noun_type proper lemma "hour-minute" in
      [{empty_cats with lemma=lemma; pos="hour-minute"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"hour",[] ->
      let nsyn,nsem = noun_type proper lemma "hour" in
      [{empty_cats with lemma=lemma; pos="hour"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"hour-minute-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "hour-minute-interval" in
      [{empty_cats with lemma=lemma; pos="hour-minute-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"hour-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "hour-interval" in
      [{empty_cats with lemma=lemma; pos="hour-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"year",[] ->
      let nsyn,nsem = noun_type proper lemma "year" in
      [{empty_cats with lemma=lemma; pos="year"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"year-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "year-interval" in
      [{empty_cats with lemma=lemma; pos="year-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"day",[] ->
      let nsyn,nsem = noun_type proper lemma "day" in
      [{empty_cats with lemma=lemma; pos="day"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"day-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "day-interval" in
      [{empty_cats with lemma=lemma; pos="day-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"day-month",[] ->
      let nsyn,nsem = noun_type proper lemma "day-month" in
      [{empty_cats with lemma=lemma; pos="day-month"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"day-month-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "day-month-interval" in
      [{empty_cats with lemma=lemma; pos="day-month-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"month-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "month-interval" in
      [{empty_cats with lemma=lemma; pos="month-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"roman",[] ->
      let nsyn,nsem = noun_type proper lemma "roman" in
      [{empty_cats with lemma=lemma; pos="roman-ordnum"; pos2="adj"; numbers=all_numbers; cases=all_cases; genders=all_genders; grads=["pos"]};
       {empty_cats with lemma=lemma; pos="roman"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"roman-interval",[] ->
      let nsyn,nsem = noun_type proper lemma "roman-interval" in
      [{empty_cats with lemma=lemma; pos="roman-interval"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"match-result",[] ->
      let nsyn,nsem = noun_type proper lemma "match-result" in
      [{empty_cats with lemma=lemma; pos="match-result"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"url",[] ->
      let nsyn,nsem = noun_type proper lemma "url" in
      [{empty_cats with lemma=lemma; pos="url"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"email",[] ->
      let nsyn,nsem = noun_type proper lemma "email" in
      [{empty_cats with lemma=lemma; pos="email"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"phone-number",[] ->
      let nsyn,nsem = noun_type proper lemma "phone-number" in
      [{empty_cats with lemma=lemma; pos="phone-number"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"postal-code",[] ->
      let nsyn,nsem = noun_type proper lemma "postal-code" in
      [{empty_cats with lemma=lemma; pos="postal-code"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"obj-id",[] ->
      let nsyn,nsem = noun_type proper lemma "obj-id" in
      [{empty_cats with lemma=lemma; pos="obj-id"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"building-number",[] ->
      let nsyn,nsem = noun_type proper lemma "building-number" in
      [{empty_cats with lemma=lemma; pos="building-number"; pos2="symbol"; nsyn=nsyn; nsem=nsem}]
  | lemma,"fixed",[] -> [{empty_cats with lemma=lemma; pos="fixed"; pos2="fixed"}]
  | lemma,"adj",[numbers;cases;genders;grads] -> (* FIXME: adjsyn *)
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      let pos,pos2 = if StringSet.mem adj_pronoun_lexemes lemma then "apron","pron" else "adj","adj" in
      [{empty_cats with lemma=lemma; pos=pos; pos2=pos2; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; grads=grads}] (* FIXME: czy dać możliwość więcej niż jednego stopnia *)
  | lemma,"adjc",[] ->
      [{empty_cats with lemma=lemma; pos="adjc"; pos2="adj"; cat=cat; coerced=coerced; numbers=["sg"]; cases=["pred"]; genders=["m1";"m2";"m3"]; grads=["pos"]}]
  | lemma,"adjp",[] ->
      [{empty_cats with lemma=lemma; pos="adjp"; pos2="adj"; cat=cat; coerced=coerced; numbers=all_numbers; cases=["postp"]; genders=all_genders; grads=["pos"]}]
  | lemma,"adja",[] -> [{empty_cats with lemma=lemma; cat=cat; coerced=coerced; pos="adja"; pos2="adja"}]
  | lemma,"adv",[grads] -> [{empty_cats with lemma=lemma; cat=cat; coerced=coerced; pos="adv"; pos2="adv"; grads=grads; modes=adv_mode lemma}]
  | lemma,"adv",[] -> [{empty_cats with lemma=lemma; cat=cat; coerced=coerced; pos="adv"; pos2="adv"; grads=["pos"]; modes=adv_mode lemma}]
  | lemma,"ger",[numbers;cases;genders;aspects;negations] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="ger"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; persons=["ter"]; aspects=aspects; negations=negations}] (* FIXME: kwestia osoby przy voc *)
  | lemma,"pact",[numbers;cases;genders;aspects;negations] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="pact"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; aspects=aspects; negations=negations}]
  | lemma,"ppas",[numbers;cases;genders;aspects;negations] ->
      let numbers = expand_numbers numbers in
      let cases = expand_cases cases in
      let cases = if Xlist.mem cases "nom" then "pred" :: cases else cases in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="ppas"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; cases=cases; genders=genders; aspects=aspects; negations=negations}]
  | lemma,"fin",[numbers;persons;aspects] ->  (* FIXME: genders bez przymnogich *)
      let numbers = expand_numbers numbers in
      let persons2 = Xlist.fold persons [] (fun l -> function "sec" -> l | s -> s :: l) in
      let cats = {empty_cats with lemma=lemma; pos="fin"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=all_genders; persons=persons; negations=["aff"; "neg"]; moods=["indicative"]} in
      (Xlist.map aspects (function
            "imperf" -> {cats with aspects=["imperf"]; tenses=["pres"]}
          | "perf" -> {cats with aspects=["perf"]; tenses=["fut"]}
          | _ -> failwith "clarify_categories")) @
      (if persons2 = [] then [] else
        [{empty_cats with lemma=lemma; pos="fin"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["imperative"]; tenses=["fut"]}])
  | lemma,"bedzie",[numbers;persons;aspects] ->
      let numbers = expand_numbers numbers in
      let persons2 = Xlist.fold persons [] (fun l -> function "sec" -> l | s -> s :: l) in
      [{empty_cats with lemma=lemma; pos="bedzie"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}] @
      (if persons2 = [] then [] else
        [{empty_cats with lemma=lemma; pos="bedzie"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["imperative"]; tenses=["fut"]}])
  | lemma,"praet",[numbers;genders;aspects;nagl] ->
      let numbers = expand_numbers numbers in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="praet"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative";"conditional"]; tenses=["past"]}] @
      (if Xlist.mem aspects "imperf" then
        [{empty_cats with lemma=lemma; pos="praet"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}]
       else [])
  | lemma,"praet",[numbers;genders;aspects] ->
      let numbers = expand_numbers numbers in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="praet"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative";"conditional"]; tenses=["past"]}] @
      (if Xlist.mem aspects "imperf" then
        [{empty_cats with lemma=lemma; pos="praet"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}]
       else [])
  | lemma,"winien",[numbers;genders;aspects] ->
      let numbers = expand_numbers numbers in
      let genders = expand_genders genders in
      [{empty_cats with lemma=lemma; pos="winien"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative";"conditional"]; tenses=["pres"]};
       {empty_cats with lemma=lemma; pos="winien"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["past"]}] @
      (if Xlist.mem aspects "imperf" then
        [{empty_cats with lemma=lemma; pos="winien"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=genders; persons=all_persons; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["fut"]}]
       else [])
  | lemma,"impt",[numbers;persons;aspects] ->
      let numbers = expand_numbers numbers in
      [{empty_cats with lemma=lemma; pos="impt"; pos2="verb"; cat=cat; coerced=coerced; numbers=numbers; genders=all_genders; persons=persons; aspects=aspects; negations=["aff"; "neg"]; moods=["imperative"]; tenses=["fut"]}]
  | lemma,"imps",[aspects] ->
      [{empty_cats with lemma=lemma; pos="imps"; pos2="verb"; cat=cat; coerced=coerced; numbers=all_numbers; genders=all_genders; persons=all_persons; aspects=aspects; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["past"]}]
  | lemma,"pred",[] -> (* FIXME: czy predykatyw zawsze jest niedokonany? *)
      [{empty_cats with lemma=lemma; pos="pred"; pos2="verb"; cat=cat; coerced=coerced; numbers=["sg"]; genders=[(*"n2"*)"n"]; persons=["ter"]; aspects=["imperf"]; negations=["aff"; "neg"]; moods=["indicative"]; tenses=["pres";"past";"fut"]}]
  | lemma,"aglt",[numbers;persons;aspects;wok] ->
      let numbers = expand_numbers numbers in
      [{empty_cats with lemma=lemma; pos="aglt"; pos2="verb"; numbers=numbers; persons=persons; aspects=aspects}]
  | lemma,"inf",[aspects] -> [{empty_cats with lemma=lemma; pos="inf"; pos2="verb"; cat=cat; coerced=coerced; aspects=aspects; negations=["aff"; "neg"]}]
  | lemma,"pcon",[aspects] -> [{empty_cats with lemma=lemma; pos="pcon"; pos2="verb"; cat=cat; coerced=coerced; aspects=aspects; negations=["aff"; "neg"]}]
  | lemma,"pant",[aspects] -> [{empty_cats with lemma=lemma; pos="pant"; pos2="verb"; cat=cat; coerced=coerced; aspects=aspects; negations=["aff"; "neg"]}]
  | lemma,"qub",[] ->
      if StringSet.mem part_set lemma then [{empty_cats with lemma=lemma; pos="part"; pos2="qub"}]
      else [{empty_cats with lemma=lemma; pos="qub"; pos2="qub"; cat=cat}]
  | lemma,"comp",[] -> [{empty_cats with lemma=lemma; pos="comp"; pos2="comp"}]
  | lemma,"conj",[] -> [{empty_cats with lemma=lemma; pos="conj"; pos2="conj"}]
  | lemma,"interj",[] -> [{empty_cats with lemma=lemma; pos="interj"; pos2="interj"; cat=cat; coerced=coerced}]
  | lemma,"sinterj",[] -> [{empty_cats with lemma=lemma; pos="sinterj"; pos2="sinterj"; (*cat=cat; coerced=coerced*)}]
  | lemma,"burk",[] -> [{empty_cats with lemma=lemma; pos="burk"; pos2="burk"}]
  | ",","interp",[] -> [{empty_cats with lemma=","; pos="conj"; pos2="conj"}]
  | lemma,"interp",[] -> [{empty_cats with lemma=lemma; pos="interp"; pos2="interp"}]
  | lemma,"unk",[] ->
      [{empty_cats with lemma=lemma; pos="unk"; pos2="noun"; numbers=all_numbers; cases=all_cases; genders=all_genders; persons=["ter"]}]
  | lemma,"xxx",[] ->
      [{empty_cats with lemma=lemma; pos="xxx"; pos2="noun"; numbers=all_numbers; cases=all_cases; genders=all_genders; persons=["ter"]}]
  | lemma,"html-tag",[] -> [{empty_cats with lemma=lemma; pos="html-tag"; pos2="html-tag"}]
  | lemma,"list-item",[] -> [{empty_cats with lemma=lemma; pos="list-item"; pos2="list-item"}]
  | lemma,c,l -> failwith ("clarify_categories: " ^ lemma ^ ":" ^ c ^ ":" ^ (String.concat ":" (Xlist.map l (String.concat "."))))

(* FIXME: przenieść gdzieś indziej *)
(* let assign token =
  match token.ENIAMtokenizerTypes.token with
    ENIAMtokenizerTypes.Lemma(lemma,pos,interp) -> List.flatten (Xlist.map interp (fun interp -> clarify_categories false (lemma,pos,interp)))
  | ENIAMtokenizerTypes.Proper(lemma,pos,interp,_) -> List.flatten (Xlist.map interp (fun interp -> clarify_categories true (lemma,pos,interp)))
  | ENIAMtokenizerTypes.Interp lemma -> clarify_categories false (lemma,"interp",[])
  | _ -> [] *)

let selector_names = StringSet.of_list [
    "lemma";"pos";"pos2";"cat";"coerced";"number";"case";"gender";"person";"grad";
    "praep";"acm";"aspect";"negation";"mood";"tense";"nsyn";"nsem";"ctype";"mode";"psem";
    "icat";"inumber";"igender";"iperson";"nperson";"ncat";"plemma";
    "unumber";"ucase";"ugender";"uperson";"amode"]


let string_of_selector = function
    Lemma -> "lemma"
  (* | NewLemma -> "newlemma" *)
  | Pos -> "pos"
  | Pos2 -> "pos2"
  | Cat -> "cat"
  | Coerced -> "coerced"
  | Number -> "number"
  | Case -> "case"
  | Gender -> "gender"
  | Person -> "person"
  | Grad -> "grad"
  | Praep -> "praep"
  | Acm -> "acm"
  | Aspect -> "aspect"
  | Negation -> "negation"
  | Mood -> "mood"
  | Tense -> "tense"
  | Nsyn -> "nsyn"
  | Nsem -> "nsem"
  | Ctype -> "ctype"
  | Mode -> "mode"
  | Psem -> "psem"
  | Icat -> "icat"
  | Inumber -> "inumber"
  | Igender -> "igender"
  | Iperson -> "iperson"
  | Nperson -> "nperson"
  | Ncat -> "ncat"
  | Plemma -> "plemma"
  | Unumber -> "unumber"
  | Ucase -> "ucase"
  | Ugender -> "ugender"
  | Uperson -> "uperson"
  | Amode -> "amode"

let string_of_selectors selectors =
  String.concat ", " (Xlist.map selectors (fun (cat,rel,l) ->
      let rel = if rel = Eq then "=" else "!=" in
      string_of_selector cat ^ rel ^ (String.concat "|" l)))

let selector_of_string = function
    "lemma" -> Lemma
  (* | NewLemma -> "newlemma" *)
  | "pos" -> Pos
  | "pos2" -> Pos2
  | "cat" -> Cat
  | "coerced" -> Coerced
  | "number" -> Number
  | "case" -> Case
  | "gender" -> Gender
  | "person" -> Person
  | "grad" -> Grad
  | "praep" -> Praep
  | "acm" -> Acm
  | "aspect" -> Aspect
  | "negation" -> Negation
  | "mood" -> Mood
  | "tense" -> Tense
  | "nsyn" -> Nsyn
  | "nsem" -> Nsem
  | "ctype" -> Ctype
  | "mode" -> Mode
  | "psem" -> Psem
  | "icat" -> Icat
  | "inumber" -> Inumber
  | "igender" -> Igender
  | "iperson" -> Iperson
  | "nperson" -> Nperson
  | "ncat" -> Ncat
  | "plemma" -> Plemma
  | "unumber" -> Unumber
  | "ucase" -> Ucase
  | "ugender" -> Ugender
  | "uperson" -> Uperson
  | "amode" -> Amode
  | s -> failwith ("selector_of_string: " ^ s)

let match_selector cats = function
    Lemma -> [cats.lemma]
(* | NewLemma -> [] *)
  | Pos -> [cats.pos]
  | Cat -> [cats.cat]
  | Coerced -> cats.coerced
  | Number -> cats.numbers
  | Case -> cats.cases
  | Gender -> cats.genders
  | Person -> cats.persons
  | Grad -> cats.grads
  | Praep -> cats.praeps
  | Acm -> cats.acms
  | Aspect -> cats.aspects
  | Negation -> cats.negations
  | Mood -> cats.moods
  | Tense -> cats.tenses
  | Nsyn -> cats.nsyn
  | Nsem -> cats.nsem
  | Mode -> cats.modes
  | Psem -> cats.psem
  | c -> failwith ("match_selector: " ^ string_of_selector c)

let set_selector cats vals = function
    Number -> {cats with numbers=vals}
  | Case -> {cats with cases=vals}
  | Gender -> {cats with genders=vals}
  | Person -> {cats with persons=vals}
  | Grad -> {cats with grads=vals}
  | Praep -> {cats with praeps=vals}
  | Acm -> {cats with acms=vals}
  | Aspect -> {cats with aspects=vals}
  | Negation -> {cats with negations=vals}
  | Mood -> {cats with moods=vals}
  | Tense -> {cats with tenses=vals}
  | Nsyn -> {cats with nsyn=vals}
  | Nsem -> {cats with nsem=vals}
  | Mode -> {cats with modes=vals}
  | Psem -> {cats with psem=vals}
  | Lemma -> (match vals with [v] -> {cats with lemma=v} | _ -> failwith "set_selector: Lemma")
  | Pos -> (match vals with [v] -> {cats with pos=v} | _ -> failwith "set_selector: Pos")
  | Cat -> (match vals with [v] -> {cats with cat=v} | _ -> failwith "set_selector: Cat")
  | Coerced -> {cats with coerced=vals}
  | c -> failwith ("set_selector: " ^ string_of_selector c)

let rec apply_selectors cats = function
    [] -> cats
  | (sel,Eq,vals) :: l ->
    let vals = StringSet.intersection (StringSet.of_list (match_selector cats sel)) (StringSet.of_list vals) in
    if StringSet.is_empty vals then raise Not_found else
      apply_selectors (set_selector cats (StringSet.to_list vals) sel) l
  | (sel,Neq,vals) :: l ->
    let vals = StringSet.difference (StringSet.of_list (match_selector cats sel)) (StringSet.of_list vals) in
    if StringSet.is_empty vals then raise Not_found else
      apply_selectors (set_selector cats (StringSet.to_list vals) sel) l

let pos_categories = Xlist.fold [
    "subst",[Lemma;Cat;Coerced;Number;Case;Gender;Person;Nsyn;Nsem;];
    "depr",[Lemma;Cat;Coerced;Number;Case;Gender;Person;Nsyn;Nsem;];
    "ppron12",[Lemma;Number;Case;Gender;Person;];
    "ppron3",[Lemma;Number;Case;Gender;Person;Praep;];
    "siebie",[Lemma;Number;Case;Gender;Person;];
    "prep",[Lemma;Cat;Coerced;Psem;Case;];
    "compar",[Lemma;Cat;Coerced;Case;];
    "num",[Lemma;Number;Case;Gender;Person;Acm;Nsem;];
    "numcomp",[Lemma];
    "intnum",[Lemma;Number;Case;Gender;Person;Acm;Nsem;];
    "realnum",[Lemma;Number;Case;Gender;Person;Acm;Nsem;];
    "intnum-interval",[Lemma;Number;Case;Gender;Person;Acm;Nsem;];
    "realnum-interval",[Lemma;Number;Case;Gender;Person;Acm;Nsem;];
    "symbol",[Lemma;Number;Case;Gender;Person;];
    "ordnum",[Lemma;Number;Case;Gender;Grad;];
    "date",[Lemma;Nsyn;Nsem;];
    "date-interval",[Lemma;Nsyn;Nsem;];
    "hour-minute",[Lemma;Nsyn;Nsem;];
    "hour",[Lemma;Nsyn;Nsem;];
    "hour-minute-interval",[Lemma;Nsyn;Nsem;];
    "hour-interval",[Lemma;Nsyn;Nsem;];
    "year",[Lemma;Nsyn;Nsem;];
    "year-interval",[Lemma;Nsyn;Nsem;];
    "day",[Lemma;Nsyn;Nsem;];
    "day-interval",[Lemma;Nsyn;Nsem;];
    "day-month",[Lemma;Nsyn;Nsem;];
    "day-month-interval",[Lemma;Nsyn;Nsem;];
    "month-interval",[Lemma;Nsyn;Nsem;];
    "roman-ordnum",[Lemma;Number;Case;Gender;Grad;];
    "roman",[Lemma;Nsyn;Nsem;];
    "roman-interval",[Lemma;Nsyn;Nsem;];
    "match-result",[Lemma;Nsyn;Nsem;];
    "url",[Lemma;Nsyn;Nsem;];
    "email",[Lemma;Nsyn;Nsem;];
    "phone-number",[Lemma;Nsyn;Nsem;];
    "postal-code",[Lemma;Nsyn;Nsem;];
    "obj-id",[Lemma;Nsyn;Nsem;];
    "building-number",[Lemma;Nsyn;Nsem;];
    "fixed",[Lemma;];
    "adj",[Lemma;Cat;Coerced;Number;Case;Gender;Grad;];
    "adjc",[Lemma;Cat;Coerced;Number;Case;Gender;Grad;];
    "adjp",[Lemma;Cat;Coerced;Number;Case;Gender;Grad;];
    "apron",[Lemma;Number;Case;Gender;Grad;];
    "adja",[Lemma;Cat;Coerced;];
    "adv",[Lemma;Cat;Coerced;Grad;Mode];(* ctype *)
    "ger",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Case;Gender;Person;Aspect;Negation;];
    "pact",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Case;Gender;Aspect;Negation;];
    "ppas",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Case;Gender;Aspect;Negation;];
    "fin",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "bedzie",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "praet",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "winien",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "impt",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "imps",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "pred",[Lemma;(*NewLemma;*)Cat;Coerced;Number;Gender;Person;Aspect;Negation;Mood;Tense;];
    "aglt",[Lemma;Number;Person;Aspect;];
    "inf",[Lemma;(*NewLemma;*)Cat;Coerced;Aspect;Negation;];
    "pcon",[Lemma;(*NewLemma;*)Cat;Coerced;Aspect;Negation;];
    "pant",[Lemma;(*NewLemma;*)Cat;Coerced;Aspect;Negation;];
    "qub",[Lemma;Cat;];
    "part",[Lemma;];
    "comp",[Lemma;];(* ctype *)
    "conj",[Lemma;];(* ctype *)
    "interj",[Lemma;Cat;Coerced;];
    "sinterj",[Lemma;];
    "burk",[Lemma;];
    "interp",[Lemma;];
    "unk",[Lemma;Number;Case;Gender;Person;];
    "xxx",[Lemma;Number;Case;Gender;Person;];
    "html-tag",[Lemma;];
    "list-item",[Lemma;];
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)
