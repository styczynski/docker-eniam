(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
 *  Copyright (C) 2016 Daniel Oklesinski <oklesinski dot daniel atSPAMfree gmail dot com>
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

open LCGtypes
open Xstd
open ExecTypes

let eniam = "eniam"
let conll = "conll"

module Strings =
  struct
    type t = string
    let compare a b = Pervasives.compare a b
  end

module StrMap = Map.Make(Strings)

let field_map = StrMap.(empty |> add eniam (ref empty) |> add conll (ref empty))

let add_to_field_map str_mode field content =
  let f_map = StrMap.find str_mode field_map in
  let c_map = if StrMap.mem field !f_map
    then StrMap.find field !f_map
    else let temp = ref StrMap.empty in
      f_map := StrMap.add field temp !f_map; temp in
  if StrMap.mem content !c_map
    then incr (StrMap.find content !c_map)
    else c_map := StrMap.add content (ref 1) !c_map

let print_field_map () =
  StrMap.iter (fun key1 val1 ->
    print_endline key1;
    StrMap.iter (fun key2 val2 ->
      let i = ref 0 in
      print_endline ("\t" ^ key2);
      StrMap.iter (fun key3 val3 ->
        i := !i + !val3;
        print_endline ("\t\t" ^ key3 ^ "\t\t" ^ (string_of_int !val3))
        ) !val2;
      print_endline ("\tsum: " ^ (string_of_int !i))
      ) !val1
    ) field_map;
  print_newline ()

  module Statuses =
    struct
      type t = status
      let compare a b = Pervasives.compare a b
    end

  module StatMap = Xmap.MakeQ(Statuses)

  let stat_map = ref StatMap.empty

  let reset () =
    stat_map := StatMap.empty

  let print_results () =
    print_endline "\nStatistics of CONLL statuses:";
    StatMap.iter !stat_map (fun key value -> print_endline ("\t" ^ (match key with
       Idle -> "Idle"
    | PreprocessingError -> "PreprocessingError"
    | LexiconError -> "LexiconError"
    | ParseError -> "ParseError"
    | ParseTimeout -> "ParseTimeout"
    | NotParsed -> "NotParsed"
    | ReductionError -> "ReductionError"
    | TooManyNodes -> "TooManyNodes"
    | NotReduced -> "NotReduced"
    | SemError -> "SemError"
    | NotTranslated -> "NotTranslated"
    | Parsed -> "Parsed") ^ "\t" ^(string_of_int value) ^ "\n"))

let field_of_node str_mode n = function
    "arole" -> let content = if n.arole = "" then "null" else n.arole in
      add_to_field_map str_mode "arole" content; content
  | _ -> failwith "field_of_node: ni"

let field_of_linear_term str_node field = function
    Node n -> field_of_node str_node n field
  | _ -> failwith "field_of_linear_term: ni"

let field_of_dependency_tree str_node fields dep_tree =
  String.concat "\n" (Xlist.map fields (fun field ->
    Array.fold_left (fun acc x ->
      acc ^ (field_of_linear_term str_node field x) ^ "\n\t\t" ) "" dep_tree))

let field_of_eniam_sentence fields (result : eniam_parse_result) =
  match result.status with
    Idle -> "Idle"
  (* | PreprocessingError -> "PreprocessingError" *)
  | LexiconError -> "LexiconError"
  | ParseError -> "ParseError"
  | ParseTimeout -> "ParseTimeout"
  | NotParsed -> "NotParsed"
  | ReductionError -> "ReductionError"
  | TooManyNodes -> "TooManyNodes"
  | NotReduced -> "NotReduced"
  | SemError -> "SemError"
  (* | NotTranslated -> "NotTranslated"  *)
  | Parsed -> ignore ("Parsed\n\t\t" ^ (field_of_dependency_tree eniam fields result.dependency_tree)); "Parsed\n"
  | _ -> failwith "field_of_eniam_sentence"

let field_of_conll_sentence fields (result : conll_parse_result) =
  stat_map := StatMap.add !stat_map result.status;
  match result.status with
    Idle -> "Idle"
  (* | PreprocessingError -> "PreprocessingError" *)
  | LexiconError -> "LexiconError " ^ result.msg
  | ParseError -> "ParseError " ^ result.msg
  | ParseTimeout -> "ParseTimeout"
  | NotParsed -> "NotParsed"
  | ReductionError -> "ReductionError " ^ result.msg
  | TooManyNodes -> "TooManyNodes"
  | NotReduced -> "NotReduced"
  | SemError -> "SemError"
  (* | NotTranslated -> "NotTranslated"  *)
  | Parsed -> ignore ("Parsed\n\t\t" ^ (field_of_dependency_tree conll fields result.dependency_tree)); "Parsed\n"
  | _ -> failwith "field_of_conll_sentence"


let rec field_of_sentence fields = function
    RawSentence s -> s
  | StructSentence _ -> "StructSentence"
  | DepSentence _ -> "DepSentence"
  | ENIAMSentence result -> field_of_eniam_sentence fields result
  | CONLLSentence result -> field_of_conll_sentence fields result
  | QuotedSentences sentences -> "QuotedSentences"
  | AltSentence l -> String.concat "\n\t" (Xlist.map l (fun (m, s) ->
      Visualization.string_of_mode m ^ "\t" ^ (field_of_sentence fields s)))
  | _ -> failwith "field_of_sentence: ni"

let rec field_of_paragraph fields = function
    RawParagraph s -> print_endline "no fields detected: only raw paragraph"; s
  | StructParagraph sentences ->
      String.concat "\n\t" (Xlist.map sentences (fun p -> field_of_sentence fields p.psentence))
  | AltParagraph l ->
      String.concat "\n" (Xlist.map (List.filter (fun (m,t) -> (*m = ENIAM ||*) m = CONLL) l) (fun (m,t) ->
        Visualization.string_of_mode m ^ "\n\t" ^ (field_of_paragraph fields t)))
      (* field_of_paragraph fields (snd @@ List.find (fun (mode,text) -> mode = ENIAM || mode = CONLL) l) *)

let rec print_fields_rec fields = function
  RawText s -> s
    (* print_endline "no fields detected: only raw text"; *)
| StructText(paragraphs) ->
    String.concat "\n\n" (Xlist.map paragraphs (field_of_paragraph fields)) ^ "\n"
| AltText l ->
    String.concat "\n" (Xlist.map (List.filter (fun (m,t) -> m = Struct || m = CONLL) l) (fun (m,t) ->
      Visualization.string_of_mode m ^ "\n\t" ^ (print_fields_rec fields t)))
    (* print_fields_rec fields (snd @@ List.find (fun (m,t) -> m = Struct (*|| m = ENIAM*) || m = CONLL) l) *)

let print_fields fields text =
  print_endline @@ print_fields_rec fields text
  (* ; print_field_map () *)
