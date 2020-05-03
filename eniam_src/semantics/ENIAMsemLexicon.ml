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

open Xstd
open ENIAM_LCGtypes
open Lexer
open ENIAMwalTypes
open ENIAMlexSemanticsTypes

let remove_comments line =
  try
    let n = String.index line '#' in
    String.sub line 0 n
  with Not_found -> line

let rec manage_tokens = function
    [arg;[T role]] -> [arg,role]
  | arg :: (T role :: arg2) :: tokens -> (arg,role) :: manage_tokens (arg2 :: tokens)
  | _ -> failwith "manage_tokens"

let parse_dir p = function
    T "/" :: tokens -> tokens, {p with dir=Forward_}
  | T "\\" :: tokens -> tokens, {p with dir=Backward_}
  | T "|" :: tokens -> tokens, {p with dir=Both_}
  | tokens -> failwith ("parse_dir: " ^ Lexer.string_of_token_list tokens)

let parse_multi p = function
    T "?" :: tokens -> tokens, {p with is_necessary=Multi}
  | tokens -> tokens,p

let parse_morf p = function
    [T "1"] -> if p.is_necessary=Multi then p else {p with is_necessary=Opt}
  | tokens ->
      let l = Xlist.map (try Lexer.split_symbol (T "*") [] tokens with _ -> failwith "parse_morf: split_symbol *") (function
          [T s] -> Atom s
        | tokens -> failwith ("parse_morf: " ^ Lexer.string_of_token_list tokens)) in
      {p with morfs=LCG (Tensor l) :: p.morfs}

let parse_arg tokens p =
  (* Printf.printf "parse_arg: %s\n" (Lexer.string_of_token_list tokens); *)
  let tokens,p = parse_dir p tokens in
  let tokens,p = parse_multi p tokens in
  match Lexer.find_brackets ["(",")"] [] tokens with
    [B("(",")",tokens)] -> Xlist.fold (try Lexer.split_symbol (T "+") [] tokens with _ -> failwith "parse_arg: split_symbol +") p parse_morf
  | tokens -> parse_morf p tokens


let parse_role p = function
    "adjunct" -> {p with gf=ADJUNCT}
  | "unk" -> {p with role="unk"}
  | "null" -> {p with role="null"}
  | "nosem" -> {p with gf=NOSEM}
  | "CORE" -> {p with role="CORE"}
  | "Poss" -> {p with role="Poss"; sel_prefs=[SynsetName "ALL"]}
  | "Coref" -> {p with role="Coref"; sel_prefs=[SynsetName "ALL"]}
  | "Count" -> {p with role="Count"; sel_prefs=[SynsetName "ALL"]}
  | "Measure" -> {p with role="Measure"; sel_prefs=[SynsetName "ALL"]}
  | "Apoz" -> {p with role="Apoz"; sel_prefs=[SynsetName "ALL"]}
  | "Has" -> {p with role="Has"; sel_prefs=[SynsetName "ALL"]}
  | "PHas" -> {p with role="PHas"; sel_prefs=[SynsetName "ALL"]}
  | "PApoz" -> {p with role="PApoz"; sel_prefs=[SynsetName "ALL"]}
  | "Merge" -> {p with role="Merge"; sel_prefs=[SynsetName "ALL"]}
  | s -> failwith ("parse_role: " ^ s)

let parse_entry = function
    [T symbol; T ":"; T "null"] -> symbol,[]
  | T symbol :: T ":" :: tokens ->
      (* Printf.printf "parse_entry: %s\n" (Lexer.string_of_token_list tokens); *)
      let tokens = try Lexer.split_symbol (T ":") [] tokens with _ -> failwith "parse_entry: split_symbol :" in
      let tokens = manage_tokens tokens in
      let positions = Xlist.map tokens (fun (arg,role) ->
        parse_arg arg (parse_role {empty_position with is_necessary=Req} role)) in
      symbol,positions
  | tokens -> failwith ("parse_entry: " ^ Lexer.string_of_token_list tokens)

let load_lexicon filename =
  let lines = File.load_lines filename in
  let lines = List.rev (Xlist.rev_map lines remove_comments) in
  let tokens = List.flatten (List.rev (Xlist.rev_map lines (Lexer.split "\\]\\| \\|\t\\|\r\\|\\?\\|:\\|;\\|&\\|!\\|=\\|}\\|{\\|,\\|\\*\\|/\\|\\+\\|)\\|(\\||\\|\\[\\|\\"))) in
  let tokens = List.rev (Xlist.fold tokens [] (fun tokens -> function
        T " " -> tokens
      | T "\t" -> tokens
      | T "\r" -> tokens
      | t -> t :: tokens)) in
  let entries = try Lexer.split_symbol (T ";") [] tokens with _ -> failwith "load_lexicon: split_symbol ;" in
  Xlist.fold entries StringMap.empty (fun map entry ->
    let symbol,args = parse_entry entry in
    StringMap.add_inc map symbol args (fun _ -> failwith ("load_lexicon: " ^ symbol)))

let sem_lexicon = ref StringMap.empty

let extend_frame symbol frame =
  try
    let positions = StringMap.find !sem_lexicon symbol in
    {frame with positions=positions @ frame.positions}
  with Not_found -> failwith ("extend_frame: " ^ symbol)

let initialize () =
  sem_lexicon := load_lexicon ENIAMsemTypes.sem_lexicon_filename;
  ()
