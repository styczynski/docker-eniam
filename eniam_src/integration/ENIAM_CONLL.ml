(*
 *  ENIAMintegration, a library that integrates ENIAM with other parsers.
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>, Jan Lupa, Daniel Oklesiński
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

open Xstd
open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes

let string_of_token mode token conll_id super label =
  let decompose_lemma = function
    | Lemma(a,b,c) -> a,b,if c = [[]]
                 then "_"
                 else String.concat "][" @@ Xlist.map c (fun x ->
                        String.concat "|" @@ Xlist.map x ( fun y ->
                          String.concat "." y))
    | t -> failwith ("string_of_token: not Lemma") in
  match mode with
    | Raw -> token.orth
    | Struct -> failwith ("function string_of_token for mode Struct is not defined")
    | CONLL -> let lemma,cat,interp = decompose_lemma token.token in
        String.concat "\t" [string_of_int conll_id;
                 token.orth; lemma; cat; cat; interp; "_"; "_";
                 string_of_int token.beg; string_of_int token.len]
    | Mate -> let lemma,cat,interp = decompose_lemma token.token in
        String.concat "\t" [string_of_int conll_id;
                 token.orth; lemma; lemma; cat; cat; interp; interp; "_"; "_"; "_"; "_"; "_"; "_"]
    | _ -> failwith "string_of_token: ni"

let string_of_paths mode tokens paths =
  let l = Int.fold 1 (Array.length paths - 1) [] (fun l conll_id ->
    let id,super,label = paths.(conll_id) in
    (string_of_token mode (ExtArray.get tokens id) conll_id super label) :: l) in
  String.concat "\n" (List.rev l) ^ "\n\n"

(******************)

let establish_next tokens =
  let n = ExtArray.size tokens in
  Int.iter 1 (n - 2) (fun i ->
    let f = ExtArray.get tokens i in
    let s = ExtArray.get tokens (i+1) in
    ExtArray.set tokens i {f with next = s.beg});
  let last = ExtArray.get tokens (n-1) in
  ExtArray.set tokens (n-1) {last with next = last.beg + last.len}

let rec establish_for_token i text tokens = function
    (id,_,_) :: t as l->
      let h = ExtArray.get tokens id in
      if Xstring.check_prefix " " text
      then establish_for_token (i+100) (Xstring.cut_prefix " " text) tokens l
      else if Xstring.check_prefix h.orth text
        then
          let n = (List.length @@ Xunicode.utf8_chars_of_utf8_string h.orth) * 100 in
          let n_h = {h with beg = i ; len = n} in
          ExtArray.set tokens id n_h;
          establish_for_token (i+n) (Xstring.cut_prefix h.orth text) tokens t
        else failwith ("establish_for_token :" ^ h.orth ^ " " ^ text)
  | [] -> 100, i

let rec establish_lengths pbeg text paths tokens =
  let pbeg, plen = establish_for_token pbeg text tokens (List.tl paths) in
  establish_next tokens;
  pbeg, plen-100

(******************)

exception Empty_line
exception Empty_sentence
exception Id_line of string

let load_token in_channel =
  let fail line =
    (* failwith ("load_token: " ^ line) *)
    () in
  let int_of_super = function
     "_" -> -1
   | s -> int_of_string s in
  let n_token id orth lemma cat interp super label =
    let interp = if interp = "_"
            then [[]]
            else [Xlist.map (Xstring.split_delim "|" interp) (fun tag -> [tag])] in
    {empty_token_env with orth = orth; token = Lemma(lemma,cat,interp);}, int_of_string id, int_of_super super, label in
  let line = input_line in_channel in
  (* print_endline ("load_token: " ^ line); *)
  if line = ""
   then raise Empty_line
   else if line.[0] = '#'
     then
       if Xstring.check_prefix "# trees/" line && Xstring.check_sufix ".xml.trees" line
         then let id = Xstring.cut_prefix "# trees/" @@ Xstring.cut_sufix ".xml.trees" line in
              raise (Id_line id)
         else failwith ("load_token: " ^ line)
     else
       match Xstring.split "\t" line with
         [id; orth; lemma; cat; cat2; interp; super; label; "_"; "_"] ->
          (if cat <> cat2 then fail line;
           n_token id orth lemma cat interp super label)
       | [id; orth; lemma; lemma2; cat; cat2; interp; interp2; "-1"; super; "_"; label; "_"; "_"] ->
          (if (cat, lemma, interp) <> (cat2, lemma2, interp2) then fail line;
           n_token id orth lemma cat interp super label)
       | [id; orth; lemma; cat; cat2; interp; super; label_err; "_"] ->
          (if cat <> cat2 && Xstring.check_sufix "_" label_err then fail line;
           let label = Xstring.cut_sufix "_" label_err in
           n_token id orth lemma cat interp super label)
       | _ -> failwith ("load_token: " ^ line)

let load_sentence in_channel =
  let tokens = ExtArray.make 100 empty_token_env in
  let _ = ExtArray.add tokens {empty_token_env with token = Interp "<conll_root>"} in
  let rec pom rev_paths id =
    (* print_endline "pom 1"; *)
    try
      (* print_endline "pom 2"; *)
      let token, conll_id, super, label = load_token in_channel in
      let id_a = ExtArray.add tokens token in
      if id_a <> conll_id then failwith "load_sentence: different ids" else
      (* print_endline "pom 3"; *)
      pom ((id_a,[super,label],"") :: rev_paths) id
    with Id_line new_id -> (*print_endline "pom 4";*)pom rev_paths new_id
      | Empty_line -> (*print_endline "pom 5";*)rev_paths, id
      | End_of_file -> (*print_endline "pom 6";*)if rev_paths = []
          then raise End_of_file
          else rev_paths, id in
  let rev_paths, id = pom [] "" in
  {id = id; beg = -1; len = -1; next = -1; file_prefix = ""; sentence = DepSentence[Array.of_list ((0,[-1,""],"") :: List.rev rev_paths)]}, tokens
(*  {s_id = id; s_text = ""; s_paths = (List.rev rev_paths)} *)

let load_corpus in_channel =
  let rec pom res =
    try
      let conll_sentence, tokens = load_sentence in_channel in
      pom ((conll_sentence, tokens) :: res)
    with End_of_file -> res in
  List.rev @@ pom []

(******************)

let parse_line line =
  let fail () =
    failwith ("parse_line: " ^ line) in
    (* () in *)
  let int_of_super = function
     "_" -> -1
   | s -> int_of_string s in
  let n_token id orth lemma cat interp super label =
    let interp = if interp = "_"
            then [[]]
            else [Xlist.map (Xstring.split_delim "|" interp) (fun tag -> [tag])] in
    {empty_token_env with orth = orth; token = Lemma(lemma,cat,interp);}, int_of_string id, int_of_super super, label in
  if line = ""
   then failwith "parse_line__empty_line"
   else
     match Xstring.split "\t" line with
       [id; orth; lemma; cat; cat2; interp; super; label; "_"; "_"] ->
        (if cat <> cat2 then fail ();
         n_token id orth lemma cat interp super label)
     | [id; orth; lemma; lemma2; cat; cat2; interp; interp2; "-1"; super; "_"; label; "_"; "_"] ->
        (if (cat, lemma, interp) <> (cat2, lemma2, interp2) then fail ();
         n_token id orth lemma cat interp super label)
     | [id; orth; lemma; cat; cat2; interp; super; label_err; "_"] ->
        (if cat <> cat2 && Xstring.check_sufix "_" label_err then fail ();
         let label = Xstring.cut_sufix "_" label_err in
         n_token id orth lemma cat interp super label)
     | _ -> failwith ("parse_line: " ^ line)

let parse_sentence sentence =
  let tokens = ExtArray.make 100 empty_token_env in
  let _ = ExtArray.add tokens {empty_token_env with token = Interp "<conll_root>"} in
  let rev_paths, id = Xlist.fold (Xstring.split "\n" sentence) ([],"") (fun (paths,id) line ->
    try
      (* print_endline "pom 2"; *)
      let token, conll_id, super, label = parse_line line in
      let id_a = ExtArray.add tokens token in
      if id_a <> conll_id then failwith "load_sentence: different ids" else
      (* print_endline "pom 3"; *)
      ((id_a,[super,label],"") :: paths), id
    with Id_line new_id -> paths, new_id) in
  {id = id; beg = -1; len = -1; next = -1; file_prefix = ""; sentence = DepSentence[Array.of_list ((0,[-1,""],"") :: List.rev rev_paths)]}, tokens
(*  {s_id = id; s_text = ""; s_paths = (List.rev rev_paths)} *)
