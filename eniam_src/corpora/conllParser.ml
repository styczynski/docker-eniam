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

open Xstd
open Types

let skladnica_zaleznosciowa_filename = "../../NLP resources/skladnica_zaleznosciowa.conll"

let oc = open_out @@ resource_path ^ "/info_sentences.txt"

let empty_token = { c_id = 0; c_orth = ""; c_lemma = ""; c_cat = "";
      c_interp = []; c_super = 0; c_label = ""; c_beg = 0; c_len = 0}

let quote_open = ref false

let hyphenated = ref false

let reset () =
  quote_open := false;
  hyphenated := false

let maybe_add_space pre_previous previous token next =
  if previous.c_orth = "" && token.c_orth = "\""
    then quote_open := true;
  if token.c_cat = "aglt" ||
    (token.c_orth = "by" && previous.c_cat = "praet") ||
    (previous.c_orth = "\"" && !quote_open) ||
    previous.c_orth = "(" ||
    previous.c_orth = "„" ||
    previous.c_orth = "" ||
    token.c_orth = "ń" || (* wyrażenie nań *)
    (token.c_orth = "że" && (previous.c_orth  = "czym" || previous.c_orth  = "Czym")) || (*wyrażenie czymże*)
(*    (token.c_orth = "r" && token.c_cat = "brev") || (*skrót r. - np. 1991r. *) *)
    (pre_previous.c_cat = "adj" && previous.c_orth = "." && token.c_cat = "num" && token.c_interp = ["pl";"nom";"f";"rec"]) (* godzina - np 13.15*)
      then token.c_orth
      else if !hyphenated
        then (hyphenated := false; token.c_orth)
        else match token.c_orth with
      "." -> "."
    | "…" -> "…"
    | "?" -> "?"
    | "!" -> "!"
    | "," -> ","
    | ":" -> ":"
    | ";" -> ";"
    | ")" -> ")"
    | "”" -> "”"
    | "-" -> if previous.c_cat = "adja" ||
               (previous.c_cat = "subst" && next.c_cat = "subst" && previous.c_interp = next.c_interp)
               then (hyphenated := true; "-")
               else " -"
    | "\"" -> if !quote_open
                then (quote_open := false; "\"")
                else (quote_open := true; " \"")
    | s -> " "^s

let get_sentence tokens =
  let rec fold4 acc = function
    a::b::c::d::t -> fold4 (acc^maybe_add_space a b c d) (b::c::d::t)
  | a::b::c::[] -> fold4 (acc^maybe_add_space a b c empty_token) (b::c::[])
  | a::b::[] -> acc
  | _ -> failwith ("get_sentence") in
  reset ();
  fold4 "" (empty_token::empty_token::tokens)

let split_word stringname =
  match Str.split (Str.regexp "\t") stringname with
    [id; orth; lemma; cat; cat2; interp; super; label; "_"; "_"] ->
      if cat <> cat2
        then failwith ("split_word :" ^ stringname)
        else let interp = if interp = "_"
          then []
          else Xstring.split_delim "|" interp in
      { c_id = int_of_string id; c_orth = orth; c_lemma = lemma; c_cat = cat;
        c_interp = interp; c_super = int_of_string super; c_label = label; c_beg = -1; c_len = -1}
  | _ -> failwith ("split_word :" ^ stringname)

let any_difference string1 string2 = if string1 = string2
  then false
  else (String.sub string2 0 (String.length string2 -1)) ^ " " ^ (String.sub string2 (String.length string2 -1) 1) <> string1

let find_info tokens =
  let text_generated = get_sentence tokens in
  try
    let sentence = Resources.InfoMap.find (Xlist.map tokens (fun token -> token.c_orth)) (Resources.conll_info ()) in
    let id, text = sentence.s_id, sentence.s_text in
    (*if any_difference text text_generated && text <> "not_found"
      then print_endline (text ^ "\n" ^ text_generated ^ "\n\n");*)
    if text = "not_found"
      then { s_id = id;
             s_text = "Auto-generated text: "^text_generated;
             s_tokens = tokens}
      else { s_id = id;
             s_text = text;
             s_tokens = tokens}
  with _ -> (*prerr_endline ("Id not found\n" ^ text_generated ^ "\n\n");*) { s_id = "Id not found";
    s_text = text_generated;
    s_tokens = tokens}

let process_sentence sentenceString =
  let pom = Xstring.split_delim "\n" sentenceString in
  let tokens = Xlist.map pom (fun word -> split_word word) in
  find_info tokens

let print_info sentence =
  let sentence = process_sentence sentence in
  let form_sequence = String.concat " " @@ Xlist.map sentence.s_tokens (fun token -> token.c_orth) in
  output_string oc (sentence.s_id^"\n"^sentence.s_text^"\n"^form_sequence^"\n\n");
  flush oc

let processSkladnica () =
  List.iter (fun sentence -> print_info sentence) (Str.split (Str.regexp "\n\n") (File.load_file_gen skladnica_zaleznosciowa_filename))
