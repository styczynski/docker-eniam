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

let skladnica_frazowa_filename = "../../NLP resources/skladnica_walencyjna"

let get_filenames path =
  Xlist.fold (Array.to_list (Sys.readdir path)) []
    (fun files_list1 folder1 -> if folder1 = ".DS_Store" then files_list1 else
        Xlist.fold (Array.to_list (Sys.readdir (path ^ "/" ^ folder1))) files_list1
          (fun files_list2 folder2 -> if folder2 = ".DS_Store" then files_list2 else
            Xlist.fold (Array.to_list (Sys.readdir (path ^ "/" ^ folder1 ^ "/" ^ folder2))) files_list2
              (fun files_list3 file -> if file = ".DS_Store" then files_list3 else
                (path ^ "/" ^ folder1 ^ "/" ^ folder2 ^ "/" ^file) :: files_list3)))

let add_to_map map = function
    Xml.Element("forest",("sent_id",sent_id) :: _,
      Xml.Element("text",[],[Xml.PCData text]) :: _) -> StringMap.add map sent_id text
  | _ -> failwith "add_to_map"

let i = ref 1

(* map(id,text) *)
let sentencesIdText () = Xlist.fold (get_filenames skladnica_frazowa_filename) StringMap.empty
  (fun acc filename -> print_endline (string_of_int !i); i := !i + 1; add_to_map acc (Xml.parse_file filename))

let number_of_sentences_skladnica_frazowa = List.length (get_filenames skladnica_frazowa_filename)

let _ = print_endline (string_of_int number_of_sentences_skladnica_frazowa)

(*************************************************************************************************************)

module Info =
  struct
    type t = string list
    let compare a b = Pervasives.compare a b
  end

module InfoMap = Map.Make(Info)

let krzaki_filename = "../../NLP resources/krzaki.conll"

let load_krzaki filename = Xstring.split "\n\n" (File.load_file_gen filename)

let split_word stringname =
  let pom = Xstring.split_delim "\t" stringname in
    { c_id = int_of_string (List.nth pom 0);
      c_orth = List.nth pom 1;
      c_lemma = List.nth pom 2;
      c_cat = List.nth pom 3;
      c_interp = (Xstring.split_delim "|" (List.nth pom 5));
      c_super = int_of_string (List.nth pom 6);
      c_label = List.nth pom 7;
      c_beg = -1;
      c_len = -1}

let split_krzak sentencesIdText stringname =
  let pom = Xstring.split_delim "\n" stringname in
  let s_id = String.sub stringname 8 ((String.length @@ List.hd pom)-17) in
    { s_id = s_id;
      s_text =
        (try
          StringMap.find sentencesIdText s_id
        with _ -> prerr_endline s_id; "not_found");
      s_tokens = Xlist.map (List.tl pom) (fun word -> split_word word)}

let parse_krzaki sentencesIdText list_of_string =
  Xlist.map list_of_string (fun krzak -> split_krzak sentencesIdText krzak)

let number_of_sentences_krzaki = List.length (load_krzaki krzaki_filename)

(* conll_sequence list *)
let data_conll () =
let sentencesIdText = sentencesIdText () in
parse_krzaki sentencesIdText (load_krzaki krzaki_filename)

(* map(form_sequence,conll_sentence) *)
let conll_info () = Xlist.fold (data_conll ()) InfoMap.empty
  (fun map sentence -> InfoMap.add (List.map (fun token -> token.c_orth) sentence.s_tokens) sentence map)

let info_file () =
  let oc = open_out @@ resource_path ^ "/info_sentences2.txt" in
  List.iter (fun (key, sentence) ->
    output_string oc (sentence.s_id^"\n"^sentence.s_text^"\n"^(String.concat " " key)^"\n\n");
    flush oc) (InfoMap.bindings (conll_info()))

(*let frazowa_info =
  let got_info = List.map (fun (_, sentence) -> sentence.s_id, sentence.s_text) (InfoMap.bindings (conll_info())) in
  List.fold_left (fun map (id, text) -> if List.mem (id, text) got_info
    then map
    else StringMap.add map text id) StringMap.empty (StringMap.bindings sentecesIdText) *)
