(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
 *  Copyright (C) 2016 Daniel Oklesinski <oklesinski dot daniel atSPAMfree gmail dot com>
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

open Xstd
open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes

let alternative_string f mode alts = if List.exists (fun (m,_) -> mode = m) alts
      then f (snd @@ List.find (fun (m,_) -> m = mode) alts)
      else f (snd @@ List.find (fun (m,_) -> m = Struct) alts)

let string_of_token mode token conll_id super label =
  let decompose_lemma = function
    | Lemma(a,b,c) -> a,b,if c = [[]]
                 then ""
                 else String.concat "][" @@ Xlist.map c (fun x ->
                        String.concat "|" @@ Xlist.map x ( fun y ->
                          String.concat "." y))
    | t -> failwith ("string_of_token: not Lemma") in
  match mode with
    | Raw -> token.orth
    | Struct -> failwith ("function string_of_token for mode Struct is not defined")
    | CONLL -> let lemma,cat,interp = decompose_lemma token.token in
        String.concat "\t" [string_of_int conll_id;
                 token.orth; lemma; cat; cat; interp;
                 string_of_int super; "_"; "_"; "_"]
        (* String.concat "\t" [string_of_int conll_id;
                 token.orth; lemma; cat; cat; interp; "_"; "_";
                 string_of_int token.beg; string_of_int token.len] *)
    | Mate -> let lemma,cat,interp = decompose_lemma token.token in
        String.concat "\t" [string_of_int conll_id;
                 token.orth; lemma; lemma; cat; cat; interp; interp; "_"; "_"; "_"; "_"; "_"; "_"]
    | _ -> failwith "string_of_token: ni"

let string_of_paths mode tokens paths =
  let l = Int.fold 1 (Array.length paths - 1) [] (fun l conll_id ->
    let id,super,label = paths.(conll_id) in
    (string_of_token mode (ExtArray.get tokens id) conll_id super label) :: l) in
  String.concat "\n" (List.rev l) ^ "\n\n"

let rec string_of_sentence mode tokens = function
      RawSentence s -> if mode = Raw then s else ""
    | StructSentence (tree_struct, _) -> failwith ("string_of_sentence: StructSentence") (*String.concat "\n" @@ Xlist.map tokens (fun x -> string_of_token mode x)*)
    | DepSentence (paths) -> string_of_paths mode tokens (List.hd paths)
    | QuotedSentences _ -> failwith ("string_of_sentence: QuotedSentences")
    | AltSentence alts -> alternative_string (string_of_sentence mode tokens) mode alts

let string_of_sentence_env mode tokens sentence_env =
  (if sentence_env.id = "" then "" else "# trees/" ^ sentence_env.id ^ ".xml.tree\n") ^
  string_of_sentence mode tokens sentence_env.sentence

(* let rec string_of_paragraph mode tokens = function
    RawParagraph s -> if mode = Raw then s else ""
  | StructParagraph sentence_envs -> String.concat "\n\n" @@ Xlist.map sentence_envs (string_of_sentence_env mode tokens)
  | AltParagraph alts -> alternative_string (string_of_paragraph mode tokens) mode alts

let rec string_of_text mode tokens = function
    RawText s -> if mode = Raw then s else ""
  | StructText paragraphs -> String.concat "\n\n" @@ Xlist.map paragraphs (string_of_paragraph mode tokens)
  | AltText alts -> alternative_string (string_of_text mode tokens) mode alts *)

(******************)

type to_text = { t_orth: string; t_cat: string; t_interp: string list list list }

let empty_to_text = { t_orth = ""; t_cat = ""; t_interp = [[[]]] }

let get_text tokens =
  let get i =
    let cat = match (ExtArray.get tokens i).token with
        Lemma(_,cat,_) -> cat
      | _ -> "" in
    let interp = match (ExtArray.get tokens i).token with
        Lemma(_,_,i) -> i
      | _ -> [[[]]] in
    { t_orth = (ExtArray.get tokens i).orth; t_cat = cat; t_interp = interp } in

  let n_tokens = Int.fold_down (ExtArray.size tokens - 1) 0 []
    (fun acc i -> (get i)::acc)in

  let quote_open = ref false in
  let hyphenated = ref false in

  let maybe_add_space pre_previous previous token next =
    if previous.t_orth = "" && token.t_orth = "\""
    then quote_open := true;
    if token.t_cat = "aglt" ||
      (token.t_orth = "by" && previous.t_cat = "praet") ||
      (previous.t_orth = "\"" && !quote_open) ||
      previous.t_orth = "(" ||
      previous.t_orth = "„" ||
      previous.t_orth = "" ||
      token.t_orth = "ń" || (* wyrażenie nań *)
      (token.t_orth = "że" && (previous.t_orth  = "czym" || previous.t_orth  = "Czym")) || (*wyrażenie czymże*)
(*    (token.orth = "r" && token.cat = "brev") || (*skrót r. - np. 1991r. *) *)
      (pre_previous.t_cat = "adj" && previous.t_orth = "." &&
       token.t_cat = "num" && token.t_interp = [[["pl"];["nom"];["f"];["rec"]]]) (* godzina - np 13.15*)
    then token.t_orth
    else if !hyphenated
      then (hyphenated := false; token.t_orth)
      else match token.t_orth with
      "." -> "."
    | "…" -> "…"
    | "?" -> "?"
    | "!" -> "!"
    | "," -> ","
    | ":" -> ":"
    | ";" -> ";"
    | ")" -> ")"
    | "”" -> "”"
    | "-" -> if previous.t_cat = "adja" ||
               (previous.t_cat = "subst" && next.t_cat = "subst" && previous.t_interp = next.t_interp)
               then (hyphenated := true; "-")
               else " -"
    | "\"" -> if !quote_open
                then (quote_open := false; "\"")
                else (quote_open := true; " \"")
    | s -> " "^s in

  let rec fold4 acc = function
    a::b::c::d::t -> fold4 (acc^maybe_add_space a b c d) (b::c::d::t)
  | a::b::c::[] -> fold4 (acc^maybe_add_space a b c empty_to_text) (b::c::[])
  | a::b::[] -> acc
  | _ -> failwith ("get_sentence") in
  fold4 "" (empty_to_text::empty_to_text::n_tokens)

(******************)

let establish_next tokens paths =
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
            (* || (h.orth = "m.in." && Xstring.check_prefix "m.in" text) *)
        then
          let n = (List.length @@ Xunicode.utf8_chars_of_utf8_string h.orth) * 100 in
          let n_h = {h with beg = i ; len = n} in
          ExtArray.set tokens id n_h;
          (* if Xstring.check_prefix h.orth text then *)
          establish_for_token (i+n) (Xstring.cut_prefix h.orth text) tokens t
          (* else establish_for_token (i+n) (Xstring.cut_prefix "m.in" text) tokens t *)
        else (prerr_endline ("establish_for_token :" ^ h.orth ^ " " ^ text);
            failwith ("establish_for_token :" ^ h.orth ^ " " ^ text))
  | [] -> 100, i

let rec establish_lengths text paths tokens =
  let pbeg, plen = establish_for_token 100 text tokens (List.tl (Array.to_list paths)) in
  establish_next tokens paths;
  pbeg, plen-100

(******************)

exception ErrorInfoFile of string

let info_file = "../corpora/info_sentences2.txt"

let info () = Xstring.split "\n\n" @@ File.load_file_gen info_file

let add_to_map map info_str =
  match Xstring.split "\n" info_str with
    [id; text; info_token] ->  StringMap.add map info_token (id, text)
  | _ -> raise (ErrorInfoFile info_str)

let info_map () =
  Xlist.fold (List.tl (info ())) StringMap.empty add_to_map

let match_sentence (p_record,tokens) =
  let rec info_token s = match s with
      RawSentence text -> failwith ("match_sentence: " ^ text)
    | StructSentence (tokens, n) -> failwith ("match_sentence: StructSentence") (*String.concat " " @@ List.map (fun x -> x.orth) tokens*)
    | DepSentence (paths) -> String.concat " " @@ List.map (fun (id,_,_) -> (ExtArray.get tokens id).orth) (List.tl (Array.to_list (List.hd paths))), paths
    | QuotedSentences _ -> failwith ("match_sentence: QuotedSentences")
    | AltSentence alts -> failwith ("match_sentence: AltSentence")
        (*if List.exists (fun (mode, s) -> mode = CONLL) alts
        then info_token (snd (List.find (fun (mode, s) -> mode = CONLL) alts))
        else failwith ("match_sentence: no CONLL mode in AltSentence")*) in
  let info_token, paths = info_token p_record.sentence in
  (* try *)
    let id, text = try
      StringMap.find (info_map ()) info_token
    with
    | _ -> p_record.id, get_text tokens in
    let beg, len = establish_lengths text (List.hd paths) tokens (* -1, -1, p_record.psentence *) in
    AltText[Raw,RawText text;CONLL,StructText[StructParagraph[{id = id; beg = beg; len = len; next = beg+len; file_prefix="";
     sentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence paths]}]]],tokens
(*  {s_id = id; s_text = text; s_tokens = sentence.s_tokens} *)
  (* with _ -> AltText[CONLL,StructText([StructParagraph[p_record]],tokens)] *)

let match_corpus corpus =
  let rec pom f = function
      [] -> []
    | a::l -> try
          let r = f a in r :: pom f l
        with e ->  pom f l in
  pom match_sentence corpus

(******************)

exception Comment_line
exception Empty_line
exception Empty_sentence
exception Id_line of string

let load_token in_channel =
  let fail line =
    print_endline ("load_token: " ^ line);
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
  if line = ""
   then raise Empty_line
   else if line.[0] = '#'
     then
       if Xstring.check_prefix "# trees/" line && Xstring.check_sufix ".xml.trees" line
       then let id = Xstring.cut_prefix "# trees/" @@ Xstring.cut_sufix ".xml.trees" line in
         raise (Id_line id)
       else if Xstring.check_prefix "# trees/" line && Xstring.check_sufix ".xml.tree" line
         then let id = Xstring.cut_prefix "# trees/" @@ Xstring.cut_sufix ".xml.tree" line in
                raise (Id_line id)
         else raise Comment_line
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
    try
      let token, conll_id, super, label = load_token in_channel in
      let id_a = ExtArray.add tokens token in
      if id_a <> conll_id then failwith "load_sentence: different ids" else
      pom ((id_a,super,label) :: rev_paths) id
    with Id_line new_id -> pom rev_paths new_id
      | Comment_line -> pom rev_paths id
      | Empty_line -> rev_paths, id
      | End_of_file -> if rev_paths = []
          then raise End_of_file
          else rev_paths, id in
  let rev_paths, id = pom [] "" in
  {id = id; beg = -1; len = -1; next = -1; file_prefix = ""; sentence = DepSentence[Array.of_list ((0,-1,"") :: List.rev rev_paths)]}, tokens
(*  {s_id = id; s_text = ""; s_paths = (List.rev rev_paths)} *)

let load_corpus in_channel =
  let rec pom res =
    try
      let conll_sentence, tokens = load_sentence in_channel in
      pom ((conll_sentence, tokens) :: res)
    with End_of_file -> res
    | e -> prerr_endline (Printexc.to_string e); res in
  List.rev @@ pom []
