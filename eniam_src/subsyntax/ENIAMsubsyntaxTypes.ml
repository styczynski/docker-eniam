(*
 *  ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish
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

open ENIAMtokenizerTypes

type mode =
    Raw | Struct | CONLL | ENIAM | Mate | Swigra | POLFIE | Error | Name | Identifier

type sentence =
    RawSentence of string
  (* | CONLL of conll list *)
  | StructSentence of (int * int * int) list * int (* (id * lnode * rnode) list * last *)
  | DepSentence of (int * (int * string) list * string) array list (* (id * (super * label) list * sem) conll_id *)
  | QuotedSentences of sentence_env list
  (* | NKJP1M of nkjp1m list *)
  (* | Skladnica of skladnica_tree *)
  | AltSentence of (mode * sentence) list  (* string = etykieta np raw, nkjp, krzaki *)
  | ErrorSentence of string

and sentence_env = {id: string; beg: int; len: int; next: int; sentence: sentence; file_prefix: string} (* beg i len liczone po znakach unicode ( * 100 ???) *)

and paragraph =
    RawParagraph of string
  | StructParagraph of sentence_env list (* zdania *)
  | AltParagraph of (mode * paragraph) list
  | ErrorParagraph of string

type text =
    RawText of string
  | StructText of paragraph list (* * token_record ExtArray.t*) (* akapity * tokeny *)
  | AltText of (mode * text) list
  | ErrorText of string

exception BrokenPaths
  
(* let strong_disambiguate_flag = ref false *)
(* let recognize_proper_names = ref true *)
let merge_lemmata = ref true
let default_category_flag = ref false

let data_path =
  try Sys.getenv "ENIAM_USER_DATA_PATH"
  with Not_found -> "data"

let brev_filename = resource_path ^ "/subsyntax/brev.tab"
let fixed_filename = resource_path ^ "/Walenty/fixed.tab"
(* let complete_entries_filename = resource_path ^ "/subsyntax/complete_entries.tab" *)
let mwe_filename = data_path ^ "/mwe.tab"
let mwe2_filename = data_path ^ "/mwe2.tab"
(* let sawa_filename = resource_path ^ "/subsyntax/SAWA.dic" *)
let sawa_ulice_filename = resource_path ^ "/subsyntax/SAWA_ulice.dic"
let sawa_dzielnice_filename = resource_path ^ "/subsyntax/SAWA_dzielnice.dic"
let sawa_sort_filename = resource_path ^ "/subsyntax/SAWA_sort.dic"
let sejf_filename = resource_path ^ "/subsyntax/SEJF.dic"
let sejfek_filename = resource_path ^ "/subsyntax/SEJFEK.dic"

let lemma_frequencies_filename = resource_path ^ "/subsyntax/NKJP1M-lemma-freq.tab"

(* let proper_names_filename = resource_path ^ "/subsyntax/proper_names_sgjp_polimorf.tab"
let proper_names_filename2 = resource_path ^ "/subsyntax/proper_names.tab" *)
let proper_names_filename = resource_path ^ "/subsyntax/proper_names_sgjp_polimorf_20151020.tab"
let proper_names_filename2 = resource_path ^ "/subsyntax/proper_names_20160104.tab"
let proper_names_filename3 = data_path ^ "/ne.tab"
let proper_names_filename4 = resource_path ^ "/subsyntax/proper_names_SAWA.tab"
let simc_filename = resource_path ^ "/subsyntax/SIMC.tab"
let terc_filename = resource_path ^ "/subsyntax/TERC.tab"

let concraft_host_name = ref "localhost"
let concraft_port = ref 4322
let concraft_enabled = ref false
let concraft_disambiguate = ref false

let coord_host_name = ref "localhost"
let coord_port = ref 4323
let coord_enabled = ref false
let coord_in = ref stdin
let coord_out = ref stdout


let int_of_mode = function
    Raw -> 0
  | Struct -> 1
  | CONLL -> 2
  | ENIAM -> 3
  | Mate -> 4
  | Swigra -> 5
  | POLFIE -> 6
  | Error -> 7
  | Name -> 8
  | Identifier -> 9

let compare_mode x y =
  compare (int_of_mode x) (int_of_mode y)


let rec map_sentence mode f = function
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = map_sentence mode f p.sentence in
        {p with sentence=sentence}) in
      QuotedSentences(List.rev sentences)
  | AltSentence l ->
      let l = Xlist.rev_map l (fun (mode,sentence) ->
        mode, map_sentence mode f sentence) in
      AltSentence(List.rev l)
  | s -> f mode s

let rec map_paragraph mode f = function
    RawParagraph s -> RawParagraph s
  | StructParagraph sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = map_sentence mode f p.sentence in
        {p with sentence=sentence}) in
      StructParagraph(List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, map_paragraph mode f paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec map_text mode f = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        map_paragraph mode f paragraph) in
      StructText(List.rev paragraphs)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, map_text mode f text))
  | ErrorText s -> ErrorText s


let rec fold_sentence mode s f = function
    QuotedSentences sentences ->
    Xlist.fold sentences s (fun s p ->
        fold_sentence mode s f p.sentence)
  | AltSentence l ->
    Xlist.fold l s (fun s (mode,sentence) ->
        fold_sentence mode s f sentence)
  | t -> f mode s t

let rec fold_paragraph mode s f = function
    RawParagraph _ -> s
  | StructParagraph sentences ->
    Xlist.fold sentences s (fun s p ->
        fold_sentence mode s f p.sentence)
  | AltParagraph l ->
    Xlist.fold l s (fun s (mode,paragraph) ->
        fold_paragraph mode s f paragraph)
  | ErrorParagraph _ -> s

let rec fold_text mode s f = function
    RawText _ -> s
  | StructText paragraphs ->
    Xlist.fold paragraphs s (fun s paragraph ->
        fold_paragraph mode s f paragraph)
  | AltText l ->
    Xlist.fold l s (fun s (mode,text) ->
        fold_text mode s f text)
  | ErrorText _ -> s
