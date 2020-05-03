(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

open ENIAMmorphologyTypes
open Xstd

let load_stems filename =
  File.fold_tab filename StringMap.empty (fun stems -> function
      [stem; lemma_suf; ids] ->
        let ids = StringSet.of_list (Xstring.split " " ids) in
        StringMap.add_inc stems stem ids (fun set -> StringSet.union set ids)
    | l -> failwith ("load_stems: " ^ String.concat " " l))

let load_tab filename =
  File.load_tab filename (function
      orth :: lemma :: interp :: _ ->
        {lemma=lemma; cat=""; forms=[{orth=orth; interp=interp; freq=1; genre=""; validated=false}]; proper_type="";
         ndm=false; stem=""}
    | line -> failwith ("load_tab: " ^ (String.concat "\t" line)))

let simplify_lemma s =
  match Xstring.split ":" s with
    [s] -> s
  | [s;_] -> s
  | _ -> failwith "simplify_lemma"

type status = LemmaVal | LemmNotVal | TokNotFound

let string_of_status = function
    LemmaVal -> "LemmaVal"
  | LemmNotVal -> "LemmNotVal"
  | TokNotFound -> "TokNotFound"

let prepare_alt alt alt_filename =
  let alt2 = load_tab alt_filename in
  let alt = Xlist.fold alt2 alt (fun alt entry ->
    Xlist.fold entry.forms alt (fun alt form ->
      let simple_lemma = simplify_lemma entry.lemma in
      let v = simple_lemma, form.interp, 1, LemmaVal in
      StringMap.add_inc alt form.orth [v] (fun l -> v :: l))) in
  alt

let prepare_rules rules_filename =
  let rules = ENIAMmorphologyRules.load_freq_rules rules_filename in
  let rules = ENIAMmorphologyRules.CharTrees.create rules in
  rules

let alt = ref (StringMap.empty : (string * string * int * status) list StringMap.t)
let stems = ref (StringMap.empty : StringSet.t StringMap.t)
let rules = ref ([] : (StringMap.key * ENIAMmorphologyRules.CharTrees.t) list)

let initialize () =
  alt := prepare_alt StringMap.empty alt_filename;
  alt := prepare_alt !alt alt_supplement_filename;
  stems := load_stems stem_filename;
  rules := prepare_rules rules_filename


let get_interpretations orth =
  let candidates = ENIAMmorphologyRules.CharTrees.find !rules orth in
  let found = try StringMap.find !alt orth with Not_found -> [] in
  let found = Xlist.fold candidates found (fun found (stem,rule) ->
    (* Printf.printf "%s\t%s\n%!" stem (ENIAMmorphologyRules.string_of_freq_rule rule); *)
    let ids = try StringMap.find !stems stem with Not_found -> StringSet.empty in
    if not (StringSet.mem ids rule.id) && rule.star <> Productive then found else
    let tags = if StringSet.mem ids rule.id then LemmaVal else LemmNotVal in
    (stem ^ rule.set, rule.interp, rule.freq, tags) :: found) in
  let found = (orth,"unk",1,TokNotFound) :: found in
  let valid = Xlist.fold found [] (fun valid -> function
      lemma,interp,quantity,LemmaVal -> (lemma,interp,quantity,LemmaVal) :: valid
    | _ -> valid) in
  if valid = [] then found else valid
