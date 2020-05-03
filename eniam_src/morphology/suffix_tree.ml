(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

type star = Productive | Star | Ndm

type rule = {star: star; pref: string; find: string; set: string; tags: (string * string) list;
  interp: string; id: string; freq: int}

let parse_star = function
    "" -> Productive
  | "*" -> Star
  | "ndm" -> Ndm
  | s -> failwith "parse_star"

let string_of_star = function
    Productive -> ""
  | Star -> "*"
  | Ndm -> "ndm"

let print_rule file rule =
  Printf.fprintf file "%s\t%d\t%s\t%s\t%s\t%s\t%s\n" rule.id rule.freq (string_of_star rule.star)
    rule.pref rule.find rule.set rule.interp

module CharTrees = struct

  type t = M of t CharMap.t * rule list

  let empty = M(CharMap.empty,[])

  let rec add_path_rules rule orth_suf i (M(map,rules)) =
    if i = -1 then M(map,rule :: rules) else
    let tree = try CharMap.find map (String.get orth_suf i) with Not_found -> empty in
    let tree = add_path_rules rule orth_suf (i-1) tree in
    M(CharMap.add map (String.get orth_suf i) tree,rules)

  let create_char_tree rules =
    let tree = Xlist.fold rules empty (fun tree rule ->
      add_path_rules rule rule.find (String.length rule.find - 1) tree) in
    tree

  let create rules =
    let prefix_map = Xlist.fold rules StringMap.empty (fun prefix_map rule ->
      StringMap.add_inc prefix_map rule.pref [rule] (fun l -> rule :: l)) in
    StringMap.fold prefix_map [] (fun trees prefix rules -> (prefix, create_char_tree rules) :: trees)

  let rec find_rec l i orth (M(map,rules)) =
    if i = 0 then Xlist.fold rules l (fun l rule -> ("", rule) :: l) else
    let l = try find_rec l (i-1) orth (CharMap.find map (String.get orth (i-1))) with Not_found -> l in
    Xlist.fold rules l (fun l rule -> (String.sub orth 0 i, rule) :: l)

  let find trees orth =
    Xlist.fold trees [] (fun found (pref,tree) ->
      (* print_endline pref; *)
      if Xstring.check_prefix pref orth then (
        let orth = Xstring.cut_prefix pref orth in
        (* printf "%s %d " orth (Xlist.size found); *)
        let found = find_rec found (String.length orth) orth tree in
        (* printf "%d\n%!" (Xlist.size found); *)
        (* Xlist.iter found (fun (stem,rule) -> printf "F %s\t%s\n" stem (string_of_rule rule)); *)
        found)
      else found)

  let add_char c rule =
    let s = String.make 1 c in
    {rule with find=s ^ rule.find; set=s ^ rule.set}

  let rec disjoint_rec super (M(map,rules)) =
    let rules = rules @ super in
    if CharMap.is_empty map then M(map,rules) else
    M(CharMap.mapi map (fun c tree ->
      disjoint_rec (Xlist.rev_map rules (add_char c)) tree),[])

  let disjoint trees =
    Xlist.rev_map trees (fun (pref,tree) ->
      pref, disjoint_rec [] tree)

  let rec print_rules_rec file (M(map,rules)) =
    Xlist.iter rules (print_rule file);
    CharMap.iter map (fun _ tree -> print_rules_rec file tree)

  let print_rules filename trees =
    File.file_out filename (fun file ->
      Xlist.iter trees (fun (_,tree) ->
        print_rules_rec file tree))

end

let load_freq_rules filename =
  File.fold_tab filename [] (fun rules -> function
    [id; freq; star; pref; find; set; interp] ->
       {id=id; freq=int_of_string freq; star=parse_star star; pref=pref; find=find; set=set;
        tags=[]; interp=interp} :: rules
  | _ -> failwith "load_freq_rules")

let load_rules resource_path rules_filename =
  let rules = load_freq_rules (resource_path ^ rules_filename) in
  let rules = CharTrees.create rules in
  let rules = CharTrees.disjoint rules in
  rules

let rules_filename = "freq_rules.tab"
let adj_rules_filename = "adj_freq_rules.tab"
let resource_path = "../resources/SGJP/"

let _ =
  let rules = load_rules resource_path adj_rules_filename in
  CharTrees.print_rules "results/disjoint_rules_adj" rules
