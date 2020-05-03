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

open Xstd
open ENIAMmorphologyTypes
open Printf

type tags =
    T of string * string
  | A of string

let parse_name s =
  if s = "" then failwith "parse_name: empty name" else
  if String.get s 0 = '@' then String.sub s 1 (String.length s - 1)
  else failwith ("parse_name: invalid name " ^ s)

let parse_tags s =
  Xlist.map (Xstring.split " " s) (fun t ->
    match Xstring.split "=" t with
      [k] -> A k
    | [k;v] -> T(k,v)
    | _ -> failwith "parse_tags")

let parse_star = function
    "" -> Productive
  | "*" -> Star
  | "ndm" -> Ndm
  | s -> failwith "parse_star"

let merge_stars = function
    Star,_ -> Star
  | _,Star -> Star
  | Ndm,_ -> failwith "merge_stars"
  | _,Ndm -> failwith "merge_stars"
  | _ -> Productive

(**********************************************************************************************)

type alternation = {astar: star; aphone: string; afind: string; aset: string}

let load_alternations filename =
  let alternations,name,alts = File.fold_tab filename ([],"",[]) (fun (found,name,alts) -> function
      [alt_name] ->
        let alt_name = parse_name alt_name in
        if name = "" then found,alt_name,[] else (name,List.rev alts) :: found,alt_name,[]
    | [star;a;b;c] -> found,name,{astar=parse_star star; aphone=a; afind=b; aset=c} :: alts
    | _ ->  failwith "load_alternations") in
  (name,List.rev alts) :: alternations

let alternations () = load_alternations "../morphology/data/alternations.dic"

let revert_alternations l =
  Xlist.map l (fun a -> {a with afind=a.aset; aset=a.afind})

let alternation_map alternations = Xlist.fold alternations StringMap.empty (fun map (k,v) ->
  StringMap.add map k v)

let rev_alternation_map alternations = Xlist.fold alternations StringMap.empty (fun map (k,v) ->
  StringMap.add map k (revert_alternations v))

type suf_rule = {sstar: star; salt_name: string; ssufix: string; stags: tags list}
type pref_rule = {pstar: star; pprefix: string; ptags: tags list}

let load_suf_rules filename =
  let suf_rules,name,rules = File.fold_tab filename ([],"",[]) (fun (found,name,rules) -> function
      [rules_name] ->
        let rules_name = parse_name rules_name in
        if name = "" then found,rules_name,[] else (name,List.rev rules) :: found,rules_name,[]
    | [star;alt_name;sufix;tags] -> found,name,{sstar=parse_star star; salt_name=alt_name; ssufix=sufix; stags=parse_tags tags} :: rules
    | _ -> failwith "load_suf_rules") in
  (name,List.rev rules) :: suf_rules

let load_pref_rules filename =
  let pref_rules,name,rules = File.fold_tab filename ([],"",[]) (fun (found,name,rules) -> function
      [rules_name] ->
        let rules_name = parse_name rules_name in
        if name = "" then found,rules_name,[] else (name,List.rev rules) :: found,rules_name,[]
    | [star;prefix;tags] -> found,name,{pstar=parse_star star; pprefix=prefix; ptags=parse_tags tags} :: rules
    | _ -> failwith "load_pref_rules") in
  (name,List.rev rules) :: pref_rules

let rules () = load_suf_rules "../morphology/data/rules.dic"
let rev_rules () = load_suf_rules "../morphology/data/rev_rules.dic"
let pref_rules () = load_pref_rules "../morphology/data/pref_rules.dic"

let load_freq_rules filename =
  File.fold_tab filename [] (fun rules -> function
    [id; freq; star; pref; find; set; interp] ->
       {id=id; freq=int_of_string freq; star=parse_star star; pref=pref; find=find; set=set;
        tags=[]; interp=interp} :: rules
  | _ -> failwith "load_freq_rules")

let expand_tags x l =
  Xlist.map l (function
      T(k,v) -> k,v
    | A k -> k,x)

let expand_tags_simple l =
  Xlist.map l (function
      T(k,v) -> k,v
    | A k -> failwith ("expand_tags_simple: " ^ k))

let prepare_rules alternation_map suf_rules =
  Xlist.fold suf_rules [] (fun rules s ->
    let alternation = try StringMap.find alternation_map s.salt_name with Not_found -> failwith ("prepare_rules: " ^ s.salt_name) in
    Xlist.fold alternation rules (fun rules a ->
        {star=merge_stars (s.sstar,a.astar); pref=""; find=a.afind ^ s.ssufix; set=a.aset;
         tags=expand_tags a.aphone s.stags; interp=""; id=""; freq=0} :: rules))

let prepare_rev_rules rev_alternation_map suf_rules =
  Xlist.fold suf_rules [] (fun rules s ->
    let alternation = try StringMap.find rev_alternation_map s.salt_name with Not_found -> failwith ("prepare_rev_rules: " ^ s.salt_name) in
    Xlist.fold alternation rules (fun rules a ->
        {star=merge_stars (s.sstar,a.astar); pref=""; find=a.afind; set=a.aset ^ s.ssufix;
         tags=expand_tags a.aphone s.stags; interp=""; id=""; freq=0} :: rules))

let prepare_pref_rules pref_rules =
  Xlist.fold pref_rules [] (fun rules p ->
    {star=p.pstar; pref=p.pprefix; find=""; set=""; tags=expand_tags "" p.ptags; interp=""; id=""; freq=0} :: rules)

let rule_map alternation_map rev_alternation_map rules rev_rules pref_rules =
  let map = Xlist.fold rules StringMap.empty (fun map (k,v) -> StringMap.add map k (prepare_rules alternation_map v)) in
  let map = Xlist.fold rev_rules map (fun map (k,v) -> StringMap.add map k (prepare_rev_rules rev_alternation_map v)) in
  Xlist.fold pref_rules map (fun map (k,v) -> StringMap.add map k (prepare_pref_rules v))

let schemata () = File.load_tab "../morphology/data/schemata.dic" (fun l -> l)

(**********************************************************************************************)

let rec extract_tag s rev = function
    [] -> "", List.rev rev
  | (k,v) :: l -> if s = k then v, List.rev rev @ l else extract_tag s ((k,v) :: rev) l

let create_compound_rules schemata rule_map =
  let found = Xlist.fold schemata [] (fun found schema ->
    let compounds = Xlist.fold schema [{star=Productive;pref="";find="";set="";tags=[];interp=""; id=""; freq=0}] (fun compounds rule_set_name ->
      let rules = try StringMap.find rule_map rule_set_name with Not_found -> failwith ("create_compound_rules: " ^ rule_set_name) in
      Xlist.fold compounds [] (fun compounds compound ->
        Xlist.fold rules compounds (fun compounds rule ->
          (* printf "compound.find=%s; compound.set=%s\n" compound.find compound.set;
          printf "rule.find=%s; rule.set=%s\n" rule.find rule.set; *)
          if rule.find = "" && rule.set = "" then
            {compound with star=merge_stars (compound.star, rule.star);
                           pref=compound.pref ^ rule.pref; tags=rule.tags@compound.tags} :: compounds
          else if Xstring.check_sufix compound.set rule.find then
            {compound with star=merge_stars (compound.star, rule.star);
                           find=Xstring.cut_sufix compound.set rule.find ^ compound.find; set=rule.set; tags=rule.tags@compound.tags} :: compounds
          else if Xstring.check_sufix rule.find compound.set then
            {compound with star=merge_stars (compound.star, rule.star);
                           find=compound.find; set=Xstring.cut_sufix rule.find compound.set ^ rule.set; tags=rule.tags@compound.tags} :: compounds
          else compounds))) in
    compounds @ found) in
  let found = Xlist.rev_map found (fun rule ->
    let suf, tags = extract_tag "suf" [] rule.tags in
    {rule with set=rule.set ^ suf; tags=tags}) in
  found

let make_compound_rules () =
  let schemata = schemata () in
  let alternations = alternations () in
  let alternation_map = alternation_map alternations in
  let rev_alternation_map = rev_alternation_map alternations in
  let rule_map = rule_map alternation_map rev_alternation_map (rules ()) (rev_rules ()) (pref_rules ()) in
  create_compound_rules schemata rule_map

(**********************************************************************************************)

let tag_value = function
    "cat" -> 1
  | "pref" -> 2
  | "con" -> 3
  | "grad" -> 4
  | "group" -> 5
  | "flex2" -> 6
  | "flex" -> 7
  | "lemma" -> 8
  | s -> failwith ("tag_value: " ^ s)

let tag_value2 = function
    "cat" -> 1
  | "flex" -> 2
  | "flex2" -> 3
  | "grad" -> 4
  | "pref" -> 5
  | "lemma" -> 6
  | "con" -> 7
  | "group" -> 8
  | s -> failwith ("tag_value2: " ^ s)

let compare_tag (a,_) (b,_) =
  compare (tag_value a) (tag_value b)

let compare_tag2 (a,_) (b,_) =
  compare (tag_value2 a) (tag_value2 b)

let load_interp_rules filename =
  File.load_tab filename (function
      star :: tags :: interp :: comment :: [] ->
        {star=parse_star star;
          pref=""; find=""; set="";
          tags=expand_tags_simple (parse_tags tags); interp=interp; (*comment=comment;*) id=""; freq=0}
    | line -> failwith ("load_tab: " ^ (String.concat "\t" line)))

module InterpTree = struct

  type t =
      N of string * t StringMap.t * rule list
    | L of rule list

  let empty = L []

  let rec create_rec rule = function
      [],N(key,map,rules) -> N(key,map,rule :: rules)
    | [],L rules -> L(rule :: rules)
    | (k,v) :: tags,N(key,map,rules) ->
        if k <> key then failwith ("create_rec: " ^ k ^ " " ^ key) else
        let tree = try StringMap.find map v with Not_found -> empty in
        let tree = create_rec rule (tags,tree) in
        N(key,StringMap.add map v tree,rules)
    | (k,v) :: tags,L rules ->
        let tree = create_rec rule (tags,empty) in
        N(k,StringMap.add StringMap.empty v tree,rules)

  let create interp_rules =
    Xlist.fold interp_rules empty (fun interp_tree rule ->
      let tags = Xlist.sort rule.tags compare_tag2 in
      create_rec rule (tags,interp_tree))

  let rec find_rec = function
      [],N(_,_,rules) -> rules
    | _,L rules -> rules
    | (k,v) :: tags,N(key,map,rules) ->
        if k <> key then find_rec (tags,N(key,map,rules)) else
        try rules @ (find_rec (tags,StringMap.find map v))
        with Not_found -> rules

  let find interp_tree tags =
    find_rec (Xlist.sort tags compare_tag2,interp_tree)

end

let interp_tree () = InterpTree.create (load_interp_rules "../morphology/data/interp_rules.dic")

(**********************************************************************************************)

let create_interp_compound_rules interp_tree compound_rules =
  Xlist.fold compound_rules [] (fun interp_compound_rules rule ->
    let interp_rules = InterpTree.find interp_tree rule.tags in
    Xlist.fold interp_rules interp_compound_rules (fun interp_compound_rules interp_rule ->
      {rule with interp=interp_rule.interp; star=merge_stars (rule.star, interp_rule.star)} :: interp_compound_rules))

let assign_ids rules =
  fst (Xlist.fold rules ([],1) (fun (rules,id) rule ->
    {rule with id=string_of_int id} :: rules, id+1))

let interp_compound_rules compound_rules = assign_ids (create_interp_compound_rules (interp_tree ()) compound_rules)

(**********************************************************************************************)

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

end

let compound_rule_trees compound_rules = CharTrees.create compound_rules
let interp_compound_rule_trees interp_compound_rules = CharTrees.create interp_compound_rules

let make_compound_rule_trees = compound_rule_trees

let make_interp_compound_rule_trees compound_rules =
  interp_compound_rule_trees (interp_compound_rules compound_rules)

(**********************************************************************************************)

module OrderedRule = struct

  type t = rule

  let compare = compare

end

module RuleQMap = Xmap.MakeQ(OrderedRule)

let string_of_star = function
    Productive -> ""
  | Star -> "*"
  | Ndm -> "ndm"

let string_of_freq_rule rule =
  sprintf "%s\t%d\t%s\t%s\t%s\t%s\t%s" rule.id rule.freq (string_of_star rule.star) rule.pref rule.find rule.set rule.interp

(**********************************************************************************************)
