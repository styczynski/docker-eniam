(*
 *  ENIAMplWordnet, a converter for Polish Wordnet "Słowosieć".
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
open ENIAMplWordnetTypes

let process_unit = function
    Xml.Element("unit-id",[],[Xml.PCData s]) -> int_of_string s, empty_lu
  | node -> failwith ("process_unit " ^ (Xml.to_string node))

let process_tests = function
    Xml.Element("test",["text",text;"pos",pos],[]) -> text,pos
  | node -> failwith ("process_tests " ^ (Xml.to_string node))

let process_abstract = function
    "true" -> true
  | "false" -> false
  | _ -> failwith "process_abstract"

(* funkcja zwraca:
lexical-unit map - wiąże leksemy z identyfikatorami
synset map
lexicalrelations
synsetrelations
relationtypes map
*)
let process_entry (lumap,synmap,lr,sr,rtmap) = function
    Xml.Element("lexical-unit",["id",id;"name",name;"pos",pos;"tagcount",tagcount;"domain",domain;"workstate",workstate;
                                "source",source;"variant",variant],[]) ->
        let lumap = IntMap.add_inc lumap (int_of_string id) {lu_id=int_of_string id; lu_name=name; lu_pos=pos; lu_tagcount=tagcount; lu_domain=domain; lu_desc="";
          lu_workstate=workstate; lu_source=source; lu_variant=variant; lu_syn=(-1)} (fun _ -> failwith "process_entry 2") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("lexical-unit",["id",id;"name",name;"pos",pos;"tagcount",tagcount;"domain",domain;"desc",desc;"workstate",workstate;
                                "source",source;"variant",variant],[]) ->
        let lumap = IntMap.add_inc lumap (int_of_string id) {lu_id=int_of_string id; lu_name=name; lu_pos=pos; lu_tagcount=tagcount; lu_domain=domain; lu_desc=desc;
          lu_workstate=workstate; lu_source=source; lu_variant=variant; lu_syn=(-1)} (fun _ -> failwith "process_entry 3") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("synset",["id",id;"workstate",workstate;"split",split;"owner",owner;"definition",definition;"desc",desc;
                                "abstract",abstract],units) ->
        let units = Xlist.map units process_unit in
        let synmap = IntMap.add_inc synmap (int_of_string id) {syn_workstate=workstate; syn_split=split; syn_owner=owner; syn_definition=definition;
          syn_desc=desc; syn_abstract=process_abstract abstract; syn_units=units; syn_pos=""; syn_no_hipo=0; syn_domain=""} (fun _ -> failwith "process_entry 4") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("synset",["id",id;"workstate",workstate;"split",split;"owner",owner;"desc",desc;
                                "abstract",abstract],units) ->
        let units = Xlist.map units process_unit in
        let synmap = IntMap.add_inc synmap (int_of_string id) {syn_workstate=workstate; syn_split=split; syn_owner=owner; syn_definition="";
          syn_desc=desc; syn_abstract=process_abstract abstract; syn_units=units; syn_pos=""; syn_no_hipo=0; syn_domain=""} (fun _ -> failwith "process_entry 4") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("lexicalrelations",["parent",parent;"child",child;"relation",relation;"valid",valid;"owner",owner],[]) ->
        let lr = {r_parent=int_of_string parent; r_child=int_of_string child; r_relation=int_of_string relation; r_valid=valid; r_owner=owner} :: lr in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("synsetrelations",["parent",parent;"child",child;"relation",relation;"valid",valid;"owner",owner],[]) ->
        let sr = {r_parent=int_of_string parent; r_child=int_of_string child; r_relation=int_of_string relation; r_valid=valid; r_owner=owner} :: sr in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("relationtypes",["id",id;"type",typ;"reverse",reverse;"name",name;"description",description;
                                 "posstr",posstr;"display",display;"shortcut",shortcut;"autoreverse",autoreverse;
                                 "pwn",pwn],tests) ->
        let tests = Xlist.map tests process_tests in
        let rtmap = IntMap.add_inc rtmap (int_of_string id) {rt_type=typ; rt_reverse=int_of_string reverse; rt_name=name; rt_description=description;
          rt_posstr=posstr; rt_display=display; rt_shortcut=shortcut; rt_autoreverse=autoreverse; rt_pwn=pwn; rt_tests=tests}
          (fun _ -> failwith "process_entry 5") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("relationtypes",["id",id;"type",typ;"name",name;"description",description;
                                 "posstr",posstr;"display",display;"shortcut",shortcut;"autoreverse",autoreverse;
                                 "pwn",pwn],tests) ->
        let tests = Xlist.map tests process_tests in
        let rtmap = IntMap.add_inc rtmap (int_of_string id) {rt_type=typ; rt_reverse=(-1); rt_name=name; rt_description=description;
          rt_posstr=posstr; rt_display=display; rt_shortcut=shortcut; rt_autoreverse=autoreverse; rt_pwn=pwn; rt_tests=tests}
          (fun _ -> failwith "process_entry 5") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("relationtypes",["id",id;"type",typ;"parent",parent;"reverse",reverse;"name",name;"description",description;
                                 "posstr",posstr;"display",display;"shortcut",shortcut;"autoreverse",autoreverse;
                                 "pwn",pwn],tests) ->
        let tests = Xlist.map tests process_tests in
        let rtmap = IntMap.add_inc rtmap (int_of_string id) {rt_type=typ; rt_reverse=int_of_string reverse; rt_name=name; rt_description=description;
          rt_posstr=posstr; rt_display=display; rt_shortcut=shortcut; rt_autoreverse=autoreverse; rt_pwn=pwn; rt_tests=tests}
          (fun _ -> failwith "process_entry 5") in
        lumap,synmap,lr,sr,rtmap
  | Xml.Element("relationtypes",["id",id;"type",typ;"parent",parent;"name",name;"description",description;
                                 "posstr",posstr;"display",display;"shortcut",shortcut;"autoreverse",autoreverse;
                                 "pwn",pwn],tests) ->
        let tests = Xlist.map tests process_tests in
        let rtmap = IntMap.add_inc rtmap (int_of_string id) {rt_type=typ; rt_reverse=(-1); rt_name=name; rt_description=description;
          rt_posstr=posstr; rt_display=display; rt_shortcut=shortcut; rt_autoreverse=autoreverse; rt_pwn=pwn; rt_tests=tests}
          (fun _ -> failwith "process_entry 5") in
        lumap,synmap,lr,sr,rtmap
  | node -> print_endline (Xml.to_string node); failwith "process_entry 1"

let load_data filename =
  match try Xml.parse_file filename with Xml.Error e -> failwith ("load_data Xml.Error " ^ Xml.error e) with
    Xml.Element("array-list",_,entries) ->
      Xlist.fold entries (IntMap.empty,IntMap.empty,[],[],IntMap.empty) process_entry
  | node -> failwith ("load_data " ^ (Xml.to_string node))

let check_lu_syn_consistency lumap synmap =
  let set = IntMap.fold lumap IntSet.empty (fun set id _ ->
    if IntSet.mem set id then failwith "check_lu_syn_consistency 1" else
    IntSet.add set id) in
  let set = IntMap.fold synmap set (fun set _ syn ->
    Xlist.fold syn.syn_units set (fun set (id,_) ->
      if not (IntSet.mem set id) then failwith "check_lu_syn_consistency 2" else
      IntSet.remove set id)) in
  if not (IntSet.is_empty set) then failwith "check_lu_syn_consistency 3" else
  ()

let merge_lu_syn lumap synmap =
  IntMap.map synmap (fun syn ->
    let units = Xlist.map syn.syn_units (fun (id,_) -> id, IntMap.find lumap id) in
    let pos = match StringSet.to_list (Xlist.fold units StringSet.empty (fun set (_,lu) ->
                 StringSet.add set lu.lu_pos)) with
        [] -> failwith "merge_lu_syn: empty synset"
      | [pos] -> pos
      | _ -> failwith "merge_lu_syn: inconsistent pos" in
    {syn with syn_units=units; syn_pos=pos})

let set_lu_syn lumap synmap =
  IntMap.fold synmap lumap (fun lumap syn_id syn ->
    Xlist.fold syn.syn_units lumap (fun lumap (id,_) ->
      let lu = try IntMap.find lumap id with Not_found -> failwith "set_lu_syn" in
      if lu.lu_syn <> -1 then failwith "set_lu_syn" else
      IntMap.add lumap id {lu with lu_syn=syn_id}))

let count_relations qmap rtmap rels =
  Xlist.fold rels qmap (fun qmap rel ->
    if not (IntMap.mem rtmap rel.r_relation) then print_endline ("unknown relation: " ^ string_of_int rel.r_relation);
    IntQMap.add qmap rel.r_relation)

let lu_name lu =
  lu.lu_name ^ "-" ^ lu.lu_variant

let syn_name syn =
  String.concat ", " (Xlist.map syn.syn_units (fun (_,lu) -> lu_name lu))

let syn_name_single syn =
  if syn.syn_units = [] then "empty" else
  lu_name (snd (List.hd syn.syn_units))

let pwn_pos = ["czasownik pwn"; "przymiotnik pwn"; "przysłówek pwn"; "rzeczownik pwn"]

let is_pwn_lu lu =
  Xlist.mem pwn_pos lu.lu_pos

let is_pwn_syn syn =
  Xlist.mem pwn_pos syn.syn_pos

let get_pos_lu lu = lu.lu_pos
let get_pos_syn syn = syn.syn_pos

let add_pwn_qmap map rel parent child =
  let s = Printf.sprintf "%s-%s" parent child in
  IntMap.add_inc map rel.r_relation (StringQMap.add StringQMap.empty s) (fun qmap -> StringQMap.add qmap s)

let test_pwn_elem is_pwn_fun map elem =
  try
    if is_pwn_fun (IntMap.find map elem) then "en" else "pl"
  with Not_found -> "NF"

let test_pos_elem get_pos_fun map elem =
  try
    get_pos_fun (IntMap.find map elem)
  with Not_found -> "NF"

let count_pwn_relation qmap lumap synmap rtmap rels t =
  Xlist.fold rels qmap (fun qmap rel ->
    match (*(IntMap.find rtmap rel.r_relation).rt_type,*)t with
      (*"relacja pomiędzy synsetami",*)"sr" -> add_pwn_qmap qmap rel (test_pwn_elem is_pwn_syn synmap rel.r_parent) (test_pwn_elem is_pwn_syn synmap rel.r_child)
    | (*"relacja leksykalna",*)"lr" -> add_pwn_qmap qmap rel (test_pwn_elem is_pwn_lu lumap rel.r_parent) (test_pwn_elem is_pwn_lu lumap rel.r_child)
    (* | "relacja synonimii" -> qmap *)
    | _ -> failwith "count_pwn_relation")

let count_pos_relation qmap lumap synmap rtmap rels t =
  Xlist.fold rels qmap (fun qmap rel ->
    match (*(IntMap.find rtmap rel.r_relation).rt_type,*)t with
      (*"relacja pomiędzy synsetami",*)"sr" -> add_pwn_qmap qmap rel (test_pos_elem get_pos_syn synmap rel.r_parent) (test_pos_elem get_pos_syn synmap rel.r_child)
    | (*"relacja leksykalna",*)"lr" -> add_pwn_qmap qmap rel (test_pos_elem get_pos_lu lumap rel.r_parent) (test_pos_elem get_pos_lu lumap rel.r_child)
    (* | "relacja synonimii" -> qmap *)
    | _ -> failwith "count_pwn_relation")

let select_plWordnet lumap synmap lr sr rtmap =
  let lr = Xlist.fold lr [] (fun lr rel ->
    if test_pwn_elem is_pwn_lu lumap rel.r_parent = "pl" &&
       test_pwn_elem is_pwn_lu lumap rel.r_child = "pl" &&
       IntSet.mem pl_pl_relations rel.r_relation then rel :: lr else lr) in
  let sr = Xlist.fold sr [] (fun sr rel ->
    if test_pwn_elem is_pwn_syn synmap rel.r_parent = "pl" &&
       test_pwn_elem is_pwn_syn synmap rel.r_child = "pl" &&
       IntSet.mem pl_pl_relations rel.r_relation then rel :: sr else sr) in
  let lumap = IntMap.fold lumap IntMap.empty (fun lumap id lu ->
    if is_pwn_lu lu then lumap else IntMap.add lumap id lu) in
  let synmap = IntMap.fold synmap IntMap.empty (fun synmap id syn ->
    if is_pwn_syn syn then synmap else IntMap.add synmap id syn) in
  let rtmap = IntMap.fold rtmap IntMap.empty (fun rtmap id rt ->
    if IntSet.mem pl_pl_relations id then IntMap.add rtmap id rt else rtmap) in
  lumap,synmap,lr,sr,rtmap

let create_relation_map rel_id rels =
  Xlist.fold rels Relation.empty (fun graph r ->
    if r.r_relation = rel_id then
      Relation.add_new graph r.r_parent r.r_child 0
    else graph)

let create_relation_maps rel_maps rels =
  Xlist.fold rels rel_maps (fun graphs r ->
    let graph = try IntMap.find graphs r.r_relation with Not_found -> Relation.empty in
    let graph = Relation.add_new graph r.r_parent r.r_child 0 in
    IntMap.add graphs r.r_relation graph)

let create_relation_map_lex lumap rel_id rels =
  Xlist.fold rels Relation.empty (fun graph r ->
    if r.r_relation = rel_id then
      let parent = (IntMap.find lumap r.r_parent).lu_syn in
      let child = (IntMap.find lumap r.r_child).lu_syn in
      Relation.add graph parent child 0
    else graph)

let create_relation_maps_lex rel_maps lumap rels =
  Xlist.fold rels rel_maps (fun graphs r ->
    let graph = try IntMap.find graphs r.r_relation with Not_found -> Relation.empty in
    let parent = (IntMap.find lumap r.r_parent).lu_syn in
    let child = (IntMap.find lumap r.r_child).lu_syn in
    let graph = Relation.add graph parent child 0 in
    IntMap.add graphs r.r_relation graph)

let assign_no_hipo synmap hipo =
  IntMap.mapi synmap (fun id syn ->
    {syn with syn_no_hipo=IntSet.size (Relation.find_descendants hipo id)})

let check_rel_class_coverage rel_maps rel_sets =
  let set = Xlist.fold (List.tl rel_sets) (List.hd rel_sets) IntSet.union in
  IntMap.iter rel_maps (fun rel_id _ ->
    if not (IntSet.mem set rel_id) then Printf.printf "only in rel_maps: %d\n" rel_id);
  IntSet.iter set (fun rel_id ->
    if not (IntMap.mem rel_maps rel_id) then Printf.printf "only in rel_sets: %d\n" rel_id)

let get_syn_id synmap lu_name lu_variant =
  let found = IntMap.fold synmap [] (fun found id syn ->
    Xlist.fold syn.syn_units found (fun found (_,lu) ->
      if lu.lu_name = lu_name && lu.lu_variant = lu_variant then
        id :: found else found)) in
  match found with
    [] -> failwith "get_syn_id: not found"
  | [id] -> id
  | _ -> failwith "get_syn_id: multiple id found"

let add_relations rel_maps rev_rel_maps ex_hipo relations =
  Xlist.fold relations ex_hipo (fun ex_hipo (cost,dir,rel_ids) ->
    Xlist.fold rel_ids ex_hipo (fun ex_hipo rel_id ->
      let graph = IntMap.find (if dir = Straight then rel_maps else rev_rel_maps) rel_id in
      IntMap.fold graph ex_hipo (fun ex_hipo parent children ->
        IntMap.fold children ex_hipo (fun ex_hipo child _ ->
          Relation.add_inc ex_hipo parent child cost min))))

let add_hipo_extensions synmap rel_maps rev_rel_maps ex_hipo hipo_extensions =
  Xlist.fold hipo_extensions ex_hipo (fun ex_hipo (cost,lu_name,lu_variant,dir,rel_ids) ->
    let hiper_id = get_syn_id synmap lu_name lu_variant in
    Xlist.fold rel_ids ex_hipo (fun ex_hipo rel_id ->
      let graph = IntMap.find (if dir = Parent then rel_maps else rev_rel_maps) rel_id in
      IntMap.fold graph ex_hipo (fun ex_hipo hipo_id _ ->
        Relation.add_inc ex_hipo hipo_id hiper_id cost min)))

let add_hipo_extensions2 synmap ex_hipo hipo_extensions =
  Xlist.fold hipo_extensions ex_hipo (fun ex_hipo (cost,lu_name,lu_variant,poss) ->
    let hiper_id = get_syn_id synmap lu_name lu_variant in
    IntMap.fold synmap ex_hipo (fun ex_hipo hipo_id syn ->
      if Xlist.mem poss syn.syn_pos then Relation.add_inc ex_hipo hipo_id hiper_id cost min else ex_hipo))

let create_ex_hipo synmap rel_maps rev_rel_maps =
  let ex_hipo = add_relations rel_maps rev_rel_maps IntMap.empty hipo_relations in
  let ex_hipo = add_hipo_extensions synmap rel_maps rev_rel_maps ex_hipo hipo_extensions in
  let ex_hipo = add_hipo_extensions2 synmap ex_hipo hipo_extensions2 in
  ex_hipo

let rec get_hipero_rec found ex_hipo id cost =
  let cost2 = try IntMap.find found id with Not_found -> max_int in
  if cost2 <= cost || cost > 7 then found else
  let found = IntMap.add found id cost in
  let map = try IntMap.find ex_hipo id with Not_found -> IntMap.empty in
  IntMap.fold map found (fun found id2 cost2 ->
    get_hipero_rec found ex_hipo id2 (cost + cost2))

let get_hipero ex_hipo syn_id =
  get_hipero_rec IntMap.empty ex_hipo syn_id 0

let select_big_synsets synmap threshold =
  IntMap.fold synmap IntSet.empty (fun selected id syn ->
    if syn.syn_no_hipo >= threshold then IntSet.add selected id else selected)

let print_subtree synmap ex_hipo path lu_name lu_variant =
  let syn_id = get_syn_id synmap lu_name lu_variant in
  let tree = Relation.descendants_tree ex_hipo syn_id 0 in
  File.file_out (path ^ lu_name ^ "-" ^ lu_variant ^ ".txt") (fun file ->
    Relation.print_tree file tree (fun syn_id cost ->
      let syn = IntMap.find synmap syn_id in
      let abstract = if syn.syn_abstract then "*" else "" in
      Printf.sprintf "%d %s%s" syn.syn_no_hipo abstract (syn_name syn)));
  File.file_out (path ^ lu_name ^ "-" ^ lu_variant ^ ".xml") (fun file ->
    Relation.print_tree_xml file tree (fun syn_id cost ->
      let syn = IntMap.find synmap syn_id in
      ["name",syn_name syn;
       "size",string_of_int syn.syn_no_hipo] @
      (if syn.syn_abstract then ["abstract","true"] else [])))

(* w semimport/plWordnet.ml była jeszcze procedura wypisująca poddrzewa słowosieci scalone z Walentym *)

let print_subtree_graph synmap hipo path lu_name lu_variant threshold =
  let syn_id = get_syn_id synmap lu_name lu_variant in
  let big = select_big_synsets synmap threshold in
  let hipo = Relation.select hipo (fun parent child cost -> IntSet.mem big parent && IntSet.mem big child) in
  let descendants = Relation.find_descendants hipo syn_id in
  let hipo2 = Relation.select hipo (fun parent child cost -> IntSet.mem descendants parent || IntSet.mem descendants child) in
  Relation.print_graph path (lu_name ^ "-" ^ lu_variant) true hipo2 (fun id ->
    let syn = IntMap.find synmap id in
    Printf.sprintf "%s\\n%d" (syn_name_single syn) syn.syn_no_hipo) (fun _ -> "")

let rt_names = ["type"; "reverse"; "name"; "description"; "posstr"; "display"; "shortcut"; "autoreverse"; "pwn"; "tests"]

let string_of_tests tests =
  String.concat " " (Xlist.map tests (fun (t,p) -> "(" ^ t ^ "," ^ p ^ ")"))

let string_of_rt rt =
  Printf.sprintf "\"%s\";\"%d\";\"%s\";\"%s\";\"%s\";\"%s\";\"%s\";\"%s\";\"%s\";\"%s\"" rt.rt_type rt.rt_reverse rt.rt_name rt.rt_description rt.rt_posstr
    rt.rt_display rt.rt_shortcut rt.rt_autoreverse rt.rt_pwn (string_of_tests rt.rt_tests)

let print_rt_map filename rel_count rtmap =
  File.file_out filename (fun file ->
    Printf.fprintf file "id;quantity;%s\n" (String.concat ";" rt_names);
    IntMap.iter rtmap (fun id rt ->
      Printf.fprintf file "%d;%d;%s\n" id (try IntQMap.find rel_count id with Not_found -> 0) (string_of_rt rt)))
