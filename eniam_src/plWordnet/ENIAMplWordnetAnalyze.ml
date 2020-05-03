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

(* UWAGA: pole rt_type nie jest wypełnione poprawnie w Słowosieci 3.0 *)

open ENIAMplWordnetTypes
open Xstd
open Printf

let print_stringqmap file qmap =
  StringQMap.iter qmap (fun k v ->
    fprintf file "%6d %s\n" v k)

let print_lexical_units_field path name selector lumap =
  File.file_out (path ^ name ^ ".tab") (fun file ->
    let qmap = IntMap.fold lumap StringQMap.empty (fun qmap _ lu ->
      StringQMap.add qmap (selector lu)) in
    StringQMap.iter qmap (fun k v ->
      fprintf file "%6d %s\n" v k))

let print_lexical_units_fields path lumap =
  print_lexical_units_field path "pos" (fun lu -> lu.lu_pos) lumap;
  print_lexical_units_field path "tagcount " (fun lu -> lu.lu_tagcount) lumap;
  print_lexical_units_field path "domain" (fun lu -> lu.lu_domain) lumap;
  print_lexical_units_field path "desc" (fun lu -> lu.lu_desc) lumap;
  print_lexical_units_field path "workstate" (fun lu -> lu.lu_workstate) lumap;
  print_lexical_units_field path "source" (fun lu -> lu.lu_source) lumap;
  ()

let print_lexical_units_full path lumap =
  let map = IntMap.fold lumap StringMap.empty (fun map _ lu ->
    StringMap.add_inc map lu.lu_pos [lu] (fun l -> lu :: l)) in
  StringMap.iter map (fun pos l ->
    File.file_out (path ^ pos ^ ".tab") (fun file ->
      Xlist.iter l (fun lu ->
        fprintf file "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" lu.lu_id lu.lu_name lu.lu_variant lu.lu_tagcount lu.lu_domain lu.lu_desc lu.lu_workstate lu.lu_source)))

let print_lu_relation filename lumap attr =
  File.file_out filename (fun file ->
    IntMap.iter attr (fun id1 l ->
      Xlist.iter l (fun id2 ->
        let lu1 = IntMap.find lumap id1 in
        let lu2 = IntMap.find lumap id2 in
        fprintf file "%s\t%s\n" (ENIAMplWordnet.lu_name lu1) (ENIAMplWordnet.lu_name lu2))))

let print_syn_relation filename synmap attr =
  File.file_out filename (fun file ->
    IntMap.iter attr (fun id1 l ->
      Xlist.iter l (fun id2 ->
        let syn1 = IntMap.find synmap id1 in
        let syn2 = IntMap.find synmap id2 in
        fprintf file "%s\t%s\n" (ENIAMplWordnet.syn_name syn1) (ENIAMplWordnet.syn_name syn2))))

let print_lu_relations path lumap lr =
  let relations = Xlist.fold lr IntMap.empty (fun relations r ->
    let map = try IntMap.find relations r.r_relation with Not_found -> IntMap.empty in
    let map = IntMap.add_inc map r.r_parent [r.r_child] (fun l -> r.r_child :: l) in
    IntMap.add relations r.r_relation map) in
  IntMap.iter relations (fun id relation ->
    print_lu_relation (path ^ string_of_int id ^ "_lu.tab") lumap relation)

let print_syn_relations path synmap sr =
  let relations = Xlist.fold sr IntMap.empty (fun relations r ->
    let map = try IntMap.find relations r.r_relation with Not_found -> IntMap.empty in
    let map = IntMap.add_inc map r.r_parent [r.r_child] (fun l -> r.r_child :: l) in
    IntMap.add relations r.r_relation map) in
  IntMap.iter relations (fun id relation ->
    print_syn_relation (path ^ string_of_int id ^ "_syn.tab") synmap relation)

let print_pwn_relations filename rel_pwn_count =
  File.file_out filename (fun file ->
    IntMap.iter rel_pwn_count (fun id qmap ->
      let l = StringQMap.fold qmap [] (fun l k v -> (sprintf "%s:%d" k v) :: l) in
      let s = String.concat "\t" (Xlist.sort l compare) in
      fprintf file "%d\t%s\n" id s))

let print_synset_names filename rel_maps rev_rel_maps synmap selected =
  File.file_out filename (fun file ->
    IntSet.iter selected (fun id ->
      let l = IntMap.fold rel_maps [] (fun l rel_id graph ->
        if Relation.mem_parent graph id then rel_id :: l else l) in
      let rels = String.concat "," (Xlist.map (List.rev l) string_of_int) in
      let l = IntMap.fold rev_rel_maps [] (fun l rel_id graph ->
        if Relation.mem_parent graph id then rel_id :: l else l) in
      let rev_rels = String.concat "," (Xlist.map (List.rev l) string_of_int) in
      let syn = IntMap.find synmap id in
      let names = ENIAMplWordnet.syn_name syn in
      fprintf file "%d\t%d\t%s\t%s\t%s\t%s\n" syn.syn_no_hipo id syn.syn_pos rels rev_rels names))

let print_synset_id synmap lu_name lu_variant =
  printf "name=%s variant=%s --> syn_id=%d\n" lu_name lu_variant (ENIAMplWordnet.get_syn_id synmap lu_name lu_variant)

let print_hipero synmap ex_hipo lu_name lu_variant =
  let syn_id = ENIAMplWordnet.get_syn_id synmap lu_name lu_variant in
  let hipero = ENIAMplWordnet.get_hipero ex_hipo syn_id in
  IntMap.iter hipero (fun id cost ->
    printf "%d %s\n" cost (ENIAMplWordnet.syn_name (IntMap.find synmap id)))

let print_subtree_cost synmap ex_hipo path lu_name lu_variant =
  let syn_id = ENIAMplWordnet.get_syn_id synmap lu_name lu_variant in
  let tree = Relation.descendants_tree ex_hipo syn_id 0 in
  File.file_out (path ^ lu_name ^ "-" ^ lu_variant ^ ".txt") (fun file ->
    Relation.print_tree file tree (fun syn_id cost -> sprintf "%d %s" cost (ENIAMplWordnet.syn_name (IntMap.find synmap syn_id))))(*;
  Relation.print_tree_as_graph "results/" "hipo_graph" tree (fun syn_id -> ENIAMplWordnet.syn_name_single (IntMap.find synmap syn_id)) string_of_int*)

(* FIXME: to trzeba dopracować: zrobić wielkość synsetu zależną od wielkości spójnej składowej *)
let print_connected_components_graph synmap hipo path threshold =
  let big = ENIAMplWordnet.select_big_synsets synmap threshold in
  let hipo = Relation.select hipo (fun parent child cost -> IntSet.mem big parent && IntSet.mem big child) in
  let r = Relation.sum hipo (Relation.reverse hipo) max in
  let conn = Relation.find_families r in
  ignore (Xlist.fold conn 1 (fun n conn ->
    let r = Relation.select hipo (fun parent child cost -> IntSet.mem conn parent && IntSet.mem conn child) in
    Relation.print_graph path ("conn_" ^ string_of_int n) true r (fun id ->
      let syn = IntMap.find synmap id in
      Printf.sprintf "%s\\n%d" (ENIAMplWordnet.syn_name_single syn) syn.syn_no_hipo) (fun _ -> "");
    n+1))

let _ =
  if Array.length Sys.argv < 2 then print_endline "missing argument" else (
  let lumap,synmap,lr,sr,rtmap = ENIAMplWordnet.load_data Sys.argv.(1) in
  print_endline "data loaded";
  print_lexical_units_fields "results/lufield_" lumap;
  print_lexical_units_full "results/lu_" lumap;
  ENIAMplWordnet.check_lu_syn_consistency lumap synmap;
  let synmap = ENIAMplWordnet.merge_lu_syn lumap synmap in
  let lumap = ENIAMplWordnet.set_lu_syn lumap synmap in
  (* Wykrycie i usunięcie angielskiego wordnetu *)
  let rel_count = ENIAMplWordnet.count_relations IntQMap.empty rtmap lr in
  let rel_count = ENIAMplWordnet.count_relations rel_count rtmap sr in
  ENIAMplWordnet.print_rt_map "results/rt.csv" rel_count rtmap;
  let rel_pwn_count = ENIAMplWordnet.count_pwn_relation IntMap.empty lumap synmap rtmap lr "lr" in
  let rel_pwn_count = ENIAMplWordnet.count_pwn_relation rel_pwn_count lumap synmap rtmap sr "sr" in
  print_pwn_relations "results/pwn_relations.csv" rel_pwn_count;
  let lumap,synmap,lr,sr,rtmap = ENIAMplWordnet.select_plWordnet lumap synmap lr sr rtmap in
  print_lu_relations "results/rels/rel_" lumap lr;
  print_syn_relations "results/rels/rel_" synmap sr;
  let rel_pos_count = ENIAMplWordnet.count_pos_relation IntMap.empty lumap synmap rtmap lr "lr" in
  let rel_pos_count = ENIAMplWordnet.count_pos_relation rel_pos_count lumap synmap rtmap sr "sr" in
  print_pwn_relations "results/pos_relations.csv" rel_pos_count;
  (* Utworzenie uogólnionej hiponimii *)
  let hipero = ENIAMplWordnet.create_relation_map 10 sr in (* klucz (parent) jest hiponimem *)
  let hipo = ENIAMplWordnet.create_relation_map 11 sr in (* klucz (parent) jest hiperonimem *)
  let synmap = ENIAMplWordnet.assign_no_hipo synmap hipo in
  Relation.test_reverse hipero hipo;
  Relation.test_reverse hipo hipero;
  let rel_maps = ENIAMplWordnet.create_relation_maps IntMap.empty sr in
  let rel_maps = ENIAMplWordnet.create_relation_maps_lex rel_maps lumap lr in
  ENIAMplWordnet.check_rel_class_coverage rel_maps [morf_relations;semimorf_relations;nomorf_relations];
  ENIAMplWordnet.check_rel_class_coverage rel_maps [instance_relations;synonymy_relations;attr_relations;has_relations;antonymy_relations;caus_relations];
  let rev_rel_maps = IntMap.map rel_maps Relation.reverse in
  let ex_hipo = ENIAMplWordnet.create_ex_hipo synmap rel_maps rev_rel_maps in
  let max_syn = Relation.select_childless (IntMap.fold synmap IntSet.empty (fun set id _ -> IntSet.add set id)) ex_hipo in
  print_synset_names "results/max_syn.tab" rel_maps rev_rel_maps synmap max_syn;
  (* print_hipero synmap ex_hipo "płatek" "2";
  print_subtree synmap ex_hipo "results/ex_tree_" "płatek" "2"; *)
  (* Analiza struktury hiponimii *)
  let max_syn = Relation.select_childless (IntMap.fold synmap IntSet.empty (fun set id _ -> IntSet.add set id)) hipero in
  print_synset_names "results/max_syn_hipo.tab" rel_maps rev_rel_maps synmap max_syn;
  Xlist.iter hipo_roots (fun (name,variant,_) ->
    ENIAMplWordnet.print_subtree synmap hipo "results/" name variant);
  Xlist.iter hipo_roots (fun (name,variant,threshold) ->
    ENIAMplWordnet.print_subtree_graph synmap hipo "results/" name variant threshold);
  print_connected_components_graph synmap hipo "results/" 100;
  ())
