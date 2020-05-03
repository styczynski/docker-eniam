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

open ENIAMplWordnetTypes
open Xstd
open Printf

let print_morf_lu_relation filename lumap attr =
  File.file_out filename (fun file ->
    IntMap.iter attr (fun id1 l ->
      Xlist.iter l (fun id2 ->
        let lu1 = IntMap.find lumap id1 in
        let lu2 = IntMap.find lumap id2 in
        fprintf file "%s\t%s\n" lu1.lu_name lu2.lu_name)))

let syn_name syn =
  String.concat "|" (Xlist.map syn.syn_units (fun (_,lu) -> lu.lu_name))

let print_morf_syn_relation filename synmap attr =
  File.file_out filename (fun file ->
    IntMap.iter attr (fun id1 l ->
      Xlist.iter l (fun id2 ->
        let syn1 = IntMap.find synmap id1 in
        let syn2 = IntMap.find synmap id2 in
        fprintf file "%s\t%s\n" (syn_name syn1) (syn_name syn2))))

let print_morf_lu_relations lumap lr =
  let relations = Xlist.fold lr IntMap.empty (fun relations r ->
    let map = try IntMap.find relations r.r_relation with Not_found -> IntMap.empty in
    let map = IntMap.add_inc map r.r_parent [r.r_child] (fun l -> r.r_child :: l) in
    IntMap.add relations r.r_relation map) in
  IntMap.iter relations (fun id relation ->
    if IntSet.mem morf_relations id then
      print_morf_lu_relation ("results/morf_rel_" ^ string_of_int id ^ "_lu.tab") lumap relation;
    if IntSet.mem semimorf_relations id then
      print_morf_lu_relation ("results/semi_morf_rel_" ^ string_of_int id ^ "_lu.tab") lumap relation)

let print_morf_syn_relations synmap sr =
  let relations = Xlist.fold sr IntMap.empty (fun relations r ->
    let map = try IntMap.find relations r.r_relation with Not_found -> IntMap.empty in
    let map = IntMap.add_inc map r.r_parent [r.r_child] (fun l -> r.r_child :: l) in
    IntMap.add relations r.r_relation map) in
  IntMap.iter relations (fun id relation ->
    if IntSet.mem morf_relations id then
      print_morf_syn_relation ("results/morf_rel_" ^ string_of_int id ^ "_syn.tab") synmap relation;
    if IntSet.mem semimorf_relations id then
      print_morf_syn_relation ("results/semi_morf_rel_" ^ string_of_int id ^ "_syn.tab") synmap relation)

(* wygenerowanie relacji wyrażonych słowotwórczo *)
let print_morf_relations lumap synmap lr sr =
  print_morf_lu_relations lumap lr;
  print_morf_syn_relations synmap sr

let print_lumap lumap =
  File.file_out "resources/lu.tab" (fun file ->
    IntMap.iter lumap (fun _ lu ->
      fprintf file "%d\t%s\t%s\t%d\n" lu.lu_id lu.lu_name lu.lu_variant lu.lu_syn))

let string_of_pos = function
    "rzeczownik" -> "noun"
  | "czasownik" -> "verb"
  | "przymiotnik" -> "adj"
  | "przysłówek" -> "adv"
  | _ -> failwith "string_of_pos"

let print_synmap synmap =
  File.file_out "resources/syn.tab" (fun file ->
    IntMap.iter synmap (fun id syn ->
      let lu_ids = String.concat "\t" (Xlist.map syn.syn_units (fun (id,_) -> string_of_int id)) in
      fprintf file "%d\t%s\t%s\n" id (string_of_pos syn.syn_pos) lu_ids))

let print_ex_hipo ex_hipo =
  File.file_out "resources/ex_hipo.tab" (fun file ->
    Relation.print file ex_hipo string_of_int)

let _ =
  if Array.length Sys.argv < 2 then print_endline "missing argument" else (
  let lumap,synmap,lr,sr,rtmap = ENIAMplWordnet.load_data Sys.argv.(1) in
  print_endline "data loaded";
  let synmap = ENIAMplWordnet.merge_lu_syn lumap synmap in
  let lumap = ENIAMplWordnet.set_lu_syn lumap synmap in
  let lumap,synmap,lr,sr,rtmap = ENIAMplWordnet.select_plWordnet lumap synmap lr sr rtmap in
  print_endline "data prepared";
  print_morf_relations lumap synmap lr sr;
  let rel_count = ENIAMplWordnet.count_relations IntQMap.empty rtmap lr in
  let rel_count = ENIAMplWordnet.count_relations rel_count rtmap sr in
  ENIAMplWordnet.print_rt_map "results/rt.csv" rel_count rtmap;
  let rel_maps = ENIAMplWordnet.create_relation_maps IntMap.empty sr in
  let rel_maps = ENIAMplWordnet.create_relation_maps_lex rel_maps lumap lr in
  let rev_rel_maps = IntMap.map rel_maps Relation.reverse in
  let ex_hipo = ENIAMplWordnet.create_ex_hipo synmap rel_maps rev_rel_maps in
  print_lumap lumap;
  print_synmap synmap;
  print_ex_hipo ex_hipo;
  let hipo = ENIAMplWordnet.create_relation_map 11 sr in (* klucz (parent) jest hiperonimem *)
  let synmap = ENIAMplWordnet.assign_no_hipo synmap hipo in
  Xlist.iter hipo_roots (fun (name,variant,_) ->
    ENIAMplWordnet.print_subtree synmap hipo "results/" name variant);
  Xlist.iter hipo_roots (fun (name,variant,threshold) ->
    ENIAMplWordnet.print_subtree_graph synmap hipo "results/" name variant threshold);
  ())
