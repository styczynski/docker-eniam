(*
 *  ENIAMlexSemantics is a library that assigns tokens with lexicosemantic information.
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
open ENIAMlexSemanticsTypes

let lu_names = ref IntMap.empty
let lumap = ref StringMap.empty
let synmap = ref IntMap.empty
let ex_hipo = ref IntMap.empty
let predef_names = ref IntMap.empty
let proper_classes = ref StringMap.empty
let predef = ref StringMap.empty

let load_lu filename =
  File.fold_tab filename (IntMap.empty,StringMap.empty) (fun (lu_names,lumap) -> function
      [lu_id; lemma; variant; syn_id] ->
        let v = variant,int_of_string syn_id in
        IntMap.add lu_names (int_of_string lu_id) (lemma,variant,int_of_string syn_id),
        StringMap.add_inc lumap lemma [v] (fun l -> v :: l)
    | l -> failwith ("load_lu: " ^ String.concat "\t" l))

let load_syn filename =
  File.fold_tab filename IntMap.empty (fun synmap -> function
      syn_id :: pos :: lu_ids ->
        let lu_ids = Xlist.map lu_ids int_of_string in
        IntMap.add synmap (int_of_string syn_id) (pos,lu_ids)
    | l -> failwith ("load_syn: " ^ String.concat "\t" l))

let load_ex_hipo filename =
  File.fold_tab filename IntMap.empty (fun ex_hipo -> function
      [parent; child; cost] ->
         let parent = int_of_string parent in
         let child = int_of_string child in
         let cost = int_of_string cost in
         let children = try IntMap.find ex_hipo parent with Not_found -> IntMap.empty in
         let children = IntMap.add_inc children child cost (fun _ -> failwith "load_ex_hipo") in
         IntMap.add ex_hipo parent children
    | l -> failwith ("load_ex_hipo: " ^ String.concat "\t" l))

let syn_id_of_sense sense =
  let lemma,variant =
    match List.rev (Xstring.split "-" sense) with
      variant :: l -> String.concat "-" (List.rev l), variant
    | _ -> failwith "syn_id_of_sense 1" in
  let l = Xlist.fold (try StringMap.find !lumap lemma with Not_found -> failwith ("syn_id_of_sense: " ^ lemma)) [] (fun l (variant2,syn_id) ->
    if variant = variant2 then syn_id :: l else l) in
  match l with
    [syn_id] -> syn_id
  | _ -> failwith ("syn_id_of_sense 2: " ^ lemma)

let load_predef ex_hipo filename =
  (* print_endline "load_predef"; *)
  let ex_hipo,predef_names,predef,_ =
    File.fold_tab filename (ex_hipo,IntMap.empty,StringMap.empty,-1) (fun (ex_hipo,predef_names,predef,id) -> function
      name :: senses ->
        let ex_hipo = Xlist.fold senses ex_hipo (fun ex_hipo sense ->
          let hipo_id = try StringMap.find predef sense with Not_found -> syn_id_of_sense sense in
          let children = try IntMap.find ex_hipo hipo_id with Not_found -> IntMap.empty in
          let children = IntMap.add_inc children id 0 (fun _ -> failwith "load_predef 1") in
          IntMap.add ex_hipo hipo_id children) in
        let predef_names = IntMap.add predef_names id name in
        let predef = StringMap.add_inc predef name id (fun _ -> failwith "load_predef 2") in
        ex_hipo, predef_names, predef, id-1
    | l -> failwith ("load_predef: " ^ String.concat "\t" l)) in
  ex_hipo,predef_names,predef

let rec get_hipero_rec found ex_hipo id cost =
  let cost2 = try IntMap.find found id with Not_found -> max_int in
  if cost2 <= cost || cost > hipero_threshold then found else
  let found = IntMap.add found id cost in
  let map = try IntMap.find ex_hipo id with Not_found -> IntMap.empty in
  IntMap.fold map found (fun found id2 cost2 ->
    get_hipero_rec found ex_hipo id2 (cost + cost2))

let get_hipero syn_id =
  get_hipero_rec IntMap.empty !ex_hipo syn_id 0

exception SynsetNotFound

let synset_name syn_id =
  if IntMap.mem !predef_names syn_id then IntMap.find !predef_names syn_id else
  let lemma,variant,_ =
    try IntMap.find !lu_names (List.hd (snd (IntMap.find !synmap syn_id)))
    with Not_found -> raise SynsetNotFound (*failwith ("synset_name: " ^ string_of_int syn_id)*) in
  lemma ^ "-" ^ variant

let load_proper_classes filename =
  (* print_endline "load_proper_classes"; *)
  File.fold_tab filename StringMap.empty (fun map -> function
      id :: senses ->
        let senses = Xlist.map senses (fun sense ->
          match List.rev (Str.split (Str.regexp " ") sense) with
            weight :: l -> String.concat " " (List.rev l), (try float_of_string weight with _ -> failwith "load_proper_classes 2")
          | _ -> failwith "load_proper_classes 4") in
        let senses = Xlist.map senses (fun (sense,weight) ->
          (* let sense = if sense = "antroponim 1" then "nazwa własna 1" else sense in
          let sense = if sense = "godzina 4" then "godzina 3" else sense in *)
(*           print_endline sense; *)
          let syn_id = syn_id_of_sense sense in
          sense,IntMap.fold (get_hipero syn_id) [] (fun hipero syn_id cost -> (synset_name syn_id, cost) :: hipero),weight) in
        StringMap.add_inc map id senses (fun _ -> failwith ("load_proper_classes 3: " ^ id))
    | l -> failwith ("load_proper_classes: " ^ String.concat "\t" l))

let simplify_pos = function
    "subst" -> "noun"
  | "depr" -> "noun"
  | "adj" -> "adj"
  | "adja" -> "adj"
  | "adjc" -> "adj"
  | "adjp" -> "adj"
  | "ger" -> "verb"
  | "pact" -> "verb"
  | "ppas" -> "verb"
  | "fin" -> "verb"
  | "bedzie" -> "verb"
  | "praet" -> "verb"
  | "winien" -> "verb"
  | "impt" -> "verb"
  | "imps" -> "verb"
  | "inf" -> "verb"
  | "pcon" -> "verb"
  | "pant" -> "verb"
  | "pred" -> "verb"
  | s -> s

let find_senses lemma pos =
(*if pos = "ppron12" || pos = "ppron3" || pos = "siebie" then {t with senses=[lemma,["0"],0.]} else*) (* FIXME: ustalić co z zaimkami *)
  let l = try StringMap.find !lumap lemma with Not_found -> [] in
  let pos = simplify_pos pos in
  Xlist.fold l [] (fun l (variant,syn_id) ->
    let pos2,_ = try IntMap.find !synmap syn_id with Not_found -> failwith "find_senses" in
    if pos <> pos2 then l else
    (lemma ^ "-" ^ variant,
     IntMap.fold (get_hipero syn_id) [] (fun hipero syn_id cost -> (synset_name syn_id, cost) :: hipero),
     log10 (1. /. (try float_of_string variant with _ -> 3.))) :: l)

let find_proper_senses senses =
  List.flatten (Xlist.rev_map senses (fun sense ->
    try StringMap.find !proper_classes sense with Not_found -> failwith ("find_proper_senses: " ^ sense)))

let find_sense lu_id =
  let lemma,variant,syn_id = IntMap.find !lu_names lu_id in
  lemma ^ "-" ^ variant,
  IntMap.fold (get_hipero syn_id) [] (fun hipero syn_id cost -> (synset_name syn_id, cost) :: hipero),
  log10 (1. /. (try float_of_string variant with _ -> 3.))

let initialize () =
  let a,b = load_lu lu_filename in
  lu_names := a;
  lumap := b;
  synmap := load_syn syn_filename;
  ex_hipo := load_ex_hipo ex_hipo_filename;
  let a,b,c = load_predef !ex_hipo predef_filename in
  ex_hipo := a;
  predef_names := b;
  predef := c;
  proper_classes := load_proper_classes proper_classes_filename;
  ()
