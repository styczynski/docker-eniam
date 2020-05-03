(*
 *  ENIAMsemantics implements semantic processing for ENIAM
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

open ENIAM_LCGtypes
open Xstd

let _ = Random.self_init ()

let rec get_nth n = function
    [] -> failwith "get_nth"
  | (i,_) :: l -> if n = 0 then i else get_nth (n-1) l

let rec select_random_rec selection = function
    Ref i -> selection
  | Node t ->
      let selection = select_random_rec selection t.args in
      Xlist.fold t.attrs selection (fun selection (_,t) -> select_random_rec selection t)
  | Variant(e,l) ->
      let selected,selection =
        if StringMap.mem selection e then
          StringMap.find selection e, selection
        else
          let selected =
           if e = "" then Xlist.map l fst
           else [get_nth (Random.int (Xlist.size l)) l] in
          selected, StringMap.add selection e selected in
      (* Printf.printf "select_random_rec: %s [%s]\n%!" e (String.concat ";" selected); *)
      Xlist.fold l selection (fun selection (i,t) ->
        if Xlist.mem selected i then select_random_rec selection t else selection)
  | Tuple l -> Xlist.fold l selection select_random_rec
  | Val _ -> selection
  | Dot -> selection
  | t -> failwith ("select_random_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let select_random tree =
  Int.fold 0 (ExtArray.size tree - 1) StringMap.empty (fun selection i ->
    select_random_rec selection (ExtArray.get tree i))

let rec apply_selection_rec selection = function
    Ref i -> Ref i
  | Node t ->
      Node{t with args=apply_selection_rec selection t.args;
        attrs=Xlist.map t.attrs (fun (k,v) -> k, apply_selection_rec selection v)}
  | Variant(e,l) ->
      if not (StringMap.mem selection e) then Dot
        (*failwith ("apply_selection_rec: unknown label '" ^ e ^ "'")*) else
      let selected = StringMap.find selection e in
      (* Printf.printf "apply_selection_rec: %s [%s]\n%!" e (String.concat ";" selected); *)
      let l = Xlist.fold l [] (fun l (i,t) ->
        if Xlist.mem selected i then (i,t) :: l else l) in
      (match l with
        [] -> (*failwith "apply_selection_rec: empty selection"*) Dot
      | [_,t] -> apply_selection_rec selection t
      | l ->
        let l = Xlist.rev_map l (fun (i,t) ->
          i, apply_selection_rec selection t) in
        Variant(e,l))
  | Tuple l ->
      let l = Xlist.rev_map l (apply_selection_rec selection) in
      Tuple(List.rev l)
  | Val s -> Val s
  | Dot -> Dot
  | t -> failwith ("apply_selection_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let apply_selection selection tree =
  let result_tree = Array.make (Array.length tree) Dot in
  Int.iter 0 (Array.length tree - 1) (fun i ->
    result_tree.(i) <- apply_selection_rec selection tree.(i));
  result_tree

let rec make_rearrange_map tree map next = function
    Ref i ->
      if IntMap.mem map i then map,next else
      let map = IntMap.add map i next in
      make_rearrange_map tree map (next+1) tree.(i)
  | Node t -> make_rearrange_map tree map next t.args
  | Variant(e,l) -> Xlist.fold l (map,next) (fun (map,next) (i,t) -> make_rearrange_map tree map next t)
  | Tuple l -> Xlist.fold l (map,next) (fun (map,next) -> make_rearrange_map tree map next)
  | Dot -> map,next
  | t -> failwith ("make_rearrange_map: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec rearrange_refs map = function
    Ref i -> Ref (try IntMap.find map i with Not_found -> failwith "rearrange_refs")
  | Node t -> Node{t with args=rearrange_refs map t.args}
  | Variant(e,l) ->
      let l = Xlist.rev_map l (fun (i,t) -> i, rearrange_refs map t) in
      Variant(e,List.rev l)
  | Tuple l ->
      let l = Xlist.rev_map l (rearrange_refs map) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("make_rearrange_map: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rearrange_tree tree =
  let map = IntMap.add IntMap.empty 0 0 in
  let map,next = make_rearrange_map tree map 1 tree.(0) in
  let result_tree = Array.make next Dot in
  IntMap.iter map (fun orig res ->
    result_tree.(res) <- rearrange_refs map tree.(orig));
  result_tree

let random_tree tokens lex_sems tree =
  (* print_endline "random_tree"; *)
  let selection = select_random tree in
  let tree = apply_selection selection (ExtArray.to_array tree) in
  rearrange_tree tree

let rec selprefs_rec cost = function
    Ref i -> cost.(i), Ref i
  | Node t -> -1, Node{t with args = snd(selprefs_rec cost t.args)}
  | Variant(e,l) ->
      let c,l = Xlist.fold  l (max_int,[]) (fun (min_c,l) (i,t) ->
        let c,t = selprefs_rec cost t in
        if c < min_c then c,[i,t] else
        if c > min_c then min_c,l else
        min_c, (i,t) :: l) in
      (match l with
        [_,t] -> c,t
      | _ -> c,Variant(e,List.rev l))
  | Tuple l ->
      let c,l = Xlist.fold l (0,[]) (fun (c,l) t ->
        let c2,t = selprefs_rec cost t in
        c+c2, t :: l) in
      c,Tuple(List.rev l)
  | Dot -> 0, Dot
  | t -> failwith ("selprefs_rec: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec get_attr pat = function
    [] -> raise Not_found
  | (s,v) :: l ->
      if s = pat then v
      else get_attr pat l

let rec list_of_selprefs = function
    Val s -> [s]
  | Dot -> []
  | Tuple l -> List.flatten (Xlist.rev_map l list_of_selprefs)
  | t -> failwith ("list_of_selprefs: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let map_of_hipero = function
    Variant(_,l) -> Xlist.fold l StringMap.empty (fun map -> function
        _,Tuple[Val hipero; Val cost] -> StringMap.add_inc map hipero (int_of_string cost) (fun cost2 -> min (int_of_string cost) cost2)
      | _ -> failwith "map_of_hipero 2")
  | Tuple[Val hipero; Val cost] -> StringMap.add StringMap.empty hipero (int_of_string cost)
  | t -> failwith ("map_of_hipero: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let rec count_selprefs_cost tree cost = function
    Ref i ->
      if cost.(i) = -1 then
        let c = count_selprefs_cost tree cost (ExtArray.get tree i) in
        cost.(i) <- c;
        c
      else cost.(i)
  | Node t ->
      (count_selprefs_cost tree cost t.args) +
      (match try get_attr "gf" t.attrs with Not_found -> Val "" with
        Val "adjunct" -> 100
      | Val "subj" | Val "obj" | Val "arg" | Val "core" ->
          if get_attr "rev-hipero" t.attrs = Val "+" then 0 else
          let selprefs = try list_of_selprefs (get_attr "selprefs" t.attrs) with Not_found -> failwith ("count_selprefs_cost: no selprefs " ^ t.lemma) in
          let hipero = try map_of_hipero (get_attr "hipero" t.attrs) with Not_found -> failwith ("count_selprefs_cost: no hipero " ^ t.lemma) in
          Xlist.fold selprefs 1000 (fun cost selpref ->
            try min cost (StringMap.find hipero selpref) with Not_found -> cost)
      | Val "" -> 200
      | Val s -> failwith ("count_selprefs_cost: unknown gf=" ^ s ^ " for " ^ t.lemma)
      | _ -> failwith "count_selprefs_cost")
  | Variant(e,l) ->
      Xlist.fold l max_int (fun min_c (_,t) ->
        min min_c (count_selprefs_cost tree cost t))
  | Tuple l -> Xlist.fold l 0 (fun c t -> c + count_selprefs_cost tree cost t)
  | Dot -> 0
  | t -> failwith ("count_selprefs_cost: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let selprefs tree =
  let cost = Array.make (ExtArray.size tree) (-1) in
  cost.(0) <- count_selprefs_cost tree cost (ExtArray.get tree 0);
  Int.iter 0 (ExtArray.size tree - 1) (fun i ->
    ExtArray.set tree i (snd (selprefs_rec cost (ExtArray.get tree i))));
  ()

let merge_variant e l =
  let set = Xlist.fold l TermSet.empty (fun set (_,t) -> TermSet.add set t) in
  if TermSet.size set = 1 then TermSet.max_elt set else
  Variant(e,l)

let merge_nodes result_tree t_map e l =
  let l = Xlist.rev_map l (function
      i, Ref id -> i, (match ExtArray.get result_tree id with Node t -> t | _ -> raise Not_found)
    | _ -> raise Not_found) in
  let _,h = List.hd l in
  let h_cat = try get_attr "CAT" h.attrs with Not_found -> Dot in
  let h_coerced = try get_attr "COERCED" h.attrs with Not_found -> Dot in
  Xlist.iter (List.tl l) (fun (_,t) ->
    let t_cat = try get_attr "CAT" t.attrs with Not_found -> Dot in
    let t_coerced = try get_attr "COERCED" t.attrs with Not_found -> Dot in
    if h.orth <> t.orth || h.lemma <> t.lemma || h.pos <> t.pos || h.weight <> t.weight ||
      h.id <> t.id || h.symbol <> t.symbol || h_cat <> t_cat || h_coerced <> t_coerced ||
      h.arg_symbol <> t.arg_symbol || h.arg_dir <> t.arg_dir then raise Not_found else ());
  let args = Xlist.fold l [] (fun l (i,t) -> (i,t.args) :: l) in
  let attrs = Xlist.fold l StringMap.empty (fun map (i,t) ->
    Xlist.fold t.attrs map (fun map (k,v) ->
      StringMap.add_inc map k [i,v] (fun l -> (i,v) :: l))) in
  let args = merge_variant e args in
  let attrs = StringMap.fold attrs [] (fun l k v ->
    (k,merge_variant e v) :: l) in
  let t = Node{h with args=args; attrs=attrs} in
      let s = ENIAM_LCGstringOf.linear_term 0 t in
      if StringMap.mem !t_map s then Ref(StringMap.find !t_map s) else (
      let id = ExtArray.add result_tree t in
      t_map := StringMap.add !t_map s id;
      Ref id)

let rec merge_rec tree result_tree id_map t_map = function
    Ref i ->
      if IntMap.mem !id_map i then Ref(IntMap.find !id_map i) else
      let t = merge_rec tree result_tree id_map t_map (ExtArray.get tree i) in
      let s = ENIAM_LCGstringOf.linear_term 0 t in
      if StringMap.mem !t_map s then Ref(StringMap.find !t_map s) else (
      let id = ExtArray.add result_tree t in
      id_map := IntMap.add !id_map i id;
      t_map := StringMap.add !t_map s id;
      Ref id)
  | Node t -> Node{t with args=merge_rec tree result_tree id_map t_map t.args}
  | Variant(e,l) ->
      let map = Xlist.fold l StringMap.empty (fun map (i,t) ->
        let t = merge_rec tree result_tree id_map t_map t in
        StringMap.add map (ENIAM_LCGstringOf.linear_term 0 t) t) in
      let _,l = StringMap.fold map (1,[]) (fun (i,l) _ t -> i+1, (string_of_int i,t) :: l) in
      (match l with
        [_,t] -> t
      | _ -> (try merge_nodes result_tree t_map e l with Not_found -> Variant(e,List.rev l)))
  | Tuple l ->
      let l = Xlist.rev_map l (merge_rec tree result_tree id_map t_map) in
      Tuple(List.rev l)
  | Dot -> Dot
  | t -> failwith ("merge: " ^ ENIAM_LCGstringOf.linear_term 0 t)

let merge tree =
  let result_tree = ExtArray.make (ExtArray.size tree / 4) Dot in
  let _ = ExtArray.add result_tree Dot in
  let t = merge_rec tree result_tree (ref IntMap.empty) (ref StringMap.empty) (ExtArray.get tree 0) in
  ExtArray.set result_tree 0 t;
  result_tree
