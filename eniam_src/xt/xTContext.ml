(*
 *  XT, a library that converts XLE output into ENIAM format.
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

open XTTypes
open Xstd

exception No_valid_choice

let intersection = function
    CEmpty,c -> c
  | c,CEmpty -> c
  | CModel a,CModel b ->
      let c = BitArray.intersection a b in
      if BitArray.is_empty c then raise No_valid_choice else CModel c
  | _,_ -> failwith "intersection"

let union = function
    CEmpty,c -> CEmpty
  | c,CEmpty -> CEmpty
  | CModel a,CModel b -> CModel(BitArray.union a b)
  | COr[],c -> c
  | c,COr[] -> c
  | _,_ -> failwith "union"

let rec difference model_size = function
    CEmpty,c -> difference model_size (CModel(BitArray.full model_size),c)
  | _,CEmpty -> raise No_valid_choice
  | CModel a, CModel b ->
     let c = BitArray.difference a b in
     if BitArray.is_empty c then raise No_valid_choice else CModel c
  | _ -> failwith "difference"

let rec assign_defines defines = function
    CEmpty -> CEmpty
  | CVar(v,i) -> CVar(v,i)
  | CDef v -> (try StringMap.find defines v with Not_found -> failwith ("assign_defines: " ^ v))
  | COr l -> COr(Xlist.map l (assign_defines defines))
  | CModel _ -> failwith "assign_defines"

let rec is_active cvar_map selected = function
    CEmpty -> true
  | CVar(v,i) -> let j = try StringMap.find selected v with Not_found -> "" in i = j
  | COr l -> Xlist.fold l false (fun b c -> b || is_active cvar_map selected c)
  | CDef _ -> failwith "is_active"
  | CModel model ->
       let l = StringMap.fold selected [] (fun l v i -> StringMap.find cvar_map (v ^ i) :: l) in
       if l = [] then failwith "is_active 1" else
       (match IntSet.to_list (Xlist.fold (List.tl l) (List.hd l) IntSet.intersection) with
          [n] -> BitArray.mem model n
        | _ -> failwith "is_active 2")

let rec iterate_paths choices selected f =
  if choices = [] then f selected else
  let l,c = List.hd choices in
  if is_active StringMap.empty selected c then
    Xlist.iter l (function
        CVar(v,i) -> iterate_paths (List.tl choices) (StringMap.add selected v i) f
      | _ -> failwith "iterate_paths")
  else iterate_paths (List.tl choices) selected f

let rec fold_paths choices selected s f =
  if choices = [] then f s selected else
  let l,c = List.hd choices in
  if is_active StringMap.empty selected c then
    Xlist.fold l s (fun s -> function
        CVar(v,i) -> fold_paths (List.tl choices) (StringMap.add selected v i) s f
      | _ -> failwith "fold_paths")
  else fold_paths (List.tl choices) selected s f

(*let x = ref 0
let time1 = ref 0.
let time2 = ref 0.*)

let create_path_list choices =
(*  Printf.printf "no choices = %d\n%!" (Xlist.size choices);
   x := 0;
  time1 := Unix.gettimeofday ();*)
  Xlist.fold choices [StringMap.empty] (fun selected_list (l,c) ->
(*    incr x;
    if !x mod 100 = 0 then (
      time2 := Unix.gettimeofday ();
      let time = (!time2 -. !time1) *. 1000. in
      let y = float (Xlist.size selected_list) in
      Printf.printf "%8d %6d / %d %12.6f %f %f %f\n%!" (Xlist.size selected_list) (!x) (Xlist.size choices) time (time /. y) (time /. y /. log y) (time /. float (!x));
      time1 := !time2);*)
    if Xlist.size selected_list > !max_count_paths then failwith "count_paths" else
    Xlist.fold selected_list [] (fun selected_list selected ->
      if is_active StringMap.empty selected c then
        Xlist.fold l selected_list (fun selected_list -> function
          CVar(v,i) -> (StringMap.add selected v i) :: selected_list
        | _ -> failwith "fold_paths")
      else selected :: selected_list))

let create_path_array choices =
  let l = fold_paths choices StringMap.empty [] (fun l path -> path :: l) in
  let paths = Array.of_list l in
  let model_size,cvar_map = Array.fold_left (fun (n,map) path ->
    n+1, StringMap.fold path map (fun map v i ->
      StringMap.add_inc map (v ^ i) (IntSet.singleton n) (fun set -> IntSet.add set n))) (0,StringMap.empty) paths in
(*  StringMap.iter cvar_map (fun k set ->
    Printf.printf "%s: %s\n" k (String.concat " " (Xlist.map (IntSet.to_list set) string_of_int)));*)
  cvar_map,model_size

let rec create_model_rec cvar_map model = function
    CEmpty -> failwith "create_model_rec"
  | CVar(v,i) -> IntSet.fold (StringMap.find cvar_map (v ^ i)) model BitArray.add
  | COr l -> Xlist.fold l model (create_model_rec cvar_map)
  | CDef _ -> failwith "create_model_rec"
  | CModel _ -> failwith "create_model_rec"

let create_model cvar_map c =
  if c = CEmpty then CEmpty else
  let model = BitArray.empty in
  let model = create_model_rec cvar_map model c in
  CModel model

let equal = function
    CModel m1, CModel m2 -> BitArray.equal m1 m2
  | CEmpty,CEmpty -> true
  | CEmpty,_ -> false
  | _,CEmpty -> false
  | CModel _,_ -> failwith "equal: ni"
  | _,CModel _ -> failwith "equal: ni"
  | x,y -> x = y

let get_choice_var_name = function
    [] -> failwith "get_choice_var_name"
  | CVar(v,_) :: l -> v
  | _ -> failwith "get_choice_var_name"

let process_choices choices =
  Xlist.fold choices StringMap.empty (fun choices (cl,c) ->
    let v = get_choice_var_name cl in
    StringMap.add choices v c)

let add_relative_complement l = function
    CEmpty -> l
  | CModel m ->
     let c = Xlist.fold l BitArray.empty (fun c -> function
         CModel m -> BitArray.union c m
       | _ -> failwith "add_relative_complement") in
     let c = BitArray.difference m c in
     if BitArray.is_empty c then l else CModel c :: l
  | _ -> failwith "add_relative_complement"

let create_partition l =
(*   print_endline ("CP " ^ String.concat " " (Xlist.map l XTStringOf.context_term)); *)
  let l = Xlist.map l (function
      CModel m -> m
    | CEmpty -> failwith "create_partition 1"
    | _ -> failwith "create_partition 2") in
  let partition = BitArray.partition l in
  fst (Xlist.fold partition ([],1) (fun (partition,i) m -> (m,string_of_int i) :: partition, i+1))

let apply_partition partition = function
     CModel m -> Xlist.fold partition [] (fun l (p,i) ->
       if BitArray.is_empty (BitArray.intersection m p) then l else (i,p) :: l)
  | _ -> failwith "apply_partition"

let part_map_empty = BitArrayMap.empty

let part_map_add map c v =
  match c with
    CModel m -> BitArrayMap.add map m v
  | _ -> failwith "part_map_add"

let part_map_find map = function
    CModel m -> BitArrayMap.find map m
  | CEmpty -> CEmpty
  | _ -> failwith "part_map_find"

let print_part_map map =
  BitArrayMap.iter map (fun m v ->
    Printf.printf "%4s %180s\n" (XTStringOf.context_term v) (XTStringOf.context_term (CModel m)))

let rec normalize vars choices = function
    [] -> []
  | CVar(v,i) :: l -> if Xlist.mem vars (StringMap.find choices v) then normalize vars choices l else CVar(v,i) :: normalize vars choices l
  | _ -> failwith "normalize"

let convert_context map choices = function
    CModel m ->
(*        print_endline ("C1 " ^ XTStringOf.context_term (CModel m)); *)
       let l = BitArrayMap.fold map [] (fun l p c ->
         if BitArray.equal (BitArray.union m p) m then c :: l else l) in
       let c = (match normalize l choices l with
          [] -> (*Printf.printf "c_c  %180s\n" (XTStringOf.context_term (CModel m));
                Printf.printf "%s\n%!" (String.concat " " (Xlist.map l XTStringOf.context_term));
                CDef "xxx"*)failwith "convert_context 2"
        | [c] -> c
        | l -> COr l) in
(*        print_endline ("C2 " ^ XTStringOf.context_term c); *)
       c
  | CEmpty -> CEmpty
  | _ -> failwith "convert_context 1"
