(*
 *  ENIAM: Categorial Syntactic-Semantic Parser for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open LCGtypes
open Xstd
open Printf

let rec disambiguate_nodes_choice references disamb weights r = function 
    Choice choice -> 
      let choice,sat = StringMap.fold choice (StringMap.empty,StringMap.empty) (fun (choice,sat) ei t ->
        let t,w = disambiguate_nodes_variant references disamb weights t in
        StringMap.add choice ei t, StringMap.add sat ei w) in
      weights.(r) <- sat;
      Choice choice
  | _ -> failwith "disambiguate_nodes_choice"
  
and disambiguate_nodes_variant references disamb weights = function 
    Variant(e,l) -> 
      let l,max_w = Xlist.fold l ([],-.max_float) (fun (l,max_w) (i,t) ->
        let t,w = disambiguate_nodes_rec references disamb weights (e ^ i) t in
        if w = max_w then (i,t) :: l, max_w else
        if w > max_w then [i,t],w else l,max_w) in
    Variant(e, List.rev l),max_w
  | _ -> failwith "disambiguate_nodes_variant"
  
and disambiguate_nodes_rec references disamb weights ei = function 
    Node t -> 
      let args,w = disambiguate_nodes_rec references disamb weights ei t.args in
      Node{t with args=args},w +. t.weight
  | Tuple l -> 
      let l,sum_w = Xlist.fold l ([],0.) (fun (l,sum_w) t ->
        let t,w = disambiguate_nodes_rec references disamb weights ei t in
        t :: l, sum_w +. w) in
      Tuple(List.rev l), sum_w
  | Variant(e,l) as t -> 
      let l,max_w = Xlist.fold l ([],-.max_float) (fun (l,max_w) (i,t) ->
        let t,w = disambiguate_nodes_rec references disamb weights ei t in
        if w = max_w then (i,t) :: l, max_w else
        if w > max_w then [i,t],w else l,max_w) in
      let l = 
        let map = Xlist.fold l TermSet.empty (fun map (_,t) -> TermSet.add map t) in
        fst (TermSet.fold map ([],1) (fun (l,i) t -> (string_of_int i,t) :: l, i+1)) in
      (match l with
        [] -> failwith ("disambiguate_nodes_rec 1" ^ LCGstringOf.linear_term 0 t)
      | [i,t] -> t,max_w
      | _ -> Variant(e, List.rev l),max_w)
  | Dot -> Dot, 0.
  | Val s -> Val s, 0.
  | Ref i -> 
       if disamb.(i) = Dot then (disamb.(i) <- disambiguate_nodes_choice references disamb weights i references.(i));
       Ref i, (try StringMap.find weights.(i) ei with Not_found -> failwith "disambiguate_nodes_rec 3")
  | t -> failwith ("disambiguate_nodes_rec 2: " ^ LCGstringOf.linear_term 0 t)

let disambiguate_nodes references =
  let disamb = Array.make (Array.length references) Dot in
  let weights = Array.make (Array.length references) StringMap.empty in
  disamb.(0) <- fst (disambiguate_nodes_variant references disamb weights references.(0));
  disamb

(***************************************************************************************)

let rec remove_unused_rec disamb (i,map,rev_map) = function 
    Tuple l -> Xlist.fold l (i,map,rev_map) (remove_unused_rec disamb)
  | Variant(e,l) -> Xlist.fold l (i,map,rev_map) (fun (i,map,rev_map) (_,t) -> remove_unused_rec disamb (i,map,rev_map) t)
  | Choice choice -> StringMap.fold choice (i,map,rev_map) (fun (i,map,rev_map) _ t -> remove_unused_rec disamb (i,map,rev_map) t)
  | Dot -> (i,map,rev_map)
  | Val s -> (i,map,rev_map)
  | Node t -> remove_unused_rec disamb (i,map,rev_map) t.args
(*   | Morf m -> (i,map,rev_map) *)
  | Ref r -> 
(*        printf "ref=%d\n%!" r; *)
       if IntMap.mem map r then i,map,rev_map else
       let i,map,rev_map = remove_unused_rec disamb (i,map,rev_map) disamb.(r) in (
(*        printf "add ref %d %d\n%!" r i; *)
       i+1, IntMap.add map r i, IntMap.add rev_map i r)
  | _ -> failwith "remove_unused_rec"

let rec rename_references map = function 
    Tuple l -> Tuple(Xlist.map l (rename_references map))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, rename_references map t))
  | Choice choice -> Choice(StringMap.map choice (fun t -> rename_references map t))
  | Dot -> Dot
  | Val s -> Val s
  | Node t -> Node{t with args=rename_references map t.args}
  | Morf m -> Morf m
  | Ref r -> Ref(IntMap.find map r)
  | _ -> failwith "rename_references"

let remove_unused disamb =
  let size,map,rev_map = remove_unused_rec disamb (1,IntMap.add IntMap.empty 0 0,IntMap.add IntMap.empty 0 0) disamb.(0) in
  let a = Array.make size Dot in
  Int.iter 0 (size-1) (fun i ->
    a.(i) <- rename_references map disamb.(IntMap.find rev_map i));
  a

let rec remove_unused_choices_rec disamb (i,map,rev_map) = function 
    Tuple l -> Xlist.fold l (i,map,rev_map) (remove_unused_choices_rec disamb)
  | Variant(e,l) -> Xlist.fold l (i,map,rev_map) (fun (i,map,rev_map) (_,t) -> remove_unused_choices_rec disamb (i,map,rev_map) t)
  | Choice choice -> StringMap.fold choice (i,map,rev_map) (fun (i,map,rev_map) _ t -> remove_unused_choices_rec disamb (i,map,rev_map) t)
  | Dot -> (i,map,rev_map)
  | Val s -> (i,map,rev_map)
  | Node t -> remove_unused_choices_rec disamb (i,map,rev_map) t.args
(*   | Morf m -> (i,map,rev_map) *)
  | Ref r -> 
(*        printf "ref=%d\n%!" r; *)
       if IntMap.mem map r then i,map,rev_map else
       let i,map,rev_map = remove_unused_choices_rec disamb (i,map,rev_map) disamb.(r) in (
(*        printf "add ref %d %d\n%!" r i; *)
       i+1, IntMap.add map r i, IntMap.add rev_map i r)
  | _ -> failwith "remove_unused_choices_rec"

let rec rename_references map = function 
    Tuple l -> Tuple(Xlist.map l (rename_references map))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, rename_references map t))
  | Choice choice -> Choice(StringMap.map choice (fun t -> rename_references map t))
  | Dot -> Dot
  | Val s -> Val s
  | Node t -> Node{t with args=rename_references map t.args}
  | Morf m -> Morf m
  | Ref r -> Ref(IntMap.find map r)
  | _ -> failwith "rename_references"

let rec create_variants_map ei_set = function
    Choice choices -> StringMap.fold choices ei_set (fun ei_set _ t -> create_variants_map ei_set t)
  | Variant(e,l) -> Xlist.fold l ei_set (fun ei_set (i,_) -> StringSet.add ei_set (e ^ i))
  | Dot -> ei_set (* FIXME: dlaczego to występuje?*)
  | t -> failwith ("create_variants_map: " ^ LCGstringOf.linear_term 0 t)
  
let remove_unused_choices disamb =
  let ei_set = Int.fold 0 (Array.length disamb - 1) StringSet.empty (fun ei_set i -> create_variants_map ei_set disamb.(i)) in
  Array.map (function 
      Choice choices -> Choice(StringMap.fold choices StringMap.empty (fun map ei t -> if StringSet.mem ei_set ei then StringMap.add map ei t else map))
    | t -> t) disamb

(***************************************************************************************)

let rec disambiguate_meanings_choice references disamb weights r = function 
    Choice choice -> 
      let choice,sat = StringMap.fold choice (StringMap.empty,StringMap.empty) (fun (choice,sat) ei t ->
        let t,w = disambiguate_meanings_variant references disamb weights t in
        StringMap.add choice ei t, StringMap.add sat ei w) in
      weights.(r) <- sat;
      Choice choice
  | _ -> failwith "disambiguate_meanings_choice"
  
and disambiguate_meanings_variant references disamb weights = function 
    Variant(e,l) -> 
      let l,max_w = Xlist.fold l ([],-.max_float) (fun (l,max_w) (i,t) ->
        let t,w = disambiguate_meanings_rec references disamb weights (e ^ i) t in
        if w = max_w then (i,t) :: l, max_w else
        if w > max_w then [i,t],w else l,max_w) in
    Variant(e, List.rev l),max_w
  | _ -> failwith "disambiguate_meanings_variant"
  
and disambiguate_meanings_rec references disamb weights ei = function 
    Node t -> 
      let args,w = disambiguate_meanings_rec references disamb weights ei t.args in
      Node{t with args=args},w +. t.meaning_weight
  | Tuple l -> 
      let l,sum_w = Xlist.fold l ([],0.) (fun (l,sum_w) t ->
        let t,w = disambiguate_meanings_rec references disamb weights ei t in
        t :: l, sum_w +. w) in
      Tuple(List.rev l), sum_w
  | Variant(e,l) as t -> 
      let l,max_w = Xlist.fold l ([],-.max_float) (fun (l,max_w) (i,t) ->
        let t,w = disambiguate_meanings_rec references disamb weights ei t in
        if w = max_w then (i,t) :: l, max_w else
        if w > max_w then [i,t],w else l,max_w) in
      let l = 
        let map = Xlist.fold l TermSet.empty (fun map (_,t) -> TermSet.add map t) in
        fst (TermSet.fold map ([],1) (fun (l,i) t -> (string_of_int i,t) :: l, i+1)) in
      (match l with
        [] -> failwith ("disambiguate_meanings_rec 1" ^ LCGstringOf.linear_term 0 t)
      | [i,t] -> t,max_w
      | _ -> Variant(e, List.rev l),max_w)
  | Dot -> Dot, 0.
  | Val s -> Val s, 0.
  | Ref i -> 
       if disamb.(i) = Dot then (disamb.(i) <- disambiguate_meanings_choice references disamb weights i references.(i));
       Ref i, (try StringMap.find weights.(i) ei with Not_found -> failwith "disambiguate_meanings_rec 3")
  | t -> failwith ("disambiguate_meanings_rec 2: " ^ LCGstringOf.linear_term 0 t)

let disambiguate_meanings references =
  let disamb = Array.make (Array.length references) Dot in
  let weights = Array.make (Array.length references) StringMap.empty in
  disamb.(0) <- fst (disambiguate_meanings_variant references disamb weights references.(0));
  disamb

  