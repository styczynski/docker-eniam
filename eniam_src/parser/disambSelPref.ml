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

open ENIAMwalTypes
open LCGtypes
open Printf
open Xstd

let fit_node1 t args w =
      let w =
        if t.agf = ADJUNCT || t.agf = CORE || t.agf = NOSEM || t.agf = CLAUSE || t.agf = SENTENCE then w else
(*         if is_nosem_node t then fit_sel_prefs_nosem_node disamb ei t + w else   *)
        if t.position.role = "" && (t.agf = SUBJ || t.agf = OBJ || t.agf = ARG) then w + 20 else
        let b =
          if StringSet.mem t.hipero "0" then true else
          Xlist.fold t.position.sel_prefs false (fun b s -> StringSet.mem t.hipero s || b) in
        (if b then 0 else 1) + w in
      Node{t with args=args},w

let fit_node2 t args w =
      let b = Xlist.fold t.position.sel_prefs false (fun b s -> StringSet.mem t.hipero s || b) in
      let t = {t with args=args} in
      if b then Node t,w else
        (match t.agf, t.position.gf with
          ADJUNCT,_ -> (* FIXME: można dodać tuszowanie braków w walentym *)
            let pos =
(*              let r,a = paths_array.(t.id).PreTypes.lroles in
              if r <> "" then (* FIXME: pomijam to, że role dla rzeczowników dotyczą tylko inst *)
                {t.position with role=r; role_attr=a} else*)
              {t.position with role=t.arole; role_attr=t.arole_attr} in
            Node{t with position=pos}, w+1
        | CLAUSE,NOGF -> Node t,w+0
        | SENTENCE,NOGF -> Node t,w+0
        | ARG,NOGF -> Node t,w+1
        | CORE,NOGF ->
            let pos =  {t.position with role=t.arole; role_attr=t.arole_attr} in
            Node{t with position=pos}, w+0
        | OBJ,NOGF -> Node t,w+0
        | SUBJ,NOGF -> Node t,w+0
        | SUBJ,SUBJ -> Node t,w+2
        | OBJ,OBJ -> Node t,w+2
        | ARG,ARG -> Node t,w+1
        | NOSEM,NOGF -> Node t,w+0
        | NOGF,NOGF -> Node t,w+0
        | NOSEM,NOSEM -> Node t,w+0
(*         | , -> 0  *)
        | a,g ->(* printf "fit_sel_prefs_rec: pred=%s agf=%s pos.gf=%s\n%!" t.pred (WalStringOf.gf a) (WalStringOf.gf g);*) Node t,w+1)

let rec fit_sel_prefs_choice fit_node_fun references disamb satisfaction r = function
    Choice choice ->
      let choice,sat = StringMap.fold choice (StringMap.empty,StringMap.empty) (fun (choice,sat) ei t ->
        let t,w = fit_sel_prefs_variant fit_node_fun references disamb satisfaction t in
        StringMap.add choice ei t, StringMap.add sat ei w) in
      satisfaction.(r) <- sat;
      Choice choice
  | _ -> failwith "fit_sel_prefs_choice"

and fit_sel_prefs_variant fit_node_fun references disamb satisfaction = function
    Variant(e,l) ->
      let l,min_w = Xlist.fold l ([],max_int) (fun (l,min_w) (i,t) ->
        let t,w = fit_sel_prefs_rec fit_node_fun references disamb satisfaction (e ^ i) t in
        if w = min_w then (i,t) :: l, min_w else
        if w < min_w then [i,t],w else l,min_w) in
    Variant(e, List.rev l),min_w
  | _ -> failwith "fit_sel_prefs_variant"

and fit_sel_prefs_rec fit_node_fun references disamb satisfaction ei = function
    Node t ->
      let args,w = fit_sel_prefs_rec fit_node_fun references disamb satisfaction ei t.args in
      fit_node2 t args w
  | Tuple l ->
      let l,sum_w = Xlist.fold l ([],0) (fun (l,sum_w) t ->
        let t,w = fit_sel_prefs_rec fit_node_fun references disamb satisfaction ei t in
        t :: l, sum_w + w) in
      Tuple(List.rev l), sum_w
  | Variant(e,l) as t ->
      let l,min_w = Xlist.fold l ([],max_int) (fun (l,min_w) (i,t) ->
        let t,w = fit_sel_prefs_rec fit_node_fun references disamb satisfaction ei t in
        if w = min_w then (i,t) :: l, min_w else
        if w < min_w then [i,t],w else l,min_w) in
      let l =
        let map = Xlist.fold l TermSet.empty (fun map (_,t) -> TermSet.add map t) in
        fst (TermSet.fold map ([],1) (fun (l,i) t -> (string_of_int i,t) :: l, i+1)) in
      (match l with
        [] -> failwith ("fit_sel_prefs_rec 1" ^ LCGstringOf.linear_term 0 t)
      | [i,t] -> t,min_w
      | _ -> Variant(e, List.rev l),min_w)
  | Dot -> Dot, 0
  | Val s -> Val s, 0
  | Ref i ->
       if disamb.(i) = Dot then (disamb.(i) <- fit_sel_prefs_choice fit_node_fun references disamb satisfaction i references.(i));
       Ref i, (try StringMap.find satisfaction.(i) ei with Not_found -> failwith ("fit_sel_prefs_rec 3: r=" ^ string_of_int i ^ " ei=" ^ ei))
  | t -> failwith ("fit_sel_prefs_rec 2: " ^ LCGstringOf.linear_term 0 t)

let fit_sel_prefs fit_node_fun references =
  let disamb = Array.make (Array.length references) Dot in
  let satisfaction = Array.make (Array.length references) StringMap.empty in
  disamb.(0) <- fst (fit_sel_prefs_variant fit_node_fun references disamb satisfaction references.(0));
  disamb

(***************************************************************************************)
