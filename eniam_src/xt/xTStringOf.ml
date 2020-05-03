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


let rec context_term = function
    CVar(v,w) -> v ^ w
  | CDef v -> v
  | COr l -> "or(" ^ String.concat "," (Xlist.map l context_term) ^ ")"
  | CEmpty -> "e"
  | CModel m -> BitArray.to_string m

let string_of_ids ids =
  match StringMap.fold ids [] (fun l id c -> (id ^ " " ^ context_term c) :: l) with
    [] -> ""
  | l -> "_" ^ (String.concat "," l)

let rec lfg_term = function
    Cons(ids,s) -> s ^ (string_of_ids ids)
  | QCons(ids,s,i,[],[],p,r) -> "'" ^ s ^ "_" ^ i ^ "'" ^ (string_of_ids ids)
  | QCons(ids,s,i,l,[],p,r) -> "'" ^ s ^ "_" ^ i ^ "<" ^ String.concat "," (Xlist.map l lfg_term) ^ ">'" ^ (string_of_ids ids)
  | QCons(ids,s,i,l,l2,p,r) -> "'" ^ s ^ "_" ^ i ^ "<" ^ String.concat "," (Xlist.map l lfg_term) ^ ">[" ^ String.concat "," (Xlist.map l2 lfg_term) ^ "]'" ^ (string_of_ids ids)
  | LVar s -> s
  | Compound (ids,l) -> "[" ^ String.concat "; " (Xlist.map l (fun (k,v) -> k ^ "," ^ (lfg_term v))) ^ "]" ^ (string_of_ids ids)
  | Set(ids,l) -> "[" ^ (String.concat "; " (Xlist.map l lfg_term)) ^ "]_set" ^ (string_of_ids ids)
  | Coordination(ids,l,l2) -> "[" ^ (String.concat "; " (Xlist.map l lfg_term)) ^ "]_[" ^ (String.concat "; " (Xlist.map l2 (fun (k,v) -> k ^ "," ^ (lfg_term v)))) ^ "]" ^ (string_of_ids ids)
  | Loop(ids,path) -> "Loop path " ^ (String.concat "," path) ^ (string_of_ids ids)
  | Context l -> "[" ^ String.concat ";" (Xlist.map l (fun (c,t) -> context_term c ^ ":" ^ lfg_term t)) ^ "]"
