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

open SemTypes
open Xstd

let rec context_term = function
    CVar(v,w) -> v ^ w
  | COr l -> "or(" ^ String.concat "," (Xlist.map l context_term) ^ ")"
  | CNeg l -> "not " ^ context_term l ^ ")"
  | CEmpty -> "1"
  
let rec type_arg = function 
    TArg s -> "#" ^ s
  | TWith l -> "〈" ^ String.concat "," (Xlist.map l type_arg) ^ "〉"
  
let rec type_term = function 
    TConst(s,[]) -> s
  | TConst(s,args) -> s ^ "(" ^ String.concat "," (Xlist.map args type_arg) ^ ")"
  | TMod(s,t) -> "(" ^ type_term s ^ "," ^ type_term t ^ ")"
  | TName s -> "'" ^ s ^ "'"
  | TVariant(s,t) -> type_term s ^ " & " ^ type_term t

let rec mrl_formula = function
    Conj l -> "(" ^ String.concat "∧" (Xlist.map l mrl_formula) ^ ")"
  | Disj l -> "(" ^ String.concat "∨" (Xlist.map l mrl_formula) ^ ")"
(*  | Impl (f,g) -> "(" ^ (mrl_formula f) ^ "⇒" ^ (mrl_formula g) ^ ")"*)
  | Neg f -> "⌐" ^ (mrl_formula f)
  | Exist((v,sub),t,f) -> "(∃ " ^ v ^ sub ^ ":" ^ mrl_term t ^ ")(" ^ (mrl_formula f) ^ ")"
  | Exist1((v,sub),f) -> "∃(" ^ v ^ sub ^ "," ^ (mrl_formula f) ^ ")"
  | Exist2((v,sub),t,f) -> "∃(" ^ v ^ sub ^ "," ^ (mrl_formula t) ^ "," ^ (mrl_formula f) ^ ")"
  | ForAll((v,sub),t,f) -> "∀(" ^ v ^ sub ^ "," ^ (mrl_formula t) ^ "," ^ (mrl_formula f) ^ ")"
  | Quant1(s,(v,sub),t) -> type_term s ^ "(" ^ v ^ sub ^ "," ^ (mrl_formula t) ^ ")"
  | Quant2(s,(v,sub),t,f) -> type_term s ^ "(" ^ v ^ sub ^ "," ^ (mrl_formula t) ^ "," ^ (mrl_formula f) ^ ")"
  | Dscr(t,f) -> "dscr(" ^ mrl_term t ^ "," ^ mrl_formula f ^ ")"
  | Pred(c,l) -> c ^ "(" ^ String.concat "," (Xlist.map l mrl_term) ^ ")"
  | Equal(s,t) -> (mrl_term s) ^ "=" ^ (mrl_term t)
  | Greater(s,t) -> (mrl_term s) ^ ")" ^ (mrl_term t)
  | True -> "true"
  | Handle h -> h
  | Seam f -> "[" ^ mrl_formula f ^ "]"
  | Cut f -> "|" ^ mrl_formula f
  | Requires(v,f) -> String.concat "," (StringSet.to_list v) ^ ":" ^ mrl_formula f
  | Context c -> context_term c 
  | Position(pos,f) -> "pos(" ^ string_of_int pos ^ "," ^ (mrl_formula f) ^ ")"
  

and mrl_term = function
  | Variable(v,sub) -> v ^ sub
  | List l -> "[" ^ String.concat ";" (Xlist.map l mrl_term) ^ "]"
  | Indexical v -> "#" ^ v
(*   | Int n -> string_of_int n *)
  | String s -> "'" ^ s ^ "'"
(*  | Term(c,[]) -> c*)
  | Term(c,l) -> c ^ "(" ^ String.concat "," (Xlist.map l mrl_term) ^ ")"

let mrs_formula = function   
    MRSeps(h,f) -> h ^ ": " ^ mrl_formula f
  | MRSqeq(h,h2) -> h ^ " =q " ^ h2
  | MRStop h -> h
  | MRSpos(h,pos) -> h ^ ".pos=" ^ string_of_int pos
  
