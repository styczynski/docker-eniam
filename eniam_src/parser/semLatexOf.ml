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
open Printf

let variable (v,sub) =
  if sub = "" then v else v ^ "_{" ^ sub ^ "}"
  
let rec context_term = function
    CVar(v,w) -> v ^ "_{" ^ w ^ "}"
  | COr l -> "(" ^ String.concat "\\vee " (Xlist.map l context_term) ^ ")"
  | CNeg t -> "\\neg " ^ context_term t
  | CEmpty -> ""
  
let rec type_arg = function 
    TArg s -> "\\#" ^ (*"\\text{{\\sc " ^*) s (*^ "}}"*)
  | TWith l -> "\\langle" ^ String.concat "," (Xlist.map l type_arg) ^ "\\rangle"
  
let rec quantification2 = function
    TConst(s,[]) -> "\\text{{\\bf " ^ LatexMain.escape_string s ^ "}}"
  | TConst(s,args) -> "\\text{{\\bf " ^ LatexMain.escape_string s ^ "}}(" ^ String.concat "," (Xlist.map args type_arg) ^ ")"
  | TMod(s,t) -> "(" ^ quantification2 s ^ "," ^ quantification2 t ^ ")"
  | TName _ -> failwith "quantification2"
  | TVariant _ -> failwith "quantification2"

let rec mrl_formula nl = function
    Conj []  -> mrl_formula nl True
  | Conj l -> "(" ^ String.concat "\\wedge\n" (Xlist.map l (mrl_formula nl)) ^ ")"
  | Disj []  -> mrl_formula nl (Neg True)
  | Disj l -> "(" ^ String.concat "\\vee\n" (Xlist.map l (mrl_formula nl)) ^ ")"
(*| Impl (f,g) -> "(" ^ (mrl_formula nl f) ^ "⇒" ^ (mrl_formula nl g) ^ ")"*)
(*   | Neg (Contextx c) -> "\\neg " ^ (mrl_formula nl (Contextx c)) *)
  | Neg f -> "\\neg(" ^ (mrl_formula nl f) ^ ")"
(*   | Exist(v,"T",f) -> "\\\\\n\\exists_{" ^ v ^ "} " ^ (mrl_formula nl f) *)
  | Exist(v,t,f) -> (if nl then "\\\\\n" else "") ^ "\\exists_{" ^ variable v ^ ": " ^ mrl_term t ^ "} " ^ (mrl_formula nl f) 
(*   | ForAll(v,t,f) -> "\\forall_{" ^ v ^ "\\in " ^ t ^ "} " ^ (mrl_formula nl f) *)
(*   | Quant(s,v,"T",f) -> "\\text{{\\it " ^ LatexMain.escape_string s ^ "}}_{" ^ v ^ "} " ^ (mrl_formula nl f) *)
(*   | Quant(s,v,t,f) -> "\\text{{\\it " ^ LatexMain.escape_string s ^ "}}_{" ^ v ^ ": \\text{" ^ LatexMain.escape_string t ^ "}} " ^ (mrl_formula nl f) *)
  | Exist1(v,f) -> (if nl then "\\\\\n" else "") ^ "\\exists(" ^ variable v ^ "," ^ (mrl_formula nl f) ^ ")"
  | Exist2(v,t,f) -> (if nl then "\\\\\n" else "") ^ "\\exists(" ^ variable v ^ "," ^ (mrl_formula nl t) ^ "," ^ (mrl_formula nl f) ^ ")"
  | ForAll(v,t,f) -> (if nl then "\\\\\n" else "") ^ "\\forall(" ^ variable v ^ "," ^ (mrl_formula nl t) ^ "," ^ (mrl_formula nl f) ^ ")"
  | Quant1(s,v,f) -> (if nl then "\\\\\n" else "") ^ quantification2 s ^ "(" ^ variable v ^ "," ^ (mrl_formula nl f) ^ ")"
  | Quant2(s,v,t,f) -> (if nl then "\\\\\n" else "") ^ quantification2 s ^ "(" ^ variable v ^ "," ^ (mrl_formula nl t) ^ "," ^ (mrl_formula nl f) ^ ")"
  | Dscr(t,f) -> "\\text{{\\sc dscr}}(" ^ mrl_term t ^ "," ^ mrl_formula nl f ^ ")"
  | Pred("type",l) -> "\\text{{\\sc type}}(" ^ String.concat "," (Xlist.map l mrl_term) ^ ")"
  | Pred(c,l) -> "\\text{{\\sc " ^ LatexMain.escape_string c ^ "}}(" ^ String.concat "," (Xlist.map l mrl_term) ^ ")"
  | Equal(s,t) -> (mrl_term s) ^ "=" ^ (mrl_term t)
  | Greater(s,t) -> (mrl_term s) ^ ">" ^ (mrl_term t)
  | True -> "true"
  | Handle h -> h
  | Seam f -> (*"{[}" ^*) mrl_formula nl f (*^ "]"*)
  | Requires(v,f) -> (*String.concat "," (StringSet.to_list v) ^ ":" ^*) mrl_formula nl f
  | Cut f -> "|" ^ mrl_formula nl f
  | Context c -> context_term c
  | Position(_,f) -> mrl_formula nl f

and mrl_term = function
  | Variable v -> variable v
  | List l -> "[" ^ String.concat ";" (Xlist.map l mrl_term) ^ "]"
  | Indexical v -> "\\#\\text{" ^ LatexMain.escape_string v ^ "}"
(*   | Int n -> string_of_int n *)
  | String s -> "\\text{„" ^ LatexMain.escape_string s ^ "”}"
  | Term("count",[x]) -> "|" ^ mrl_term x ^ "|"
  | Term(c,[]) -> "\\text{" ^ LatexMain.escape_string c ^ "}"
  | Term(c,l) -> "\\text{" ^ LatexMain.escape_string c ^ "}(" ^ String.concat "," (Xlist.map l mrl_term) ^ ")"
  
let meaning_mrl t =
  "\\begin{flalign*}\n" ^ "\\begin{array}{l}" ^ mrl_formula true t ^ "\\end{array}" ^ "& &\\end{flalign*}\n"    
  
let print_mrls_latex path name query mrls =
  LatexMain.latex_file_out path name "a2" false (fun file ->
    fprintf file "\\section*{%s}\n" query;
    ignore(Xlist.fold mrls 1 (fun n mrl ->
      fprintf file "\\includegraphics[scale=0.5]{tree_%d.png}\\\\\n" n;
      fprintf file "%s" (meaning_mrl mrl);
      n+1)));
  LatexMain.latex_compile_and_clean path name 

