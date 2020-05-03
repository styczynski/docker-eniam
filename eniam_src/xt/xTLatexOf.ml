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
open LatexMain

let rec context_term = function
    CVar(v,w) -> v ^ "_{" ^ w ^ "}"
  | CDef v -> escape_string v
  | COr l -> String.concat "\\vee " (Xlist.map l context_term)
  | CEmpty -> ""
  | CModel m -> BitArray.to_string m

let rec floating_context_term = function
    CVar(v,w) -> [v ^ "_{" ^ w ^ "}"]
  | CDef v -> [escape_string v]
  | COr l -> List.flatten (Xlist.map l floating_context_term)
  | CEmpty -> []
  | CModel m -> [BitArray.to_string m]

type id_selection = All | Selected of StringSet.t

let latex_of_ids selection ids =
  let ids = match selection with
         All -> ids
       | Selected set -> StringMap.fold ids StringMap.empty (fun ids id c ->
           if StringSet.mem set id then StringMap.add ids id c else ids) in
  match StringMap.fold ids [] (fun l id c -> (id ^ " " ^ context_term c) :: l) with
    [] -> ""
  | l -> (String.concat "," l)

let rec lfg_term_arg id_sel = function
    Cons(ids,s) ->
      let x = latex_of_ids id_sel ids in
      "\\text{" ^ (escape_string s) ^ "}" ^ (if x = "" then "" else "{" ^ x ^ "}")
  | _ -> failwith "lfg_term_arg"

let rec lfg_term id_sel = function
    Cons(ids,s) -> LatexFeatureStruct.Val(1,"\\text{" ^ (escape_string s) ^ "}" ^ (latex_of_ids id_sel ids))
  | QCons(ids,s,i,[],[],p,r) -> LatexFeatureStruct.Val(1,"{'{\\text{" ^ (escape_string s) ^ "}_{" ^ i ^ "}}'}" ^ (latex_of_ids id_sel ids) ^ "\\; " ^ string_of_int p ^ "\\text{-}" ^ string_of_int r)
  | QCons(ids,s,i,l,[],p,r) -> LatexFeatureStruct.Val(1,"{'\\text{" ^ (escape_string s) ^ "}_{" ^ i ^ "}\\langle" ^ String.concat "," (Xlist.map l (lfg_term_arg id_sel)) ^ "\\rangle'}" ^ (latex_of_ids id_sel ids) ^ "\\; " ^ string_of_int p ^ "\\text{-}" ^ string_of_int r)
  | QCons(ids,s,i,l,l2,p,r) -> LatexFeatureStruct.Val(1,"{'\\text{" ^ (escape_string s) ^ "}_{" ^ i ^ "}\\langle" ^ String.concat "," (Xlist.map l (lfg_term_arg id_sel)) ^ "\\rangle[" ^ String.concat "," (Xlist.map l2 (lfg_term_arg id_sel)) ^ "]'}" ^ (latex_of_ids id_sel ids) ^ "\\; " ^ string_of_int p ^ "\\text{-}" ^ string_of_int r)
  | LVar s -> LatexFeatureStruct.Val(1,"\\text{{\\sc var}}(" ^ s ^ ")")
  | Compound (ids,l) -> LatexFeatureStruct.Frame(0,LatexFeatureStruct.Bracket,
         Xlist.map l (fun (e,v) -> LatexFeatureStruct.Attr(0,"\\text{" ^ escape_string e ^ "}",lfg_term id_sel v)),latex_of_ids id_sel ids)
  | Set(ids,l) -> LatexFeatureStruct.Frame(0,LatexFeatureStruct.Curly,Xlist.map l (lfg_term id_sel),latex_of_ids id_sel ids)
  | Coordination(ids,l,l2) -> LatexFeatureStruct.Frame(0,LatexFeatureStruct.Bracket,
         LatexFeatureStruct.Frame(0,LatexFeatureStruct.Curly,Xlist.map l (lfg_term id_sel),"") ::
         Xlist.map l2 (fun (e,v) -> LatexFeatureStruct.Attr(0,"\\text{" ^ escape_string e ^ "}",lfg_term id_sel v)),latex_of_ids id_sel ids)
  | Loop(ids,path) ->
           LatexFeatureStruct.Val(1,"\\rightarrow" ^ String.concat ", " (Xlist.map path (function
                "inset" -> "\\in"
              | s -> "\\text{" ^ escape_string s ^ "}")) ^ (latex_of_ids id_sel ids))
  | Context [c,t] -> if c = CEmpty then lfg_term id_sel t else LatexFeatureStruct.FAttr(0,floating_context_term c, lfg_term id_sel t)
  | Context l -> LatexFeatureStruct.Frame(0,LatexFeatureStruct.Mid,
         Xlist.map l (fun (c,t) ->
           if c = CEmpty then lfg_term id_sel t else LatexFeatureStruct.FAttr(0,floating_context_term c, lfg_term id_sel t)),"")

let f_paper_lines = function
    "a4" -> 70
  | "a3" -> 70
  | "a2" -> 70
  | "a1" -> 70
  | "a0" -> 150
  | "b4" -> 70
  | "b3" -> 70
  | "b2" -> 70
  | "b1" -> 70
  | "b0" -> 70
  | _ -> print_endline "Unknown paper type"; 70

let rec context_term_size = function
    CVar(v,w) -> 2 * (String.length v) + 1
  | _ -> failwith "context_term_size"

let rec add_choice_qtree c cl = function
    LatexTree.N(_,s,l) ->
       if s = c then LatexTree.N(0,s,l @ Xlist.map cl (fun x -> LatexTree.L(context_term_size x, context_term x))) else
       LatexTree.N(0,s,add_choice_qtree_list c cl [] l)
  | LatexTree.L(_,s) -> if s = c then LatexTree.N(0,c,Xlist.map cl (fun x -> LatexTree.L (context_term_size x, context_term x))) else raise Not_found
  | LatexTree.C -> failwith "add_choice_qtree"

and add_choice_qtree_list c cl revl = function
    [] -> raise Not_found
  | t :: l ->
      (try
        let t = add_choice_qtree c cl t in
        List.rev revl @ [t] @ l
      with Not_found -> add_choice_qtree_list c cl (t :: revl) l)

let rec create_choice_qtree revl tree = function
    (cl,CVar(v,i)) :: l ->
     (try
       let tree = add_choice_qtree (context_term (CVar(v,i))) cl tree in
       create_choice_qtree revl tree l
     with Not_found -> create_choice_qtree ((cl,CVar(v,i)) :: revl) tree l)
  | (cl,CEmpty) :: l ->
     (try
       let tree = add_choice_qtree "1" cl tree in
       create_choice_qtree revl tree l
     with Not_found -> create_choice_qtree ((cl,CEmpty) :: revl) tree l)
 | (cl,t) :: l -> create_choice_qtree ((cl,t) :: revl) tree l
 | [] -> List.rev revl, tree

let tree_min_row_size  = function
    "a4" -> 70
  | "a3" -> 130
  | "a2" -> 200
  | "a1" -> 300
  | "a0" -> 490
(*  | "b4" -> 100
  | "b3" -> 100
  | "b2" -> 100
  | "b1" -> 100
  | "b0" -> 100*)
  | _ -> print_endline "Unknown paper type"; 70

let tree_max_row_size  = function
    "a4" -> 100
  | "a3" -> 160
  | "a2" -> 240
  | "a1" -> 350
  | "a0" -> 540
(*  | "b4" -> 100
  | "b3" -> 100
  | "b2" -> 100
  | "b1" -> 100
  | "b0" -> 100*)
  | _ -> print_endline "Unknown paper type"; 100

let choices paper_size l =
  if l = [] then "" else
  let l,tree = create_choice_qtree [] (LatexTree.L(0,"1")) l in
  "Choices: \\\\ " ^ LatexTree.render (tree_min_row_size paper_size) (tree_max_row_size paper_size) tree ^
  String.concat ",\\\\ " (Xlist.map l (function
      l,c -> "$[" ^ String.concat ";" (Xlist.map l context_term) ^ "]\\;" ^ context_term c ^ "$"))

let print latex_path latex_filename paper_size sentence choicess tree =
  latex_file_out latex_path latex_filename paper_size false (fun file ->
    Printf.fprintf file "%s\\\\\n" sentence;
    Printf.fprintf file "%s\n" (choices paper_size choicess);
    Printf.fprintf file "$\\;$\\\\F-structure:\\\\\n";
    let tree = lfg_term All tree in
    LatexFeatureStruct.render file (f_paper_lines paper_size) tree);
  latex_compile_and_clean latex_path latex_filename
