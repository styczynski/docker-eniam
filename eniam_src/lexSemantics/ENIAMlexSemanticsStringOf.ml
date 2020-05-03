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

open ENIAMlexSemanticsTypes
open Printf

(*let lex_sem t = "not implemented"

let lex_sems t =
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size t - 1) [] (fun l id ->
    let t2 = ExtArray.get t id in
    (Printf.sprintf "%3d %s" id (lex_sem t2)) :: l)))*)

let arole f =
  let l = Xlist.fold ([f.arole;f.arole_attr;if f.arev then "rev" else "";if f.rev_hipero then "rev_hipero" else ""] @ f.sem_args) [] (fun l -> function "" -> l | s -> s :: l) in
  String.concat "," (List.rev l)

let string_of_lex_sems tokens lex_sems =
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size lex_sems - 1) [] (fun l id ->
    let t = ExtArray.get lex_sems id in
    let t2 = ExtArray.get tokens id in
    let orth = t2.ENIAMtokenizerTypes.orth in
    let lemma = ENIAMtokens.string_of_token t2.ENIAMtokenizerTypes.token in
    let core = Printf.sprintf "%3d %s %s" id orth lemma  in
    let lex_entries = Xlist.map t.lex_entries (fun (selectors,s) ->
        "&[" ^ ENIAMcategoriesPL.string_of_selectors selectors ^ "] " ^ ENIAM_LCGstringOf.grammar_symbol 0 s) in
    let schemata = Xlist.map t.schemata (fun (selectors,cat,(*snode,*)l,l2,l3) ->
        "[" ^ ENIAMcategoriesPL.string_of_selectors selectors ^ "]" ^
        String.concat "," (Xlist.map cat (fun (m,l) -> m ^ "[" ^ String.concat "," l ^ "]")) ^
        (*String.concat "|" snode ^*)
        " {" ^ String.concat "," (Xlist.map l (fun (d,s) ->
            ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 s)) ^ "}" ^
        " {" ^ String.concat "," (Xlist.map l2 (fun (d,s) ->
            ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 s)) ^ "}" ^
        " {" ^ String.concat "," (Xlist.map l3 (fun (d,s) ->
            ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 s)) ^ "}") in
    let frames = Xlist.map t.frames (fun f ->
        "*" ^ arole f ^ "[" ^ ENIAMcategoriesPL.string_of_selectors f.selectors ^ "] {" ^ ENIAMwalStringOf.schema f.positions ^ "} " ^
        String.concat "," (Xlist.map f.senses (fun (sense,hipero,weight) ->
        Printf.sprintf "%s[%s]%.2f" sense (String.concat "," (Xlist.map hipero (fun (s,n) -> s ^ " " ^ string_of_int n))) weight))) in
    (String.concat "\n    " ([core] @ schemata @ frames @ lex_entries)) :: l)))
