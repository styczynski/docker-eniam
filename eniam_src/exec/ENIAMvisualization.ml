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

open ENIAM_LCGtypes
open Xstd
open Printf
open ENIAMtokenizerTypes
open ENIAMexecTypes

let string_of_status = function
      Idle -> "Idle"
    | PreprocessingError -> "PreprocessingError"
(*     | NotLemmatized -> "NotLemmatized" *)
    | LexiconError -> "LexiconError"
    | ParseError -> "ParseError"
    | ParseTimeout -> "ParseTimeout"
    | Parsed -> "Parsed"
    | PartialParsed -> "PartialParsed"
    | TooManyNodes -> "TooManyNodes"
    | NotParsed -> "NotParsed"
    | NotReduced -> "NotReduced"
    | ReductionError -> "ReductionError"
    | ReductionError2 -> "ReductionError2"
    | ReductionError3 -> "ReductionError3"
    | SemValenceError -> "SemValenceError"
    | SemGraphError -> "SemGraphError"
    | SemGraphError2 -> "SemGraphError2"
    | SemNotValidated -> "SemNotValidated"
    | SemParsed -> "SemParsed"
    | PartialSemParsed -> "PartialSemParsed"
    | Inferenced -> "Inferenced"
    | InferenceError -> "InferenceError"

(* | NotTranslated -> "NotTranslated" *)

(*
let string_of_interps interps =
      String.concat "|" (Xlist.map interps (fun interp ->
        (String.concat ":" (Xlist.map interp (fun interp2 ->
          (String.concat "." interp2))))))

let lemma_of_token = function
    SmallLetter orth -> orth
  | CapLetter(orth,lc) -> orth
  | AllSmall orth -> orth
  | AllCap(orth,lc,lc2) -> orth
  | FirstCap(orth,lc,cl,ll) -> orth
  | SomeCap orth -> orth
  | RomanDig(v,t) -> v
  | Interp orth -> orth
  | Symbol orth  -> orth
  | Dig(v,t) -> v
  | Other orth  -> orth
  | Lemma(lemma,cat,interps) -> lemma
  | Proper(lemma,cat,interps,senses) -> lemma
  | Compound(sense,l) -> "Compound"
  | Tokens(cat,l) -> "Tokens"

let rec spaces i =
  if i = 0 then "" else "  " ^ spaces (i-1)
(*
let rec string_of_tokens i = function
    Token t -> sprintf "%s{orth=%s;beg=%d;len=%d;next=%d;token=%s;weight=%.2f;attrs=[%s];\n%s senses=[%s];\n%s valence=[%s];\n%s simple_valence=[%s];lroles=%s,%s}" (spaces i) t.orth t.beg t.len t.next (ENIAMtokens.string_of_token t.token)
      t.weight (String.concat ";" t.PreTypes.attrs) (spaces i) (String.concat ";" (Xlist.map t.PreTypes.senses (fun (sense,hipero,weight) -> sprintf "%s[%s]%.2f" sense (String.concat "," hipero) weight)))
      (spaces i) (String.concat ";" (Xlist.map t.PreTypes.valence (ENIAMwalStringOf.fnum_frame ""))) (spaces i) (String.concat ";" (Xlist.map t.PreTypes.simple_valence (ENIAMwalStringOf.fnum_frame ""))) (fst t.lroles) (snd t.lroles)
  | PreTypes.Variant l -> sprintf "%sVariant[\n%s]" (spaces i) (String.concat ";\n" (Xlist.map l (string_of_tokens (i+1))))
  | PreTypes.Seq l -> sprintf "%sSeq[\n%s]" (spaces i) (String.concat ";\n" (Xlist.map l (string_of_tokens (i+1))))

let paths_to_string_indexed (paths,last,next_id) =
  String.concat "\n" (Xlist.map paths (fun (i,j,t) ->
    Printf.sprintf "%2d %2d %s" i j (string_of_tokens 0 (PreTypes.Token t))))
  ^ Printf.sprintf "\nlast=%d next_id=%d" last next_id
*)
(*let string_of_token_record1 t =
  sprintf "{orth=%s;beg=%d;len=%d;next=%d;token=%s;id=%d;lnode=%d;rnode=%d;conll_id=%s;conll_super=%s;conll_label=%s;attrs=[%s]}"
    t.PreTypes.orth t.PreTypes.beg t.PreTypes.len t.PreTypes.next (ENIAMtokens.string_of_token t.PreTypes.token)
    t.PreTypes.id t.PreTypes.lnode t.PreTypes.rnode t.PreTypes.conll_id t.PreTypes.conll_super t.PreTypes.conll_label
    (String.concat ";" t.PreTypes.attrs)
  (* sprintf "{orth=%s;beg=%d;len=%d;next=%d;token=%s}"
    t.PreTypes.orth t.PreTypes.beg t.PreTypes.len t.PreTypes.next (ENIAMtokens.string_of_token t.PreTypes.token)  *)

let string_of_paths1 paths =
  String.concat "\n            " (Xlist.map paths string_of_token_record1)*)

let string_of_status = function
    ExecTypes.Idle -> "idle"
  | ExecTypes.PreprocessingError -> "error_pre"
  | ExecTypes.LexiconError -> "error_lex"
  | ExecTypes.ParseError -> "error_parse"
  | ExecTypes.ParseTimeout -> "timeout"
  | ExecTypes.NotParsed -> "not_parsed"
  | ExecTypes.ReductionError -> "error_reduction"
  | ExecTypes.TooManyNodes -> "too_many_nodes"
  | ExecTypes.NotReduced -> "not_reduced"
  | ExecTypes.SemError -> "error_sem"
  | ExecTypes.NotTranslated -> "not_translated"
  | ExecTypes.Parsed -> "parsed"

let rec xml_of_dependency_tree = function
    Node t -> Xml.Element("node",["pred",t.pred;"cat",t.cat;"weight",string_of_float t.weight;"id",string_of_int t.id],[
          Xml.Element("gs",[],[xml_of_dependency_tree t.gs]);
          Xml.Element("agf",[],[Xml.PCData (ENIAMwalStringOf.gf t.agf)]);
          Xml.Element("amorf",[],[Xml.PCData (ENIAMwalStringOf.morf t.amorf)]);
          Xml.Element("attrs",[],Xlist.map t.attrs (fun (e,t) -> Xml.Element("attr",["label",e],[xml_of_dependency_tree t])));
          Xml.Element("args",[],[xml_of_dependency_tree t.args])])
  | Concept c -> Xml.Element("concept",["var",fst c.c_variable ^ snd c.c_variable;"pos",string_of_int c.c_pos],[
      Xml.Element("sense",[],[xml_of_dependency_tree c.c_sense]);
      Xml.Element("quant",["local",if c.c_local_quant then "t" else "f"],[xml_of_dependency_tree c.c_quant]);
      Xml.Element("relations",[],[xml_of_dependency_tree c.c_relations])])
  | Context c -> Xml.Element("context",["var",fst c.cx_variable ^ snd c.cx_variable;"pos",string_of_int c.cx_pos],[
      Xml.Element("sense",[],[xml_of_dependency_tree c.cx_sense]);
      Xml.Element("contents",[],[xml_of_dependency_tree c.cx_contents]);
      Xml.Element("relations",[],[xml_of_dependency_tree c.cx_relations])])
  | Relation(r,a,t) -> Xml.Element("relation",[],[
      Xml.Element("role",[],[xml_of_dependency_tree r]);
      Xml.Element("role_attr",[],[xml_of_dependency_tree r]);
      xml_of_dependency_tree t])
  | RevRelation(r,a,t) -> Xml.Element("revrelation",[],[
      Xml.Element("role",[],[xml_of_dependency_tree r]);
      Xml.Element("role_attr",[],[xml_of_dependency_tree r]);
      xml_of_dependency_tree t])
  | SingleRelation(r) -> Xml.Element("singlerelation",[],[xml_of_dependency_tree r])
  | Tuple l -> Xml.Element("tuple",[],Xlist.map l xml_of_dependency_tree)
  | Val s -> Xml.Element("val",[],[Xml.PCData s])
  | Variant(e,l) -> Xml.Element("variants",["label",e],Xlist.map l (fun (i,t) ->
          Xml.Element("variant",["id",i],[xml_of_dependency_tree t])))
  | Dot -> Xml.Element("dot",[],[])
  | Ref i -> Xml.Element("ref",["id",string_of_int i],[])
  | Morf _ -> Xml.Element("dot",[],[]) (* FIXME!!! *)
  | t -> failwith ("xml_of_dependency_tree: " ^ LCGstringOf.linear_term 0 t)

let print_xml_dependency_tree path name dependency_tree =
  let l = Int.fold 0 (Array.length dependency_tree - 1) [] (fun l i ->
    (i, xml_of_dependency_tree dependency_tree.(i)) :: l) in
  let xml = Xml.Element("graph",[],Xlist.rev_map l (fun (i,xml) ->
    Xml.Element("graph_node",["id",string_of_int i],[xml]))) in
  File.file_out (path ^ name ^ ".xml") (fun file ->
    fprintf file "%s\n" (Xml.to_string_fmt xml))

let print_xml_tree path name tree =
  let xml = xml_of_dependency_tree tree in
  File.file_out (path ^ name ^ ".xml") (fun file ->
    fprintf file "%s\n" (Xml.to_string_fmt xml))



let rec get_refs rev = function
    Ref i -> i :: rev
  | Tuple l -> Xlist.fold l rev get_refs
  | Variant(e,l) -> Xlist.fold l rev (fun rev (i,t) -> get_refs rev t)
  | Dot -> rev
  | _ -> (*failwith*)print_endline "get_refs"; rev

let escape_string s =
  Int.fold 0 (String.length s - 1) "" (fun t i ->
    match String.sub s i 1 with
       "<" -> t ^ "〈"
     | ">" -> t ^ "〉"
     | c -> t ^ c)

let string_of_node t =
  let l = [
    "PRED",Val t.pred;"CAT",Val t.cat;"ID",Val (string_of_int t.id);"WEIGHT",Val (string_of_float t.weight);"GS",t.gs;
    "AGF",Gf t.agf;"AMORF",Morf t.amorf;"AROLE",Val t.arole;"AROLE-ATTR",Val t.arole_attr;
    "MEANING",Val t.meaning;"HIPERO",Tuple(Xlist.map (StringSet.to_list t.hipero) (fun s -> Val s));"MEANING-WEIGHT",Val (string_of_float t.meaning_weight);
    "ROLE",Val t.position.ENIAMwalTypes.role;"ROLE-ATTR",Val t.position.ENIAMwalTypes.role_attr;"SEL-PREFS",Tuple(Xlist.map t.position.ENIAMwalTypes.sel_prefs (fun s -> Val s));
    "GF",Gf t.position.ENIAMwalTypes.gf] @ t.attrs in
  "{ " ^ String.concat " | " (Xlist.map l (fun (e,t) -> "{ " ^ e ^ " | " ^ escape_string (LCGstringOf.linear_term 0 t) ^ " }")) ^ " }"

let single_rel_id_count = ref 0

let get_single_rel_id () =
  let id = !single_rel_id_count in
  incr single_rel_id_count;
  "s" ^ string_of_int id

let print_edge file label upper id =
  if upper <> "" then
    if label = "" then fprintf file "  %s -> %s\n" upper id
    else fprintf file "  %s -> %s  [label=\"%s\"]\n" upper id label

(*let rec print_graph_rec2 file edge upper = function
    Tuple l -> Xlist.iter l (print_graph_rec2 file edge upper)
  | Node t ->
          let id = get_single_rel_id () in
          fprintf file "  %s [label=\"%s\"]\n" id (string_of_node t);
          print_edge file edge upper id;
          print_graph_rec2 file "" id t.args
  | Concept t ->
          let id = get_single_rel_id () in
          fprintf file "  %s [shape=box,label=\"%s %s\"]\n" id
            (LCGchart.string_of_linear_term 0 t.c_sense)
            (if t.c_name=Dot then "" else "„" ^ LCGchart.string_of_linear_term 0 t.c_name ^ "”"); (* FIXME *)
          print_edge file edge upper id;
          print_graph_rec2 file "" id t.c_relations
  | SingleRelation(role) ->
          let id = get_single_rel_id () in
          fprintf file "  %s [shape=circle,label=\"%s\"]\n" id (LCGchart.string_of_linear_term 0 role);
          if upper <> "" then fprintf file "  %s -> %s\n" upper id
  | Variant(e,l) ->
          fprintf file "  %s [shape=diamond]\n" e;
          print_edge file edge upper e;
          Xlist.iter l (fun (i,t) -> print_graph_rec2 file i e t)
  | Dot -> ()
  | Ref i -> print_edge file edge upper ("x" ^ string_of_int i)
  | t -> failwith ("print_graph_rec2: " ^ LCGchart.string_of_linear_term 0 t)    *)

let rec string_of_quant_rec quant = function
    Tuple l -> Xlist.fold l quant string_of_quant_rec
  | Variant(e,l) -> (LCGstringOf.linear_term 0 (Variant(e,l))) :: quant
  | Dot -> quant
  | Val s -> s :: quant
  | _ -> failwith "string_of_quant_rec"

let string_of_quant t =
  let l = string_of_quant_rec [] t in
  let s = String.concat " " l in
  if s = "" then "" else "<I>" ^ s ^ "</I> "

let rec print_dependency_tree_rec file edge upper id = function
    Node t ->
          fprintf file "  %s [label=\"%s\"]\n" id (string_of_node t);
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t.args
  | Concept t ->
          fprintf file "  %s [shape=box,label=<%s%s %s>]\n" id
            (string_of_quant t.c_quant)
            (LCGstringOf.linear_term 0 t.c_sense)
            (if t.c_name=Dot then "" else "„" ^ LCGstringOf.linear_term 0 t.c_name ^ "”"); (* FIXME *)
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t.c_relations
  | Context t ->
          if t.cx_sense = Dot then fprintf file "  %s [shape=Msquare,label=\"\"]\n" id
          else fprintf file "  %s [shape=Msquare,label=\"%s\"]\n" id (LCGstringOf.linear_term 0 t.cx_sense);
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t.cx_contents;
          print_dependency_tree_rec2 file "" id t.cx_relations;
  | Relation(role,role_attr,t) ->
          fprintf file "  %s [shape=circle,label=\"%s\\n%s\"]\n" id (LCGstringOf.linear_term 0 role) (LCGstringOf.linear_term 0 role_attr);
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t
  | RevRelation(role,role_attr,t) -> (* FIXME: odwrócenie strzałek *)
          fprintf file "  %s [shape=circle,label=\"%s\\n%s\"]\n" id (LCGstringOf.linear_term 0 role) (LCGstringOf.linear_term 0 role_attr);
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t
  | SingleRelation(role) ->
          fprintf file "  %s [shape=circle,label=\"%s\"]\n" id (LCGstringOf.linear_term 0 role);
          print_edge file edge upper id
  | AddRelation(t,role,role_attr,s) ->
          fprintf file "  %s [shape=circle,label=\"AddRelation\\n%s\\n%s\"]\n" id role role_attr;
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t;
          print_dependency_tree_rec2 file "" id s;
  | SetContextName(s,t) ->
          fprintf file "  %s [shape=circle,label=\"SetContextName\\n%s\"]\n" id s;
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t
  | RemoveRelation t ->
          fprintf file "  %s [shape=circle,label=\"RemoveRelation\"]\n" id;
          print_edge file edge upper id;
          print_dependency_tree_rec2 file "" id t
  | Variant(e,l) ->
          fprintf file "  %s [shape=diamond,label=\"%s\"]\n" id e;
          print_edge file edge upper id;
          Xlist.iter l (fun (i,t) -> print_dependency_tree_rec2 file i id t)
  | Choice choice ->
          fprintf file "  %s [shape=Mdiamond,label=\"%s\"]\n" id "";
          print_edge file edge upper id;
          StringMap.iter choice (fun ei t -> print_dependency_tree_rec2 file ei id t)
  | Val s ->
          fprintf file "  %s [shape=box,label=\"%s\"]\n" id s;
          print_edge file edge upper id
  | Dot -> ()
(*          fprintf file "  %s [shape=box,label=\"Dot\"]\n" id;
          print_edge file edge upper id*)
  | Ref i -> print_edge file edge upper ("x" ^ string_of_int i)
  | t -> failwith ("print_dependency_tree_rec: " ^ LCGstringOf.linear_term 0 t)

and print_dependency_tree_rec2 file edge upper = function
    Tuple l -> Xlist.iter l (print_dependency_tree_rec2 file edge upper)
  | t -> print_dependency_tree_rec file edge upper (get_single_rel_id ()) t

(*let rec print_graph_rec file is_rev upper i = function  (* FIXME: dokończyć is_rev *)
    Node t ->
 (*         let orth = if t.id = 0 then "" else.(t.id).PreTypes.orth in
          fprintf file "  %s [label=\"%s\\n%s\\n%s:%s\"]\n" i (LCGstringOf.linear_term 0 t.gs) orth t.pred t.cat;*)
          fprintf file "  %s [label=\"%s\"]\n" i (string_of_node t);
          if upper <> "" then
            if is_rev then fprintf file "  %s -> %s\n" i upper
            else fprintf file "  %s -> %s\n" upper i;
          print_graph_rec file false i i t.args
  | Concept t ->
          fprintf file "  %s [shape=box,label=\"%s %s\"]\n" ("c" ^ i)
            (LCGstringOf.linear_term 0 t.c_sense)
            (if t.c_name=Dot then "" else "„" ^ LCGstringOf.linear_term 0 t.c_name ^ "”"); (* FIXME *)
          if upper <> "" then
            if is_rev then fprintf file "  %s -> %s\n" ("c" ^ i) upper
            else fprintf file "  %s -> %s\n" upper ("c" ^ i);
          print_graph_rec file false ("c" ^ i) i t.c_relations
  | Context t ->
          fprintf file "  %s [shape=Msquare,label=\"\"]\n" ("i" ^ i);
          if upper <> "" then
            if is_rev then fprintf file "  %s -> %s\n" ("i" ^ i) upper
            else fprintf file "  %s -> %s\n" upper ("i" ^ i);
          print_graph_rec file false ("i" ^ i) i t.cx_contents
  | SingleRelation(role) ->
          let id = get_single_rel_id () in
          fprintf file "  %s [shape=circle,label=\"%s\"]\n" id (LCGstringOf.linear_term 0 role);
          if upper <> "" then fprintf file "  %s -> %s\n" upper id
  | Relation(role,role_attr,t) ->
          fprintf file "  %s [shape=circle,label=\"%s\\n%s\"]\n" i (LCGstringOf.linear_term 0 role) (LCGstringOf.linear_term 0 role_attr);
          if upper <> "" then fprintf file "  %s -> %s\n" upper i;
          print_graph_rec file false i i t
  | RevRelation(role,role_attr,t) ->
          fprintf file "  %s [shape=circle,label=\"%s\\n%s\"]\n" i (LCGstringOf.linear_term 0 role) (LCGstringOf.linear_term 0 role_attr);
          if upper <> "" then fprintf file "  %s -> %s\n" i upper;
          print_graph_rec file true i i t
  | Tuple l -> Xlist.iter l (print_graph_rec file is_rev upper i)
  | Variant(e,l) ->
          fprintf file "  %s [shape=diamond]\n" e;
          if upper <> "" then fprintf file "  %s -> %s\n" upper e;
          Xlist.iter l (fun (i2,t) -> print_graph_rec file false e ("x" ^ i ^ "y" ^ i2) t)
  | Dot -> ()
  | Ref i2 -> fprintf file "  %s -> %d\n" upper i2
  | t -> failwith ("print_graph_rec: " ^ LCGstringOf.linear_term 0 t)*)

let print_dependency_tree path name dependency_tree =
  single_rel_id_count := 0;
  File.file_out (path ^ name ^ ".gv") (fun file ->
    fprintf file "digraph G {\n  node [shape=record]\n";
    Int.iter 0 (Array.length dependency_tree - 1) (fun i -> print_dependency_tree_rec file (*false*) "" "" ("x" ^ string_of_int i) dependency_tree.(i));
(*    Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
      match dependency_tree.(i) with
        Node t ->
          fprintf file "  %d [label=\"%s\"]\n" i (string_of_node t);
          let refs = get_refs [] t.args in
          Xlist.iter refs (fun r ->
            fprintf file "  %d -> %d\n" i r)
      | t -> failwith ("print_dependency_tree: " ^ LCGstringOf.linear_term 0 t));*)
    fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path

let id_counter = ref 0

let print_edge2 file edge_rev edge_label edge_head edge_tail upper id =
  let edge_head,edge_tail,upper,id = if edge_rev then edge_tail,edge_head,id,upper else edge_head,edge_tail,upper,id in
  let l =
    (if edge_label = "" then [] else ["label=\"" ^ edge_label ^ "\""]) @
    (if edge_head = "" then [] else ["ltail=\"" ^ edge_head ^ "\""]) @
    (if edge_tail = "" then [] else ["lhead=\"" ^ edge_tail ^ "\""]) in
  if upper <> 0 then
    if l = [] then fprintf file "  %d -> %d\n" upper id
    else fprintf file "  %d -> %d  [%s]\n" upper id (String.concat "," l)

let rec print_graph2_rec file edge_rev edge_label edge_head upper = function
    Node t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [label=\"%s\"]\n" id (string_of_node t);
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t.args
  | Concept t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=<%s%s %s>]\n" id
            (string_of_quant t.c_quant)
            (escape_string (LCGstringOf.linear_term 0 t.c_sense))
            (if t.c_name=Dot then "" else "„" ^ LCGstringOf.linear_term 0 t.c_name ^ "”"); (* FIXME *)
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t.c_relations
  | Context t ->
          let id = !id_counter in
          incr id_counter;
          if t.cx_sense = Dot then fprintf file "  subgraph cluster%d {\nlabel=\"\"\n" id
          else fprintf file "  subgraph cluster%d {\nlabel=\"%s\"\n" id (LCGstringOf.linear_term 0 t.cx_sense);
          print_graph2_rec file false "" "" 0 t.cx_contents;
          fprintf file "  }\n";
          print_edge2 file edge_rev edge_label edge_head ("cluster" ^ string_of_int id) upper (id+1);
          print_graph2_rec file false "" ("cluster" ^ string_of_int id) (id+1) t.cx_relations;
  | Relation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id (LCGstringOf.linear_term 0 role) (LCGstringOf.linear_term 0 role_attr);
          print_edge2 file false edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t
  | RevRelation(role,role_attr,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\\n%s\"]\n" id (LCGstringOf.linear_term 0 role) (LCGstringOf.linear_term 0 role_attr);
          print_edge2 file true edge_label edge_head "" upper id;
          print_graph2_rec file true "" "" id t
  | SingleRelation(role) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"%s\"]\n" id (LCGstringOf.linear_term 0 role);
          print_edge2 file false edge_label edge_head "" upper id
  | AddRelation(t,role,role_attr,s) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"AddRelation\\n%s\\n%s\"]\n" id role role_attr;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t;
          print_graph2_rec file false "" "" id s
  | RemoveRelation t ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"RemoveRelation\"]\n" id;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t
  | SetContextName(s,t) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=circle,label=\"SetContextName\\n%s\"]\n" id s;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          print_graph2_rec file false "" "" id t;
  | Tuple l -> Xlist.iter l (print_graph2_rec file edge_rev edge_label edge_head upper)
  | Variant(e,l) ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=diamond,label=\"%s\"]\n" id e;
          print_edge2 file edge_rev edge_label edge_head "" upper id;
          Xlist.iter l (fun (i,t) -> print_graph2_rec file edge_rev  i "" id t)
  | Val s ->
          let id = !id_counter in
          incr id_counter;
          fprintf file "  %d [shape=box,label=\"%s\"]\n" id s;
          print_edge2 file edge_rev edge_label edge_head "" upper id
  | Dot -> ()
  | t -> failwith ("print_graph_rec: " ^ LCGstringOf.linear_term 0 t)

let print_graph2 path name query t =
(*   print_endline  *)
  id_counter := 1;
  File.file_out (path ^ name ^ ".gv") (fun file ->
    fprintf file "digraph G {\n  compound=true\n  node [shape=record]\n";
    print_graph2_rec file false "" "" 0 t;
    fprintf file "label=\"%s\"\n  }\n" query);
(*   Sys.chdir path; *)
  ignore (Sys.command ("dot -Tpng " ^ path ^ name ^ ".gv -o " ^ path ^ name ^ ".png"))(*;
  Xlist.iter (Str.split (Str.regexp path) path) (fun _ -> Sys.chdir "..")*)

let rec get_lemma = function
    ENIAMtokenizerTypes.Interp orth -> orth
  | ENIAMtokenizerTypes.Lemma(lemma,cat,_) -> lemma ^ "\n" ^ cat
  | _ -> ""

let print_paths path name paths =
  File.file_out (path ^ name ^ ".gv") (fun file ->
    fprintf file "digraph G {\n";
    Array.iter (fun t ->
      let lemma = get_lemma t.ENIAMtokenizerTypes.token in
      if lemma <> "" then fprintf file "  %d -> %d [label=\"%s\\n%s\"]\n" t.ENIAMtokenizerTypes.beg t.ENIAMtokenizerTypes.next t.ENIAMtokenizerTypes.orth lemma) paths;
    fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path

let rec print_simplified_dependency_tree_rec2 file tokens edge upper = function
    Tuple l -> Xlist.iter l (print_simplified_dependency_tree_rec2 file tokens edge upper)
  | Variant(e,l) ->
          fprintf file "  %s [shape=diamond]\n" e;
          print_edge file edge upper e;
          Xlist.iter l (fun (i,t) -> print_simplified_dependency_tree_rec2 file tokens i e t)
  | Dot -> ()
  | Ref i -> print_edge file edge upper ("x" ^ string_of_int i)
  | t -> failwith ("print_simplified_dependency_tree_rec: " ^ LCGstringOf.linear_term 0 t)

let rec print_simplified_dependency_tree_rec file tokens edge upper id = function
    Node t ->
          let orth = if t.id = 0 then "" else (ExtArray.get tokens t.id).ENIAMtokenizerTypes.orth in
          fprintf file "  %s [label=\"%s\\n%s\\n%s:%s\\n%f\"]\n" id (LCGstringOf.linear_term 0 t.gs) orth t.pred t.cat t.weight;
          print_edge file edge upper id;
          print_simplified_dependency_tree_rec2 file tokens "" id t.args
  | Variant(e,l) ->
          fprintf file "  %s [shape=diamond,label=\"%s\"]\n" id e;
          print_edge file edge upper id;
          Xlist.iter l (fun (i,t) -> print_simplified_dependency_tree_rec file tokens i id (id ^ "y" ^ i) t)
  | Choice choice ->
          fprintf file "  %s [shape=Mdiamond,label=\"%s\"]\n" id "";
          print_edge file edge upper id;
          StringMap.iter choice (fun ei t -> print_simplified_dependency_tree_rec file tokens ei id (id ^ "b" ^ ei) t)
  | Dot -> ()
  | t -> failwith ("print_simplified_dependency_tree_rec: " ^ LCGstringOf.linear_term 0 t)

let print_simplified_dependency_tree path name tokens dependency_tree =
  File.file_out (path ^ name ^ ".gv") (fun file ->
    fprintf file "digraph G {\n  node [shape=box]\n";
    Int.iter 0 (Array.length dependency_tree - 1) (fun i -> print_simplified_dependency_tree_rec file tokens "" "" ("x" ^ string_of_int i) dependency_tree.(i));
(*      match dependency_tree.(i) with
        Node t ->
          let orth = if t.id = 0 then "" else tokens.(t.id).ENIAMtokenizerTypes.orth in
          fprintf file "  %d [label=\"%s\\n%s\\n%s:%s\"]\n" i (LCGstringOf.linear_term 0 t.gs) orth t.pred t.cat;
          let refs = get_refs [] t.args in
          Xlist.iter refs (fun r ->
            fprintf file "  %d -> %d\n" i r)
      | t -> failwith ("print_simplified_dependency_tree: " ^ LCGstringOf.linear_term 0 t));*)
    fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path

(*let print_tree filename paths dependency_tree =
  File.file_out filename (fun file ->
    fprintf file "digraph G {\n";
    let set = Xlist.fold paths IntSet.empty (fun set t ->
      IntSet.add (IntSet.add set t.ENIAMtokenizerTypes.beg) t.ENIAMtokenizerTypes.next) in
    IntSet.iter set (fun i -> fprintf file "  %d [width=0; height=0; label=\"\"]\n" i);
    Xlist.iter paths (fun t ->
      let lemma = get_lemma t.ENIAMtokenizerTypes.token in
      if lemma <> "" then (
        let s = if t.ENIAMtokenizerTypes.orth = "" then lemma else t.ENIAMtokenizerTypes.orth ^ "\n" ^ lemma in
        fprintf file "  %d -> i%d -> %d [arrowhead=none]\n" t.ENIAMtokenizerTypes.beg t.ENIAMtokenizerTypes.id t.ENIAMtokenizerTypes.next;
        fprintf file "  i%d [label=\"%s\"]\n" t.ENIAMtokenizerTypes.id s));
    fprintf file "}\n");
  Sys.chdir "results";
  ignore (Sys.command "dot -Tpng tree.gv -o tree.png");
  String.iter (function '/' -> Sys.chdir ".." | _ -> ()) path*)

(*let print_tree filename paths dependency_tree =
  File.file_out filename (fun file ->
    fprintf file "digraph G {\n";
      fprintf file "  subgraph {\n  ordering=out\n";
      let same = Xlist.fold (Xlist.sort paths (fun s t -> compare s.ENIAMtokenizerTypes.beg t.ENIAMtokenizerTypes.beg)) [] (fun same t ->
        let lemma = get_lemma t.ENIAMtokenizerTypes.token in
        if lemma <> "" then (
          let s = if t.ENIAMtokenizerTypes.orth = "" then lemma else t.ENIAMtokenizerTypes.orth ^ "\n" ^ lemma in
          fprintf file "    i%d -> out [arrowhead=none]\n" t.ENIAMtokenizerTypes.id;
          fprintf file "    i%d [label=\"%s\"]\n" t.ENIAMtokenizerTypes.id s;
          t.ENIAMtokenizerTypes.id :: same)
        else same) in
      fprintf file "  }\n";
      fprintf file "  { rank = same; %s }\n" (String.concat "; " (Xlist.map same (fun i -> sprintf "\"i%d\"" i)));
    Int.iter 0 (Array.length dependency_tree - 1) (fun i ->
      match dependency_tree.(i) with
        Node t ->
          fprintf file "  %d [label=\"%s\"]\n" i t.pred;
          fprintf file "  %d -> i%d\n" i t.id;
          let refs = get_refs [] t.args in
          Xlist.iter refs (fun r ->
            fprintf file "  %d -> %d\n" i r)
      | _ -> failwith "print_graph");
    fprintf file "}\n");
  Sys.chdir "results";
  ignore (Sys.command "dot -Tpng tree.gv -o tree.png");
  Sys.chdir ".."*)

let rec schema_latex schema =
  "\\begin{tabular}{l}" ^
  String.concat "\\\\" (Xlist.map schema (fun s ->
    LatexMain.escape_string (String.concat "," (
      (if s.ENIAMwalTypes.gf = ENIAMwalTypes.ARG then [] else [ENIAMwalStringOf.gf s.ENIAMwalTypes.gf])@
      (if s.ENIAMwalTypes.role = "" then [] else [s.ENIAMwalTypes.role])@
      (if s.ENIAMwalTypes.role_attr = "" then [] else [s.ENIAMwalTypes.role_attr])@
      s.ENIAMwalTypes.sel_prefs@(ENIAMwalStringOf.controllers s.ENIAMwalTypes.cr)@(ENIAMwalStringOf.controllees s.ENIAMwalTypes.ce)) ^ ENIAMwalStringOf.direction s.ENIAMwalTypes.dir ^ "{" ^  String.concat ";" (Xlist.map s.ENIAMwalTypes.morfs ENIAMwalStringOf.morf) ^ "}"))) ^
  "\\end{tabular}"

let fnum_frame_latex = function
    fnum,ENIAMwalTypes.Frame(atrs,s) ->
      Printf.sprintf "%d: %s: %s" fnum (LatexMain.escape_string (ENIAMwalStringOf.frame_atrs atrs)) (schema_latex s)
  | fnum,ENIAMwalTypes.LexFrame(id,p,r,s) ->
      Printf.sprintf "%d: %s: %s: %s: %s" fnum id (LatexMain.escape_string (ENIAMwalStringOf.pos p)) (ENIAMwalStringOf.restr r) (schema_latex s)
  | fnum,ENIAMwalTypes.ComprepFrame(le,p,r,s) ->
      Printf.sprintf "%d: %s: %s: %s: %s" fnum le (LatexMain.escape_string (ENIAMwalStringOf.pos p)) (ENIAMwalStringOf.restr r) (schema_latex s)

(*let print_paths_latex name paths =
  LatexMain.latex_file_out "results/" name "a0" false (fun file ->
    fprintf file "\\begin{longtable}{|l|l|l|l|l|l|l|p{4cm}|l|l|l|l|}\n\\hline\north & beg & len & next & token & id & weight & attrs & lroles & senses & simple valence & valence\\\\\n";
    Int.iter 0 (Array.length paths - 1) (fun i ->
      let t = paths.(i) in
      fprintf file "%s & %d & %d & %d & %s & %d & %.4f & %s & %s %s &\\begin{tabular}{l|l|p{4cm}}%s\\end{tabular} &\\begin{tabular}{l}%s\\end{tabular} &\\begin{tabular}{l}%s\\end{tabular}\\\\\n\\hline\n"
        t.ENIAMtokenizerTypes.orth t.ENIAMtokenizerTypes.beg t.ENIAMtokenizerTypes.len t.ENIAMtokenizerTypes.next (LatexMain.escape_string (ENIAMtokens.string_of_token t.ENIAMtokenizerTypes.token)) t.ENIAMtokenizerTypes.id t.ENIAMtokenizerTypes.weight
        (String.concat ";" t.ENIAMtokenizerTypes.attrs) (fst t.ENIAMtokenizerTypes.lroles) (snd t.ENIAMtokenizerTypes.lroles)
        (String.concat "\\\\\n" (Xlist.map t.ENIAMtokenizerTypes.senses (fun (sense,hipero,weight) -> sprintf "%s & %.2f & %s" sense weight (String.concat "," hipero))))
        (String.concat "\\\\\n\\hline\n" (Xlist.map t.ENIAMtokenizerTypes.simple_valence (fun x -> fnum_frame_latex x)))
        (String.concat "\\\\\n\\hline\n" (Xlist.map t.ENIAMtokenizerTypes.valence (fun x -> fnum_frame_latex x))));
    fprintf file "\\end{longtable}");
  LatexMain.latex_compile_and_clean "results/" name*)

let print_mml path name mml =
  File.file_out (path ^ name ^ ".mml") (fun file ->
    fprintf file "<!DOCTYPE math PUBLIC \"-//W3C//DTD MathML 2.0//EN\" \"http://www.w3.org/Math/DTD/mathml2/mathml2.dtd\">\n";
    fprintf file "%s\n" (Xml.to_string_fmt mml))
*)

let page_header path =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
        <META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
        <TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
        <META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let page_trailer =
"</center>
  </body>
</html>"
(*
let print_webpage file cg_bin_path html_path id query n max_n mml =
  fprintf file "%s\n" (page_header cg_bin_path);
  fprintf file "\n<H3>%s</H3>\n" query;
  fprintf file "<P>%s %s\n"
    (if n = 1 then "" else sprintf "<A HREF=\"%spage%s_%d.html\">Poprzednia interpretacja</A>" html_path id (n-1))
    (if n = max_n then "" else sprintf "<A HREF=\"%spage%s_%d.html\">Następna interpretacja</A>" html_path id (n+1));
  fprintf file "<P><IMG SRC=\"%stree%s_%d.png\">\n" html_path id n;
  fprintf file "<P>%s\n" (Xml.to_string_fmt mml);
  fprintf file "<P><A HREF=\"%stree%s_%d.xml\">Graf w formacie XML</A>\n" html_path id n;
  fprintf file "<P><A HREF=\"%sformula%s_%d.mml\">Formuła w formacie MathML</A>\n" html_path id n;
  fprintf file "<P>%s %s\n"
    (if n = 1 then "" else sprintf "<A HREF=\"%spage%s_%d.html\">Poprzednia interpretacja</A>" html_path id (n-1))
    (if n = max_n then "" else sprintf "<A HREF=\"%spage%s_%d.html\">Następna interpretacja</A>" html_path id (n+1));
  fprintf file "%s\n" page_trailer


let generate_status_message result = function
    Idle -> "Server error: " ^ result.msg
  | PreprocessingError -> "Error during preprocessing: " ^ result.msg
  | LexiconError -> "Error during LCG lexicon generation: " ^ result.msg
  | ParseError -> "Error during parsing: " ^ result.msg
  | ParseTimeout -> "Parser timeout: " ^ result.msg
  | NotParsed -> "Unable to parse query"
  | ReductionError -> "Error during dependency tree generation: " ^ result.msg
  | TooManyNodes -> "Depencency tree is too big"
  | NotReduced -> "Unable to generate dependency tree"
  | SemError -> "Error during logical form generation: " ^ result.msg
  | NotTranslated -> "Unable to generate logical form"
  | Parsed -> "parsed"
  *)

let print_other_result file cg_bin_path query msg =
  fprintf file "%s\n" (page_header cg_bin_path);
  fprintf file "\n<H3>%s</H3>\n" query;
  fprintf file "\n<P>%s\n" msg(*generate_status_message result result.status*);
  fprintf file "%s\n" page_trailer

let string_of_mode = function
    Raw -> "Raw"
  | Struct -> "Struct"
  | CONLL -> "CONLL"
  | ENIAM -> "ENIAM"
  | Mate -> "Mate"
  | Swigra -> "Swigra"
  | POLFIE -> "POLFIE"
  | Error -> "Error"
  | Name -> "Name"
  | Identifier -> "Id"
(*
(*let rec string_of_sentence = function
    RawSentence s -> sprintf "RawSentence(%s)" s
  | StructSentence(id,paths,last) -> sprintf "StructSentence(%s,%s,%d)" id (string_of_paths1 paths) last
  | ORSentence _ -> failwith "string_of_sentence: ni"
  | AltSentence l -> sprintf "AltSentence([\n          %s])" (String.concat ";\n          " (Xlist.map l (fun (mode,sentence) ->
      string_of_mode mode ^ ", " ^ string_of_sentence sentence)))
  | _ -> failwith "string_of_sentence: ni"

let rec string_of_paragraph = function
    RawParagraph s -> sprintf "RawParagraph(%s)" s
  | StructParagraph sentences ->
      sprintf "StructParagraph([\n        %s])" (String.concat ";\n        " (Xlist.map sentences (fun p ->
        sprintf "{pid=%s; pbeg=%d; plen=%d; psentence=%s}" p.pid p.pbeg p.plen (string_of_sentence p.psentence))))
  | AltParagraph l -> sprintf "AltParagraph(\n      %s)" (String.concat "\n      " (Xlist.map l (fun (mode,paragraph) ->
      string_of_mode mode ^ ", " ^ string_of_paragraph paragraph)))

let rec string_of_text = function
    RawText s -> sprintf "RawText(%s)" s
  | StructText(paragraphs,next_id) ->
      sprintf "StructText([\n    %s],%d)" (String.concat ";\n    " (Xlist.map paragraphs string_of_paragraph)) next_id
  | AltText l -> sprintf "AltText(\n  %s)" (String.concat "\n  " (Xlist.map l (fun (mode,text) ->
      string_of_mode mode ^ ", " ^ string_of_text text)))*)
*)
let html_header =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_header_title title =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>" ^ title ^ "</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_trailer =
"</center>
  </body>
</html>"

let escape_html s =
(*   try *)
  let t = Buffer.create (Xstring.size s) in
  Int.iter 0 (String.length s - 1) (fun i ->
    match String.get s i with
       '<' -> Buffer.add_string t "&lt;"
     | '>' -> Buffer.add_string t "&gt;"
     | '&' -> Buffer.add_string t "&amp;"
     | '\n' -> Buffer.add_string t "<BR>"
     | c -> Buffer.add_char t c);
  Buffer.contents t
(*   with e -> failwith ("escape_html: '" ^ s ^ "' " ^ Printexc.to_string e) *)

let get_prefix s =
  if Xstring.size s > 1000 then 
    String.sub s 0 1000 ^ "........"
  else s

(*
let html_of_tokens tokens =
  "<table><tr><td><b>id</b></td><td><b>orth</b></td><td><b>beg</b></td><td><b>len</b></td><td><b>next</b></td><td><b>token</b></td></td><td><b>attrs</b></td></tr>" ^
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size tokens - 1) [] (fun l id ->
    let t = ExtArray.get tokens id in
    (sprintf "<tr><td>%d</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%s</td><td>%s</td></tr>"
      id t.ENIAMtokenizerTypes.orth t.ENIAMtokenizerTypes.beg t.ENIAMtokenizerTypes.len t.ENIAMtokenizerTypes.next (escape_html (ENIAMtokens.string_of_token t.ENIAMtokenizerTypes.token))
      (String.concat "; " t.ENIAMtokenizerTypes.attrs)) :: l))) ^
  "</table>"

let html_of_tokens_simple_valence tokens lex_sems =
  "<table><tr><td><b>id</b></td><td><b>orth</b></td><td><b>simple_valence</b></td></tr>" ^
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size tokens - 1) [] (fun l id ->
    let t = ExtArray.get tokens id in
    let d = ExtArray.get lex_sems id in
    Xlist.fold d.ENIAMlexSemanticsTypes.simple_valence l (fun l (fnum,frame) ->
      (sprintf "<tr><td>%d</td><td>%s</td><td>%s</td></tr>"
        id t.ENIAMtokenizerTypes.orth (ENIAMwalStringOf.fnum_frame (lemma_of_token t.token) (fnum,frame))) :: l)))) ^
  "</table>"

let html_of_tokens_valence tokens lex_sems =
  "<table><tr><td><b>id</b></td><td><b>orth</b></td><td><b>simple_valence</b></td></tr>" ^
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size tokens - 1) [] (fun l id ->
    let t = ExtArray.get tokens id in
    let d = ExtArray.get lex_sems id in
    Xlist.fold d.ENIAMlexSemanticsTypes.valence l (fun l (fnum,frame) ->
      (sprintf "<tr><td>%d</td><td>%s</td><td>%s</td></tr>"
        id t.ENIAMtokenizerTypes.orth (ENIAMwalStringOf.fnum_frame (lemma_of_token t.token) (fnum,frame))) :: l)))) ^
  "</table>"

let create_latex_chart path name chart =
  LatexMain.latex_file_out path name "a1" false (fun file ->
    Printf.fprintf file "%s\n" (LCGlatexOf.chart chart));
  LatexMain.latex_compile_and_clean path name

let create_latex_parsed_dep_chart path name parsed_dep_chart =
  LatexMain.latex_file_out path name "a1" false (fun file ->
    Printf.fprintf file "%s\n" (LCGlatexOf.parsed_dep_chart parsed_dep_chart));
  LatexMain.latex_compile_and_clean path name

let create_latex_not_parsed_dep_chart path name not_parsed_dep_chart =
  LatexMain.latex_file_out path name "a1" false (fun file ->
    Printf.fprintf file "%s\n" (LCGlatexOf.not_parsed_dep_chart not_parsed_dep_chart));
  LatexMain.latex_compile_and_clean path name

let create_latex_dep_chart path name dep_chart =
  LatexMain.latex_file_out path name "a1" false (fun file ->
    Printf.fprintf file "%s\n" (LCGlatexOf.dep_chart dep_chart));
  LatexMain.latex_compile_and_clean path name
*)

let rec extract_pos_cat_internal vars = function
  | Atom x -> x
  | AVar x -> (try extract_pos_cat_internal vars (Xlist.assoc vars x) with Not_found -> failwith "extract_pos_cat_internal")
  | With l -> String.concat "&" (Xlist.map l (extract_pos_cat_internal vars))
  | Zero -> "0"
  | Top -> "T"

let rec extract_pos_cat vars = function
  | Tensor [] -> failwith "extract_pos_cat: ni"
  | Tensor [pos] -> extract_pos_cat_internal vars pos
  | Tensor [pos;_] -> extract_pos_cat_internal vars pos
  | Tensor [pos;_;_] -> extract_pos_cat_internal vars pos
  | Tensor (Atom "num" :: _) -> "Number"
  | Tensor (Atom "aglt" :: _) -> "Aglt"
  | Tensor (Atom "prepnp" :: _) -> "Prep"
  | Tensor (Atom "comparp" :: _) -> "Compar"
  | Tensor (Atom "cp" :: _) -> "Comp"
  | Tensor [_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  | Tensor [_;_;_;_;_;_;cat;_;_] -> extract_pos_cat_internal vars cat
  (* | Tensor (pos :: cat :: _) -> (*extract_pos_cat_internal vars pos ^ "*" ^*) extract_pos_cat_internal vars cat *)
  | Tensor _ as t -> print_endline ("Unknown symbol " ^ ENIAM_LCGstringOf.grammar_symbol 0 t); "Unknown"
  | Plus l -> failwith "extract_pos_cat: ni"
  | StarWith l -> failwith "extract_pos_cat: ni"
  | Imp(s,d,t2) -> extract_pos_cat vars s
  | One -> failwith "extract_pos_cat: ni"
  | ImpSet(s,l) -> extract_pos_cat vars s
  | WithVar(v,g,e,s) -> extract_pos_cat ((v,g) :: vars) s
  | Star(_,s) -> extract_pos_cat vars s
  | Conj _ -> "Conj"
  | Preconj -> "Preconj"
  | Bracket(lf,rf,s) -> extract_pos_cat vars s
  | BracketSet d -> "BracketSet"
  | Maybe s -> failwith "extract_pos_cat: ni"

let get_text_fragment text_fragments node1 node2 =
  try IntMap.find text_fragments.(node1) node2
  with (*Not_found*)_ -> "???"(*failwith (Printf.sprintf "chart: text_fragment not found %d-%d" node1 node2)*)

let omited = StringSet.of_list ["<subst>";"<depr>";"<ppron12>";"<ppron3>";"<siebie>";"<prep>";
  "<num>";"<intnum>";"<realnum>";"<intnum-interval>";"<realnum-interval>";"<symbol>";"<ordnum>";
  "<date>";"<date-interval>";"<hour-minute>";"<hour>";"<hour-minute-interval>";"<hour-interval>";
  "<year>";"<year-interval>";"<day>";"<day-interval>";"<day-month>";"<day-month-interval>";
  "<month-interval>";"<roman>";"<roman-interval>";"<roman-ordnum>";"<match-result>";"<url>";
  "<email>";"<obj-id>";"<adj>";"<apron>";"<adjc>";"<adjp>";"<adja>";"<adv>";"<ger>";"<pact>";
  "<ppas>";"<fin>";"<bedzie>";"<praet>";"<winien>";"<impt>";"<imps>";"<pred>";"<aglt>";"<inf>";
  "<pcon>";"<pant>";"<qub>";"<comp>";"<compar>";"<conj>";"<interj>";"<sinterj>";"<burk>";
  "<interp>";"<part>";"<unk>";"<building-number>";"<html-tag>";"<list-item>";"<numcomp>";
  "<phone-number>";"<postal-code>";"<sentence>";"<paragraph>"]

let cat_tokens_sequence text_fragments g =
  let _,_,l = ENIAM_LCGchart.fold g (0,0,[]) (fun (m,n,l) (symbol,node1,node2,cost,sem,layer) ->
    node1,node2,
    (if m < node1 then
      if n < node1 then [n, node1, get_text_fragment text_fragments n node1, "null"]
      else if n = node1 then []
      else [node1, n, get_text_fragment text_fragments node1 n, "overlap"]
    else if m = node1 then
      if n < node2 then [m, n, get_text_fragment text_fragments m n, "overlap"]
      else if n = node2 then []
      else [node1, node2, get_text_fragment text_fragments node1 node2, "overlap"]
    else failwith "cat_tokens_sequence") @
    [node1, node2, get_text_fragment text_fragments node1 node2, extract_pos_cat [] symbol] @ l) in
  let map = Xlist.fold l IntMap.empty (fun map (m,n,text,symbol) ->
    IntMap.add_inc map (1000000*m+n) [text,symbol] (fun l -> (text,symbol) :: l)) in
  let map = IntMap.map map (fun l ->
    let t,ov,set = Xlist.fold l ("",false,StringSet.empty) (fun (t,ov,set) (text,symbol) ->
      if symbol = "null" then text,ov,set
      else if symbol = "overlap" then t,true,set
      else if StringSet.mem omited symbol then text,ov,set
      else t,ov,StringSet.add set symbol) in
    let l = if StringSet.is_empty set then [t] else StringSet.to_list set in
    if ov then "OVERLAP{" ^ String.concat " " l ^ "}" else
    match l with
      [t] -> t
    | _ -> "{" ^ String.concat " " l ^ "}") in
  let l = List.sort compare (IntMap.fold map [] (fun l k texts -> (k,texts) :: l)) in
(*  let l = Xlist.sort l (fun (m1,n1,text1,symbol1) (m2,n2,text2,symbol2) ->
    if m1 <> m2 then compare m1 m2 else
    if n1 <> n2 then compare n1 n2 else
    compare symbol1 symbol2) in
  let l = if l = [] then l else
    Xlist.fold (List.tl l) [List.hd l] (fun l a ->
      match l with
        [] -> failwith "cat_tokens_sequence"
      | b :: l -> if a = b then b :: l else a :: b :: l) in*)
  String.concat " " (Xlist.map l (fun (n,texts) -> texts))


(* verbosity:
  0 -> jedynie informacja o statusie zdania
  1 -> zawartość struktur danych istotnych dla uzyskanego statusu
  2 -> zawartość wszystkich struktur danych
*)

let html_of_eniam_sentence path file_prefix img verbosity tokens (result : eniam_parse_result) =
  match result.status with
    Idle -> "<font color=\"red\">idle</font>\n"
(*   | NotLemmatized -> sprintf "<font color=\"red\">not_lemmatized</font>\n"  *)
  | LexiconError -> sprintf "<font color=\"red\">error_lex</font>: %s paths_size=%d\n" (escape_html result.msg) result.paths_size
  | ParseError ->
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2);
      sprintf "<font color=\"red\">error_parse</font>: %s paths_size=%d\n" (escape_html result.msg) result.paths_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix) ^
      ""
  | ParseTimeout ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2);
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2);
      sprintf "<font color=\"red\">timeout</font>: %s paths_size=%d\n" (escape_html result.msg) result.paths_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix) ^
      ""
  | NotParsed ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1);
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4" result.text_fragments result.chart3);
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_chart2 path (file_prefix ^ "_3_chart_selection") "a4" result.text_fragments (ENIAM_LCGchart.select_maximal result.chart3));
      sprintf "<font color=\"red\">not_parsed</font>: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix) ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR>%s\n" (escape_html (cat_tokens_sequence result.text_fragments (ENIAM_LCGchart.select_maximal result.chart1))) ^
        sprintf "<BR><A HREF=\"%s_3_chart_selection.pdf\">Chart 3 Selection</A>\n" file_prefix) ^
      ""
  | ReductionError ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4" result.text_fragments result.chart3);
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3);
      sprintf "<font color=\"red\">error_reduction</font>: %s paths_size=%d chart_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix) ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix) ^
      ""
  | TooManyNodes ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4" result.text_fragments result.chart3;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3);
      sprintf "<font color=\"red\">to_many_nodes</font>: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix) ^
      ""
  | NotReduced ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4" result.text_fragments result.chart3);
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a4" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (ENIAM_LCGlatexOf.linear_term 0 result.term4));
        Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0" result.dependency_tree4);
      sprintf "<font color=\"red\">not_reduced</font>: paths_size=%d chart_size=%d dependency_tree_size=%d\n" result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix)  ^
      ""
  | ReductionError2 ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4" result.text_fragments result.chart3);
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a4" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (ENIAM_LCGlatexOf.linear_term 0 result.term4));
        Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0" result.dependency_tree4);
      sprintf "<font color=\"red\">error_reduction2</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix) ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix)  ^
      ""
  | ReductionError3 ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a0" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a0" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a0" result.text_fragments result.chart3;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a4" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (ENIAM_LCGlatexOf.linear_term 0 result.term4));
          Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0" result.dependency_tree4;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_5_dependency_tree") "a4" result.dependency_tree5;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") "a4" result.dependency_tree6a;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a4" result.dependency_tree6b);
      if verbosity = 0 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") result.dependency_tree6a;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6a_simple_dependency_tree") result.dependency_tree6a;
        ENIAM_LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6b_simple_dependency_tree") result.dependency_tree6b);
      sprintf "<font color=\"red\">error_reduction3</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_5_dependency_tree.pdf\">Dependency Tree References 5</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6a_dependency_tree.pdf\">Dependency Tree References 6a</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6a_dependency_tree.png\">Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6b_dependency_tree.png\">Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6a_simple_dependency_tree.png\">Simplified Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_simple_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6b_simple_dependency_tree.png\">Simplified Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_simple_dependency_tree.png\">\n" file_prefix)) ^
      ""
  | Parsed | PartialParsed ->
      if verbosity < 2 then () else (
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_1_chart") "a1" result.text_fragments result.chart1;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_2_chart") "a4" result.text_fragments result.chart2;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_2_references") "a0" result.references2;
        ENIAM_LCGlatexOf.print_chart path (file_prefix ^ "_3_chart") "a4" result.text_fragments result.chart3;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_3_references") "a0" result.references3;
        Xlatex.latex_file_out path (file_prefix ^ "_4_term") "a4" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (ENIAM_LCGlatexOf.linear_term 0 result.term4));
          Xlatex.latex_compile_and_clean path (file_prefix ^ "_4_term");
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_4_dependency_tree") "a0" result.dependency_tree4;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_5_dependency_tree") "a4" result.dependency_tree5;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") "a4" result.dependency_tree6a;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a4" result.dependency_tree6b);
      if verbosity = 0 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6a_dependency_tree") result.dependency_tree6a;
        ENIAM_LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6a_simple_dependency_tree") result.dependency_tree6a);
      if verbosity < 2 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_simplified_dependency_tree path (file_prefix ^ "_6b_simple_dependency_tree") result.dependency_tree6b);
      sprintf "%s: paths_size=%d chart_size=%d dependency_tree_size=%d\n" 
        (if result.status = Parsed then "parsed" else "partial_parsed") result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_1_chart.pdf\">Chart 1</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_chart.pdf\">Chart 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_2_references.pdf\">References 2</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_chart.pdf\">Chart 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_3_references.pdf\">References 3</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_term.pdf\">Term 4</A>\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_4_dependency_tree.pdf\">Dependency Tree References 4</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_5_dependency_tree.pdf\">Dependency Tree References 5</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6a_dependency_tree.pdf\">Dependency Tree References 6a</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6a_dependency_tree.png\">Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6a_simple_dependency_tree.png\">Simplified Dependency Tree 6a</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6a_simple_dependency_tree.png\">\n" file_prefix)) ^
      (if verbosity < 2 then "" else
        (if img <> 2 then sprintf "<BR><A HREF=\"%s_6b_dependency_tree.png\">Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix) ^
        (if img <> 1 then sprintf "<BR><A HREF=\"%s_6b_simple_dependency_tree.png\">Simplified Dependency Tree 6b</A>\n" file_prefix
         else sprintf "<BR><IMG SRC=\"%s_6b_simple_dependency_tree.png\">\n" file_prefix)) ^
      ""
  | SemValenceError ->
      if verbosity = 0 then () else (
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        if result.dependency_tree7 <> [| |] then ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        if ExtArray.size result.dependency_tree8 <> 0 then ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        if result.dependency_tree9 <> [| |] then ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        if result.dependency_tree9 <> [| |] then ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9);
      sprintf "<font color=\"red\">error_sem_valence</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix ^
        (if result.dependency_tree7 <> [| |] then sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix else "")  ^
        (if ExtArray.size result.dependency_tree8 <> 0 then sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix else "")  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix)  ^
      ""
  | SemGraphError ->
      if verbosity = 2 then (
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        if result.dependency_tree7 <> [| |] then ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        if ExtArray.size result.dependency_tree8 <> 0 then ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        if result.dependency_tree9 <> [| |] then ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        if result.dependency_tree9 <> [| |] then ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        if result.semantic_graph10 <> [| |] then ENIAMsemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3" result.semantic_graph10;
        if result.semantic_graph11 <> ENIAMsemTypes.Dot then ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 1 then (
        if result.semantic_graph11 <> ENIAMsemTypes.Dot then ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11 else
        if result.semantic_graph10 <> [| |] then ENIAMsemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3" result.semantic_graph10 else (
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        if result.dependency_tree7 <> [| |] then ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        if ExtArray.size result.dependency_tree8 <> 0 then ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        if result.dependency_tree9 <> [| |] then ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        if result.dependency_tree9 <> [| |] then ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9));
      sprintf "<font color=\"red\">error_sem_graph</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity = 2 then
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix ^
        (if result.semantic_graph10 <> [| |] then sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix else "") ^
        (if result.semantic_graph11 <> ENIAMsemTypes.Dot then sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix else "")  ^
        (if result.dependency_tree7 <> [| |] then sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix else "")  ^
        (if ExtArray.size result.dependency_tree8 <> 0 then sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix else "")  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix else
      if verbosity = 1 then (
        if result.semantic_graph11 <> ENIAMsemTypes.Dot then sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix else
        if result.semantic_graph10 <> [| |] then sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix ^
        (if result.dependency_tree7 <> [| |] then sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix else "")  ^
        (if ExtArray.size result.dependency_tree8 <> 0 then sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix else "")  ^
        (if result.dependency_tree9 <> [| |] then sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix else "")  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix) else "")  ^
      ""
  | SemGraphError2 ->
      if verbosity = 0 then () else (
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      sprintf "<font color=\"red\">error_sem_graph2</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | SemNotValidated ->
(*       print_endline "html_of_eniam_sentence: SemNotValidated 1"; *)
      if verbosity < 2 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        ENIAMsemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3" result.semantic_graph10;
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12;
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_13_semantic_graph") "" result.semantic_graph13);
(*      print_endline ("html_of_eniam_sentence: SemNotValidated 2: ^ |result.msg|=" ^ string_of_int (Xstring.size result.msg));
      let s = escape_html result.msg in
      print_endline ("html_of_eniam_sentence: SemNotValidated 3: ^ |s|=" ^ string_of_int (Xstring.size s));*)
      let s = sprintf "<font color=\"red\">sem_not_validated</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html (get_prefix result.msg)) result.paths_size result.chart_size result.dependency_tree_size in
(*       print_endline "html_of_eniam_sentence: SemNotValidated 4"; *)
      s ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_13_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | SemParsed | PartialSemParsed ->
      if verbosity < 2 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        ENIAMsemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3" result.semantic_graph10;
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12);
      sprintf "%s: paths_size=%d chart_size=%d dependency_tree_size=%d\n" 
        (if result.status = SemParsed then "sem_parsed" else "partial_sem_parsed") result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | InferenceError ->
      if verbosity < 2 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        ENIAMsemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3" result.semantic_graph10;
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12);
      sprintf "<font color=\"red\">inference_error</font>: %s paths_size=%d chart_size=%d dependency_tree_size=%d\n" (escape_html result.msg) result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | Inferenced ->
      if verbosity < 2 then () else (
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") result.dependency_tree6b;
        ENIAM_LCGgraphOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") result.dependency_tree9;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_6b_dependency_tree") "a3" result.dependency_tree6b;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_7_dependency_tree") "a2" result.dependency_tree7;
        ENIAM_LCGlatexOf.print_references path (file_prefix ^ "_8_dependency_tree") "a3" result.dependency_tree8;
        ENIAM_LCGlatexOf.print_dependency_tree path (file_prefix ^ "_9_dependency_tree") "a3" result.dependency_tree9;
        ENIAMsemLatexOf.print_semantic_graph path (file_prefix ^ "_10_semantic_graph") "a3" result.semantic_graph10;
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_11_semantic_graph") "" result.semantic_graph11);
      if verbosity = 0 then () else (
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_12_semantic_graph") "" result.semantic_graph12;
        ENIAMsemGraphOf.print_semantic_graph2 path (file_prefix ^ "_13_semantic_graph") "" result.semantic_graph13);
      sprintf "inferenced: paths_size=%d chart_size=%d dependency_tree_size=%d\n" result.paths_size result.chart_size result.dependency_tree_size ^
      (if verbosity < 2 then "" else
        sprintf "<BR><A HREF=\"%s_6b_dependency_tree.pdf\">Dependency Tree References 6b</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_7_dependency_tree.pdf\">Dependency Tree References 7</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_8_dependency_tree.pdf\">Dependency Tree References 8</A>\n" file_prefix  ^
        sprintf "<BR><A HREF=\"%s_9_dependency_tree.pdf\">Dependency Tree References 9</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_6b_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><IMG SRC=\"%s_9_dependency_tree.png\">\n" file_prefix ^
        sprintf "<BR><A HREF=\"%s_10_semantic_graph.pdf\">Semantic Graph References 10</A>\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_11_semantic_graph.png\">\n" file_prefix)  ^
      (if verbosity = 0 then "" else
        sprintf "<BR><IMG SRC=\"%s_12_semantic_graph.png\">\n" file_prefix  ^
        sprintf "<BR><IMG SRC=\"%s_13_semantic_graph.png\">\n" file_prefix)  ^
      ""
  | _ -> failwith "html_of_eniam_sentence"

(*
let html_of_conll_sentence path img verbosity tokens (result : conll_parse_result) =
  match result.status with
    Idle -> "idle\n"
  (* | PreprocessingError -> "error_pre: %s\n" result.msg *)
  | LexiconError -> sprintf "error_lex: %s\n" result.msg
  | ParseError ->
      create_latex_dep_chart path (result.file_prefix ^ "_dep_chart") result.dep_chart;
      create_latex_parsed_dep_chart path (result.file_prefix ^ "_parsed_dep_chart") result.parsed_dep_chart;
      sprintf "error_parse: %s\n" result.msg ^
      sprintf "<BR><A HREF=\"%s_dep_chart.pdf\">Chart</A>\n" result.file_prefix ^
      sprintf "<BR><A HREF=\"%s_parsed_dep_chart.pdf\">Parsed Chart</A>\n" result.file_prefix
  | ParseTimeout ->
      create_latex_dep_chart path (result.file_prefix ^ "_dep_chart") result.dep_chart;
      create_latex_parsed_dep_chart path (result.file_prefix ^ "_parsed_dep_chart") result.parsed_dep_chart;
      sprintf "timeout: %s\n" result.msg ^
      sprintf "<BR><A HREF=\"%s_dep_chart.pdf\">Chart</A>\n" result.file_prefix ^
      sprintf "<BR><A HREF=\"%s_parsed_dep_chart.pdf\">Parsed Chart</A>\n" result.file_prefix
  | NotParsed ->
      create_latex_dep_chart path (result.file_prefix ^ "_dep_chart") result.dep_chart;
      create_latex_not_parsed_dep_chart path (result.file_prefix ^ "_not_parsed_dep_chart") result.not_parsed_dep_chart;
      sprintf "not_parsed\n" ^
      html_of_dep_sentence tokens result.paths ^
      sprintf "<BR><A HREF=\"%s_dep_chart.pdf\">Chart</A>\n" result.file_prefix ^
      sprintf "<BR><A HREF=\"%s_not_parsed_dep_chart.pdf\">Not Parsed Chart</A>\n" result.file_prefix
  | ReductionError -> sprintf "error_reduction: %s\n" result.msg
  | TooManyNodes -> sprintf "to_many_nodes: paths_size=%d\n" result.paths_size
  | NotReduced ->
      LCGlatexOf.print_dependency_tree path (result.file_prefix ^ "_dependency_tree_references") result.dependency_tree;
      sprintf "not_reduced: paths_size=%d\n" result.paths_size ^
      sprintf "<BR><A HREF=\"%s_dependency_tree_references.pdf\">Dependency Tree References</A>\n" result.file_prefix
  | SemError -> sprintf "error_sem: %s dependency_tree_size=%d\n" result.msg result.dependency_tree_size
  (* | NotTranslated -> "not_translated: \n"  *)
  | Parsed ->
      (* print_simplified_dependency_tree path (result.file_prefix ^ "_simplified_dependency_tree") tokens result.dependency_tree; *)
      print_dependency_tree path (result.file_prefix ^ "_dependency_tree") result.dependency_tree;
      (* LCGlatexOf.print_dependency_tree path (result.file_prefix ^ "_dependency_tree_references") result.dependency_tree; *)
      sprintf "parsed: paths_size=%d dependency_tree_size=%d\n" result.paths_size result.dependency_tree_size ^
      (* sprintf "<BR><A HREF=\"%s_simplified_dependency_tree.png\">Simplified Dependency Tree</A>\n" result.file_prefix ^ *)
      sprintf "<BR><A HREF=\"%s_dependency_tree.png\">Dependency Tree</A>\n" result.file_prefix ^
      (* sprintf "<BR><A HREF=\"%s_dependency_tree_references.pdf\">Dependency Tree References</A>\n" result.file_prefix *)
      ""
  | _ -> failwith "html_of_conll_sentence"

let html_of_sem_sentence path tokens (result : semantic_processing_result) =
  match result.status with
    Idle -> "idle\n"
  | SemError -> sprintf "error_sem: %s\n" result.msg
      (* print_dependency_tree path (result.file_prefix ^ "_disamb") result.disamb;
      print_dependency_tree path (result.file_prefix ^ "_sem") result.sem;
      print_dependency_tree path (result.file_prefix ^ "_sem2") result.sem2;
      print_graph2 "results/" "sem3" query result.sem3; *)
  | NotTranslated ->
      (* print_dependency_tree path (result.file_prefix ^ "_disamb") result.disamb;
      print_dependency_tree path (result.file_prefix ^ "_sem") result.sem;
      print_dependency_tree path (result.file_prefix ^ "_sem2") result.sem2; *)
      print_graph2 path (result.file_prefix ^ "_sem3") "" result.sem3;
      sprintf "not_translated \n" ^
      (* sprintf "<BR><A HREF=\"%s_disamb.png\">Disambiguated Dependency Tree</A>\n" result.file_prefix ^
      sprintf "<BR><A HREF=\"%s_sem.png\">Semantic Graph 1</A>\n" result.file_prefix ^
      sprintf "<BR><A HREF=\"%s_sem2.png\">Semantic Graph 2</A>\n" result.file_prefix ^ *)
      sprintf "<BR><A HREF=\"%s_sem3.png\">Semantic Graph</A>\n" result.file_prefix
  | Parsed ->
      print_graph2 path (result.file_prefix ^ "_sem3") "" result.sem3;
      sprintf "parsed \n" ^
      sprintf "<BR><A HREF=\"%s_sem3.png\">Semantic Graph</A>\n" result.file_prefix
  | _ -> failwith "html_of_sem_sentence"
*)
let file_prefix_of_mode = function
    Raw -> "R"
  | Struct -> "St"
  | CONLL -> "C"
  | ENIAM -> "E"
  | Mate -> "M"
  | Swigra -> "S"
  | POLFIE -> "P"
  | Error -> "Er"
  | Name -> "N"
  | Identifier -> "I"

let rec html_of_sentence path file_prefix mode img verbosity tokens = function
    RawSentence s -> escape_html s
  | StructSentence(paths,last) -> ENIAMsubsyntaxHTMLof.html_of_struct_sentence tokens paths last
  | DepSentence paths -> String.concat "<BR>\n" (Xlist.map paths (ENIAMsubsyntaxHTMLof.html_of_dep_sentence tokens))
  | ENIAMSentence result ->
(*     Int.iter 0 (Array.length result.dependency_tree6a - 1) (fun i -> print_endline ("\n6a " ^ (string_of_int i) ^ ":\n" ^ (ENIAM_LCGstringOf.linear_term 0 result.dependency_tree6a.(i))));
     Int.iter 0 (Array.length result.dependency_tree6b - 1) (fun i -> print_endline ("\n6b " ^ (string_of_int i) ^ ":\n" ^ (ENIAM_LCGstringOf.linear_term 0 result.dependency_tree6b.(i))));*)
     let file_prefix = file_prefix_of_mode mode ^ file_prefix in
(*      print_endline "html_of_sentence 1"; *)
     let s = html_of_eniam_sentence path file_prefix img verbosity tokens result in
(*      print_endline "html_of_sentence 2"; *)
     s
  (* | CONLLSentence result -> html_of_conll_sentence path img verbosity tokens result
  | SemSentence result -> html_of_sem_sentence path img verbosity tokens result *)
  | QuotedSentences sentences ->
      String.concat "<BR>\n" (Xlist.map sentences (fun p ->
        sprintf "id=%s beg=%d len=%d next=%d<BR>%s" p.id p.beg p.len p.next (html_of_sentence path p.file_prefix mode img verbosity tokens p.sentence)))
  | AltSentence l -> (*print_endline "AltSentence";*)
     "<table border=1>" ^
     String.concat "\n" (Xlist.map l (fun (mode,sentence) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (string_of_mode mode) (html_of_sentence path file_prefix mode img verbosity tokens sentence))) ^
     "</table>"
  (* | _ -> failwith "html_of_sentence: ni" *)
  | ErrorSentence s -> sprintf "<font color=\"red\">sentence_error</font>: %s\n" (escape_html s)

let rec html_of_paragraph path mode img verbosity tokens = function
    RawParagraph s -> (*print_endline "RawParagraph";*) escape_html s
  | StructParagraph sentences -> (*print_endline "StructParagraph";*)
      String.concat "<BR>\n" (Xlist.map sentences (fun p ->
        sprintf "id=%s beg=%d len=%d next=%d<BR>%s" p.id p.beg p.len p.next (html_of_sentence path p.file_prefix mode img verbosity tokens p.sentence)))
  | AltParagraph l -> (*print_endline "AltParagraph";*)
     "<table border=2>" ^
     String.concat "\n" (Xlist.map l (fun (mode,paragraph) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (string_of_mode mode) (html_of_paragraph path mode img verbosity tokens paragraph))) ^
     "</table>"
  | ErrorParagraph s -> sprintf "<font color=\"red\">paragraph_error</font>: %s\n" (escape_html s)

let rec html_of_text path mode img verbosity tokens = function
    RawText s -> escape_html s
  | StructText paragraphs ->
      String.concat "<BR>\n" (Xlist.map paragraphs (html_of_paragraph path mode img verbosity tokens))
  | JSONtext s -> "<pre>" ^ escape_html s ^ "</pre><BR>"
  | AltText l ->
     "<table border=3>" ^
     String.concat "\n" (Xlist.map l (fun (mode,text) ->
       sprintf "<tr><td>%s</td><td>%s</td></tr>" (string_of_mode mode) (html_of_text path mode img verbosity tokens text))) ^
     "</table>"
  | ErrorText s -> sprintf "<font color=\"red\">text_error</font>: %s\n" (escape_html s)

let html_of_text_as_paragraph path mode img verbosity tokens = function
    AltText[Raw,_;Struct,StructText[paragraph]] -> html_of_paragraph path mode img verbosity tokens paragraph
  | text -> html_of_text path mode img verbosity tokens text

let print_html_text path name text img verbosity tokens (*lex_sems*) =
  File.file_out (path ^ name ^ ".html") (fun file ->
    fprintf file "%s\n" html_header;
    fprintf file "%s<BR>\n" (html_of_text path Struct img verbosity tokens text);
    (* fprintf file "%s<BR>\n" (html_of_tokens tokens); *)
(*    fprintf file "%s<BR>\n" (html_of_tokens_simple_valence tokens);
    fprintf file "%s<BR>\n" (html_of_tokens_valence tokens);*)
    fprintf file "%s\n" html_trailer)

let rec find_prev_next_sentence pid file_prefix rev = function
    AltSentence[Raw,_;Struct,QuotedSentences sentences] ->
      Xlist.fold sentences rev (fun rev p -> find_prev_next_sentence p.id p.file_prefix rev p.sentence)
  | AltSentence[Raw,RawSentence s; mode,ENIAMSentence result] -> file_prefix :: rev
  | AltSentence((Raw,RawSentence s) :: _) -> file_prefix :: rev
  (* | AltSentence[Raw,RawSentence s] -> ("p" ^ pid) :: rev *)
  | _ -> failwith "find_prev_next_sentence: ni"

let rec find_prev_next_paragraph rev = function
    RawParagraph s -> rev
  | StructParagraph sentences ->
      Xlist.fold sentences rev (fun rev p -> find_prev_next_sentence p.id p.file_prefix rev p.sentence)
  | AltParagraph l -> Xlist.fold l rev (fun rev (mode,paragraph) -> find_prev_next_paragraph rev paragraph)
  | ErrorParagraph s -> rev

let rec make_prev_next_map map prev = function
    [x] -> StringMap.add map x (prev,"")
  | x :: next :: l -> make_prev_next_map (StringMap.add map x (prev,next)) x (next :: l)
  | [] -> failwith "make_prev_next_map"

let print_main_result results_web_path mode path id file_prefix (result : eniam_parse_result) file =
  if mode = ENIAM then () else
  begin
    fprintf file "<h4>Parsed by %s\n</h4>" (string_of_mode mode);
    if result.status = Parsed (* else = SemParsed *)
    then ENIAM_LCGgraphOf.print_simplified_dependency_tree path ("tree" ^ id ^ "_" ^ (string_of_mode mode) ^ "_" ^ file_prefix) result.dependency_tree6a
    else ENIAMsemGraphOf.print_semantic_graph2 path ("tree" ^ id ^ "_" ^ (string_of_mode mode) ^ "_" ^ file_prefix) "" result.semantic_graph11;
    fprintf file "<P><IMG SRC=\"%stree%s_%s_%s.png\">\n" results_web_path id (string_of_mode mode) file_prefix
  end

(*  ignore (Xlist.fold2 result.trees result.mrls 1 (fun n tree mrl ->
    print_graph2 path ("tree" ^ id ^ "_" ^ result.file_prefix ^ "_" ^ string_of_int n) "" tree;
    print_xml_tree path ("tree" ^ id ^ "_" ^ result.file_prefix ^ "_" ^ string_of_int n) tree;
    let mml = SemMmlOf.mml_of_mrl mrl in
    print_mml path ("formula" ^ id ^ "_" ^ result.file_prefix ^ "_" ^ string_of_int n) mml;
    fprintf file "<P><IMG SRC=\"tree%s_%s_%d.png\">\n" id result.file_prefix n;
    fprintf file "<P>%s\n" (Xml.to_string_fmt mml);
    fprintf file "<P><A target=\"_blank\" HREF=\"tree%s_%s_%d.xml\">Graf w formacie XML</A>\n" id result.file_prefix n;
    fprintf file "<P><A target=\"_blank\" HREF=\"formula%s_%s_%d.mml\">Formuła w formacie MathML</A>\n" id result.file_prefix n;
    n+1));*)

let print_not_parsed_main_result mode file =
(*  fprintf file "<h4>Parsing by %s</h4>\n" (string_of_mode mode);
  fprintf file "<p>Not parsed\n";
*)  ()

let print_sentence_to_file path cg_bin_path results_web_path title id file_prefix prev_next_map query sentences file =
  let print_local file = (function
      mode,ENIAMSentence result ->
        if result.status = Parsed || result.status = SemParsed || result.status = PartialParsed || result.status = PartialSemParsed
        then print_main_result results_web_path mode path id file_prefix result file
        else print_not_parsed_main_result mode file
    | _ -> failwith "print_sentence_to_file: not_eniam_sentence") in
  let prev,next = (try StringMap.find prev_next_map file_prefix with Not_found -> failwith "print_sentence_to_file: prev_next") in
  fprintf file "%s\n" (page_header cg_bin_path);
  if prev <> "" then fprintf file "<A HREF=\"%spage%s_%s.html\">Poprzednie zdanie</A> " results_web_path id prev;
  if next <> "" then fprintf file " <A HREF=\"%spage%s_%s.html\">Następne zdanie</A>" results_web_path id next;
  if title then fprintf file "\n<h2>%s</h2>\n" query;

  Xlist.iter sentences (print_local file);

  fprintf file "<p>";
  if prev <> "" then fprintf file "<A HREF=\"%spage%s_%s.html\">Poprzednie zdanie</A><br>" results_web_path id prev;
  if next <> "" then fprintf file " <A HREF=\"%spage%s_%s.html\">Następne zdanie</A><br>" results_web_path id next;
(*  fprintf file "<a target=\"_blank\" href=\"%sparsed_text_%s.html\">Struktura całości</a><br>" results_web_path id;
*)  fprintf file "<a target=\"_blank\" href=\"%spage%s_%s.html\">Statyczny link</a><br>" results_web_path id file_prefix;
  fprintf file "%s\n" page_trailer

let rec print_main_result_sentence path cg_bin_path results_web_path id file_prefix tokens pid prev_next_map = function
    AltSentence[Raw,_;Struct,QuotedSentences sentences] ->
      Xlist.iter sentences (fun p -> print_main_result_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence)
  | AltSentence((Raw,RawSentence query) :: sentences) ->
      File.file_out (path ^ "page" ^ id ^ "_" ^ file_prefix ^ ".html") (fun file ->
        print_sentence_to_file path cg_bin_path results_web_path true id file_prefix prev_next_map query sentences file)
  | _ -> failwith "print_main_result_sentence: ni"

let rec print_main_result_paragraph path cg_bin_path results_web_path id tokens prev_next_map = function
    RawParagraph s -> ()
  | StructParagraph sentences ->
      Xlist.iter sentences (fun p -> print_main_result_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence)
  | AltParagraph l -> Xlist.iter l (fun (mode,paragraph) -> print_main_result_paragraph path cg_bin_path results_web_path id tokens prev_next_map paragraph)
  | ErrorParagraph s -> File.file_out (path ^ "page" ^ id ^ "_Er.html") (fun file ->
      print_sentence_to_file path cg_bin_path results_web_path false id "Er" prev_next_map ("ErrorParagraph: " ^ s) [] file)

let rec print_main_result_text path cg_bin_path results_web_path id tokens = function
    RawText s -> ()
  | StructText paragraphs ->
      let prev_next_map = make_prev_next_map StringMap.empty ""
        (List.rev (Xlist.fold paragraphs [] find_prev_next_paragraph)) in
      Xlist.iter paragraphs (print_main_result_paragraph path cg_bin_path results_web_path id tokens prev_next_map)
  | JSONtext s -> () (*"<pre>" ^ escape_html s ^ "</pre><BR>"*) (* FIXME *)
  | AltText l -> Xlist.iter l (fun (mode,text) -> print_main_result_text path cg_bin_path results_web_path id tokens text)
  | ErrorText s -> failwith "print_main_result_text: ni"

let rec print_main_result_first_page_sentence path cg_bin_path results_web_path id file_prefix tokens pid prev_next_map = function
    AltSentence[Raw,_;Struct,QuotedSentences sentences] ->
      let p = List.hd sentences in
      print_main_result_first_page_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence
  | AltSentence((Raw,RawSentence query) :: sentences) ->
      print_sentence_to_file path cg_bin_path results_web_path false id file_prefix prev_next_map query sentences stdout
  | _ -> failwith "print_main_result_first_page_sentence: ni"

let rec print_main_result_first_page_paragraph path cg_bin_path results_web_path id tokens prev_next_map = function
    RawParagraph s -> ()
  | StructParagraph sentences ->
      let p = List.hd sentences in
      print_main_result_first_page_sentence path cg_bin_path results_web_path id p.file_prefix tokens p.id prev_next_map p.sentence
  | AltParagraph l -> Xlist.iter l (fun (mode,paragraph) -> print_main_result_first_page_paragraph path cg_bin_path results_web_path id tokens prev_next_map paragraph)
  | ErrorParagraph s -> print_sentence_to_file path cg_bin_path results_web_path false id "Er" prev_next_map ("ErrorParagraph: " ^ s) [] stdout

let rec print_main_result_first_page_text path cg_bin_path results_web_path id tokens = function
    RawText s -> ()
  | StructText paragraphs ->
      let prev_next_map = make_prev_next_map StringMap.empty ""
        (List.rev (Xlist.fold paragraphs [] find_prev_next_paragraph)) in
      print_main_result_first_page_paragraph path cg_bin_path results_web_path id tokens prev_next_map (List.hd paragraphs)
  | JSONtext s -> () (*"<pre>" ^ escape_html s ^ "</pre><BR>"*) (* FIXME *)
  | AltText l -> Xlist.iter l (fun (mode,text) -> print_main_result_first_page_text path cg_bin_path results_web_path id tokens text)
  | ErrorText s -> failwith "print_main_result_first_page_text: ni"

let to_string_eniam_sentence verbosity tokens (result : eniam_parse_result) =
  let status_string = string_of_status result.status in
  if result.status = NotParsed then
    [status_string ^ ": " ^ cat_tokens_sequence result.text_fragments (ENIAM_LCGchart.select_maximal result.chart1)]
  else [status_string]

let rec to_string_sentence verbosity tokens = function
    RawSentence s -> []
  | StructSentence(paths,last) -> []
  | DepSentence paths -> []
  | ENIAMSentence result -> to_string_eniam_sentence verbosity tokens result
  | QuotedSentences sentences -> List.flatten (Xlist.map sentences (fun p -> to_string_sentence verbosity tokens p.sentence))
  | AltSentence l -> List.flatten (Xlist.map l (fun (mode,sentence) -> to_string_sentence verbosity tokens sentence))
  | ErrorSentence s -> []

let rec to_string_paragraph verbosity tokens = function
    RawParagraph s -> []
  | StructParagraph sentences -> List.flatten (Xlist.map sentences (fun p -> to_string_sentence verbosity tokens p.sentence))
  | AltParagraph l -> List.flatten (Xlist.map l (fun (mode,paragraph) -> to_string_paragraph verbosity tokens paragraph))
  | ErrorParagraph s -> ["SubsyntaxError"]

let rec to_string_text verbosity tokens = function
    RawText s -> []
  | StructText paragraphs -> List.flatten (Xlist.map paragraphs (to_string_paragraph verbosity tokens))
  | JSONtext s -> [] (*"<pre>" ^ escape_html s ^ "</pre><BR>"*) (* FIXME *)
  | AltText l -> List.flatten (Xlist.map l (fun (mode,text) -> to_string_text verbosity tokens text))
  | ErrorText s -> ["ErrorText"]
