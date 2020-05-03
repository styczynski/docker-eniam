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

type prolog_symbols =
    T of string
  | C of string
  | Te of string * prolog_symbols list
  | Li of prolog_symbols list
  | LP | RP | Com | Do | LB | RB

let regexp2 = Str.regexp "%\\|\n\\|\\\\'\\|'"
let regexp3 = Str.regexp " \\|\t\\|(\\|)\\|\\,\\|\\.\\|\\[\\|\\]"

(*let rec eliminate_comments = function
    Str.Delim "%" :: Str.Text _ :: Str.Delim "\n" :: l -> eliminate_comments l
  | Str.Delim "'" :: Str.Text s :: Str.Delim "'" :: l -> C s :: (eliminate_comments l)
  | Str.Delim "'" :: Str.Text s1 :: Str.Delim "\\'" :: Str.Text s2 :: Str.Delim "'" :: l -> C (s1 ^ "\\'" ^ s2) :: (eliminate_comments l)
  | Str.Delim "'" :: Str.Text s1 :: Str.Delim "\\'" :: Str.Text s2 :: Str.Delim "\\'" :: Str.Text s3 :: Str.Delim "'" :: l -> C (s1 ^ "\\'" ^ s2 ^ "\\'" ^ s3) :: (eliminate_comments l)
  | Str.Delim "\n" :: l -> eliminate_comments l
  | Str.Text s :: l -> T s :: (eliminate_comments l)
  | [] -> []
  | l -> failwith ("eliminate_comments: " ^ (String.concat "+" (Xlist.map l (function Str.Delim s -> s | Str.Text s -> s))))*)

let rec eliminate_comments rev = function
    Str.Delim "%" :: Str.Text _ :: Str.Delim "\n" :: l -> eliminate_comments rev l
  | Str.Delim "'" :: Str.Text s :: Str.Delim "'" :: l -> eliminate_comments (C s :: rev) l
  | Str.Delim "'" :: Str.Text s1 :: Str.Delim "\\'" :: Str.Text s2 :: Str.Delim "'" :: l -> eliminate_comments (C (s1 ^ "\\'" ^ s2) :: rev) l
  | Str.Delim "'" :: Str.Text s1 :: Str.Delim "\\'" :: Str.Text s2 :: Str.Delim "\\'" :: Str.Text s3 :: Str.Delim "'" :: l -> eliminate_comments (C (s1 ^ "\\'" ^ s2 ^ "\\'" ^ s3) :: rev) l
  | Str.Delim "'" :: Str.Delim "\\'" :: Str.Delim "'" :: l -> eliminate_comments (C "\\'" :: rev) l
  | Str.Delim "\n" :: l -> eliminate_comments rev l
  | Str.Text s :: l -> eliminate_comments (T s :: rev) l
  | [] -> List.rev rev
  | l -> failwith ("eliminate_comments: " ^ (String.concat "+" (Xlist.map l (function Str.Delim s -> s | Str.Text s -> s))))

let rec map_prolog_symbols = function
    Str.Delim " " :: l -> map_prolog_symbols l
  | Str.Delim "\t" :: l -> map_prolog_symbols l
  | Str.Delim "(" :: l -> LP :: (map_prolog_symbols l)
  | Str.Delim ")" :: l -> RP :: (map_prolog_symbols l)
  | Str.Delim "," :: l -> Com :: (map_prolog_symbols l)
  | Str.Delim "." :: l -> Do :: (map_prolog_symbols l)
  | Str.Delim "[" :: l -> LB :: (map_prolog_symbols l)
  | Str.Delim "]" :: l -> RB :: (map_prolog_symbols l)
  | Str.Text s :: l -> T s :: (map_prolog_symbols l)
  | Str.Delim s :: _ -> failwith ("map_prolog_symbols: " ^ s)
  | [] -> []

let rec string_of_prolog_symbol = function
    T s -> "T \"" ^ s ^ "\""
  | C s -> "C \"" ^ s ^ "\""
  | LP -> "LP"
  | RP -> "RP"
  | Com -> "Com"
  | Do -> "Do"
  | LB -> "LB"
  | RB -> "RB"
  | Te (s,l) -> "Te(" ^ s ^ ",[" ^ (String.concat "," (Xlist.map l string_of_prolog_symbol)) ^ "])"
  | Li l -> "Li[" ^ (String.concat "," (Xlist.map l string_of_prolog_symbol)) ^ "]"

let rec recognize_prolog_term = function
    T s :: LP :: l ->
      let args,l = recognize_prolog_term_list [] l in
      Te(s,args),l
  | C s :: LP :: l ->
      let args,l = recognize_prolog_term_list [] l in
      Te(s,args),l
  | C s :: Com :: l -> C s, Com :: l
  | T s :: Com :: l -> T s, Com :: l
  | C s :: RP :: l -> C s, RP :: l
  | T s :: RP :: l -> T s, RP :: l
  | C s :: RB :: l -> C s, RB :: l
  | T s :: RB :: l -> T s, RB :: l
  | LB :: l ->
      let args,l = recognize_prolog_term_list2 [] l in
      Li args,l
  | l -> failwith ("recognize_prolog_term: " ^ (String.concat " " (Xlist.map l string_of_prolog_symbol)))

and recognize_prolog_term_list rev = function
    RP :: l -> List.rev rev, l
  | Com :: l -> recognize_prolog_term_list rev l
  | l -> let t,l = recognize_prolog_term l in recognize_prolog_term_list (t :: rev) l

and recognize_prolog_term_list2 rev = function
    RB :: l -> List.rev rev, l
  | Com :: l -> recognize_prolog_term_list2 rev l
  | l -> let t,l = recognize_prolog_term l in recognize_prolog_term_list2 (t :: rev) l

let process_prolog_properties p = function
    Te("sentence_id",[C id]) :: Te("markup_free_sentence",[C t]) :: l ->
       if p.p_sentence = t then {p with p_id=id} else failwith ("process_prolog_properties: " ^ p.p_sentence ^ "  " ^ t)
  | Te("markup_free_sentence",[C t]) :: l ->
       if p.p_sentence = t then p else failwith ("process_prolog_properties: " ^ p.p_sentence ^ "  " ^ t)
  | _ -> failwith "process_prolog_properties"

let rec split_cvar_rec n s =
  if String.get s n >= 'A' && String.get s n <= 'Z' then
    split_cvar_rec (n+1) s
  else n

let rec check_is_number n s =
  if String.length s <= n then () else
  if String.get s n >= '0' && String.get s n <= '9' then
    check_is_number (n+1) s
  else failwith "check_is_number"

let split_cvar s =
  if String.length s > 3 && String.sub s 0 3 = "CV_" then (
    check_is_number 3 s;
    CDef s)
  else (
    let n = split_cvar_rec 0 s in
    check_is_number n s;
    CVar(String.sub s 0 n, String.sub s n (String.length s - n)))

let rec process_prolog_context = function
  | T "1" -> CEmpty
  | T v -> split_cvar v
  | Te("or",l) -> COr(Xlist.map l process_prolog_context)
  | t -> failwith ("process_prolog_context: " ^ string_of_prolog_symbol t)

let process_prolog_choices p = function
    Te("choice",[Li l;c]) -> {p with p_choices = (Xlist.map l process_prolog_context,process_prolog_context c) :: p.p_choices}
  | t -> failwith ("process_prolog_choices: " ^ string_of_prolog_symbol t)

let process_prolog_equivalences p = function
    Te("define",[T v;t]) -> {p with p_defines = StringMap.add_inc p.p_defines v (process_prolog_context t)
       (fun _ -> failwith ("process_prolog_equivalences: " ^ v))}
  | Te("select",_) -> p
  | t -> failwith ("process_prolog_equivalences: " ^ string_of_prolog_symbol t)

let parse_prolog_var = function
    Te("var",[T v]) -> LVar v
  | C "NULL" -> Cons (StringMap.empty,"NULL")
  | t -> failwith ("parse_prolog_var: " ^ string_of_prolog_symbol t)

let process_prolog_constraint_term = function
    Te("var",[T v]) -> LVar v
  | C s -> Cons(StringMap.empty,s)
  | Te("semform",[C s;T i;Li a;Li b]) -> QCons(StringMap.empty,s,i,Xlist.map a parse_prolog_var,Xlist.map b parse_prolog_var,0,0)
  | t -> failwith ("process_prolog_constraint_term: " ^ string_of_prolog_symbol t)

let process_prolog_constraint c p = function
    Te("in_set",[Te("var",[T v]);Te("var",[T w])]) -> {p with p_in_sets = StringMap.add_inc p.p_in_sets w [c,v] (fun l -> (c,v) :: l)}
  | Te("eq",[Te("var",[T v]);Te("var",[T w])]) -> {p with p_equi =
       let equi = StringMap.add_inc p.p_equi w [c,v] (fun l -> (c,v) :: l) in
       StringMap.add_inc equi v [c,w] (fun l -> (c,w) :: l)}
  | Te("eq",[Te("var",[T v]);t]) -> {p with p_constraints = StringMap.add_inc p.p_constraints v
          [c,process_prolog_constraint_term t] (fun l -> (c,process_prolog_constraint_term t) :: l)}
  | Te("eq",[Te("attr",[Te("var",[T v]);C s]);t]) -> {p with p_subfields = StringMap.add_inc p.p_subfields v
          [c,(s,process_prolog_constraint_term t)] (fun l -> (c,(s,process_prolog_constraint_term t)) :: l)}
  | Te("subsume",[Te("var",[T v]);t]) -> {p with p_subsumes = (c,(v,process_prolog_constraint_term t)) :: p.p_subsumes}
  | t -> failwith ("process_prolog_constraint: " ^ string_of_prolog_symbol t)

let process_prolog_constraints p = function
  | Te("cf",[c;t]) -> process_prolog_constraint (process_prolog_context c) p t
  | t -> failwith ("process_prolog_constraints: " ^ string_of_prolog_symbol t)

let process_prolog_node = function
    T v -> v
  | _ -> failwith "process_prolog_node"

let process_prolog_cstructure2 c p = function
    (* Trees are represented by subtree(mother,label,partial,complete), where 'mother' is the mother constituent,
        'label' is the label of the mother, 'complete' is the right daughter, and 'partial' is a new constituent
         that represents all of the daughters to the left of the right daughter. For consistency,
         we always have a partial edge even if there is only one left daughter. *)
    Te("subtree",[T mother;C label;T partial;T complete]) -> {p with p_subtree = (c,(mother,label,partial,complete)) :: p.p_subtree}
    (* Terminal nodes are represented as having a node id, a label, and a list of token ids that they map to. *)
  | Te("terminal",[T node;C label;Li l]) -> {p with p_terminal = (c,(node,label,Xlist.map l process_prolog_node)) :: p.p_terminal}
    (* The phi projection is represented using a separate predicate. *)
  | Te("phi",[T node;Te("var",[T v])]) -> {p with p_phi = (c,(node,v)) :: p.p_phi}
    (* A semform_data predicate has four arguments: the lexical id for a semantic form, a node id for the node that
      the semantic form was instantiated in, the left input position of the surfaceform corresponding to the node,
      and the right input position of the surfaceform corresponding to the node. If the node does not have a corresponding
      surfaceform (e.g. a null_pro), the left input position and the right input position is the position of the left edge of the node.
      Note that the node that a semantic form is instantiated in is often a morpheme rather than a surface form,
      so the input positions may be a subset of the input positions of the surface form that the lexical id came from. *)
  | Te("semform_data",[T id;T node;T left;T right]) -> {p with p_semform_data =
         let x = c,(node,
           (try int_of_string left with _ -> failwith "process_prolog_cstructure2"),
            try int_of_string right with _ -> failwith "process_prolog_cstructure2") in
         StringMap.add_inc p.p_semform_data id [x] (fun l -> x :: l)}
    (* An fspan predicate represents the span of the input string that an f-structure covers. An f-structure can have more than
       one fspan if the f-structure is discontinuous. An fspan predicate has three arguments: a var, the left input position
       of the var, and the right input position of the var. *)
  | Te("fspan",[Te("var",[T v]);T left;T right]) -> {p with p_fspan = (c,(v,left,right)) :: p.p_fspan}
    (* Surface forms (e.g. tokens) are represented as by the surfaceform predicate. The surfaceform predicate has
       a node id, a label, and a left and right input position in the input string *)
  | Te("surfaceform",[T node;C label;T left;T right]) -> {p with p_surfaceform = (c,(node,label,
           (try int_of_string left with _ -> failwith "process_prolog_cstructure2"),
            try int_of_string right with _ -> failwith "process_prolog_cstructure2")) :: p.p_surfaceform}
  | t -> failwith ("process_prolog_cstructure: " ^ string_of_prolog_symbol t)

let process_prolog_cstructure p = function
  | Te("cf",[c;t]) -> process_prolog_cstructure2 (process_prolog_context c) p t
  | t -> failwith ("process_prolog_cstructure: " ^ string_of_prolog_symbol t)

let process_prolog_graph s =
(*   print_endline "process_prolog_graph 1"; *)
  let l = eliminate_comments [] (Str.full_split regexp2 s) in
(*   print_endline "process_prolog_graph 2"; *)
  let l = List.flatten (List.rev (Xlist.rev_map l (function
      T s -> map_prolog_symbols (Str.full_split regexp3 s)
    | x -> [x]))) in
(*   print_endline "process_prolog_graph 3"; *)
  let t,l = recognize_prolog_term l in
(*   print_endline "process_prolog_graph 4"; *)
  if l <> [Do] then failwith "process_prolog_graph" else
  match t with
    Te("fstructure",[C s;Li properties; Li choices; Li equivalences; Li constraints; Li cstructure]) ->
(*   print_endline "process_prolog_graph 5"; *)
      let p = {p_sentence=s; p_id=""; p_choices=[]; p_defines=StringMap.empty;
               p_in_sets=StringMap.empty; p_equi=StringMap.empty; p_constraints=StringMap.empty; p_subfields=StringMap.empty; p_subsumes=[];
               p_subtree=[]; p_phi=[]; p_terminal=[]; p_semform_data=StringMap.empty; p_fspan=[]; p_surfaceform=[]} in
(*   print_endline "process_prolog_graph 6"; *)
      let p = process_prolog_properties p properties in
(*   print_endline "process_prolog_graph 7"; *)
      let p = Xlist.fold choices p process_prolog_choices in
(*   print_endline "process_prolog_graph 8"; *)
      let p = {p with p_choices = List.rev p.p_choices} in
(*   print_endline "process_prolog_graph 9"; *)
      let p = Xlist.fold equivalences p process_prolog_equivalences in
(*   print_endline "process_prolog_graph 10"; *)
      let p = Xlist.fold constraints p process_prolog_constraints in
(*   print_endline "process_prolog_graph 11"; *)
      Xlist.fold cstructure p process_prolog_cstructure
  | t -> failwith ("process_prolog_graph: " ^ string_of_prolog_symbol t)

let select_context2 cvar_map path map =
  StringMap.fold map StringMap.empty (fun map k l ->
    let l = Xlist.fold l [] (fun l (c,v) ->
      if XTContext.is_active cvar_map path c then (CEmpty,v) :: l else l) in
    if l = [] then map else StringMap.add map k l)

let select_context cvar_map path p =
  {p with p_in_sets=select_context2 cvar_map path p.p_in_sets;
          p_equi=select_context2 cvar_map path p.p_equi;
          p_constraints=select_context2 cvar_map path p.p_constraints;
          p_subfields=select_context2 cvar_map path p.p_subfields}

let model_context2 cvar_map map =
  StringMap.map map (fun  l ->
    Xlist.map l (fun (c,v) -> XTContext.create_model cvar_map c, v))

let model_context cvar_map p =
  {p with p_in_sets=model_context2 cvar_map p.p_in_sets;
          p_equi=model_context2 cvar_map p.p_equi;
          p_constraints=model_context2 cvar_map p.p_constraints;
          p_subfields=model_context2 cvar_map p.p_subfields}
