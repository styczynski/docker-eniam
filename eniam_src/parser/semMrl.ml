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
 
open Xstd  
open SemTypes
open LCGtypes


let idnum = ref 0 
 
let get_id () =  
  incr idnum; 
  !idnum
 
let make_handle () = "h" ^ string_of_int (get_id ())
 
(* 
 
let rec extract_nth n rev = function
    [] -> failwith "extract_nth"
  | s :: l -> 
      if n = 1 then s, (List.rev rev) @ l 
      else extract_nth (n-1) (s :: rev) l
  
let rec merge_quant_arg = function
    Variable v -> TArg v
  | Term("variant",l) -> TWith(Xlist.map l merge_quant_arg)
  | s -> failwith ("merge_quant_arg: " ^ StringOf.mrl_term s)
  
let merge_quant = function
(*    TConst("",[]),TConst("",[]) -> TConst("",[])
  | TConst("",[]),a -> a
  | a, TConst("",[]) -> a
  | a, TConst("local",[]) -> a
  | TConst("local",[]),a -> a
  | a, b -> TMod(a,b)*)
    Pred("type",[_;Term(s,l)]),TConst("",[]) -> TConst(s,Xlist.map l merge_quant_arg)
  | Pred("type",[_;Term(s,l)]),TConst("local",[]) -> TConst(s,Xlist.map l merge_quant_arg)
  | Pred("type",[_;Term("mod",[Term(s1,l1);Term(s2,l2)])]),x -> TMod(TMod(TConst(s1,Xlist.map l1 merge_quant_arg),TConst(s2,Xlist.map l2 merge_quant_arg)),x)
  | Pred("type",[_;Term(s,l)]),x -> TMod(TConst(s,Xlist.map l merge_quant_arg),x)
  | _ -> failwith "merge_quant"
  
  *)
(*************************************************************************)

let get_variable (id,_) = function
    Val s, _ -> id,s
  | Dot, Val s -> id, s
  | Dot, Dot -> id, ""
  | _ -> failwith "get_variable"

let rec get_variable_list rev = function
    Concept c -> let x = get_variable c.c_variable (c.c_sense, c.c_name) in get_variable_list (x :: rev) c.c_relations
  | Context c -> let x = get_variable c.cx_variable (c.cx_sense, Dot) in get_variable_list (get_variable_list (x :: rev) c.cx_relations) c.cx_contents
  | Relation(r,a,t) -> get_variable_list rev t
  | RevRelation(r,a,t) -> get_variable_list rev t
  | SingleRelation r  -> rev
  | Tuple l -> Xlist.fold l rev get_variable_list
  | Variant(e,l) -> Xlist.fold l rev (fun rev (_,t) ->  get_variable_list rev t)
  | Dot -> rev
  | Val s -> rev
  | t -> failwith ("get_variable_list: " ^ LCGstringOf.linear_term 0 t)

let make_variable_name t =
  if t = "" then "x" else
  let c = String.get t 0 in
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then String.sub t 0 1 else
  if c >= '0' && c <= '9' then "q" else "y"
    
let replace_variable map id =
  try StringMap.find map id with Not_found -> failwith "replace_variable"
    
let rec replace_variables map = function
    Concept c -> Concept{c with c_variable=replace_variable map (fst c.c_variable); c_relations=replace_variables map c.c_relations}
  | Context c -> Context{c with cx_variable=replace_variable map (fst c.cx_variable); cx_contents=replace_variables map c.cx_contents; cx_relations=replace_variables map c.cx_relations}
  | Relation(r,a,t) -> Relation(r,a,replace_variables map t)
  | RevRelation(r,a,t) -> RevRelation(r,a,replace_variables map t)
  | SingleRelation r  -> SingleRelation r
  | Tuple l -> Tuple(Xlist.map l (replace_variables map))
  | Variant(e,l) -> Variant(e,Xlist.map l (fun (i,t) -> i, replace_variables map t))
  | Dot -> Dot
  | Val s -> Val s
  | t -> failwith ("replace_variables: " ^ LCGstringOf.linear_term 0 t)
        
let variable_alpha_convertion t = 
  let l = get_variable_list [] t in
  let map = Xlist.fold l StringMap.empty (fun map (v,t) -> 
    let t = make_variable_name t in
    StringMap.add_inc map v t (function "x" -> t | s -> s)) in
  let bucket = StringMap.fold map StringMap.empty (fun bucket v t ->
    StringMap.add_inc bucket t [v] (fun l -> v :: l)) in
  let map = StringMap.fold bucket StringMap.empty (fun map t l ->
    let map = StringMap.add map (List.hd l) (t,"") in
    fst (Xlist.fold (List.tl l) (map,2) (fun (map,n) v ->
      StringMap.add map v (t,string_of_int n), n+1))) in   
  replace_variables map t

(*************************************************************************)
  
let simplify_conj = function
    Conj [] -> True
  | Conj [t] -> t
  | f -> f

let simplify_disj = function
    Disj [] -> Neg True
  | Disj [t] -> t
  | f -> f

let rec make_mrl_local_predicates_term_arg = function
    TArg s -> Variable("\\#" ^ s,"")
  | TWith l -> Term("variant",Xlist.map l make_mrl_local_predicates_term_arg)
  
let rec make_mrl_local_predicates_term = function
    TConst(t,l) -> Term(t,Xlist.map l make_mrl_local_predicates_term_arg)
  | TMod(s,t) -> Term("mod",[make_mrl_local_predicates_term s;make_mrl_local_predicates_term t])
  | _ -> failwith "make_mrl_local_predicates_term"

let make_local_predicates v sense name =
  (match sense with
    Dot -> []
  | Val s -> [Pred("type",[Variable v;Term(s,[])])]
  | _ -> failwith "make_local_predicates") @
  (match name with
    Dot -> []
  | Val s -> [Pred("hasName",[Variable v;String s])]
  | _ -> failwith "make_local_predicates") 
  
let rec sort_quant_rec = function
    Dot -> []
  | Val s -> [s]
  | Tuple l -> List.flatten (Xlist.map l sort_quant_rec)
  | t -> failwith "sort_quant_rec"
  
let sort_quant t =   
  List.sort compare (sort_quant_rec t)
  
let binary_quant v t sub sup local pos quant =
  if local then quant (simplify_conj (Conj(t @ sub))) (simplify_conj (Conj sup)) else
  Seam(Position(pos,quant (simplify_conj (Conj(t @ sub))) (Seam(simplify_conj (Conj sup)))))
  
let unary_quant v t sub local pos quant =
  if local then quant (simplify_conj (Conj(t @ sub))) else
  Seam(Position(pos,quant (simplify_conj (Conj(t @ sub)))))
  
let rec make_mrl_binary_quantifier v t sub sup local pos = function
    [],Dot -> binary_quant v t sub sup local pos (fun x y -> Exist2(v,x,y))
  | ["interrogative"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("interrogative",[]),v,x,y))
  | ["deictic"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("deictic",[]),v,x,y))
  | ["coreferential"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("coreferential",[]),v,x,y))
  | ["indexical"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("indexical",[]),v,x,y))
  | ["order"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("order",[]),v,x,y))
  | ["comparative"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("comparative",[]),v,x,y))
  | ["nie"],Dot -> binary_quant v t sub sup local pos (fun x y -> Neg(Exist2(v,x,y)))
  | ["mass";"sg"],q ->  make_mrl_binary_quantifier v t sub sup local pos ([],q)
(*   | ["mass"],Dot -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst("",[]),v,x,y)) *)
  | ["sg"],Dot -> binary_quant v (t @ [Equal(Term("count",[Variable v]),Term("1",[]))]) sub sup local pos (fun x y -> Exist2(v,x,y))
  | ["pl"],Dot -> binary_quant v (t @ [Greater(Term("count",[Variable v]),Term("1",[]))]) sub sup local pos (fun x y -> Exist2(v,x,y))
  | ["sg"],Concept{c_sense=Val s; c_name=Dot; c_quant=Dot; c_relations=Dot} -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst(s,[]),v,x,y))
  | [],Concept{c_sense=Val s; c_name=Dot; c_quant=Dot; c_relations=Dot} -> binary_quant v t sub sup local pos (fun x y -> Quant2(TConst(s,[]),v,x,y))
(*  | TMod(TConst("exact",[]), TConst(n,[])) ->
      let restr = simplify_conj (Conj(t @ [Equal(Term("count",[Variable v]),Term(n,[]))] @ sub)) in
      Seam (Exist2(v,restr, Seam (simplify_conj (Conj sup))))
  | TMod(TConst("approx",[]), s) ->
      let restr = simplify_conj (Conj(t @ [Pred("count",[Variable v;make_mrl_local_predicates_term s])] @ sub)) in
      Seam (Exist2(v,restr, Seam (simplify_conj (Conj sup))))
  | TMod(TConst("exact",[]), s) ->
      let restr = simplify_conj (Conj(t @ [Pred("count",[Variable v;make_mrl_local_predicates_term s])] @ sub)) in
      Seam (Exist2(v,restr, Seam (simplify_conj (Conj sup))))*)
  | l,q2 -> (*failwith*)(*print_endline ("make_mrl_binary_quantifier: " ^ 
                    String.concat " " l ^ " " ^ LCGstringOf.linear_term 0 q2); *)
      binary_quant v t sub sup local pos (fun x y -> Exist2(v,x,y))
         
let rec make_mrl_unary_quantifier v t sub local pos = function
    [],Dot -> unary_quant v t sub local pos (fun x -> Exist1(v,x))
  | ["interrogative"],Dot -> unary_quant v t sub local pos (fun x -> Quant1(TConst("interrogative",[]),v,x))
  | ["deictic"],Dot -> unary_quant v t sub local pos (fun x -> Quant1(TConst("deictic",[]),v,x))
  | ["coreferential"],Dot -> unary_quant v t sub local pos (fun x -> Quant1(TConst("coreferential",[]),v,x))
  | ["indexical"],Dot -> unary_quant v t sub local pos (fun x -> Quant1(TConst("indexical",[]),v,x))
  | ["order"],Dot -> unary_quant v t sub local pos (fun x -> Quant1(TConst("order",[]),v,x))
  | ["comparative"],Dot -> unary_quant v t sub local pos (fun x -> Quant1(TConst("comparative",[]),v,x))
  | ["nie"],Dot -> unary_quant v t sub local pos (fun x -> Neg(Exist1(v,x)))
  | ["mass";"sg"],q ->  make_mrl_unary_quantifier v t sub local pos ([],q)
  | ["sg"],Dot -> unary_quant v (t @ [Equal(Term("count",[Variable v]),Term("1",[]))]) sub local pos (fun x -> Exist1(v,x))
  | ["pl"],Dot -> unary_quant v (t @ [Greater(Term("count",[Variable v]),Term("1",[]))]) sub local pos (fun x -> Exist1(v,x))
  | ["sg"],Concept{c_sense=Val s; c_name=Dot; c_quant=Dot; c_relations=Dot} -> unary_quant v t sub local pos (fun x -> Quant1(TConst(s,[]),v,x))
  | [],Concept{c_sense=Val s; c_name=Dot; c_quant=Dot; c_relations=Dot} -> unary_quant v t sub local pos (fun x -> Quant1(TConst(s,[]),v,x))
  | l,q2 -> (*failwith*)(*print_endline ("make_mrl_unary_quantifier: " ^ 
                    String.concat " " l ^ " " ^ LCGstringOf.linear_term 0 q2); *)
      unary_quant v t sub local pos (fun x -> Exist1(v,x))

(*let make_mrl_modalities f l = 
  let focus,syntax = Xlist.fold l ([],[]) (fun (focus,syntax) -> function
      v, TMod(t,TConst("syntax",[])) -> focus, (v,t) :: syntax
    | v, TMod(t,TConst("focus",[])) -> (v,t) :: focus, syntax
    | _ -> failwith "make_mrl_modalities") in
  let f = 
    if syntax = [] then f else
    let v = fst (List.hd syntax) in
    Seam (Exist2(v,simplify_conj (Conj(Xlist.map syntax (fun (_,t) -> 
      Pred("type",[Variable v;make_mrl_local_predicates_term t])))), Dscr(Variable v, Cut f))) in
  Xlist.fold focus f (fun f (v,t) -> 
    Seam (Exist2(v, Pred("type",[Variable v;make_mrl_local_predicates_term t]), Dscr(Variable v, f))))*)
      
let get_variable = function
    Concept c -> c.c_variable
  | Context c -> c.cx_variable
(*   | ConceptList c -> c.cl_variable *)
  | (*LT*) _ -> failwith "get_variable"
      
let make_rel_name = function
    Val r, Val "" -> r
  | Val r, Dot -> r
  | Val r, Val a -> r ^ (*"-" ^*) a
  | _ -> failwith "make_rel_name"

let rec extract_quantifier_rec (quant,relations) = function
    Relation(Val "Quantifier",Val "",t)  -> t :: quant,relations
  | Relation _ as t  -> quant,t :: relations
  | RevRelation _ as t -> quant,t :: relations
  | SingleRelation _ as t -> quant,t :: relations
  | Tuple l -> Xlist.fold l (quant,relations) extract_quantifier_rec
  | Dot -> quant,relations
  | t -> failwith ("extract_quantifier_rec: " ^ LCGstringOf.linear_term 0 t)
  
let extract_quantifier l =
  let quant,relations = extract_quantifier_rec ([],[]) l in
  (match quant with
    [] -> Dot
  | [t] -> t
  | _ -> failwith "extract_quantifier"),
  Tuple relations
  
let concat_var (v,sub) = v ^ "_" ^ sub
  
let rec make_mrl = function
    Context c -> make_mrl_concept [] (Context c) 
  | t -> failwith ("make_mrl: " ^ LCGstringOf.linear_term 0 t)

and make_mrl_relations v = function
    Relation(r,a,t)  ->  [make_mrl_concept [Requires(StringSet.singleton (concat_var v),Pred(make_rel_name (r,a),[Variable v;Variable (get_variable t)]))] t]
  | RevRelation(r,a,t)  -> [make_mrl_concept [Requires(StringSet.singleton (concat_var v),Pred(make_rel_name (r,a),[Variable (get_variable t);Variable v]))] t]
  | SingleRelation r -> [Requires(StringSet.singleton (concat_var v),Pred(make_rel_name (r,Dot),[Variable v]))]
  | Tuple l -> List.flatten (Xlist.map l (make_mrl_relations v))
  | Dot -> []
  | t -> failwith ("make_mrl_relations: " ^ LCGstringOf.linear_term 0 t)
  
and make_mrl_concept rels = function
    Concept c -> 
      let quant,relations = extract_quantifier c.c_relations in
      let sub_formulae = make_mrl_relations c.c_variable relations in
      let local_predicates = make_local_predicates c.c_variable c.c_sense c.c_name in
      if rels = [] then make_mrl_unary_quantifier c.c_variable local_predicates sub_formulae c.c_local_quant c.c_pos (sort_quant c.c_quant,quant)
        else make_mrl_binary_quantifier c.c_variable local_predicates sub_formulae rels c.c_local_quant c.c_pos (sort_quant c.c_quant,quant)
  | Context c -> 
      let sub_formulae = make_mrl_relations c.cx_variable c.cx_relations in
      let contents = simplify_conj (Conj (make_mrl_contents c.cx_contents)) in
      let dscr = Dscr(Variable c.cx_variable, contents) in
      let sub_formulae = dscr :: sub_formulae in
      let local_predicates = make_local_predicates c.cx_variable c.cx_sense Dot in
      if rels = [] then make_mrl_unary_quantifier c.cx_variable local_predicates sub_formulae true 0 ([],Dot)
      else make_mrl_binary_quantifier c.cx_variable local_predicates sub_formulae rels true 0 ([],Dot)
  | t -> failwith ("make_mrl_concept: " ^ LCGstringOf.linear_term 0 t)

and make_mrl_contents = function
    Concept c -> [make_mrl_concept [] (Concept c)]
  | Context c -> [make_mrl_concept [] (Context c)]
  | Tuple l -> List.flatten (Xlist.map l make_mrl_contents)
  | t -> failwith ("make_mrl_contents: " ^ LCGstringOf.linear_term 0 t)

(*************************************************************************)
  
(* FIXME: w mrs 
- brak uchwytu dla Root 
- problem z brakiem alfa konwersji przy kontekstach
*)
    
let rec move_requirements_var v f =
  match move_requirements f with
    Requires(req,f) -> 
      let req = StringSet.remove req (concat_var v) in
      f,req
  | f -> f,StringSet.empty
  
and move_requirements = function 
    Conj l -> 
      let l,req = Xlist.fold l ([],StringSet.empty) (fun (l,req) f ->
        match move_requirements f with
          Requires(v,f) -> f :: l, StringSet.union v req
        | f -> f :: l,req) in
      if StringSet.is_empty req then Conj(List.rev l) else Requires(req,Conj(List.rev l))
  | Disj l -> 
      let l,req = Xlist.fold l ([],StringSet.empty) (fun (l,req) f ->
        match move_requirements f with
          Requires(v,f) -> f :: l, StringSet.union v req
        | f -> f :: l,req) in
      if StringSet.is_empty req then Disj(List.rev l) else Requires(req,Disj(List.rev l))
  | Neg f -> 
      (match move_requirements f with
        Requires(v,f) -> Requires(v,Neg f)
      | f -> Neg f)
  | Exist(v,t,f) -> failwith "move_requirements: ni"
  | Exist1(v,f) -> 
      let f,req = move_requirements_var v f in
      if StringSet.is_empty req then Exist1(v,f) else Requires(req,Exist1(v,f))
  | Exist2(v,t,f) -> 
      let t,req = move_requirements_var v t in
      let f,req2 = move_requirements_var v f in
      let req = StringSet.union req req2 in
      if StringSet.is_empty req then Exist2(v,t,f) else Requires(req,Exist2(v,t,f))
  | ForAll(v,t,f) -> 
      let t,req = move_requirements_var v t in
      let f,req2 = move_requirements_var v f in
      let req = StringSet.union req req2 in
      if StringSet.is_empty req then ForAll(v,t,f) else Requires(req,ForAll(v,t,f))
  | Quant1(s,v,f) -> 
      let f,req = move_requirements_var v f in
      if StringSet.is_empty req then Quant1(s,v,f) else Requires(req,Quant1(s,v,f))
  | Quant2(s,v,t,f) -> 
      let t,req = move_requirements_var v t in
      let f,req2 = move_requirements_var v f in
      let req = StringSet.union req req2 in
      if StringSet.is_empty req then Quant2(s,v,t,f) else Requires(req,Quant2(s,v,t,f))
  | Dscr(t,f) -> 
      (match move_requirements f with
        Requires(v,f) -> Requires(v,Dscr(t,f))
      | f -> Dscr(t,f))
  | Pred(_,_) as f -> f
  | Equal(_,_) as f -> f
  | Greater(_,_) as f -> f
  | True as f -> f
  | Handle _ -> failwith "move_requirements"
  | Seam f -> Seam(move_requirements f) 
  | Requires(v,f) -> 
      (match move_requirements f with
        Requires(w,f) -> Requires(StringSet.union v w,f)
      | f -> Requires(v,f))
  | Cut f -> 
      (match move_requirements f with
        Requires(v,f) -> Requires(v,Cut f)
      | f -> Cut f)
  | Context _ as f -> f
  | Position(i,f) -> 
      (match move_requirements f with
        Requires(v,f) -> Requires(v,Position(i,f))
      | f -> Position(i,f))
  
(*************************************************************************)
  
let rec make_mrs_of_mrl_formula gaps new_gaps mrs = function 
    Conj l -> 
      let l,mrs = Xlist.fold l ([],mrs) (fun (l,mrs) t -> 
        let t,mrs = make_mrs_of_mrl_formula gaps new_gaps mrs t in t :: l, mrs) in
      Conj(List.rev l),mrs
  | Disj l -> 
      let l,mrs = Xlist.fold l ([],mrs) (fun (l,mrs) t -> 
        let t,mrs = make_mrs_of_mrl_formula gaps new_gaps mrs t in t :: l, mrs) in
      Disj(List.rev l),mrs
  | Neg f -> let f,mrs = make_mrs_of_mrl_formula gaps new_gaps mrs f in Neg f, mrs
  | Exist(v,t,f) -> failwith "make_mrs_of_mrl_formula 4"
  | Exist1(v,f) -> 
      let f,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs f in Exist1(v,f), mrs
  | Exist2(v,t,f) -> 
      let t,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs t in
      let f,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs f in
      Exist2(v,t,f),mrs
  | ForAll(v,t,f) -> 
      let t,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs t in
      let f,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs f in
      ForAll(v,t,f),mrs
  | Quant1(s,v,f) -> 
      let f,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs f in Quant1(s,v,f), mrs
  | Quant2(s,v,t,f) -> 
      let t,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs t in
      let f,mrs = make_mrs_of_mrl_formula gaps (v :: new_gaps) mrs f in
      Quant2(s,v,t,f),mrs
  | Dscr(t,f) -> 
      let f,mrs = make_mrs_of_mrl_formula gaps new_gaps mrs f in Dscr(t,f), mrs
  | Pred(_,_) as f -> f,mrs
  | Equal(_,_) as f -> f,mrs
  | Greater(_,_) as f -> f,mrs
  | True as f -> f,mrs
  | Handle _ -> failwith "make_mrs_of_mrl_formula 1"
  | Requires(v,f) -> failwith "make_mrs_of_mrl_formula 2"
(*       let f,mrs = make_mrs_of_mrl_formula gaps new_gaps mrs f in Requires(v,f), mrs *)
  | Seam(Requires(v,f)) -> 
      let label = make_handle () in
      let handle = make_handle () in
      let gaps = Xlist.fold new_gaps gaps (fun gaps v -> StringMap.add gaps (concat_var v) handle) in
      let mrs = StringSet.fold v mrs (fun mrs v -> 
        let gap = try StringMap.find gaps v with Not_found -> failwith "make_mrs_of_mrl_formula 3" in
        MRSqeq(gap,label) :: mrs) in
      let f,mrs = make_mrs_of_mrl_formula gaps [] mrs f in
      let f,mrs = match f with 
          Position(pos,f) -> f, MRSpos(label,pos) :: mrs
        | _ -> f,mrs in
      Handle handle, MRSeps(label,f) :: mrs
  | Seam f -> 
      let label = make_handle () in
      let handle = make_handle () in
      let gaps = Xlist.fold new_gaps gaps (fun gaps v -> StringMap.add gaps (concat_var v) handle) in
      let f,mrs = make_mrs_of_mrl_formula gaps [] mrs f in
      let f,mrs = match f with 
          Position(pos,f) -> f, MRSpos(label,pos) :: mrs
        | _ -> f,mrs in
      Handle handle, MRSeps(label,f) :: mrs
  | Cut f -> make_mrs_of_mrl_formula gaps new_gaps mrs f
  | Context _ as f -> f,mrs
  | Position(pos,f) -> 
      let f,mrs = make_mrs_of_mrl_formula gaps new_gaps mrs f in Position(pos,f), mrs
  
(* TODO!!! 
- dodać odwołanie do htop i implementacje Cut
- dodać tłumaczenie kwantyfikatorów 
- usunięcie zewnętrznych konteksow sytuacyjnych
- dodanie Cut do kontekstów spójników podrzędnych
- wyróżnianie dystrybutywności
- wyróżnianie interrogative i nazw władnych przestawianych na początek
- ujednolicenie 'ja' w imp i #i 

na potem:
- parsowanie niejednoznaczych fstruktur
*)

let make_mrs_of_mrl f = 
  let f,mrs = make_mrs_of_mrl_formula StringMap.empty [] [] f in
  let label = make_handle () in
  MRStop label :: MRSeps(label,f) :: mrs
    
(*************************************************************************)
  
let sort_mrs l = 
  let a,b,c,d = Xlist.fold l ([],[],[],[]) (fun (a,b,c,d) -> function 
    MRSeps(h,f) -> a,MRSeps(h,f) :: b,c,d
  | MRSqeq(h,h2) -> a,b,MRSqeq(h,h2) :: c,d
  | MRStop h -> MRStop h :: a,b,c,d
  | MRSpos(h,pos) -> a,b,c,MRSpos(h,pos) :: d) in
  List.rev a,List.rev b,List.rev c
  
(*************************************************************************)
  
let rec get_handle_list_mrl_formula = function
    Conj l -> List.flatten (Xlist.map l get_handle_list_mrl_formula)
  | Disj l -> List.flatten (Xlist.map l get_handle_list_mrl_formula)
(*| Impl of mrl_formula * mrl_formula*)
  | Neg t -> get_handle_list_mrl_formula t
  | Exist _ -> failwith "get_handle_list_mrl_formula: ni"
  | Exist1(_,s) -> get_handle_list_mrl_formula s
  | Exist2(_,s,t) -> get_handle_list_mrl_formula s @ get_handle_list_mrl_formula t
  | ForAll(_,s,t) -> get_handle_list_mrl_formula s @ get_handle_list_mrl_formula t
  | Quant1(_,_,t) -> get_handle_list_mrl_formula t
  | Quant2(_,_,s,t) -> get_handle_list_mrl_formula s @ get_handle_list_mrl_formula t
  | Dscr(_,t) -> get_handle_list_mrl_formula t
  | Pred(_,_) -> []
(*   | Incl of term * term *)
  | Equal(_,_) -> []
  | Greater(_,_) -> []
  | True -> []
(*  | Impt of mrl_term  (* modalność rozkazująca *)
  | Qest of mrl_term  (* modalność pytająca *)
  | Stat of mrl_term  (* modalność oznajmująca *)*)
(*   | LTf t -> failwith "get_handle_list_mrl_formula" *)
  | Handle h -> [h]
  | Requires(_,s) -> get_handle_list_mrl_formula s
  | Seam s -> get_handle_list_mrl_formula s
  | Cut s -> get_handle_list_mrl_formula s
  | Context _ -> []
  | Position(_,t) -> get_handle_list_mrl_formula t
  
and get_handle_list_mrs_formula = function   
    MRSeps(h,f) -> h :: (get_handle_list_mrl_formula f)
  | MRSqeq(h,h2) -> [h;h2]
  | MRStop h -> [h]
  | MRSpos(h,_) -> [h]
  
let replace_handle map h =
  try StringMap.find map h with Not_found -> failwith ("replace_handle: " ^ h)
  
let rec replace_handles_mrl_formula map = function
    Conj l -> Conj (Xlist.map l (replace_handles_mrl_formula map))
  | Disj l -> Disj (Xlist.map l (replace_handles_mrl_formula map))
(*  | Impl (f,g) -> Impl (replace_handles_mrl_formula map f,replace_handles_mrl_formula map g)*)
  | Neg f -> Neg (replace_handles_mrl_formula map f)
  | Exist _ -> failwith "replace_handles_mrl_formula: ni"
  | Exist1(v,f) -> Exist1(v,replace_handles_mrl_formula map f)
  | Exist2(v,t,f) -> Exist2(v,replace_handles_mrl_formula map t,replace_handles_mrl_formula map f)
  | ForAll(v,t,f) -> ForAll(v,replace_handles_mrl_formula map t,replace_handles_mrl_formula map f)
  | Quant1(q,v,f) -> Quant1(q,v,replace_handles_mrl_formula map f)
  | Quant2(q,v,t,f) -> Quant2(q,v,replace_handles_mrl_formula map t,replace_handles_mrl_formula map f)
  | Dscr(t,f) -> Dscr(t,replace_handles_mrl_formula map f)
  | Pred(c,l) -> Pred(c,l)
  | Equal(s,t) -> Equal(s,t)
  | Greater(s,t) -> Greater(s,t)
  | True -> True
  | Handle h -> Handle(replace_handle map h)
  | Requires(v,t) -> Requires(v,replace_handles_mrl_formula map t)
  | Seam t -> Seam(replace_handles_mrl_formula map t)
  | Cut t -> Cut(replace_handles_mrl_formula map t)
  | Context c -> Context c
  | Position(pos,f) -> Position(pos,replace_handles_mrl_formula map f)
  
and replace_handles_mrs_formula map = function   
    MRSeps(h,f) -> MRSeps(replace_handle map h, replace_handles_mrl_formula map f)
  | MRSqeq(h,h2) -> MRSqeq(replace_handle map h, replace_handle map h2)
  | MRStop h -> MRStop(replace_handle map h)
  | MRSpos(h,pos) -> MRSpos(replace_handle map h,pos)
  
let mrs_handle_alpha_convertion t =
  let l = List.flatten (Xlist.map t get_handle_list_mrs_formula) in
  let map,_ = Xlist.fold l (StringMap.empty,0) (fun (map,n) x -> 
    if StringMap.mem map x then map,n else
    StringMap.add map x ("h" ^ string_of_int n), n+1) in
  Xlist.map t (replace_handles_mrs_formula map)
    
(*************************************************************************)
    
(* budujemy drzewo formuły od korzenia.
   uzgadnianie qeq: pamiętamy ścieżkę uchwytów przez które przechodziliśmy i mamy słownik qeq, przy wyborze maczowania sprawdzamy zgodność
   uzgadnianie zmiennych: 
     mamy listę zmiennych zdefiniowanych w zakresie węzła drzewa, 
     i słownik wskazujący jakie zmienne występują w formule
     przy wyborze maczowania sprawdzamy zgodność
   słownik wskazujący powiązania pomiędzy uchwytami. 
   drzewo formuły reprezentujemy jako zbiór uzgodnień 
   zbiory uzgodnień dla alternatywnych formuł pakujemy w drzewo decyzyjne *)

let rec create_foll_of_mrs_maps_mrl_term vl (vars,handles) h = function
  | Variable v -> 
(*       let p = try String.sub v 0 2 with _ -> "" in *)
      if Xlist.mem vl v (*|| p = "\\#"*) then vars,handles else StringMap.add_inc vars h [v] (fun l -> v :: l), handles
  | List l -> Xlist.fold l (vars,handles) (fun (vars,handles) t -> create_foll_of_mrs_maps_mrl_term vl (vars,handles) h t)
  | Indexical v -> vars,handles
  | String s -> vars,handles
  | Term(c,l) -> Xlist.fold l (vars,handles) (fun (vars,handles) t -> create_foll_of_mrs_maps_mrl_term vl (vars,handles) h t)

let rec create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h = function
    Conj l -> Xlist.fold l (vars,handles) (fun (vars,handles) t -> create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h t)
  | Disj l -> Xlist.fold l (vars,handles) (fun (vars,handles) t -> create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h t)
  | Neg f -> create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h f
  | Exist _ -> failwith "create_foll_of_mrs_maps_mrl_formula: ni"
  | Exist1(v,t) -> create_foll_of_mrs_maps_mrl_formula (v :: vl) (vars,handles) h t
(*   | Exist2(v,Handle h2,f) -> create_foll_of_mrs_maps_mrl_formula (v :: vl) (vars,StringMap.add_inc handles h [h2,v :: vl,true] (fun l -> (h2,v :: vl,true) :: l)) h f  *)
  | Exist2(v,t,f) -> create_foll_of_mrs_maps_mrl_formula (v :: vl) (create_foll_of_mrs_maps_mrl_formula (v :: vl) (vars,handles) h t) h f  
(*   | Exist2(v,t,f) -> failwith "create_foll_of_mrs_maps_mrl_formula: ni"  *)
  | ForAll(v,t,f) -> create_foll_of_mrs_maps_mrl_formula (v :: vl) (create_foll_of_mrs_maps_mrl_formula (v :: vl) (vars,handles) h t) h f
  | Quant1(_,v,f) -> create_foll_of_mrs_maps_mrl_formula (v :: vl) (vars,handles) h f
  | Quant2(_,v,t,f) -> create_foll_of_mrs_maps_mrl_formula (v :: vl) (create_foll_of_mrs_maps_mrl_formula (v :: vl) (vars,handles) h t) h f
  | Dscr(t,f) -> create_foll_of_mrs_maps_mrl_term vl (create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h f) h t
  | Pred(c,l) -> Xlist.fold l (vars,handles) (fun (vars,handles) t -> create_foll_of_mrs_maps_mrl_term vl (vars,handles) h t)
  | Equal(s,t) -> create_foll_of_mrs_maps_mrl_term vl (create_foll_of_mrs_maps_mrl_term vl (vars,handles) h s) h t
  | Greater(s,t) -> create_foll_of_mrs_maps_mrl_term vl (create_foll_of_mrs_maps_mrl_term vl (vars,handles) h s) h t
  | True -> vars,handles
  | Handle h2 -> vars,StringMap.add_inc handles h [h2,vl(*,false*)] (fun l -> (h2,vl(*,false*)) :: l)
  | Requires(_,f) -> create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h f
  | Seam f -> create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h f
  | Cut f -> create_foll_of_mrs_maps_mrl_formula vl (vars,handles) h f
  | Context _ -> vars,handles
  | Position _ -> failwith "create_foll_of_mrs_maps_mrl_formula: ni"
  
   
let create_foll_of_mrs_maps l = 
  let top,qeqs,vars,handles,hu,eps,positions = Xlist.fold l ([],StringMap.empty,StringMap.empty,StringMap.empty,StringSet.empty,StringMap.empty,StringMap.empty) 
    (fun (top,qeqs,vars,handles,hu,eps,positions) -> function 
      MRSeps(h,f) -> 
        let vars,handles = create_foll_of_mrs_maps_mrl_formula [] (vars,handles) h f in 
        let hu = StringSet.add hu h in
        top, qeqs, vars, handles, hu,  StringMap.add_inc eps h [f] (fun l -> f :: l), positions
    | MRSqeq(h,h2) -> top, StringMap.add_inc qeqs h2 [h] (fun l -> h :: l), vars, handles, hu,  eps, positions
    | MRStop h -> h :: top, qeqs, vars, handles, hu,  eps, positions
    | MRSpos(h,pos) -> top,qeqs,vars,handles,hu,eps,StringMap.add positions h pos) in
  match top with
    [t] -> t,qeqs,vars,handles,hu,eps,positions
  | _ -> failwith "create_foll_of_mrs_maps"
     
type decision_tree = 
    T of string * (string list * decision_tree) list
  | Tfinish
  | Terror

let rec string_of_decision_tree = function
    T(h,l) -> 
      "T(" ^ h ^ ",[" ^ String.concat "; " (Xlist.map l (fun (l,t) -> 
        "[" ^ String.concat ";" l ^ "], " ^ string_of_decision_tree t)) ^ "])"
  | Tfinish -> "Tfinish"
  | Terror -> "Terror"
  
  
let get_possible_matchings def_handles hu def_vars qeqs vars positions =
  let found = StringSet.fold hu [] (fun found h -> 
    let qeql = try StringMap.find qeqs h with Not_found -> [] in
    let varl = try StringMap.find vars h with Not_found -> [] in
(*     Printf.printf "  h=%s varl={%s}\n" h (String.concat ";" varl);  *)
    if Xlist.fold qeql true (fun b h2 -> if StringSet.mem def_handles h2 then b else false) && 
       Xlist.fold varl true (fun b v -> if StringSet.mem def_vars (concat_var v) then b else false) then
    h :: found else found) in
  let l = Xlist.rev_map found (fun h -> (try StringMap.find positions h with Not_found -> max_int),h) in
  let l = List.sort (fun (x,_) (y,_) -> compare x y) l in
  Xlist.map l snd

let generate_subsets l =
  (*let ll =*) Xlist.map (Xlist.multiply_list (Xlist.map l (fun x -> [[];[x]]))) List.flatten (*in
  Xlist.fold ll [] (fun ll l -> if Xlist.size l <= 2 then l :: ll else ll)*)
(*   Xlist.map l (fun x -> [x]) *)
  
let rec spaces i =   
  if i = 0 then "" else "  " ^ spaces (i-1)
  
let rec create_decision_tree_greedy n qeqs vars handles hu positions = function
    [] -> if StringSet.size hu = 0 then Tfinish else Terror
  | (gap,def_vars,def_handles) :: tops -> 
(*      Printf.printf "%sgap=%s def_vars={%s} def_handles={%s} hu={%s}\n" (spaces n) gap 
        (String.concat ";" (StringSet.to_list def_vars)) 
        (String.concat ";" (StringSet.to_list def_handles))
        (String.concat ";" (StringSet.to_list hu));*)
      let def_handles = StringSet.add def_handles gap in
      let l = get_possible_matchings def_handles hu def_vars qeqs vars positions in
(*       Printf.printf "%sl=[%s]\n" (spaces n) (String.concat ";" l); *)
     get_possible_matchings_greedy_list n qeqs vars handles hu positions gap def_vars def_handles tops l
      
and get_possible_matchings_greedy_list n qeqs vars handles hu positions gap def_vars def_handles tops = function
    [] -> Terror
  | h :: l -> 
      let new_tops = Xlist.fold (try StringMap.find handles h with Not_found -> []) tops (fun tops (h2,vl) ->
        (h2, Xlist.fold vl def_vars (fun def_vars v -> StringSet.add def_vars (concat_var v)), def_handles) :: tops) in 
      let t = create_decision_tree_greedy (n+1) qeqs vars handles (StringSet.remove hu h) positions new_tops in
      if t = Terror then get_possible_matchings_greedy_list n qeqs vars handles hu positions gap def_vars def_handles tops l 
      else (
(*         Printf.printf "%s%s=%s\n" (spaces n) gap h; *)
        T(gap,[[h], t]))

let rec create_decision_tree_bound m n qeqs vars handles hu positions = function
    [] -> if StringSet.size hu = 0 then Tfinish,m-1,n else Terror,m,n
  | (h,def_vars,def_handles) :: tops -> 
(*      Printf.printf "h=%s def_vars={%s} def_handles={%s} hu={%s}\n" h 
        (String.concat ";" (StringSet.to_list def_vars)) 
        (String.concat ";" (StringSet.to_list def_handles))
        (String.concat ";" (StringSet.to_list hu));*)
      if m < 0 then ((*print_endline "Terror1";*) Terror,m,n) else
      if n < 0 then ((*print_endline "Terror2";*) Terror,m,n) else
      let def_handles = StringSet.add def_handles h in
      let l = get_possible_matchings def_handles hu def_vars qeqs vars positions in
      if l = [] then ((*print_endline "Terror3";*) Terror,m,n) else
      let ll = Xlist.map l (fun x -> [x]) (*else generate_subsets l*) in
(*       Printf.printf "ll=[%s]\n" (String.concat "];[" (Xlist.map ll (fun l -> String.concat ";" (Xlist.map l fst)))); *)
      let trees,m,n = Xlist.fold ll ([],m,n) (fun (trees,m,n) l ->
        if l = [] then trees,m,n else
        let tops = Xlist.fold l tops (fun tops h ->
          Xlist.fold (try StringMap.find handles h with Not_found -> []) tops (fun tops (h2,vl) ->
              (h2, Xlist.fold vl def_vars (fun def_vars v -> StringSet.add def_vars (concat_var v)), def_handles) :: tops)) in 
        if n < 0 then trees,m,n else
        let t,m,n = create_decision_tree_bound m (n-1) qeqs vars handles 
          (Xlist.fold l hu (fun hu x -> StringSet.remove hu x)) positions tops in
        if t = Terror then trees,m,n else (l, t) :: trees,m,n) in
      let tree = if trees = [] then Terror else T(h,trees) in
(*       print_endline (string_of_decision_tree tree); *)
      tree,m,n

let rec substitute_handles map = function
    Conj l -> Conj (Xlist.map l (substitute_handles map))
  | Disj l -> Disj (Xlist.map l (substitute_handles map))
  | Neg f -> Neg (substitute_handles map f)
  | Exist _ -> failwith "substitute_handles: ni"
  | Exist1(v,t) -> Exist1(v,substitute_handles map t)
  | Exist2(v,t,f) -> Exist2(v,substitute_handles map t,substitute_handles map f)
  | ForAll(v,t,f) -> ForAll(v,substitute_handles map t,substitute_handles map f)
  | Quant1(q,v,f) -> Quant1(q,v,substitute_handles map f)
  | Quant2(q,v,t,f) -> Quant2(q,v,substitute_handles map t,substitute_handles map f)
  | Dscr(t,f) -> Dscr(t,substitute_handles map f)
  | Pred(c,l) -> Pred(c,l)
  | Equal(s,t) -> Equal(s,t)
  | Greater(s,t) -> Greater(s,t)
  | True -> True
  | Handle h -> (try StringMap.find map h with Not_found -> failwith "substitute_handles")
  | Requires(v,f) -> Requires(v,substitute_handles map f)
  | Seam f -> Seam (substitute_handles map f)
  | Cut f -> Cut (substitute_handles map f)
  | Context c -> Context c
  | Position(pos,f) -> Position(pos,substitute_handles map f)
        
let rec create_formulae eps = function
    T(h,l) -> 
      List.flatten (Xlist.map l (fun (handles,tree) ->
        let alter_list = create_formulae eps tree in
        Xlist.map alter_list (fun map -> 
          let f = Conj(List.flatten (Xlist.map handles (fun h2 -> 
            Xlist.map (try StringMap.find eps h2 with Not_found -> failwith ("create_formulae: " ^ h2)) (substitute_handles map)))) in
          StringMap.add map h f)))
  | Tfinish -> [StringMap.empty]
  | Terror -> []       

let rec create_formulae_single eps = function
    T(h,l) -> 
      [List.hd (List.flatten (Xlist.map l (fun (handles,tree) ->
        let alter_list = create_formulae_single eps tree in
        Xlist.map alter_list (fun map -> 
          let f = Conj(List.flatten (Xlist.map handles (fun h2 -> 
            Xlist.map (StringMap.find eps h2) (substitute_handles map)))) in
          StringMap.add map h f))))]
  | Tfinish -> [StringMap.empty]
  | Terror -> []       

let foll_of_mrs_greedy mrs = 
(*   Xlist.iter mrs (fun x -> Printf.printf "%s\n" (SemStringOf.mrs_formula x)); *)
  let top,qeqs,vars,handles,hu,eps,positions = create_foll_of_mrs_maps mrs in
  let tree = 
    if StringSet.mem hu top then
      let tops = Xlist.fold (try StringMap.find handles top with Not_found -> []) [] (fun tops (h2,vl(*,single*)) ->
        (h2, Xlist.fold vl StringSet.empty (fun set v -> StringSet.add set (concat_var v)), StringSet.singleton top) :: tops) in 
      let t = create_decision_tree_greedy 0 qeqs vars handles (StringSet.remove hu top) positions tops in
      T(top,[[top],t])
    else create_decision_tree_greedy 0 qeqs vars handles hu positions [top,StringSet.empty,StringSet.empty] in
  let alter_list = create_formulae eps tree in
  Xlist.map alter_list (fun map -> StringMap.find map top)
 
let foll_of_mrs_bound m n l = 
  let top,qeqs,vars,handles,hu,eps,positions = create_foll_of_mrs_maps l in
  let tree,_,n2 = 
    if StringSet.mem hu top then
      let tops = Xlist.fold (try StringMap.find handles top with Not_found -> []) [] (fun tops (h2,vl(*,single*)) ->
        (h2, Xlist.fold vl StringSet.empty (fun set v -> StringSet.add set (concat_var v)), StringSet.singleton top) :: tops) in 
      let t,m,n = create_decision_tree_bound m n qeqs vars handles (StringSet.remove hu top) positions tops in
      T(top,[[top],t]),m,n
    else create_decision_tree_bound m n qeqs vars handles hu positions [top,StringSet.empty,StringSet.empty] in
(*   print_endline (string_of_decision_tree tree); *)
  let alter_list = create_formulae eps tree in
(*  let alter_list = if alter_list <> [] then alter_list else (
    Printf.printf "solution not found %d\n%!"(n - n2);
    let handles = StringMap.map handles (fun l -> Xlist.map l (fun (h,vl,_) -> h,vl,false)) in
    let tree,_,n2 = create_decision_tree_bound m n qeqs vars handles hu [top,StringSet.empty,StringSet.empty,false,""] in
    create_formulae eps tree) in*)
  Xlist.map alter_list (fun map -> StringMap.find map top)(*, n - n2*)
  

