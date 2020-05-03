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

exception Timeout

let check_empty_context_map map =
  StringMap.fold map true (fun b _ l ->
    Xlist.fold l b (fun b -> function
      CEmpty,_ -> b
    | _ -> false))

let check_empty_context p =
  check_empty_context_map p.p_in_sets &&
  check_empty_context_map p.p_equi &&
  check_empty_context_map p.p_constraints &&
  check_empty_context_map p.p_subfields

let assign_defines_list defines l =
  Xlist.map l (fun (c,t) -> XTContext.assign_defines defines c, t)

let assign_defines_map defines map =
  StringMap.map map (assign_defines_list defines)

let assign_defines p =
  {p with p_in_sets = assign_defines_map p.p_defines p.p_in_sets;
          p_equi = assign_defines_map p.p_defines p.p_equi;
          p_constraints = assign_defines_map p.p_defines p.p_constraints;
          p_subfields = assign_defines_map p.p_defines p.p_subfields;
          p_subsumes = assign_defines_list p.p_defines p.p_subsumes}


let make_semform_data_map semform_data = (* heurystycznie określam zakres leksemu niezależnie od kontekstu *)
  StringMap.map semform_data (fun l ->
    let _,(_,left,right) = List.hd l in
    Xlist.fold (List.tl l) (left,right) (fun (left,right) (_,(_,left2,right2)) -> max left left2, min right right2))

let assign_semform_data_term semform_data = function
    QCons(ids,t,i,a,b,p,r) -> let p,r = try StringMap.find semform_data i with Not_found -> 0,0 in QCons(ids,t,i,a,b,p,r)
  | t -> t

let assign_semform_data semform_data p =
  {p with p_constraints = StringMap.map p.p_constraints (fun l -> Xlist.map l (fun (c,t) -> c, assign_semform_data_term semform_data t));
          p_subfields = StringMap.map p.p_subfields (fun l -> Xlist.map l (fun (c,(v,t)) -> c, (v, assign_semform_data_term semform_data t)))}

let transitive_closure map =
  let f = ref true in
  let r = ref map in
  while !f do
    f := false;
    r := StringMap.fold (!r) (!r) (fun map k set ->
      let set2 = StringSet.fold set set (fun set2 v ->
        StringSet.union set2 (StringMap.find map v)) in
      if StringSet.size set2 > StringSet.size set then f := true;
      StringMap.add map k set2)
  done;
  !r

let equi_closure p =
  let map = StringMap.map p.p_equi (fun l ->
    Xlist.fold l StringSet.empty (fun set (_,v) -> StringSet.add set v)) in
  let map = transitive_closure map in
  StringMap.map map (fun set ->
    StringSet.fold set StringMap.empty (fun map v -> StringMap.add map v CEmpty))

let rec equi_closure_amb_rec map visited paths context l =
  Xlist.fold l paths (fun paths (c,v) ->
    if StringSet.mem visited v then paths else
    try
      let context = XTContext.intersection (context,c) in
      let paths = StringMap.add_inc paths v context (fun c -> XTContext.union (context,c)) in
      let paths = StringMap.add paths v (StringMap.find paths v) in
      equi_closure_amb_rec map (StringSet.add visited v) paths context
        (try StringMap.find map v with Not_found -> [])
    with XTContext.No_valid_choice -> paths)

let equi_closure_amb equi_map p =
  StringMap.mapi p.p_equi (fun v l ->
    equi_closure_amb_rec p.p_equi (StringSet.singleton v) (StringMap.add StringMap.empty v CEmpty) CEmpty l)

let make_ids equi_map v c ids =
  let ids2 = try StringMap.find equi_map v with Not_found -> StringMap.add StringMap.empty v CEmpty in
  StringMap.fold ids2 ids (fun ids v c2 ->
    try
      let context = XTContext.intersection (c,c2) in
      StringMap.add_inc ids v context (fun c -> XTContext.union (context,c))
    with XTContext.No_valid_choice -> ids)

let find_node_in_constraint_map_amb equi_map p cnl =
  let ids,cons_vals,qcons_vals = Xlist.fold cnl (StringMap.empty,[],[]) (fun (ids,cons_vals,qcons_vals) -> function
      _,QCons(_,_,_,_,_,_,_) as t -> ids, cons_vals, t :: qcons_vals
    | c,Cons(_,_) as t -> ids, t :: cons_vals, qcons_vals
    | c,LVar v ->
        let ids = make_ids equi_map v c ids in
        ids, cons_vals, qcons_vals
    | _ -> failwith "find_node_in_constraint_map_amb") in
  let subfield_vals,in_set_vals,(cons_vals,qcons_vals) = StringMap.fold ids ([],[],(cons_vals,qcons_vals)) (fun (subfield_vals,in_set_vals,(cons_vals,qcons_vals)) v c ->
    let in_sets = try StringMap.find p.p_in_sets v with Not_found -> [] in
    let subfields = try StringMap.find p.p_subfields v with Not_found -> [] in
    let constraints = try StringMap.find p.p_constraints v with Not_found -> [] in
    Xlist.fold subfields subfield_vals (fun subfield_vals (c2,t) ->
      try (XTContext.intersection (c,c2),t) :: subfield_vals with XTContext.No_valid_choice -> subfield_vals),
    Xlist.fold in_sets in_set_vals (fun in_set_vals (c2,v) ->
      try (XTContext.intersection (c,c2),v) :: in_set_vals with XTContext.No_valid_choice -> in_set_vals),
    Xlist.fold constraints (cons_vals,qcons_vals) (fun (cons_vals,qcons_vals) -> function
        c2,(QCons(_,_,_,_,_,_,_) as t) -> cons_vals,(try (XTContext.intersection (c,c2),t) :: qcons_vals with XTContext.No_valid_choice -> qcons_vals)
      | c2,(Cons(_,_) as t) -> (try (XTContext.intersection (c,c2),t) :: cons_vals with XTContext.No_valid_choice -> cons_vals),qcons_vals
      | _ -> failwith "find_node_in_constraint_map_amb")) in
  ids,subfield_vals,in_set_vals,cons_vals,qcons_vals

let parse_subfields l =
  Xlist.fold l StringMap.empty (fun map (c,(s,t)) ->
    StringMap.add_inc map s [c,t] (fun l -> (c,t) :: l))

let ids_union ids ids2 =
  StringMap.fold ids2 ids (fun ids id c ->
    StringMap.add_inc ids id c (fun c2 -> XTContext.union (c,c2)))

let ids_scalar_intersection context ids =
(*   Printf.printf "c=%s ids=%s ----> \n" (XTStringOf.context_term context) (XTStringOf.string_of_ids ids); *)
  let ids = StringMap.fold ids StringMap.empty (fun ids id c ->
    try
      let c = XTContext.intersection (c,context) in
      StringMap.add ids id c
    with XTContext.No_valid_choice -> ids) in
(*   Printf.printf "----> ids=%s\n" (XTStringOf.string_of_ids ids); *)
  ids


let parse_cons ids l =
  let l = Xlist.fold l [] (fun l -> function
    | c,Cons(ids2,t) -> if StringMap.is_empty ids2 then (c,Cons(ids,t)) :: l else (c,Cons(ids2,t)) :: l
    | _ -> failwith "parse_cons") in
  let map = Xlist.fold l StringMap.empty (fun map -> function
      c,Cons(ids,t) ->
         StringMap.add_inc map t (c,Cons(ids,t)) (function
             (c2,Cons(ids2,_)) -> XTContext.union (c,c2), Cons(ids_union ids ids2, t)
           | _ -> failwith "parse_cons")
    | _ -> failwith "parse_cons") in
  StringMap.fold map [] (fun l _ (c,t) -> (c,t) :: l)

let rec assign_pred_args_list equi_map map c = function
    [] -> [c,[]]
  | LVar a :: l ->
      let map_a = try StringMap.find map a
      with Not_found -> (
        let ids = make_ids equi_map a c StringMap.empty in
        StringMap.fold ids StringMap.empty (fun map_a id c2 ->
          let map_id = try StringMap.find map id with Not_found -> StringMap.empty in
          StringMap.fold map_id map_a (fun map_a e c3 ->
            try
              let context = XTContext.intersection (XTContext.intersection (c,c2), c3) in
              StringMap.add_inc map_a e context (fun c -> XTContext.union (c,context))
            with XTContext.No_valid_choice -> map_a))
      (*print_endline "assign_pred_args_list 1";*) (*StringMap.add StringMap.empty "??" CEmpty*)) in
      StringMap.fold map_a [] (fun found e c2 ->
        try
          let context = XTContext.intersection (c,c2) in
          let la = assign_pred_args_list equi_map map context l in
          let x = Cons(make_ids equi_map a context StringMap.empty(*StringMap.add StringMap.empty a context*),e) in
          Xlist.fold la found (fun found (c,l) -> (c, x :: l) :: found)
        with XTContext.No_valid_choice -> found)
  | Cons(ids,"NULL") :: l ->
      let la = assign_pred_args_list equi_map map c l in
      Xlist.fold la [] (fun found (c,l) -> (c, Cons(ids,"NULL") :: l) :: found)
  | _ -> failwith "assign_pred_args_list 2"

let rec assign_pred_args equi_map map found = function
    [] -> found
  | (c,QCons(ids,t,i,a,b,p,r)) :: l ->
       let la = assign_pred_args_list equi_map map c a in
       let found = Xlist.fold la found (fun found (c,a) ->
         let lb = assign_pred_args_list equi_map map c b in
         Xlist.fold lb found (fun found (c,b) ->
           (c,QCons(ids,t,i,a,b,p,r)) :: found)) in
       assign_pred_args equi_map map found l
  | _ -> failwith "assign_pred_args"

let string_of_args l =
  String.concat "^" (Xlist.map l (function
     Cons(_,s) -> s
   | _ -> failwith "string_of_args"))

let rec merge_args = function
    [],[] -> []
  | Cons(ids,s) :: l,Cons(ids2,_) :: l2 -> Cons(ids_union ids ids2,s) :: merge_args (l,l2)
  | _ -> failwith "merge_args"

let parse_qcons  equi_map pred_arg_map ids l =
  let l = Xlist.fold l [] (fun l -> function
    | c,QCons(ids2,t,i,a,b,p,r) -> if StringMap.is_empty ids2 then (c,QCons(ids,t,i,a,b,p,r)) :: l else (c,QCons(ids2,t,i,a,b,p,r)) :: l
    | _ -> failwith "parse_qcons") in
  let l = assign_pred_args equi_map pred_arg_map [] l in
  let map = Xlist.fold l StringMap.empty (fun map -> function
      c,QCons(ids,t,i,a,b,p,r) ->
         StringMap.add_inc map (t ^ "#" ^ i ^ "#" ^ string_of_args a ^ "#" ^ string_of_args b) (c,QCons(ids,t,i,a,b,p,r)) (function
             (c2,QCons(ids2,_,_,a2,b2,_,_)) -> XTContext.union (c,c2), QCons(ids_union ids ids2, t,i,merge_args (a,a2),merge_args (b,b2),p,r)
           | _ -> failwith "parse_qcons")
    | _ -> failwith "parse_qcons") in
  StringMap.fold map [] (fun l _ (c,t) -> (c,t) :: l)

let parse_in l =
  let map = Xlist.fold l StringMap.empty (fun map (c,v) ->
    StringMap.add_inc map v c (fun c2 -> XTContext.union (c,c2))) in
  StringMap.fold map [] (fun l v c -> (c,LVar v) :: l)

let get_qcons_id = function
    QCons(ids2,t,i,a,b,p,r) -> i
  | _ -> failwith "get_qcons_id"

let rec find_path pred_id = function
    (id,e) :: l -> if id = pred_id then [e] else e :: (find_path pred_id l)
  | [] -> failwith "find_path"

let make_arg_map subfields =
  StringMap.fold subfields StringMap.empty (fun map s l ->
    Xlist.fold l map (fun map -> function
        c,LVar v -> StringMap.add_inc map v (StringMap.add StringMap.empty s c)
                      (fun map2 -> StringMap.add_inc map2 s c (fun c2 -> XTContext.union (c,c2)))
      | c,Cons _ -> map
      | c,QCons _ -> map
      | _ -> failwith "make_arg_map"))


let rec create_constraint_tree_amb equi_map p visited path pred_arg_map cnl =
  let ids,subfield_vals,in_set_vals,cons_vals,qcons_vals = find_node_in_constraint_map_amb equi_map p cnl in
  if Xlist.size subfield_vals + Xlist.size in_set_vals + Xlist.size cons_vals + Xlist.size qcons_vals = 0 then [] else
  match Xlist.size subfield_vals, Xlist.size in_set_vals, Xlist.size cons_vals, Xlist.size qcons_vals with
    _,0,0,0 ->
      let subfields = parse_subfields subfield_vals in
      if StringMap.mem subfields "PRED" then
        let preds = create_constraint_tree_amb equi_map p visited (("","PRED") :: path) (make_arg_map subfields) (StringMap.find subfields "PRED") in
        let subfields = StringMap.remove subfields "PRED" in
        Xlist.fold preds [] (fun results (c,pred) ->
          let pred_id = get_qcons_id pred in
          if StringSet.mem visited pred_id then (c,Loop(ids_scalar_intersection c ids,find_path pred_id path)) :: results else
          let visited = StringSet.add visited pred_id in
          let l = StringMap.fold subfields [] (fun l e vals ->
            let vals = Xlist.fold vals [] (fun vals (c2,t) ->
              try (XTContext.intersection (c,c2),t) :: vals with XTContext.No_valid_choice -> vals) in
            let vals = create_constraint_tree_amb equi_map p visited ((pred_id,e) :: path) StringMap.empty vals in
            if vals = [] then l else (e,Context vals) :: l) in
          (c,Compound(ids_scalar_intersection c ids,("PRED",pred) :: l)) :: results)
      else
          let l = StringMap.fold subfields [] (fun l e vals ->
            let vals = create_constraint_tree_amb equi_map p visited (("",e) :: path) StringMap.empty vals in
            if vals = [] then l else (e,Context vals) :: l) in
          [CEmpty,Compound(ids,l)]
  | 0,_,0,0 -> (* FIXME: brakuje scalania identycznych fstruktur *)
      let vals = Xlist.map (parse_in in_set_vals) (fun ct ->
        Context (create_constraint_tree_amb equi_map p visited (("","inset") :: path) StringMap.empty [ct])) in
      [CEmpty,Set(ids,vals)]
  | 0,0,_,0 -> parse_cons ids cons_vals
  | 0,0,0,_ -> parse_qcons equi_map pred_arg_map ids qcons_vals
  | _,_,0,0 ->  (* FIXME: podzial zwn coordform *)
      let subfields = parse_subfields subfield_vals in
      let pred_sf =
        if StringMap.mem subfields "PRED" then
          let preds = create_constraint_tree_amb equi_map p visited (("","PRED") :: path) (make_arg_map subfields) (StringMap.find subfields "PRED") in
          let subfields = StringMap.remove subfields "PRED" in
          Xlist.fold preds [] (fun results (c,pred) ->
            let pred_id = get_qcons_id pred in
            if StringSet.mem visited pred_id then (c,Loop(ids_scalar_intersection c ids,find_path pred_id path)) :: results else
            let visited = StringSet.add visited pred_id in
            let l = StringMap.fold subfields [] (fun l e vals ->
              let vals = Xlist.fold vals [] (fun vals (c2,t) ->
                try (XTContext.intersection (c,c2),t) :: vals with XTContext.No_valid_choice -> vals) in
              let vals = create_constraint_tree_amb equi_map p visited ((pred_id,e) :: path) StringMap.empty vals in
              if vals = [] then l else (e,Context vals) :: l) in
            (c,Compound(ids_scalar_intersection c ids,("PRED",pred) :: l)) :: results)
        else [] in
      let coord_context = Xlist.fold in_set_vals (COr[]) (fun context (c,_) -> XTContext.union (context,c)) in
      let subfields = StringMap.fold subfields [] (fun l e vals ->
        let vals = Xlist.fold vals [] (fun vals (c,t) ->
              try (XTContext.intersection (coord_context,c),t) :: vals with XTContext.No_valid_choice -> vals) in
        let vals = create_constraint_tree_amb equi_map p visited (("",e) :: path) StringMap.empty vals in
        if vals = [] then l else (e,Context vals) :: l) in
      let set = Xlist.map (parse_in in_set_vals) (fun ct ->
        Context (create_constraint_tree_amb equi_map p visited (("","inset") :: path) StringMap.empty [ct])) in
      [coord_context,Coordination(ids_scalar_intersection coord_context ids,set,subfields)] @ pred_sf
  | _ -> failwith "create_constraint_tree_amb: ni"

let rec manage_coordination attr = function
    Cons _ as t -> t
  | QCons _ as t -> t
  | LVar _ as t -> t
  | Compound(ids,l) ->
       Compound(ids,Xlist.map l (fun (e,t) -> e, manage_coordination e t))
  | Set(ids,l) ->
      if Xlist.mem (!adjuncts) attr then Set(ids,Xlist.map l (manage_coordination attr))
      else Coordination(ids,Xlist.map l (manage_coordination attr), [])
  | Coordination(ids,l,l2) ->
       Coordination(ids,Xlist.map l (manage_coordination attr),
       Xlist.map l2 (fun (e,t) -> e, manage_coordination e t))
  | Loop _ as t -> t
  | Context l ->
      Context(Xlist.map l (fun (c,t) -> c,manage_coordination attr t))

let add_punctuation_cons s l = ("PUN",Cons(StringMap.empty,s)) :: l

let rec assign_punctuation_rec s = function
    Compound(ids,l) -> Compound(ids,add_punctuation_cons s l)
  | Coordination(ids,l,l2) ->
       (match List.rev l with
          [] -> failwith "assign_punctuation_rec"
        | t :: l -> Coordination(ids,List.rev ((assign_punctuation_rec s t) :: l),l2)  )
(*   Coordination(ids,List.h(List.rev l),add_punctuation_cons s l2) *)
  | Context l -> Context(Xlist.map l (fun (c,t) -> c,assign_punctuation_rec s t))
  | _ -> failwith "assign_punctuation_rec"

(*let other_tokens = StringSet.of_list [",";"się";"em";"m";"by";"EŚ";"ś";"śmy";"eś";")";"ście";"jest";"\"";"”";"nie";"będzie"]

let other_token s =
  StringSet.mem other_tokens s*)

let rec get_punctuation rev = function
    [] -> rev
  | ")" :: "?" :: "(" :: l -> get_punctuation ("(?)" :: rev) l
  | "." :: l -> get_punctuation ("." :: rev) l
  | "?" :: l -> get_punctuation ("?" :: rev) l
  | "!" :: l -> get_punctuation ("!" :: rev) l
  | ":" :: l -> get_punctuation (":" :: rev) l
  | "\"" :: l -> get_punctuation rev l
  | ")" :: l -> get_punctuation rev l
  | "”" :: l -> get_punctuation rev l
  | "]" :: l -> get_punctuation rev l
  | "," :: l -> get_punctuation rev l
  | "" :: l -> get_punctuation rev l
  | s :: l ->
     let c = String.get s 0 in
     if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then rev
     else if String.length s = 1 then (print_endline ("get_punctuation: " ^ s); rev)
     else (match String.sub s 0 2 with
       "ś" -> rev
     | "Ś" -> rev
     | "ł" -> rev
     | "Ł" -> rev
     | "ż" -> rev
     | "Ż" -> rev
     | "ć" -> rev
     | "Ć" -> rev
     | "ź" -> rev
     | "Ź" -> rev
     | "ó" -> rev
     | "Ó" -> rev
     | _ -> print_endline ("get_punctuation: " ^ s); rev)


let assign_punctuation p tree =
  let s = String.concat "" (get_punctuation [] (List.rev (Str.split (Str.regexp " ") p.p_sentence))) in
(*  let n = StringMap.fold semform_data 0 (fun n _ (i,_) -> max n i) in
  let l = Xlist.fold p.p_surfaceform [] (fun l (c,(_,s,i,_)) ->
    if i > n && not (other_token s) then (*if c = CEmpty then*) (i,s) :: l (*else (print_endline ("weird concext: " ^ s); (i,s) :: l)*) else l) in
  let l = List.sort (fun x y -> compare (fst x) (fst y)) l in
  let s = match Xlist.map l snd with
      ["."] -> "."
    | ["?"] -> "?"
    | ["!"] -> "!"
    | ["?";"!"] -> "?!"
    | ["!";".";".";"."] -> "!..."
    | ["?";".";".";"."] -> "?..."
    | [".";".";".";"?"] -> "...?"
    | ["!";"!";"!"] -> "!!!"
    | [".";".";"."] -> "..."
    | [".";".";".";"."] -> "..." (* FIXME: tu by trzeba sprawdzać rozłączność kontekstów *)
    | ["?";"?"] -> "?" (* FIXME: tu by trzeba sprawdzać rozłączność kontekstów *)
    | [".";"?"] -> "?" (* FIXME: tu by trzeba sprawdzać rozłączność kontekstów *)
    | [".";"."] -> "." (* FIXME: tu by trzeba sprawdzać rozłączność kontekstów *)
    | l -> print_endline ("assign_punctuation: [\"" ^ String.concat "\";\"" l ^ "\"]"); String.concat " " l in*)
  assign_punctuation_rec s tree

let normalize_contexts_ids context ids =
  StringMap.map ids (fun c ->
    if XTContext.equal (c,context) then CEmpty else c)

let rec normalize_contexts context = function
    Cons(ids,t) -> Cons(normalize_contexts_ids context ids,t)
  | QCons(ids,t,i,a,b,p,r) -> QCons(normalize_contexts_ids context ids,t,i,a,b,p,r)
  | LVar _ as t -> t
  | Compound(ids,l) ->
       Compound(normalize_contexts_ids context ids,Xlist.map l (fun (e,t) -> e, normalize_contexts context t))
  | Set(ids,l) -> Set(normalize_contexts_ids context ids,Xlist.map l (normalize_contexts context))
  | Coordination(ids,l,l2) ->
       Coordination(normalize_contexts_ids context ids,Xlist.map l (normalize_contexts context),
       Xlist.map l2 (fun (e,t) -> e, normalize_contexts context t))
  | Loop(ids,t) -> Loop(normalize_contexts_ids context ids,t)
  | Context[c,t] ->
      let c = if c = CEmpty then context else c in
      if XTContext.equal (c,context) then normalize_contexts context t
      else Context[c,normalize_contexts c t]
  | Context l ->
      Context(Xlist.map l (fun (c,t) ->
        let c = if c = CEmpty then context else c in
        if XTContext.equal (c,context) then CEmpty, normalize_contexts context t
        else c,normalize_contexts c t))

let normalize_root_context = function
    Context[_,t] -> t
  | t -> t

let rec has_sem = function
    Cons _ -> false
  | QCons _ -> false
  | LVar _ -> false
  | Compound(_,l) ->
       (try
         let _ = Xlist.assoc l "PRED" in true
       with Not_found -> false)
  | Set _ -> true
  | Coordination _ -> true
  | Loop _ -> true
  | Context ((_,t) :: _) -> has_sem t
  | Context [] -> failwith "has_sem"

let get_complement model_size context l =
(*   Printf.printf "gc1 %d %s %s\n%!" model_size (XTStringOf.context_term context) (XTStringOf.lfg_term (Context l)); *)
  let c = Xlist.fold l (COr[]) (fun context (c,_) -> XTContext.union (context,c)) in
  XTContext.difference model_size (context,c)

(*exception EmptyContext

let rec normalize_cons_contexts model_size context = function
    Cons(ids,t) -> Cons(ids,t)
  | QCons(ids,t,i,a,b,p,r) -> QCons(ids,t,i,a,b,p,r)
  | LVar _ as t -> t
  | Compound(ids,l) ->
(*        Printf.printf "a0 %s\n%!" (XTStringOf.lfg_term (Compound(ids,l))); *)
       let l = normalize_cons_contexts_list2 model_size context l in
(*        Printf.printf "a1 %s\n%!" (XTStringOf.lfg_term (Compound(ids,l))); *)
       let ll = Xlist.multiply_list (Xlist.map l (fun (e,t) ->
         if has_sem t then [CEmpty,e,t] else
         match t with
           Context l -> (try [get_complement model_size context l,e,LVar ""] with XTContext.No_valid_choice -> []) @
                        Xlist.map l (fun (c,t) -> c,e,t)
         | _ -> [CEmpty,e,t])) in
(*        Printf.printf "a2 %d %s\n%!" (Xlist.size ll) (XTStringOf.context_term context); *)
(*        Printf.printf "   %s\n%!" (String.concat "\n   " (Xlist.map ll (fun l -> XTStringOf.lfg_term (Compound(StringMap.empty,Xlist.map l (fun (c,e,t) -> e,Context[c,t])))))); *)
       let found = Xlist.fold ll [] (fun found l ->
         try
           let c,l = Xlist.fold l (context,[]) (fun (context,l) -> function
               (c,e,LVar "") -> XTContext.intersection (context,c),l
             | (c,e,t) -> XTContext.intersection (context,c),(e,t) :: l)  in
           (c,Compound(ids,l)) :: found
         with XTContext.No_valid_choice -> found) in
(*        Printf.printf "a3 %d\n%!" (Xlist.size found); *)
       (match found with
          [] -> Context []
        | [c,t] -> if XTContext.equal (c,context) then t else Context [c,t]
        | l -> Context l)
  | Set(ids,l) -> Set(ids,normalize_cons_contexts_list model_size context l)
  | Coordination(ids,l2,l) ->
       let l2 = normalize_cons_contexts_list model_size context l2 in
       let l = try normalize_cons_contexts_list2 model_size context l with EmptyContext -> [] in
       let ll = Xlist.multiply_list (Xlist.map l (fun (e,t) ->
         if has_sem t then [CEmpty,e,t] else
         match t with
           Context l -> (try [get_complement model_size context l,e,LVar ""] with XTContext.No_valid_choice -> []) @
                        Xlist.map l (fun (c,t) -> c,e,t)
         | _ -> [CEmpty,e,t])) in
       let found = Xlist.fold ll [] (fun found l ->
         try
           let c,l = Xlist.fold l (context,[]) (fun (context,l) -> function
               (c,e,LVar "") -> XTContext.intersection (context,c),l
             | (c,e,t) -> XTContext.intersection (context,c),(e,t) :: l)  in
           (c,Coordination(ids,l2,l)) :: found
         with XTContext.No_valid_choice -> found) in
       (match found with
          [] -> Context []
        | [c,t] -> if XTContext.equal (c,context) then t else Context [c,t]
        | l -> Context l)
  | Loop(ids,t) -> Loop(ids,t)
  | Context l -> (* FIXME: problem, gdy kontekst pozostanie pusty *)
      let l = List.flatten (Xlist.map l (fun (c,t) ->
        match (try normalize_cons_contexts model_size c t with EmptyContext -> Context []) with
          Context l -> l
        | t -> [c,t])) in
      if l = [] then raise EmptyContext
      else Context l

and normalize_cons_contexts_list model_size context l =
  let l = Xlist.fold l [] (fun l t ->
    try (normalize_cons_contexts model_size context t) :: l with EmptyContext -> l) in
  if l = [] then raise EmptyContext else List.rev l

and normalize_cons_contexts_list2 model_size context l =
  let l = Xlist.fold l [] (fun l (e,t) ->
    try (e, normalize_cons_contexts model_size context t) :: l with EmptyContext -> l) in
  if l = [] then raise EmptyContext else List.rev l*)

let rec normalize_cons_contexts model_size context = function
    Cons(ids,t) -> Cons(ids,t)
  | QCons(ids,t,i,a,b,p,r) -> QCons(ids,t,i,a,b,p,r)
  | LVar _ as t -> t
  | Compound(ids,l) ->
       let l = Xlist.map l (fun (e,t) -> e, normalize_cons_contexts model_size context t) in
       let ll = Xlist.multiply_list (Xlist.map l (fun (e,t) ->
         if has_sem t then [CEmpty,e,t] else
         match t with
           Context l -> (try [get_complement model_size context l,e,LVar ""] with XTContext.No_valid_choice -> []) @
                        Xlist.map l (fun (c,t) -> c,e,t)
         | _ -> [CEmpty,e,t])) in
       let found = Xlist.fold ll [] (fun found l ->
         try
           let c,l = Xlist.fold l (context,[]) (fun (context,l) -> function
               (c,e,LVar "") -> XTContext.intersection (context,c),l
             | (c,e,t) -> XTContext.intersection (context,c),(e,t) :: l)  in
           (c,Compound(ids,l)) :: found
         with XTContext.No_valid_choice -> found) in
       (match found with
          [c,t] -> if XTContext.equal (c,context) then t else Context [c,t]
        | l -> Context l)
  | Set(ids,l) -> Set(ids,Xlist.map l (normalize_cons_contexts model_size context))
  | Coordination(ids,l2,l) ->
       let l2 = Xlist.map l2 (normalize_cons_contexts model_size context) in
       let l = Xlist.map l (fun (e,t) -> e, normalize_cons_contexts model_size context t) in
       let ll = Xlist.multiply_list (Xlist.map l (fun (e,t) ->
         if has_sem t then [CEmpty,e,t] else
         match t with
           Context l -> (try [get_complement model_size context l,e,LVar ""] with XTContext.No_valid_choice -> []) @
                        Xlist.map l (fun (c,t) -> c,e,t)
         | _ -> [CEmpty,e,t])) in
       let found = Xlist.fold ll [] (fun found l ->
         try
           let c,l = Xlist.fold l (context,[]) (fun (context,l) -> function
               (c,e,LVar "") -> XTContext.intersection (context,c),l
             | (c,e,t) -> XTContext.intersection (context,c),(e,t) :: l)  in
           (c,Coordination(ids,l2,l)) :: found
         with XTContext.No_valid_choice -> found) in
       (match found with
          [c,t] -> if XTContext.equal (c,context) then t else Context [c,t]
        | l -> Context l)
  | Loop(ids,t) -> Loop(ids,t)
  | Context l ->
      let l = List.flatten (Xlist.map l (fun (c,t) ->
        match normalize_cons_contexts model_size c t with
          Context l -> l
        | t -> [c,t])) in
      Context l

let normalize_contexts2_ids context ids =
  StringMap.fold ids StringMap.empty (fun map v c ->
    try
      let c = XTContext.intersection (c,context) in
      if XTContext.equal (c,context) then StringMap.add map v CEmpty
      else StringMap.add map v c
    with XTContext.No_valid_choice -> map)

let rec normalize_contexts2 context = function
    Cons(ids,t) -> Cons(normalize_contexts2_ids context ids,t)
  | QCons(ids,t,i,a,b,p,r) -> QCons(normalize_contexts2_ids context ids,t,i,a,b,p,r)
  | LVar _ as t -> t
  | Compound(ids,l) ->
       Compound(normalize_contexts2_ids context ids,Xlist.map l (fun (e,t) -> e, normalize_contexts2 context t))
  | Set(ids,l) -> Set(normalize_contexts2_ids context ids,Xlist.map l (normalize_contexts2 context))
  | Coordination(ids,l,l2) ->
       Coordination(normalize_contexts2_ids context ids,Xlist.map l (normalize_contexts2 context),
       Xlist.map l2 (fun (e,t) -> e, normalize_contexts2 context t))
  | Loop(ids,t) -> Loop(normalize_contexts2_ids context ids,t)
  | Context[c,t] ->
      let c = if c = CEmpty then context else c in
      if XTContext.equal (c,context) then normalize_contexts2 context t
      else Context[c,normalize_contexts2 c t]
  | Context l ->
      let l = Xlist.fold l [] (fun l (c,t) ->
        try
          let c = XTContext.intersection (c,context) in
            if XTContext.equal (c,context) then (CEmpty, normalize_contexts2 context t) :: l
            else (c,normalize_contexts2 c t) :: l
        with XTContext.No_valid_choice -> l) in
      (match List.rev l with
         [CEmpty,t] -> t
       | l -> Context l)

let rec map_compound f = function
    Cons(_,_) as t -> t
  | QCons(_,_,_,_,_,_,_) as t-> t
  | LVar _ as t -> t
  | Compound (ids,l) ->
       let ids,l = f ids l in
       Compound(ids,Xlist.map l (fun (e,t) -> e, map_compound f t))
  | Set(ids,l) -> Set(ids,Xlist.map l (map_compound f))
  | Coordination(ids,l,l2) ->
       let ids,l2 = f ids l2 in
       Coordination(ids,Xlist.map l (map_compound f),
       Xlist.map l2 (fun (e,t) -> e, map_compound f t))
  | Loop _ as t -> t
  | Context l -> Context(Xlist.map l (fun (c,t) -> c,map_compound f t))

let attribute_order = fst (Xlist.fold [
  "FIRST";
  "REST";
  "STMT-TYPE";
  "PRED";
  "COORD-FORM";
  "PRECOORD-FORM";
  "SUBJ";
  "OBJ";
  "OBJ-TH";
  "APP";
  "OBL";
  "OBL2";
  "OBL-INST";
  "OBL-GEN";
  "OBL-AG";
  "OBL-STR";
  "OBL-COMPAR";
  "OBL-LOCAT";
  "OBL-TEMP";
  "OBL-MOD";
  "OBL-ADL";
  "OBL-ABL";
  "OBL-ADV";
  "OBL-DUR";
  "OBL-PERL";
  "COMP";
  "XCOMP";
  "XCOMP-PRED";
  "POSS";
  "ADJUNCT";
  "XADJUNCT";
  "CHECK";
  "ATYPE";
  "NTYPE";
  "TYPE";
  "CLAUSE-TYPE";
  "IMPERSONAL";
  "DEGREE";
  "TNS-ASP";
  "PTYPE";
  "PFORM";
  "PCASE";
  "COMP-FORM";
  "CASE";
  "GEND";
  "NUM";
  "PERS";
  "NEG";
  "PASSIVE";
  "REFLEXIVE";
  "CORRELATIVE";
  "ACM";
  "NSEM";
  "NSYN";
  "_CAT";
  "_VOC";
  "_PREDICATIVE";
  "_ACC";
  "_PPREP";
  "_RQR";
  "ASPECT";
  "MOOD";
  "TENSE";
  "SPAN";
  "PUN";
  "COMMON";
  "TIME";
  "SEM-NUM";
  ] (StringMap.empty,0) (fun (map,n) k -> StringMap.add map k n, n+1))

let compare_attributes (a,_) (b,_) =
  try
    compare (StringMap.find attribute_order a) (StringMap.find attribute_order b)
  with Not_found -> print_endline ("compare_attributes: " ^ a ^ " " ^ b); compare a b

let rec sort_fstructure_attributes =
  map_compound (fun ids l ->
    ids,List.sort compare_attributes l)



(*
let equi_closure_amb_rec_timeout = 1000.
let create_constraint_tree_amb_bound_timeout = 1000.

let list_of_ids ids =
  List.sort compare (StringMap.fold ids [] (fun l id _ -> id :: l))

let ids_union ids ids2 =
  StringMap.fold ids2 ids (fun ids id c ->
    if c <> CEmpty then failwith "ids_union" else
    StringMap.add_inc ids id CEmpty (function CEmpty -> CEmpty | _ -> failwith "ids_union"))

let rec check_identity2_ids_rec = function
    id :: l, id2 :: l2 -> if id = id2 then check_identity2_ids_rec (l,l2) else failwith ("check_identity2_ids_rec: " ^ id ^ " " ^ id2)
  | [],[] -> ()
  | _ -> failwith "check_identity2_ids_rec 2"

let check_identity2_ids ids ids2 =
  let l = list_of_ids ids in
  let l2 = list_of_ids ids2 in
  try check_identity2_ids_rec (List.sort compare l,List.sort compare l2)
  with _ -> failwith ("check_identity2_ids: " ^ String.concat " " l ^ " <> " ^ String.concat " " l2)*)


(*************************************************************************************************)


(*let rec has_cat3 cat cl context = function
    Cons(_,s) -> if Xlist.mem cat s then context :: cl else cl
  | Context l -> Xlist.fold l cl (fun cl (c,t) ->
     try has_cat3 cat cl (XTContext.intersection (c,context)) t with XTContext.No_valid_choice -> cl)
  | _ -> failwith "has_cat3"

let rec has_cat2 cat cl context = function
    Compound(_,l) ->
        Xlist.fold l cl (fun cl -> function
           "_CAT",t -> has_cat3 cat cl context t
         | _ -> cl)
  | Context l -> Xlist.fold l cl (fun cl (c,t) ->
     try has_cat2 cat cl (XTContext.intersection (c,context)) t with XTContext.No_valid_choice -> cl)
  | _ -> failwith "has_cat2"

let has_cat cat l =
  let cl = Xlist.fold l [] (fun cl -> function
      "CHECK",t -> has_cat2 cat cl CEmpty t
    | _ -> cl) in
  match cl with
      [] -> raise XTContext.No_valid_choice
    | [c] -> c
    | _ -> COr cl

let rec find_attribute a = function
    (e,v) :: l -> if e = a then v else find_attribute a l
  | [] -> failwith ("find_attribute: " ^ a)

let rec replace_attribute a v = function
    (e,w) :: l -> if e = a then (e,v) :: l else (e,w) :: (replace_attribute a v l)
  | [] -> failwith "replace_attribute"

let rec has_attr attr = function
    (e,_) :: l -> if e = attr then true else has_attr attr l
  | [] -> false

let rec has_attr2 attr = function
    (e,_,_) :: l -> if e = attr then true else has_attr2 attr l
  | [] -> false

let rec find_attribute2 cat cl context = function
    Compound(_,l) -> (context,find_attribute cat l) :: cl
  | Coordination(_,_,l) -> (context,find_attribute cat l) :: cl
  | Context l -> Xlist.fold l cl (fun cl (c,t) ->
     try find_attribute2 cat cl (XTContext.intersection (c,context)) t with XTContext.No_valid_choice -> cl)
  | _ -> failwith "find_attribute2"*)

let rec find_attribute found context path = function
    Cons(_,s) -> if path = [] then (context,s) :: found else found
  | QCons _ -> found
  | LVar _ -> failwith "find_attribute"
  | Compound (_,l) ->
       if path = [] then found else
       Xlist.fold l found (fun found (e,t) ->
         if e = List.hd path then find_attribute found context (List.tl path) t else found)
  | Set _ -> found
  | Coordination(ids,l,l2) ->
       let found = if path = [] then found else
         Xlist.fold l2 found (fun found (e,t) ->
           if e = List.hd path then find_attribute found context (List.tl path) t else found) in
       Xlist.fold l found (fun found t -> find_attribute found context path t)
  | Loop _ -> found
  | Context l ->
       Xlist.fold l found (fun found (c,t) ->
         try find_attribute found (XTContext.intersection (c,context)) path t
         with XTContext.No_valid_choice -> found)

let rec assign_prep_cases =
  map_compound (fun ids l ->
      let l =
        let cats = find_attribute [] CEmpty ["CHECK";"_CAT"] (Compound(StringMap.empty,l)) in
        let cases = Xlist.fold cats [] (fun cases (context,s) ->
          if s <> "prep" then cases else
          find_attribute cases context ["OBJ";"CASE"] (Compound(StringMap.empty,l))) in
        let cases = Xlist.fold cats cases (fun cases (context,s) ->
          if s <> "prep" then cases else
          let l = find_attribute [] context ["OBJ";"CHECK";"_CAT"] (Compound(StringMap.empty,l)) in
          Xlist.fold l cases (fun cases -> function
              c,"adjp" -> (c,"adjp") :: cases
            | _,"subst" -> cases
            | _,"adj" -> cases
            | _,"pron" -> cases
            | _,"num" -> cases
            | _,"ger" -> cases
            | _,"prep" -> cases
            | _,"pact" -> cases
            | _,"ppas" -> cases
            | _,s -> failwith ("assign_prep_cases: " ^ s))) in
        let cats = find_attribute [] CEmpty ["PFORM"] (Compound(StringMap.empty,l)) in
        let cases = Xlist.fold cats cases (fun cases (context,s) ->
          find_attribute cases context ["CASE"] (Compound(StringMap.empty,l))) in
        let map = Xlist.fold cases StringMap.empty (fun map (c,s) -> StringMap.add_inc map s c (fun c2 -> XTContext.union (c,c2))) in
        match StringMap.fold map [] (fun l s c -> (c,Cons(StringMap.empty,s)) :: l) with
          [] -> l
        | [CEmpty,t] -> ("PCASE",t) :: l
        | cl -> ("PCASE",Context cl) :: l in
      ids,l)

let rec create_choices_tree = function
    Cons(_,_) -> Context []
  | QCons(_,_,_,_,_,_,_) -> Context []
  | LVar _ ->  failwith "create_choices"
  | Compound (_,l) -> create_choices_tree_list (Xlist.map l snd)
  | Set(_,l) -> create_choices_tree_list l
  | Coordination(_,l,l2) -> create_choices_tree_list (l @ Xlist.map l2 snd)
  | Loop _ -> Context []
  | Context l -> Context(Xlist.map l (fun (c,t) -> c,create_choices_tree t))

and create_choices_tree_list l =
  let l = Xlist.map l create_choices_tree in
  let l = Xlist.fold l [] (fun l -> function
      Context l2 -> l @ l2
    | _ -> failwith "create_choices_tree_list") in
  Context l

let rec intersect_context context = function
    Context l -> Context (Xlist.fold l [] (fun l (c,t) ->
      (try (XTContext.intersection (context,c), intersect_context context t) :: l
      with XTContext.No_valid_choice -> l)))
  | _ -> failwith "intersect_context"

let rec add_v = function
    [] -> ["A"]
  | "Z" :: l -> "A" :: add_v l
  | s :: l -> String.make 1 (Char.chr (Char.code (String.get s 0) + 1)) :: l

let rec create_choices_rec root_context vl choices choice part_map = function
    Context [] -> vl,choices,part_map
(*   | Context [CEmpty,t] -> print_endline "create_choices_rec: CEmpty"; create_choices_rec vl choices choice part_map t *)
(*  | Context [c,t] ->
(*       print_endline (XTStringOf.context_term c); *)
      let _ = try XTContext.part_map_find part_map c with Not_found -> failwith ("create_choices_rec 1: " ^ XTStringOf.lfg_term (Context [c,t])) in
      create_choices_rec root_context vl choices choice part_map t*)
  | Context l ->
      let vl = add_v vl in
      let v = String.concat "" (List.rev vl) in
      let l2 = XTContext.add_relative_complement (Xlist.map l fst) root_context in
      let partition = XTContext.create_partition l2 in
(*      Printf.printf "%4s %180s\n" (XTStringOf.context_term choice) (XTStringOf.context_term root_context);
(*       print_endline (XTStringOf.context_term choice ^ " " ^ XTStringOf.context_term root_context); *)
      print_endline (XTStringOf.lfg_term (Context l));
      print_endline (String.concat " " (Xlist.map l2 XTStringOf.context_term));
      print_endline (String.concat " " (Xlist.map partition (fun (m,i) -> XTStringOf.context_term (CModel m) ^ " -> " ^ i)));*)
      if Xlist.size partition = 1 then create_choices_rec root_context vl choices choice part_map (Context (Xlist.fold l [] (fun l -> function (_,Context t) -> t @ l | _ -> failwith "create_choices_rec 5"))) else
      let choices = (Xlist.rev_map partition (fun (_,i) -> CVar(v,i)),choice) :: choices in
      let part_map = Xlist.fold partition part_map (fun part_map (m,i) ->
        XTContext.part_map_add part_map (CModel m) (CVar(v,i))) in
      let map = Xlist.fold l StringMap.empty (fun map -> function
          (c,Context t) ->
            let l = XTContext.apply_partition partition c in
            Xlist.fold l map (fun map (i,p) ->
              let t = match intersect_context (CModel p) (Context t) with Context t -> t | _ -> failwith "create_choices_rec 2" in
              StringMap.add_inc map i (p,t) (fun (_,t2) -> p,t @ t2))
        | _ -> failwith "create_choices_rec 3") in
      StringMap.fold map (vl,choices,part_map) (fun (vl,choices,part_map) i (p,t) ->
        create_choices_rec (CModel p) vl choices (CVar(v,i)) part_map (Context t))
  | _ -> failwith "create_choices_rec 4"

let create_choices tree =
  let tree = create_choices_tree tree in
  let _,choices,part_map = create_choices_rec CEmpty [] [] CEmpty XTContext.part_map_empty tree in
  List.rev choices,part_map

let convert_context_ids part_map choices ids =
  (*StringMap.map ids (fun c -> XTContext.convert_context part_map choices c(*(try XTContext.part_map_find part_map c with Not_found -> c)*))*)ids

let rec convert_context part_map choices = function
    Cons(ids,t) -> Cons(convert_context_ids part_map choices ids,t)
  | QCons(ids,t,i,a,b,p,r) -> QCons(convert_context_ids part_map choices ids,t,i,a,b,p,r)
  | LVar _ as t -> t
  | Compound(ids,l) ->
       Compound(convert_context_ids part_map choices ids,Xlist.map l (fun (e,t) -> e, convert_context part_map choices t))
  | Set(ids,l) -> Set(convert_context_ids part_map choices ids,Xlist.map l (convert_context part_map choices))
  | Coordination(ids,l,l2) ->
       Coordination(convert_context_ids part_map choices ids,Xlist.map l (convert_context part_map choices),
       Xlist.map l2 (fun (e,t) -> e, convert_context part_map choices t))
  | Loop(ids,t) -> Loop(convert_context_ids part_map choices ids,t)
  | Context l ->
      Context(Xlist.map l (fun (c,t) ->
        XTContext.convert_context part_map choices c
        (*(try XTContext.part_map_find part_map c with Not_found -> failwith ("convert_context: " ^ XTStringOf.context_term c))*), convert_context part_map choices t))

let load_fstructure filename =
  let s = File.load_file filename in
  let p = XTPrologParser.process_prolog_graph s in
  let f = check_empty_context p in
  let p = if f then p else assign_defines p in
  let cvar_map,model_size = XTContext.create_path_array p.p_choices in
  let p = XTPrologParser.model_context cvar_map p in
  let semform_data = if !assign_semform_data_flag then make_semform_data_map p.p_semform_data else StringMap.empty in
  let p = if !assign_semform_data_flag then assign_semform_data semform_data p else p in
  let equi_constraints = equi_closure p in
  let equi_constraints_amb = if f then equi_constraints else equi_closure_amb equi_constraints p in
  let trees = create_constraint_tree_amb equi_constraints_amb p StringSet.empty [] StringMap.empty [CEmpty,LVar "0"] in
  let tree = match trees with [CEmpty,t] -> t | l -> Context l in
  let tree = manage_coordination "" tree in
  let tree = if !assign_punctuation_flag then assign_punctuation p tree else tree in
  let tree = if !assign_prep_cases_flag then assign_prep_cases tree else tree in
  let tree = normalize_root_context (normalize_contexts CEmpty tree) in
  let tree = normalize_cons_contexts model_size CEmpty tree in
  let tree = normalize_root_context (normalize_contexts2 CEmpty tree) in
  let tree = if !disambiguate_flag then XTDisambiguation.disambiguate model_size tree else tree in
  let choices,part_map = create_choices tree in
(*   XTContext.print_part_map part_map; *)
  let choices_map = XTContext.process_choices choices in
  let tree = convert_context part_map choices_map tree in
  let tree = sort_fstructure_attributes tree in
  p.p_sentence,choices,tree,p.p_surfaceform

let count_paths filename =
  let s = File.load_file filename in
  let p = XTPrologParser.process_prolog_graph s in
  Xlist.size (XTContext.create_path_list p.p_choices)
(*   XTContext.fold_paths p.p_choices StringMap.empty 0 (fun n _ -> if n > !max_count_paths then failwith "count_paths" else n+1) *)
