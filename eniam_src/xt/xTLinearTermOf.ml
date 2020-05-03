open ENIAM_LCGtypes
open XTTypes
open Xstd

let unused = ref StringMap.empty

let possible_args_labels = ["ADJUNCT";"APP";"POSS";"XADJUNCT"]

let empty_node = {
  orth=""; lemma=""; pos=""; weight=0.; id=0; symbol=Dot; arg_symbol=Dot; arg_dir=""; attrs=[]; args=Dot;}

let retuple a =
  let rec retuple_helper l acc = Xlist.fold l acc (fun acc' -> function
    | Tuple l' -> retuple_helper l' acc'
    | s -> s :: acc) in
  let tuple_list = List.rev @@ retuple_helper a [] in
  if List.length tuple_list = 0
  then Dot
  else if List.length tuple_list = 1
    then List.hd tuple_list
    else Tuple(List.rev @@ retuple_helper a [])

let rec value_of_lfg = function
  | Cons(_,s) -> s
  | QCons(_,s,_,_,_,_,_) -> s
  | Compound(_,l) -> String.concat "; " (Xlist.map l (fun (k,v) -> k ^ "," ^ (value_of_lfg v)))
  | _ -> failwith "value_of_lfg"

let id_of_lfg = function
  | QCons(_,_,i,_,_,_,_) -> int_of_string i
  | _ -> (* failwith "id_of_lfg" *) -1

let args_of_pred = function
  | QCons(_,_,_,l,l2,_,_) -> Xlist.map (l2 @ l) value_of_lfg
  | _ -> failwith "args_of_pred"

let omit_first l = Xlist.map l (fun (_,y) -> y)

let from_context_term c = XTStringOf.context_term c

let of_label tmplts = List.partition (fun (x,_) -> List.mem x tmplts)

let add_cat cat attrs = ("CHECK", Cons(StringMap.empty,cat)) :: attrs

let get_cat = function
  | Cons(_,s) -> s, []
  | Compound(_,(_,lfg)::tl) -> value_of_lfg lfg, tl
  | _ -> failwith "get_cat"

let get_attr_labels = function
  | "adj" -> ["ATYPE";"DEGREE";"CASE";"GEND";"NUM"]
  | "adv" -> ["DEGREE"]
  | "fin" -> ["TNS-ASP"]
  | "pcon" -> ["TNS-ASP"]
  | "praet" -> ["TNS-ASP"]
  | "prep" -> ["SEM"; "INST"]
  | "pro" -> ["CASE";"NUM";"PERS"]
  | "pron" -> ["NTYPE";"CASE";"GEND";"NUM";"PERS"]
  | "subst" -> ["NTYPE";"CASE";"GEND";"NUM";"PERS"]
(*  | s -> failwith ("get_attr_labels:" ^ s) *)
  | _ -> []

let attrs_to_stay = ["ACM";"CASE";"CLAUSE-TYPE";"DEGREE";"GEND";"INST";"NEG";"NTYPE";"NUM";"PERS";"PTYPE";"SEM";"TNS-ASP";"TYPE"]
let attrs_to_omit = ["_ACC";"ATYPE";"CORRELATIVE";"IMPERSONAL";"PASSIVE";"_PPREP";"_PREDICATIVE";"REFLEXIVE";"_RQR";"_VOC"]

exception OmitAttr

let translate_attr = function
  | "ACM",v -> "ACM",v
  | "ASPECT",v -> "ASPECT",v
  | "CASE",v -> "CASE",v
  | "CLAUSE-TYPE",v -> (match v with
      | "imp" -> "MOOD", "imperative"
      | s -> "CTYPE",s)
  | "DEGREE",v -> ("GRAD",match v with
      | "positive" -> "pos"
      | "comparative" -> "com"
      | "superlative" -> "sup"
      | s -> s)
  | "GEND",v -> "GEND",v
  | "MOOD",v -> "MOOD",v
  | "NEG",v -> ("NEGATION",match v with
      | "+" -> "aff"
      | s -> s)
  | "NSEM",v -> ("NSEM",match v with
      | "gerund" -> raise OmitAttr
      | s -> s)
  | "NSYN",v -> "NSYN",v
  | "NUM",v -> "NUM",v
  | "PCASE",v -> ("CASE", match v with
      | "adjp" -> "?"
      | s -> s)
  | "PERS",v -> ("PERS",match v with
      | "1" -> "pri"
      | "2" -> "sec"
      | "3" -> "ter"
      | s -> s)
  | "PTYPE",v -> "PSEM",v
  | "TENSE",v -> "TENSE",v
  | "TYPE",v -> "TYPE?",v
  | l -> l

let rec to_attrs acc (label,lfg) =
  let parse_one (label,lfg) = 
    let a,b = translate_attr (label,value_of_lfg lfg) in
    a, Var b in
  match lfg with
  | Cons(_,_) -> (try (parse_one (label,lfg)) :: acc with OmitAttr -> acc)
  | Compound(_,l) -> Xlist.fold l acc to_attrs
  | _ -> failwith "to_attrs"

let rec get_all_labels pattern acc (label,lfg) =
  let parse_one (label,lfg) = translate_attr (label,value_of_lfg lfg) in
  match lfg with
  | Cons(_,_) -> let a,b = parse_one (label,lfg) in (pattern ^ a,b) :: acc
(*  | QCons(_,_,_,_,_,_,_) -> (pattern ^ label) :: acc *)
  | Compound(_,l) -> Xlist.fold l acc (get_all_labels (pattern ^ label ^ "|"))
  | _ -> failwith ("get_all_labels:" ^ pattern ^ label)

let rec to_node pred pre_attrs args =
  try
    let ch,rest = of_label ["CHECK"] pre_attrs in
    let cat, other_attrs = match ch with
      | [] -> "pro",[]
      | [_,check] -> get_cat check
      | _ -> failwith "to_node__check" in
    let for_attrs = other_attrs @ rest in
    let for_attrs = Xlist.filter for_attrs (fun (x,_) -> not @@ Xlist.mem attrs_to_omit x) in
(*    let rest = List.filter (fun x -> not (List.mem x for_attrs)) (other_attrs @ rest) in
    let rest_labels = Xlist.fold rest [] (get_all_labels "") in
    let _ = print_endline @@ cat ^ ":" ^ (String.concat "\t" rest_labels) in
    unused := Xlist.fold rest_labels !unused (fun acc x -> StringMap.add_inc acc cat (StringSet.singleton x) (fun t ->
               StringSet.add t x)); *)
    let stat_labels = Xlist.fold for_attrs [] (get_all_labels "") in
    let _ = print_endline (String.concat "\t" (Xlist.map stat_labels (fun (x,y) -> x ^ ":" ^ y))) in
    unused := Xlist.fold stat_labels !unused (fun acc (l,v) -> StringMap.add_inc acc l (StringSet.singleton v) (fun t ->
               StringSet.add t v));
    let pf, rest = of_label ["PFORM"] rest in
    if List.length pf = 1
    then
      let pform = match pf with
        | [_,lfg] -> lfg
        | _ -> failwith "to_node__pform" in
      let attrs, rest = of_label ["PCASE"; "PTYPE"] rest in
      let attrs = add_cat "pform" attrs in
      to_node pform attrs (to_node pred rest args)
    else
      let cf, rest = of_label ["COMP-FORM"] rest in
      if List.length cf = 1
      then
        let cform = match cf with
          | [_,lfg] -> lfg
          | _ -> failwith "to_node__cform" in
        let attrs = add_cat "comp-form" [] in
        to_node cform attrs (to_node pred rest args)
      else
        let attrs = Xlist.fold for_attrs [] to_attrs in
        Node{empty_node with lemma = value_of_lfg pred;
                         id = id_of_lfg pred;
                         attrs = attrs;
                         args = args}
  with
  | e -> failwith (* Var *) ("to_node(" ^ (Printexc.to_string e) ^ ")")

let rec split_cvar_rec n s =
  if String.get s n >= 'A' && String.get s n <= 'Z' then
    split_cvar_rec (n+1) s
  else n

let determine_cvar_beg l =
  let (c_term,_) = try List.hd l with _ -> failwith "determine_cvar_beg__empty_l" in
  let label = from_context_term c_term in
  let n = split_cvar_rec 0 label in
  let cvar_beg = String.sub label 0 n in
  let _ = Xlist.iter l (fun (label,_) -> let label = from_context_term c_term in
                         if String.sub label 0 n = cvar_beg then failwith "determine_cvar_beg") in
  n, cvar_beg

let rec from_lfg_term = function
    Cons(ids,s) -> Var "cons"
  | QCons(ids,s,i,l,l2,p,r) as q -> Var ("qcons: " ^ (XTStringOf.lfg_term q))
  | LVar s -> Var "lvar"
  | Compound(ids,l) ->
      let p, rest = of_label ["PRED"] l in
      let pred = match p with
        | [_,pred] -> pred
        | _ -> failwith "to_node__pred" in
      let for_args_labels = possible_args_labels @ (args_of_pred pred) in
      let for_args, for_node = of_label for_args_labels rest in
(*      let rest, for_node = of_label ["PUN"] rest in
      let _ = print_endline (String.concat "\t" (Xlist.map rest (fun (x,_) -> x))) in
*)      let for_args = omit_first for_args in
      let args = Xlist.map for_args from_lfg_term in
      let args = retuple args in (* untuple [a;Tuple([b;Tuple([c;d]);e])] = Tuple[a;b;c;d;e]*)
      to_node pred for_node args
  | Set(ids,l) -> Tuple(Xlist.map l from_lfg_term)
  | Coordination(ids,for_args,attrs) ->
      let cf, attrs = of_label ["COORD-FORM"] attrs in
      let result = match cf with
        | [_,cform] ->
          let attrs = add_cat "conj" attrs in
          let args = Xlist.map for_args from_lfg_term in
          to_node cform attrs (Tuple args)
        | [] -> from_lfg_term (Set(ids,for_args))
        | _ -> failwith "from_lfg_term__coordination" in
      result
  | Loop(ids,path) -> Dot
  | Context l -> let n, cvar_beg = (* determine_cvar_beg l *) 0, "HERE HERE" in
          Variant(cvar_beg,Xlist.map l (fun (e,v) ->
             let full_label = from_context_term e in
             let label = String.sub full_label n (String.length full_label - n) in
             (label,from_lfg_term v)))
(*  | t -> failwith ("from_lfg_term:\n" ^ (XTStringOf.lfg_term t)) *)

let print_unused () =
  StringMap.iter !unused (fun key set ->
    print_endline (key ^ ":\n\t" ^ (String.concat "\t" @@ StringSet.to_list set)))
