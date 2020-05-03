open XTTypes
open Xstd

let c_term_info = function
    CVar(v,w) -> v,w
  | CEmpty -> "", ""
  | _ -> failwith "c_term_info"

let value_of_lfg = function
  | Cons(_,s) -> s
  | QCons(_,s,_,_,_,_,_) -> s
  | _ -> failwith "value_of_lfg"

let find_args = function
  | QCons(_,_,_,con_args,_,_,_) -> Xlist.map con_args value_of_lfg
  | _ -> failwith "find_args"

let unpack_set acc = function
  | Set(_,l) -> Xlist.fold l acc (fun acc x -> x :: acc)
  | _ -> failwith "unpack_set"

let split_lfgs l =
 let label, main = List.hd l in
 let args_labels = find_args main in
 let pre_args, pre_tags = List.partition (fun (label,_) -> List.mem label args_labels) (List.tl l) in
 let adjuncts, tags = List.partition (fun (label,_) -> label = "ADJUNCT") pre_tags in
 let adjuncts = Xlist.map adjuncts (fun (_,arg) -> arg) in
 let args = Xlist.map pre_args (fun (_,arg) -> arg) in
 main, tags, Xlist.fold adjuncts args unpack_set

let create_node main tags id super =
  (string_of_int id) ^ "\t" ^ (value_of_lfg main) ^ "\t" ^ (string_of_int super)

let rec compound_to_nodes super (acc,id) = function
  | Compound(ids,l) ->
      let main, tags, args = split_lfgs l in
      Xlist.fold args ((create_node main tags id super) :: acc,id+1) (compound_to_nodes id)
  | _ -> failwith "compound_to_nodes"

let variant_to_nodes (c_term,lfg_term) =
  let name, num = c_term_info c_term in
  let nodes, _ = compound_to_nodes 0 ([],1) lfg_term in
  print_endline ("Name: " ^ name ^ ", Num:" ^ num);
  List.iter print_endline nodes;
  print_newline ();
  nodes

let lfg_to_conll  = function
  | Compound(ids,l) as c -> [variant_to_nodes (CEmpty,c)]
  | Context l -> Xlist.map l variant_to_nodes
  | _ -> failwith "context_to_conll"
