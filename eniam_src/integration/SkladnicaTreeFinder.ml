(*
18702 plików
7708 znalezionych drzew *)
open SkladnicaTypes

(* module JustString =
       struct
         type t = string
         let compare a b =
           Pervasives.compare a b
        end

module StringSet = Set.Make(JustString)

let m = ref StringSet.empty
let oc = open_out "ChildrenTypes.txt" *)

let are_true_children = function
    ChoiceDef(_,true,_) -> true
  | _ -> false

let get_childlist = function
    ChoiceDef(_,_,ch) -> ch
  | _ -> failwith "get_childlist"

let add_child child = function
    Node(from,sto,fs,head,children) -> Node(from,sto,fs,head,child::children)
  | _ -> failwith "add_child"

let make_node h = function
    Forest(_,_,_,_,_,nodes) -> List.nth nodes h.nid
  | _ -> failwith "make_node"

let is_node_chosen = function
    Nonterminal(atr,_,_,_) -> atr.chosen
  | Terminal(atr,_,_,_,_) -> atr.chosen

let make_tree h forest =
  let node = make_node h forest in
  match node with
    Nonterminal(atr,text,fs,_) -> Node(atr.from,atr.sto,[text,fs],[],[])
  | Terminal(atr,a,b,c,d) -> Leaf(atr.from,atr.sto,a,b,c,d)

let make_tree_from_node = function
    Nonterminal(atr,text,fs,_) -> Node(atr.from,atr.sto,[text,fs],[],[])
  | Terminal(atr,a,b,c,d) -> Leaf(atr.from,atr.sto,a,b,c,d)

let rec establish_head tp childlist forest =
  let decide_priority forest = function
      [a; b] ->
        let g h = expand_tree (make_tree h forest) forest (make_node h forest) in

        let rec get_base = function
          Node(_,_,_,[head],_) -> get_base head
        | Leaf(_,_,_,_,base,_) -> base
        | _ -> failwith "decide_priority__get_base" in

        let base_a = get_base (g a) and base_b = get_base (g b) in
          if (base_a = "być" && base_b <> "to") || base_a = "by" || base_a = "to"
          then [b], [a]
          else [a], [b]
    | _ -> failwith "decide_priority" in

  if tp = "formaczas" && List.length childlist = 2 && List.for_all (fun h -> h.head) childlist
    then
      decide_priority forest childlist
    else
      if tp = "formarzecz" && List.length childlist = 2 && List.for_all (fun h -> h.head) childlist
      then
        [List.hd childlist], List.tl childlist
      else
        if List.length childlist = 1
        then
          childlist, []
        else
          let a, b = List.partition (fun h -> h.head) childlist in
          if List.length a > 1
            then
              failwith "decide_priority/ManyHeads"
            else
              a, b

and add_headchild head forest = function
    Node(from,sto,fs,[],children) ->
      let child = expand_tree (make_tree head forest) forest (make_node head forest) in
      if head.from = from && head.sto = sto
        then match make_node head forest with
            Nonterminal(_,text,nodefs,_) -> expand_tree (Node(from,sto,fs@[text,nodefs],[],children)) forest (make_node head forest)
          | Terminal(_,_,_,_,_) -> expand_tree (Node(from,sto,fs,[child],[])) forest (make_node head forest)
        else Node(from,sto,fs,[child],children)
  | _ -> failwith "add_headchild"

(* and add_to_set tp childlist head forest =
  let string_of_node = function
      Nonterminal(_,str,_,_) -> str
    | Terminal(_,_,_,_,_) -> "terminal" in
  let str_nodelist = String.concat " " @@ List.map (fun ch -> string_of_node @@ make_node ch forest) childlist in
  let str_head = string_of_node @@ make_node head forest in
  let str = tp ^ " (" ^ str_nodelist ^ "): " ^ str_head in
  print_endline str;
  m := StringSet.add str !m *)

and expand_tree tree forest = function
    Nonterminal(_,tp,_,ch) ->
      begin
        try
          let childlist = get_childlist (List.find are_true_children ch) in
          let h, children = establish_head tp childlist forest in
          let head = match h with
              [a] -> a
            | _ -> failwith "expand_tree__head" in
          (* add_to_set tp childlist head forest; *)
          let new_tree = add_headchild head forest tree in
          List.fold_right (fun h acc -> add_child (expand_tree (make_tree h forest) forest (make_node h forest)) acc) children new_tree
        with
        _ -> failwith "expand_tree"
      end
  | Terminal(_,_,_,_,_) -> tree

let get_tree filename = function
    NoTree(_,_,_,_) -> TreeNotFound
  | Forest(atr,text,start,stats,answer,nodes) as forest ->
      if answer.base_answer.tree_type <> FULL
      then TreeNotFound
      else
        try
          expand_tree (make_tree_from_node (List.hd nodes)) forest (List.hd nodes)
        with
        _ -> TreeNotFound

let text_of_forest = function
    Forest(_,text,_,_,_,_) -> text
  | NoTree(_,_,_,_) -> "NoTree"
