open Xstd

(* type 'a nodes = 'a IntMap.t *)
type 'a edges = 'a IntMap.t IntMap.t
type 'a tree = Tree of int * 'a * 'a tree list | Visited of int * 'a


(* Klucze zewnętrznego słownika określamy jako parent,
   klucze wewnętrznego słownika określamy jako child. *)

let empty = IntMap.empty

let add_new r parent child v =
  let children = try IntMap.find r parent with Not_found -> IntMap.empty in
  if IntMap.mem children child then
    failwith ("add_new: " ^ string_of_int parent ^ "-->" ^ string_of_int child ^ " is already in graph")
  else
    let children = IntMap.add children child v in
    IntMap.add r parent children

let add r parent child v =
  let children = try IntMap.find r parent with Not_found -> IntMap.empty in
  let children = IntMap.add children child v in
  IntMap.add r parent children

let add_inc r parent child v merge_fun =
  let children = try IntMap.find r parent with Not_found -> IntMap.empty in
  let children = IntMap.add_inc children child v (merge_fun v) in
  IntMap.add r parent children

let mem_parent r parent =
  IntMap.mem r parent

let rec find_descendants_rec r descendants parent =
  if IntSet.mem descendants parent then descendants else
  let descendants = IntSet.add descendants parent in
  let children = try IntMap.find r parent with Not_found -> IntMap.empty in
  IntMap.fold children descendants (fun descendants child _ -> find_descendants_rec r descendants child)

let find_descendants r parent =
  IntSet.remove (find_descendants_rec r IntSet.empty parent) parent

let test_reverse ra rb =
  IntMap.iter ra (fun parent_a children_a ->
    IntMap.iter children_a (fun parent_b v ->
      try
        let children_b = IntMap.find rb parent_b in
        if IntMap.mem children_b parent_a then ()
        else print_endline ("test_reverse_rel a: " ^ string_of_int parent_a)
      with Not_found -> print_endline ("test_reverse_rel b: " ^ string_of_int parent_b)))

let reverse r =
  IntMap.fold r IntMap.empty (fun r parent children ->
    IntMap.fold children r (fun r child v ->
      add r child parent v))

let sum ra rb merge_fun =
  IntMap.fold rb ra (fun r parent children ->
    IntMap.fold children r (fun r child v ->
      add_inc r parent child v merge_fun))

let select_childless selected r =
  IntMap.fold r selected (fun selected parent _ ->
    IntSet.remove selected parent)

let select_founders selected r =
  IntMap.fold r selected (fun selected _ children ->
    IntMap.fold children selected (fun selected child _ ->
      IntSet.remove selected child))

let rec descendants_tree_rec r visited parent v =
  if IntSet.mem visited parent then visited, Visited(parent,v) else
  let visited = IntSet.add visited parent in
  let children = try IntMap.find r parent with Not_found -> IntMap.empty in
  let visited, l = IntMap.fold children (visited, []) (fun (visited,l) child v ->
    let visited,tree = descendants_tree_rec r visited child v in
    visited, tree :: l) in
  visited, Tree(parent,v,l)

let descendants_tree r parent v =
  snd (descendants_tree_rec r IntSet.empty parent v)

let rec create_spaces n =
  if n = 0 then "" else " " ^ (create_spaces (n - 1))

let rec print_tree_rec file node_fun level = function
    Tree(parent,cost,children) ->
      Printf.fprintf file "%s%s\n" (create_spaces (2*level)) (node_fun parent cost);
      Xlist.iter children (print_tree_rec file node_fun (level+1))
  | Visited(parent,cost) ->
      Printf.fprintf file "%sVISITED %s\n" (create_spaces (2*level)) (node_fun parent cost)

let print_tree file tree node_fun =
  print_tree_rec file node_fun 0 tree

let rec create_tree_xml file node_fun = function
    Tree(parent,cost,children) ->
      Xml.Element("node",node_fun parent cost,Xlist.map children (create_tree_xml file node_fun))
  | Visited(parent,cost) ->
      Xml.Element("node",node_fun parent cost,[])

let print_tree_xml file tree node_fun =
  let xml = create_tree_xml file node_fun tree in
  Printf.fprintf file "%s" (Xml.to_string_fmt xml)

let rec print_tree_as_graph_rec file node_fun edge_fun grand_parent = function
    Tree(parent,cost,children) ->
      Printf.fprintf file "  %d [label=\"%s\"]\n" parent (node_fun parent);
      if grand_parent <> (-1) then Printf.fprintf file "  %d -> %d [label=\"%s\"]\n" grand_parent parent (edge_fun cost);
      Xlist.iter children (print_tree_as_graph_rec file node_fun edge_fun parent)
  | Visited(parent,cost) ->
      Printf.fprintf file "  %d [label=\"%s\"]\n" parent (node_fun parent);
      if grand_parent <> (-1) then Printf.fprintf file "  %d -> %d [label=\"%s\"]\n" grand_parent parent (edge_fun cost)


let print_tree_as_graph path name tree node_fun edge_fun =
  File.file_out (path ^ name ^ ".gv") (fun file ->
    (*if lr then Printf.fprintf file "digraph G {\n  node [shape=box]\n  rankdir = LR\n"
    else*) Printf.fprintf file "digraph G {\n  node [shape=box]\n";
    print_tree_as_graph_rec file node_fun edge_fun (-1) tree;
    Printf.fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng " ^ name ^ ".gv -o " ^ name ^ ".png"));
  Sys.chdir "..";
  ()

let print file r cost_fun =
  IntMap.iter r (fun parent children ->
    IntMap.iter children (fun child cost ->
      Printf.fprintf file "%d\t%d\t%s\n" parent child (cost_fun cost)))

let rec find_families_rec r visited conn parent =
  (* if parent = "28358" then print_endline "find_families_rec 1"; *)
  if IntSet.mem visited parent then conn,visited else (
  (* if parent = "28358" then print_endline "find_families_rec 2"; *)
  let conn = IntSet.add conn parent in
  let visited = IntSet.add visited parent in
  let children = try IntMap.find r parent with Not_found -> IntMap.empty in
  (* if parent = "28358" then Printf.printf "find_families_rec 3: |conn|=%d\n%!" (IntSet.size conn); *)
  IntMap.fold children (conn,visited) (fun (conn,visited) child _ ->
    find_families_rec r visited conn child))

let find_families r =
  let l,_ = IntMap.fold r ([],IntSet.empty) (fun (l,visited) parent _ ->
    if IntSet.mem visited parent then l,visited else
    let conn,visited = find_families_rec r visited IntSet.empty parent in
    (* if IntSet.mem conn "28358" then print_endline "find_families 1"; *)
    conn :: l, visited) in
  l

let get_all_members r =
  IntMap.fold r IntSet.empty (fun members parent children ->
    IntMap.fold children (IntSet.add members parent) (fun members child _ -> IntSet.add members child))

let print_graph path name lr r node_fun edge_fun =
  let members = get_all_members r in
  let founders = select_founders members r in
  File.file_out (path ^ name ^ ".gv") (fun file ->
    if lr then Printf.fprintf file "digraph G {\n  node [shape=box]\n  rankdir = LR\n"
    else Printf.fprintf file "digraph G {\n  node [shape=box]\n";
    IntSet.iter members (fun member ->
      if IntSet.mem founders member then
        Printf.fprintf file "  %d [color=\".7 .3 1.0\",style=filled,label=\"%s\"]\n" member (node_fun member)
      else Printf.fprintf file "  %d [label=\"%s\"]\n" member (node_fun member));
    IntMap.iter r (fun parent nodes ->
      IntMap.iter nodes (fun child cost ->
        Printf.fprintf file "  %d -> %d [label=\"%s\"]\n" parent child (edge_fun cost)));
    Printf.fprintf file "}\n");
  Sys.chdir path;
  ignore (Sys.command ("dot -Tpng \"" ^ name ^ ".gv\" -o \"" ^ name ^ ".png\""));
  Sys.chdir "..";
  ()

let select r selector =
  IntMap.fold r IntMap.empty (fun r parent children ->
    IntMap.fold children r (fun r child v ->
      if selector parent child v then add r parent child v else r))
