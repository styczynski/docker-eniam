(*Module allows basic operations on xml sources*)

let before_split s before =
  let sp = Str.split (Str.regexp before) s
  in
    match sp with
    | h::t -> h
    | [] -> failwith "Segmentation.before_split: wrong input"

let after_split s after =
  let rec last_element l =
    match l with
    | h::[] -> h
    | h::t -> last_element t
    | [] -> failwith ("Segmentation.after_split: wrong input" ^ s^ " " ^ after)
  in
  let sp = Str.split (Str.regexp after) s in
    last_element sp


(*extract searches from given xml node - source - for a sequence of subnodes
having identifiers as given in path. returns children of these subnodes
dependencies: Xml, List *)
let extract (source:Xml.xml) (path:string list) =
  let rec temp (tocheck:Xml.xml list) (curpath:string list) (result:Xml.xml list)=
  (*tocheck - children of current xml node that have to be checked
    curpath - path left to be found
    result - upcoming children or result*)
    if curpath = [] then tocheck
    else
      match tocheck with
      | [] -> temp result (List.tl curpath) []
      | h::t ->
        match h with
        | Xml.Element(id, _, toadd) when id = (List.hd curpath) ->
          temp t curpath (toadd @ result)
        | _ -> temp t curpath result
  in temp [source] path []

(*go_to searches from given xml node - source - for a sequence of subnodes
having identifiers as given in path. returns these subnodes
dependencies: Xml, List, Xmlbasics.extract *)
let go_to (source:Xml.xml) (path:string list) =
  let revpath = List.rev path in
  let condition = List.hd revpath in
  let extpath = List.rev (List.tl revpath) in
    List.filter
      (function
      | Xml.Element(id, _, _) when id = condition -> true
      | _ -> false)
      (extract source extpath)

(*find_attribute searches for attribute in node and returns string*)
let find_attribute (source:Xml.xml) (att:string) =
  match source with
  | Xml.Element(_, l, _) ->
    (match List.filter (fun x -> fst x = att) l with
    | h::t -> snd h
    | _ -> "";)
  | _ -> failwith ("find_attribute: "^att)

(*find_text returns for given node all pcdata in it*)
let find_text (source:Xml.xml) =
  let rec temp curlist =
    match curlist with
    | [] -> None
    | hdlist::tllist ->
      match hdlist with
      | Xml.Element(_, _, templist) ->
        begin
          match temp templist, temp tllist with
          | None, x
          | x, None -> x
          | Some x, Some y -> Some (x ^ y)
        end
      | Xml.PCData s -> Some s
  in temp [source]

(*print_xml_list prints list of Xml.xml elements using Xml.to_string_fmt
dependencies: Xml*)
let rec print_xml_list (l:(Xml.xml list)) =
  match l with
  | [] -> ()
  | hd::tl ->
    print_endline (Xml.to_string_fmt hd);
    print_xml_list tl

let tag_name x =
  match x with
  | Xml.Element(a, _, _) -> a
  | Xml.PCData _ -> ""
