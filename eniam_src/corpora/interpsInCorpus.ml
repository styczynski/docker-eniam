open Xstd
open Types

exception ErrorInfoFile of string

let info_file = "../corpora/info_sentences2.txt"

let info () = Xstring.split "\n\n" @@ File.load_file_gen info_file

let get_info_token info_str =
  match Xstring.split "\n" info_str with
    [id; text; info_token] ->  Xstring.split " " info_token
  | _ -> raise (ErrorInfoFile info_str)

let info_map () =
  Xlist.map (List.tl (info ())) get_info_token

module Interp =
  struct
    type t = int * int * int * int
    let compare a b = -(Pervasives.compare a b)
  end

module InterpMap = Map.Make(Interp)

let isIt patterns x = List.exists (fun y -> y = x) patterns

let count_interps info_token =
  let countIf pred ls = Xlist.fold ls 0 (fun acc x -> if pred x then acc + 1 else acc) in
  info_token,
  (countIf (isIt ["-";"‐";"‑";"‒";"−";"–";"—"]) info_token,
  countIf (isIt [":"]) info_token,
  countIf (isIt ["\"";"˝";"„";"“"]) info_token,
  countIf (isIt ["."]) info_token)

let diagnose () =
  let add_inc map key v =
    try
      InterpMap.add key (v :: (InterpMap.find key map)) map
    with _ -> InterpMap.add key [v] map in
  let counted = Xlist.map (info_map ()) count_interps in
  Xlist.fold counted InterpMap.empty (fun acc (info,count) ->
    add_inc acc count info)

let soi x = string_of_int x

let print_diagnose () =
  let oc = open_out "../../NLP resources/krzaki_interp_statistics.txt" in
  output_string oc ("(myślniki, dwukropki, cudzysłowy, kropki)\n\n");
  flush oc;
  Xlist.iter (InterpMap.bindings (diagnose ())) (fun ((a,b,c,d), infos) ->
  output_string oc ("("^(soi a)^", "^(soi b)^", "^(soi c)^", "^(soi d)^") - "^(soi @@ List.length infos)^"\n" ^
  (String.concat "\n\n" @@ Xlist.map infos (String.concat " ")) ^ "\n\n\n");
  flush oc)
