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

open Printf
open Xstd
(* open LCGtypes *)
(* open ExecTypes *)

let port = 3456

type mode = ENIAM | ALL

let get_sock_addr host_name port =
  let he =
    try Unix.gethostbyname host_name
    with Not_found -> failwith ("get_sock_addr: host " ^ host_name ^ " not found") in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)


let process_query id query mode =
  if mode = ALL then
  begin
    let sock = get_sock_addr "localhost" port in
    let ic,oc =
      try Unix.open_connection sock
      with e -> failwith ("server connection error: " ^ Printexc.to_string e) in
    Printf.fprintf oc "%s\n\n%!" query;
    let text,tokens,lex_sems,msg = (Marshal.from_channel ic : ENIAMexecTypes.text * ENIAMtokenizerTypes.token_env ExtArray.t * ENIAMlexSemanticsTypes.lex_sem ExtArray.t * string) in
    Printf.fprintf oc "\n%!";
    let _ = Unix.shutdown_connection ic in
    if msg <> "" then ENIAMvisualization.print_other_result stdout "" query msg else
    let path = "results/web/" in
    ENIAMvisualization.print_main_result_text path "../../" "" id tokens text;
    ENIAMvisualization.print_main_result_first_page_text path "" path id tokens text
  end

let get_input () =
  let r = ref [] in
  (try
    while true do
      r := (input_line stdin) :: (!r)
    done;
    !r
  with End_of_file -> !r)

let snd_of_triple (_,a,_) = a
let trd_of_triple (_,_,a) = a

let process_data values =
  let exists name = List.exists (fun (a,_,_) -> a = name) values in
  let find name which = try
      which @@ List.find (fun (a,_,_) -> a = name) values 
    with Not_found -> failwith ("missing_" ^ name ^ "_tag") in
  let mode = match find "parsers" snd_of_triple with
      "eniam" -> ENIAM
    | "all" -> ALL
    | _ -> failwith "improper_form_data" in
  find "text0" snd_of_triple, find "text0" trd_of_triple, mode

let rec translate_value_rec buf i size query =
  if i >= size then Buffer.contents buf else (
  let c,i =
    if String.get query i = '%' then
      Scanf.sscanf (String.sub query (i+1) 2) "%x" (fun a -> Char.chr a), i+3 else
    if String.get query i = '+' then ' ', i+1 else
    String.get query i, i+1 in
  Buffer.add_char buf c;
  translate_value_rec buf i size query)

let translate_input = function
    [query] ->
      let data_values = Xstring.split "&" query in
      let translate_value value =
        let buf = Buffer.create (String.length value) in
        translate_value_rec buf 0 (String.length value) value in
      let extract_name data_value = (match Str.bounded_split (Str.regexp "=") data_value 2 with
          [name;value] -> name, translate_value value, value
        | _ ->  failwith "translate_input__extract_name") in
      process_data (Xlist.map data_values extract_name)
  | _ -> failwith "translate_input"

let get_query_id () =
  let filename = Filename.temp_file ~temp_dir:"results/web/" "page_" "" in
  let n = String.length "results/web/" + String.length "page_" in
  let id = String.sub filename n (String.length filename - n) in
  id

let generate_header () =
  Printf.printf "Content-type: text/html\n";
  Printf.printf "\n"

let generate_trailer () = ()

let generate_error_message path id e =
  Printf.printf
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>
<!--   <h1>ENIAM: Kategorialny Parser Składniowo-Semantyczny</h1> -->
    <h3>%s</h3>
    <a target=\"_blank\" href=\"%sparsed_text_%s.html\">Struktura zdania</a>
</center>
  </body>
</html>" e path id

let _ =
  let id = get_query_id () in
    (try
      let query = get_input () in
      let query,text,mode = translate_input query in
      ignore (Sys.command ("echo \"text0=" ^ text ^ "\" | ./parser2.cgi")); (* dopiero tu jest wysyłany header html *)
      process_query id query mode;
    with
      Failure e -> generate_error_message "results/web/" id e
    | e -> generate_error_message "results/web/" id (Printexc.to_string e));
    generate_trailer ()

(* testowanie z linii poleceń:
echo "text0=Ala ma kota. Ela ma psa." | ./parser4.cgi
echo "text0=Szpak powiedział: „Frunę. Kiszę.”" | ./parser4.cgi
*)
