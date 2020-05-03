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
(* open LCGtypes *)
(* open ExecTypes *)

let get_sock_addr host_name port =
  let he =
    try Unix.gethostbyname host_name
    with Not_found -> failwith ("get_sock_addr: host " ^ host_name ^ " not found") in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

(* let ala = Relation(Val "Initiator",Val "",Concept{c_sense=Val "osoba 1"; c_name=Val "Ala"; c_local_quant=false; c_quant=Val "sg"; c_relations=Dot; c_variable="a",""; c_pos=1})
let kot = Relation(Val "Theme",Val "",Concept{c_sense=Val "kot 1"; c_name=Dot; c_local_quant=false; c_quant=Val "sg"; c_relations=Dot; c_variable="k",""; c_pos=3})
let ala_ma_kota n = Context{cx_contents=Concept{c_sense=Val ("mieć " ^ n); c_name=Dot; c_local_quant=false; c_quant=Dot; c_variable="m",""; c_pos=2; c_relations=Tuple[ala;kot]};
                            cx_sense=Dot; cx_relations=Dot; cx_variable="x",""; cx_pos=2} *)

let process_query id query =
  let sock = get_sock_addr (*"wloczykij"*)"localhost" (*"mozart.ipipan.waw.pl"*) 2345(*Paths.server_host Paths.server_port*) in
  let ic,oc =
    try Unix.open_connection sock
    with e -> failwith ("server connection error: " ^ Printexc.to_string e) in
  Printf.fprintf oc "%s\n\n%!" query;
  let text,tokens,lex_sems,msg = (Marshal.from_channel ic : ENIAMexecTypes.text * ENIAMtokenizerTypes.token_env ExtArray.t * ENIAMlexSemanticsTypes.lex_sem ExtArray.t * string) in
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  if msg <> "" then ENIAMvisualization.print_other_result stdout "" query msg else
  let path = "results/web/" in
  let id = "_" ^ id in
  ENIAMvisualization.print_main_result_text "../../" path id tokens text;
  ENIAMvisualization.print_main_result_first_page_text "" "results/web/" id tokens text

let get_input () =
  let r = ref [] in
  (try
    while true do
      r := (input_line stdin) :: (!r)
    done;
    !r
  with End_of_file -> !r)

let rec translate_input_rec buf i size query =
  if i >= size then Buffer.contents buf else (
  let c,i =
    if String.get query i = '%' then
      Scanf.sscanf (String.sub query (i+1) 2) "%x" (fun a -> Char.chr a), i+3 else
    if String.get query i = '+' then ' ', i+1 else
    String.get query i, i+1 in
  Buffer.add_char buf c;
  translate_input_rec buf i size query)

let translate_input query =
  match query with
    [query] ->
      if String.sub query 0 6 = "text0=" then
        let buf = Buffer.create (String.length query) in
        translate_input_rec buf 6 (String.length query) query
      else failwith "translate_input 1"
  | _ -> failwith "translate_input 2"

let get_query_id () =
  let filename = Filename.temp_file ~temp_dir:"results/web/" "page_" "" in
(*   print_endline filename; *)
  let n = String.length "results/web/" + String.length "page_" in
  let id = String.sub filename n (String.length filename - n) in
(*   print_endline id; *)
  id

let generate_header () =
  Printf.printf "Content-type: text/html\n";
  Printf.printf "\n"
(*(*   Printf.printf "HTTP/1.1 301 Moved Permanently\n"; *)
  Printf.printf "Status: 301 Moved Permanently\n";
  Printf.printf "Location: http://students.mimuw.edu.pl/~wjaworski/cgi-bin/results/web/page_%s_1.html\n" id;
  Printf.printf "Content-Type: text/html\n";
(* Content-Length: 174 *)
  Printf.printf "\n"*)

let generate_trailer () =
  (*Printf.printf "</BODY>\n</HTML>\n"*)()

let generate_error_message e =
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
   <h1>ENIAM: Kategorialny Parser Składniowo-Semantyczny</h1>
    <h3>%s</h3>
</center>
  </body>
</html>" e

let _ =
  generate_header ();
  (try
    let query = get_input () in
    let query = translate_input query in
    let id = get_query_id () in
    process_query id query;
  with
    Failure e -> generate_error_message e
  | e -> generate_error_message (Printexc.to_string e));
  generate_trailer ()

(* testowanie z linii poleceń:
echo "text0=Ala ma kota. Ela ma psa." | ./parser3.cgi
echo "text0=Szpak powiedział: „Frunę. Kiszę.”" | ./parser3.cgi
*)
