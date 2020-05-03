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

let port = 2345

type mode =
  ENIAM_SYN_000 | ENIAM_SYN_001 | ENIAM_SYN_010 | ENIAM_SYN_011 |
  ENIAM_SYN_100 | ENIAM_SYN_101 | ENIAM_SYN_110 | ENIAM_SYN_111 |
  ENIAM_SEM_000 | ENIAM_SEM_001 | ENIAM_SEM_010 | ENIAM_SEM_011 |
  ENIAM_SEM_100 | ENIAM_SEM_101 | ENIAM_SEM_110 | ENIAM_SEM_111 |
  ALL_SEPAR_SYN | ALL_SEPAR_SEM |
  ALL_MERGE_SYN | ALL_MERGE_SEM

let mode_to_port = function
    ENIAM_SYN_000 -> port + 0
  | ENIAM_SYN_001 -> port + 1
  | ENIAM_SYN_010 -> port + 2
  | ENIAM_SYN_011 -> port + 3
  | ENIAM_SYN_100 -> port + 4
  | ENIAM_SYN_101 -> port + 5
  | ENIAM_SYN_110 -> port + 6
  | ENIAM_SYN_111 -> port + 7
  | ENIAM_SEM_000 -> port + 8
  | ENIAM_SEM_001 -> port + 9
  | ENIAM_SEM_010 -> port + 10
  | ENIAM_SEM_011 -> port + 11
  | ENIAM_SEM_100 -> port + 12
  | ENIAM_SEM_101 -> port + 13
  | ENIAM_SEM_110 -> port + 14
  | ENIAM_SEM_111 -> port + 15
  | ALL_SEPAR_SYN -> port + 16
  | ALL_SEPAR_SEM -> port + 17
  | ALL_MERGE_SYN -> port + 18
  | ALL_MERGE_SEM -> port + 19

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

let process_query id query mode =
  let sock = get_sock_addr (*"wloczykij"*)"localhost" (*"mozart.ipipan.waw.pl"*) (mode_to_port mode) (*Paths.server_host Paths.server_port*) in
  let _ = print_int (mode_to_port mode) in
  let ic,oc =
    try Unix.open_connection sock
    with e -> failwith ("server connection error: " ^ Printexc.to_string e) in
  Printf.fprintf oc "%s\n\n%!" query;
  let text,tokens,lex_sems,msg = (Marshal.from_channel ic : ENIAMexecTypes.text * ENIAMtokenizerTypes.token_env ExtArray.t * ENIAMlexSemanticsTypes.lex_sem ExtArray.t * string) in
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  if msg <> "" then ENIAMvisualization.print_other_result stdout "" query msg else
  let path = "results/web/" in
  ENIAMvisualization.print_html_text path ("parsed_text_" ^ id) text 1 2 tokens;
  ENIAMvisualization.print_main_result_text path "../../" "" id tokens text;
  ENIAMvisualization.print_main_result_first_page_text path "" path id tokens text

let get_input () =
  let r = ref [] in
  (try
    while true do
      r := (input_line stdin) :: (!r)
    done;
    !r
  with End_of_file -> !r)

let process_data values =
  let exists name = List.exists (fun (a,b) -> a = name) values in
  let find name = try
      snd @@ List.find (fun (a,b) -> a = name) values 
    with Not_found -> failwith ("missing_" ^ name ^ "_tag") in
  let mode = match find "parsers", find "analysis", exists "discontinuity", exists "partial", exists "desamb" with
    "eniam", "syn", false, false, false -> ENIAM_SYN_000
  | "eniam", "syn", false, false, true  -> ENIAM_SYN_001
  | "eniam", "syn", false, true, false  -> ENIAM_SYN_010
  | "eniam", "syn", false, true, true   -> ENIAM_SYN_011
  | "eniam", "syn", true, false, false  -> ENIAM_SYN_100
  | "eniam", "syn", true, false, true   -> ENIAM_SYN_101
  | "eniam", "syn", true, true, false   -> ENIAM_SYN_110
  | "eniam", "syn", true, true, true    -> ENIAM_SYN_111
  | "eniam", "sem", false, false, false -> ENIAM_SEM_000
  | "eniam", "sem", false, false, true  -> ENIAM_SEM_001
  | "eniam", "sem", false, true, false  -> ENIAM_SEM_010
  | "eniam", "sem", false, true, true   -> ENIAM_SEM_011
  | "eniam", "sem", true, false, false  -> ENIAM_SEM_100
  | "eniam", "sem", true, false, true   -> ENIAM_SEM_101
  | "eniam", "sem", true, true, false   -> ENIAM_SEM_110
  | "eniam", "sem", true, true, true    -> ENIAM_SEM_111
  | "separ", "syn", false, false, false -> ALL_SEPAR_SYN
  | "separ", "sem", false, false, false -> ALL_SEPAR_SEM
  | "merge", "syn", false, false, false -> ALL_MERGE_SYN
  | "merge", "sem", false, false, false -> ALL_MERGE_SEM
  | a,b,c,d,e -> failwith (String.concat "_" ["improper_form_data:"^a;b;string_of_bool c;string_of_bool d;string_of_bool e]) in
  find "text0", mode

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
          [name;value] -> name, translate_value value
        | _ ->  failwith "translate_input__extract_name") in
      process_data (Xlist.map data_values extract_name)
  | _ -> failwith "translate_input"

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
  generate_header ();
  let id = get_query_id () in
    (try
      let query = get_input () in
      let query,mode = translate_input query in
      process_query id query mode;
    with
      Failure e -> generate_error_message "results/web/" id e
    | e -> generate_error_message "results/web/" id (Printexc.to_string e));
    generate_trailer ()

(* testowanie z linii poleceń:
echo "text0=Ala ma kota. Ela ma psa." | ./parser4.cgi
echo "text0=Szpak powiedział: „Frunę. Kiszę.”" | ./parser4.cgi
*)
