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

open Xstd
open Printf
open ExecTypes

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)
 
let lcg_process path query =
  ignore(Sys.command ("mkdir -p " ^ path));
  let path = if path = "" then path else if String.get path (String.length path - 1) = '/' then path else path ^ "/" in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  let result = Exec.process_query ic oc Paths.lcg_timeout false "x" query Paths.max_no_solutions in
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  File.file_out (path ^ "log") (fun file -> fprintf file "%s\n" (Visualization.generate_status_message result result.status));
  if result.status = Parsed then (
    ignore(Xlist.fold2 result.trees result.mrls 1 (fun n tree mrl -> 
      Visualization.print_graph2 path ("tree_" ^ string_of_int n) "" tree;
      Visualization.print_xml_tree path ("tree_" ^ string_of_int n) tree;
      let mml = SemMmlOf.mml_of_mrl mrl in
      Visualization.print_mml path ("formula_" ^ string_of_int n) mml;
(*       print_webpage path query n (Xlist.size result.trees) mml; *)
      n+1)))
  
let _ =  
  if Array.length Sys.argv < 2 then print_endline "missing argument" else
  lcg_process Paths.results_path Sys.argv.(1)
