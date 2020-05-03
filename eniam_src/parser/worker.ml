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

open ExecTypes

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  let id = string_of_int (Unix.getpid ()) in
  let f = ref true in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  Marshal.to_channel stdout (Ready_to_work id) [Marshal.No_sharing];
  flush stdout;
  while !f do
    match Marshal.from_channel stdin with
      Work_with (akt_id,(query,timeout)) ->
        let result = Exec.process_query ic oc timeout false "" query Paths.max_no_solutions in
        File.file_out_append (Paths.results_path ^ "worker.log") (fun file -> Printf.fprintf file "%s %s\n%!" akt_id query);
        if result.status = Parsed then (
            File.file_out_append (Paths.results_path ^ "worker.log") (fun file -> Printf.fprintf file "%s parsed\n%!" akt_id);
          ignore(Xlist.fold2 result.trees result.mrls 1 (fun n tree mrl ->
            File.file_out_append (Paths.results_path ^ "worker.log") (fun file -> Printf.fprintf file "%s parsed2\n%!" akt_id);
            Visualization.print_graph2 Paths.results_path (akt_id ^ "_tree_" ^ string_of_int n) "" tree;
            Visualization.print_xml_tree Paths.results_path (akt_id ^ "_tree_" ^ string_of_int n) tree;
            let mml = SemMmlOf.mml_of_mrl mrl in
            Visualization.print_mml Paths.results_path (akt_id ^ "_formula_" ^ string_of_int n) mml;
            n+1)));
        let result = {result with chart=[| |]; term=[| |]; disamb=[| |]; sem=[| |]; sem2=[| |]; sem3=LCGtypes.Dot; trees=[]; mrls=[]; paths=[| |]} in
        Marshal.to_channel stdout (Work_done(id, result)) [Marshal.No_sharing];
        flush stdout
    | Kill_yourself -> f := false
  done;
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  print_endline (id ^ " work finished")
