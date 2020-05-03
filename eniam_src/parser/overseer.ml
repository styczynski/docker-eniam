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

let execution n_workers work file =
  let size = Xlist.size work in
  let r = ref (size + n_workers) in 
  let size = string_of_int size in
  let work = ref work in
(*   let output = ref [] in *)
  let sum_result = ref Exec.empty_sum_result in
  let id = string_of_int (Unix.getpid ()) in
  let io_list = Int.fold 1 n_workers [] (fun io_list _ ->
    print_endline (id ^ " create_worker");
    let in_chan,out_chan = Unix.open_process "./eniam.worker" in
    let descr = Unix.descr_of_in_channel in_chan in
    (in_chan,out_chan,descr) :: io_list) in
  let descr_list = Xlist.map io_list (fun (_,_,descr) -> descr) in
  while !r <> 0 do 
    print_endline (id ^ " Unix.select"); 
    let list,_,_ = Unix.select descr_list [] [] (-1.) in
    print_endline (id ^ " selected " ^ (string_of_int (Xlist.size list))); 
    Xlist.iter list (fun descr2 ->
      decr r; 
      Xlist.iter io_list (fun (in_chan,out_chan,descr) ->
        if descr = descr2 then (
          let idw = match Marshal.from_channel in_chan with
            Ready_to_work idw -> 
              print_endline (idw ^ " ready");
              idw
          | Work_done (idw,s) -> 
              print_endline (idw ^ " work done");
(*               output := s :: (!output); *)
              Exec.print_result file s;
              sum_result := Exec.add_result !sum_result s;
              Exec.print_sum_result file !sum_result;
              idw in
          match !work with 
            (id,params) :: l -> 
              Marshal.to_channel out_chan (Work_with (id,params)) [Marshal.No_sharing]; 
              flush out_chan;
              print_endline (idw ^ " scheduled " ^ id ^ " of " ^ size);
              work := l
          | [] ->
              Marshal.to_channel out_chan Kill_yourself [Marshal.No_sharing]; 
              print_endline (idw ^ " finished"))))
  done;
  print_endline (id ^ " exit");
(*   !output *)()

let _ = 
  if Array.length Sys.argv < 2 then print_endline "missing argument" else
  let n_workers = Paths.no_processes in 
  let work = Exec.generate_queries Sys.argv.(1) Paths.lcg_timeout in
  File.file_out (Paths.results_path ^ "log") (fun file -> 
  execution n_workers work file)
