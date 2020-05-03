let concraft_exists () =
  let check_in, check_out, check_err = Unix.open_process_full ("command -v concraft-pl")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  let close_check () = Unix.close_process_full (check_in, check_out, check_err) in
  try
    ignore @@ input_line check_in;
    ignore @@ close_check ();
    true
  with End_of_file -> ignore @@ close_check (); false

let wait_for_server () =
  let rec wait s a =
    try Unix.connect s a
    with e -> Unix.sleep 1; wait s a in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 6 in
  let a = Unix.ADDR_INET (Unix.inet_addr_loopback, 10089) in
  wait s a;
  Unix.shutdown s Unix.SHUTDOWN_SEND;
  Unix.close s

let start_server m =
  let client_out, server_out = Unix.pipe () in
  let client_err, server_err = Unix.pipe () in
  let pid = Unix.create_process "concraft-pl" [|"concraft-pl"; "server"; "--inmodel"; m|]
    Unix.stdin server_out server_err in
  List.iter Unix.close [client_out; server_out; client_err; server_err];
  wait_for_server ();
  pid

let stop_server pid =
  Unix.kill pid Sys.sigint

let tag s =
  Unix.open_process_full ("echo \"" ^ s ^ "\" | concraft-pl client")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|]

let _ =
  if not @@ concraft_exists () then
    print_endline "Error: The command concraft-pl is missing. Please make sure Concraft is installed properly."
  else if Array.length @@ Sys.argv < 2 then
    print_endline "Usage: concraft_test <model_file>"
  else if not @@ Sys.file_exists Sys.argv.(1) then
    print_endline "Error: The provided model file does not exist."
  else
  (
    print_endline "Starting Server";
    let pid = start_server @@ Sys.argv.(1) in
    print_endline "Ready";
    let concraft_in, concraft_out, concraft_err = tag "Ala ma kota." in
    try
      while true do
        print_endline @@ input_line concraft_in
      done
    with End_of_file -> ();
    try
      while true do
        print_endline @@ "concraft error message: " ^ input_line concraft_err
      done
    with End_of_file -> ();
    ignore @@ Unix.close_process_full (concraft_in, concraft_out, concraft_err);
    stop_server pid
  )
