let start_server dir =
  let serv_in, serv_out, serv_err = Unix.open_process_full ("cd " ^ dir ^ "; ./swigra -w")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  ignore @@ input_line serv_in;
  serv_in, serv_out, serv_err

let stop_server (serv_in, serv_out, serv_err) =
  output_string serv_out "halt.\n";
  ignore @@ Unix.close_process_full (serv_in, serv_out, serv_err)

let curl s =
  let curl_in, curl_out, curl_err = Unix.open_process_full ("curl 'http://localhost:3333/swigra' --data-urlencode 'q=" ^ s ^ "'")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  try
    while true do
      ignore @@ input_line curl_in
    done
  with End_of_file -> ignore @@ Unix.close_process_full (curl_in, curl_out, curl_err)

let print_xml dir =
  let xml_in = open_in @@ dir ^ "/httpd/forest-disamb.xml" in
  try
    while true do
      print_endline @@ input_line xml_in
    done
  with End_of_file -> close_in xml_in

let _ =
  if Array.length @@ Sys.argv < 2 then
    print_endline "Usage: swigra_test <swigra_directory>"
  else if (not @@ Sys.file_exists Sys.argv.(1)) || (not @@ Sys.is_directory Sys.argv.(1)) then
    print_endline "Error: The provided directory does not exist."
  else
  (
    let dir = Sys.argv.(1) in
    let server = start_server dir in
    print_endline "Ready";
    curl "Ala ma kota.";
    print_xml dir;
    curl "Ala ma psa.";
    print_xml dir;
    stop_server server
  )
